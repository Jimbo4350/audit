{-# LANGUAGE OverloadedStrings #-}

module Audit.Operations
  ( buildPackageList
  , checkHash
  , clearAuditorTable
  , clearDiffTable
  , deleteAuditorEntry
  , deleteHash
  , diffDepToPackage
  , insertAuditorDeps
  , insertHash
  , insertPackageDiff
  , insertRemovedDependencies
  , loadNewDirDepsDiff
  , loadNewIndirectDepsDiff
  , loadDiffTableRemovedDeps
  , loadDiffTable
  , loadDiffIntoAuditor
  , pkgToAuditor
  , pkgToDiff
  , updateAuditorEntryWithDiff
  ) where


import           Audit.Database                             (Auditor,
                                                             AuditorDb (..),
                                                             AuditorT (..),
                                                             Diff, DiffT (..),
                                                             HashT (..),
                                                             PrimaryKey (..),
                                                             auditorDb)
import           Audit.Queries                              (queryAuditorDepNames,
                                                             queryDiff,
                                                             queryDiff',
                                                             queryHash)
import           Audit.Sorting                              (removedDeps)
import           Audit.Types                                (AnalysisStatus, ConversionError (..),
                                                             DirectDependency,
                                                             HashStatus (..),
                                                             IndirectDependency,
                                                             OperationError (..),
                                                             OperationResult (..),
                                                             Package (..),
                                                             PackageName,
                                                             QPResult (..),
                                                             Version)
import           Control.Monad.Trans.Either                 (EitherT,
                                                             hoistEither)
import           Data.Either                                (rights)
import           Data.List                                  (all, find, sort)
import           Data.Maybe                                 (isNothing)
import           Data.Maybe                                 (fromMaybe)
import           Data.Set                                   (difference,
                                                             fromList, toList)
import           Data.Text                                  (Text, pack, unpack)
import           Data.Time.Clock                            (UTCTime)
import           Data.Time.Format                           (defaultTimeLocale,
                                                             formatTime)
import           Database.Beam                              (all_, delete,
                                                             insert, insertFrom,
                                                             insertValues,
                                                             liftIO, lookup_,
                                                             runDelete,
                                                             runInsert,
                                                             runSelectReturningOne,
                                                             val_, (==.))
import           Database.Beam.Sqlite.Connection            (runBeamSqlite)
import           Database.SQLite.Simple                     (close, open)
import           Database.SQLite.Simple.Time.Implementation (parseUTCTime)


--------------------------------------------------------------------------------
-- Conversion Functions -- ALL TESTED
--------------------------------------------------------------------------------

-- | Build package/dependency list for the initialization of
-- the database.
buildPackageList :: [(PackageName, Version)]
                 -> [DirectDependency]
                 -> [IndirectDependency]
                 -> UTCTime
                 -> [Package]
buildPackageList pVersions dDeps indirDeps currTime = do

    let directPackages = map (directPkg currTime) dDeps
    let indirectPackages = map (indirectPkg currTime) indirDeps

    directPackages ++ indirectPackages
  where
    lookupVersion x = (pack $ fromMaybe "No version Found" (lookup x pVersions))
    directPkg time x = Package (pack x) (lookupVersion x) time True True []
    indirectPkg time x = Package (pack x) (lookupVersion x) time False True []

createPackage :: Text -> Text -> UTCTime -> Bool -> Bool -> [AnalysisStatus] -> Package
createPackage pName pVer dateFirSeen dirDep stillUsed aStat =
    Package
      pName
      pVer
      dateFirSeen
      dirDep
      stillUsed
      aStat

pkgToAuditor :: Package -> Auditor
pkgToAuditor (Package pName pVersion dateFS dDep sUsed aStatus) = do
    let fTime = formatTime defaultTimeLocale "%F %X%Q" dateFS
    Auditor
        pName
        pVersion
        (pack fTime)
        (pack $ show dDep)
        (pack $ show sUsed)
        (pack $ show aStatus)

pkgToDiff :: Package -> Diff
pkgToDiff (Package pName pVersion dateFS dDep sUsed aStatus) = do
    let fTime = formatTime defaultTimeLocale "%F %X%Q" dateFS
    Diff
        pName
        pVersion
        (pack fTime)
        (pack $ show dDep)
        (pack $ show sUsed)
        (pack $ show aStatus)


diffDepToPackage :: Diff -> Either ConversionError Package
diffDepToPackage diff =
    case diffDepToText diff of
        [ pName, pVer, dateFirSeen, dirDep, stillUsed, aStat] -> do
            case (parseUTCTime dateFirSeen) of
                Left err -> Left $ UTCTimeParseError err
                Right time -> Right $ createPackage pName pVer time (read $ unpack dirDep) (read $ unpack stillUsed) ((read $ unpack aStat) :: [AnalysisStatus])
        _ -> error "diffDepToPackage: An additional column was added to the Diff table, please account for it here"

audDepToPackage :: Auditor -> Either ConversionError Package
audDepToPackage diff =
    case audDepToText diff of
        [ pName, pVer, dateFirSeen, dirDep, stillUsed, aStat] -> do
            case (parseUTCTime dateFirSeen) of
                Left err -> Left $ UTCTimeParseError err
                Right time -> Right $ createPackage pName pVer time (read $ unpack dirDep) (read $ unpack stillUsed) ((read $ unpack aStat) :: [AnalysisStatus])
        _ -> error "diffDepToPackage: An additional column was added to the Diff table, please account for it here"

--------------------------------------------------------------------------------
-- Auditor Table Operations - All Tested
--------------------------------------------------------------------------------

-- | Delete the dependencies in the auditor table.
clearAuditorTable :: String -> IO ()
clearAuditorTable fp = do
    conn <- open fp
    allAuditorDeps <- queryAuditorDepNames fp
    mapM_ (\x -> runBeamSqlite conn $ runDelete $
        delete (auditor auditorDb)
            (\c -> auditorPackageName c ==. val_ x)) allAuditorDeps
    close conn

-- | Deletes the hash in the hash table.
deleteHash :: String -> IO ()
deleteHash dbName = do
    conn <- open dbName
    cHash <- queryHash dbName
    case cHash of
        Just dbH ->  do
            let dbHashInt = hashCurrentHash dbH
            runBeamSqlite conn $ runDelete $
                delete (hash auditorDb)
                    (\table -> hashCurrentHash table ==. val_ dbHashInt)
        Nothing -> print ("Hash not found in database" :: String)
    close conn

deleteAuditorEntry :: String -> Auditor -> IO ()
deleteAuditorEntry dbName aud = do
    conn <- open dbName
    runBeamSqlite conn $ runDelete $
      delete (auditor auditorDb)
          (\table -> auditorPackageName table ==. val_ (auditorPackageName aud))
    close conn

-- | Inserts original direct & indirect dependencies into auditor table.
insertAuditorDeps :: String -> [Package] -> IO ()
insertAuditorDeps dbFilename pkgs = do
    let auditorConversion = map pkgToAuditor pkgs
    conn <- open dbFilename
    runBeamSqlite conn $ runInsert $
        insert (auditor auditorDb) $
        insertValues auditorConversion
    close conn

-- | Takes a hash and inserts it into the Hash table.
insertHash :: String -> Int -> IO ()
insertHash dbFilename dHash = do
    curHash <- queryHash dbFilename
    case curHash of
        Just _ -> error $ "insertHash: A hash is already present in " ++ dbFilename
        Nothing -> do
          conn <- open dbFilename
          runBeamSqlite conn $ runInsert $
              insert (hash auditorDb) $
              insertValues [ Hash dHash ]
          close conn

--------------------------------------------------------------------------------
-- Diff Table Operations - All Tested
--------------------------------------------------------------------------------


-- | Delete the dependencies in the diff table.
clearDiffTable :: String -> IO ()
clearDiffTable dbName = do
    conn <- open dbName
    allDiffDeps <- queryDiff dbName
    mapM_ (\x -> runBeamSqlite conn $ runDelete $
        delete (diff auditorDb)
            (\c -> diffPackageName c ==. val_ x)) allDiffDeps
    close conn

-- | Takes a newly added `Package` and inserts it into the Diff table.
insertPackageDiff :: String -> Package -> IO ()
insertPackageDiff dbFilename (Package pName pVersion dateFS dDep sUsed aStatus) = do
    conn <- open dbFilename
    let fTime = formatTime defaultTimeLocale "%F %X%Q" dateFS
    runBeamSqlite conn $ runInsert $
        insert (diff  auditorDb) $
        insertValues [ Diff
                           pName
                           pVersion
                           (pack fTime)
                           (pack $ show dDep)
                           (pack $ show sUsed)
                           (pack $ show aStatus)
                     ]
    close conn

-- | Insert updated dependencies into a db Diff table.
-- depList    = list of dependencies you want to insert.
-- dirOrIndir = Bool signlaing whether or not the dependencies you are
--              inserting are direct or indirect.
-- inYaml     = Bool signaling whether or not the dependencies you
--              are still in use.

-- | Load a list of 'Package's into the Diff table.
-- TODO: This can still throw an error from runBeamSqlite
-- capture this in EitherT
loadDiffTable :: String -> [Package] -> IO OperationResult
loadDiffTable dbName pkgs = do
    let diffConversion = map pkgToDiff pkgs
    conn <- open dbName
    runBeamSqlite conn $ runInsert $
        insert (diff auditorDb) $
        insertValues diffConversion
    close conn
    pure AddedDependenciesDiff

-- | Inserts new direct dependencies into the diff table.
loadNewDirDepsDiff :: String -> [Package] -> EitherT OperationError IO OperationResult
loadNewDirDepsDiff dbName parsedDeps = do
    dirDeps <- hoistEither $ checkAllAreDirectDeps parsedDeps
    queriedDiffDeps <- liftIO $ queryDiff' dbName
    -- TODO: You are ignoring a potential failure here with rights
    let queriedPkgs =  rights $ map diffDepToPackage queriedDiffDeps
    case parseQueryDifference dirDeps queriedPkgs of
        QueryAndParseIdentical -> pure AlreadyAddedDependenciesDiff
        QPDifference new -> do
            _ <- liftIO $ loadDiffTable dbName new
            pure AddedDependenciesDiff

-- | Inserts new indirect dependencies into the diff table.
loadNewIndirectDepsDiff :: String -> [Package] -> EitherT OperationError IO OperationResult
loadNewIndirectDepsDiff dbName parsedDeps = do
    indirDeps <- hoistEither $ checkAllAreIndirectDeps parsedDeps
    queriedDiffDeps <- liftIO $ queryDiff' dbName
    let queriedPkgs =  rights $ map diffDepToPackage queriedDiffDeps
    case parseQueryDifference indirDeps queriedPkgs of
        QueryAndParseIdentical -> pure AddedDependenciesDiff
        QPDifference new -> do
            _ <- liftIO $ loadDiffTable dbName new
            pure AddedDependenciesDiff

loadDiffTableRemovedDeps :: String -> IO ()
loadDiffTableRemovedDeps dbName = do
    rDeps <- removedDeps
    let rDepText = map pack rDeps -- TODO: Neaten this up
    depsInDiff <- queryDiff dbName
    if all (== False ) [x `elem` map unpack depsInDiff | x <- rDeps]
        then insertRemovedDependencies dbName rDepText True False
        -- TODO: Need to differentiate between direct and indirect removed deps
        else print ("Already added removed dependencies to the Diff table" :: String)

--------------------------------------------------------------------------------
-- Operations involving both Diff & Auditor tables.
--------------------------------------------------------------------------------

-- | Compare an entry from the Diff table and Auditor table.
-- Return an updated `Auditor` that will be inserted into the auditor table.
updateAuditorEntryWithDiff :: Diff -> Auditor -> Either Text Auditor
updateAuditorEntryWithDiff updatedDep aud = do
    let diffTextVals = diffDepToText updatedDep

    let audTextVals = map (\f -> f aud) [ auditorPackageName
                                        , auditorPackageVersion
                                        , auditorDateFirstSeen
                                        , auditorDirectDep
                                        , auditorStillUsed
                                        , auditorAnalysisStatus
                                        ]

    let zipped = zip audTextVals diffTextVals
    toAuditor [if audText == diffText then audText else diffText | (audText, diffText) <- zipped] (auditorDateFirstSeen aud)
  where
    toAuditor :: [Text] -> Text -> Either Text Auditor
    toAuditor [pName, pVer, _, dDep, sUsed, aStat] origDateFirstSeen = Right $ Auditor pName pVer origDateFirstSeen dDep sUsed aStat
    toAuditor _ _ = Left "toAuditor: The impossible happened, this should only yield a list of length 6"

-- | Updates diff with removed dep. Queries auditor because
-- after the dep is removed from the repository,
-- `stack ls dependencies` can no longer get the version.
-- List of text is the list of dependency names
insertRemovedDependencies :: String -> [Text] -> Bool -> Bool -> IO ()
insertRemovedDependencies _ [] _ _ = pure ()
insertRemovedDependencies dbFilename (x:xs) dirOrIndir inYaml = do
    conn <- open dbFilename
    -- Queries auditor
    audQresult <- runBeamSqlite conn $ do
        let primaryKeyLookup = lookup_ (auditor auditorDb)
        let sqlQuery = primaryKeyLookup $ AuditorPackageName x
        runSelectReturningOne sqlQuery
    -- Gets original time the package was first added.
    case audQresult of
        -- Nothing branch should never match
        Nothing -> insertRemovedDependencies dbFilename xs dirOrIndir False
        Just result -> do
                         let dFSeen = auditorDateFirstSeen result
                         case parseUTCTime dFSeen of
                             Right utcTime -> do
                                                insertPackageDiff dbFilename (Package
                                                    x
                                                    (auditorPackageVersion result)
                                                    utcTime
                                                    dirOrIndir
                                                    inYaml
                                                    [])
                                                close conn
                                                insertRemovedDependencies dbFilename xs dirOrIndir False
                             Left err -> print $ "insertRemovedDependencies: " ++ err


-- | Insert dependencies from the Diff table to the Auditor table.
-- If they are new dependencies, directly insert them otherwise
-- call `updateOrModify`
loadDiffIntoAuditor :: String -> IO ()
loadDiffIntoAuditor dbName = do
    conn <- open dbName
    diffDeps <-  queryDiff dbName
    -- Check to see if the dependency in Diff exists in Auditor
    -- If it does, indicates either a version change or dependency removal
    -- Returns a list of Maybes
    existingDeps <- mapM (\x -> runBeamSqlite conn $ do
                                let primaryKeyLookup = lookup_ (auditor auditorDb)
                                let sqlQuery = primaryKeyLookup $ AuditorPackageName x
                                runSelectReturningOne sqlQuery) diffDeps
    case all isNothing existingDeps of
        -- Inserts all the dependencies from the Diff table into the Auditor table
        -- These are new dependencies
        True -> runBeamSqlite conn $ runInsert $
                    insert (auditor auditorDb) $
                        insertFrom $ do
                            allEntries <- all_ (diff auditorDb)
                            pure (Auditor (diffPackageName allEntries)
                                          (diffPackageVersion allEntries)
                                          (diffDateFirstSeen allEntries)
                                          (diffDirectDep allEntries)
                                          (diffStillUsed allEntries)
                                          (diffAnalysisStatus allEntries))
        -- There are deps in the Diff table that exists in the Auditor table
        -- I.e this could be a version change
        _ -> updateOrModify diffDeps
    -- TODO: New plan, query the Auditor db with each of the "diffDeps". If it exists,
    -- pull all the fields and see what has changed. You need to copy the timestamp to Diff
    -- entry. Delete the existing entry in  Auditor and insert the new updated Diff entry
    close conn

-- | Add a new dependency to the Auditor table or modify an
-- existing dependency in the Auditor table
updateOrModify :: [Text]-> IO ()
updateOrModify [] = pure ()
updateOrModify (x:xs) = do
    conn <- open "auditor.db"
    -- Check to see if the dependency in Diff exists in Auditor
    -- If it does, indicates either a version change or dependency removal
    audQresult <- runBeamSqlite conn $ do
                  let primaryKeyLookup = lookup_ (auditor auditorDb)
                  let sqlQuery = primaryKeyLookup $ AuditorPackageName x
                  runSelectReturningOne sqlQuery
    case audQresult of
        -- Either the version has changed or the dependency has been removed
        -- from the repo.
        Just audDep -> do
            -- Get dependency from Diff table
            diffQresult <- runBeamSqlite conn $ do
                           let primaryKeyLookup = lookup_ (diff auditorDb)
                           let sqlQuery = primaryKeyLookup $ DiffPackageName x
                           runSelectReturningOne sqlQuery
            case diffQresult of
                Nothing -> updateOrModify xs
                Just diffDep ->   -- Compare Diff and Audit query; return the updated dependency information

                  case updateAuditorEntryWithDiff diffDep audDep of
                    Left err -> error $ show err
                    Right depToIns -> do
                        runBeamSqlite conn $ runDelete $
                            delete (auditor auditorDb)
                                (\table -> auditorPackageName table ==. (val_ $ diffPackageName diffDep))
                        runBeamSqlite conn $ runInsert $
                            insert (auditor auditorDb) $
                            insertValues [ depToIns ]
                        updateOrModify xs
        -- New dependency to add to Auditor table.
        Nothing -> do
            diffList <- queryDiff' "auditor.db"
            case find (\d -> diffPackageName d == x ) diffList of
                Nothing -> updateOrModify xs
                Just matchedDep -> do
                                     runBeamSqlite conn $ runInsert $
                                         insert (auditor auditorDb) $
                                             insertFrom $
                                                 pure (Auditor (val_ $ diffPackageName matchedDep)
                                                               (val_ $ diffPackageVersion matchedDep)
                                                               (val_ $ diffDateFirstSeen matchedDep)
                                                               (val_ $ diffDirectDep matchedDep)
                                                               (val_ $ diffStillUsed matchedDep)
                                                               (val_ $ diffAnalysisStatus matchedDep))
                                     updateOrModify xs

--------------------------------------------------------------------------------
-- Hash Table Operations
--------------------------------------------------------------------------------


-- | Takes a hash and compares it to the database hash.
checkHash :: String -> Int -> IO HashStatus
checkHash dbName testHash = do
    entry <- queryHash dbName
    case entry of
      Just dbH -> do
          let dbHashInt = hashCurrentHash dbH
          if testHash == dbHashInt
              then liftIO $ return HashMatches
              else liftIO $ return HashDoesNotMatch
      Nothing -> liftIO $ return HashNotFound

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

checkAllAreDirectDeps :: [Package] -> Either OperationError [Package]
checkAllAreDirectDeps pkgs =
    if all ((True ==) . directDep) pkgs
        then return pkgs
        else Left . OnlyDirectDepenciesAllowed $ filter ((False ==) . directDep) pkgs

checkAllAreIndirectDeps :: [Package] -> Either OperationError [Package]
checkAllAreIndirectDeps pkgs =
    if all ((False ==) . directDep) pkgs
        then return pkgs
        else Left . OnlyIndirectDepenciesAllowed $ filter ((True ==) . directDep) pkgs

diffDepToText :: Diff -> [Text]
diffDepToText diffDep =
    map
      (\f -> f diffDep)
      [ diffPackageName
      , diffPackageVersion
      , diffDateFirstSeen
      , diffDirectDep
      , diffStillUsed
      , diffAnalysisStatus
      ]

audDepToText :: Auditor -> [Text]
audDepToText audDep =
    map
      (\f -> f audDep)
      [ auditorPackageName
      , auditorPackageVersion
      , auditorDateFirstSeen
      , auditorDirectDep
      , auditorStillUsed
      , auditorAnalysisStatus
      ]

-- | Returns the associative difference of two lists.
-- If the parsed dependencies differ from the database
-- dependencies, we return the difference or a nullary constructor.
parseQueryDifference :: (Eq a, Ord a) => [a] -> [a] -> QPResult a
parseQueryDifference parsed queriedDeps
    | parsed == queriedDeps = QueryAndParseIdentical
    | otherwise = QPDifference diff
  where
    diff = sort . toList $ difference (fromList parsed) (fromList queriedDeps)
