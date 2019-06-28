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
  , insertRemovedDependenciesDiff
  , loadNewDirDepsDiff
  , loadNewIndirectDepsDiff
  , loadDiffTableRemovedDeps
  , loadDiffTable
  , loadDiffIntoAuditorNew
  , loadDiffIntoAuditorUpdate
  , pkgToAuditor
  , pkgToDiff
  , updateAuditorEntryWithDiff
  , updateAuditorEntry
  ) where


import           Audit.Database                             (Auditor,
                                                             AuditorDb (..),
                                                             AuditorT (..),
                                                             Diff, DiffT (..),
                                                             HashT (..),
                                                             PrimaryKey (..),
                                                             auditorDb)
import           Audit.Queries                              (queryAuditor, queryAuditorDepNames,
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
                                                             firstEitherT,
                                                             hoistEither, left,
                                                             right, runEitherT)
import           Data.Either                                (isRight, rights)
import           Data.List                                  (all, find,
                                                             intersect, sort,
                                                             (\\))
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
                                                             runUpdate, save,
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
-- stillUsed     = Bool signaling whether or not the dependencies you
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
        then insertRemovedDependenciesDiff dbName rDepText True False
        -- TODO: Need to differentiate between direct and indirect removed deps
        else print ("Already added removed dependencies to the Diff table" :: String)

--------------------------------------------------------------------------------
-- Operations involving both Diff & Auditor tables.
--------------------------------------------------------------------------------

-- | Compare an entry from the Diff table and Auditor table.
-- Return an updated `Auditor` that will be inserted into the auditor table
-- with the preserved date first seen.
updateAuditorEntryWithDiff :: Diff -> Auditor -> Either ConversionError Auditor
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
    toAuditor :: [Text] -> Text -> Either ConversionError Auditor
    toAuditor [pName, pVer, _newDfSeen, dDep, sUsed, aStat] origDfSeen = Right $ Auditor pName pVer origDfSeen dDep sUsed aStat
    toAuditor _ _ = Left $ DiffToAuditorError "toAuditor: The impossible happened, this should only yield a list of length 6"

-- | Updates diff with removed dep. Queries auditor because
-- after the dep is removed from the repository,
-- `stack ls dependencies` can no longer get the version.
-- List of text is the list of dependency names
insertRemovedDependenciesDiff :: String -> [Text] -> Bool -> Bool -> IO ()
insertRemovedDependenciesDiff _ [] _ _ = pure ()
insertRemovedDependenciesDiff dbFilename (x:xs) dirOrIndir stillUsed = do
    conn <- open dbFilename
    -- Queries auditor
    audQresult <- runBeamSqlite conn $ do
        let primaryKeyLookup = lookup_ (auditor auditorDb)
        let sqlQuery = primaryKeyLookup $ AuditorPackageName x
        runSelectReturningOne sqlQuery
    -- Gets original time the package was first added.
    case audQresult of
        -- Nothing branch should never match
        Nothing -> insertRemovedDependenciesDiff dbFilename xs dirOrIndir False
        Just result -> do
                         let dFSeen = auditorDateFirstSeen result
                         case parseUTCTime dFSeen of
                             Right utcTime -> do
                                                insertPackageDiff dbFilename (Package
                                                    x
                                                    (auditorPackageVersion result)
                                                    utcTime
                                                    dirOrIndir
                                                    stillUsed
                                                    [])
                                                close conn
                                                insertRemovedDependenciesDiff dbFilename xs dirOrIndir False
                             Left err -> print $ "insertRemovedDependenciesDiff: " ++ err


-- | Updates an entry in the Auditor table and deletes the corresponding entry in Diff table.
updateAuditorEntry :: String -> Diff -> Auditor -> EitherT OperationError IO OperationResult
updateAuditorEntry dbName updatedPackage currentPackage = do
    conn <- liftIO $ open dbName
    updated <-  firstEitherT ConvError . hoistEither $ updateAuditorEntryWithDiff updatedPackage currentPackage
    -- Updates corresponding package in auditor table.
    liftIO $ runBeamSqlite conn $ runUpdate $ save (auditor auditorDb) updated
    -- Deletes package in diff
    liftIO $ runBeamSqlite conn $ runDelete $ delete (diff auditorDb)
        (\table -> diffPackageName table ==. (val_ $ diffPackageName updatedPackage))
    liftIO $ close conn
    pure AddedRemovedDependenciesAuditor


-- | Updates Auditor table with new dependencies from the Diff table
loadDiffIntoAuditorNew :: String -> EitherT OperationError IO OperationResult
loadDiffIntoAuditorNew dbName = do
    diffDeps <- liftIO $ queryDiff' dbName
    audDeps <- liftIO $ queryAuditor dbName
    let audPackages = mapM audDepToPackage audDeps
    let diffPackages = mapM diffDepToPackage diffDeps
    case (audPackages, diffPackages) of
        (Right aPkgs, Right dPkgs) -> do
                     let inCommonPkgs = aPkgs `intersect` dPkgs
                     let newPkgs = dPkgs \\ inCommonPkgs
                     conn <- liftIO $ open dbName

                     -- Insert new packages into Auditor table
                     liftIO $ runBeamSqlite conn $ runInsert $
                                insert (auditor auditorDb) $
                                  insertValues (map pkgToAuditor newPkgs)
                     liftIO $ close conn
                     right AddedDependenciesDiff
        (_,_) -> error "loadDiffIntoAuditorNew: Conversion from Diff/Auditor to Package went wrong"

-- | Updates existing dependencies in the Auditor table
-- with changes introduced by the Diff table.
loadDiffIntoAuditorUpdate :: String -> EitherT OperationError IO OperationResult
loadDiffIntoAuditorUpdate dbName = do
    diffDeps <- liftIO $ queryDiff' dbName
    audDeps <- liftIO $ queryAuditor dbName
    -- Convert 'Auditor' and 'Diff' to 'Packages'
    let audPackages = mapM audDepToPackage audDeps
    let diffPackages = mapM diffDepToPackage diffDeps
    case (audPackages, diffPackages) of
        (Right aPkgs, Right dPkgs) -> do
             let inCommonPkgs = aPkgs `intersect` dPkgs
             -- Prepare updated Auditor values for entry into the Auditor table
             let audEntries = zipWith (updateAuditorEntry dbName) diffDeps audDeps
             -- TODO: Handle these errors
             _ <- liftIO $ mapM runEitherT audEntries
             right AddedDependenciesDiff
        (_,_) -> error "loadDiffIntoAuditorUpdate: Conversion from Diff/Auditor to Package went wrong"



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
