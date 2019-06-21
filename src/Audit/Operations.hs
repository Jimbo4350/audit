{-# LANGUAGE OverloadedStrings #-}

module Audit.Operations
  ( buildPackageList
  , checkHash
  , clearAuditorTable
  , clearDiffTable
  , updateAuditorEntryWithDiff
  , deleteHash
  , insertAuditorDeps
  , loadDiffTable
  , insertHash
  , insertPackageDiff
  , insertRemovedDependencies
  , loadDiffIntoAuditor
  , pkgToAuditor
  , pkgToDiff
  , loadDiffTableDirectDeps
  , loadDiffTableIndirectDeps
  , loadDiffTableRemovedDeps
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
import           Audit.Types                                (DirectDependency,
                                                             HashStatus (..),
                                                             IndirectDependency,
                                                             Package (..),
                                                             PackageName,
                                                             Version)
import           Data.List                                  (all, find)
import           Data.Maybe                                 (isNothing)
import           Data.Maybe                                 (fromMaybe)
import           Data.Text                                  (Text, pack, unpack)
import           Data.Time.Clock                            (getCurrentTime)
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


------------------- Agnostic Ops -------------------

-- | Build package/dependency list for the initialization of
-- the database.
buildPackageList :: [(PackageName, Version)]
                 -> [DirectDependency]
                 -> [IndirectDependency]
                 -> IO [Package]
buildPackageList pVersions dDeps indirDeps = do
    cTime <- getCurrentTime
    let directPackages = map (directPkg cTime) dDeps
    let indirectPackages = map (indirectPkg cTime) indirDeps

    return $ directPackages ++ indirectPackages
  where
    lookupVersion x = (pack $ fromMaybe "No version Found" (lookup x pVersions))
    directPkg time x = Package (pack x) (lookupVersion x) time True True []
    indirectPkg time x = Package (pack x) (lookupVersion x) time False True []

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

------------------- Auditor table Ops -------------------

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

------------------- Diff table Ops -------------------


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

loadDiffTable :: String -> [Package] -> IO ()
loadDiffTable dbName pkgs = do
    let diffConversion = map pkgToDiff pkgs
    conn <- open dbName
    runBeamSqlite conn $ runInsert $
        insert (diff auditorDb) $
        insertValues diffConversion
    close conn

-- | Inserts new direct dependencies into the diff table.
loadDiffTableDirectDeps :: String -> [Package] -> IO ()
loadDiffTableDirectDeps dbName pkgs = do
    depsInDiff <- queryDiff dbName
    if all (== False ) [packageName x `elem` depsInDiff | x <- pkgs]
        then loadDiffTable dbName pkgs
        else print ("Already added new direct dependencies to the Diff table" :: String)

-- | Inserts new indirect dependencies into the diff table.
loadDiffTableIndirectDeps :: String -> [Package] -> IO ()
loadDiffTableIndirectDeps dbName pkgs = do
    depsInDiff <- queryDiff dbName
    if all (== False ) [packageName x `elem` depsInDiff | x <- pkgs]
        then loadDiffTable dbName pkgs
        else print ("Already added new indirect dependencies to the Diff table" :: String)

loadDiffTableRemovedDeps :: String -> IO ()
loadDiffTableRemovedDeps dbName = do
    rDeps <- removedDeps
    let rDepText = map pack rDeps -- TODO: Neaten this up
    depsInDiff <- queryDiff dbName
    if all (== False ) [x `elem` map unpack depsInDiff | x <- rDeps]
        then insertRemovedDependencies dbName rDepText True False
        -- TODO: Need to differentiate between direct and indirect removed deps
        else print ("Already added removed dependencies to the Diff table" :: String)

----------------------- Involves Both Auditor and Diff ------------------

-- | Compare an entry from the Diff table and Auditor table.
-- Return an updated `Auditor` that will be inserted into the auditor table.
updateAuditorEntryWithDiff :: Diff -> Auditor -> Either Text Auditor
updateAuditorEntryWithDiff updatedDep aud = do
    let diffTextVals = map (\f -> f updatedDep) [ diffPackageName
                                                , diffPackageVersion
                                                , diffDateFirstSeen
                                                , diffDirectDep
                                                , diffStillUsed
                                                , diffAnalysisStatus
                                                ]

    let audTextVals = map (\f -> f aud) [ auditorPackageName
                                        , auditorPackageVersion
                                        , auditorDateFirstSeen
                                        , auditorDirectDep
                                        , auditorStillUsed
                                        , auditorAnalysisStatus
                                        ]

    let zipped = zip audTextVals diffTextVals

    toAuditor [if audText == diffText then audText else diffText | (audText, diffText) <- zipped]
  where
    toAuditor :: [Text] -> Either Text Auditor
    toAuditor [pName, pVer, dFs, dDep, sUsed, aStat] = Right $ Auditor pName pVer dFs dDep sUsed aStat
    toAuditor _ = Left "toAuditor: The impossible happened, this should only yield a list of length 6"


-- | Updates diff with removed dep. Queries auditor because
-- after the dep is removed from the repository,
-- `stack ls dependencies` can no longer get the version.
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

------------------- Hash table operations -------------------


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
