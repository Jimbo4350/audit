{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Audit.Database
       ( Auditor
       , AuditorT (..)
       , buildPackageList
       , checkHash
       , clearAuditorTable
       , clearDiffTable
       , deleteHash
       , Diff
       , DiffT (..)
       , insertHash
       , insertDeps
       , insertPackageDiff
       , insertRemovedDependencies
       , insertDiffDependencies
       , loadDiffIntoAuditor
       , queryAuditor
       , queryAuditorDepNames
       , queryAuditorDepVersions
       , queryAuditorRemovedDeps
       , queryDiff
       , queryDiff'
       , queryDiffRemovedDeps
       , queryHash
       , updateDiffTableDirectDeps
       , updateDiffTableIndirectDeps
       , updateDiffTableRemovedDeps
       ) where

import           Data.List                                  (all, find)
import           Data.Maybe                                 (fromMaybe,
                                                             isNothing)
import           Data.Text                                  (Text, pack, unpack)
import           Data.Time.Clock                            (getCurrentTime)
import           Data.Time.Format                           (defaultTimeLocale,
                                                             formatTime)
import           Database.Beam                              (Beamable, Columnar,
                                                             Database,
                                                             DatabaseSettings,
                                                             Generic, Identity,
                                                             PrimaryKey (..),
                                                             Table (..),
                                                             TableEntity, all_,
                                                             defaultDbSettings,
                                                             delete, insert,
                                                             insertFrom,
                                                             insertValues,
                                                             liftIO, runDelete,
                                                             runInsert, select)
import           Database.Beam.Query                        (lookup_, runSelectReturningList,
                                                             runSelectReturningOne,
                                                             val_, (==.))
import           Database.Beam.Sqlite
import           Database.SQLite.Simple                     (close, open)
import           Database.SQLite.Simple.Time.Implementation (parseUTCTime)

import           Audit.Sorting                              (removedDeps)
import           Audit.Types                                (DirectDependency,
                                                             HashStatus (..),
                                                             IndirectDependency,
                                                             Package (..),
                                                             PackageName,
                                                             Version)

-- | Auditor Table. Stores the current direct, indirect and
-- removed dependencies.

data AuditorT f = Auditor
    { _auditorPackageName    :: Columnar f Text
    , _auditorPackageVersion :: Columnar f Text
    , _auditorDateFirstSeen  :: Columnar f Text
    , _auditorDirectDep      :: Columnar f Text
    , _auditorStillUsed      :: Columnar f Text
    , _auditorAnalysisStatus :: Columnar f Text
    } deriving Generic

type Auditor = AuditorT Identity

deriving instance Eq Auditor
deriving instance Show Auditor

instance Beamable AuditorT

instance Table AuditorT where
    data PrimaryKey AuditorT f = AuditorPackageName (Columnar f Text) deriving Generic
    primaryKey = AuditorPackageName . _auditorPackageName

instance Beamable (PrimaryKey AuditorT)

-- | Hash Table. Stores the hash of the original .dot file
-- and .txt file.

data HashT f = Hash
    { _hashDotHash :: Columnar f Int
    } deriving Generic

type Hash = HashT Identity

deriving instance Eq Hash
deriving instance Show Hash

instance Beamable HashT

instance Table HashT where
    data PrimaryKey HashT f = HashDotHash (Columnar f Int) deriving Generic
    primaryKey = HashDotHash . _hashDotHash

instance Beamable (PrimaryKey HashT)

-- | Dependency difference table. This table
-- stores any changes to the dependency tree.

data DiffT f = Diff
    { _diffPackageName    :: Columnar f Text
    , _diffPackageVersion :: Columnar f Text
    , _diffDateFirstSeen  :: Columnar f Text
    , _diffDirectDep      :: Columnar f Text
    , _diffStillUsed      :: Columnar f Text
    , _diffAnalysisStatus :: Columnar f Text
    } deriving Generic

type Diff = DiffT Identity

deriving instance Eq Diff
deriving instance Show Diff

instance Beamable DiffT

instance Table DiffT where
    data PrimaryKey DiffT f = DiffPackageName (Columnar f Text) deriving Generic
    primaryKey = DiffPackageName . _diffPackageName

instance Beamable (PrimaryKey DiffT)

-- | Database

data AuditorDb f = AuditorDb
    { _auditor :: f (TableEntity AuditorT)
    , _hash    :: f (TableEntity HashT)
    , _diff    :: f (TableEntity DiffT)
    } deriving Generic

instance Database be AuditorDb

auditorDb :: DatabaseSettings be AuditorDb
auditorDb = defaultDbSettings

-- | SQL queries etc

------------------- Auditor table Ops -------------------

-- | Delete the dependencies in the auditor table.
clearAuditorTable :: String -> IO ()
clearAuditorTable dbName = do
    conn <- open dbName
    allAuditorDeps <- queryAuditorDepNames dbName
    mapM_ (\x -> runBeamSqlite conn $ runDelete $
        delete (_auditor auditorDb)
            (\c -> _auditorPackageName c ==. val_ x)) allAuditorDeps
    close conn

-- | Deletes the hash in the hash table.
deleteHash :: String -> IO ()
deleteHash dbName = do
    conn <- open dbName
    currentHash <- queryHash
    case currentHash of
        Just dbH ->  do
            let dbHashInt = _hashDotHash dbH
            runBeamSqlite conn $ runDelete $
                delete (_hash auditorDb)
                    (\table -> _hashDotHash table ==. val_ dbHashInt)
        Nothing -> print ("Hash not found in database" :: String)
    close conn

-- | Inserts original direct & indirect dependencies into auditor table.
insertDeps :: String -> [Package] -> IO ()
insertDeps dbFilename pkgs = do
    let auditorConversion = map pkgToAuditor pkgs
    conn <- open dbFilename
    runBeamSqlite conn $ runInsert $
        insert (_auditor auditorDb) $
        insertValues auditorConversion
    close conn

-- | Takes a hash and inserts it into the Hash table.
insertHash :: String -> Int -> IO ()
insertHash dbFilename dotHash = do
    conn <- open dbFilename
    runBeamSqlite conn $ runInsert $
        insert (_hash auditorDb) $
        insertValues [ Hash dotHash ]
    close conn

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

-- | Returns all entries in the Auditor table.
queryAuditor :: String -> IO [Auditor]
queryAuditor  dbName = do
    conn <- open dbName
    let allEntries = all_ (_auditor auditorDb)
    entries <- runBeamSqlite conn $
        runSelectReturningList $ select allEntries
    close conn
    return entries

queryAuditorDepNames :: String -> IO [Text]
queryAuditorDepNames dbName  = do
    conn <- open dbName
    let allEntries = all_ (_auditor auditorDb)
    entries <- runBeamSqlite conn $
        runSelectReturningList $ select allEntries
    close conn
    return $ map _auditorPackageName entries

queryAuditorDepVersions :: String -> IO [Text]
queryAuditorDepVersions dbName  = do
    conn <- open dbName
    let allEntries = all_ (_auditor auditorDb)
    entries <- runBeamSqlite conn $
        runSelectReturningList $ select allEntries
    close conn
    return $ map _auditorPackageVersion entries

queryAuditorRemovedDeps :: String -> IO [Text]
queryAuditorRemovedDeps dbName  = do
    conn <- open dbName
    let allEntries = all_ (_auditor auditorDb)
    entries <- runBeamSqlite conn $
        runSelectReturningList $ select allEntries
    close conn
    let remDeps = filter (\x -> _auditorStillUsed x == "False") entries
    return $ map _auditorPackageName remDeps

------------------- Diff table Ops -------------------


-- | Delete the dependencies in the diff table.
clearDiffTable :: String -> IO ()
clearDiffTable dbName = do
    conn <- open dbName
    allDiffDeps <- queryDiff dbName
    mapM_ (\x -> runBeamSqlite conn $ runDelete $
        delete (_diff auditorDb)
            (\c -> _diffPackageName c ==. val_ x)) allDiffDeps
    close conn

-- | Takes a newly added `Package` and inserts it into the Diff table.
insertPackageDiff :: String -> Package -> IO ()
insertPackageDiff dbFilename (Package pName pVersion dateFS dDep sUsed aStatus) = do
    conn <- open dbFilename
    let fTime = formatTime defaultTimeLocale "%F %X%Q" dateFS
    runBeamSqlite conn $ runInsert $
        insert (_diff  auditorDb) $
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

insertDiffDependencies :: String -> [Package] -> IO ()
insertDiffDependencies dbName pkgs = do
    let diffConversion = map pkgToDiff pkgs
    conn <- open dbName
    runBeamSqlite conn $ runInsert $
        insert (_diff auditorDb) $
        insertValues diffConversion
    close conn

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

queryDiff :: String ->  IO [Text]
queryDiff dbName = do
    conn <- open dbName
    let allEntries = all_ (_diff auditorDb)
    entries <- runBeamSqlite conn $
               runSelectReturningList $ select allEntries
    close conn
    return $ map  _diffPackageName entries

queryDiffRemovedDeps :: String -> IO [Text]
queryDiffRemovedDeps dbName  = do
    conn <- open dbName
    let allEntries = all_ (_diff auditorDb)
    entries <- runBeamSqlite conn $
        runSelectReturningList $ select allEntries
    close conn
    let remDeps = filter (\x -> _diffStillUsed x == "False") entries
    return $ map _diffPackageName remDeps

queryDiff' :: String ->  IO [Diff]
queryDiff' dbName = do
    conn <- open dbName
    let allEntries = all_ (_diff auditorDb)
    entries <- runBeamSqlite conn $
               runSelectReturningList $ select allEntries
    close conn
    return entries

-- | Inserts new direct dependencies into the diff table.
updateDiffTableDirectDeps :: String -> [Package] -> IO ()
updateDiffTableDirectDeps dbName pkgs = do
    depsInDiff <- queryDiff dbName
    if all (== False ) [packageName x `elem` depsInDiff | x <- pkgs]
        then insertDiffDependencies dbName pkgs
        else print ("Already added new direct dependencies to the Diff table" :: String)

-- | Inserts new indirect dependencies into the diff table.
updateDiffTableIndirectDeps :: String -> [Package] -> IO ()
updateDiffTableIndirectDeps dbName pkgs = do
    depsInDiff <- queryDiff dbName
    if all (== False ) [packageName x `elem` depsInDiff | x <- pkgs]
        then insertDiffDependencies dbName pkgs
        else print ("Already added new indirect dependencies to the Diff table" :: String)

updateDiffTableRemovedDeps :: String -> IO ()
updateDiffTableRemovedDeps dbName = do
    rDeps <- removedDeps
    let rDepText = map pack rDeps -- TODO: Neaten this up
    depsInDiff <- queryDiff dbName
    if all (== False ) [x `elem` map unpack depsInDiff | x <- rDeps]
        then insertRemovedDependencies dbName rDepText True False
        -- TODO: Need to differentiate between direct and indirect removed deps
        else print ("Already added removed dependencies to the Diff table" :: String)

------------------- Agnostic Ops -------------------

-- | Build package/dependency list for the initialization of
-- the database.
buildPackageList :: [(PackageName, Version)]
                 -> [DirectDependency]
                 -> [IndirectDependency]
                 -> IO [Package]
buildPackageList pVersions dDeps indirDeps = do
    cTime <- getCurrentTime
    let directPackages = map (\x -> (Package
                                 (pack x)
                                 (lookupVersion x)
                                 cTime
                                 True
                                 True
                                 [])) dDeps

    let indirectPackages = map (\x -> (Package
                                   (pack x)
                                   (lookupVersion x)
                                   cTime
                                   False
                                   True
                                   [])) indirDeps
    return $ directPackages ++ indirectPackages
  where
    lookupVersion x = (pack $ fromMaybe "No version Found" (lookup x pVersions))

----------------------- Involves Both Auditor and Diff ------------------

-- | Compare an entry from the Diff table and Auditor table.
-- Return a modified `Auditor` that will be inserted into the auditor table.
compareDiffAuditor :: Diff -> Auditor -> Maybe Auditor
compareDiffAuditor (Diff _ dVersion _ _ diffStUsed _)
                   (Auditor aPkgName aVersion aDateFSeen aDirDep aStUsed aStat) =
    case (dVersion == aVersion, diffStUsed) of
        -- The version has changed and the package is still used.
        (False, "True") -> Just (Auditor aPkgName dVersion aDateFSeen aDirDep aStUsed aStat)
        -- The version has not changed and the pakcage is no longer used.
        (True, "False") -> Just (Auditor aPkgName aVersion aDateFSeen aDirDep "False" aStat)
        -- The version has not changed and the package is still used.
        (True, "True")  -> Just (Auditor aPkgName aVersion aDateFSeen aDirDep "True" aStat)
        _               -> Nothing

-- | Updates diff with removed dep. Queries auditor because
-- after the dep is removed from the repository,
-- `stack ls dependencies` can no longer get the version.
insertRemovedDependencies :: String -> [Text] -> Bool -> Bool -> IO ()
insertRemovedDependencies _ [] _ _ = pure ()
insertRemovedDependencies dbFilename (x:xs) dirOrIndir inYaml = do
    conn <- open dbFilename
    -- Queries auditor
    audQresult <- runBeamSqlite conn $ do
        let primaryKeyLookup = lookup_ (_auditor auditorDb)
        let sqlQuery = primaryKeyLookup $ AuditorPackageName x
        runSelectReturningOne sqlQuery
    -- Gets original time the package was first added.
    case audQresult of
        Nothing -> insertRemovedDependencies dbFilename xs dirOrIndir False
        Just result -> do
                         let dFSeen = _auditorDateFirstSeen result
                         case parseUTCTime dFSeen of
                             Right utcTime -> do
                                                insertPackageDiff dbFilename (Package
                                                    x
                                                    (_auditorPackageVersion result)
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
                                let primaryKeyLookup = lookup_ (_auditor auditorDb)
                                let sqlQuery = primaryKeyLookup $ AuditorPackageName x
                                runSelectReturningOne sqlQuery) diffDeps
    case all isNothing existingDeps of
        -- Inserts all the dependencies from the Diff table into the Auditor table
        True -> runBeamSqlite conn $ runInsert $
                    insert (_auditor auditorDb) $
                        insertFrom $ do
                            allEntries <- all_ (_diff auditorDb)
                            pure (Auditor (_diffPackageName allEntries)
                                          (_diffPackageVersion allEntries)
                                          (_diffDateFirstSeen allEntries)
                                          (_diffDirectDep allEntries)
                                          (_diffStillUsed allEntries)
                                          (_diffAnalysisStatus allEntries))
        -- There are deps in the Diff table that exists in the Auditor table
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
                  let primaryKeyLookup = lookup_ (_auditor auditorDb)
                  let sqlQuery = primaryKeyLookup $ AuditorPackageName x
                  runSelectReturningOne sqlQuery
    case audQresult of
        -- Either the version has changed or the dependency has been removed
        -- from the repo.
        Just audDep -> do
            -- Get dependency from Diff table
            diffQresult <- runBeamSqlite conn $ do
                           let primaryKeyLookup = lookup_ (_diff auditorDb)
                           let sqlQuery = primaryKeyLookup $ DiffPackageName x
                           runSelectReturningOne sqlQuery
            case diffQresult of
                Nothing -> updateOrModify xs
                Just diffDep ->   -- Compare Diff and Audit query; return the updated dependency information
                                  case compareDiffAuditor diffDep audDep of
                                      Nothing -> error "Error in compareDiffAuditor: This should not happen"
                                      Just depToIns -> do
                                          runBeamSqlite conn $ runDelete $
                                              delete (_auditor auditorDb)
                                                  (\table -> _auditorPackageName table ==. (val_ $ _diffPackageName diffDep))
                                          runBeamSqlite conn $ runInsert $
                                              insert (_auditor auditorDb) $
                                              insertValues [ depToIns ]
                                          updateOrModify xs
        -- New dependency to add to Auditor table.
        Nothing -> do
            diffList <- queryDiff' "auditor.db"
            case find (\diff -> _diffPackageName diff == x ) diffList of
                Nothing -> updateOrModify xs
                Just matchedDep -> do
                                     runBeamSqlite conn $ runInsert $
                                         insert (_auditor auditorDb) $
                                             insertFrom $
                                                 pure (Auditor (val_ $ _diffPackageName matchedDep)
                                                               (val_ $ _diffPackageVersion matchedDep)
                                                               (val_ $ _diffDateFirstSeen matchedDep)
                                                               (val_ $ _diffDirectDep matchedDep)
                                                               (val_ $ _diffStillUsed matchedDep)
                                                               (val_ $ _diffAnalysisStatus matchedDep))
                                     updateOrModify xs

------------------- Hash table operations -------------------


-- | Takes a hash and compares it to the database hash.
checkHash :: Int -> IO HashStatus
checkHash testHash = do
    entry <- queryHash
    case entry of
      Just dbH -> do
          let dbHashInt = _hashDotHash dbH
          if testHash == dbHashInt
              then liftIO $ return HashMatches
              else liftIO $ return HashDoesNotMatch
      Nothing -> liftIO $ return HashNotFound

-- | Query and returns the hash in db.
queryHash :: IO (Maybe Hash)
queryHash = do
    conn <- open "auditor.db"
    let allEntries = all_ (_hash auditorDb)
    entry <- runBeamSqlite conn $
             runSelectReturningOne $ select allEntries
    close conn
    return entry


