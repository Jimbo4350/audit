{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Database
       ( checkHash
       , clearDiffTable
       , deleteHash
       , initialAuditorTable
       , insertHash
       , insertPackageDiff
       , insertRemovedDependencies
       , loadDiffIntoAuditor
       , queryAuditor
       , queryDiff
       , queryDiff'
       , queryHash
       , updateDiffTableDirectDeps
       , updateDiffTableIndirectDeps
       , updateDiffTableRemovedDeps
       ) where

import           Data.Hashable          (hash)
import           Data.List              (all, find)
import           Data.Maybe             (fromJust, fromMaybe, isNothing)
import           Data.Text              (Text, pack, unpack)
import           Data.Time.Clock        (getCurrentTime)
import           Database.Beam          (Beamable, Columnar, Database,
                                         DatabaseSettings, Generic, Identity,
                                         PrimaryKey (..), Table (..),
                                         TableEntity, all_, defaultDbSettings,
                                         delete, insert, insertFrom,
                                         insertValues, liftIO, runDelete,
                                         runInsert, select)
import           Database.Beam.Query    (lookup_, runSelectReturningList,
                                         runSelectReturningOne, val_, (==.))
import           Database.Beam.Sqlite
import           Database.SQLite.Simple (close, open)
import           Sorting                (allOriginalRepoIndirDeps,
                                         allOriginalRepoVers,
                                         allUpdatedRepoVers, newDirDeps,
                                         newIndirectDeps, originalDirectDeps,
                                         removedDeps)
import           Types                  (HashStatus (..), Package (..))

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
type AuditorPackageName = PrimaryKey AuditorT Identity

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
type HashDotHash = PrimaryKey HashT Identity

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
type DiffPackageName = PrimaryKey DiffT Identity

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

-- | Takes a hash and compares it to the databse hash.
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

-- | Compare an entry from the Diff table and Auditor table.
-- Return a modified `Auditor` that will be inserted into the auditor table.
-- TODO: Matching on a string isn't particular nice....fix this.
compareDiffAuditor :: Diff -> Auditor -> Maybe Auditor
compareDiffAuditor (Diff _ dVersion _ _ diffStUsed _)
                   (Auditor aPkgName aVersion aDateFSeen aDirDep aStUsed aStat) =
    case (dVersion == aVersion, diffStUsed) of
        (False, "True") -> Just (Auditor aPkgName dVersion aDateFSeen aDirDep aStUsed aStat)
        (True, "False") -> Just (Auditor aPkgName aVersion aDateFSeen aDirDep "False" aStat)
        (True, "True")  -> Just (Auditor aPkgName aVersion aDateFSeen aDirDep "True" aStat)
        _               -> Nothing

-- | Delete the dependencies in the diff table.
clearDiffTable :: IO ()
clearDiffTable = do
    conn <- open "auditor.db"
    allDiffDeps <- queryDiff
    mapM_ (\x -> runBeamSqlite conn $ runDelete $
        delete (_diff auditorDb)
            (\c -> _diffPackageName c ==. val_ x)) allDiffDeps
    close conn

-- | Deletes the hash in the hash table.
deleteHash :: IO ()
deleteHash = do
    conn <- open "auditor.db"
    currentHash <- queryHash
    case currentHash of
        Just dbH ->  do
            let dbHashInt = _hashDotHash dbH
            runBeamSqlite conn $ runDelete $
                delete (_hash auditorDb)
                    (\table -> _hashDotHash table ==. val_ dbHashInt)
        Nothing -> print ("Hash not found in database" :: String)
    close conn

------------------- Original Database insertion -------------------

-- | Inserts direct and indirect dependencies into the sqlite db
-- and inserts a hash of the dot and txt files generated. This
-- represents the "original" or starting state of the repository.
initialAuditorTable :: IO ()
initialAuditorTable = do
    insertOriginalDepsAuditor
    (++) <$> readFile "repoinfo/currentDepTree.dot"
         <*> readFile "repoinfo/currentDepTreeVersions.txt" >>= insertHash . hash

-- | Inserts original direct & indirect dependencies into auditor table.
insertOriginalDepsAuditor :: IO ()
insertOriginalDepsAuditor = do
    pVersions <- allOriginalRepoVers
    dDeps <- originalDirectDeps
    indirectDeps <- allOriginalRepoIndirDeps
    cTime <- getCurrentTime
    -- Direct deps insertion
    mapM_ (\x -> insertPackageAuditor
                     (Package
                         (pack x)
                         (pack $ fromMaybe "No version Found" (lookup x pVersions))
                         cTime
                         True
                         True
                         [])) dDeps
    -- Indirect deps insertion
    mapM_ (\x -> insertPackageAuditor
                 (Package
                     (pack x)
                     (pack $ fromMaybe "No version Found" (lookup x pVersions))
                     cTime
                     False
                     True
                     [])) indirectDeps
  where
    -- Takes a `Package` and inserts it into the Auditor table.
    insertPackageAuditor :: Package -> IO ()
    insertPackageAuditor pkg = do
        conn <- open "auditor.db"
        runBeamSqlite conn $ runInsert $
            insert (_auditor auditorDb) $
            insertValues [ toAuditor pkg ]
        close conn

-- | Takes a hash and inserts it into the Hash table.
insertHash :: Int -> IO ()
insertHash dotHash = do
    conn <- open "auditor.db"
    runBeamSqlite conn $ runInsert $
        insert (_hash auditorDb) $
        insertValues [ Hash dotHash ]
    close conn

-- | Takes a newly added `Package` and inserts it into the Diff table.
insertPackageDiff :: Package -> IO ()
insertPackageDiff (Package pName pVersion dateFS dDep sUsed aStatus) = do
    conn <- open "auditor.db"
    runBeamSqlite conn $ runInsert $
        insert (_diff  auditorDb) $
        insertValues [ Diff
                           pName
                           pVersion
                           (pack $ show dateFS)
                           (pack $ show dDep)
                           (pack $ show sUsed)
                           (pack $ show aStatus)
                     ]
    close conn

-- | Updates diff with removed dep. Queries auditor because
-- after the dep is removed from the repository,
-- `stack ls dependencies` can no longer get the version.
insertRemovedDependencies :: [Text] -> Bool -> Bool -> IO ()
insertRemovedDependencies [] _ _ = pure ()
insertRemovedDependencies (x:xs) dirOrIndir inYaml = do
    conn <- open "auditor.db"
    audQresult <- runBeamSqlite conn $ do
        let primaryKeyLookup = lookup_ (_auditor auditorDb)
        let sqlQuery = primaryKeyLookup $ AuditorPackageName x
        runSelectReturningOne sqlQuery
    cTime <- getCurrentTime
    insertPackageDiff (Package
        x
        (_auditorPackageVersion $ fromJust audQresult)
        -- TODO: You are generating a new time here
        -- need to clarify if you should take the old time
        -- or generate a new time.
        cTime
        dirOrIndir
        inYaml
        [])
    close conn
    insertRemovedDependencies xs dirOrIndir inYaml

-- | Insert updated dependencies into a db table.
-- depList    = list of dependencies you want to insert.
-- dirOrIndir = Bool signlaing whether or not the depdencies you are
--              inserting are direct or indirect.
-- stillUsed  = Bool signaling whether or not the depencies you
--              are inserting are direct or indirt.
insertUpdatedDependencies :: [String] -> Bool -> Bool -> IO ()
insertUpdatedDependencies depList dirOrIndir inYaml = do
    cTime <- getCurrentTime
    pVersions <- allUpdatedRepoVers
    mapM_ (\x -> insertPackageDiff
        (Package
            (pack x)
            (pack $ fromMaybe "No version Found" (lookup x pVersions))
            cTime
            dirOrIndir
            inYaml
            [])) depList

loadDiffIntoAuditor :: IO ()
loadDiffIntoAuditor = do
    conn <- open "auditor.db"
    diffDeps <-  queryDiff
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

------------------- Database updates -------------------

-- | Inserts new direct dependencies into the diff table.
updateDiffTableDirectDeps :: IO ()
updateDiffTableDirectDeps = do
    dDeps <- newDirDeps
    depsInDiff <- queryDiff
    if all (== False ) [x `elem` map unpack depsInDiff | x <- dDeps]
        then insertUpdatedDependencies dDeps True True
        else print ("Already added new direct dependencies to the Diff table" :: String)

-- | Inserts new indirect dependencies into the diff table.
updateDiffTableIndirectDeps :: IO ()
updateDiffTableIndirectDeps = do
    newDeps <- newIndirectDeps
    depsInDiff <- queryDiff
    if all (== False ) [x `elem` map unpack depsInDiff | x <- newDeps]
        then insertUpdatedDependencies newDeps False True
        else print ("Already added new indirect dependencies to the Diff table" :: String)

updateDiffTableRemovedDeps :: IO ()
updateDiffTableRemovedDeps = do
    rDeps <- removedDeps
    let rDepText = map pack rDeps -- TODO: Neaten this up
    depsInDiff <- queryDiff
    if all (== False ) [x `elem` map unpack depsInDiff | x <- rDeps]
        then insertRemovedDependencies rDepText True False
        -- TODO: Need to differentiate between direct and indirect removed deps
        else print ("Already added removed dependencies to the Diff table" :: String)

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
            let diffDep = fromJust diffQresult
            -- Compare Diff and Audit query; return the updated dependency information
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
            diffList <- queryDiff'
            runBeamSqlite conn $ runInsert $
                insert (_auditor auditorDb) $
                    insertFrom $ do
                        -- TODO: Handle this fromJust properly
                        let matchedDep = fromJust $ find (\diff -> _diffPackageName diff == x ) diffList
                        pure (Auditor (val_ $ _diffPackageName matchedDep)
                                      (val_ $ _diffPackageVersion matchedDep)
                                      (val_ $ _diffDateFirstSeen matchedDep)
                                      (val_ $ _diffDirectDep matchedDep)
                                      (val_ $ _diffStillUsed matchedDep)
                                      (val_ $ _diffAnalysisStatus matchedDep))
            updateOrModify xs



-- | Returns all entries in the Auditor table.
queryAuditor :: IO [Auditor]
queryAuditor  = do
    conn <- open "auditor.db"
    let allEntries = all_ (_auditor auditorDb)
    entries <- runBeamSqlite conn $
        runSelectReturningList $ select allEntries
    close conn
    return entries

-- | Query and returns the hash in db.
queryHash :: IO (Maybe Hash)
queryHash = do
    conn <- open "auditor.db"
    let allEntries = all_ (_hash auditorDb)
    entry <- runBeamSqlite conn $
             runSelectReturningOne $ select allEntries
    close conn
    return entry

queryDiff :: IO [Text]
queryDiff  = do
    conn <- open "auditor.db"
    let allEntries = all_ (_diff auditorDb)
    entries <- runBeamSqlite conn $
               runSelectReturningList $ select allEntries
    close conn
    return $ map  _diffPackageName entries

queryDiff' :: IO [Diff]
queryDiff'  = do
    conn <- open "auditor.db"
    let allEntries = all_ (_diff auditorDb)
    entries <- runBeamSqlite conn $
               runSelectReturningList $ select allEntries
    close conn
    return entries


toAuditor :: Package -> Auditor
toAuditor (Package pName pVersion dateFS dDep sUsed aStatus) =
    Auditor
        pName
        pVersion
        (pack $ show dateFS)
        (pack $ show dDep)
        (pack $ show sUsed)
        (pack $ show aStatus)

