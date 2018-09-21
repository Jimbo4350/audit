{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Sqlite
       ( checkHash
       , deleteDepsDiff
       , deleteHash
       , insertHash
       , insertPackageAuditor
       , insertPackageDiff
       , loadDiffIntoAuditor
       , queryAuditor
       , queryDiff
       , queryDiff'
       , queryHash
       ) where

import           Data.List              (all, find)
import           Data.Maybe             (fromJust, isNothing)
import           Data.Text              (Text, pack)
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
import           Types                  (HashStatus (..), Package (..))

-- | Auditor Table

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

-- | Hash Table

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
-- stores any changes to the dependency tree

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

-- | Compare an entry from the Diff table and Auditor table
-- TODO: Matching on a string isn't particular nice....fix this.
compareDiffAuditor :: Diff -> Auditor -> Maybe Auditor
compareDiffAuditor (Diff _ dVersion _ _ diffStUsed _)
                   (Auditor aPkgName aVersion aDateFSeen aDirDep aStUsed aStat) =
    case (dVersion == aVersion, diffStUsed, aStUsed) of
        (False, "True", "True") -> Just (Auditor aPkgName dVersion aDateFSeen aDirDep aStUsed aStat)
        (True, "False", "True") -> Just (Auditor aPkgName aVersion aDateFSeen aDirDep "False" aStat)
        _                       -> Nothing

-- | Delete the dependencies in the diff table.
deleteDepsDiff :: IO ()
deleteDepsDiff = do
    conn <- open "auditor.db"
    deps <- queryDiff
    mapM_ (\x -> runBeamSqlite conn $ runDelete $
        delete (_diff auditorDb)
            (\c -> _diffPackageName c ==. val_ x)) deps
    close conn

-- | Takes a hash and inserts it into the Hash table.
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
        Nothing -> print "Hash not found in database"
    close conn

-- | Takes a hash and inserts it into the Hash table.
insertHash :: Int -> IO ()
insertHash dotHash = do
    conn <- open "auditor.db"
    runBeamSqlite conn $ runInsert $
        insert (_hash auditorDb) $
        insertValues [ Hash dotHash ]
    close conn

-- | Takes a `Package` and inserts it into the Auditor table.
insertPackageAuditor :: Package -> IO ()
insertPackageAuditor pkg = do
    conn <- open "auditor.db"
    runBeamSqlite conn $ runInsert $
        insert (_auditor auditorDb) $
        insertValues [ toAuditor pkg ]
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

loadDiffIntoAuditor :: IO ()
loadDiffIntoAuditor = do
    conn <- open "auditor.db"
    diffDeps <-  queryDiff
    -- Check to see if the dependency in Diff exists in Auditor
    -- If it does, indicates either a version change or dependency removal
    -- Returns a list of Maybes
    {-
    existingDeps <- runBeamSqlite conn $ do
                        let primaryKeyLookup = lookup_ (_auditor auditorDb)
                        let sqlQuery = map (primaryKeyLookup . AuditorPackageName) diffDeps
                        mapM runSelectReturningOne sqlQuery
    -}
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
    {-
    mapM_ (\x -> runBeamSqlite conn $ runDelete $
                     delete (_auditor auditorDb)
                         (\c -> _auditorPackageName c ==. val_ x)) updatedDeps
                         -}
    -- Inserts the updated dependencies from the Diff table to the Auditor table
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
            let diffDep = fromJust diffQresult
            -- Compare Diff and Audit query; return the updated dependency information
            case compareDiffAuditor diffDep audDep of
                -- TODO: No version found is why this case is being matched
                Nothing -> error "Error in compareDiffAuditor: This should not happen"
                Just depToIns -> do
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

