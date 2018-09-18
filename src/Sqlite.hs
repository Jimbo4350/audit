{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Sqlite
       ( checkHash
       , insertHash
       , insertPackageAuditor
       , insertPackageDiff
       , queryAuditor
       , queryDiff
       , queryHash
       ) where

import           Data.Text              (Text, pack)
import           Database.Beam          (Beamable, Columnar, Database,
                                         DatabaseSettings, Generic, Identity,
                                         PrimaryKey (..), Table (..),
                                         TableEntity, all_, defaultDbSettings,
                                         insert, insertValues, liftIO,
                                         runInsert, runSelectReturningOne,
                                         select)
import           Database.Beam.Query    (runSelectReturningList)
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


-- | Takes a hash and inserts it into the Hash table.
insertHash :: Int -> IO ()
insertHash dotHash = do
    conn <- open "auditor.db"
    runBeamSqliteDebug putStrLn {- for debug output -} conn $ runInsert $
        insert (_hash auditorDb) $
        insertValues [ Hash dotHash ]
    close conn

-- | Takes a `Package` and inserts it into the Auditor table.
insertPackageAuditor :: Package -> IO ()
insertPackageAuditor pkg = do
    conn <- open "auditor.db"
    runBeamSqliteDebug putStrLn {- for debug output -} conn $ runInsert $
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

updateAuditor :: Package -> IO ()
updateAuditor pkg = do
    conn <- open "auditor.db"
    runBeamSqliteDebug putStrLn {- for debug output -} conn $ runInsert $
        insert (_auditor  auditorDb) $
        insertValues [ toAuditor pkg ]
    close conn

-- | Returns all enteries in the Auditor table.
queryAuditor :: IO [Auditor]
queryAuditor  = do
    conn <- open "auditor.db"
    let allEntries = all_ (_auditor auditorDb)
    entries <- runBeamSqliteDebug putStrLn conn $
        runSelectReturningList $ select allEntries
    close conn
    return entries

-- | Query and returns the hash in db.
queryHash :: IO (Maybe Hash)
queryHash = do
    conn <- open "auditor.db"
    let allEntries = all_ (_hash auditorDb)
    entry <- runBeamSqliteDebug putStrLn conn $
             runSelectReturningOne $ select allEntries
    close conn
    return entry

queryDiff:: IO [Text]
queryDiff  = do
    conn <- open "auditor.db"
    let allEntries = all_ (_diff auditorDb)
    entries <- runBeamSqliteDebug putStrLn conn $
               runSelectReturningList $ select allEntries
    close conn
    return $ map  _diffPackageName entries

toAuditor :: Package -> Auditor
toAuditor (Package pName pVersion dateFS dDep sUsed aStatus) =
    Auditor
        pName
        pVersion
        (pack $ show dateFS)
        (pack $ show dDep)
        (pack $ show sUsed)
        (pack $ show aStatus)