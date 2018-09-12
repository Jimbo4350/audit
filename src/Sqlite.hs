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
       , insertPackage
       , queryAuditor
       , queryHash
       ) where

import           Data.Text              (Text, pack)
import           Database.Beam          (Beamable, Columnar, Database,
                                         DatabaseSettings, Generic, Identity,
                                         PrimaryKey (..), Table (..),
                                         TableEntity, all_, defaultDbSettings,
                                         insert, insertValues, liftIO,
                                         runInsert, runSelectReturningList,
                                         runSelectReturningOne, select)
import           Database.Beam.Sqlite
import           Database.SQLite.Simple (close, open)
import           Types                  (Package (..), HashStatus (..))

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

-- | Database

data AuditorDb f = AuditorDb
    { _auditor :: f (TableEntity AuditorT)
    , _hash    :: f (TableEntity HashT)
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
insertPackage :: Package -> IO ()
insertPackage (Package pName pVersion dateFS dDep sUsed aStatus) = do
    conn <- open "auditor.db"
    runBeamSqliteDebug putStrLn {- for debug output -} conn $ runInsert $
        insert (_auditor auditorDb) $
        insertValues [ Auditor
                           pName
                           pVersion
                           (pack $ show dateFS)
                           (pack $ show dDep)
                           (pack $ show sUsed)
                           (pack $ show aStatus)
                     ]
    close conn

-- | Query's all enteries in the Auditor table.
queryAuditor :: IO ()
queryAuditor  = do
    conn <- open "auditor.db"
    let allEntries = all_ (_auditor auditorDb)
    runBeamSqliteDebug putStrLn conn $ do
      entries <- runSelectReturningList $ select allEntries
      mapM_ (liftIO . putStrLn . show) entries
    close conn

-- | Query and returns the hash in db.
queryHash :: IO (Maybe (HashT Identity))
queryHash = do
    conn <- open "auditor.db"
    let allEntries = all_ (_hash auditorDb)
    entry <- runBeamSqliteDebug putStrLn conn $
      runSelectReturningOne $ select allEntries
    close conn
    return entry
