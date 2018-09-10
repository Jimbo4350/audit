{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Sqlite
       ( insertPackage
       , queryAuditor
       ) where

import           Data.Text              (Text, pack)
import           Database.Beam
import           Database.Beam.Sqlite
import           Database.SQLite.Simple (close, open)
import           Types                  (Package (..))

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

-- | Database

data AuditorDb f = AuditorDb
    { _auditor :: f (TableEntity AuditorT)
    } deriving Generic

instance Database be AuditorDb

auditorDb :: DatabaseSettings be AuditorDb
auditorDb = defaultDbSettings

-- | SQL queries etc

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

-- | Queries all enteries in the Auditor table.
queryAuditor :: IO ()
queryAuditor  = do
    conn <- open "auditor.db"
    let allEntries = all_ (_auditor auditorDb)
    runBeamSqliteDebug putStrLn conn $ do
      entries <- runSelectReturningList $ select allEntries
      mapM_ (liftIO . putStrLn . show) entries
    close conn
