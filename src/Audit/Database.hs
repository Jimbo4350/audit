{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Audit.Database
  ( Auditor
  , AuditorDb(..)
  , auditorDb
  , AuditorT(..)
  , Hash
  , HashT(..)
  , PrimaryKey(..)
  )
where

import Data.Text (Text)
import Data.Int (Int32)
import Data.Time.Clock (UTCTime)

import Database.Beam
import Database.Beam.Sqlite.Connection (Sqlite)

-- | Auditor Table. Stores the current direct, indirect and
-- removed dependencies.

data AuditorT f = Auditor
  { auditorDependencyId   :: Columnar f Int32
  , auditorPackageName    :: Columnar f Text
  , auditorPackageVersion :: Columnar f Text
  , auditorDateFirstSeen  :: Columnar f UTCTime
  , auditorDirectDep      :: Columnar f Bool
  , auditorStillUsed      :: Columnar f Bool
  , auditorAnalysisStatus :: Columnar f Text
  } deriving Generic

type Auditor = AuditorT Identity

deriving instance Eq Auditor
deriving instance Show Auditor

instance Beamable AuditorT

instance Table AuditorT where
  data PrimaryKey AuditorT f = AuditorDependencyId (Columnar f Int32) deriving Generic
  primaryKey = AuditorDependencyId . auditorDependencyId

instance Beamable (PrimaryKey AuditorT)

-- | Hash Table. Stores the hash of the original .dot file
-- and .txt file.

data HashT f = Hash
  { hashCurrentHash :: Columnar f Int
  } deriving Generic

type Hash = HashT Identity

deriving instance Eq Hash
deriving instance Show Hash

instance Beamable HashT

instance Table HashT where
  data PrimaryKey HashT f = HashId (Columnar f Int) deriving Generic
  primaryKey = HashId . hashCurrentHash

instance Beamable (PrimaryKey HashT)

-- | Database

data AuditorDb f = AuditorDb
  { auditor :: f (TableEntity AuditorT)
  , hash    :: f (TableEntity HashT)
  } deriving Generic

instance Database be AuditorDb

auditorDb :: DatabaseSettings Sqlite AuditorDb
auditorDb = defaultDbSettings
