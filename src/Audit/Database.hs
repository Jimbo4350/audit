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
  , Diff
  , DiffT(..)
  , Hash
  , HashT(..)
  , PrimaryKey(..)
  )
where

import Data.Text (Text)
import Data.Int (Int32)

import Database.Beam
import Database.Beam.Sqlite.Connection (Sqlite)

-- | Auditor Table. Stores the current direct, indirect and
-- removed dependencies.

data AuditorT f = Auditor
  { auditorDependencyId   :: Columnar f Int32
  , auditorPackageName    :: Columnar f Text
  , auditorPackageVersion :: Columnar f Text
  , auditorDateFirstSeen  :: Columnar f Text
  , auditorDirectDep      :: Columnar f Text
  , auditorStillUsed      :: Columnar f Text
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

-- | Dependency difference table. This table
-- stores any changes to the dependency tree.

data DiffT f = Diff
  { diffDependencyId   :: Columnar f Int32
  , diffPackageName    :: Columnar f Text
  , diffPackageVersion :: Columnar f Text
  , diffDateFirstSeen  :: Columnar f Text
  , diffDirectDep      :: Columnar f Text
  , diffStillUsed      :: Columnar f Text
  , diffAnalysisStatus :: Columnar f Text
  } deriving Generic

type Diff = DiffT Identity

deriving instance Ord Diff
deriving instance Eq Diff
deriving instance Show Diff

instance Beamable DiffT

instance Table DiffT where
  data PrimaryKey DiffT f = DiffDependencyId (Columnar f Int32) deriving Generic
  primaryKey = DiffDependencyId . diffDependencyId


instance Beamable (PrimaryKey DiffT)

-- | Database

data AuditorDb f = AuditorDb
  { auditor :: f (TableEntity AuditorT)
  , hash    :: f (TableEntity HashT)
  , diff    :: f (TableEntity DiffT)
  } deriving Generic

instance Database be AuditorDb

auditorDb :: DatabaseSettings Sqlite AuditorDb
auditorDb = defaultDbSettings
