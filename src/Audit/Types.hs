module Audit.Types
       ( AnalysisStatus (..)
       , Command (..)
       , ConversionError (..)
       , DirectDependency
       , HashStatus (..)
       , IndirectDependency
       , OperationError (..)
       , OperationResult (..)
       , Package (..)
       , PackageName
       , QPResult(..)
       , Version
       ) where

import           Data.Text          (Text)
import           Data.Time.Clock    (UTCTime (..))


data AnalysisStatus
  = ASGhcBoot   -- Library is a GHC boot library shipped with GHC
  | ASCommon    -- Well know Haskell library with a well know Haskell maintainer
  | ASCritical  -- A library critical to the security/functioning of Cardano
                -- (eg its network facing)
  | ASCrypto    -- A crypto related lib
  | ASUncategoried
  | ASNewDependency
  deriving (Eq, Ord, Show, Read)

newtype Command = Command String

data ConversionError =
     UTCTimeParseError String
  |  DiffToAuditorError Text
  deriving Show

type DirectDependency = String

-- | Tells us if the hash of the existing .dot file matches
-- the hash of a newly generated .dot file or if the hash
-- exists in the db.
data HashStatus = HashMatches
                | HashDoesNotMatch
                | HashNotFound
                deriving Show

type IndirectDependency = String

data OperationError =
   OnlyDirectDepenciesAllowed [Package]
 | OnlyIndirectDepenciesAllowed [Package]
 | ConvError ConversionError
 deriving Show

data OperationResult =
    AlreadyAddedIndirectDependenciesDiff
    -- ^ Already added indirect dependencies to the Diff table.
  | AlreadyAddedDirectDependenciesDiff
    -- ^ Already added direct dependencies to the Diff table.
  | AlreadyAddedremovedDependenciesDiff
    -- ^ Already added removed dependencies to the Diff table.
  | AddedDirectDependenciesDiff
    -- ^ Successfully added direct dependencies to the Diff table.
  | AddedIndirectDependenciesDiff
    -- ^ Successfully added indirect dependencies to the Diff table.
  | NoDependenciesRemoved
    -- ^ No dependencies have been removed.
  | AddedRemovedDependenciesDiff
    -- ^ Successfully added removed dependencies to the Diff table.
  | AddedRemovedDependenciesAuditor
    -- ^ Successfully added removed dependencies to the Auditor table.
  | LoadedDiffTable
    -- ^ Successfully updated the diff tables with dependencies.
  | UpdatedExistingAuditorDepsWithDiffDeps
    -- ^ Successfully updated existing Auditor table entries with
    -- Diff table enteries
  | LoadedNewDepsFromDiffIntoAuditor
    -- ^ Successfully added new dependencies from the Diff table
    -- to the Auditor table.
  | NoDirectDependenciesToAdd
    -- ^ After parsing the '.dot' files no changes were detected i.e
    -- no new direct dependencies have been added.
  | NoIndirectDependenciesToAdd
    -- ^ After parsing the '.dot' files no changes were detected i.e
    -- no new indirect dependencies have been added.
  | NoRemovedDependenciesToAdd
    -- ^ After parsing the '.dot' files no changes were detected i.e
    -- no dependencies were removed.


  deriving Show

data Package = Package
    { packageName    :: Text
    , packageVersion :: Text
    , dateFirstSeen  :: UTCTime
    , directDep      :: Bool
    , stillUsed      :: Bool
    , analysisStatus :: [AnalysisStatus]
    } deriving (Eq, Ord, Show)


type PackageName = String

-- | 'QPResult a' captures the difference
-- between a database query of dependencies and
-- the dependencies from the parsing of
-- the '.dot' files in the repoinfo dir.
data QPResult a =
    QueryAndParseIdentical
    -- ^ Database query and parse result are the same
  | QPDifference [a]
    -- ^ Database query and parse result are different
  | QPParseIsEmpty
    -- ^ No changes detected between '.dot' files.
  deriving (Eq, Show)

type Version = String
