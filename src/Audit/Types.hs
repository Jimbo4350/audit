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

data ConversionError = UTCTimeParseError String deriving Show

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
 deriving Show

data OperationResult =
    AlreadyAddedDependenciesDiff
    -- ^ Already added dependencies to the Diff table.
  | AddedDependenciesDiff
    -- ^ Successfully added dependencies to the Diff table.
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
  deriving Show

type Version = String
