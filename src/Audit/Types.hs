module Audit.Types
       ( AnalysisStatus (..)
       , Command (..)
       , DirectDependency
       , HashStatus (..)
       , IndirectDependency
       , OperationError (..)
       , Package (..)
       , PackageName
       , QPResult(..)
       , Version
       ) where

import           Data.Text          (Text)
import           Data.Time.Clock    (UTCTime (..))

data Package = Package
    { packageName    :: Text
    , packageVersion :: Text
    , dateFirstSeen  :: UTCTime
    , directDep      :: Bool
    , stillUsed      :: Bool
    , analysisStatus :: [AnalysisStatus]
    } deriving (Eq, Ord, Show)

data AnalysisStatus
    = ASGhcBoot   -- Library is a GHC boot library shipped with GHC
    | ASCommon    -- Well know Haskell library with a well know Haskell maintainer
    | ASCritical  -- A library critical to the security/functioning of Cardano
                  -- (eg its network facing)
    | ASCrypto    -- A crypto related lib
    | ASUncategoried
    | ASNewDependency
    deriving (Eq, Ord, Show, Read)

-- | Tells us if the hash of the existing .dot file matches
-- the hash of a newly generated .dot file or if the hash
-- exists in the db.
data HashStatus = HashMatches
                | HashDoesNotMatch
                | HashNotFound
                deriving Show

newtype Command = Command String

type Version = String
type IndirectDependency = String
type DirectDependency = String
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

data OperationError =
   OnlyDirectDepenciesAllowed [Package]
 | OnlyIndirectDepenciesAllowed [Package]
 deriving Show
