module Types
       ( Package (..)
       , AnalysisStatus (..)
       , HashStatus (..)
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
    } deriving Show

data AnalysisStatus
    = ASGhcBoot   -- Library is a GHC boot library shipped with GHC
    | ASCommon    -- Well know Haskell library with a well know Haskell maintainer
    | ASCritical  -- A library critical to the security/functioning of Cardano
                  -- (eg its network facing)
    | ASCrypto    -- A crypto related lib
    | ASUncategoried
    | ASNewDependency
    deriving Show

-- | Tells us if the hash of the existing .dot file matches
-- the hash of a newly generated .dot file or if the hash
-- exists in the db.
data HashStatus = HashMatches
                | HashDoesNotMatch
                | HashNotFound
                deriving Show
