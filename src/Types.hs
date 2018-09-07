{-# LANGUAGE OverloadedStrings #-}

module Types
       ( Package (..)
       , AnalysisStatus (..)
       , examplePackage
       ) where

import           Data.Text          (Text)
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock    (UTCTime (..), secondsToDiffTime)

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

examplePackage :: Package
examplePackage =
    Package "testName" "testVersion" (UTCTime (fromGregorian 10 10 10) (secondsToDiffTime 10)) True True []
