{-# LANGUAGE OverloadedStrings #-}

module Test.Audit.Gen
       ( genNameVersions
       , genPackage
       , genPackageName
       , genPackageVersion
       , genRemovedPackage
       , genSimpleDepList
       ) where

import qualified Data.Text
import           Data.Time.Calendar (Day (..))
import           Data.Time.Clock    (DiffTime (..), UTCTime (..),
                                     secondsToDiffTime)
import           Hedgehog
import qualified Hedgehog.Gen       as Gen
import qualified Hedgehog.Range     as Range
import           Audit.Sorting      (groupParseResults)
import           Audit.Types        (AnalysisStatus (..), Package (..))

genNameVersions :: [String] -> Gen [(String,String)]
genNameVersions pNames = do
    pVersions <- Gen.list (Range.constant 3 20) genPackageVersion
    pure $ zip pNames pVersions

genSimpleDepList :: Gen [(String, [String])]
genSimpleDepList = do
    directDep <- genPackageName
    indirectDep <- genPackageName
    furthestDep <- genPackageName
    pure $ groupParseResults [("MainRepository", directDep), (directDep, indirectDep), (indirectDep, furthestDep)]

genPackageName :: Gen String
genPackageName =
    Gen.choice [ concat <$> sequence [rString, Gen.constant "-", rString]
               , rString
               ]
  where
    rString = Gen.string (Range.constant 4 8) Gen.lower

genPackageVersion :: Gen String
genPackageVersion = concat <$> sequence [rInt, Gen.constant ".", rInt, Gen.constant ".", rInt, Gen.constant ".", rInt]
  where
    rInt = Gen.string (Range.constant 1 2) Gen.digit

genPackage :: Gen Package
genPackage = do
    pName <- genPackageName
    pVersion <- genPackageVersion
    pTime <- genUTCTime
    dDep <- Gen.bool
    sUsed <- Gen.bool
    aStat <- genAnalysisStatus
    pure $ Package
               (Data.Text.pack pName)
               (Data.Text.pack pVersion)
               pTime
               dDep
               sUsed
               aStat

genRemovedPackage :: Gen Package
genRemovedPackage = do
    pName <- genPackageName
    pVersion <- genPackageVersion
    pTime <- genUTCTime
    dDep <- Gen.bool
    aStat <- genAnalysisStatus
    pure $ Package
               (Data.Text.pack pName)
               (Data.Text.pack pVersion)
               pTime
               dDep
               False
               aStat

genAnalysisStatus :: Gen [AnalysisStatus]
genAnalysisStatus = Gen.list (Range.singleton 1) (Gen.element [ ASGhcBoot
                                                              , ASCommon
                                                              , ASCritical
                                                              , ASCrypto
                                                              , ASUncategoried
                                                              , ASNewDependency
                                                              ])
genUTCTime :: Gen UTCTime
genUTCTime = do
    diffTime <- Gen.integral (Range.linear 0 1000000)
    day <- Gen.integral (Range.linear 0 1000000)
    pure $ UTCTime (ModifiedJulianDay day) (secondsToDiffTime diffTime)
