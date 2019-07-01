{-# LANGUAGE OverloadedStrings #-}

module Test.Audit.Gen
       ( genDirectPackage
       , genIndirectPackage
       , genHash
       , genNameVersions
       , genPackage
       , genPackageName
       , genPackageVersion
       , genRemovedPackage
       , genSimpleDepList
       , genUTCTime
       , populateAuditorTempDb
       , populateDiffTempDb
       ) where

import           Audit.Operations       (buildPackageList, insertAuditorDeps
                                         )
import           Audit.DiffOperations   (loadDiffTable)
import           Audit.Sorting          (groupParseResults)
import           Audit.Tree             (buildDepTree, directDeps, indirectDeps)
import           Audit.Types            (AnalysisStatus (..), Package (..))
import qualified Data.Text
import           Data.Time.Calendar     (Day (..))
import           Data.Time.Clock        (DiffTime (..), UTCTime (..),
                                         getCurrentTime, secondsToDiffTime)
import           Control.Monad.IO.Class (liftIO)
import           Hedgehog
import qualified Hedgehog.Gen           as Gen
import           Hedgehog.Internal.Gen  (generalize)
import qualified Hedgehog.Range         as Range


genAnalysisStatus :: Gen [AnalysisStatus]
genAnalysisStatus = Gen.list
  (Range.singleton 1)
  (Gen.element [ ASGhcBoot
               , ASCommon
               , ASCritical
               , ASCrypto
               , ASUncategoried
               , ASNewDependency
               ])

-- | Generate a direct dependency
genDirectPackage :: Gen Package
genDirectPackage = do
  pName <- genPackageName
  pVersion <- genPackageVersion
  pTime <- genUTCTime
  dDep <- pure True
  sUsed <- Gen.bool
  aStat <- genAnalysisStatus
  pure $ Package
           (Data.Text.pack pName)
           (Data.Text.pack pVersion)
           pTime
           dDep
           sUsed
           aStat

-- | Generate an indirect dependency
genIndirectPackage :: Gen Package
genIndirectPackage = do
  pName <- genPackageName
  pVersion <- genPackageVersion
  pTime <- genUTCTime
  dDep <- pure False
  sUsed <- Gen.bool
  aStat <- genAnalysisStatus
  pure $ Package
           (Data.Text.pack pName)
           (Data.Text.pack pVersion)
           pTime
           dDep
           sUsed
           aStat

genHash :: Gen Int
genHash = Gen.int Range.constantBounded

genNameVersions :: [String] -> Gen [(String,String)]
genNameVersions pNames = do
    pVersions <- Gen.list (Range.constant 3 20) genPackageVersion
    pure $ zip pNames pVersions

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

genSimpleDepList :: Gen [(String, [String])]
genSimpleDepList = do
    directDep <- genPackageName
    indirectDep <- genPackageName
    furthestDep <- genPackageName
    pure $ groupParseResults [("MainRepository", directDep), (directDep, indirectDep), (indirectDep, furthestDep)]


genUTCTime :: Gen UTCTime
genUTCTime = do
    diffTime <- Gen.integral (Range.linear 0 86401)
    day <- Gen.integral (Range.linear 0 1000000)
    pure $ UTCTime (ModifiedJulianDay day) (secondsToDiffTime diffTime)

-- | Note this does not create the database.
populateAuditorTempDb :: GenT IO [Package]
populateAuditorTempDb = do
  -- Generate packages with versions.
  xs <- generalize genSimpleDepList
  let dDeps = directDeps $ buildDepTree "MainRepository" xs
  let inDeps = indirectDeps $ buildDepTree "MainRepository" xs
  versions <- generalize $ genNameVersions (dDeps ++ inDeps)

  -- Populate auditor table with initial deps.
  cTime <- liftIO $ getCurrentTime
  let packages =  buildPackageList versions dDeps inDeps cTime
  liftIO $ insertAuditorDeps "temp.db" packages
  return packages

  -- | Note this does not create the database.
populateDiffTempDb :: GenT IO [Package]
populateDiffTempDb = do
  -- Generate packages with versions.
  xs <- generalize genSimpleDepList
  let dDeps = directDeps $ buildDepTree "MainRepository" xs
  let inDeps = indirectDeps $ buildDepTree "MainRepository" xs
  versions <- generalize $ genNameVersions (dDeps ++ inDeps)

  -- Populate auditor table with initial deps.
  cTime <- liftIO $ getCurrentTime
  let packages = buildPackageList versions dDeps inDeps cTime
  liftIO $ loadDiffTable "temp.db" packages
  return packages
