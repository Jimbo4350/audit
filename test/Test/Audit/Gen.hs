{-# LANGUAGE OverloadedStrings #-}

module Test.Audit.Gen
  ( genAnalysisStatus
  , genAuditor
  , genDirectParsedDependency
  , genIndirectParsedDependency
  , genHash
  , genNameVersions
  , genPackage
  , genPackageName
  , genDependencyVersion
  , genRemovedPackage
  , genSimpleDepList
  , genUTCTime
  , populateAuditorTempDb
  )
where

import           Audit.Database                 ( Auditor
                                                , AuditorT(..)
                                                )
import           Audit.Conversion               ( newParsedDeps )
import           Audit.Operations               ( insertAuditorDeps )
import           Audit.Sorting                  ( groupParseResults )
import           Audit.Tree                     ( buildDepTree
                                                , directDeps
                                                , indirectDeps
                                                )
import           Audit.Types                    ( AnalysisStatus(..)
                                                , Package(..)
                                                , ParsedDependency(..)
                                                )

import           Data.Text                      ( pack )
import           Data.Time.Calendar             ( Day(..) )
import           Data.Time.Clock                ( DiffTime(..)
                                                , UTCTime(..)
                                                , getCurrentTime
                                                , secondsToDiffTime
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.Either     ( runEitherT )
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import           Hedgehog.Internal.Gen          ( generalize )
import qualified Hedgehog.Range                as Range


genAnalysisStatus :: Gen [AnalysisStatus]
genAnalysisStatus = Gen.list
  (Range.singleton 1)
  (Gen.element
    [ASGhcBoot, ASCommon, ASCritical, ASCrypto, ASUncategoried, ASNewDependency]
  )

genAuditor :: Gen Auditor
genAuditor = do
  parDep <- Gen.choice [genDirectParsedDependency, genIndirectParsedDependency]
  pId    <- Gen.integral Range.constantBounded
  pure $ Auditor pId
                 (depName parDep)
                 (depVersion parDep)
                 (firstSeen parDep)
                 (isDirect parDep)
                 (inUse parDep)
                 (pack . show $ aStatus parDep)

-- | Generate a direct dependency
genDirectParsedDependency :: Gen ParsedDependency
genDirectParsedDependency = do
  pName    <- genPackageName
  pVersion <- genDependencyVersion
  pTime    <- genUTCTime
  dDep     <- pure True
  sUsed    <- Gen.bool
  aStat    <- genAnalysisStatus
  pure $ ParsedDependency (pack pName) (pack pVersion) pTime dDep sUsed aStat

-- | Generate an indirect dependency
genIndirectParsedDependency :: Gen ParsedDependency
genIndirectParsedDependency = do
  pName    <- genPackageName
  pVersion <- genDependencyVersion
  pTime    <- genUTCTime
  dDep     <- pure False
  sUsed    <- Gen.bool
  aStat    <- genAnalysisStatus
  pure $ ParsedDependency (pack pName) (pack pVersion) pTime dDep sUsed aStat

genHash :: Gen Int
genHash = Gen.int Range.constantBounded

genNameVersions :: [String] -> Gen [(String, String)]
genNameVersions pNames = do
  pVersions <- Gen.list (Range.constant 3 20) genDependencyVersion
  pure $ zip pNames pVersions

genPackageName :: Gen String
genPackageName = Gen.choice
  [concat <$> sequence [rString, Gen.constant "-", rString], rString]
  where rString = Gen.string (Range.constant 4 8) Gen.lower

genDependencyVersion :: Gen String
genDependencyVersion = concat <$> sequence
  [rInt, Gen.constant ".", rInt, Gen.constant ".", rInt, Gen.constant ".", rInt]
  where rInt = Gen.string (Range.constant 1 2) Gen.digit

genPackage :: Gen Package
genPackage = do
  pId      <- Gen.int32 Range.constantBounded
  pName    <- genPackageName
  pVersion <- genDependencyVersion
  pTime    <- genUTCTime
  dDep     <- Gen.bool
  sUsed    <- Gen.bool
  aStat    <- genAnalysisStatus
  pure $ Package pId (pack pName) (pack pVersion) pTime dDep sUsed aStat


genRemovedPackage :: Gen Package
genRemovedPackage = do
  pId      <- Gen.int32 Range.constantBounded
  pName    <- genPackageName
  pVersion <- genDependencyVersion
  pTime    <- genUTCTime
  dDep     <- Gen.bool
  aStat    <- genAnalysisStatus
  pure $ Package pId (pack pName) (pack pVersion) pTime dDep False aStat

genSimpleDepList :: Gen [(String, [String])]
genSimpleDepList = do
  directDep   <- genPackageName
  indirectDep <- genPackageName
  furthestDep <- genPackageName
  pure $ groupParseResults
    [ ("MainRepository", directDep)
    , (directDep       , indirectDep)
    , (indirectDep     , furthestDep)
    ]


genUTCTime :: Gen UTCTime
genUTCTime = do
  diffTime <- Gen.integral (Range.linear 0 86401)
  day      <- Gen.integral (Range.linear 0 1000000)
  pure $ UTCTime (ModifiedJulianDay day) (secondsToDiffTime diffTime)

-- | Note this does not create the database.
populateAuditorTempDb :: GenT IO [ParsedDependency]
populateAuditorTempDb = do
  -- Generate packages with versions.
  xs <- generalize genSimpleDepList
  let dDeps  = directDeps $ buildDepTree "MainRepository" xs
  let inDeps = indirectDeps $ buildDepTree "MainRepository" xs
  versions <- generalize $ genNameVersions (dDeps ++ inDeps)

  -- Populate auditor table with initial deps.
  cTime    <- liftIO getCurrentTime
  let packages = newParsedDeps versions dDeps inDeps cTime
  liftIO . runEitherT $ insertAuditorDeps "temp.db" packages
  return packages
