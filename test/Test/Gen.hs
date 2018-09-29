module Test.Gen
       ( genNameVersion
       , genPackageName
       , genPackageVersion
       , genSimpleDepList
       ) where

import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import           Sorting        (groupParseResults)

genNameVersion :: Gen [(String,String)]
genNameVersion = do
    pNames <- Gen.list (Range.constant 3 20) genPackageName
    pVersions <- Gen.list (Range.constant 3 20) genPackageVersion
    pure $ zip pNames pVersions

genSimpleDepList :: Gen [(String, [String])]
genSimpleDepList = do
    directDep <- genPackageName
    indirectDep <- genPackageName
    pure $ groupParseResults [("MainRepository", directDep), (directDep, indirectDep)]

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
