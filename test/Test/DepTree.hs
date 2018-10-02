{-# LANGUAGE TemplateHaskell #-}

module Test.DepTree where

import           Hedgehog
import           Hedgehog.Internal.Property (Property, forAll, property,
                                             withTests, (===))
import           Sorting                    (allOriginalDepsGrouped, repoName)
import           Test.Gen                   (genSimpleDepList)
import           Tree                       (buildDepTree, deconstructDepTree)


prop_simpleDeptree_construction_deconstruction :: Property
prop_simpleDeptree_construction_deconstruction =
    withTests 1000 . property $ do
        xs <- forAll genSimpleDepList
        deconstructDepTree (buildDepTree "MainRepository" xs) === xs

tests :: IO Bool
tests = and <$> sequence
    [ checkParallel $$(discover) ]
