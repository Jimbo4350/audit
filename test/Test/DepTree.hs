{-# LANGUAGE TemplateHaskell #-}

module Test.DepTree where

import           Control.Monad.IO.Class
import           Data.List                  (sort, nub)
import           Hedgehog
import           Hedgehog.Internal.Property (Property, forAll, property,
                                             withTests, (===))
import           Sorting                    (allOriginalDepsGrouped, repoName)
import           Test.Gen                   (genSimpleDepList)
import           Tree                       (buildDepTree, deconstructDepTree)


prop_simpleDeptree_construction_deconstruction :: Property
prop_simpleDeptree_construction_deconstruction =
    withTests 500 . property $ do
        xs <- forAll genSimpleDepList
        deconstructDepTree (buildDepTree "MainRepository" xs) === xs

prop_repoDeptree_construction_deconstruction :: Property
prop_repoDeptree_construction_deconstruction =
    withTests 1 . property $ do
        allDeps <- liftIO allOriginalDepsGrouped
        name <- liftIO repoName
        (sort . nub . deconstructDepTree $ buildDepTree name allDeps) === sort allDeps

tests :: IO Bool
tests = and <$> sequence
    [ checkParallel $$discover ]
