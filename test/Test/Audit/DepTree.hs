{-# LANGUAGE TemplateHaskell #-}

module Test.Audit.DepTree where

import           Audit.Sorting                  ( allInitialDepsGrouped
                                                , parseRepoName
                                                )
import           Audit.Tree                     ( buildDepTree
                                                , deconstructDepTree
                                                )
import           Test.Audit.Gen                 ( genSimpleDepList )

import           Control.Monad.IO.Class
import           Data.List                      ( sort
                                                , nub
                                                )
import           Hedgehog
import           Hedgehog.Internal.Property     ( Property
                                                , forAll
                                                , property
                                                , withTests
                                                , (===)
                                                )


prop_simpleDeptree_construction_deconstruction :: Property
prop_simpleDeptree_construction_deconstruction = withTests 500 . property $ do
  xs <- forAll genSimpleDepList
  deconstructDepTree (buildDepTree "MainRepository" xs) === xs

prop_repoDeptree_construction_deconstruction :: Property
prop_repoDeptree_construction_deconstruction = withTests 1 . property $ do
  allDeps <- liftIO allInitialDepsGrouped
  name    <- liftIO parseRepoName
  (sort . nub . deconstructDepTree $ buildDepTree name allDeps) === sort allDeps


tests :: IO Bool
tests = and <$> sequence [checkParallel $$discover]
