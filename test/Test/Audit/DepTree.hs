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
import           System.Process                 ( callCommand )


prop_simpleDeptree_construction_deconstruction :: Property
prop_simpleDeptree_construction_deconstruction = withTests 500 . property $ do
  xs <- forAll genSimpleDepList
  deconstructDepTree (buildDepTree "MainRepository" xs) === xs

prop_repoDeptree_construction_deconstruction :: Property
prop_repoDeptree_construction_deconstruction = withTests 1 . property $ do
  liftIO $ callCommand "stack dot --external > currentDepTree.dot"
  allDeps <- liftIO $ allInitialDepsGrouped "currentDepTree.dot"
  name    <- liftIO $ parseRepoName "currentDepTree.dot"
  liftIO $ callCommand "rm currentDepTree.dot"
  (sort . nub . deconstructDepTree $ buildDepTree name allDeps) === sort allDeps


tests :: IO Bool
tests = and <$> sequence [checkParallel $$discover]
