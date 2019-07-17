module Audit.Tree
  ( buildDepTree
  , deconstructDepTree
  , depTreeLevel
  , directDeps
  , indirectDeps
  )
where

import           Data.List                      ( drop
                                                , nub
                                                , (\\)
                                                )
import           Data.Tree                      ( Forest
                                                , Tree(..)
                                                , levels
                                                )

-- Why Tree? To find the shortest path to a dependency.

buildDepTree :: String -> [(String, [String])] -> Tree String
buildDepTree sP ls = Node sP (buildDepForest sP ls)

-- | Builds a dependency tree given the parser results.
buildDepForest :: String -> [(String, [String])] -> Forest String
buildDepForest _            []   = []
buildDepForest startPackage list = case lookup startPackage list of
  Just sPsubDeps ->
      -- This will cycle through all the sub dependencies and
      -- ensure that the Tree is fully built. However, the
      -- startPackage that you enter must succeed otherwise
      -- the Tree will not be built.
    [ Node x (buildDepForest x list) | x <- sPsubDeps ]
  Nothing -> []

deconstructDepTree :: Tree String -> [(String, [String])]
deconstructDepTree (Node _ []     ) = []
deconstructDepTree (Node x subDeps) = (x, map extractDep subDeps)
  : deconstructDepForest subDeps
 where
  deconstructDepForest :: Forest String -> [(String, [String])]
  deconstructDepForest []     = []
  deconstructDepForest forest = concatMap deconstructDepTree forest

-- | Returns direct dependencies given a cross section
-- of the dependency tree denoted by Int.
depTreeLevel :: Int -> Tree String -> [(String, [String])]
depTreeLevel level (Node pName sub)
  | level == 0 = [(pName, map extractDep sub)]
  | otherwise  = concatMap (depTreeLevel (level - 1)) sub

-- | Returns direct dependencies given a dependency tree.
directDeps :: Tree String -> [String]
directDeps tree = concat . take 1 . drop 1 $ levels tree

-- | Returns indirect dependencies given a dependency tree.
indirectDeps :: Tree String -> [String]
indirectDeps tree = (nub . concat . drop 2 $ levels tree) \\ directDeps tree

extractDep :: Tree String -> String
extractDep (Node pName _) = pName
