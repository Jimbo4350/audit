module Sorting
       ( sortingFunction
       ) where

import Data.List (groupBy)

-- | Sorts the results of the parser to [(package, [directDependencies])]
sortingFunction :: [(String, String)] -> [(String, [String])]
sortingFunction list = do
    let grouped = groupBy (\x y -> fst x == fst y) list
    [(fst $ head x, map snd x)| x <- grouped]


