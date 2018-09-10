module Sorting
       ( tuplesToList
       , sortParseResults
       ) where

import           Data.List (groupBy)

-- | Sorts the results of the parser to [(package, [directDependencies])]
sortParseResults :: [(String, String)] -> [(String, [String])]
sortParseResults list = do
    let grouped = groupBy (\x y -> fst x == fst y) list
    [(fst $ head x, map snd x)| x <- grouped]

tuplesToList :: [(String, [String])] -> [String]
tuplesToList allDeps = do
    let tupleList = unzip allDeps
    fst tupleList ++ (concat $ snd tupleList)
