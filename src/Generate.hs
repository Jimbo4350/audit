module Generate
       ( createDeps
       , streamDeps
       ) where

import           Parser         (mainPackages)
import           Sorting (sortingFunction)
import           System.Process (callCommand)
import           Text.Parsec    (parse)
import           Control.Monad.State.Lazy


createDeps :: IO ()
createDeps = callCommand "stack dot --external > gendeps.dot"

streamDeps :: IO ()
streamDeps = do
    result <- parse mainPackages "" <$> readFile "gendeps.dot"
    case result of
        Left pError-> error . show $ pError
        Right result -> print $ sortingFunction result
