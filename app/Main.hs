module Main where

import           Generate (createDB, createDeps, streamDeps)
import           Sqlite   (queryAuditor)

main :: IO ()
main = do
    createDB
    createDeps
    streamDeps
    putStrLn "Insert example package into auditor.db and query db"
    queryAuditor


