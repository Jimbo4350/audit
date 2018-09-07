module Main where

import           Generate (createDB, createDeps, streamDeps)
import           Types (examplePackage)
import           Sqlite (insertAuditor, queryAuditor)

main :: IO ()
main = do
    createDB
    createDeps
    streamDeps
    putStrLn "Insert example package into auditor.db and query db"
    insertAuditor examplePackage
    queryAuditor


