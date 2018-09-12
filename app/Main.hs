module Main where

import           Generate (createDB, createDeps, insertDB)
import           Sqlite   (queryAuditor)
import           Types    (HashStatus (..))

main :: IO ()
main = do
    hashStat <- createDB
    case hashStat of
        HashMatches -> print "Hashes match, dependancy tree has not been changed."
        HashDoesNotMatch -> print "Hashes do not match" --TODO: Add logic required for this.
        HashNotFound -> do
            print "Hash not found, generating db."
            createDeps
            insertDB
            putStrLn "Insert example package into auditor.db and query db"
            queryAuditor


