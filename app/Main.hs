module Main where

import           Data.List (intersperse)
import           Generate  (checkNewPackages, createDB, createDeps, insertDB)
import           Sqlite    (queryAuditor)
import           Types     (HashStatus (..))

main :: IO ()
main = do
    hashStat <- createDB
    case hashStat of
        HashMatches -> print "Hashes match, dependancy tree has not been changed."
        HashDoesNotMatch -> do
            print "Hashes do not match, dependency tree has changed."
            checkNewPackages >>= (\x -> print $ "New package(s): " ++ (concat $ intersperse ", " x))
        HashNotFound -> do
            print "Hash not found, generating db."
            createDeps
            insertDB
            putStrLn "Insert example package into auditor.db and query db."
            queryAuditor


