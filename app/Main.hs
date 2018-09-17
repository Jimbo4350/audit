module Main where

import           Generate (checkNewPackages, checkNewVersions,
                           checkRemovedPackages, createDB, createDeps, insertDB)
import           Sqlite   (queryAuditor)
import           Types    (HashStatus (..))

main :: IO ()
main = do
    hashStat <- createDB
    case hashStat of
        HashMatches -> print "Hashes match, dependancy tree has not been changed."
        HashDoesNotMatch -> do
            print "Hashes do not match, dependency tree has changed."
            checkNewPackages >>= (\x -> print $ "New package(s): " ++ (show x))
            checkNewVersions >>= (\x -> print $ "New package versions: " ++ (show x))
            checkRemovedPackages >>= (\x -> print $ "Removed packages: " ++ show x)
            -- TODO: Need to incorporate optparse-applicative to give the option to
            -- load new/removed pkgs (and changed versions)
        HashNotFound -> do
            print "Hash not found, generating db."
            createDeps
            insertDB
            putStrLn "Insert example package into auditor.db and query db."
            queryAuditor


