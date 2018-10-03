module Generate
       ( commandHandler
       , audit
       , originalDirectDeps
       , update
       ) where

import           Data.Hashable    (hash)
import           Database         (checkHash, clearDiffTable, deleteHash,
                                   initialAuditorTable, insertHash,
                                   loadDiffIntoAuditor, queryDiff',
                                   updateDiffTableDirectDeps,
                                   updateDiffTableIndirectDeps,
                                   updateDiffTableRemovedDeps)
import           Sorting          (newDirDeps, newVersions, originalDirectDeps,
                                   removedDeps)
import           System.Directory (getDirectoryContents)
import           System.Process   (callCommand)
import           Types            (Command (..), HashStatus (..))

-- | Checks if "auditor.db" exists in pwd, if not creates it with the
-- tables `auditor` (which holds all the dependency data) and `hash`
-- which holds the hash of the existing .dot file.
-- If "auditor.db" is present, it generates a new `.dot` file and checks
-- the hash of the existing `.dot` file against the new `.dot` file generated.
checkDB :: IO HashStatus
checkDB = do
    filenames <- getDirectoryContents "."
    if "auditor.db" `elem` filenames
        then do
             print "auditor.db present"
             print "Generating new dot file and checking hash"
             callCommand "stack dot --external > repoinfo/updatedDepTree.dot"
             callCommand "stack ls dependencies > repoinfo/updatedDepTreeVersions.txt"
             contents <- (++) <$> readFile "repoinfo/updatedDepTree.dot" <*> readFile "repoinfo/updatedDepTreeVersions.txt"
             checkHash $ hash contents
        else return HashNotFound

generateInitialDepFiles :: IO ()
generateInitialDepFiles = do
    callCommand "stack dot --external > repoinfo/currentDepTree.dot"
    callCommand "stack ls dependencies > repoinfo/currentDepTreeVersions.txt"

initializeDB :: IO ()
initializeDB =
    callCommand "sqlite3 auditor.db \
    \\"CREATE TABLE auditor ( package_name VARCHAR NOT NULL\
                           \, package_version VARCHAR NOT NULL\
                           \, date_first_seen VARCHAR NOT NULL\
                           \, direct_dep VARCHAR NOT NULL\
                           \, still_used VARCHAR NOT NULL\
                           \, analysis_status VARCHAR NOT NULL\
                           \, PRIMARY KEY( package_name )); \
     \CREATE TABLE hash ( dot_hash INT NOT NULL\
                           \, PRIMARY KEY ( dot_hash )); \
     \CREATE TABLE diff ( package_name VARCHAR NOT NULL\
                           \, package_version VARCHAR NOT NULL\
                           \, date_first_seen VARCHAR NOT NULL\
                           \, direct_dep VARCHAR NOT NULL\
                           \, still_used VARCHAR NOT NULL\
                           \, analysis_status VARCHAR NOT NULL\
                           \, PRIMARY KEY( package_name )); \""


------------------------------Handler-----------------------------------

commandHandler :: Command -> IO ()
commandHandler (Command cmd)
    | cmd == "audit" = audit
    | cmd == "load" = update
    | otherwise = print "Invalid command!"

audit :: IO ()
audit = do
        hashStat <- checkDB
        case hashStat of
            HashMatches -> print "Hashes match, dependency tree has not \
                                 \been changed."
            HashDoesNotMatch -> do
                print "Hashes do not match, dependency tree has changed."
                newDirDeps >>= (\x -> print $ "New dependencies: " ++ show x)
                newVersions >>= (\x -> print $ "New dependency versions: " ++ show x)
                removedDeps >>= (\x -> print $ "Removed dependencies: " ++ show x)
                updateDiffTableDirectDeps
                updateDiffTableIndirectDeps
                updateDiffTableRemovedDeps
            HashNotFound -> do
                print "Hash not found, generating db."
                initializeDB
                generateInitialDepFiles
                initialAuditorTable "auditor.db"

update :: IO ()
update = do
    entries <- queryDiff'
    case entries of
        [] -> print "Diff table empty"
        _ -> do
                  print "Inserting changes from Diff table into \
                        \Auditor table.."
                  loadDiffIntoAuditor "auditor.db"
                  print "Overwriting original repository dependency \
                        \tree & clearing Diff table"
                  callCommand "stack dot --external > repoinfo/currentDepTree.dot"
                  print "Deleting old hash."
                  deleteHash
                  -- `contents` will become the new hash
                  contents <- (++) <$> readFile "repoinfo/updatedDepTree.dot"
                                   <*> readFile "repoinfo/updatedDepTreeVersions.txt"
                  -- Update hash in hash table
                  insertHash "auditor.db" $ hash contents
                  -- Delete the new dependencies (that was just added to the auditor table)
                  -- in the Diff table
                  clearDiffTable "auditor.db"
                  callCommand "rm repoinfo/updatedDepTreeVersions.txt"
                  callCommand "rm repoinfo/updatedDepTree.dot"
