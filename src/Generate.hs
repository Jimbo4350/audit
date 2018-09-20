module Generate
       ( commandHandler
       , audit
       , createDB
       , createDeps
       , insertUpdatedDependencies
       , originalDirectDeps
       , populateAuditorTable
       , update
       , updateDiffTableDirectDeps
       , updateDiffTableIndirectDeps
       ) where

import           Data.Hashable    (hash)
import           Data.Maybe       (fromMaybe)
import           Data.Text        (pack, unpack)
import           Data.Time.Clock  (getCurrentTime)
import           Sorting          (allOriginalRepoIndirDeps,
                                   allOriginalRepoVers, allUpdatedRepoVers,
                                   newDirDeps, newIndirectDeps, newVersions,
                                   originalDirectDeps, removedDeps)
import           Sqlite           (checkHash, deleteDepsDiff, deleteHash,
                                   insertHash, insertPackageAuditor,
                                   insertPackageDiff, loadDiffIntoAuditor,
                                   queryDiff)
import           System.Directory (getDirectoryContents)
import           System.Process   (callCommand)
import           Types            (Command (..), HashStatus (..), Package (..))

-- | Checks if "auditor.db" exists in pwd, if not creates it with the
-- tables `auditor` (which holds all the dependency data) and `hash`
-- which holds the hash of the existing .dot file.
-- If "auditor.db" is present, it generates a new `.dot` file and checks
-- the hash of the existing `.dot` file against the new `.dot` file generated.
createDB :: IO HashStatus
createDB = do
    filenames <- getDirectoryContents "."
    if "auditor.db" `elem` filenames
        then do
             print "auditor.db present"
             print "Generating new dot file and checking hash"
             callCommand "stack dot --external > repoinfo/gendepsUpdated.dot"
             callCommand "stack ls dependencies > repoinfo/depsVersUpdated.txt"
             contents <- (++) <$> readFile "repoinfo/gendepsUpdated.dot" <*> readFile "repoinfo/depsVersUpdated.txt"
             checkHash $ hash contents
        else do
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
             return HashNotFound

createDeps :: IO ()
createDeps = do
    callCommand "stack dot --external > repoinfo/gendeps.dot"
    callCommand "stack ls dependencies > repoinfo/depsVers.txt"

-- | Writes direct dependencies to the db.
insDirDepsAuditor :: IO ()
insDirDepsAuditor = do
    pVersions <- allOriginalRepoVers
    dDeps <- originalDirectDeps
    cTime <- getCurrentTime
    mapM_ (\x -> insertPackageAuditor
                     (Package
                         (pack x)
                         (pack $ fromMaybe "No version Found" (lookup x pVersions))
                         cTime
                         True
                         True
                         [])) dDeps
    -- |TODO: stillUsed will have to be updated in the DB after
    -- 1) Generate the new stack dot file
    -- 2) Check all the direct dependencies against what
    --    currently exists in the database. Amend `stillUsed`
    --    as necessary. Default to `True` for now.
    --    Also default `[AnalysisStatus]` to [] for now.

-- | Writes indirect dependencies to the db.
insIndirDepsAuditor :: IO ()
insIndirDepsAuditor = do
    pVersions <- allOriginalRepoVers
    indirectDeps <- allOriginalRepoIndirDeps
    cTime <- getCurrentTime
    mapM_ (\x -> insertPackageAuditor
                     (Package
                         (pack x)
                         (pack $ fromMaybe "No version Found" (lookup x pVersions))
                         cTime
                         False
                         True
                         [])) indirectDeps

-- | Inserts direct and indirect dependencies into the sqlite db.
populateAuditorTable :: IO ()
populateAuditorTable = do
    insDirDepsAuditor
    insIndirDepsAuditor
    (++) <$> readFile "repoinfo/gendeps.dot"
         <*> readFile "repoinfo/depsVers.txt" >>= insertHash . hash



-- | Inserts new direct dependencies into the sqlite db.
updateDiffTableDirectDeps :: IO ()
updateDiffTableDirectDeps = do
    dDeps <- newDirDeps
    depsInDiff <- queryDiff
    if all (== False ) [x `elem` map unpack depsInDiff | x <- dDeps]
        then insertUpdatedDependencies dDeps True True
        else print "Already added new direct dependencies to the Diff table"

-- | Inserts new indirect dependencies into the sqlite db.
updateDiffTableIndirectDeps :: IO ()
updateDiffTableIndirectDeps = do
    newDeps <- newIndirectDeps
    depsInDiff <- queryDiff
    if all (== False ) [x `elem` map unpack depsInDiff | x <- newDeps]
        then insertUpdatedDependencies newDeps False True
        else print "Already added new indirect dependencies to the Diff table"

updateDiffTableRemovedDeps :: IO ()
updateDiffTableRemovedDeps = do
    rDeps <- removedDeps
    depsInDiff <- queryDiff
    if all (== False ) [x `elem` map unpack depsInDiff | x <- map snd rDeps]
        then insertRemovedDependencies (map snd rDeps) True False
        -- TODO: Need to differentiate between direct and indirect removed deps
        else print "Already added removed dependencies to the Diff table"

------------------------------Handler-----------------------------------

commandHandler :: Command -> IO ()
commandHandler (Command cmd)
    | cmd == "audit" = audit
    | cmd == "load" = update
    | otherwise = print "Invalid command!"

audit :: IO ()
audit = do
        hashStat <- createDB
        case hashStat of
            HashMatches -> print "Hashes match, dependancy tree has not been changed."
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
                createDeps
                populateAuditorTable

update :: IO ()
update = do
    print "Inserting changes from Diff table into Auditor table.."
    loadDiffIntoAuditor
    print "Overwriting original repo dependency tree & clearing Diff table"
    callCommand "stack dot --external > repoinfo/gendeps.dot"
    -- This will become the new hash
    contents <- (++) <$> readFile "repoinfo/gendepsUpdated.dot" <*> readFile "repoinfo/depsVersUpdated.txt"
    deleteHash
    -- Update hash in hash table
    insertHash $ hash contents
    -- Delete the new dependencies (that was just added to the auditor table)
    -- in the Diff table
    deleteDepsDiff
    callCommand "rm repoinfo/depsVersUpdated.txt"
    callCommand "rm repoinfo/gendepsUpdated.dot"


------------------------------Helpers-----------------------------------

-- | Insert updated dependencies into a db table.
-- depList    = list of dependencies you want to insert.
-- dirOrIndir = Bool signlaing whether or not the depdencies you are
--              inserting are direct or indirect.
-- stillUsed  = Bool signaling whether or not the depencies you
--              are inserting are direct or indirt.
insertUpdatedDependencies :: [String] -> Bool -> Bool -> IO ()
insertUpdatedDependencies depList dirOrIndir inYaml = do
    cTime <- getCurrentTime
    pVersions <- allUpdatedRepoVers
    mapM_ (\x -> insertPackageDiff
        (Package
            (pack x)
            (pack $ fromMaybe "No version Found" (lookup x pVersions))
            cTime
            dirOrIndir
            inYaml
            [])) depList

insertRemovedDependencies :: [String] -> Bool -> Bool -> IO ()
insertRemovedDependencies depList dirOrIndir inYaml = do
    cTime <- getCurrentTime
    pVersions <- allOriginalRepoVers
    mapM_ (\x -> insertPackageDiff
        (Package
            (pack x)
            (pack $ fromMaybe "No version Found" (lookup x pVersions))
            cTime
            dirOrIndir
            inYaml
            [])) depList
