module Generate
       ( commandHandler
       , audit
       , createDB
       , createDeps
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
                                   checkNewIndirectPackages, checkNewPackages,
                                   checkNewVersions, checkRemovedPackages,
                                   originalDirectDeps)
import           Sqlite           (checkHash, insertHash, insertPackageAuditor,
                                   insertPackageDiff, queryAuditor, queryDiff)
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
    print "I'm here"

-- | Writes direct dependencies to the db.
insDirDepsAuditor :: IO ()
insDirDepsAuditor = do
    pVersions <- allOriginalRepoVers
    dDeps <- originalDirectDeps
    cTime <- getCurrentTime
    --insertDependencies dDeps insertPackageAuditor True
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
    dDeps <- checkNewPackages
    depsInDiff <- queryDiff
    -- TODO: Refactor using unless/when
    if all (== False ) [x `elem` map unpack depsInDiff | x <- map snd dDeps]
        then do
            pVersions <- allUpdatedRepoVers
            cTime <- getCurrentTime
            mapM_ ((\x -> insertPackageDiff
                (Package
                    (pack x)
                    (pack $ fromMaybe "No version Found" (lookup x pVersions))
                    cTime
                    True
                    True
                    [])) . snd) dDeps
            else print "Already added"

-- | Inserts new indirect dependencies into the sqlite db.
updateDiffTableIndirectDeps :: IO ()
updateDiffTableIndirectDeps = do
    newPs <- checkNewIndirectPackages
    pVersions <- allUpdatedRepoVers
    cTime <- getCurrentTime
    mapM_ (\x -> insertPackageDiff
        (Package
            (pack x)
            (pack $ fromMaybe "No version Found" (lookup x pVersions))
            cTime
            False
            True
            [])) newPs

------------------------------Handler-----------------------------------

commandHandler :: Command -> IO ()
commandHandler (Command cmd)
    | cmd == "audit" = audit
    | cmd == "insert" = update
    | otherwise = print "Invalid command!"

audit :: IO ()
audit = do
        hashStat <- createDB
        case hashStat of
            HashMatches -> print "Hashes match, dependancy tree has not been changed."
            HashDoesNotMatch -> do
                print "Hashes do not match, dependency tree has changed."
                checkNewPackages >>= (\x -> print $ "New dependency(s): " ++ (show $ map snd x))
                checkNewVersions >>= (\x -> print $ "New dependency versions: " ++ show x)
                checkRemovedPackages >>= (\x -> print $ "Removed dependencies: " ++ show x)
                updateDiffTableDirectDeps
                updateDiffTableIndirectDeps

                -- TODO: Need to incorporate optparse-applicative to give the option to
                -- load new/removed pkgs (and changed versions)
            HashNotFound -> do
                print "Hash not found, generating db."
                createDeps
                populateAuditorTable

update :: IO ()
update = print "Should update Auditor table with what's in the Diff table"

------------------------------Helpers-----------------------------------

-- | Insert dependencies into a db table.
-- depList    = list of dependencies you want to insert.
-- tableIns   = insertion function (changes depending on which table you
--              want to insert into)
-- dirOrIndir = Bool signlaing whether or not the depdencies you are
--              inserting are direct or indirect.
insertDependencies :: [String] -> (Package -> IO ()) -> Bool -> IO ()
insertDependencies depList tableIns dirOrIndir = do
    cTime <- getCurrentTime
    pVersions <- allUpdatedRepoVers
    mapM_ (\x -> tableIns
        (Package
            (pack x)
            (pack $ fromMaybe "No version Found" (lookup x pVersions))
            cTime
            dirOrIndir
            True
            [])) depList
