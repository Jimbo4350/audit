module Generate
       ( commandHandler
       , audit
       , createDB
       , createDeps
       , originalDirectDeps
       , insertDB
       , insertNewDirectDB
       , insertNewIndirectDB
       , update
       ) where

import           Data.Hashable    (hash)
import           Data.List        (nub, (\\))
import           Data.Maybe       (fromMaybe)
import           Data.Text        (pack)
import           Data.Time.Clock  (getCurrentTime)
import           Sorting          (allOriginalRepoDeps, allOriginalRepoVers,
                                   allUpdatedRepoVers, checkNewIndirectPackages,
                                   checkNewPackages, checkNewVersions,
                                   checkRemovedPackages, groupParseResults,
                                   originalDirectDeps, tuplesToList)
import           Sqlite           (checkHash, insertAddedPackage, insertHash,
                                   insertPackage, queryAuditor)
import           System.Directory (getDirectoryContents)
import           System.Process   (callCommand)
import           Types            (Command (..), HashStatus (..),
                                   IndirectDependency, Package (..),
                                   PackageName, Version)



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
insDirDeps :: [(PackageName, Version)] -> IO ()
insDirDeps pVersions  = do
    dDeps <- originalDirectDeps
    cTime <- getCurrentTime
    mapM_ (\x -> insertPackage
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
insIndirDeps
    :: [(PackageName, [IndirectDependency])]
    -> [(PackageName, Version)]
    -> IO ()
insIndirDeps allDeps pVersions = do
    dDeps <- originalDirectDeps
    let indirectDeps = nub (tuplesToList allDeps) \\ dDeps
    cTime <- getCurrentTime
    mapM_ (\x -> insertPackage
                     (Package
                         (pack x)
                         (pack $ fromMaybe "No version Found" (lookup x pVersions))
                         cTime
                         False
                         True
                         [])) indirectDeps

-- | Inserts direct and indirect dependencies into the sqlite db.
insertDB :: IO ()
insertDB = do
    rDeps <- allOriginalRepoDeps
    rVersions <- allOriginalRepoVers
    insDirDeps rVersions
    insIndirDeps (groupParseResults rDeps) rVersions
    (++) <$> readFile "repoinfo/gendeps.dot"
         <*> readFile "repoinfo/depsVers.txt" >>= insertHash . hash



-- | Inserts new direct dependencies into the sqlite db.
insertNewDirectDB :: IO ()
insertNewDirectDB = do
    dDeps <- checkNewPackages
    pVersions <- allUpdatedRepoVers
    cTime <- getCurrentTime
    mapM_ ((\x -> insertAddedPackage
        (Package
            (pack x)
            (pack $ fromMaybe "No version Found" (lookup x pVersions))
            cTime
            True
            True
            [])) . snd) dDeps

-- | Inserts new indirect dependencies into the sqlite db.
insertNewIndirectDB :: IO ()
insertNewIndirectDB = do
    newPs <- checkNewIndirectPackages
    pVersions <- allUpdatedRepoVers
    cTime <- getCurrentTime
    mapM_ (\x -> insertAddedPackage
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
                checkNewPackages >>= (\x -> print $ "New package(s): " ++ (show $ map snd x))
                checkNewVersions >>= (\x -> print $ "New package versions: " ++ show x)
                checkRemovedPackages >>= (\x -> print $ "Removed packages: " ++ show x)
                -- TODO: Need to incorporate optparse-applicative to give the option to
                -- load new/removed pkgs (and changed versions)
            HashNotFound -> do
                print "Hash not found, generating db."
                createDeps
                insertDB
                putStrLn "Insert example package into auditor.db and query db."
                queryAuditor

update :: IO ()
update = do
    insertNewDirectDB
    insertNewIndirectDB

