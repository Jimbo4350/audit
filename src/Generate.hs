module Generate
       ( allRepoDeps
       , allRepoVers
       , checkNewPackages
       , checkNewVersions
       , createDB
       , createDeps
       , directDeps
       , insertDB
       , insertNewDirectDB
       ) where

import           Data.Hashable    (hash)
import           Data.List        (nub, (\\))
import           Data.Maybe       (fromMaybe)
import           Data.Text        (pack)
import           Data.Time.Clock  (getCurrentTime)
import           Parser           (allDependencies, packageName', versions)
import           Sorting          (groupParseResults, tuplesToList)
import           Sqlite           (checkHash, insertAddedPackage, insertHash,
                                   insertPackage)
import           System.Directory (getDirectoryContents)
import           System.Process   (callCommand)
import           Text.Parsec      (parse)
import           Types            (DirectDependency, HashStatus (..),
                                   IndirectDependency, Package (..),
                                   PackageName, Version)

-- | Returns all the packages in the repo and their direct depdendencies.
-- The repo itself is considered a package. NB: The direct dependency in
-- the tuple is the direct dependency of the `PackageName` within that tuple.
allRepoDeps :: IO [(PackageName , DirectDependency)]
allRepoDeps = do
    pDeps <- parse allDependencies "" <$> readFile "repoinfo/gendeps.dot"
    case pDeps of
        Left parserError -> error $ show parserError
        Right deps       -> return deps

-- | Returns all the packages in the repo and their versions. Again
-- the repo itself is considered a package and is included. NB: The version
-- in the tuple is the version of the `PackageName` within that tuple.
allRepoVers :: IO [(PackageName, Version)]
allRepoVers = do
    pVers <- parse versions "" <$> readFile "repoinfo/depsVers.txt"
    case pVers of
        Left parserError -> error $ show parserError
        Right vers       -> return vers

createDeps :: IO ()
createDeps = do
    callCommand "stack dot --external > repoinfo/gendeps.dot"
    callCommand "stack ls dependencies > repoinfo/depsVers.txt"

-- | Returns direct depedencies of the repo.
directDeps :: IO [String]
directDeps = do
    rName <- repoName
    aDeps <- allRepoDeps
    let repoDirDeps = filter (\x -> rName == fst x) (groupParseResults aDeps)
    pure $ concatMap snd repoDirDeps


-- | Writes direct dependencies to the db.
insDirDeps :: [(PackageName, Version)] -> IO ()
insDirDeps pVersions  = do
    dDeps <- directDeps
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
    dDeps <- directDeps
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
    rDeps <- allRepoDeps
    rVersions <- allRepoVers
    insDirDeps rVersions
    insIndirDeps (groupParseResults rDeps) rVersions
    (++) <$> readFile "repoinfo/gendeps.dot"
         <*> readFile "repoinfo/depsVers.txt" >>= insertHash . hash


-- | Checks for new direct dependencies added to the cabal file.
checkNewPackages :: IO [(String, String)]
checkNewPackages = do
    oldDeps <- parse allDependencies "" <$> readFile "repoinfo/gendeps.dot" -- TODO: refactor
    newDeps <- parse allDependencies "" <$> readFile "repoinfo/gendepsUpdated.dot"
    case (newDeps, oldDeps) of
        (Right nDeps, Right oDeps) -> return $ nDeps \\ oDeps
        _                                       -> error "ParseFailure" --TODO: Handle error properly

-- | Checks for new versions added to the cabal file.
checkNewVersions :: IO [(String, String)]
checkNewVersions = do
    oldVersions <- parse versions "" <$> readFile "repoinfo/depsVers.txt" -- TODO: refactor
    newVersions <- parse versions "" <$> readFile "repoinfo/depsVersUpdated.txt"
    case (oldVersions, newVersions) of
        (Right oVersions, Right nVersions) -> return $ nVersions \\ oVersions
        _                                  -> error "ParseFailure" --TODO: Handle error properly

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

-- | Inserts new direct dependencies into the sqlite db.
insertNewDirectDB :: IO ()
insertNewDirectDB = do
    dDeps <- directDeps
    pVersions <- parse versions "" <$> readFile "repoinfo/depsVersUpdated.txt" -- TODO: refactor
    cTime <- getCurrentTime
    case pVersions of
        (Right versionsL) ->
            mapM_ (\x -> insertAddedPackage
                (Package
                    (pack x)
                    (pack $ fromMaybe "No version Found" (lookup x versionsL))
                    cTime
                    True
                    True
                    [])) dDeps
        _                            -> error "Parse Failure"    --TODO: Handle failure propery
{-
insertNewIndirectDB :: IO ()
insertNewIndirectDB = do
    newPs <- checkNewPackages
    pVersions <- parse versions "" <$> readFile "repoinfo/depsVersUpdated.txt"
    cTime <- getCurrentTime
    case pVersions of
        (Right versionsL) ->
            mapM_ (\x -> insertAddedPackage
                (Package
                    (pack x)
                    (pack $ fromMaybe "No version Found" (lookup x versionsL))
                    cTime
                    True
                    True
                    [])) newPs
        _                            -> error "Parse Failure"    --TODO: Handle failure propery

-}

repoName :: IO String
repoName = do
    name <- parse packageName' "" <$> readFile "repoinfo/gendeps.dot"
    case name of
        Left parserError -> error $ show parserError
        Right deps       -> return deps
