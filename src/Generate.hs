module Generate
       ( createDB
       , createDeps
       , insertDB
       ) where

import           Control.Monad    (join)
import           Data.Either      (fromRight)
import           Data.Hashable    (hash)
import           Data.List        (nub, (\\))
import           Data.Maybe       (fromMaybe)
import           Data.Text        (pack)
import           Data.Time.Clock  (getCurrentTime)
import           Parser           (allDependencies, packageName', versions)
import           Sorting          (sortParseResults, tuplesToList)
import           Sqlite           (checkHash, insertHash, insertPackage)
import           System.Directory (getDirectoryContents)
import           System.Process   (callCommand)
import           Text.Parsec      (parse)
import           Types            (Package (..), HashStatus (..))

createDeps :: IO ()
createDeps = do
    callCommand "stack dot --external > gendeps.dot"
    callCommand "stack ls dependencies > depsVers.txt"

-- | Writes direct dependencies to the db.
insDirDeps :: String -> [(String,[String])] -> [(String,String)] -> IO ()
insDirDeps mainP allDeps pVersions  = do
    let directDeps = filter (\x -> fst x == mainP) allDeps
    cTime <- getCurrentTime
    mapM_ (\x -> insertPackage
                     (Package
                         (pack x)
                         (pack $ fromMaybe "No version Found" (lookup x pVersions))
                         cTime
                         True
                         True
                         [])) (join $ snd <$> directDeps)
    -- |TODO: stillUsed will have to be updated in the DB after
    -- 1) Generate the new stack dot file
    -- 2) Check all the direct dependencies against what
    --    currently exists in the database. Amend `stillUsed`
    --    as necessary. Default to `True` for now.
    --    Also default `[AnalysisStatus]` to [] for now.

-- | Writes indirect dependencies to the db.
insIndirDeps :: String -> [(String,[String])] -> [(String,String)] -> IO ()
insIndirDeps mainP allDeps pVersions = do
    let directDeps = tuplesToList $ filter (\x -> fst x == mainP) allDeps
    let indirectDeps = nub (tuplesToList allDeps) \\ directDeps
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
    pName <- parse packageName' "" <$> readFile "gendeps.dot"
    pDeps <- parse allDependencies "" <$> readFile "gendeps.dot"
    pVersions <- parse versions "" <$> readFile "depsVers.txt"
    case (pDeps, pVersions) of
        (Left pErrorDep , Left pErrorVer)  ->
            error ("Dependency parser:" ++ show pErrorDep ++ "Version parser: " ++ show pErrorVer)
        (Left pErrorDep , Right vResult)   -> do
            print pErrorDep
            print vResult
        (Right pResult , Left pErrorVer)   -> do
            print pErrorVer
            print pResult
        (Right pResult, Right vResult)     -> do
            insDirDeps
                (fromRight "Insert direct dependancies failure" pName)
                (sortParseResults pResult) vResult -- |TODO: Handle pName properly
            insIndirDeps
                (fromRight "Insert indirect dependencies failure" pName)
                (sortParseResults pResult) vResult -- |TODO: Handle pName properly
            readFile "gendeps.dot" >>= insertHash . hash

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
             callCommand "stack dot --external > gendepsUpdated.dot"
             let updatedHash = hash <$> readFile "gendepsUpdated.dot"
             updatedHash >>= checkHash
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
                                 \, PRIMARY KEY ( dot_hash )); \""
             return HashNotFound


