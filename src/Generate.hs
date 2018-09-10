module Generate
       ( createDB
       , createDeps
       , streamDeps
       ) where

import           Control.Monad    (join)
import           Data.Either      (fromRight)
import           Data.List        (nub, (\\))
import           Data.Maybe       (fromMaybe)
import           Data.Text        (pack)
import           Data.Time.Clock  (getCurrentTime)
import           Parser           (allDependencies, packageName', versions)
import           Sorting          (sortParseResults, tuplesToList)
import           Sqlite           (insertPackage)
import           System.Directory (getDirectoryContents)
import           System.Process   (callCommand)
import           Text.Parsec      (parse)
import           Types            (Package (..))

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
    let indirectDeps = (nub $ tuplesToList allDeps) \\ directDeps
    cTime <- getCurrentTime
    mapM_ (\x -> insertPackage
                     (Package
                         (pack x)
                         (pack $ fromMaybe "No version Found" (lookup x pVersions))
                         cTime
                         False
                         True
                         [])) indirectDeps

-- | First prints [(package, dependencies)] then [(package, version)].
-- Inserts direct and indirect dependencies into sqlite db.
streamDeps :: IO ()
streamDeps = do
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
            print pName
            putStrLn ""
            insDirDeps (fromRight "Failure" pName) (sortParseResults pResult) vResult -- |TODO: Handle pName properly
            insIndirDeps (fromRight "Failure" pName) (sortParseResults pResult) vResult -- |TODO: Handle pName properly
            print $ sortParseResults pResult
            putStrLn ""
            print vResult

-- | Checks if "auditor.db" exists in pwd, if not creates it and the table `auditor`
createDB :: IO ()
createDB = do
    filenames <- getDirectoryContents "."
    if "auditor.db" `elem` filenames
        then print "auditor.db present"
        else callCommand "sqlite3 auditor.db \
           \\"CREATE TABLE auditor ( package_name VARCHAR NOT NULL\
                                  \, package_version VARCHAR NOT NULL\
                                  \, date_first_seen VARCHAR NOT NULL\
                                  \, direct_dep VARCHAR NOT NULL\
                                  \, still_used VARCHAR NOT NULL\
                                  \, analysis_status VARCHAR NOT NULL\
                                  \, PRIMARY KEY( package_name ));\""




