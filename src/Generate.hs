module Generate
       ( createDB
       , createDeps
       , streamDeps
       ) where

import           Parser                   (mainPackages, versions)
import           Sorting                  (sortingFunction)
import           System.Directory         (getDirectoryContents)
import           System.Process           (callCommand)
import           Text.Parsec              (parse)


createDeps :: IO ()
createDeps = do
    callCommand "stack dot --external > gendeps.dot"
    callCommand "stack ls dependencies > depsVers.txt"

-- | First prints [(package, dependencies)] then [(package, version)].
streamDeps :: IO ()
streamDeps = do
    pDeps <- parse mainPackages "" <$> readFile "gendeps.dot"
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
            print $ sortingFunction pResult
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




