module Sorting
       ( allOriginalRepoDeps
       , allOriginalRepoIndirDeps
       , allOriginalRepoVers
       , allUpdatedRepoDeps
       , allUpdatedRepoVers
       , groupParseResults
       , newIndirectDeps
       , newVersions
       , newDirDeps
       , originalDirectDeps
       , removedDeps
       , repoName
       , tuplesToList
       ) where

import           Data.List   (nub, (\\), groupBy)
import           Parser      (allDependencies, packageName', versions)
import           Text.Parsec (parse)
import           Types       (DirectDependency, PackageName, Version)


-- | Returns all the packages in the repo and their direct depdendencies.
-- The repo itself is considered a package. NB: The direct dependency in
-- the tuple is the direct dependency of the `PackageName` within that tuple.
allOriginalRepoDeps :: IO [(PackageName , DirectDependency)]
allOriginalRepoDeps = do
    pDeps <- parse allDependencies "" <$> readFile "repoinfo/gendeps.dot"
    case pDeps of
        Left parserError -> error $ show parserError
        Right deps       -> return deps

allUpdatedRepoDeps :: IO [(PackageName , DirectDependency)]
allUpdatedRepoDeps = do
    pDeps <- parse allDependencies "" <$> readFile "repoinfo/gendepsUpdated.dot"
    case pDeps of
        Left parserError -> error $ show parserError
        Right deps       -> return deps

allOriginalRepoIndirDeps :: IO [PackageName]
allOriginalRepoIndirDeps = do
    allDeps <- groupParseResults <$> allOriginalRepoDeps
    dDeps <- originalDirectDeps
    return $ nub (tuplesToList allDeps) \\ dDeps

-- | Returns all the packages in the repo and their versions. Again
-- the repo itself is considered a package and is included. NB: The version
-- in the tuple is the version of the `PackageName` within that tuple.
allOriginalRepoVers :: IO [(PackageName, Version)]
allOriginalRepoVers = do
    pVers <- parse versions "" <$> readFile "repoinfo/depsVers.txt"
    case pVers of
        Left parserError -> error $ show parserError
        Right vers       -> return vers

allUpdatedRepoVers :: IO [(PackageName, Version)]
allUpdatedRepoVers = do
    pVers <- parse versions "" <$> readFile "repoinfo/depsVersUpdated.txt"
    case pVers of
        Left parserError -> error $ show parserError
        Right vers       -> return vers

-- | Returns all new indirect dependencies. NB: The hierarchy is lost and
-- packages in this list could be dependencies of each other.
newIndirectDeps :: IO [String]
newIndirectDeps = do
    aOrgDeps <- allOriginalRepoDeps
    oDirDeps <- originalDirectDeps
    let originalIndirDeps =  (nub . tuplesToList $ groupParseResults aOrgDeps) \\ oDirDeps
    aUpdDeps <- allUpdatedRepoDeps
    uDirDeps <- updatedDirectDeps
    let updatedIndirDeps =  (nub . tuplesToList $ groupParseResults aUpdDeps) \\ uDirDeps
    return $ updatedIndirDeps \\ originalIndirDeps

-- | Checks for new direct dependencies added to the cabal file.
newDirDeps :: IO [String]
newDirDeps = do
    rName <- repoName
    oldDeps <- allOriginalRepoDeps
    newDeps <- allUpdatedRepoDeps
    return . map snd $ filter (\x -> rName == fst x) newDeps \\ oldDeps

-- | Checks for new versions added to the cabal file.
newVersions :: IO [(String, String)]
newVersions = do
    rName <- repoName
    oldVers <- allOriginalRepoVers
    newVers <- allUpdatedRepoVers
    return $ filter (\x -> rName == fst x) newVers \\ oldVers

-- | Checks for direct dependencies that were removed.
--removedDeps :: IO [(String, String)]
removedDeps :: IO [String]
removedDeps = do
    rName <- repoName
    oldDeps <- allOriginalRepoDeps
    newDeps <- allUpdatedRepoDeps
    return . map snd $ filter (\x -> rName == fst x) (oldDeps \\ newDeps)

-- | Returns direct depedencies of the repo before any changes were
-- made to the cabal file.
originalDirectDeps :: IO [String]
originalDirectDeps = do
    rName <- repoName
    aDeps <- allOriginalRepoDeps
    let repoDirDeps = filter (\x -> rName == fst x) (groupParseResults aDeps)
    pure $ concatMap snd repoDirDeps

-- | Returns direct depedencies of the repo after any changes were
-- made to the cabal file.
updatedDirectDeps :: IO [String]
updatedDirectDeps = do
    rName <- repoName
    aDeps <- allUpdatedRepoDeps
    let repoDirDeps = filter (\x -> rName == fst x) (groupParseResults aDeps)
    pure $ concatMap snd repoDirDeps

------------------------------Helpers-----------------------------------

-- | Sorts the results of the parser to [(package, [directDependencies])]
groupParseResults :: [(String, String)] -> [(String, [String])]
groupParseResults list = do
    let grouped = groupBy (\x y -> fst x == fst y) list
    [(fst $ head x, map snd x)| x <- grouped]

tuplesToList :: [(String, [String])] -> [String]
tuplesToList allDeps = do
    let tupleList = unzip allDeps
    fst tupleList ++ (concat $ snd tupleList)

repoName :: IO String
repoName = do
    name <- parse packageName' "" <$> readFile "repoinfo/gendeps.dot"
    case name of
        Left parserError -> error $ show parserError
        Right deps       -> return deps

