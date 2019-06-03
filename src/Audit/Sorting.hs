module Audit.Sorting
       ( allOriginalRepoDeps
       , allInitialDepsGrouped
       , allOriginalRepoIndirDeps
       , allOriginalRepoVers
       , allUpdatedRepoDeps
       , allUpdatedRepoVers
       , groupParseResults
       , initialDepTree
       , newIndirectDeps
       , newVersions
       , newDirDeps
       , originalDirectDeps
       , removedDeps
       , repoName
       , tuplesToList
       ) where

import           Data.Bifunctor (bimap)
import           Data.List      (groupBy, nub, (\\))
import           Data.Text      (Text, pack, unpack)
import           Data.Tree      (Tree)
import           Text.Parsec    (parse)

import           Audit.Parser   (allDependencies, packageName', versions)
import           Audit.Tree     (buildDepTree)
import           Audit.Types    (DirectDependency, IndirectDependency,
                                 PackageName, Version)

allInitialDepsGrouped :: IO [(PackageName, [DirectDependency])]
allInitialDepsGrouped = groupParseResults <$> allOriginalRepoDeps

-- | Returns all the packages in the repo and their direct depdendencies.
-- The repo itself is considered a package. NB: The direct dependency in
-- the tuple is the direct dependency of the `PackageName` within that tuple.
allOriginalRepoDeps :: IO [(PackageName , DirectDependency)]
allOriginalRepoDeps = do
    pDeps <- parse allDependencies "" <$> readFile "repoinfo/currentDepTree.dot"
    case pDeps of
        Left parserError -> error $ show parserError
        Right deps       -> return deps

allUpdatedRepoDeps :: IO [(PackageName , DirectDependency)]
allUpdatedRepoDeps = do
    pDeps <- parse allDependencies "" <$> readFile "repoinfo/updatedDepTree.dot"
    case pDeps of
        Left parserError -> error $ show parserError
        Right deps       -> return deps

allOriginalRepoIndirDeps :: IO [IndirectDependency]
allOriginalRepoIndirDeps = do
    allDeps <- allInitialDepsGrouped
    dDeps <- originalDirectDeps
    return $ nub (tuplesToList allDeps) \\ dDeps

-- | Returns all the packages in the repo and their versions. Again
-- the repo itself is considered a package and is included. NB: The version
-- in the tuple is the version of the `PackageName` within that tuple.
allOriginalRepoVers :: IO [(Text, Text)]
allOriginalRepoVers = do
    pVers <- parse versions "" <$> readFile "repoinfo/currentDepTreeVersions.txt"
    case pVers of
        Left parserError -> error $ show parserError
        Right vers       -> return [ bimap pack pack x | x <- vers ]

allUpdatedRepoVers :: IO [(PackageName, Version)]
allUpdatedRepoVers = do
    pVers <- parse versions "" <$> readFile "repoinfo/updatedDepTreeVersions.txt"
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
    oldVers' <- allOriginalRepoVers
    -- TODO: Thread `Text` through the repo
    let oldVers = [bimap unpack unpack x | x <- oldVers']
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

-- | Initial dependency tree (does not contain package versions)

initialDepTree :: IO (Tree String)
initialDepTree = do
    rName <- repoName
    iDeps <- allInitialDepsGrouped
    pure $ buildDepTree rName iDeps


-- | Returns direct depedencies of the repo before any changes were
-- made to the cabal file.
originalDirectDeps ::  IO [String]
originalDirectDeps = do
    rName <- repoName
    aDeps <- allOriginalRepoDeps
    let repoDirDeps = packageDependencies rName aDeps
    pure $ concatMap snd repoDirDeps

-- | Returns direct depedencies of the repo after any changes were
-- made to the cabal file.
updatedDirectDeps :: IO [String]
updatedDirectDeps = do
    rName <- repoName
    aDeps <- allUpdatedRepoDeps
    let repoDirDeps = packageDependencies rName aDeps
    pure $ concatMap snd repoDirDeps

------------------------------Helpers-----------------------------------

-- | Sorts the results of the parser to [(package, [directDependencies])]
groupParseResults :: [(String, String)] -> [(String, [String])]
groupParseResults list = do
    let grouped = groupBy (\x y -> fst x == fst y) list
    [(fst $ head x, map snd x)| x <- grouped]

packageDependencies :: String -> [(String, String)] -> [(String, [String])]
packageDependencies pkg depTree =
    filter (\x -> pkg == fst x) (groupParseResults depTree)

tuplesToList :: [(String, [String])] -> [String]
tuplesToList allDeps = do
    let tupleList = unzip allDeps
    fst tupleList ++ (concat $ snd tupleList)

repoName :: IO String
repoName = do
    name <- parse packageName' "" <$> readFile "repoinfo/currentDepTree.dot"
    case name of
        Left parserError -> error $ show parserError
        Right deps       -> return deps
