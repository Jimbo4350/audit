module Sorting
       ( allOriginalRepoDeps
       , allOriginalRepoVers
       , allUpdatedRepoDeps
       , allUpdatedRepoVers
       , checkNewIndirectPackages
       , checkNewPackages
       , checkNewVersions
       , checkRemovedPackages
       , groupParseResults
       , originalDirectDeps
       , repoName
       , tuplesToList
       ) where

import           Data.List   (groupBy)
import           Data.List   (nub, (\\))
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
checkNewIndirectPackages :: IO [String]
checkNewIndirectPackages = do
    aOrgDeps <- allOriginalRepoDeps
    oDirDeps <- originalDirectDeps
    let originalIndirDeps =  (nub . tuplesToList $ groupParseResults aOrgDeps) \\ oDirDeps
    aUpdDeps <- allUpdatedRepoDeps
    uDirDeps <- updatedDirectDeps
    let updatedIndirDeps =  (nub . tuplesToList $ groupParseResults aUpdDeps) \\ uDirDeps
    return $ updatedIndirDeps \\ originalIndirDeps

-- | Checks for new direct dependencies added to the cabal file.
checkNewPackages :: IO [(String, String)]
checkNewPackages = do
    rName <- repoName
    oldDeps <- parse allDependencies "" <$> readFile "repoinfo/gendeps.dot" -- TODO: refactor
    newDeps <- parse allDependencies "" <$> readFile "repoinfo/gendepsUpdated.dot"
    case (newDeps, oldDeps) of
        (Right nDeps, Right oDeps) -> return $ filter (\x -> rName == fst x) nDeps \\ oDeps
        _                                       -> error "ParseFailure" --TODO: Handle error properly

-- | Checks for new versions added to the cabal file.
checkNewVersions :: IO [(String, String)]
checkNewVersions = do
    rName <- repoName
    oldVersions <- parse versions "" <$> readFile "repoinfo/depsVers.txt" -- TODO: refactor
    newVersions <- parse versions "" <$> readFile "repoinfo/depsVersUpdated.txt"
    case (oldVersions, newVersions) of
        (Right oVersions, Right nVersions) -> return $ filter (\x -> rName == fst x) nVersions \\ oVersions
        _                                  -> error "ParseFailure" --TODO: Handle error properly

-- | Checks for direct dependencies that were removed.
checkRemovedPackages :: IO [(String, String)]
checkRemovedPackages = do
    rName <- repoName
    oldDeps <- parse allDependencies "" <$> readFile "repoinfo/gendeps.dot" -- TODO: refactor
    newDeps <- parse allDependencies "" <$> readFile "repoinfo/gendepsUpdated.dot"
    case (newDeps, oldDeps) of
        (Right nDeps, Right oDeps) ->  return $ filter (\x -> rName == fst x) (oDeps \\ nDeps)

        _                          -> error "ParseFailure" --TODO: Handle error properly

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

