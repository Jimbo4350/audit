
module Audit.Sorting
  ( InitialDepVersions(..)
  , allInitialDepsGrouped
  , parseAllOriginalRepoDeps
  , parseAllOriginalRepoVers
  , parseAllUpdatedRepoVers
  , groupParseResults
  , initialDepTree
  , filterNewIndirectDeps
  , filterVersionChanges
  , filterNewDirDeps
  , filterRemovedIndirectDeps
  , originalDirectDeps
  , filterAllOriginalRepoIndirDeps
  , filterAllUpdatedRepoIndirDeps
  , filterRemovedDirDeps
  , originalDirectDeps
  , parseRepoName
  , updatedDepTree
  )
where

import           Audit.Parser                   ( allDependencies
                                                , packageName'
                                                , versions
                                                )
import           Audit.Tree                     ( buildDepTree )
import           Audit.Types                    ( DirectDependency
                                                , IndirectDependency
                                                , DependencyName
                                                , InitialDepVersions(..)
                                                , UpdatedDepVersions(..)
                                                , Version
                                                )

import           Data.List                      ( groupBy
                                                , nub
                                                , (\\)
                                                , intersect
                                                )
import           Data.Set                       ( difference
                                                , fromList
                                                , toList
                                                )
import           Data.Tree                      ( Tree )
import           Text.Parsec                    ( parse )


-- | Most of the logic in this file is based on parsing
-- "currentDepTree.dot" vs "updatedDepTree.dot"
-- and
-- "currentDepTreeVersions.txt" vs "updatedDepTreeVersions.txt"
-- then performing list operations on the two results to get
-- new dependencies, versions etc.

allInitialDepsGrouped :: FilePath -> IO [(DependencyName, [DirectDependency])]
allInitialDepsGrouped fp = do
  depsWithDependencies <- groupParseResults <$> parseAllOriginalRepoDeps fp

  let indirDeps = nub $ concatMap snd depsWithDependencies
  let dWithDeps = map fst depsWithDependencies

  let depsWithNoDetectedDeps = map (\x -> (x, [])) (indirDeps \\ dWithDeps)
  return $ depsWithDependencies ++ depsWithNoDetectedDeps


allUpdatedDepsGrouped :: IO [(DependencyName, [DirectDependency])]
allUpdatedDepsGrouped = groupParseResults <$> parseAllUpdatedRepoDeps "updatedDepTree.dot"

-- | Returns all the packages in the repo and their direct depdendencies.
-- The repo itself is considered a package. NB: The direct dependency in
-- the tuple is the direct dependency of the `PackageName` within that tuple.
parseAllOriginalRepoDeps :: FilePath -> IO [(DependencyName, DirectDependency)]
parseAllOriginalRepoDeps fp = do
  pDeps <- parse allDependencies "" <$> readFile fp
  case pDeps of
    Left  parserError -> error $ show parserError
    Right deps        -> return deps

parseAllUpdatedRepoDeps :: String -> IO [(DependencyName, DirectDependency)]
parseAllUpdatedRepoDeps updatedDepTree = do
  pDeps <- parse allDependencies "" <$> readFile updatedDepTree
  case pDeps of
    Left  parserError -> error $ show parserError
    Right deps        -> return deps

-- | This returns a list of all the indirect dependencies of the original
-- repository.
filterAllOriginalRepoIndirDeps :: String -> IO [IndirectDependency]
filterAllOriginalRepoIndirDeps curDepTree = do
  allDeps <- allInitialDepsGrouped curDepTree
  -- NB: These direct dependencies may also be indirect dependencies
  dDeps   <- originalDirectDeps curDepTree
  let allDepsList     = nub (tuplesToList allDeps)
  -- Account for the possibility that a direct dependency is also
  -- an indirect dependency
  let dirAndIndirDeps = allDepsList `intersect` dDeps
  return $ (allDepsList \\ dDeps) ++ dirAndIndirDeps


filterAllUpdatedRepoIndirDeps :: IO [IndirectDependency]
filterAllUpdatedRepoIndirDeps = do
  alUpdatedDeps  <- allUpdatedDepsGrouped
  updatedDirDeps <- updatedDirectDeps
  let allUpdatedRepoDepsList = nub (tuplesToList alUpdatedDeps)
  -- Account for the possibility that a direct dependency is also
  -- an indirect dependency
  let dirAndIndirDeps        = allUpdatedRepoDepsList `intersect` updatedDirDeps
  return $ (allUpdatedRepoDepsList \\ updatedDirDeps) ++ dirAndIndirDeps

-- | Returns all the packages in the repo and their versions. Again
-- the repo itself is considered a package and is included. NB: The version
-- in the tuple is the version of the `PackageName` within that tuple.
parseAllOriginalRepoVers :: String -> IO InitialDepVersions
parseAllOriginalRepoVers fp = do
  pVers <- parse versions "" <$> readFile fp
  case pVers of
    Left  parserError -> error $ show parserError
    Right vers        -> return $ InitialDepVersions vers

parseAllUpdatedRepoVers :: IO UpdatedDepVersions
parseAllUpdatedRepoVers = do
  pVers <- parse versions "" <$> readFile "updatedDepTreeVersions.txt"
  case pVers of
    Left  parserError -> error $ show parserError
    Right vers        -> return $ UpdatedDepVersions vers

-- | Returns all new indirect dependencies. NB: The hierarchy is lost and
-- packages in this list could be dependencies of each other.
-- Also a dependency that was a direct dependency previously, will be listed
-- as a new indirect dependency
filterNewIndirectDeps :: String -> String -> IO [IndirectDependency]
filterNewIndirectDeps curDepTree updatedDepTree = do
  originalIndirDeps <- filterAllOriginalRepoIndirDeps curDepTree
  aUpdDeps          <- parseAllUpdatedRepoDeps updatedDepTree
  uDirDeps          <- updatedDirectDeps
  let updatedIndirDeps =
        (nub . tuplesToList $ groupParseResults aUpdDeps) \\ uDirDeps
  return $ updatedIndirDeps \\ originalIndirDeps

-- | Checks for new direct dependencies added to the cabal file.
filterNewDirDeps :: String -> String -> IO [DirectDependency]
filterNewDirDeps curDepTree updatedDepTree   = do
  rName   <- parseRepoName curDepTree
  oldDeps <- parseAllOriginalRepoDeps curDepTree
  newDeps <- parseAllUpdatedRepoDeps updatedDepTree
  return . map snd $ filter (\x -> rName == fst x) newDeps \\ oldDeps

-- | Checks for new versions added to the cabal file.
filterVersionChanges :: IO [(DependencyName, Version)]
filterVersionChanges = do
  oldVers <- parseAllOriginalRepoVers "currentDepTreeVersions.txt"
  -- TODO: Thread `Text` through the repo
  newVers <- parseAllUpdatedRepoVers
  let updatedVersSet = fromList $ updatedDeps newVers
  let initialVersSet = fromList $ initDeps oldVers
  return . toList $ updatedVersSet `difference` initialVersSet

-- | Checks for direct dependencies that were removed.
filterRemovedDirDeps :: IO [DirectDependency]
filterRemovedDirDeps = do
  rName   <- parseRepoName "currentDepTree.dot"
  oldDeps <- parseAllOriginalRepoDeps "currentDepTree.dot"
  newDeps <- parseAllUpdatedRepoDeps "updatedDepTree.dot"
  return . map snd $ filter (\x -> rName == fst x) (oldDeps \\ newDeps)

filterRemovedIndirectDeps :: IO [IndirectDependency]
filterRemovedIndirectDeps = do
  dDeps     <- originalDirectDeps "currentDepTree.dot"
  origIndir <- filterAllOriginalRepoIndirDeps "currentDepTree.dot"
  upIndir   <- filterAllUpdatedRepoIndirDeps
  return
    [ updIndirDeps
    | updIndirDeps <- origIndir \\ upIndir
    , updIndirDeps `notElem` dDeps
    ]


-- | Initial dependency tree (does not contain package versions)

initialDepTree :: String -> IO (Tree String)
initialDepTree fp = do
  rName <- parseRepoName fp
  iDeps <- allInitialDepsGrouped fp
  pure $ buildDepTree rName iDeps

updatedDepTree :: IO (Tree String)
updatedDepTree = do
  rName <- parseRepoName "currentDepTree.dot"
  iDeps <- allUpdatedDepsGrouped
  pure $ buildDepTree rName iDeps

-- | Returns direct depedencies of the repo before any changes were
-- made to the cabal file.
originalDirectDeps :: String ->  IO [DirectDependency]
originalDirectDeps currDotFile = do
  rName <- parseRepoName currDotFile
  aDeps <- parseAllOriginalRepoDeps currDotFile
  let repoDirDeps = packageDependencies rName aDeps
  pure $ concatMap snd repoDirDeps

-- | Returns direct depedencies of the repo after any changes were
-- made to the cabal file.
updatedDirectDeps :: IO [DirectDependency]
updatedDirectDeps = do
  rName <- parseRepoName "currentDepTree.dot"
  aDeps <- parseAllUpdatedRepoDeps "updatedDepTree.dot"
  let repoDirDeps = packageDependencies rName aDeps
  pure $ concatMap snd repoDirDeps

------------------------------Helpers-----------------------------------

-- | Sorts the results of the parser to [(dependency, [directDependencies])]
groupParseResults
  :: [(String, String)] -> [(DependencyName, [DirectDependency])]
groupParseResults list = do
  let grouped = groupBy (\x y -> fst x == fst y) list
  [ (fst $ head x, map snd x) | x <- grouped ]

packageDependencies :: String -> [(String, String)] -> [(String, [String])]
packageDependencies pkg depTree =
  filter (\x -> pkg == fst x) (groupParseResults depTree)

tuplesToList :: [(String, [String])] -> [String]
tuplesToList allDeps = do
  let tupleList = unzip allDeps
  fst tupleList ++ (concat $ snd tupleList)

parseRepoName :: FilePath -> IO String
parseRepoName fp = do
  name <- parse packageName' "" <$> readFile fp
  case name of
    Left  parserError -> error $ show parserError
    Right deps        -> return deps
