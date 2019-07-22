module Audit.Sorting
  ( InitialDepVersions(..)
  , allInitialDepsGrouped
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
-- "repoinfo/currentDepTree.dot" vs "repoinfo/updatedDepTree.dot"
-- and
-- "repoinfo/currentDepTreeVersions.txt" vs "repoinfo/updatedDepTreeVersions.txt"
-- then performing list operations on the two results to get
-- new dependencies, versions etc.

allInitialDepsGrouped :: IO [(DependencyName, [DirectDependency])]
allInitialDepsGrouped = groupParseResults <$> parseAllOriginalRepoDeps

allUpdatedDepsGrouped :: IO [(DependencyName, [DirectDependency])]
allUpdatedDepsGrouped = groupParseResults <$> parseAllUpdatedRepoDeps

-- | Returns all the packages in the repo and their direct depdendencies.
-- The repo itself is considered a package. NB: The direct dependency in
-- the tuple is the direct dependency of the `PackageName` within that tuple.
parseAllOriginalRepoDeps :: IO [(DependencyName, DirectDependency)]
parseAllOriginalRepoDeps = do
  pDeps <- parse allDependencies "" <$> readFile "repoinfo/currentDepTree.dot"
  case pDeps of
    Left  parserError -> error $ show parserError
    Right deps        -> return deps

parseAllUpdatedRepoDeps :: IO [(DependencyName, DirectDependency)]
parseAllUpdatedRepoDeps = do
  pDeps <- parse allDependencies "" <$> readFile "repoinfo/updatedDepTree.dot"
  case pDeps of
    Left  parserError -> error $ show parserError
    Right deps        -> return deps

-- | This returns a list of all the indirect dependencies of the original
-- repository.
filterAllOriginalRepoIndirDeps :: IO [IndirectDependency]
filterAllOriginalRepoIndirDeps = do
  allDeps <- allInitialDepsGrouped
  -- NB: These direct dependencies may also be indirect dependencies
  dDeps   <- originalDirectDeps
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
parseAllOriginalRepoVers :: IO InitialDepVersions
parseAllOriginalRepoVers = do
  pVers <- parse versions "" <$> readFile "repoinfo/currentDepTreeVersions.txt"
  case pVers of
    Left  parserError -> error $ show parserError
    Right vers        -> return $ InitialDepVersions vers

parseAllUpdatedRepoVers :: IO UpdatedDepVersions
parseAllUpdatedRepoVers = do
  pVers <- parse versions "" <$> readFile "repoinfo/updatedDepTreeVersions.txt"
  case pVers of
    Left  parserError -> error $ show parserError
    Right vers        -> return $ UpdatedDepVersions vers

-- | Returns all new indirect dependencies. NB: The hierarchy is lost and
-- packages in this list could be dependencies of each other.
-- Also a dependency that was a direct dependency previously, will be listed
-- as a new indirect dependency
filterNewIndirectDeps :: IO [IndirectDependency]
filterNewIndirectDeps = do
  originalIndirDeps <- filterAllOriginalRepoIndirDeps
  aUpdDeps          <- parseAllUpdatedRepoDeps
  uDirDeps          <- updatedDirectDeps
  let updatedIndirDeps =
        (nub . tuplesToList $ groupParseResults aUpdDeps) \\ uDirDeps
  return $ updatedIndirDeps \\ originalIndirDeps

-- | Checks for new direct dependencies added to the cabal file.
filterNewDirDeps :: IO [DirectDependency]
filterNewDirDeps = do
  rName   <- parseRepoName
  oldDeps <- parseAllOriginalRepoDeps
  newDeps <- parseAllUpdatedRepoDeps
  return . map snd $ filter (\x -> rName == fst x) newDeps \\ oldDeps

-- | Checks for new versions added to the cabal file.
filterVersionChanges :: IO [(DependencyName, Version)]
filterVersionChanges = do
  oldVers <- parseAllOriginalRepoVers
  -- TODO: Thread `Text` through the repo
  newVers <- parseAllUpdatedRepoVers
  let updatedVersSet = fromList $ updatedDeps newVers
  let initialVersSet = fromList $ (initDeps oldVers)
  return . toList $ updatedVersSet `difference` initialVersSet

-- | Checks for direct dependencies that were removed.
filterRemovedDirDeps :: IO [DirectDependency]
filterRemovedDirDeps = do
  rName   <- parseRepoName
  oldDeps <- parseAllOriginalRepoDeps
  newDeps <- parseAllUpdatedRepoDeps
  return . map snd $ filter (\x -> rName == fst x) (oldDeps \\ newDeps)

filterRemovedIndirectDeps :: IO [IndirectDependency]
filterRemovedIndirectDeps = do
  dDeps     <- originalDirectDeps
  origIndir <- filterAllOriginalRepoIndirDeps
  upIndir   <- filterAllUpdatedRepoIndirDeps
  return
    [ updIndirDeps
    | updIndirDeps <- origIndir \\ upIndir
    , updIndirDeps `notElem` dDeps
    ]


-- | Initial dependency tree (does not contain package versions)

initialDepTree :: IO (Tree String)
initialDepTree = do
  rName <- parseRepoName
  iDeps <- allInitialDepsGrouped
  pure $ buildDepTree rName iDeps

updatedDepTree :: IO (Tree String)
updatedDepTree = do
  rName <- parseRepoName
  iDeps <- allUpdatedDepsGrouped
  pure $ buildDepTree rName iDeps

-- | Returns direct depedencies of the repo before any changes were
-- made to the cabal file.
originalDirectDeps :: IO [DirectDependency]
originalDirectDeps = do
  rName <- parseRepoName
  aDeps <- parseAllOriginalRepoDeps
  let repoDirDeps = packageDependencies rName aDeps
  pure $ concatMap snd repoDirDeps

-- | Returns direct depedencies of the repo after any changes were
-- made to the cabal file.
updatedDirectDeps :: IO [DirectDependency]
updatedDirectDeps = do
  rName <- parseRepoName
  aDeps <- parseAllUpdatedRepoDeps
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

parseRepoName :: IO String
parseRepoName = do
  name <- parse packageName' "" <$> readFile "repoinfo/currentDepTree.dot"
  case name of
    Left  parserError -> error $ show parserError
    Right deps        -> return deps
