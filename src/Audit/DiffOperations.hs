module Audit.DiffOperations
  ( audDepToPackage
  , clearDiffTable
  , diffDepToPackage
  , diffDepToText
  , insertRemovedDependenciesDiff
  , insertPackageDiff
  , loadDiffTableNewDepsOnly
  , loadNewDirDepsDiff
  , loadNewIndirectDepsDiff
  , loadDiffTableRemovedDirDeps
  , checkForNewDependenciesAgainstDb
  , pkgToDiff
  , queryDiff
  , queryDiff'
  )
where

import Audit.Conversion
  ( audDepToPackage
  , diffDepToPackage
  , diffDepToText
  , parsedDepToDiffQExpr
  , packageToParsedDep
  , pkgToDiff
  , fst3
  , pkgThruples
  )
import Audit.Database (AuditorT(..), auditorDb, AuditorDb(..), DiffT(..), Diff)
import Audit.Sorting (removedDirDeps)
import Audit.Queries (queryAuditor)
import Audit.Types
  (OperationError(..), OperationResult(..), Package(..), ParsedDependency(..), QPResult(..))
import Control.Monad.Trans.Either (EitherT, hoistEither, right, runEitherT)
import Data.Either (rights)
import Data.List (sort, intersect)
import Data.Set (difference, fromList, toList)
import Database.Beam

import Database.Beam.Query (filter_, just_)
import Database.Beam.Sqlite.Connection (runBeamSqlite)
import Database.SQLite.Simple (close, open)
import Data.Text (pack, Text)
import Data.Time.Format (defaultTimeLocale, formatTime)


--------------------------------------------------------------------------------
-- Diff Table Operations
--------------------------------------------------------------------------------


-- | Delete the dependencies in the diff table.
clearDiffTable :: String -> IO ()
clearDiffTable dbName = do
  conn        <- open dbName
  allDiffDeps <- queryDiff dbName
  mapM_
    (\x -> runBeamSqlite conn $ runDelete $ delete
      (diff auditorDb)
      (\c -> diffPackageName c ==. val_ x)
    )
    allDiffDeps
  close conn

-- | Takes a newly added `Package` and inserts it into the Diff table.
insertPackageDiff :: String -> Package -> IO ()
insertPackageDiff dbFilename (Package pId pName pVersion dateFS dDep sUsed aStatus)
  = do
    conn <- open dbFilename
    let fTime = formatTime defaultTimeLocale "%F %X%Q" dateFS
    runBeamSqlite conn $ runInsert $ insert (diff auditorDb) $ insertValues
      [ Diff
          pId
          pName
          pVersion
          (pack fTime)
          (pack $ show dDep)
          (pack $ show sUsed)
          (pack $ show aStatus)
      ]
    close conn

-- | Updates diff with removed dep. Queries auditor because
-- after the dep is removed from the repository,
-- `stack ls dependencies` can no longer get the version.
-- List of text is the list of dependency names
insertRemovedDependenciesDiff :: String -> [Text] -> IO ()
insertRemovedDependenciesDiff _          []      = pure ()
insertRemovedDependenciesDiff dbFilename remDeps = do
  conn       <- open dbFilename
  audQresult <-
    runBeamSqlite conn
    $ runSelectReturningList
    $ select
    $ (all_ (auditor auditorDb))
  close conn
  -- TODO: This discards possible errors
  let audPackages     = rights $ map audDepToPackage audQresult
  let inCommon        = [ x | x <- audPackages, packageName x `elem` remDeps ]
  let insertTheseDeps = map (\x -> x { stillUsed = False }) inCommon

  mapM_ (insertPackageDiff dbFilename) insertTheseDeps


-- | Load a list of 'Package's into the Diff table.
-- TODO: This can still throw an error from runBeamSqlite
--       capture this in EitherT
loadDiffTableNewDepsOnly
  :: String -> [ParsedDependency] -> EitherT OperationError IO OperationResult
loadDiffTableNewDepsOnly dbName newDeps = do
  conn <- liftIO $ open dbName
  liftIO
    $ runBeamSqlite conn
    $ runInsert
    $ insert (diff auditorDb)
    $ insertExpressions (map parsedDepToDiffQExpr newDeps)
    -- $ insertValues diffConversion
  liftIO $ close conn
  right LoadedDiffTable

-- | Inserts new direct dependencies into the diff table.
loadNewDirDepsDiff
  :: String -> [ParsedDependency] -> EitherT OperationError IO OperationResult
loadNewDirDepsDiff dbName parsedDeps = do
  dirDeps         <- hoistEither $ checkAllAreDirectDeps parsedDeps
  queriedDiffDeps <- liftIO $ queryDiff' dbName
  -- TODO: You are ignoring a potential failure here with rights
  let queriedPkgs = rights $ map diffDepToPackage queriedDiffDeps
  case checkForNewDependenciesAgainstDb dirDeps queriedPkgs of
    QPParseIsEmpty         -> pure NoDirectDependenciesToAdd
    QueryAndParseIdentical -> pure AlreadyAddedDirectDependenciesDiff
    QPDifference new       -> do
      loadDiffTableNewDepsOnly dbName new
      pure AddedDirectDependenciesDiff

-- | Inserts new indirect dependencies into the diff table.
-- checkes to see if the newly parsed dependencies already

loadNewIndirectDepsDiff
  :: String -> [ParsedDependency] -> EitherT OperationError IO OperationResult
loadNewIndirectDepsDiff dbName parsedDeps = do
  indirDeps       <- hoistEither $ checkAllAreIndirectDeps parsedDeps
  queriedDiffDeps <- liftIO $ queryDiff' dbName
  let queriedPkgs = rights $ map diffDepToPackage queriedDiffDeps
  case checkForNewDependenciesAgainstDb indirDeps queriedPkgs of
    QPParseIsEmpty         -> pure NoIndirectDependenciesToAdd
    QueryAndParseIdentical -> pure AlreadyAddedIndirectDependenciesDiff
    QPDifference new       -> do
      loadDiffTableNewDepsOnly dbName new
      pure AddedIndirectDependenciesDiff

-- | Inserts removed dependencies into the Diff table.
loadDiffTableRemovedDirDeps
  :: String -> EitherT OperationError IO OperationResult
loadDiffTableRemovedDirDeps dbName = do
  rDeps <- liftIO removedDirDeps
  case rDeps of
    []        -> right NoRemovedDependenciesToAdd
    otherwise -> do
      -- You need to update the relevant entry in the auditor table
      -- only change the still used leave all other fields unchanged
      -- update
      let rDepText = map pack rDeps
      audDeps <- liftIO $ queryAuditor dbName
      let audPackages = mapM audDepToPackage audDeps
      case audPackages of
        Right aPkgs ->
          let
            entryToUpdate =
              -- this matches all the aeson packages, should only match
              -- the indirect dep version.
              [ x
              | x <- aPkgs
              , packageName x `elem` rDepText
              , directDep x == True
              ]
          in
            case entryToUpdate of
              []         -> right AlreadyAddedremovedDependenciesDiff
              remDirDeps -> do
                -- You need to update the existing values, confirm everything
                -- about the value is identical except the fact that it it no
                -- longer a direct dep
                liftIO $ mapM_
                  (insertPackageDiff dbName)
                  (map (\x -> x { stillUsed = False }) remDirDeps)
                pure AddedRemovedDependenciesDiff
        Left err -> error $ show err

-- | Differentiates between existing and new indirect and direct dependencies.
-- Lets us know if there are new dependencies to be added to the Diff table or not.
-- NB: This does not check for modified dependencies.
checkForNewDependenciesAgainstDb :: [ParsedDependency] -> [Package] -> QPResult ParsedDependency
checkForNewDependenciesAgainstDb parsed queriedDeps
  | parsed == []
  = QPParseIsEmpty
  | (  parsedNames
    == queriedNames
    && parsedVersions
    == queriedVersions
    && parsedDirIndir
    == queriedDirIndir
    )
  = QueryAndParseIdentical
  | otherwise
  = QPDifference diff
 where
  diff = sort . toList $ difference (fromList parsed) (fromList $ map packageToParsedDep queriedDeps)
  ---------------------------------------------------------
  parsedNames     = sort $ map depName parsed
  parsedVersions  = sort $ map depVersion parsed
  ---------------------------------------------------------
  queriedNames    = sort $ map packageName queriedDeps
  queriedVersions = sort $ map packageVersion queriedDeps
  ---------------------------------------------------------
  parsedDirIndir  = sort $ map isDirect parsed
  queriedDirIndir = sort $ map directDep queriedDeps


--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

queryDiff :: String -> IO [Text]
queryDiff dbName = do
  conn <- open dbName
  let allEntries = all_ (diff auditorDb)
  entries <- runBeamSqlite conn $ runSelectReturningList $ select allEntries
  close conn
  return $ map diffPackageName entries

queryDiff' :: String -> IO [Diff]
queryDiff' dbName = do
  conn <- open dbName
  let allEntries = all_ (diff auditorDb)
  entries <- runBeamSqlite conn $ runSelectReturningList $ select allEntries
  close conn
  return entries

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------


checkAllAreDirectDeps :: [ParsedDependency] -> Either OperationError [ParsedDependency]
checkAllAreDirectDeps pkgs = if all ((True ==) . isDirect) pkgs
  then return pkgs
  else Left . OnlyDirectDepenciesAllowed $ filter ((False ==) . isDirect) pkgs

checkAllAreIndirectDeps :: [ParsedDependency] -> Either OperationError [ParsedDependency]
checkAllAreIndirectDeps pkgs = if all ((False ==) . isDirect) pkgs
  then return pkgs
  else Left . OnlyIndirectDepenciesAllowed $ filter ((True ==) . isDirect) pkgs
