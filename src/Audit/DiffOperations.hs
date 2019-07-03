module Audit.DiffOperations
  ( audDepToPackage
  , clearDiffTable
  , diffDepToPackage
  , diffDepToText
  , insertRemovedDependenciesDiff
  , insertPackageDiff
  , loadDiffTable
  , loadNewDirDepsDiff
  , loadNewIndirectDepsDiff
  , loadDiffTableRemovedDeps
  , parseQueryDifference
  , pkgToDiff
  , queryDiff
  , queryDiff'
  )
where

import Audit.Conversion
  (audDepToPackage, diffDepToPackage, diffDepToText, pkgToDiff)
import Audit.Database
  (PrimaryKey(..), auditorDb, AuditorDb(..), DiffT(..), Diff)
import Audit.Sorting (removedDeps)
import Audit.Types
  (OperationError(..), OperationResult(..), Package(..), QPResult(..))
import Control.Monad.Trans.Either (EitherT, hoistEither, right, runEitherT)
import Data.Either (rights)
import Data.List (sort, intersect)
import Data.Set (difference, fromList, toList)
import Database.Beam
  ( all_
  , delete
  , liftIO
  , runSelectReturningList
  , runSelectReturningOne
  , select
  , runDelete
  , lookup_
  , val_
  , (==.)
  , runInsert
  , insert
  , insertValues
  )
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
insertPackageDiff dbFilename (Package pName pVersion dateFS dDep sUsed aStatus)
  = do
    conn <- open dbFilename
    let fTime = formatTime defaultTimeLocale "%F %X%Q" dateFS
    runBeamSqlite conn $ runInsert $ insert (diff auditorDb) $ insertValues
      [ Diff
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
insertRemovedDependenciesDiff _          []       = pure ()
insertRemovedDependenciesDiff dbFilename (x : xs) = do
  conn       <- open dbFilename
  -- Queries auditor
  audQresult <- runBeamSqlite conn $ do
    let primaryKeyLookup = lookup_ (auditor auditorDb)
    let sqlQuery         = primaryKeyLookup $ AuditorPackageName x
    runSelectReturningOne sqlQuery
  close conn
  -- Gets original time the package was first added.
  case audQresult of
      -- Nothing branch should never match
    Nothing     -> insertRemovedDependenciesDiff dbFilename xs
    Just result -> case audDepToPackage result of
      Left _ ->
        error "insertRemovedDependenciesDiff: audDepToPackage con error"
      Right pkg -> do
        insertPackageDiff dbFilename (pkg { stillUsed = False })
        insertRemovedDependenciesDiff dbFilename xs


-- | Load a list of 'Package's into the Diff table.
-- TODO: This can still throw an error from runBeamSqlite
--       capture this in EitherT
loadDiffTable
  :: String -> [Package] -> EitherT OperationError IO OperationResult
loadDiffTable dbName pkgs = do
  let diffConversion = map pkgToDiff pkgs
  conn <- liftIO $ open dbName
  liftIO
    $ runBeamSqlite conn
    $ runInsert
    $ insert (diff auditorDb)
    $ insertValues diffConversion
  liftIO $ close conn
  right LoadedDiffTable

-- | Inserts new direct dependencies into the diff table.
loadNewDirDepsDiff
  :: String -> [Package] -> EitherT OperationError IO OperationResult
loadNewDirDepsDiff dbName parsedDeps = do
  dirDeps         <- hoistEither $ checkAllAreDirectDeps parsedDeps
  queriedDiffDeps <- liftIO $ queryDiff' dbName
  -- TODO: You are ignoring a potential failure here with rights
  let queriedPkgs = rights $ map diffDepToPackage queriedDiffDeps
  case parseQueryDifference dirDeps queriedPkgs of
    QueryAndParseIdentical -> pure AlreadyAddedDirectDependenciesDiff
    QPDifference new       -> do
      loadDiffTable dbName new
      pure AddedDirectDependenciesDiff

-- | Inserts new indirect dependencies into the diff table.
loadNewIndirectDepsDiff
  :: String -> [Package] -> EitherT OperationError IO OperationResult
loadNewIndirectDepsDiff dbName parsedDeps = do
  indirDeps       <- hoistEither $ checkAllAreIndirectDeps parsedDeps
  queriedDiffDeps <- liftIO $ queryDiff' dbName
  let queriedPkgs = rights $ map diffDepToPackage queriedDiffDeps
  case parseQueryDifference indirDeps queriedPkgs of
    QPParseIsEmpty         -> pure NoIndirectDependenciesToAdd
    QueryAndParseIdentical -> pure AlreadyAddedIndirectDependenciesDiff
    QPDifference new       -> do
      loadDiffTable dbName new
      pure AddedIndirectDependenciesDiff

-- | Inserts removed dependencies into the Diff table.
loadDiffTableRemovedDeps :: String -> EitherT OperationError IO OperationResult
loadDiffTableRemovedDeps dbName = do
  rDeps <- liftIO removedDeps
  case rDeps of
    []        -> right NoRemovedDependenciesToAdd
    otherwise -> do
      let rDepText = map pack rDeps
      depsInDiff <- liftIO $ queryDiff dbName
      let inCommon = depsInDiff `intersect` rDepText
      case inCommon of
        []      -> right AlreadyAddedremovedDependenciesDiff
        remDeps -> do
          liftIO $ print remDeps
          liftIO $ insertRemovedDependenciesDiff dbName rDepText
          pure AddedRemovedDependenciesDiff
          -- TODO: Need to differentiate between direct and indirect removed deps

-- | Returns the associative difference of two lists.
-- If the parsed dependencies differ from the database
-- dependencies, we return the difference or a nullary constructor.
parseQueryDifference :: [Package] -> [Package] -> QPResult Package
parseQueryDifference parsed queriedDeps
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
  diff = sort . toList $ difference (fromList parsed) (fromList queriedDeps)
  parsedNames     = sort $ map packageName parsed
  parsedVersions  = sort $ map packageVersion parsed
  queriedNames    = sort $ map packageName queriedDeps
  queriedVersions = sort $ map packageVersion queriedDeps
  parsedDirIndir  = sort $ map directDep parsed
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


checkAllAreDirectDeps :: [Package] -> Either OperationError [Package]
checkAllAreDirectDeps pkgs = if all ((True ==) . directDep) pkgs
  then return pkgs
  else Left . OnlyDirectDepenciesAllowed $ filter ((False ==) . directDep) pkgs

checkAllAreIndirectDeps :: [Package] -> Either OperationError [Package]
checkAllAreIndirectDeps pkgs = if all ((False ==) . directDep) pkgs
  then return pkgs
  else Left . OnlyIndirectDepenciesAllowed $ filter ((True ==) . directDep) pkgs
