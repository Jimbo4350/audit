{-# LANGUAGE OverloadedStrings #-}

module Audit.Operations
  ( buildParsedDependencyList
  , checkHash
  , clearAuditorTable
  , deleteAuditorEntry
  , deleteHash
  , diffDepToPackage
  , insertAuditorDeps
  , insertHash
  , loadDiffIntoAuditorNew
  , loadDiffIntoAuditorUpdate
  , loadDiffIntoAuditorUpdateStillUsed
  , pkgToAuditor
  , updateAuditorEntryWithDiff
  , updateAuditorEntry
  , updateAuditorEntryDirect
  )
where

import Audit.AuditorOperations
  (clearAuditorTable, deleteAuditorEntry, insertAuditorDeps)
import Audit.Database
  (Auditor, AuditorDb(..), AuditorT(..), Diff, DiffT(..), HashT(..), auditorDb)
import Audit.DiffOperations (queryDiff', diffDepToPackage, diffDepToText, clearDiffTable)
import Audit.Queries (queryAuditor, queryHash)
import Audit.Types
  ( AnalysisStatus
  , ConversionError(..)
  , DirectDependency
  , HashStatus(..)
  , IndirectDependency
  , OperationError(..)
  , OperationResult(..)
  , Package(..)
  , PackageName
  , ParsedDependency (..)
  , Version
  )
import Audit.Conversion (pkgToDiff, pkgThruples, fst3, pkgToAuditor, createPackage, audDepToPackage, parsedDepToAudQExpr, packageToParsedDep)
import Control.Monad.Trans.Either (EitherT, firstEitherT, hoistEither, right)
import Data.Int (Int32)
import Data.List (intersect, (\\))
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Database.Beam

import Database.Beam.Sqlite.Connection (runBeamSqlite)
import Database.SQLite.Simple (close, open)
import Database.SQLite.Simple.Time.Implementation (parseUTCTime)


--------------------------------------------------------------------------------
-- Conversion Functions
--------------------------------------------------------------------------------

-- | Build package/dependency list for the initialization of
-- the database.
buildParsedDependencyList
  :: [(PackageName, Version)]
  -> [DirectDependency]
  -> [IndirectDependency]
  -> UTCTime
  -> [ParsedDependency]
buildParsedDependencyList pVersions dDeps indirDeps currTime = do

  let directPackages   = map (directPkg currTime) dDeps
  let indirectPackages = map (indirectPkg currTime) indirDeps

  directPackages ++ indirectPackages
 where
  lookupVersion x =
    (pack $ fromMaybe "No version Found" (lookup x pVersions))
  directPkg time x = ParsedDependency (pack x) (lookupVersion x) time True True []
  indirectPkg time x = ParsedDependency (pack x) (lookupVersion x) time False True []


--------------------------------------------------------------------------------
-- Operations involving both Diff & Auditor tables.
--------------------------------------------------------------------------------

-- | Compare an entry from the Diff table and Auditor table.
-- Return an updated `Auditor` that will be inserted into the auditor table
-- with the preserved date first seen.
updateAuditorEntryWithDiff :: Diff -> Auditor -> Either ConversionError Auditor
updateAuditorEntryWithDiff updatedDep aud = do
  let diffTextVals = diffDepToText updatedDep

  let
    audTextVals = map
      (\f -> f aud)
      [ (pack . show .  auditorDependencyId)
      , auditorPackageName
      , auditorPackageVersion
      , auditorDateFirstSeen
      , auditorDirectDep
      , auditorStillUsed
      , auditorAnalysisStatus
      ]

  let zipped = zip audTextVals diffTextVals
  toAuditor
    [ if audText == diffText then audText else diffText
    | (audText, diffText) <- zipped
    ]
    (auditorDateFirstSeen aud)
 where
  toAuditor :: [Text] -> Text -> Either ConversionError Auditor
  toAuditor [pId, pName, pVer, _newDfSeen, dDep, sUsed, aStat] origDfSeen =
    Right $ Auditor (read $ show pId :: Int32) pName pVer origDfSeen dDep sUsed aStat
  toAuditor _ _ =
    Left
      $ DiffToAuditorError
          "toAuditor: The impossible happened, this should only yield a list of length 6"


-- | Updates an entry in the Auditor table and deletes the corresponding entry in Diff table.
updateAuditorEntry
  :: String -> Diff -> Auditor -> EitherT OperationError IO OperationResult
updateAuditorEntry dbName updatedPackage currentPackage = do
  conn    <- liftIO $ open dbName
  updated <- firstEitherT ConvError . hoistEither $ updateAuditorEntryWithDiff
    updatedPackage
    currentPackage
  -- Updates corresponding package in auditor table.
  liftIO $ runBeamSqlite conn $ runUpdate $ save (auditor auditorDb) updated
  -- Deletes package in diff
  liftIO $ runBeamSqlite conn $ runDelete $ delete
    (diff auditorDb)
    (\table -> diffPackageName table ==. (val_ $ diffPackageName updatedPackage)
    )
  liftIO $ close conn
  pure AddedRemovedDependenciesAuditor


-- | Updates Auditor table with new dependencies from the Diff table
loadDiffIntoAuditorNew :: String -> EitherT OperationError IO OperationResult
loadDiffIntoAuditorNew dbName = do
  diffDeps <- liftIO $ queryDiff' dbName
  audDeps  <- liftIO $ queryAuditor dbName
  let audPackages  = mapM audDepToPackage audDeps
  let diffPackages = mapM diffDepToPackage diffDeps
  case (audPackages, diffPackages) of
    (Right aPkgs, Right dPkgs) -> do
      -- What constitutes a new package?
      -- packageName
      -- packageVersion
      -- directDep (may be added as a direct dependency whereas before it was
      -- and indirect dependency. This is treated as a new package)
      let inCommonPkgs = pkgThruples aPkgs `intersect` pkgThruples dPkgs
      -- Here we exclude dependencies that already exist in the Auditor table
      let newPkgsName  = pkgThruples dPkgs \\ inCommonPkgs
      let
        newPkgs =
          [ x | x <- dPkgs, packageName x `elem` map fst3 newPkgsName ]
      conn <- liftIO $ open dbName
      liftIO $ print "From loadDiffIntoAuditorNew"
      liftIO $ print newPkgs
      -- Insert new packages into Auditor table
      liftIO
        $ runBeamSqlite conn
        $ runInsert
        $ insert (auditor auditorDb)
        $ insertExpressions (map (parsedDepToAudQExpr . packageToParsedDep) newPkgs)
      -- Delete packages from Diff that were added to Auditor
      liftIO $ clearDiffTable dbName
      liftIO $ close conn
      right LoadedNewDepsFromDiffIntoAuditor
    (_, _) ->
      error
        "loadDiffIntoAuditorNew: Conversion from Diff/Auditor to Package went wrong"

-- | Updates existing dependencies in the Auditor table
-- with changes introduced by the Diff table.
loadDiffIntoAuditorUpdate :: String -> EitherT OperationError IO OperationResult
loadDiffIntoAuditorUpdate dbName = do
  diffDeps <- liftIO $ queryDiff' dbName
  audDeps  <- liftIO $ queryAuditor dbName
  -- Convert 'Auditor' and 'Diff' to 'Packages'
  let audPackages  = mapM audDepToPackage audDeps
  let diffPackages = mapM diffDepToPackage diffDeps
  case (audPackages, diffPackages) of
    (Right aPkgs, Right dPkgs) -> do
      -- Prepare updated Auditor values for entry into the Auditor table
      -- What constitutes an update to an entry?
      -- stillUsed
      -- analysis status
      -- Which entry to update?
      -- Need to separate into two further functions: stillUsed and analysisStatus
      let inCommonPkgs = pkgThruples aPkgs `intersect` pkgThruples dPkgs
      let
        updatedPackages =
          [ x | x <- dPkgs, packageName x `elem` map fst3 inCommonPkgs ]
      conn <- liftIO $ open dbName
      liftIO $ print updatedPackages
      liftIO $ mapM
        (runBeamSqlite conn . runUpdate . save (auditor auditorDb))
        (map pkgToAuditor updatedPackages)
      right UpdatedExistingAuditorDepsWithDiffDeps
    (_, _) ->
      error
        "loadDiffIntoAuditorUpdate: Conversion from Diff/Auditor to Package went wrong"

loadDiffIntoAuditorUpdateStillUsed :: String -> EitherT OperationError IO OperationResult
loadDiffIntoAuditorUpdateStillUsed dbName = do
  diffDeps <- liftIO $ queryDiff' dbName
  audDeps  <- liftIO $ queryAuditor dbName
  -- Convert 'Auditor' and 'Diff' to 'Packages'
  let audPackages  = mapM audDepToPackage audDeps
  let diffPackages = mapM diffDepToPackage diffDeps
  case (audPackages, diffPackages) of
    (Right aPkgs, Right dPkgs) -> do
      let inCommonPkgs = pkgThruples aPkgs `intersect` pkgThruples dPkgs
      let
        updatedPackages =
          [ x | x <- dPkgs, packageName x `elem` (map fst3 inCommonPkgs) ]
      let final = map pkgToAuditor updatedPackages :: [Auditor]
      liftIO $ print "From loadDiffIntoAuditorStillUsed"
      liftIO $ print updatedPackages
      mapM_ (updateAuditorEntryDirect dbName) final
      liftIO $ clearDiffTable dbName
      right UpdatedAuditorTableStillUsed

updateAuditorEntryDirect
  :: String -> Auditor -> EitherT OperationError IO OperationResult
updateAuditorEntryDirect dbName updatedPackage = do
  conn    <- liftIO $ open dbName
  -- Updates corresponding package in auditor table.
  liftIO $ runBeamSqlite conn $ runUpdate $ save (auditor auditorDb) updatedPackage

  liftIO $ close conn
  pure UpdatedAuditorTableStillUsed

--------------------------------------------------------------------------------
-- Hash Table Operations
--------------------------------------------------------------------------------

-- | Takes a hash and inserts it into the Hash table.
insertHash :: String -> Int -> IO ()
insertHash dbFilename dHash = do
  curHash <- queryHash dbFilename
  case curHash of
    Just _  -> error $ "insertHash: A hash is already present in " ++ dbFilename
    Nothing -> do
      conn <- open dbFilename
      runBeamSqlite conn $ runInsert $ insert (hash auditorDb) $ insertValues
        [Hash dHash]
      close conn

-- | Takes a hash and compares it to the database hash.
checkHash :: String -> Int -> IO HashStatus
checkHash dbName testHash = do
  entry <- queryHash dbName
  case entry of
    Just dbH -> do
      let dbHashInt = hashCurrentHash dbH
      if testHash == dbHashInt
        then liftIO $ return HashMatches
        else liftIO $ return HashDoesNotMatch
    Nothing -> liftIO $ return HashNotFound

-- | Deletes the hash in the hash table.
deleteHash :: String -> IO ()
deleteHash dbName = do
  conn  <- open dbName
  cHash <- queryHash dbName
  case cHash of
    Just dbH -> do
      let dbHashInt = hashCurrentHash dbH
      runBeamSqlite conn $ runDelete $ delete
        (hash auditorDb)
        (\table -> hashCurrentHash table ==. val_ dbHashInt)
    Nothing -> print ("Hash not found in database" :: String)
  close conn
--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------



audDepToText :: Auditor -> [Text]
audDepToText audDep = map
  (\f -> f audDep)
  [ auditorPackageName
  , auditorPackageVersion
  , auditorDateFirstSeen
  , auditorDirectDep
  , auditorStillUsed
  , auditorAnalysisStatus
  ]
