{-# LANGUAGE OverloadedStrings #-}

module Audit.Operations
  ( newParsedDeps
  , checkHash
  , clearAuditorTable
  , deleteHash
  , insertAuditorDeps
  , insertHash
  , updateAuditorEntryDirect
  )
where

import Audit.AuditorOperations
  (clearAuditorTable, insertAuditorDeps)
import Audit.Database
  (Auditor, AuditorDb(..), HashT(..), auditorDb)
import Audit.Queries (queryHash)
import Audit.Types
  ( DirectDependency
  , HashStatus(..)
  , IndirectDependency
  , OperationError(..)
  , OperationResult(..)
  , DependencyName
  , ParsedDependency (..)
  , Version
  )
import Control.Monad.Trans.Either (EitherT)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Time.Clock (UTCTime)
import Database.Beam

import Database.Beam.Sqlite.Connection (runBeamSqlite)
import Database.SQLite.Simple (close, open)

--------------------------------------------------------------------------------
-- Conversion Functions
--------------------------------------------------------------------------------

-- | Build package/dependency list for the initialization of
-- the database.
newParsedDeps
  :: [(DependencyName, Version)]
  -> [DirectDependency]
  -> [IndirectDependency]
  -> UTCTime
  -> [ParsedDependency]
newParsedDeps pVersions dDeps indirDeps currTime = do

  let directPackages   = map (directPkg currTime) dDeps
  let indirectPackages = map (indirectPkg currTime) indirDeps

  directPackages ++ indirectPackages
 where
  lookupVersion x =
    pack $ fromMaybe "No version Found" (lookup x pVersions)
  directPkg time x = ParsedDependency (pack x) (lookupVersion x) time True True []
  indirectPkg time x = ParsedDependency (pack x) (lookupVersion x) time False True []

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
