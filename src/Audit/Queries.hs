{-# LANGUAGE OverloadedStrings #-}

module Audit.Queries
  ( queryAuditor
  , queryAuditorDepVersions
  , queryAuditorRemovedDeps
  , queryHash
  )
where

import Audit.Database
  (Auditor, AuditorDb(..), AuditorT(..), Hash, auditorDb)
import Data.Text (Text)
import Database.Beam
  (all_, runSelectReturningList, runSelectReturningOne, select)
import Database.Beam.Sqlite.Connection (runBeamSqlite)
import Database.SQLite.Simple (close, open)

-- | Returns all entries in the Auditor table.
queryAuditor :: String -> IO [Auditor]
queryAuditor dbName = do
  conn <- open dbName
  let allEntries = all_ (auditor auditorDb)
  entries <- runBeamSqlite conn $ runSelectReturningList $ select allEntries
  close conn
  return entries

queryAuditorDepVersions :: String -> IO [Text]
queryAuditorDepVersions dbName = do
  conn <- open dbName
  let allEntries = all_ (auditor auditorDb)
  entries <- runBeamSqlite conn $ runSelectReturningList $ select allEntries
  close conn
  return $ map auditorPackageVersion entries

queryAuditorRemovedDeps :: String -> IO [Text]
queryAuditorRemovedDeps dbName = do
  conn <- open dbName
  let allEntries = all_ (auditor auditorDb)
  entries <- runBeamSqlite conn $ runSelectReturningList $ select allEntries
  close conn
  let remDeps = filter (\x -> auditorStillUsed x == False) entries
  return $ map auditorPackageName remDeps

-- | Query and returns the hash in db.
queryHash :: String -> IO (Maybe Hash)
queryHash str = do
  conn <- open str
  let allEntries = all_ (hash auditorDb)
  entry <- runBeamSqlite conn $ runSelectReturningOne $ select allEntries
  close conn
  return entry
