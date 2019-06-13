{-# LANGUAGE OverloadedStrings #-}

module Audit.Queries
  ( queryAuditor
  , queryAuditorDepNames
  , queryAuditorDepVersions
  , queryAuditorRemovedDeps
  , queryDiff
  , queryDiffRemovedDeps
  , queryDiff'
  , queryHash
  ) where

import           Audit.Database                  (Auditor, AuditorDb (..),
                                                  AuditorT (..), Diff,
                                                  DiffT (..), Hash, auditorDb)
import           Data.Text                       (Text)
import           Database.Beam                   (all_, runSelectReturningList,
                                                  runSelectReturningOne, select)
import           Database.Beam.Sqlite.Connection (runBeamSqlite)
import           Database.SQLite.Simple          (close, open)

-- | Returns all entries in the Auditor table.
queryAuditor :: String -> IO [Auditor]
queryAuditor  dbName = do
    conn <- open dbName
    let allEntries = all_ (auditor auditorDb)
    entries <- runBeamSqlite conn $
        runSelectReturningList $ select allEntries
    close conn
    return entries

queryAuditorDepNames :: String -> IO [Text]
queryAuditorDepNames dbName  = do
    conn <- open dbName
    let allEntries = all_ (auditor auditorDb)
    entries <- runBeamSqlite conn $
        runSelectReturningList $ select allEntries
    close conn
    return $ map auditorPackageName entries

queryAuditorDepVersions :: String -> IO [Text]
queryAuditorDepVersions dbName  = do
    conn <- open dbName
    let allEntries = all_ (auditor auditorDb)
    entries <- runBeamSqlite conn $
        runSelectReturningList $ select allEntries
    close conn
    return $ map auditorPackageVersion entries

queryAuditorRemovedDeps :: String -> IO [Text]
queryAuditorRemovedDeps dbName  = do
    conn <- open dbName
    let allEntries = all_ (auditor auditorDb)
    entries <- runBeamSqlite conn $
        runSelectReturningList $ select allEntries
    close conn
    let remDeps = filter (\x -> auditorStillUsed x == "False") entries
    return $ map auditorPackageName remDeps

queryDiff :: String ->  IO [Text]
queryDiff dbName = do
    conn <- open dbName
    let allEntries = all_ (diff auditorDb)
    entries <- runBeamSqlite conn $
               runSelectReturningList $ select allEntries
    close conn
    return $ map  diffPackageName entries

queryDiffRemovedDeps :: String -> IO [Text]
queryDiffRemovedDeps dbName  = do
    conn <- open dbName
    let allEntries = all_ (diff auditorDb)
    entries <- runBeamSqlite conn $
        runSelectReturningList $ select allEntries
    close conn
    let remDeps = filter (\x -> diffStillUsed x == "False") entries
    return $ map diffPackageName remDeps

queryDiff' :: String ->  IO [Diff]
queryDiff' dbName = do
    conn <- open dbName
    let allEntries = all_ (diff auditorDb)
    entries <- runBeamSqlite conn $
               runSelectReturningList $ select allEntries
    close conn
    return entries

-- | Query and returns the hash in db.
queryHash :: IO (Maybe Hash)
queryHash = do
    conn <- open "auditor.db"
    let allEntries = all_ (hash auditorDb)
    entry <- runBeamSqlite conn $
             runSelectReturningOne $ select allEntries
    close conn
    return entry
