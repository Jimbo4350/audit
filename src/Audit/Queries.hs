{-# LANGUAGE OverloadedStrings #-}

module Audit.Queries
  ( getDirAudEntryByDepName
  , getInDirAudEntryByDepName
  , queryAuditor
  , queryAuditorDepNames
  , queryAuditorDepVersions
  , queryAuditorRemovedDeps
  , queryHash
  )
where

import           Audit.Database                 ( Auditor
                                                , AuditorDb(..)
                                                , AuditorT(..)
                                                , Hash
                                                , auditorDb
                                                )
import           Audit.Types                    ( DirectDependency
                                                , IndirectDependency
                                                , OperationError(..)
                                                )

import           Control.Monad.Trans.Either     ( EitherT , left)
import           Data.Text                      ( pack, Text )
import           Database.Beam
import           Database.Beam.Sqlite.Connection
                                                ( runBeamSqlite
                                                , Sqlite
                                                )
import           Database.SQLite.Simple         ( close
                                                , open
                                                )

filterDirDepQuery
  :: DirectDependency -> Q Sqlite AuditorDb s (AuditorT (QExpr Sqlite s))
filterDirDepQuery dirDep =
  filter_
      (\dependency ->
        auditorPackageName dependency
          ==. (val_ $ pack dirDep)
          &&. auditorDirectDep dependency
          ==. (val_ True)
      )
    $ all_ (auditor auditorDb)

filterInDirDepQuery
  :: IndirectDependency -> Q Sqlite AuditorDb s (AuditorT (QExpr Sqlite s))
filterInDirDepQuery dirDep =
  filter_
      (\dependency ->
        auditorPackageName dependency
          ==. (val_ $ pack dirDep)
          &&. auditorDirectDep dependency
          ==. (val_ False)
      )
    $ all_ (auditor auditorDb)

getDirAudEntryByDepName
  :: String -> DirectDependency -> EitherT OperationError IO Auditor
getDirAudEntryByDepName dbName dirDep = do
  conn  <- liftIO $ open dbName
  entry <-
    liftIO
    . runBeamSqlite conn
    . runSelectReturningOne
    . select
    $ filterDirDepQuery dirDep
  liftIO $ close conn
  case entry of
    Just aEntry -> pure aEntry
    Nothing     -> left $ NotInAuditorTable dirDep

getInDirAudEntryByDepName
  :: String -> DirectDependency -> EitherT OperationError IO Auditor
getInDirAudEntryByDepName dbName dirDep = do
  conn  <- liftIO $ open dbName
  entry <-
    liftIO
    . runBeamSqlite conn
    . runSelectReturningOne
    . select
    $ filterInDirDepQuery dirDep
  liftIO $ close conn
  case entry of
    Just aEntry -> pure aEntry
    Nothing     -> left $ NotInAuditorTable dirDep

-- | Returns all entries in the Auditor table.
queryAuditor :: String -> IO [Auditor]
queryAuditor dbName = do
  conn <- open dbName
  let allEntries = all_ (auditor auditorDb)
  entries <- runBeamSqlite conn $ runSelectReturningList $ select allEntries
  close conn
  return entries

queryAuditorDepNames :: String -> IO [Text]
queryAuditorDepNames dbName = do
  conn <- open dbName
  let allEntries = all_ (auditor auditorDb)
  entries <- runBeamSqlite conn $ runSelectReturningList $ select allEntries
  close conn
  return $ map auditorPackageName entries

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
