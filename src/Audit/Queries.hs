{-# LANGUAGE OverloadedStrings #-}

module Audit.Queries
  ( getAllVersionsOfDep
  , getDirAudEntryByDepName
  , getInDirAudEntryByDepName
  , queryAuditor
  , queryAuditorDepNames
  , queryAuditorSpecificVersion
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
                                                , DependencyName
                                                , IndirectDependency
                                                , OperationError(..)
                                                , OperationResult(..)
                                                , Version
                                                )

import           Control.Monad.Trans.Either     ( EitherT , left, right)
import           Data.Text                      ( pack, Text )
import           Database.Beam
import           Database.Beam.Sqlite.Connection
                                                ( runBeamSqlite
                                                , Sqlite
                                                )
import           Database.SQLite.Simple         ( close
                                                , open
                                                )

filterAllVersionsOfDepQuery
  :: DependencyName -> Q Sqlite AuditorDb s (AuditorT (QExpr Sqlite s))
filterAllVersionsOfDepQuery dep =
  filter_
      (\dependency ->
        auditorPackageName dependency
          ==. (val_ $ pack dep)
      )
    $ all_ (auditor auditorDb)

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

filterSpecificDependencyVersion
  :: (DependencyName, Version) -> Q Sqlite AuditorDb s (AuditorT (QExpr Sqlite s))
filterSpecificDependencyVersion (dep, version) =
  filter_
      (\dependency ->
        auditorPackageName dependency
          ==. (val_ $ pack dep)
          &&. auditorPackageVersion dependency
          ==. (val_ $ pack version)
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

getAllVersionsOfDep
  :: String -> DependencyName -> EitherT OperationError IO [Auditor]
getAllVersionsOfDep dbName dep = do
  conn  <- liftIO $ open dbName
  entries <-
    liftIO
    . runBeamSqlite conn
    . runSelectReturningList
    . select
    $ filterAllVersionsOfDepQuery dep
  liftIO $ close conn
  case entries of
    [] -> left $ NotInAuditorTable dep
    _ -> pure entries

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

queryAuditorSpecificVersion :: String -> (DependencyName, Version) -> EitherT OperationError IO OperationResult
queryAuditorSpecificVersion dbName (depName,version) = do
  conn <- liftIO $ open dbName
  entries <-
    liftIO
    . runBeamSqlite conn
    . runSelectReturningList
    . select
    $ filterSpecificDependencyVersion (depName, version)
  liftIO $ close conn
  case entries of
    [] -> right VersionDoesNotExistInDb
    _ -> right $ VersionExistsInDb entries


-- | Query and returns the hash in db.
queryHash :: String -> IO (Maybe Hash)
queryHash str = do
  conn <- open str
  let allEntries = all_ (hash auditorDb)
  entry <- runBeamSqlite conn $ runSelectReturningOne $ select allEntries
  close conn
  return entry
