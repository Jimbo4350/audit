{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Audit.Operations
  ( checkHash
  , clearAuditorTable
  , deleteAuditorEntry
  , deleteHash
  , insertAuditorDeps
  , insertHash
  , updateAuditorEntryDirect
  , updateAuditorVersionChange
  )
where

import           Audit.Database                 ( Auditor
                                                , AuditorT(..)
                                                , AuditorDb(..)
                                                , HashT(..)
                                                , auditorDb
                                                )
import           Audit.Conversion               ( auditorEntryToParsedDep
                                                , parsedDepToAudQExpr
                                                )
import           Audit.Queries                  ( getAllVersionsOfDep
                                                , queryAuditorDepNames
                                                , queryAuditorSpecificVersion
                                                , queryHash
                                                )
import           Audit.Types                    ( DependencyName
                                                , HashStatus(..)
                                                , OperationError(..)
                                                , OperationResult(..)
                                                , ParsedDependency(..)
                                                , Version
                                                , report
                                                )

import           Control.Exception              ( catches
                                                , Handler(..)
                                                )
import           Control.Monad.Trans.Either     ( EitherT
                                                , firstEitherT
                                                , hoistEither
                                                , right
                                                )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.Text                      ( pack )
import           Database.Beam
import           Database.Beam.Sqlite.Connection
                                                ( runBeamSqlite )
import           Database.SQLite.Simple         ( close
                                                , open
                                                , FormatError
                                                , ResultError
                                                , SQLError
                                                )



--------------------------------------------------------------------------------
-- Hash Table Operations
--------------------------------------------------------------------------------

-- | Takes a hash and compares it to the database hash.
checkHash :: String -> Int -> EitherT OperationError IO HashStatus
checkHash dbName testHash = do
  entry <- liftIO $ queryHash dbName
  case entry of
    Just dbH -> do
      let dbHashInt = hashCurrentHash dbH
      if testHash == dbHashInt
        then right HashMatches
        else right HashDoesNotMatch
    Nothing -> right HashNotFound


-- | Deletes the hash in the hash table.
deleteHash :: String -> EitherT OperationError IO OperationResult
deleteHash dbName = do
  conn  <- liftIO $ open dbName
  cHash <- liftIO $ queryHash dbName
  case cHash of
    Just dbH -> do
      let dbHashInt = hashCurrentHash dbH
      let deleteCmd = runBeamSqlite conn $ runDelete $ delete
            (hash auditorDb)
            (\table -> hashCurrentHash table ==. val_ dbHashInt)
      liftIO $ exceptionCatcher deleteCmd DeleteHashError
      liftIO $ close conn
      right HashDeleted
    Nothing -> do
      liftIO $ close conn
      right $ HashOperationResult HashNotFound

-- | Takes a hash and inserts it into the Hash table.
insertHash :: String -> Int -> EitherT OperationError IO OperationResult
insertHash dbFilename dHash = do
  curHash <- liftIO $ queryHash dbFilename
  case curHash of
    Just h  -> right . HashAlreadyPresent $ hashCurrentHash h
    Nothing -> do
      conn <- liftIO $ open dbFilename
      let insertCmd =
            runBeamSqlite conn
              $ runInsert
              $ insert (hash auditorDb)
              $ insertValues [Hash dHash]
      liftIO $ exceptionCatcher insertCmd InsertHashError
      liftIO $ close conn
      right $ HashInserted dHash


--------------------------------------------------------------------------------
-- Auditor Table Operations
--------------------------------------------------------------------------------

-- | Delete the dependencies in the auditor table.
clearAuditorTable :: String -> IO ()
clearAuditorTable fp = do
  conn           <- open fp
  allAuditorDeps <- queryAuditorDepNames fp
  let deleteCmd x = runBeamSqlite conn $ runDelete $ delete
        (auditor auditorDb)
        (\c -> auditorPackageName c ==. val_ x)

  mapM_ (\x -> exceptionCatcher (deleteCmd x) ClearAuditorTableError)
        allAuditorDeps
  close conn

deleteAuditorEntry :: String -> Auditor -> IO ()
deleteAuditorEntry dbName aud = do
  conn <- open dbName
  let deleteCmd = runBeamSqlite conn $ runDelete $ delete
        (auditor auditorDb)
        (\table -> auditorPackageName table ==. val_ (auditorPackageName aud))
  exceptionCatcher deleteCmd DeleteAuditorEntryError
  close conn

-- | Inserts original direct & indirect dependencies
-- directly into auditor table.
insertAuditorDeps
  :: String -> [ParsedDependency] -> EitherT OperationError IO OperationResult
insertAuditorDeps dbFilename pkgs = do
  let audExp = map parsedDepToAudQExpr pkgs
  conn <- liftIO $ open dbFilename
  let insertCMD =
        runBeamSqlite conn
          $ runInsert
          $ insert (auditor auditorDb)
          $ insertExpressions audExp
  liftIO $ exceptionCatcher insertCMD InsertAuditorDepsError
  liftIO $ close conn
  right AuditorDepsInserted

updateAuditorEntryDirect
  :: String -> Auditor -> EitherT OperationError IO OperationResult
updateAuditorEntryDirect dbName updatedPackage = do
  conn <- liftIO $ open dbName
  -- Updates corresponding package in auditor table.
  let updateCmd =
        runBeamSqlite conn $ runUpdate $ save (auditor auditorDb) updatedPackage
  liftIO $ exceptionCatcher updateCmd UpdateAuditorEntryError
  --let updateCmd =
  liftIO $ close conn
  pure UpdatedAuditorTableEntry

updateAuditorVersionChange
  :: String
  -> (DependencyName, Version)
  -> EitherT OperationError IO OperationResult
updateAuditorVersionChange dbName changedVersion = do
  -- Returns all dependencies (dir and indir) with this version.
  result <- queryAuditorSpecificVersion dbName changedVersion
  case result of
    -- | If the version exists in the database, we need
    -- to return all packages with the same name, change
    -- stillUsed to True for packages matching the updated version
    -- and false for the rest.

    VersionExistsInDb dirAndIndirEntries -> do
      -- Update stillUsed to True for entries of 'changedVersion'
      let updatedStillUsed = map
            (\audDep -> audDep { auditorStillUsed = True })
            dirAndIndirEntries
      mapM_ (updateAuditorEntryDirect dbName) updatedStillUsed

      -- Update stillUsed to False for all other versions
      allVersions <- getAllVersionsOfDep dbName $ fst changedVersion
      let oldVersions = map (\dep -> dep { auditorStillUsed = False }) $ filter
            (\dep -> auditorPackageVersion dep /= pack (snd changedVersion))
            allVersions
      mapM_ (updateAuditorEntryDirect dbName) oldVersions
      right SuccessfullyUpdatedVersion

    VersionDoesNotExistInDb -> do
      -- Update stillUsed to False for entries of all other existing versions
      deps <- getAllVersionsOfDep dbName $ fst changedVersion
      mapM_
        ( updateAuditorEntryDirect dbName
        . (\audDep -> audDep { auditorStillUsed = False })
        )
        deps

      -- Convert an existing Auditor entry to a ParsedDepenency
      -- so we can add new entries of the new version, add a new
      -- current time, and force a new unique package_id.

      pDeps <- firstEitherT ReadError . hoistEither $ mapM
        auditorEntryToParsedDep
        deps
      cTime <- liftIO getCurrentTime
      let newAuditorEntryVersions = map
            (\dep -> dep { inUse      = True
                         , depVersion = pack $ snd changedVersion
                         , firstSeen  = cTime
                         }
            )
            pDeps
      insertAuditorDeps dbName newAuditorEntryVersions
      right SuccessfullyUpdatedVersion
    _ -> error "updateAuditorVersionChange: Should not happen"


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Catches exceptions from 'Database.SQLite.Simple'.
exceptionCatcher :: IO () -> (String -> OperationError) -> IO ()
exceptionCatcher action errorConstr = catches
  action
  [ Handler (\(e :: FormatError) -> report . Left . errorConstr $ show e)
  , Handler (\(e :: ResultError) -> report . Left . errorConstr $ show e)
  , Handler (\(e :: SQLError) -> report . Left . errorConstr $ show e)
  ]
