{-# LANGUAGE OverloadedStrings #-}

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
                                                )



--------------------------------------------------------------------------------
-- Hash Table Operations
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Auditor Table Operations
--------------------------------------------------------------------------------

-- | Delete the dependencies in the auditor table.
clearAuditorTable :: String -> IO ()
clearAuditorTable fp = do
  conn           <- open fp
  allAuditorDeps <- queryAuditorDepNames fp
  mapM_
    (\x -> runBeamSqlite conn $ runDelete $ delete
      (auditor auditorDb)
      (\c -> auditorPackageName c ==. val_ x)
    )
    allAuditorDeps
  close conn


deleteAuditorEntry :: String -> Auditor -> IO ()
deleteAuditorEntry dbName aud = do
  conn <- open dbName
  runBeamSqlite conn $ runDelete $ delete
    (auditor auditorDb)
    (\table -> auditorPackageName table ==. val_ (auditorPackageName aud))
  close conn

-- | Inserts original direct & indirect dependencies
-- directly into auditor table.
insertAuditorDeps :: String -> [ParsedDependency] -> IO ()
insertAuditorDeps dbFilename pkgs = do
  let audExp = map parsedDepToAudQExpr pkgs
  conn <- open dbFilename
  runBeamSqlite conn
    $ runInsert
    $ insert (auditor auditorDb)
    $ insertExpressions audExp
  close conn

updateAuditorEntryDirect
  :: String -> Auditor -> EitherT OperationError IO OperationResult
updateAuditorEntryDirect dbName updatedPackage = do
  conn <- liftIO $ open dbName
  -- Updates corresponding package in auditor table.
  liftIO $ runBeamSqlite conn $ runUpdate $ save (auditor auditorDb)
                                                 updatedPackage

  liftIO $ close conn
  pure UpdatedAuditorTableStillUsed

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
      let oldVersions = map (\dep -> dep {auditorStillUsed = False}) $ filter (\dep -> auditorPackageVersion dep /= (pack $ snd changedVersion)) allVersions
      mapM_ (updateAuditorEntryDirect dbName) oldVersions
      pure SuccessfullyUpdatedVersion

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
      liftIO $ insertAuditorDeps dbName newAuditorEntryVersions
      right SuccessfullyUpdatedVersion
    _ -> error "updateAuditorVersionChange: Should not happen"


