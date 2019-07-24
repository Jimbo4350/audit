{-# LANGUAGE OverloadedStrings #-}

module Audit.Generate
  ( commandHandler
  , audit
  , cleanUpAndUpdateHash
  , originalDirectDeps
  , update
  , initializeDB
  )
where

import           Audit.Conversion               ( auditorEntryToNotUsed
                                                , newParsedDeps
                                                , updatedAuditorValues
                                                )
import           Audit.Operations               ( checkHash
                                                , deleteHash
                                                , insertAuditorDeps
                                                , insertHash
                                                , updateAuditorEntryDirect
                                                , updateAuditorVersionChange
                                                )
import           Audit.Queries                  ( queryAuditor
                                                , getDirAudEntryByDepName
                                                , getInDirAudEntryByDepName
                                                )
import           Audit.Sorting                  ( InitialDepVersions(..)
                                                , parseAllOriginalRepoVers
                                                , parseAllUpdatedRepoVers
                                                , initialDepTree
                                                , filterNewDirDeps
                                                , filterNewIndirectDeps
                                                , filterVersionChanges
                                                , filterRemovedIndirectDeps
                                                , originalDirectDeps
                                                , filterRemovedDirDeps
                                                )
import           Audit.Tree                     ( directDeps
                                                , indirectDeps
                                                )
import           Audit.Types                    ( Command(..)
                                                , HashStatus(..)
                                                , OperationError(..)
                                                , OperationResult(..)
                                                , UpdatedDepVersions(..)
                                                , report
                                                )

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.Either     ( EitherT
                                                , right
                                                , runEitherT
                                                )
import           Data.Hashable                  ( hash )
import           Data.Time.Clock                ( getCurrentTime )
import           System.Directory               ( getDirectoryContents )
import           System.Process                 ( callCommand )



-- | Checks if "auditor.db" exists in pwd, if not creates it with the
-- tables `auditor` (which holds all the dependency data) and `hash`
-- which holds the hash of the existing .dot file.
-- If "auditor.db" is present, it generates a new `.dot` file and checks
-- the hash of the existing `.dot` file against the new `.dot` file generated.
checkDB :: EitherT OperationError IO HashStatus
checkDB = do
  filenames <- liftIO $ getDirectoryContents "."
  if "auditor.db" `elem` filenames
    then do
      liftIO $ print "auditor.db present"
      liftIO $ print "Generating new dot file and checking hash"
      liftIO $ callCommand "stack dot --external > repoinfo/updatedDepTree.dot"
      liftIO $ callCommand
        "stack ls dependencies --test > repoinfo/updatedDepTreeVersions.txt"
      contents <-
        (++)
        <$> (liftIO $ readFile "repoinfo/updatedDepTree.dot")
        <*> (liftIO $ readFile "repoinfo/updatedDepTreeVersions.txt")
      checkHash "auditor.db" $ hash contents
    else return HashNotFound

generateInitialDotFiles :: IO ()
generateInitialDotFiles = do
  callCommand "stack dot --external > repoinfo/currentDepTree.dot"
  callCommand
    "stack ls dependencies --test > repoinfo/currentDepTreeVersions.txt"

initializeDB :: String -> IO ()
initializeDB dbName =
  callCommand
    $ "sqlite3 "
    ++ dbName
    ++ " \
    \\"CREATE TABLE auditor ( dependency_id INTEGER PRIMARY KEY AUTOINCREMENT\
                           \, package_name VARCHAR NOT NULL\
                           \, package_version VARCHAR NOT NULL\
                           \, date_first_seen VARCHAR NOT NULL\
                           \, direct_dep BOOL NOT NULL\
                           \, still_used BOOL NOT NULL\
                           \, analysis_status VARCHAR NOT NULL);\
     \CREATE TABLE hash ( current_hash INT NOT NULL\
                           \, PRIMARY KEY ( current_hash )); \""

------------------------------Handler-----------------------------------

commandHandler :: Command -> IO ()
commandHandler (Command cmd)
  | cmd == "audit" = do
    result <- runEitherT audit
    report result
  | cmd == "load" = do
    result <- runEitherT update
    report result
  | otherwise = print "Invalid command!"

audit :: EitherT OperationError IO OperationResult
audit = do
  hashStat <- checkDB
  case hashStat of
    HashMatches -> do
      liftIO $ print "Hashes match, dependency tree has not been changed."
      right AuditHashMatches
    HashDoesNotMatch -> do
      liftIO $ print "Hashes do not match, dependency tree has changed."
      newDirDeps <- liftIO filterNewDirDeps
      liftIO . print $ "New direct dependencies: " ++ show newDirDeps
      newIndirDeps <- liftIO filterNewIndirectDeps
      liftIO . print $ "New indirect dependencies: " ++ show newIndirDeps
      vChanges <- liftIO filterVersionChanges
      liftIO . print $ "New dependency versions: " ++ show vChanges
      remDirDeps <- liftIO filterRemovedDirDeps
      liftIO . print $ "Removed direct dependencies: " ++ show remDirDeps
      remInDeps <- liftIO filterRemovedIndirectDeps
      liftIO . print $ "Removed indirect dependencies: " ++ show remInDeps
      right AuditHashDoesNotMatch
    HashNotFound -> do
      liftIO $ print "Hash not found, generating db."
      liftIO $ initializeDB "auditor.db"
      liftIO generateInitialDotFiles
      pVersions <- liftIO parseAllOriginalRepoVers
      iDepTree  <- liftIO initialDepTree
      cTime     <- liftIO getCurrentTime
      let packages = newParsedDeps (initDeps pVersions)
                                   (directDeps iDepTree)
                                   (indirectDeps iDepTree)
                                   cTime
      insResult <- insertAuditorDeps "auditor.db" packages
      liftIO . report $ pure insResult
      contents <-
        (++)
        <$> (liftIO $ readFile "repoinfo/currentDepTree.dot")
        <*> (liftIO $ readFile "repoinfo/currentDepTreeVersions.txt")
      hInsResult <- insertHash "auditor.db" $ hash contents
      liftIO . report $ pure hInsResult
      right AuditHashNotFound

update :: EitherT OperationError IO OperationResult
update = do
  -- New dependencies (Add to Auditor table)
  newVersions            <- liftIO parseAllUpdatedRepoVers
  newDDeps               <- liftIO filterNewDirDeps
  newInDeps              <- liftIO filterNewIndirectDeps
  cTime                  <- liftIO getCurrentTime


  allAuditorEntries      <- liftIO $ queryAuditor "auditor.db"

  -- Removed dependencies (change stillUsed flag)
  remDirDepsPackageNames <- liftIO filterRemovedDirDeps

  -- Removed indirect dependencies
  remIndirDeps           <- liftIO filterRemovedIndirectDeps

  -- New dependencies
  let newDeps =
        newParsedDeps (updatedDeps newVersions) newDDeps newInDeps cTime

  -- Version Changes
  vChanges <- liftIO $ filterVersionChanges

  case (newDeps, remDirDepsPackageNames, remIndirDeps, vChanges) of
    ([], [], [], []) -> right NoDependencyUpdatesDetected
    _                -> do
      -- OVERALL FLOW. QUERY AUDITOR TABLE, USE LIST COMPREHENSION & PARSE RESULTS TO FILTER
      -- THE ENTRY/ENTRIES YOU ARE LOOKING FOR.

      -- New dependencies consits of two cases: actual never before seen dependencies
      -- or when adding a direct dep (when the identical indirect dep exists)
      -- or vice versa

      -- Check to see if the new parsed deps results are already in auditor
      let newDepsAlreadyInAuditor =
            updatedAuditorValues allAuditorEntries newDeps

      -- Checks all auditor enteries, checks that the package name exists in the remDirDepsPAcakageNames
      -- list then also checks that the value from the auditor is inface a direct dependency. You need
      -- to run getDirAudEntryByDepName on every value of remDirDepsPackageNames and then propagate an error
      -- if the package does not exist in the auditor table. This needs to be done for indirDeps aswell
      -- so your function should be agnostic.....
      eitherDirDepsToBeRemovedAuditor <- mapM
        (getDirAudEntryByDepName "auditor.db")
        remDirDepsPackageNames
      let dirDepsToBeRemovedAuditor =
            map auditorEntryToNotUsed eitherDirDepsToBeRemovedAuditor

      eitherIndirDepsToBeRemovedAuditor <- mapM
        (getInDirAudEntryByDepName "auditor.db")
        remIndirDeps
      let indirDepsToBeRemovedAuditor =
            map auditorEntryToNotUsed eitherIndirDepsToBeRemovedAuditor

      case
          ( newDepsAlreadyInAuditor
          , dirDepsToBeRemovedAuditor
          , indirDepsToBeRemovedAuditor
          , vChanges
          )
        of
        -- Insert actual new dependencies
          ([], [], [], []) -> do
            liftIO . print $ "Inserting new dependencies:" ++ show newDDeps
            insResult <- insertAuditorDeps "auditor.db" newDeps
            liftIO . report $ pure insResult
            cleanUpAndUpdateHash
          -- Update existing dependencies/ Add new dependencies (see above)
          _ -> do
              -- If this is a new dep already in the auditor, it means we have
              -- re-added a direct dependency and need to adjust the stillUsed bool

            updateResults <- mapM (updateAuditorEntryDirect "auditor.db") newDepsAlreadyInAuditor
            mapM_ (liftIO . report . pure) updateResults

            -- Update removed dependencies (removedDirs = change DirectDep flag to false)
            removedDirResults <- mapM (updateAuditorEntryDirect "auditor.db")
                 dirDepsToBeRemovedAuditor
            mapM_ (liftIO . report . pure) removedDirResults

            -- Update removed indirect dependencies
            removedIndirResults <- mapM (updateAuditorEntryDirect "auditor.db")
                 indirDepsToBeRemovedAuditor
            mapM_ (liftIO . report . pure) removedIndirResults

            -- Version change updates
            vChangeResults <- mapM (updateAuditorVersionChange "auditor.db") vChanges
            mapM_ (liftIO . report . pure) vChangeResults

            liftIO
              $ print
                  "Overwriting original repository dependency \
                              \tree & clearing Diff table"
            cleanUpAndUpdateHash

cleanUpAndUpdateHash :: EitherT OperationError IO OperationResult
cleanUpAndUpdateHash = do
  liftIO generateInitialDotFiles
  liftIO $ print "Deleting old hash."
  delResult <- deleteHash "auditor.db"
  liftIO . report $ pure delResult
  -- `contents` will become the new hash
  contents <-
    (++)
    <$> (liftIO $ readFile "repoinfo/updatedDepTree.dot")
    <*> (liftIO $ readFile "repoinfo/updatedDepTreeVersions.txt")
  -- Update hash in hash table
  hResult <- insertHash "auditor.db" $ hash contents
  liftIO . report $ pure hResult
  -- Delete the new dependency tree.
  liftIO $ callCommand "rm repoinfo/updatedDepTreeVersions.txt"
  liftIO $ callCommand "rm repoinfo/updatedDepTree.dot"
  right CleanUpAndUpdateHash
