{-# LANGUAGE OverloadedStrings #-}

module Audit.Generate
  ( commandHandler
  , audit
  , originalDirectDeps
  , update
  , initializeDB
  )
where

import Control.Monad.Trans.Either (runEitherT)
import Data.Hashable (hash)
import System.Directory (getDirectoryContents)
import System.Process (callCommand)

import Audit.Conversion (compareParsedWithAuditor)
import Audit.Database (AuditorT(..))
import Audit.Operations
  ( newParsedDeps
  , checkHash
  , deleteHash
  , insertAuditorDeps
  , insertHash
  , updateAuditorEntryDirect
  )
import Audit.Queries (queryAuditor)
import Audit.Sorting
  ( InitialDepVersions(..)
  , parseAllOriginalRepoVers
  , parseAllUpdatedRepoVers
  , initialDepTree
  , filterNewDirDeps
  , filterNewIndirectDeps
  , filterNewVersions
  , filterRemovedIndirectDeps
  , originalDirectDeps
  , filterRemovedDirDeps
  )
import Audit.Tree (directDeps, indirectDeps)
import Audit.Types
  (Command(..), HashStatus(..), OperationError(..), OperationResult(..))
import Data.Time.Clock (getCurrentTime)
import Data.Text (pack)
-- | Checks if "auditor.db" exists in pwd, if not creates it with the
-- tables `auditor` (which holds all the dependency data) and `hash`
-- which holds the hash of the existing .dot file.
-- If "auditor.db" is present, it generates a new `.dot` file and checks
-- the hash of the existing `.dot` file against the new `.dot` file generated.
checkDB :: IO HashStatus
checkDB = do
  filenames <- getDirectoryContents "."
  if "auditor.db" `elem` filenames
    then do
      print "auditor.db present"
      print "Generating new dot file and checking hash"
      callCommand "stack dot --external > repoinfo/updatedDepTree.dot"
      callCommand "stack ls dependencies > repoinfo/updatedDepTreeVersions.txt"
      contents <- (++) <$> readFile "repoinfo/updatedDepTree.dot" <*> readFile
        "repoinfo/updatedDepTreeVersions.txt"
      checkHash "auditor.db" $ hash contents
    else return HashNotFound

generateInitialDotFiles :: IO ()
generateInitialDotFiles = do
  callCommand "stack dot --external > repoinfo/currentDepTree.dot"
  callCommand "stack ls dependencies > repoinfo/currentDepTreeVersions.txt"

initializeDB :: String -> IO ()
initializeDB dbName =
  callCommand $
    "sqlite3 " ++ dbName ++ " \
    \\"CREATE TABLE auditor ( dependency_id INTEGER PRIMARY KEY AUTOINCREMENT\
                           \, package_name VARCHAR NOT NULL\
                           \, package_version VARCHAR NOT NULL\
                           \, date_first_seen VARCHAR NOT NULL\
                           \, direct_dep BOOL NOT NULL\
                           \, still_used BOOL NOT NULL\
                           \, analysis_status VARCHAR NOT NULL);\
     \CREATE TABLE hash ( current_hash INT NOT NULL\
                           \, PRIMARY KEY ( current_hash )); \
     \CREATE TABLE diff     ( dependency_id INTEGER PRIMARY KEY AUTOINCREMENT\
                           \, package_name VARCHAR NOT NULL\
                           \, package_version VARCHAR NOT NULL\
                           \, date_first_seen VARCHAR NOT NULL\
                           \, direct_dep VARCHAR NOT NULL\
                           \, still_used VARCHAR NOT NULL\
                           \, analysis_status VARCHAR NOT NULL); \""


------------------------------Handler-----------------------------------

commandHandler :: Command -> IO ()
commandHandler (Command cmd)
  | cmd == "audit" = audit
  | cmd == "load"  = update
  | otherwise      = print "Invalid command!"

audit :: IO ()
audit = do
  hashStat <- checkDB
  case hashStat of
    HashMatches -> do
      print
        "Hashes match, dependency tree has not \
                                 \been changed."
    HashDoesNotMatch -> do
      print "Hashes do not match, dependency tree has changed."
      filterNewDirDeps >>= (\x -> print $ "New direct dependencies: " ++ show x)
      filterNewIndirectDeps >>= (\x -> print $ "New indirect dependencies: " ++ show x)
      filterNewVersions >>= (\x -> print $ "New dependency versions: " ++ show x)
      filterRemovedDirDeps >>= (\x -> print $ "Removed direct dependencies: " ++ show x)
      filterRemovedIndirectDeps >>= (\x -> print $ "Removed indirect dependencies: " ++ show x)
      pure ()
    HashNotFound -> do
      print "Hash not found, generating db."
      initializeDB "auditor.db"
      generateInitialDotFiles
      pVersions <- parseAllOriginalRepoVers
      iDepTree  <- initialDepTree
      cTime <- getCurrentTime
      let
        packages = newParsedDeps
          (initDeps pVersions)
          (directDeps iDepTree)
          (indirectDeps iDepTree)
          cTime
      insertAuditorDeps "auditor.db" packages
      (++)
        <$> readFile "repoinfo/currentDepTree.dot"
        <*> readFile "repoinfo/currentDepTreeVersions.txt"
        >>= insertHash "auditor.db"
        .   hash


_report :: Either OperationError OperationResult -> IO ()
_report (Right opRes) = print $ show opRes
_report (Left  e    ) = print $ _renderOperationError e

_renderOperationError :: OperationError -> String
_renderOperationError (OnlyDirectDepenciesAllowed pkgs) =
  "The following packages are not direct dependencies: " <> show pkgs
_renderOperationError (OnlyIndirectDepenciesAllowed pkgs) =
  "The following packages are not indirect dependencies: " <> show pkgs
_renderOperationError (ConvError err) =
  "ConversionError: " <> show err

update :: IO ()
update = do
  -- New dependencies (Add to Auditor table)
  newVersions <- parseAllUpdatedRepoVers
  newDDeps     <- filterNewDirDeps
  newInDeps <- filterNewIndirectDeps
  cTime     <- getCurrentTime


  allAuditorEntries <- queryAuditor "auditor.db"

  -- Removed dependencies (change stillUsed flag)
  remDirDepsPackageNames <- filterRemovedDirDeps

  -- Removed indirect dependencies
  remIndirDeps <- filterRemovedIndirectDeps

  -- New dependencies
  let newDeps = newParsedDeps newVersions newDDeps newInDeps cTime

  case (newDeps, remDirDepsPackageNames, remIndirDeps) of
    ([],[],[]) -> print "No dependency updates detected."
    _  -> do
      -- OVERALL FLOW. QUERY AUDITOR TABLE, USE LIST COMPREHENSION & PARSE RESULTS TO FILTER
      -- THE ENTRY/ENTRIES YOU ARE LOOKING FOR.

      -- New dependencies consits of two cases: actual never before seen dependencies
      -- or when adding a direct dep (when the identical indirect dep exists)
      -- or vice versa

      -- Check to see if the new parsed deps results are already in auditor
      let checkForNew = map compareParsedWithAuditor newDeps
      let newDepsAlreadyInAuditor = [ aEntry {auditorStillUsed = True}
                                    | aEntry <- allAuditorEntries
                                    , f <- checkForNew
                                    , f aEntry == True
                                    ]
      let dirDepsToBeRemovedAuditor = [ aEntry { auditorStillUsed = False}
                                      | aEntry <- allAuditorEntries
                                      , auditorPackageName aEntry `elem` map pack remDirDepsPackageNames
                                      , auditorDirectDep aEntry == True
                                      ]
      let indirDepsToBeRemovedAuditor = [ aEntry {auditorStillUsed = False}
                                        | aEntry <- allAuditorEntries
                                        , auditorPackageName aEntry `elem` map pack remIndirDeps
                                        , auditorDirectDep aEntry == False
                                        ]
      case (newDepsAlreadyInAuditor,dirDepsToBeRemovedAuditor, indirDepsToBeRemovedAuditor) of
        -- Insert actual new dependencies
        ([],[],[]) -> do
          print $ "Inserting new dependencies:" ++ show newDDeps
          insertAuditorDeps "auditor.db" newDeps
          cleanUpAndUpdateHash
        -- Update existing dependencies/ Add new dependencies (see above)
        _ -> do
            -- If this is a new dep already in the auditor, it means we have
            -- re-added a direct dependency and need to adjust the stillUsed bool

            mapM_ (runEitherT . updateAuditorEntryDirect "auditor.db") newDepsAlreadyInAuditor

            -- Update removed dependencies (removedDirs = change DirectDep flag to false)
            mapM_ (runEitherT . updateAuditorEntryDirect "auditor.db") dirDepsToBeRemovedAuditor

            -- Update removed indirect dependencies
            mapM_ (runEitherT . updateAuditorEntryDirect "auditor.db") indirDepsToBeRemovedAuditor

            print
              "Overwriting original repository dependency \
                              \tree & clearing Diff table"
            cleanUpAndUpdateHash

cleanUpAndUpdateHash :: IO ()
cleanUpAndUpdateHash = do
  callCommand "stack dot --external > repoinfo/currentDepTree.dot"
  print "Deleting old hash."
  deleteHash "auditor.db"
  -- `contents` will become the new hash
  contents <- (++) <$> readFile "repoinfo/updatedDepTree.dot" <*> readFile
    "repoinfo/updatedDepTreeVersions.txt"
  -- Update hash in hash table
  insertHash "auditor.db" $ hash contents
  -- Delete the new dependencies (that was just added to the auditor table)
  -- in the Diff table
  callCommand "rm repoinfo/updatedDepTreeVersions.txt"
  callCommand "rm repoinfo/updatedDepTree.dot"