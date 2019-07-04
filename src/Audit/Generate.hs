{-# LANGUAGE OverloadedStrings #-}

module Audit.Generate
  ( commandHandler
  , audit
  , originalDirectDeps
  , update
  )
where

import Control.Monad.Trans.Either (runEitherT)
import Data.Bifunctor (bimap)
import Data.Hashable (hash)
import Data.Text (unpack)
import System.Directory (getDirectoryContents)
import System.Process (callCommand)

import Audit.Operations
  ( buildPackageList
  , checkHash
  , deleteHash
  , insertAuditorDeps
  , insertHash
  , loadDiffIntoAuditorNew
  , loadDiffIntoAuditorUpdate
  )
import Audit.Sorting
  ( allOriginalRepoVers
  , allUpdatedRepoVers
  , initialDepTree
  , newDirDeps
  , newIndirectDeps
  , newVersions
  , originalDirectDeps
  , removedDeps
  )
import Audit.Database (DiffT(..))
import Audit.Tree (directDeps, indirectDeps)
import Audit.Types
  (Command(..), HashStatus(..), OperationError(..), OperationResult(..),Package(..))
import Data.Time.Clock (getCurrentTime)
import Audit.DiffOperations
  ( clearDiffTable
  , loadNewDirDepsDiff
  , loadNewIndirectDepsDiff
  , loadDiffTableRemovedDeps
  , queryDiff'
  )

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

generateInitialDepFiles :: IO ()
generateInitialDepFiles = do
  callCommand "stack dot --external > repoinfo/currentDepTree.dot"
  callCommand "stack ls dependencies > repoinfo/currentDepTreeVersions.txt"

initializeDB :: IO ()
initializeDB =
  callCommand
    "sqlite3 auditor.db \
    \\"CREATE TABLE auditor ( package_id INTEGER PRIMARY KEY AUTOINCREMENT\
                           \, package_name VARCHAR NOT NULL\
                           \, package_version VARCHAR NOT NULL\
                           \, date_first_seen VARCHAR NOT NULL\
                           \, direct_dep VARCHAR NOT NULL\
                           \, still_used VARCHAR NOT NULL\
                           \, analysis_status VARCHAR NOT NULL);\
     \CREATE TABLE hash ( current_hash INT NOT NULL\
                           \, PRIMARY KEY ( current_hash )); \
     \CREATE TABLE diff ( package_name VARCHAR NOT NULL\
                           \, package_version VARCHAR NOT NULL\
                           \, date_first_seen VARCHAR NOT NULL\
                           \, direct_dep VARCHAR NOT NULL\
                           \, still_used VARCHAR NOT NULL\
                           \, analysis_status VARCHAR NOT NULL\
                           \, PRIMARY KEY( package_name, direct_dep )); \""


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
      clearDiffTable "auditor.db"
      print
        "Hashes match, dependency tree has not \
                                 \been changed."
    HashDoesNotMatch -> do
      print "Hashes do not match, dependency tree has changed."
      newDirDeps >>= (\x -> print $ "New dependencies: " ++ show x)
      newVersions >>= (\x -> print $ "New dependency versions: " ++ show x)
      removedDeps >>= (\x -> print $ "Removed dependencies: " ++ show x)
      pVersions <- allUpdatedRepoVers
      dDeps     <- newDirDeps
      newInDeps <- newIndirectDeps

      cTime     <- getCurrentTime
      -- Add new direct dependencies to the Diff table
      -- TODO: shift direct and indirect checks here and create custom
      -- types for them to feed into loadNewDir* functions
      queriedDiffDeps <- queryDiff' "auditor.db"
      report =<< runEitherT
        ( loadNewDirDepsDiff "auditor.db"
        $ buildPackageList pVersions dDeps [] cTime
        )

      -- Add new indirect dependencies to the Diff table
      report =<< runEitherT
        ( loadNewIndirectDepsDiff "auditor.db"
        $ buildPackageList pVersions [] newInDeps cTime
        )

      report =<< (runEitherT $ loadDiffTableRemovedDeps "auditor.db")
    HashNotFound -> do
      print "Hash not found, generating db."
      initializeDB
      generateInitialDepFiles
      pVersions <- allOriginalRepoVers
      iDepTree  <- initialDepTree
      let pVersions' = [ bimap unpack unpack x | x <- pVersions ]
      cTime <- getCurrentTime
      let
        packages = buildPackageList
          pVersions'
          (directDeps iDepTree)
          (indirectDeps iDepTree)
          cTime
      insertAuditorDeps "auditor.db" packages
      (++)
        <$> readFile "repoinfo/currentDepTree.dot"
        <*> readFile "repoinfo/currentDepTreeVersions.txt"
        >>= insertHash "auditor.db"
        .   hash


report :: Either OperationError OperationResult -> IO ()
report (Right opRes) = putStrLn $ show opRes
report (Left  e    ) = putStrLn $ renderOperationError e

renderOperationError :: OperationError -> String
renderOperationError (OnlyDirectDepenciesAllowed pkgs) =
  "The following packages are not direct dependencies: " <> show pkgs
renderOperationError (OnlyIndirectDepenciesAllowed pkgs) =
  "The following packages are not indirect dependencies: " <> show pkgs
renderOperationError (ConvError err) =
  "ConversionError: " <> show err

update :: IO ()
update = do
  entries <- queryDiff' "auditor.db"
  case entries of
    [] -> print "Diff table empty"
    _  -> do
      print
        "Inserting changes from Diff table into \
                        \Auditor table.."
      _ <- runEitherT $ loadDiffIntoAuditorNew "auditor.db"
    --  _ <- runEitherT $ loadDiffIntoAuditorUpdate "auditor.db"
      print
        "Overwriting original repository dependency \
                        \tree & clearing Diff table"
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
      clearDiffTable "auditor.db"
      callCommand "rm repoinfo/updatedDepTreeVersions.txt"
      callCommand "rm repoinfo/updatedDepTree.dot"
