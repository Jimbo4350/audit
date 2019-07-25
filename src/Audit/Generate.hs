{-# LANGUAGE OverloadedStrings #-}

module Audit.Generate
  ( commandHandler
  , audit
  , cleanUpAndUpdateHash
  , genParsedDepsAndHash
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
                                                , filterNewDirDeps
                                                , filterNewIndirectDeps
                                                , filterAllOriginalRepoIndirDeps
                                                , filterVersionChanges
                                                , filterRemovedIndirectDeps
                                                , originalDirectDeps
                                                , parseRepoName
                                                , filterRemovedDirDeps
                                                )
import           Audit.Types                    ( Command(..)
                                                , HashStatus(..)
                                                , OperationError(..)
                                                , OperationResult(..)
                                                , ParsedRepo(..)
                                                , RepoName
                                                , UpdatedDepVersions(..)
                                                , report
                                                , reportMultiple
                                                )

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans.Either     ( EitherT
                                                , right
                                                , runEitherT
                                                )
import           Data.Hashable                  ( hash )
import           Data.List                      ( isSuffixOf
                                                , notElem
                                                )
import           Data.Time.Clock                ( getCurrentTime )
import           System.Directory               ( getCurrentDirectory
                                                , getDirectoryContents
                                                , makeAbsolute
                                                , setCurrentDirectory
                                                )
import           System.Process                 ( callCommand )

audit :: EitherT OperationError IO [OperationResult]
audit = do
  liftIO $ makeAbsolute "allRepos/" >>= setCurrentDirectory
  hashStat <- checkDBHashAndGenUpdatedFilesHashAndGenUpdatedFiles
  mapM hashStatus hashStat

-- | Checks if "auditor.db" exists in pwd, if not creates it with the
-- tables `auditor` (which holds all the dependency data) and `hash`
-- which holds the hash of the existing .dot file.
-- If "auditor.db" is present, it generates a new `.dot` file and checks
-- the hash of the existing `.dot` file against the new `.dot` file generated.
checkDBHashAndGenUpdatedFilesHashAndGenUpdatedFiles :: EitherT OperationError IO [HashStatus]
checkDBHashAndGenUpdatedFilesHashAndGenUpdatedFiles = do
  filenamesAndRepoNames <- liftIO $ getDirectoryContents "."
  if "auditor.db" `elem` filenamesAndRepoNames
    then do
      liftIO $ print "auditor.db present"
      liftIO $ print "Generating new dot files and checking hashes"
      -- Need to generate .dot files (dependency tree) and .txt files
      -- (package versions) for each repo directory here.
      let onlyRepoNames = filterRepoNames filenamesAndRepoNames
      liftIO . print $ show onlyRepoNames
      let generateDotTextFiles repoFolder = do
            liftIO $ setCurrentDirectory repoFolder
            liftIO . callCommand $ "stack dot --external > updatedDepTree.dot"
            liftIO
              . callCommand
              $ "stack ls dependencies --test > updatedDepTreeVersions.txt"
            contents <-
              (++)
              <$> (liftIO . readFile $ "updatedDepTree.dot")
              <*> (liftIO . readFile $ "updatedDepTreeVersions.txt")
            liftIO $ setCurrentDirectory ".."
            checkHash "auditor.db" (hash contents) repoFolder
      mapM generateDotTextFiles onlyRepoNames
    else return [HashNotFound]

hashStatus :: HashStatus -> EitherT OperationError IO OperationResult
hashStatus hStat = case hStat of
  HashMatches rName -> do
    liftIO $ print "Hashes match, dependency tree has not been changed."
    right $ AuditHashMatches rName
  HashDoesNotMatch rName -> do
    liftIO . print $ rName ++ " cabal file has changed."

    liftIO $ setCurrentDirectory rName

    newDirDeps <- liftIO
      $ filterNewDirDeps "currentDepTree.dot" "updatedDepTree.dot"
    liftIO . print $ "New direct dependencies: " ++ show newDirDeps

    newIndirDeps <- liftIO
      $ filterNewIndirectDeps "currentDepTree.dot" "updatedDepTree.dot"
    liftIO . print $ "New indirect dependencies: " ++ show newIndirDeps

    vChanges <- liftIO filterVersionChanges
    liftIO . print $ "New dependency versions: " ++ show vChanges
    remDirDeps <- liftIO filterRemovedDirDeps
    liftIO . print $ "Removed direct dependencies: " ++ show remDirDeps
    remInDeps <- liftIO filterRemovedIndirectDeps
    liftIO . print $ "Removed indirect dependencies: " ++ show remInDeps

    liftIO $ setCurrentDirectory ".."

    right $ AuditHashDoesNotMatch rName
  HashNotFound -> do

    -- Initialize auditor.db
    liftIO $ print "Hash not found, generating db."
    liftIO $ initializeDB "auditor.db"

    -- Get curr dir so that you can update auditor.db
    -- that sits in `allRepos`
    liftIO $ print "CurrentDir after initializeDb:"
    allRepoDir <- liftIO getCurrentDirectory
    liftIO $ print allRepoDir


    -- Need to generate intial dot files for all repositories
    -- in the allRepos folder. In here the directory is changed
    -- to each of the repo directories in allRepos and a list of
    -- ParsedDependencys is created for each repo and returned here.
    -- Note the last repo where the initial dot files were created
    -- will be the directory that the program is in. Therefore you must
    -- change it back to allRepos dir in order to update the auditor

    -- Generate .dot and .txt files for all the repos
    -- ParsedDependency has an additional field to accommodate
    -- the name of the repo which will be added to the SQL table
    SuccessfullyParsedRepos pkgsAndHashes <- generateInitialParsedDepsAndHashes

    liftIO $ setCurrentDirectory allRepoDir

     --TODO: insertAuditorDeps will also need an additional parameter
    -- to indicate which repo dir the deps came from [priority:1]

    -- Packages are empty because they are not reading from the
    -- dot and text files defined in cardano node.
    insResult <- mapM (insertAuditorDeps "auditor.db")
                      (map parsedDeps pkgsAndHashes)
    liftIO $ print insResult
    --TODO: Include which deps were inserted in the constructor AuditorDepsInserted [priority:3]
    liftIO . reportMultiple $ pure insResult
    hInsResult <- mapM
      (uncurry $ insertHash "auditor.db")
      (map (\x -> (repoHash x, parsedRepoName x)) pkgsAndHashes)
    liftIO . reportMultiple $ pure hInsResult
    right AuditHashNotFound

-- Currently propagating the hash of the readFile results to `audit` command
generateInitialParsedDepsAndHashes :: EitherT OperationError IO OperationResult
generateInitialParsedDepsAndHashes = do
  filenamesAndRepoNames <- liftIO $ getDirectoryContents "."
  let onlyRepoNames =
        [ n
        | n <- filenamesAndRepoNames
        , not $ ".dot" `isSuffixOf` n
        , not $ ".txt" `isSuffixOf` n
        , n `notElem` [".", "..", "auditor.db"]
        ]
  liftIO $ print "Current Repos in allRepos dir: "
  liftIO $ mapM print onlyRepoNames

  --TODO: Now you need to apply the commands below
  -- inside each repo in the allRepos folder. Note
  -- you need to also implement a different audit command
  -- that allows you to generate the relevant files and db updates
  -- from a repo filepath
  liftIO $ SuccessfullyParsedRepos <$> mapM genParsedDepsAndHash onlyRepoNames

-- TODO: Change this string to a Hash, ie compute the hash here [priority:1]
genParsedDepsAndHash :: String -> IO ParsedRepo
genParsedDepsAndHash repoName = do
  makeAbsolute repoName >>= setCurrentDirectory
  callCommand "stack dot --external > currentDepTree.dot"
  callCommand "stack ls dependencies --test > currentDepTreeVersions.txt"
  -- TODO: All of the parsing functions will need to be
  -- parameterized based on the repo dir [priority:1]
  pVersions <- liftIO $ parseAllOriginalRepoVers "currentDepTreeVersions.txt"
  --TODO: This is your issue here, initialDepTree. The tree is too big and the indirect dep function
  -- tries to build the entire ABSOLUTE dep tree with duplicated data
  -- which is too much in the case of "cardano-node"  (quadratic complexity in building the dep tree)
  -- resort to the results of parsing function that have been VERIFIED against stack ls dependencies
  -- Only use the tree for the shortest path to a dep. Maybe you can tag the results of the parsed
  -- dependencies. FOR NOW FOCUS ON INDIRECT AND DIRECT DEPS I.E TREE LEVEL 1 & 2  [priority:1]

  -- This should be managable and get you back on track to finish the multiple repo monitoring
  -- and then onto model testing.

  dirDeps   <- originalDirectDeps "currentDepTree.dot"
  indirDeps <- filterAllOriginalRepoIndirDeps "currentDepTree.dot"
  cTime     <- getCurrentTime
  repoName  <- parseRepoName "currentDepTree.dot"
  let packages =
        newParsedDeps repoName (initDeps pVersions) dirDeps indirDeps cTime
  contents <-
    (++)
    <$> (liftIO $ readFile "currentDepTree.dot")
    <*> (liftIO $ readFile "currentDepTreeVersions.txt")
  -- Update hash in hash table
  setCurrentDirectory ".."
  return $ ParsedRepo packages (hash contents) repoName

initializeDB :: String -> IO ()
initializeDB dbName =
  callCommand
    $ "sqlite3 "
    ++ dbName
    ++ " \
    \\"CREATE TABLE auditor ( dependency_id INTEGER PRIMARY KEY AUTOINCREMENT\
                           \, repo_name VARCHAR NOT NULL\
                           \, package_name VARCHAR NOT NULL\
                           \, package_version VARCHAR NOT NULL\
                           \, date_first_seen VARCHAR NOT NULL\
                           \, direct_dep BOOL NOT NULL\
                           \, still_used BOOL NOT NULL\
                           \, analysis_status VARCHAR NOT NULL);\
     \CREATE TABLE hash ( repo_name VARCHAR NOT NULL\
                           \, current_hash INT NOT NULL\
                           \, PRIMARY KEY ( current_hash )); \""

------------------------------Handler-----------------------------------

commandHandler :: Command -> IO ()
commandHandler (Command cmd)
  | cmd == "audit" = do
    result <- runEitherT audit
    reportMultiple result
  | cmd == "load" = do
    result <- runEitherT updateAll
    reportMultiple result
  | otherwise = print "Invalid command!"


updateAll :: EitherT OperationError IO [OperationResult]
updateAll = do
  liftIO $ makeAbsolute "allRepos/" >>= setCurrentDirectory
  filenamesAndRepoNames <- liftIO $ getDirectoryContents "."
  let onlyRepoNames = filterRepoNames filenamesAndRepoNames
  mapM update onlyRepoNames


-- | Parses the dependency files (.dot & .txt) for changes
-- then updates the db.
update :: RepoName -> EitherT OperationError IO OperationResult
update rName = do
  liftIO $ makeAbsolute rName >>= setCurrentDirectory
  -- New dependencies (Add to Auditor table)
  newVersions <- liftIO parseAllUpdatedRepoVers
  newDDeps    <- liftIO $ filterNewDirDeps "currentDepTree.dot" "updatedDepTree.dot"
  newInDeps <- liftIO $ filterNewIndirectDeps "currentDepTree.dot" "updatedDepTree.dot"
  cTime                  <- liftIO getCurrentTime


  -- Removed dependencies (change stillUsed flag)
  remDirDepsPackageNames <- liftIO filterRemovedDirDeps

  -- Removed indirect dependencies
  remIndirDeps           <- liftIO filterRemovedIndirectDeps

  -- Version Changes
  vChanges <- liftIO filterVersionChanges

  -- Create new dependencies
  let newDeps = newParsedDeps rName
                              (updatedDeps newVersions)
                              newDDeps
                              newInDeps
                              cTime
  liftIO $ setCurrentDirectory ".."

  allAuditorEntries      <- liftIO $ queryAuditor "auditor.db"

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
      -- list then also checks that the value from the auditor is infact a direct dependency. You need
      -- to run getDirAudEntryByDepName on every value of remDirDepsPackageNames and then propagate an error
      -- if the package does not exist in the auditor table. This needs to be done for indirDeps aswell.

      -- NB: getDirAudEntryByDepName can short circuit the whole process if the package cannot be found in
      -- the DB
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

      -- Insert actual new dependencies
      liftIO . print $ "Inserting new dependencies:" ++ show newDDeps
      insResult <- insertAuditorDeps "auditor.db" newDeps
      liftIO . report $ pure insResult

      -- Update existing dependencies/ Add new dependencies (see above)

      -- If this is a new dep already in the auditor, it means we have
      -- re-added a direct dependency and need to adjust the stillUsed bool

      updateResults <- mapM (updateAuditorEntryDirect "auditor.db")
                            newDepsAlreadyInAuditor
      liftIO $ print updateResults
      mapM_ (liftIO . report . pure) updateResults

      -- Update removed dependencies (removedDirs = change DirectDep flag to false)
      removedDirResults <- mapM (updateAuditorEntryDirect "auditor.db")
                                dirDepsToBeRemovedAuditor
      mapM_ (liftIO . report . pure) removedDirResults

      -- Update removed indirect dependencies
      removedIndirResults <- mapM
        (updateAuditorEntryDirect "auditor.db")
        indirDepsToBeRemovedAuditor
      mapM_ (liftIO . report . pure) removedIndirResults

      -- Version change updates
      liftIO . print $ show vChanges
      vChangeResults <- mapM (updateAuditorVersionChange "auditor.db")
                             vChanges
      liftIO . print $ show vChangeResults
      mapM_ (liftIO . report . pure) vChangeResults
      liftIO
        $ print
            "Overwriting original repository dependency \
                        \tree & clearing Diff table"
      cleanUpAndUpdateHash rName

-- | cleanUpAndUpdateHash:
-- Calculates the hash of the updated repo dependency files
-- Removes the updated repo dependency files
-- Generates new dependency files (to reflect the new state of the repo)
-- Updates the old hash of respective repo in the db.
cleanUpAndUpdateHash :: RepoName -> EitherT OperationError IO OperationResult
cleanUpAndUpdateHash rName = do
  liftIO $ makeAbsolute rName >>= setCurrentDirectory

  -- Get contents to derive the hash of the new dep tree
  -- i.e `contents` will become the new hash
  contents <-
    (++)
    <$> (liftIO $ readFile "updatedDepTree.dot")
    <*> (liftIO $ readFile "updatedDepTreeVersions.txt")

  -- Delete the new dependency tree.
  liftIO $ callCommand "rm updatedDepTreeVersions.txt"
  liftIO $ callCommand "rm updatedDepTree.dot"

  -- Create new current dep tree
  liftIO $ callCommand "stack dot --external > currentDepTree.dot"
  liftIO $ callCommand "stack ls dependencies --test > currentDepTreeVersions.txt"

  -- Go back to allRepos/ to update auditor.db
  liftIO $ setCurrentDirectory ".."

  -- Delete old hash
  liftIO $ print "Deleting old hash."
  delResult <- deleteHash "auditor.db" rName
  liftIO . report $ pure delResult

  -- Update hash in hash table
  hResult <- insertHash "auditor.db" (hash contents) rName
  liftIO . report $ pure hResult

  right CleanUpAndUpdateHash


filterRepoNames :: [String] -> [String]
filterRepoNames filenames = filter
  (\x ->
    x
      `notElem` [".", "..", "auditor.db"]
      &&        (not $ ".dot" `isSuffixOf` x)
      &&        (not $ ".txt" `isSuffixOf` x)
  )
  filenames
