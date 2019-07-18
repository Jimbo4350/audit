{-# LANGUAGE TemplateHaskell #-}


module Test.Audit.Properties where


import           Audit.Generate                 ( initializeDB )
import           Audit.Conversion               ( packageToParsedDep
                                                , auditorEntryToParsedDep
                                                , compareParsedWithAuditor
                                                , newParsedDeps
                                                , returnUpdatedAuditorEntry
                                                , updatedAuditorValues
                                                , auditorEntryToNotUsed
                                                )
import           Audit.Database                 ( Auditor
                                                , AuditorT(..)
                                                , HashT(..)
                                                )
import           Audit.Operations               ( clearAuditorTable
                                                , deleteHash
                                                , insertAuditorDeps
                                                , insertHash
                                                , updateAuditorEntryDirect
                                                , updateAuditorVersionChange
                                                )

import           Audit.Queries                  ( getDirAudEntryByDepName
                                                , getInDirAudEntryByDepName
                                                , queryAuditor
                                                , queryHash
                                                )
import           Audit.Tree                     ( buildDepTree
                                                , directDeps
                                                , indirectDeps
                                                )
import           Audit.Types                    ( Package(..)
                                                , QPResult(..)
                                                , ParsedDependency(..)
                                                )
import           Test.Audit.Gen                 ( genDependencyVersion
                                                , genDirectParsedDependency
                                                , genHash
                                                , genIndirectParsedDependency
                                                , genNameVersions
                                                , genPackage
                                                , genRemovedPackage
                                                , genSimpleDepList
                                                , populateAuditorTempDb
                                                )


import           Control.Exception              ( bracket_ )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either     ( runEitherT )
import           Data.Bifunctor                 ( bimap )
import           Data.Either                    ( rights )
import           Data.List                      ( all
                                                , sort
                                                , (\\)
                                                )
import           Data.Text                      ( pack
                                                , unpack
                                                )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                )
import           Database.SQLite.Simple         ( SQLError )
import           Data.Set                       ( difference
                                                , fromList
                                                , toList
                                                )
import           GHC.Stack                      ( HasCallStack
                                                , withFrozenCallStack
                                                )
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import           Hedgehog.Internal.Property     ( Property
                                                , TestLimit
                                                , assert
                                                , evalM
                                                , evalEither
                                                , failWith
                                                , forAll
                                                , forAllT
                                                , property
                                                , withTests
                                                , (===)
                                                )
import qualified Hedgehog.Range                as Range
import           System.Process                 ( callCommand )



-- | Makes sure that no information is lost in `newParsedDeps`
prop_newParsedDeps :: Property
prop_newParsedDeps = withTests 100 . property $ do
        -- Generate packages with versions.
  xs <- forAll genSimpleDepList
  let dDeps  = directDeps $ buildDepTree "MainRepository" xs
  let inDeps = indirectDeps $ buildDepTree "MainRepository" xs
  versions <- forAll $ genNameVersions (dDeps ++ inDeps)

  cTime    <- liftIO getCurrentTime
  -- Build `Package` list
  let pkgs      = newParsedDeps versions dDeps inDeps cTime

  -- Build direct dependencies only
  let dirPkgs   = newParsedDeps versions dDeps [] cTime

  -- Build direct dependencies only
  let indirPkgs = newParsedDeps versions [] inDeps cTime


  -- Deconstruct `Packages`s to strings
  let dDeps' = map (unpack . depName) (filter (\x -> isDirect x == True) pkgs)
  let inDeps' =
        map (unpack . depName) (filter (\x -> isDirect x == False) pkgs)
  let versions' = map (\x -> (unpack $ depName x, unpack $ depVersion x)) pkgs
  -- From direct packages
  let dDeps'' =
        map (unpack . depName) (filter (\x -> isDirect x == True) dirPkgs)
  -- From indirect packages
  let inDeps'' =
        map (unpack . depName) (filter (\x -> isDirect x == False) indirPkgs)


  -- Compare
  dDeps' === dDeps
  inDeps' === inDeps
  versions' === versions

  dDeps'' === dDeps
  inDeps'' === inDeps


prop_clearAuditorTable :: Property
prop_clearAuditorTable = withTests 100 . property $ do
  _ <- forAllT populateAuditorTempDb
  liftIO $ clearAuditorTable "temp.db"
  potentialValues <- liftIO $ queryAuditor "temp.db"
  potentialValues === []

-- TODO: Redo this property
--prop_deleteAuditorEntry :: Property
--prop_deleteAuditorEntry = do
--  withTests 100 . property $ do
--    pkg <- forAll genPackage
--    liftIO . insertAuditorDeps "temp.db" $ map packageToParsedDep [pkg]
--    liftIO $ deleteAuditorEntry "temp.db" (pkgToAuditor pkg)
--    query <- liftIO $ queryAuditor "temp.db"
--    query === []


-- | Insert a hash, query the hash from the db and test for equivalence.
prop_insertHash :: Property
prop_insertHash = withTests 100 . property $ do
  testHash <- forAll genHash
  liftIO $ insertHash "temp.db" testHash
  mHash <- liftIO $ queryHash "temp.db"
  liftIO $ deleteHash "temp.db"
  case mHash of
    Just h  -> hashCurrentHash h === testHash
    Nothing -> testHash === 0

-- | Insert a hash, delete it and make sure the db is empty.
prop_deleteHash :: Property
prop_deleteHash = withTests 100 . property $ do
  testHash <- forAll genHash
  liftIO $ insertHash "temp.db" testHash
  liftIO $ deleteHash "temp.db"
  mHash <- liftIO $ queryHash "temp.db"
  case mHash of
    Just h  -> failWith Nothing $ "Hash still exists in the db: " ++ show h
    Nothing -> assert True

-- | Inserts direct and indirect dependencies into the auditor table.
-- It then compares what was inserted into the
-- database to what was queried from the database.
prop_insertAuditorDeps :: Property
prop_insertAuditorDeps = withTests 100 . property $ do
        -- Uses 'insertAuditorDeps' to populate a the auditor table with
        -- simulated newly parsed dependences.
  parsedDeps <- forAllT populateAuditorTempDb

  -- Query auditor table.
  queried    <- liftIO $ queryAuditor "temp.db"
  liftIO $ clearAuditorTable "temp.db"

  -- Compare generated `Package` with `Package` added to auditor table.
  map isDirect parsedDeps === map auditorDirectDep queried
  map depName parsedDeps === map auditorPackageName queried
  map inUse parsedDeps === map auditorStillUsed queried
  map depVersion parsedDeps === map auditorPackageVersion queried
  map firstSeen parsedDeps === map auditorDateFirstSeen queried
  map (pack . show . aStatus) parsedDeps === map auditorAnalysisStatus queried


-- | Inserts a new direct dependency that already exists
-- as an indirect dependency in the `Auditor` table. This
-- makes sure that the insertion of the new dependency adds
-- a new row and labels it appropriately without modifying
-- the exisisting identical indirect dependency entry.

prop_insertNewDirectDependency :: Property
prop_insertNewDirectDependency = withTests 100 . property $ do
  indirParDep <- forAll genIndirectParsedDependency
  liftIO $ insertAuditorDeps "temp.db" [indirParDep]

  let identicalDirDep = indirParDep { isDirect = True }
  liftIO $ insertAuditorDeps "temp.db" [identicalDirDep]

  deps <- liftIO $ queryAuditor "temp.db"
  liftIO $ clearAuditorTable "temp.db"
  let parsedDepsFromAud = map auditorEntryToParsedDep deps
  mapM evalEither parsedDepsFromAud

  rights parsedDepsFromAud === [indirParDep, identicalDirDep]

-- | Newly parsed dependencies that already exist in the `Auditor` table
-- Check that the relevant enteries are correctly updated in the `Auditor` table.
prop_updateExistingDepStillUsed :: Property
prop_updateExistingDepStillUsed = withTests 100 . property $ do
    -- Populate db with dependency
  parsedDep <- forAll
    $ Gen.choice [genDirectParsedDependency, genIndirectParsedDependency]
  liftIO $ insertAuditorDeps "temp.db" [parsedDep]

  -- Create dependencies that are identical except the still used flag is switched
  deps <- liftIO $ queryAuditor "temp.db"
  let simulatedNewlyParsedUpdatedDep =
        parsedDep { inUse = not $ inUse parsedDep }

  -- Update the db
  let updatedAuditorVals =
        updatedAuditorValues deps [simulatedNewlyParsedUpdatedDep]
  liftIO . runEitherT $ mapM (updateAuditorEntryDirect "temp.db")
                             updatedAuditorVals

  updatedDeps <- liftIO $ queryAuditor "temp.db"
  liftIO $ clearAuditorTable "temp.db"
  updatedAuditorVals === updatedDeps

-- | Direct dependencies have been removed and detected by the parsers.
-- Update the Auditor table to reflect this.
prop_updateRemovedDirectDependency :: Property
prop_updateRemovedDirectDependency = withTests 100 . property $ do
        -- Populate db with dependency
  parsedDep <- forAll
    $ Gen.choice [genDirectParsedDependency, genIndirectParsedDependency]
  liftIO $ insertAuditorDeps "temp.db" [parsedDep]
  initialDeps <- liftIO $ queryAuditor "temp.db"

  -- Create removed dependency names
  -- NB: When a dependency is removed
  -- only the db has its relevant information
  -- and therefore you must query the db
  -- to rebuild the dependency correctly.
  let remDep = unpack $ depName parsedDep

  -- Retrieve removed dependency information from the Auditor table
  eitherDirDepsToBeRemovedAuditor <- liftIO
    $ mapM (runEitherT . getDirAudEntryByDepName "temp.db") [remDep]

  -- Update the retrieved values to reflect the removal of the given dependencies.
  let updatedAuditorValues =
        map auditorEntryToNotUsed $ rights eitherDirDepsToBeRemovedAuditor

  -- Update the db
  liftIO $ mapM (runEitherT . updateAuditorEntryDirect "temp.db")
                updatedAuditorValues
  updatedDeps <- liftIO $ queryAuditor "temp.db"
  case updatedAuditorValues of
    [] -> do
      liftIO $ clearAuditorTable "temp.db"
      updatedDeps === initialDeps
    _ -> do
      liftIO $ clearAuditorTable "temp.db"
      updatedAuditorValues === updatedDeps

-- | Indirect dependencies have been removed and detected by the parsers.
-- Update the Auditor table to reflect this.
prop_updateRemovedIndirectDependency :: Property
prop_updateRemovedIndirectDependency = withTests 100 . property $ do
        -- Populate db with dependency
  parsedDep <- forAll genIndirectParsedDependency
  liftIO $ insertAuditorDeps "temp.db" [parsedDep]
  initialDeps <- liftIO $ queryAuditor "temp.db"

  -- Create removed dependency name
  -- NB: When a dependency is removed
  -- only the db has its relevant information
  -- and therefore you must query the db
  -- to rebuild the dependency correctly.
  let remDep = unpack $ depName parsedDep

  -- Retrieve removed dependency information from the Auditor table
  eitherIndirDepsToBeRemovedAuditor <- liftIO
    $ mapM (runEitherT . getInDirAudEntryByDepName "temp.db") [remDep]

  -- Update the retrieved values to reflect the removal of the given dependencies.
  let updatedAuditorValues =
        map auditorEntryToNotUsed $ rights eitherIndirDepsToBeRemovedAuditor

  -- Update the db
  liftIO $ mapM (runEitherT . updateAuditorEntryDirect "temp.db")
                updatedAuditorValues
  updatedDeps <- liftIO $ queryAuditor "temp.db"
  case updatedAuditorValues of
    [] -> do
      liftIO $ clearAuditorTable "temp.db"
      updatedDeps === initialDeps
    _ -> do
      liftIO $ clearAuditorTable "temp.db"
      updatedAuditorValues === updatedDeps



-- | Check if the version already exists
-- Yes -> Return all packages with that name
--        change stillUsed to True for packages with that version
--        and stillUsed to false for the rest
-- False -> Return all packages with that name, change stillUsed
--          to false and readd all the returned packages with a
--          dependency verison update and a currentTime update.
prop_updateChangedVersions :: Property
prop_updateChangedVersions = withTests 100 . property $ do
  -- Generate a db with dir and indir
  inDirParsedDep <- forAll genIndirectParsedDependency
  let dirParsedDep = inDirParsedDep {isDirect = True}
  liftIO $ insertAuditorDeps "temp.db" [inDirParsedDep, dirParsedDep]
  initialDeps <- liftIO $ queryAuditor "temp.db"

  -- Generate a version change
  newVer <- forAll genDependencyVersion

  -- update db with the changes
  liftIO . runEitherT $ updateAuditorVersionChange "temp.db" (unpack $ depName dirParsedDep,newVer)

  -- Query the auditor for all instances of the change
  -- and compare that with the updated changes generated.
  updatedDeps <- liftIO $ queryAuditor "temp.db"
  liftIO $ clearAuditorTable "temp.db"

  let dbUpatedDeps = filter (\audDep -> auditorPackageVersion audDep == pack newVer) updatedDeps
  let dbInitialDeps = updatedDeps \\ dbUpatedDeps

  -- Check the updated deps have indeed been updated
  mapM_ (\dep -> auditorPackageVersion dep === pack newVer) dbUpatedDeps

  -- There should be two updated deps
  2 === length dbUpatedDeps

  -- The initial deps should only have changed stillUsed flags
  map (\dep -> dep {auditorStillUsed = False}) initialDeps === dbInitialDeps


tests :: IO Bool
tests = and <$> sequence [checkSequential $$discover]

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

withTempDB :: IO a -> IO a
withTempDB = bracket_ tempDb remTempDb
 where
  tempDb :: IO ()
  tempDb = initializeDB "temp.db"
  remTempDb :: IO ()
  remTempDb = callCommand "rm temp.db"
