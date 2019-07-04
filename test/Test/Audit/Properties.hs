{-# LANGUAGE TemplateHaskell #-}


module Test.Audit.Properties where

import Control.Exception (bracket_)
import Control.Monad.IO.Class
import Control.Monad.Trans.Either (runEitherT)
import Data.Bifunctor (bimap)
import Data.Either (rights)
import Data.List (all, sort)
import Data.Text (pack, unpack)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Database.SQLite.Simple (SQLError)
import Data.Set (difference, fromList, toList)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Property
  ( Property
  , TestLimit
  , assert
  , evalEither
  , failWith
  , forAll
  , forAllT
  , property
  , withTests
  , (===)
  )
import qualified Hedgehog.Range as Range
import System.Process (callCommand)

import Audit.Database (Auditor, AuditorT(..), Diff, DiffT(..), HashT(..))
import Audit.Operations
  ( buildPackageList
  , audDepToPackage
  , clearAuditorTable
  , deleteAuditorEntry
  , deleteHash
  , updateAuditorEntry
  , diffDepToPackage
  , loadDiffIntoAuditorUpdate
  , insertAuditorDeps
  , insertHash
  , loadDiffIntoAuditorNew
  , pkgToAuditor
  , updateAuditorEntryWithDiff
  )
import Audit.DiffOperations
  ( insertRemovedDependenciesDiff
  , clearDiffTable
  , insertPackageDiff
  , loadDiffTable
  , loadNewDirDepsDiff
  , loadNewIndirectDepsDiff
  , parseQueryDifference
  , pkgToDiff
  , queryDiff
  , queryDiff'
  )
import Audit.Queries
  ( queryAuditor
  , queryAuditorDepVersions
  , queryAuditorRemovedDeps
  , queryDiffRemovedDeps
  , queryHash
  )
import Audit.Tree (buildDepTree, directDeps, indirectDeps)
import Audit.Types (Package(..), QPResult(..))
import Test.Audit.Gen
  ( genDirectPackage
  , genHash
  , genIndirectPackage
  , genNameVersions
  , genPackage
  , genRemovedPackage
  , genSimpleDepList
  , populateAuditorTempDb
  , populateDiffTempDb
  )


prop_parseQueryDifference_emptyParse :: Property
prop_parseQueryDifference_emptyParse = withTests 100 . property $ do
  queried <- forAll $ Gen.list (Range.linear 0 10) genPackage
  parseQueryDifference [] queried === QPParseIsEmpty

prop_parseQueryDifference_identical :: Property
prop_parseQueryDifference_identical = withTests 100 . property $ do
  parsed  <- forAll $ Gen.list (Range.linear 1 10) genPackage
  parseQueryDifference parsed parsed === QueryAndParseIdentical

prop_parseQueryDifference_different :: Property
prop_parseQueryDifference_different = withTests 100 . property $ do
  parsed <- forAll $ Gen.list (Range.linear 1 10) genPackage
  queried <- forAll $ Gen.list (Range.linear 0 10) genPackage
  let diff = sort . toList $ difference (fromList parsed) (fromList queried)
  case parsed == queried of
    False  -> parseQueryDifference parsed queried === QPDifference diff
    True -> parseQueryDifference parsed queried === QueryAndParseIdentical

-- | Makes sure that no information is lost in `buildPackageList`
prop_buildPackageList :: Property
prop_buildPackageList =
  withTests 100
    . property
    $ do
        -- Generate packages with versions.
        xs <- forAll genSimpleDepList
        let dDeps  = directDeps $ buildDepTree "MainRepository" xs
        let inDeps = indirectDeps $ buildDepTree "MainRepository" xs
        versions <- forAll $ genNameVersions (dDeps ++ inDeps)

        cTime    <- liftIO $ getCurrentTime
        -- Build `Package` list
        let pkgs      = buildPackageList versions dDeps inDeps cTime

        -- Build direct dependencies only
        let dirPkgs   = buildPackageList versions dDeps [] cTime

        -- Build direct dependencies only
        let indirPkgs = buildPackageList versions [] inDeps cTime


        -- Deconstruct `Packages`s to strings
        let
          dDeps' =
            map (unpack . packageName) (filter (\x -> directDep x == True) pkgs)
        let
          inDeps' = map
            (unpack . packageName)
            (filter (\x -> directDep x == False) pkgs)
        let
          versions' =
            map (\x -> (unpack $ packageName x, unpack $ packageVersion x)) pkgs
        -- From direct packages
        let
          dDeps'' = map
            (unpack . packageName)
            (filter (\x -> directDep x == True) dirPkgs)
        -- From indirect packages
        let
          inDeps'' = map
            (unpack . packageName)
            (filter (\x -> directDep x == False) indirPkgs)


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

prop_clearDiffTable :: Property
prop_clearDiffTable = withTests 100 . property $ do
  _ <- forAllT populateDiffTempDb
  liftIO $ clearDiffTable "temp.db"
  potentialValues <- liftIO $ queryDiff "temp.db"
  potentialValues === []

prop_deleteAuditorEntry :: Property
prop_deleteAuditorEntry = do
  withTests 100 . property $ do
    pkg <- forAll genPackage
    liftIO $ insertAuditorDeps "temp.db" [pkg]
    liftIO $ deleteAuditorEntry "temp.db" (pkgToAuditor pkg)
    query <- liftIO $ queryAuditor "temp.db"
    query === []

prop_diffDepToPackage :: Property
prop_diffDepToPackage = withTests 100 . property $ do
  pkg <- forAll genPackage
  let genDiff = pkgToDiff pkg
  case diffDepToPackage genDiff of
    Right pkg' -> pkg' === pkg
    Left  err  -> failWith Nothing $ show err

-- | Makes sure that no information is lost in `pkgToAuditor`
prop_pkgToAuditor :: Property
prop_pkgToAuditor =
  withTests 100
    . property
    $ do
        pkg <- forAll genPackage
        let auditor = pkgToAuditor pkg

        -- Compare values of the 'Package' with the values
        -- in 'Auditor'
        packageName pkg === auditorPackageName auditor
        packageVersion pkg === auditorPackageVersion auditor
        (formatTime defaultTimeLocale "%F %X%Q" $ dateFirstSeen pkg)
          === (unpack $ auditorDateFirstSeen auditor)
        directDep pkg === (read . unpack $ auditorDirectDep auditor)
        stillUsed pkg === (read . unpack $ auditorStillUsed auditor)
        analysisStatus pkg === (read . unpack $ auditorAnalysisStatus auditor)

prop_pkgToDiff :: Property
prop_pkgToDiff =
  withTests 100
    . property
    $ do
        pkg <- forAll genPackage
        let diff = pkgToDiff pkg

        -- Compare values of the 'Package' with the values
        -- in 'Auditor'
        packageName pkg === diffPackageName diff
        packageVersion pkg === diffPackageVersion diff
        (formatTime defaultTimeLocale "%F %X%Q" $ dateFirstSeen pkg)
          === (unpack $ diffDateFirstSeen diff)
        directDep pkg === (read . unpack $ diffDirectDep diff)
        stillUsed pkg === (read . unpack $ diffStillUsed diff)
        analysisStatus pkg === (read . unpack $ diffAnalysisStatus diff)

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

{-
-- | Tests if a version change update is handled correctly.
prop_version_change :: Property
prop_version_change =
    withTests 100 . property $ do
        -- Populate auditor table with initial deps.
        initial <- forAll genSimpleDepList
        let dDeps = directDeps $ buildDepTree "MainRepository" initial
        let inDeps = indirectDeps $ buildDepTree "MainRepository" initial
        versions <- forAll $ genNameVersions (dDeps ++ inDeps)
        packages = buildPackageList versions dDeps inDeps
        liftIO $ insertAuditorDeps "temp.db" packages

        -- Put new deps into diff table.
        new <- forAll genSimpleDepList
        let newDirDeps = directDeps $ buildDepTree "MainRepository" new
        let newInDirDeps = indirectDeps $ buildDepTree "MainRepository" new
        newVersions <- forAll $ genNameVersions (newDirDeps ++ newInDirDeps)
        newPkgs = buildPackageList newVersions newDirDeps newInDirDeps
        liftIO $ loadDiffTable "temp.db" newPkgs
        --liftIO $ loadDiffTable "temp.db" <$> buildPackageList versions [] newInDirDeps

        -- Update auditor table with the new direct dependencies.
        liftIO $ loadDiffIntoAuditor "temp.db"

        -- Query auditor table and compare the result to the generated dependencies.
        queriedDeps <- liftIO $ queryAuditorDepNames "temp.db"
        queriedVers <- liftIO $ queryAuditorDepVersions "temp.db"
        --let tests = [ (==) (sort $ map unpack queriedDeps) (sort $ newDirDeps ++ newInDirDeps ++ dDeps ++ inDeps)
        --            ]
        liftIO $ clearDiffTable "temp.db"
        liftIO $ clearAuditorTable "temp.db"
        (sort $ map unpack queriedDeps) === (sort $ newDirDeps ++ newInDirDeps ++ dDeps ++ inDeps)
        --all (== True) tests === True
-}

-- | Inserts direct and indirect dependencies into the auditor table.
-- It then compares what was inserted into the
-- database to what was queried from the database.
prop_insertAuditorDeps :: Property
prop_insertAuditorDeps =
  withTests 100
    . property
    $ do
        -- Uses 'insertAuditorDeps' to populate a the auditor table with 'Packages'
        packages <- forAllT populateAuditorTempDb

        -- Query auditor table.
        queried  <- liftIO $ queryAuditor "temp.db"
        liftIO $ clearAuditorTable "temp.db"

        -- Compare generated `Package` with `Package` added to auditor table.
        comparePackageWithAuditorEntry queried packages

prop_insertDiffDeps :: Property
prop_insertDiffDeps =
  withTests 100
    . property
    $ do
        -- Uses 'loadDiffTable' to populate a the diff table with 'Packages'
        packages <- forAllT populateDiffTempDb

        -- Query diff table.
        queried  <- liftIO $ queryDiff' "temp.db"
        liftIO $ clearDiffTable "temp.db"

        -- Compare generated `Package` with `Package` added to diff table.
        comparePackageWithDiffEntry queried packages

-- | Inserts direct and indirect dependencies into the diff table
-- via a `Tree`. The property compares what was inserted into the
-- diff table to what was queried from the database.
prop_loadDiffTable :: Property
prop_loadDiffTable =
  withTests 100
    . property
    $ do
        -- Populate auditor table with initial deps.
        packages <- forAll $ Gen.list (Range.linear 0 50) genPackage
        liftIO $ insertAuditorDeps "temp.db" packages

        -- Put new deps into diff table.
        newPkgs <- forAll $ Gen.list (Range.linear 0 50) genPackage
        result <- liftIO . runEitherT $ loadDiffTable "temp.db" newPkgs
        evalEither result

        -- Update auditor table with the new direct dependencies.
        result <- liftIO . runEitherT $ loadDiffIntoAuditorNew "temp.db"
        evalEither result
        -- Query auditor table
        queried <- liftIO $ queryAuditor "temp.db"
        liftIO $ clearDiffTable "temp.db"
        liftIO $ clearAuditorTable "temp.db"

        -- Compare generated `Package` with `Package` added to auditor table.
        comparePackageWithAuditorEntry queried (packages ++ newPkgs)

-- | Insert `Package`s into the Diff table.
-- Tests if the `Package`s inserted and queried are equivalent.
prop_insertPackageDiff :: Property
prop_insertPackageDiff =
  withTests 100
    . property
    $ do

        pkg <- forAll genPackage

        -- Populate diff with test `Package`s
        liftIO $ insertPackageDiff "temp.db" pkg

        -- Query diff table
        queried <- liftIO $ queryDiff' "temp.db"
        liftIO $ clearDiffTable "temp.db"

        -- Compare generated `Package` with `Package` added to diff table.
        comparePackageWithDiffEntry queried [pkg]

prop_insertRemovedDependenciesDiff :: Property
prop_insertRemovedDependenciesDiff =
  withTests 100
    . property
    $ do

        initPkg <- forAll genPackage

        -- Ensure that the Auditor entry has True for stillUsed
        liftIO $ insertAuditorDeps "temp.db" [(initPkg { stillUsed = True })]

        liftIO $ insertRemovedDependenciesDiff "temp.db" [packageName initPkg]

        diffDepRemoved <- liftIO $ queryDiff' "temp.db"
        let diffPackage = rights $ diffDepToPackage <$> diffDepRemoved

        audDep <- liftIO $ queryAuditor "temp.db"
        let audPackage = rights $ audDepToPackage <$> audDep

        liftIO $ clearDiffTable "temp.db"
        liftIO $ clearAuditorTable "temp.db"
        -- The diffPackage should be identical to the entry in the Auditor
        -- table with the exception of the stillUsed flag
        diffPackage === (map (\x -> x { stillUsed = False }) audPackage)


-- | When there is a detected change in the dependencies,
-- this change is uploaded to the diff table as a Diff. We
-- then update the Auditor table via 'updateAuditorEntryWithDiff'.
prop_updateAuditorEntryWithDiff :: Property
prop_updateAuditorEntryWithDiff =
  withTests 100
    . property
    $ do
        -- Original Package
        pkg@(Package packageName packageVersion dateFirstSeen directDep stillUsed analysisStatus) <-
          forAll genPackage
        -- Updated Package
        pkg'@(Package packageName' packageVersion' dateFirstSeen' directDep' stillUsed' analysisStatus') <-
          forAll genPackage

        -- The case where they are the same. This shouldn't happen (as only
        -- dependency differences end up in the diff table) but
        -- we test to be sure that the inputs are equivalent.
        case updateAuditorEntryWithDiff (pkgToDiff pkg) (pkgToAuditor pkg) of
          Left  err -> failWith Nothing $ show err
          Right aud -> aud === pkgToAuditor pkg

        -- The case where they are different. Ensure the generated Auditor
        -- matches the input Diff.
        case updateAuditorEntryWithDiff (pkgToDiff pkg') (pkgToAuditor pkg) of
          Left  err  -> failWith Nothing $ show err
          Right aud' -> do
            let original = pkgToAuditor pkg
            aud' === (pkgToAuditor pkg')
              { auditorDateFirstSeen = (auditorDateFirstSeen original)
              }


prop_loadDiffIntoAuditorNew_newDirDependencies_NEWVERSION :: Property
prop_loadDiffIntoAuditorNew_newDirDependencies_NEWVERSION =
  withTests 100
    . property
    $ do
        -------------------------------------------------------------
        -- Insert New Direct Dependencies --
        -------------------------------------------------------------

        pkg <- forAll genPackage

        -- Populate diff table with test `Package`s
        liftIO $ insertPackageDiff "temp.db" pkg


        -- Update auditor table with the new direct dependencies.
        liftIO . runEitherT $ loadDiffIntoAuditorNew "temp.db"

        -- Query auditor table
        queried <- liftIO $ queryAuditor "temp.db"
        liftIO $ clearDiffTable "temp.db"
        liftIO $ clearAuditorTable "temp.db"

        -- Compare generated `Package` with `Package` added to auditor table.
        comparePackageWithAuditorEntry queried [pkg]


prop_loadDiffIntoAuditorUpdate_removedDependencies_NEWVERSION :: Property
prop_loadDiffIntoAuditorUpdate_removedDependencies_NEWVERSION =
  withTests 100
    . property
    $ do
        -------------------------------------------------------------
        -- Update Dependencies to reflect removed dependencies
        -------------------------------------------------------------

        currentPackage <- forAll genPackage

        -- Populate auditor table with a single initial dependency.
        liftIO $ insertAuditorDeps "temp.db" [currentPackage]


        -- Populate diff with removed dependencies
        let updatedPkg = currentPackage { stillUsed = False }
        liftIO $ insertPackageDiff "temp.db" updatedPkg
        liftIO . runEitherT $ loadDiffIntoAuditorUpdate "temp.db"

        queried <- liftIO $ queryAuditor "temp.db"
        liftIO $ clearDiffTable "temp.db"
        liftIO $ clearAuditorTable "temp.db"

        -- Compare generated `Package` with `Package` added to auditor table.
        comparePackageWithAuditorEntry queried [updatedPkg]

prop_updateAuditorEntry :: Property
prop_updateAuditorEntry =
  withTests 100
    . property
    $ do
        pkg <- forAll genPackage

        -- Populate auditor table with a single initial dependency.
        liftIO $ insertAuditorDeps "temp.db" [pkg]
        -- second package not added here


        -- Populate diff with removed dependencies
        let removedPkg = pkg { stillUsed = False }
        liftIO $ insertPackageDiff "temp.db" removedPkg
        -- second package note added here
        liftIO . runEitherT $ updateAuditorEntry
          "temp.db"
          (pkgToDiff removedPkg)
          (pkgToAuditor pkg)
        --TODO: I wonder if this has to do with having multiple connections open. Check the function
        -- Query auditor table
        -- Test out the runUpdate save command by itself. That may be the issue.
        queried <- liftIO $ queryAuditor "temp.db"
        liftIO $ clearDiffTable "temp.db"
        liftIO $ clearAuditorTable "temp.db"

        queried === [pkgToAuditor removedPkg]

prop_loadNewDirDepsDiff :: Property
prop_loadNewDirDepsDiff =
  withTests 100
    . property
    $ do

      -- Generate direct deps
        packages     <- forAll $ Gen.list (Range.constant 0 10) genDirectPackage

        -- Insert direct deps into diff table
        checkDirDeps <- liftIO . runEitherT $ loadNewDirDepsDiff
          "temp.db"
          packages
        case checkDirDeps of
          Left  err -> failWith Nothing $ show err
          Right _   -> pure ()

        -- Query diff table
        queried <- liftIO $ queryDiff' "temp.db"
        liftIO $ clearDiffTable "temp.db"

        -- Compare generated `Package` with `Package` added to diff table.
        comparePackageWithDiffEntry (sort queried) (sort packages)

prop_loadNewIndirectDepsDiff :: Property
prop_loadNewIndirectDepsDiff =
  withTests 100
    . property
    $ do

      -- Generate indirect deps
        packages <- forAll $ Gen.list (Range.constant 0 10) genIndirectPackage
        -- Insert indirect deps into diff table
        checkDirDeps <- liftIO . runEitherT $ loadNewIndirectDepsDiff
          "temp.db"
          packages
        case checkDirDeps of
          Left  err -> failWith Nothing $ show err
          Right _   -> pure ()

        -- Query diff table
        queried <- liftIO $ queryDiff' "temp.db"
        liftIO $ clearDiffTable "temp.db"

        -- Compare generated `Package` with `Package` added to diff table.
        comparePackageWithDiffEntry (sort queried) (sort packages)



tests :: IO Bool
tests = and <$> sequence [checkSequential $$discover]

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

withTempDB :: IO a -> IO a
withTempDB = bracket_ tempDb remTempDb
 where
  tempDb :: IO ()
  tempDb =
    callCommand
      "sqlite3 temp.db \
        \\"CREATE TABLE auditor ( package_name VARCHAR NOT NULL\
                               \, package_version VARCHAR NOT NULL\
                               \, date_first_seen VARCHAR NOT NULL\
                               \, direct_dep VARCHAR NOT NULL\
                               \, still_used VARCHAR NOT NULL\
                               \, analysis_status VARCHAR NOT NULL\
                               \, PRIMARY KEY( package_name )); \
         \CREATE TABLE hash ( current_hash INT NOT NULL\
                               \, PRIMARY KEY ( current_hash )); \
         \CREATE TABLE diff ( package_name VARCHAR NOT NULL\
                               \, package_version VARCHAR NOT NULL\
                               \, date_first_seen VARCHAR NOT NULL\
                               \, direct_dep VARCHAR NOT NULL\
                               \, still_used VARCHAR NOT NULL\
                               \, analysis_status VARCHAR NOT NULL\
                               \, PRIMARY KEY( package_name )); \""

  remTempDb :: IO ()
  remTempDb = callCommand "rm temp.db"

-- Compare generated `Package` with an entry in the Auditor table

comparePackageWithAuditorEntry :: MonadTest m => [Auditor] -> [Package] -> m ()
comparePackageWithAuditorEntry queried pkgs = do
  map auditorPackageName queried === map packageName pkgs
  map auditorPackageVersion queried === map packageVersion pkgs
  let
    time =
      map (pack . formatTime defaultTimeLocale "%F %X%Q" . dateFirstSeen) pkgs
  map auditorDateFirstSeen queried === time
  map auditorDirectDep queried === map (pack . show . directDep) pkgs
  map auditorStillUsed queried === map (pack . show . stillUsed) pkgs
  map auditorAnalysisStatus queried === map (pack . show . analysisStatus) pkgs

-- Compare generated `Package` with an entry in the  Diff table

comparePackageWithDiffEntry :: MonadTest m => [Diff] -> [Package] -> m ()
comparePackageWithDiffEntry queried pkgs = do
  map diffPackageName queried === map packageName pkgs
  map diffPackageVersion queried === map packageVersion pkgs
  let
    times =
      map (pack . formatTime defaultTimeLocale "%F %X%Q" . dateFirstSeen) pkgs
  map diffDateFirstSeen queried === times
  map diffDirectDep queried === map (pack . show . directDep) pkgs
  map diffStillUsed queried === map (pack . show . stillUsed) pkgs
  map diffAnalysisStatus queried === map (pack . show . analysisStatus) pkgs
