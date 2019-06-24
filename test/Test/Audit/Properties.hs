{-# LANGUAGE TemplateHaskell #-}


module Test.Audit.Properties where

import           Control.Exception          (bracket_)
import           Control.Monad.IO.Class
import           Data.Bifunctor             (bimap)
import           Data.List                  (all, sort)
import           Data.Text                  (pack, unpack)
import           Data.Time.Clock            (getCurrentTime)
import           Control.Monad.Trans.Either  (runEitherT)
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import           Database.SQLite.Simple     (SQLError)
import           GHC.Stack                  (HasCallStack, withFrozenCallStack)
import           Hedgehog
import qualified Hedgehog.Gen               as Gen
import           Hedgehog.Internal.Property (Property, TestLimit, assert,
                                             evalEither, failWith, forAll,
                                             forAllT, property, withTests,
                                             (===))
import qualified Hedgehog.Range             as Range
import           System.Process             (callCommand)

import           Audit.Database             (Auditor, AuditorT (..), Diff,
                                             DiffT (..), HashT (..))
import           Audit.Operations           (buildPackageList,
                                             clearAuditorTable, clearDiffTable,
                                             deleteHash, insertAuditorDeps,
                                             insertHash, insertPackageDiff,
                                             insertRemovedDependencies,
                                             loadDiffIntoAuditor, loadDiffTable,
                                             loadNewDirDepsDiff, pkgToAuditor,
                                             pkgToDiff,
                                             updateAuditorEntryWithDiff)
import           Audit.Queries              (queryAuditor, queryAuditorDepNames,
                                             queryAuditorDepVersions,
                                             queryAuditorRemovedDeps, queryDiff,
                                             queryDiff', queryDiffRemovedDeps,
                                             queryHash)
import           Audit.Tree                 (buildDepTree, directDeps,
                                             indirectDeps)
import           Audit.Types                (Package (..))
import           Test.Audit.Gen             (genHash, genNameVersions,
                                             genPackage, genRemovedPackage,
                                             genSimpleDepList,
                                             populateAuditorTempDb,
                                             populateDiffTempDb)



prop_clearAuditorTable :: Property
prop_clearAuditorTable =
    withTests 100 . property $ do
        _ <- forAllT populateAuditorTempDb
        liftIO $ clearAuditorTable "temp.db"
        potentialValues <- liftIO $ queryAuditor "temp.db"
        potentialValues === []

prop_clearDiffTable :: Property
prop_clearDiffTable =
    withTests 100 . property $ do
        _ <- forAllT populateDiffTempDb
        liftIO $ clearDiffTable "temp.db"
        potentialValues <- liftIO $ queryDiff "temp.db"
        potentialValues === []

-- | Makes sure that no information is lost in `buildPackageList`
prop_buildPackageList :: Property
prop_buildPackageList =
    withTests 100 . property $ do
        -- Generate packages with versions.
        xs <- forAll genSimpleDepList
        let dDeps = directDeps $ buildDepTree "MainRepository" xs
        let inDeps = indirectDeps $ buildDepTree "MainRepository" xs
        versions <- forAll $ genNameVersions (dDeps ++ inDeps)

        cTime <- liftIO $ getCurrentTime
        -- Build `Package` list
        let pkgs = buildPackageList versions dDeps inDeps cTime

        -- Build direct dependencies only
        let dirPkgs = buildPackageList versions dDeps [] cTime

        -- Build direct dependencies only
        let indirPkgs = buildPackageList versions [] inDeps cTime


        -- Deconstruct `Packages`s to strings
        let dDeps' = map (unpack . packageName ) (filter (\x -> directDep x == True) pkgs)
        let inDeps' = map (unpack . packageName ) (filter (\x -> directDep x == False) pkgs)
        let versions' = map (\x -> (unpack $ packageName x, unpack $ packageVersion x)) pkgs
        -- From direct packages
        let dDeps'' = map (unpack . packageName ) (filter (\x -> directDep x == True) dirPkgs)
        -- From indirect packages
        let inDeps'' = map (unpack . packageName ) (filter (\x -> directDep x == False) indirPkgs)


        -- Compare
        dDeps' === dDeps
        inDeps' === inDeps
        versions' === versions

        dDeps'' === dDeps
        inDeps'' === inDeps

-- | Makes sure that no information is lost in `pkgToAuditor`
prop_pkgToAuditor :: Property
prop_pkgToAuditor =
    withTests 100 . property $ do
        pkg <- forAll genPackage
        let auditor = pkgToAuditor pkg

        -- Compare values of the 'Package' with the values
        -- in 'Auditor'
        packageName pkg === auditorPackageName auditor
        packageVersion pkg === auditorPackageVersion auditor
        (formatTime defaultTimeLocale "%F %X%Q" $ dateFirstSeen pkg) === (unpack $ auditorDateFirstSeen auditor)
        directDep pkg === (read . unpack $ auditorDirectDep auditor)
        stillUsed pkg === (read . unpack $ auditorStillUsed auditor)
        analysisStatus pkg === (read . unpack $ auditorAnalysisStatus auditor)

prop_pkgToDiff :: Property
prop_pkgToDiff =
    withTests 100 . property $ do
        pkg <- forAll genPackage
        let diff = pkgToDiff pkg

        -- Compare values of the 'Package' with the values
        -- in 'Auditor'
        packageName pkg === diffPackageName diff
        packageVersion pkg === diffPackageVersion diff
        (formatTime defaultTimeLocale "%F %X%Q" $ dateFirstSeen pkg) === (unpack $ diffDateFirstSeen diff)
        directDep pkg === (read . unpack $ diffDirectDep diff)
        stillUsed pkg === (read . unpack $ diffStillUsed diff)
        analysisStatus pkg === (read . unpack $ diffAnalysisStatus diff)

-- | Insert a hash, query the hash from the db and test for equivalence.
prop_insertHash :: Property
prop_insertHash =
  withTests 100 . property $ do
    testHash <- forAll genHash
    liftIO $ insertHash "temp.db" testHash
    mHash <- liftIO $ queryHash "temp.db"
    liftIO $ deleteHash "temp.db"
    case mHash of
      Just h  -> hashCurrentHash h === testHash
      Nothing -> testHash === 0

-- | Insert a hash, delete it and make sure the db is empty.
prop_deleteHash :: Property
prop_deleteHash =
  withTests 100 . property $ do
    testHash <- forAll genHash
    liftIO $ insertHash "temp.db" testHash
    liftIO $ deleteHash "temp.db"
    mHash <- liftIO $ queryHash "temp.db"
    case mHash of
      Just h  -> failWith Nothing $ "Hash still exists in the db: " ++ show h
      Nothing -> assert True

-- TODO: Need to distill out update or modify and loadDiffIntoAuditor
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
    withTests 100 . property $ do
        -- Uses 'insertAuditorDeps' to populate a the auditor table with 'Packages'
        packages <- forAllT populateAuditorTempDb

        -- Query auditor table.
        queried <- liftIO $ queryAuditor "temp.db"
        liftIO $ clearAuditorTable "temp.db"

        -- Compare generated `Package` with `Package` added to auditor table.
        comparePackageWithAuditorEntry packages queried

prop_insertDiffDeps :: Property
prop_insertDiffDeps =
    withTests 100 . property $ do
        -- Uses 'loadDiffTable' to populate a the diff table with 'Packages'
        packages <- forAllT populateDiffTempDb

        -- Query diff table.
        queried <- liftIO $ queryDiff' "temp.db"
        liftIO $ clearDiffTable "temp.db"

        -- Compare generated `Package` with `Package` added to diff table.
        comparePackageWithDiffEntry packages queried

-- | Inserts direct and indirect dependencies into the diff table
-- via a `Tree`. The property compares what was inserted into the
-- diff table to what was queried from the database.
prop_loadDiffTable :: Property
prop_loadDiffTable =
    withTests 100 . property $ do
        -- Populate auditor table with initial deps.
        packages <- forAll $ Gen.list (Range.linear 0 50) genPackage
        liftIO $ insertAuditorDeps "temp.db" packages

        -- Put new deps into diff table.
        newPkgs <- forAll $ Gen.list (Range.linear 0 50) genPackage
        liftIO $ loadDiffTable "temp.db" newPkgs

        -- Update auditor table with the new direct dependencies.
        liftIO $ loadDiffIntoAuditor "temp.db"

        -- Query auditor table
        queried <- liftIO $ queryAuditor "temp.db"
        liftIO $ clearDiffTable "temp.db"
        liftIO $ clearAuditorTable "temp.db"

        -- Compare generated `Package` with `Package` added to auditor table.
        comparePackageWithAuditorEntry (packages ++ newPkgs) queried

-- | Insert `Package`s into the Diff table.
-- Tests if the `Package`s inserted and queried are equivalent.
prop_insertPackageDiff :: Property
prop_insertPackageDiff =
    withTests 100 . property $ do
        pkg <- forAll genPackage

        -- Populate diff with test `Package`s
        liftIO $ insertPackageDiff "temp.db" pkg

        -- Query diff table
        queried <- liftIO $ queryDiff' "temp.db"
        liftIO $ clearDiffTable "temp.db"

        -- Compare generated `Package` with `Package` added to diff table.
        comparePackageWithDiffEntry [pkg] queried

-- | When there is a detected change in the dependencies,
-- this change is uploaded to the diff table as a Diff. We
-- then update the Auditor table via 'updateAuditorEntryWithDiff'.
prop_updateAuditorEntryWithDiff :: Property
prop_updateAuditorEntryWithDiff =
    withTests 100 . property $ do
        pkg@( Package
               packageName
               packageVersion
               dateFirstSeen
               directDep
               stillUsed
               analysisStatus
            ) <- forAll genPackage
        pkg'@( Package
                packageName'
                packageVersion'
                dateFirstSeen'
                directDep'
                stillUsed'
                analysisStatus'
             ) <- forAll genPackage

        -- The case where they are the same. This shouldn't happen (as only
        -- dependency differences end up in the diff table) but
        -- we test to be sure that the inputs are equivalent.
        case updateAuditorEntryWithDiff (pkgToDiff pkg) (pkgToAuditor pkg) of
            Left err  -> failWith Nothing $ show err
            Right aud -> aud === pkgToAuditor pkg

        -- The case where they are different. Ensure the generated Auditor
        -- matches the input Diff.
        case updateAuditorEntryWithDiff (pkgToDiff pkg') (pkgToAuditor pkg) of
            Left err   -> failWith Nothing $ show err
            Right aud' -> aud' === pkgToAuditor pkg'

-- | Insert a `Package` from Diff table to Auditor table.
-- Tests if the `Package` generated is the same as
-- the what was stored in the auditor table.
prop_loadDiffIntoAuditor :: Property
prop_loadDiffIntoAuditor =
    withTests 100 . property $ do
        pkg <- forAll genPackage

        -- Populate diff table with test `Package`s
        liftIO $ insertPackageDiff "temp.db" pkg


        -- Update auditor table with the new direct dependencies.
        liftIO $ loadDiffIntoAuditor "temp.db"

        -- Query auditor table
        queried <- liftIO $ queryAuditor "temp.db"
        liftIO $ clearDiffTable "temp.db"
        liftIO $ clearAuditorTable "temp.db"

        -- Compare generated `Package` with `Package` added to auditor table.
        comparePackageWithAuditorEntry [pkg] queried

-- | Remove dependencies from the auditor table.
-- Tests if the `Package` generated is the same as
-- the what was stored in the auditor table.
-- TODO: Need to test updateDiffTable* functions first
{-
prop_db_remove_dependencies :: Property
prop_db_remove_dependencies =
    withTests 100 . property $ do
        -- Populate auditor table with initial deps.
        initial <- forAll genSimpleDepList
        let dDeps = directDeps $ buildDepTree "MainRepository" initial
        let inDeps = indirectDeps $ buildDepTree "MainRepository" initial
        versions <- forAll $ genNameVersions (dDeps ++ inDeps)
        packages = buildPackageList versions dDeps inDeps
        liftIO $ insertAuditorDeps "temp.db" packages

        -- Package to be removed
        pkg <- forAll genPackage
        liftIO $ insertAuditorDeps "temp.db" [pkg]


        -- Populate diff with removed dependencies
        let removedPkg = pkg {stillUsed = False}
        liftIO $ insertPackageDiff "temp.db" removedPkg

        -- Update auditor table with the new direct dependencies.
        liftIO $ loadDiffIntoAuditor "temp.db"

        -- Query auditor table
        queried <- liftIO $ queryAuditor "temp.db"
        liftIO $ clearDiffTable "temp.db"
        liftIO $ clearAuditorTable "temp.db"

        -- Filter removed dependencies
        let remDeps = filter (\x -> auditorStillUsed x == pack "False") queried
        liftIO $ print remDeps
        -- TODO: This fails because buildPackageList already has removed packages included in it
        -- Need to generate a package list with packages only currently in use!
        (map auditorPackageName remDeps) === [packageName removedPkg]
        --map auditorPackageVersion remDeps === [packageVersion removedPkg]
        --let time = pack . formatTime defaultTimeLocale "%F %X%Q" $ dateFirstSeen removedPkg
        --map auditorDateFirstSeen remDeps === [time]
        --map auditorDirectDep remDeps === [pack . show $ directDep removedPkg]
        --map auditorStillUsed remDeps === [pack . show $ directDep removedPkg]
        --map auditorAnalysisStatus remDeps === [pack . show $ analysisStatus removedPkg]
-}

prop_loadNewDirDepsDiff :: Property
prop_loadNewDirDepsDiff =
    withTests 100 . property $ do

    -- Generate direct deps
    xs <- forAll genSimpleDepList
    let dDeps = directDeps $ buildDepTree "MainRepository" xs
    versions <- forAll $ genNameVersions dDeps

    cTime <- liftIO $ getCurrentTime
    let packages = buildPackageList versions dDeps [] cTime

    -- Insert direct deps into diff table
    checkDirDeps <- liftIO . runEitherT $ loadNewDirDepsDiff "temp.db" packages
    case checkDirDeps of
        Left err -> failWith Nothing $ show err
        Right eff-> pure $ eff

    -- Query diff table
    queried <- liftIO $ queryDiff' "temp.db"
    liftIO $ clearDiffTable "temp.db"

    -- Compare generated `Package` with `Package` added to diff table.
    comparePackageWithDiffEntry packages queried

tests :: IO Bool
tests = and <$> sequence
    [ checkSequential $$discover ]

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

withTempDB :: IO a -> IO a
withTempDB =
    bracket_ tempDb remTempDb
  where
     tempDb :: IO ()
     tempDb =
        callCommand "sqlite3 temp.db \
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

comparePackageWithAuditorEntry :: MonadTest m => [Package] -> [Auditor] -> m ()
comparePackageWithAuditorEntry pkgs queried = do
  map auditorPackageName queried === map packageName pkgs
  map auditorPackageVersion queried === map packageVersion pkgs
  let time = map (pack . formatTime defaultTimeLocale "%F %X%Q" . dateFirstSeen) pkgs
  map auditorDateFirstSeen queried === time
  map auditorDirectDep queried === map (pack . show . directDep) pkgs
  map auditorStillUsed queried === map (pack . show . stillUsed) pkgs
  map auditorAnalysisStatus queried === map (pack . show . analysisStatus) pkgs

-- Compare generated `Package` with an entry in the  Diff table

comparePackageWithDiffEntry :: MonadTest m => [Package] -> [Diff] -> m ()
comparePackageWithDiffEntry pkgs queried = do
  map diffPackageName queried === map packageName pkgs
  map diffPackageVersion queried === map packageVersion pkgs
  let times = map (pack . formatTime defaultTimeLocale "%F %X%Q" . dateFirstSeen) pkgs
  map diffDateFirstSeen queried === times
  map diffDirectDep queried === map (pack . show . directDep) pkgs
  map diffStillUsed queried === map (pack . show . stillUsed) pkgs
  map diffAnalysisStatus queried === map (pack . show . analysisStatus) pkgs
