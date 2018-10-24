{-# LANGUAGE TemplateHaskell #-}

module Test.TempDatabase where

import           Control.Monad.IO.Class
import           Data.Bifunctor             (bimap)
import           Data.List                  (all, sort)
import           Data.Text                  (pack, unpack)
import           Database                   (Auditor (..), buildPackageList,
                                             clearAuditorTable, clearDiffTable,
                                             deleteHash, insertDeps,
                                             insertPackageDiff,
                                             insertRemovedDependencies,
                                             insertUpdatedDependencies,
                                             loadDiffIntoAuditor,
                                             queryAuditorDepNames,
                                             queryAuditorDepVersions,
                                             queryAuditorRemovedDeps, queryDiff,
                                             queryDiffRemovedDeps)
import           Hedgehog
import           Hedgehog.Internal.Property (Property, assert, forAll, property,
                                             withTests, (===))
import           System.Process             (callCommand)
import           Test.Gen                   (genNameVersions, genPackage,
                                             genRemovedPackage,
                                             genSimpleDepList)
import           Tree                       (buildDepTree, directDeps,
                                             indirectDeps)
import           Types                      (Package (..))

prop_buildPackageList :: Property
prop_buildPackageList =
    withTests 100 . property $ do
        -- Generate packages with versions.
        xs <- forAll genSimpleDepList
        let dDeps = directDeps $ buildDepTree "MainRepository" xs
        let inDeps = indirectDeps $ buildDepTree "MainRepository" xs
        versions <- forAll $ genNameVersions (dDeps ++ inDeps)

        -- Build `Package` list
        pkgs <- liftIO $ buildPackageList versions dDeps inDeps

        -- Deconstruct `Packages`s to strings
        let dDeps' = map (unpack . packageName ) (filter (\x -> directDep x == True) pkgs)
        let inDeps' = map (unpack . packageName ) (filter (\x -> directDep x == False) pkgs)
        let versions' = map (\x -> (unpack $ packageName x, unpack $ packageVersion x)) pkgs

        -- Compare
        let tests = [ (==) (sort dDeps') (sort dDeps)
                    -- Make sure versions match
                    , (==) (sort inDeps') (sort inDeps)
                    , (==) (sort versions) (sort versions')
                    ]
        if all (== True) tests then assert True else assert False


-- | Inserts direct and indirect dependencies into the database
-- via a `Tree`. It then compares what was inserted into the
-- database to what was queried from the database.
prop_db_insert_initial_dependencies_auditor :: Property
prop_db_insert_initial_dependencies_auditor =
    withTests 100 . property $ do
        -- Generate packages with versions.
        xs <- forAll genSimpleDepList
        let dDeps = directDeps $ buildDepTree "MainRepository" xs
        let inDeps = indirectDeps $ buildDepTree "MainRepository" xs
        versions <- forAll $ genNameVersions (dDeps ++ inDeps)

        -- Populate auditor table with initial deps.
        packages <- liftIO $ buildPackageList versions dDeps inDeps
        liftIO $ insertDeps "temp.db" packages
        queriedDeps <- liftIO $ queryAuditorDepNames "temp.db"
        queriedVers <- liftIO $ queryAuditorDepVersions "temp.db"
        liftIO $ clearAuditorTable "temp.db"
        let tests = [ (==) (sort queriedDeps) (sort (map pack dDeps ++ map pack inDeps))
                    -- Make sure versions match
                    , (==) (zip (map unpack queriedDeps) (map unpack queriedVers)) versions
                    ]
        if all (== True) tests then assert True else assert False

-- | Inserts direct and indirect dependencies into the diff table
-- via a `Tree`. It then compares what was inserted into the
-- diff table to what was queried from the database.

prop_db_insert_new_dependencies_auditor :: Property
prop_db_insert_new_dependencies_auditor =
    withTests 100 . property $ do
        -- Populate auditor table with initial deps.
        initial <- forAll genSimpleDepList
        let dDeps = directDeps $ buildDepTree "MainRepository" initial
        let inDeps = indirectDeps $ buildDepTree "MainRepository" initial
        versions <- forAll $ genNameVersions (dDeps ++ inDeps)
        packages <- liftIO $ buildPackageList versions dDeps inDeps
        liftIO $ insertDeps "temp.db" packages

        -- Put new deps into diff table.
        new <- forAll genSimpleDepList
        let newDirDeps = directDeps $ buildDepTree "MainRepository" new
        let newInDirDeps = indirectDeps $ buildDepTree "MainRepository" new
        newVersions <- forAll $ genNameVersions (newDirDeps ++ newInDirDeps)
        newPkgs <- liftIO $ buildPackageList newVersions newDirDeps newInDirDeps
        liftIO $ insertUpdatedDependencies "temp.db" newPkgs
        --liftIO $ insertUpdatedDependencies "temp.db" <$> buildPackageList versions [] newInDirDeps

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

-- | Insert `Package`s into the Diff table.
prop_insertPackageDiff :: Property
prop_insertPackageDiff =
    withTests 100 . property $ do
        pkg <- forAll genPackage

        -- Populate diff with test `Package`s
        liftIO $ insertPackageDiff "temp.db" pkg

        -- Query diff table
        queryDeps <- liftIO $ queryDiff "temp.db"
        let tests = [(==) queryDeps [packageName pkg]]

        liftIO $ clearDiffTable "temp.db"
        liftIO $ clearAuditorTable "temp.db"
        if all (== True) tests then assert True else assert False


-- | Insert `Package`s from Diff table to Auditor table.
prop_loadDiffIntoAuditor :: Property
prop_loadDiffIntoAuditor =
    withTests 100 . property $ do
        pkg <- forAll genPackage

        -- Populate diff with test `Package`s
        liftIO $ insertPackageDiff "temp.db" pkg


        -- Update auditor table with the new direct dependencies.
        liftIO $ loadDiffIntoAuditor "temp.db"

        -- Query diff table
        queryDeps <- liftIO $ queryAuditorDepNames "temp.db"
        let tests = [(==) queryDeps [packageName pkg]]

        liftIO $ clearDiffTable "temp.db"
        liftIO $ clearAuditorTable "temp.db"
        if all (== True) tests then assert True else assert False

-- | Remove dependencies from the auditor table.
prop_db_remove_dependencies :: Property
prop_db_remove_dependencies =
    withTests 100 . property $ do
        -- Populate auditor table with initial deps.
        initial <- forAll genSimpleDepList
        let dDeps = directDeps $ buildDepTree "MainRepository" initial
        let inDeps = indirectDeps $ buildDepTree "MainRepository" initial
        versions <- forAll $ genNameVersions (dDeps ++ inDeps)
        packages <- liftIO $ buildPackageList versions dDeps inDeps
        liftIO $ insertDeps "temp.db" packages

        -- Populate diff with removed dependencies
        pkg <- forAll genRemovedPackage
        liftIO $ insertPackageDiff "temp.db" pkg

        -- Update auditor table with the new direct dependencies.
        liftIO $ loadDiffIntoAuditor "temp.db"

        queriedRemovedDeps <- liftIO $ queryAuditorRemovedDeps "temp.db"
        let tests = [(==) (sort queriedRemovedDeps) [packageName pkg] ]

        liftIO $ clearDiffTable "temp.db"
        liftIO $ clearAuditorTable "temp.db"
        if all (== True) tests then assert True else assert False

prop_insertDeps :: Property
prop_insertDeps =
    withTests 100 . property $ do
        -- Generate packages with versions.
        xs <- forAll genSimpleDepList
        let dDeps = directDeps $ buildDepTree "MainRepository" xs
        let inDeps = indirectDeps $ buildDepTree "MainRepository" xs
        versions <- forAll $ genNameVersions (dDeps ++ inDeps)

        -- Populate auditor 'table with test deps.
        packages <- liftIO $ buildPackageList versions dDeps inDeps
        liftIO $ insertDeps "temp.db" packages

        -- Query auditor table and compare the result to the generated dependencies.
        queriedDeps <- liftIO $ queryAuditorDepNames "temp.db"
        queriedVers <- liftIO $ queryAuditorDepVersions "temp.db"
        liftIO $ clearAuditorTable "temp.db"
        let tests = [ (==) (sort queriedDeps) (sort (map pack dDeps ++ map pack inDeps))
                       -- Make sure versions match
                    , (==) (sort $ zip (map unpack queriedDeps) (map unpack queriedVers)) (sort versions)
                    ]
        if all (== True) tests then assert True else assert False


-- TODO: Write test for `initialAuditorTable` then refactor using `buildInitialPackageList`
-- be sure to use the dependency tree!

tests :: IO Bool
tests = and <$> sequence
    [ checkSequential $$discover ]

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

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
     \CREATE TABLE hash ( dot_hash INT NOT NULL\
                           \, PRIMARY KEY ( dot_hash )); \
     \CREATE TABLE diff ( package_name VARCHAR NOT NULL\
                           \, package_version VARCHAR NOT NULL\
                           \, date_first_seen VARCHAR NOT NULL\
                           \, direct_dep VARCHAR NOT NULL\
                           \, still_used VARCHAR NOT NULL\
                           \, analysis_status VARCHAR NOT NULL\
                           \, PRIMARY KEY( package_name )); \""

remTempDb :: IO ()
remTempDb = callCommand "rm temp.db"
