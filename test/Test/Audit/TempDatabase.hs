{-# LANGUAGE TemplateHaskell #-}


module Test.Audit.TempDatabase where

import           Control.Exception          (bracket_)
import           Control.Monad.IO.Class
import           Data.Bifunctor             (bimap)
import           Data.List                  (all, sort)
import           Data.Text                  (pack, unpack)
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import           Hedgehog
import           Hedgehog.Internal.Property (Property, assert, failWith, forAll,
                                             forAllT, property, withTests,
                                             (===))
import           System.Process             (callCommand)

import           Audit.Database             (Auditor, AuditorT (..), Diff,
                                             DiffT (..), HashT (..))
import           Audit.Operations           (buildPackageList,
                                             clearAuditorTable, clearDiffTable,
                                             deleteHash, insertDeps,
                                             insertDiffDependencies, insertHash,
                                             insertPackageDiff,
                                             insertRemovedDependencies,
                                             loadDiffIntoAuditor,
                                             updateDiffTableDirectDeps)
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
                                             genSimpleDepList, populateTempDb)



prop_clearAuditorTable :: Property
prop_clearAuditorTable =
    withTests 100 . property $ do
        _ <- forAllT populateTempDb
        liftIO $ clearAuditorTable "temp.db"
        potentialValues <- liftIO $ queryAuditor "temp.db"
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

        -- Build `Package` list
        pkgs <- liftIO $ buildPackageList versions dDeps inDeps

        -- Build direct dependencies only
        dirPkgs <- liftIO $ buildPackageList versions dDeps []

        -- Build direct dependencies only
        indirPkgs <- liftIO $ buildPackageList versions [] inDeps


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
        packages <- liftIO $ buildPackageList versions dDeps inDeps
        liftIO $ insertDeps "temp.db" packages

        -- Put new deps into diff table.
        new <- forAll genSimpleDepList
        let newDirDeps = directDeps $ buildDepTree "MainRepository" new
        let newInDirDeps = indirectDeps $ buildDepTree "MainRepository" new
        newVersions <- forAll $ genNameVersions (newDirDeps ++ newInDirDeps)
        newPkgs <- liftIO $ buildPackageList newVersions newDirDeps newInDirDeps
        liftIO $ insertDiffDependencies "temp.db" newPkgs
        --liftIO $ insertDiffDependencies "temp.db" <$> buildPackageList versions [] newInDirDeps

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

-- | Inserts direct and indirect dependencies into the database
-- via a `Tree`. It then compares what was inserted into the
-- database to what was queried from the database.
prop_db_insert_initial_dependenciesauditor :: Property
prop_db_insert_initial_dependenciesauditor =
    withTests 100 . property $ do
        packages <- forAllT populateTempDb

        -- Query auditor table.
        queried <- liftIO $ queryAuditor "temp.db"
        liftIO $ clearAuditorTable "temp.db"

        -- Compare generated `Package` with `Package` added to auditor table.
        map auditorPackageName queried === map packageName packages
        map auditorPackageVersion queried === map packageVersion packages
        let times = map (pack . formatTime defaultTimeLocale "%F %X%Q" . dateFirstSeen) packages
        map auditorDateFirstSeen queried === times
        map auditorDirectDep queried === map (pack . show . directDep) packages
        map auditorStillUsed queried === map (pack . show . stillUsed) packages
        map auditorAnalysisStatus queried === map (pack . show . analysisStatus) packages



-- | Inserts direct and indirect dependencies into the diff table
-- via a `Tree`. It then compares what was inserted into the
-- diff table to what was queried from the database.
prop_db_insertdiff_dependencies :: Property
prop_db_insertdiff_dependencies =
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
        liftIO $ insertDiffDependencies "temp.db" newPkgs

        -- Update auditor table with the new direct dependencies.
        liftIO $ loadDiffIntoAuditor "temp.db"

        -- Query auditor table
        queried <- liftIO $ queryAuditor "temp.db"
        liftIO $ clearDiffTable "temp.db"
        liftIO $ clearAuditorTable "temp.db"

        -- Compare generated `Package` with `Package` added to auditor table.
        map auditorPackageName queried === map packageName (packages ++ newPkgs)
        map auditorPackageVersion queried === map packageVersion (packages ++ newPkgs)
        let times = map (pack . formatTime defaultTimeLocale "%F %X%Q" . dateFirstSeen) (packages ++ newPkgs)
        map auditorDateFirstSeen queried === times
        map auditorDirectDep queried === map (pack . show . directDep) (packages ++ newPkgs)
        map auditorStillUsed queried === map (pack . show . stillUsed) (packages ++ newPkgs)
        map auditorAnalysisStatus queried === map (pack . show . analysisStatus) (packages ++ newPkgs)

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
        liftIO $ clearAuditorTable "temp.db"

        -- Compare generated `Package` with `Package` added to diff table.
        map diffPackageName queried === [packageName pkg]
        map diffPackageVersion queried === [packageVersion pkg]
        let time = pack . formatTime defaultTimeLocale "%F %X%Q" $ dateFirstSeen pkg
        map diffDateFirstSeen queried === [time]
        map diffDirectDep queried === [pack . show $ directDep pkg]
        map diffStillUsed queried === [pack . show $ stillUsed pkg]
        map diffAnalysisStatus queried === [pack . show $ analysisStatus pkg]

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
        map auditorPackageName queried === [packageName pkg]
        map auditorPackageVersion queried === [packageVersion pkg]
        let time = pack . formatTime defaultTimeLocale "%F %X%Q" $ dateFirstSeen pkg
        map auditorDateFirstSeen queried === [time]
        map auditorDirectDep queried === [pack . show $ directDep pkg]
        map auditorStillUsed queried === [pack . show $ stillUsed pkg]
        map auditorAnalysisStatus queried === [pack . show $ analysisStatus pkg]


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
        packages <- liftIO $ buildPackageList versions dDeps inDeps
        liftIO $ insertDeps "temp.db" packages

        -- Package to be removed
        pkg <- forAll genPackage
        liftIO $ insertDeps "temp.db" [pkg]


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
-- TODO: Test other diff table update functions. Something is wrong and new packages are not being added to Diff table.
prop_updateDiffTableDirectDeps :: Property
prop_updateDiffTableDirectDeps =
    withTests 100 . property $ do

    -- Generate direct deps
    xs <- forAll genSimpleDepList
    let dDeps = directDeps $ buildDepTree "MainRepository" xs
    versions <- forAll $ genNameVersions dDeps
    packages <- liftIO $ buildPackageList versions dDeps []

    -- Insert direct deps into diff table
    liftIO $ updateDiffTableDirectDeps "temp.db" packages

    -- Query diff table
    queried <- liftIO $ queryDiff' "temp.db"
    liftIO $ clearDiffTable "temp.db"

    -- Compare generated `Package` with `Package` added to diff table.
    map diffPackageName queried === map packageName packages
    map diffPackageVersion queried === map packageVersion packages
    let times = map (pack . formatTime defaultTimeLocale "%F %X%Q" . dateFirstSeen) packages
    map diffDateFirstSeen queried === times
    map diffDirectDep queried === map (pack . show . directDep) packages
    map diffStillUsed queried === map (pack . show . stillUsed) packages
    map diffAnalysisStatus queried === map (pack . show . analysisStatus) packages

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
          \CREATE TABLE hash ( dothash INT NOT NULL\
                                \, PRIMARY KEY ( dothash )); \
          \CREATE TABLE diff ( package_name VARCHAR NOT NULL\
                                \, package_version VARCHAR NOT NULL\
                                \, date_first_seen VARCHAR NOT NULL\
                                \, direct_dep VARCHAR NOT NULL\
                                \, still_used VARCHAR NOT NULL\
                                \, analysis_status VARCHAR NOT NULL\
                                \, PRIMARY KEY( package_name )); \""

     remTempDb :: IO ()
     remTempDb = callCommand "rm temp.db"
