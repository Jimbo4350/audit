{-# LANGUAGE TemplateHaskell #-}

module Test.TempDatabase where

import           Control.Monad.IO.Class
import           Data.List                  (all, sort)
import           Data.Text                  (pack, unpack)
import           Database                   (Auditor (..), clearAuditorTable,
                                             clearDiffTable, insertDeps,
                                             insertRemovedDependencies,
                                             loadDiffIntoAuditor,
                                             queryAuditorDepNames,
                                             queryAuditorDepVersions,
                                             queryAuditorRemovedDeps)
import           Hedgehog
import           Hedgehog.Internal.Property (Property, assert, forAll, property,
                                             withTests, (===))
import           System.Process             (callCommand)
import           Test.Gen                   (genNameVersions, genSimpleDepList)
import           Tree                       (buildDepTree, directDeps,
                                             indirectDeps)


-- | Inserts direct and indirect dependencies into the database
-- via a `Tree`. It then compares what was inserted into the
-- database to what was queried from the database.

prop_db_insert_dependencies :: Property
prop_db_insert_dependencies =
    withTests 100 . property $ do
        xs <- forAll genSimpleDepList
        let dDeps = directDeps $ buildDepTree "MainRepository" xs
        let inDeps = indirectDeps $ buildDepTree "MainRepository" xs
        versions <- forAll $ genNameVersions (dDeps ++ inDeps)
        liftIO $ insertDeps "temp.db" versions dDeps inDeps
        queriedDeps <- liftIO $ queryAuditorDepNames "temp.db"
        queriedVers <- liftIO $ queryAuditorDepVersions "temp.db"
        liftIO $ clearAuditorTable "temp.db"
        let tests = [ (==) (sort queriedDeps) (sort (map pack dDeps ++ map pack inDeps))
                    , (==) (zip (map unpack queriedDeps) (map unpack queriedVers)) versions
                    ]
        if all (== True) tests then assert True else assert False
{-
prop_db_insert_removed_indirect_dependencies :: Property
prop_db_insert_removed_indirect_dependencies =
    withTests 1 . property $ do
        liftIO tempDb
        xs <- forAll genSimpleDepList
        let dDeps = directDeps $ buildDepTree "MainRepository" xs
        let inDeps = indirectDeps $ buildDepTree "MainRepository" xs
        versions <- forAll $ genNameVersions (dDeps ++ inDeps)
        -- inserts into auditor table
        liftIO $ insertDeps "temp.db" versions dDeps inDeps

        liftIO $ insertRemovedDependencies "temp.db" (map pack inDeps) False False
        liftIO $ loadDiffIntoAuditor "temp.db"
        queriedRemDeps <- liftIO $ queryAuditorRemovedDeps "temp.db"
        --liftIO $ clearDiffTable "temp.db"
       -- liftIO $ clearAuditorTable "temp.db"
        let tests = [ (==) (sort queriedRemDeps) (sort $ map pack dDeps)
                    ]
        if all (== True) tests then assert True else assert False

       -- sort queriedRemDeps === (sort $ map pack dDeps)
-}


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
