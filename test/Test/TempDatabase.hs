{-# LANGUAGE TemplateHaskell #-}

module Test.TempDatabase where

import           Control.Monad.IO.Class
import           Data.List                  (sort, all)
import           Data.Text                  (pack, unpack)
import           Database                   (Auditor (..), clearAuditorTable,
                                             insertDeps, queryAuditorDepNames,
                                             queryAuditorDepVersions)
import           Hedgehog
import           Hedgehog.Internal.Property (Property, forAll, property,
                                             withTests, (===), assert)
import           System.Process             (callCommand)
import           Test.Gen                   (genSimpleDepList, genNameVersions)
import           Tree                       (buildDepTree, directDeps,
                                             indirectDeps)

prop_db_insert_dependencies :: Property
prop_db_insert_dependencies =
    withTests 100 . property $ do
        xs <- forAll genSimpleDepList
        let dDeps = directDeps $ buildDepTree "MainRepository" xs
        let inDeps = indirectDeps $ buildDepTree "MainRepository" xs
        versions <- forAll $ genNameVersions (dDeps ++ inDeps)
        liftIO $ insertDeps "temp.db" versions dDeps inDeps
        allDbDeps <- liftIO $ queryAuditorDepNames "temp.db"
        allDbVersions <- liftIO $ queryAuditorDepVersions "temp.db"
        liftIO $ clearAuditorTable "temp.db"
        let test = [ (==) (sort allDbDeps) (sort (map pack dDeps ++ map pack inDeps))
                   , ((==) versions (zip (map unpack allDbDeps) (map unpack allDbVersions)))
                   ]
        if all (== True) test then assert True else assert False


-- TODO: Incorporate versions into the insertion property.
-- you will need to generate versions and therefore change
-- your tree structure.

tests :: IO Bool
tests = and <$> sequence
    [ checkParallel $$discover ]

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
