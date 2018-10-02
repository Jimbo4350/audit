{-# LANGUAGE TemplateHaskell #-}

module Test.TempDatabase where

import           Control.Monad.IO.Class
import           Data.List                  (sort)
import           Data.Text                  (unpack)
import           Database                   (Auditor (..), clearAuditorTable,
                                             insertDeps, queryAuditorDepNames)
import           Hedgehog
import           Hedgehog.Internal.Property (Property, forAll, property,
                                             withTests, (===))
import           System.Process             (callCommand)
import           Test.Gen                   (genSimpleDepList)
import           Tree                       (buildDepTree, directDeps,
                                             indirectDeps)

prop_db_insert_dependencies :: Property
prop_db_insert_dependencies =
    withTests 1 . property $ do
        xs <- forAll genSimpleDepList
        let dDeps = directDeps $ buildDepTree "MainRepository" xs
        let inDeps = indirectDeps $ buildDepTree "MainRepository" xs
        liftIO $ insertDeps "temp.db" [] dDeps inDeps
        allDbDeps <- liftIO $ queryAuditorDepNames "temp.db"
        liftIO $ clearAuditorTable "temp.db"
        sort (map unpack allDbDeps) === sort (dDeps ++ inDeps)

tests :: IO Bool
tests = and <$> sequence
    [ checkParallel $$(discover) ]

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
