module Test.TempDatabase
       ( remTempDb
       , tempDb
       ) where

import           Hedgehog.Internal.Property (Property, forAll, property, (===))
import           System.Process             (callCommand)
import           Test.Gen                   (genNameVersion)

-- TODO: You need to generate a proper test dependency tree.
prop_insert_current_deps :: Property
prop_insert_current_deps = undefined

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
