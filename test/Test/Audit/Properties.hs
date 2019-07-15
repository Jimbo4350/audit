{-# LANGUAGE TemplateHaskell #-}


module Test.Audit.Properties where

import Audit.Generate (initializeDB)
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
  , evalM
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

import Audit.Conversion (packageToParsedDep)
import Audit.Database (Auditor, AuditorT(..), HashT(..))
import Audit.Operations
  ( newParsedDeps
  , clearAuditorTable
  , deleteHash
  , insertAuditorDeps
  , insertHash
  )

import Audit.Queries
  ( queryAuditor
  , queryAuditorDepVersions
  , queryAuditorRemovedDeps
  , queryHash
  )
import Audit.Tree (buildDepTree, directDeps, indirectDeps)
import Audit.Types (Package(..), QPResult(..), ParsedDependency(..))
import Test.Audit.Gen
  ( genDirectPackage
  , genHash
  , genIndirectPackage
  , genNameVersions
  , genPackage
  , genRemovedPackage
  , genSimpleDepList
  , populateAuditorTempDb
  )


-- | Makes sure that no information is lost in `newParsedDeps`
prop_newParsedDeps :: Property
prop_newParsedDeps =
  withTests 100
    . property
    $ do
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
        let
          dDeps' =
            map (unpack . depName) (filter (\x -> isDirect x == True) pkgs)
        let
          inDeps' = map
            (unpack . depName)
            (filter (\x -> isDirect x == False) pkgs)
        let
          versions' =
            map (\x -> (unpack $ depName x, unpack $ depVersion x)) pkgs
        -- From direct packages
        let
          dDeps'' = map
            (unpack . depName)
            (filter (\x -> isDirect x == True) dirPkgs)
        -- From indirect packages
        let
          inDeps'' = map
            (unpack . depName)
            (filter (\x -> isDirect x == False) indirPkgs)


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
prop_insertAuditorDeps =
  withTests 100
    . property
    $ do
        -- Uses 'insertAuditorDeps' to populate a the auditor table with
        -- simulated newly parsed dependences.
        parsedDeps <- forAllT populateAuditorTempDb

        -- Query auditor table.
        queried  <- liftIO $ queryAuditor "temp.db"
        liftIO $ clearAuditorTable "temp.db"

        -- Compare generated `Package` with `Package` added to auditor table.
        map isDirect parsedDeps === map auditorDirectDep queried
        map depName parsedDeps === map auditorPackageName queried
        map inUse parsedDeps === map auditorStillUsed queried
        map depVersion parsedDeps === map auditorPackageVersion queried
        map firstSeen parsedDeps === map auditorDateFirstSeen queried
        map (pack . show . aStatus ) parsedDeps === map auditorAnalysisStatus queried



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

