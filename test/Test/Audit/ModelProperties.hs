{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


module Test.Audit.ModelProperties where

import           Audit.Conversion               ( auditorEntryToParsedDep
                                                , parsedDepToAuditor
                                                )
import           Audit.Database                 ( Auditor
                                                , AuditorT(..)
                                                )
import           Audit.Operations               ( clearAuditorTable
                                                , insertAuditorDeps
                                                , updateAuditorEntryDirect
                                                )
import           Audit.Queries                  ( queryAuditor )
import           Audit.Types                    ( AnalysisStatus
                                                , OperationResult(..)
                                                , ParsedDependency(..)
                                                )

import           Hedgehog
import           Hedgehog.Internal.Tree
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.List                      ( (\\) )
import           Data.Text                      ( pack )
import           System.Random
import           Test.Audit.Gen                 ( genDirectParsedDependency
                                                , genAuditorEntries
                                                , genIndirectParsedDependency
                                                )



data DatabaseStateVar (v :: * -> *) =
  DatabaseStateVar
    { depToAdd :: [ParsedDependency]
    , database :: [Var Auditor v]
    }
---------------------------------------------------------------------
-- Inputs
---------------------------------------------------------------------

data GenDependency (v :: * -> *) =
  GenDependency deriving (Eq,Show)

instance HTraversable GenDependency where
  htraverse _ _ = pure GenDependency

cGenDependency
  :: (MonadGen n, MonadIO m, MonadTest m, MonadGen m) => Command n m DatabaseStateVar
cGenDependency =
  let
    gen _ = Just $ pure GenDependency
    execute _ = pure ()
  in
    Command
      gen
      execute
      [ Update $ \(DatabaseStateVar shouldBeEmpty database) input o ->
          DatabaseStateVar shouldBeEmpty database
      ]


newtype AddDependency (v :: * -> *) =
    AddDependency ParsedDependency deriving (Eq ,Show)

instance HTraversable AddDependency where
  htraverse _ (AddDependency depsToAdd) = pure $ AddDependency depsToAdd

cAddParsedDependencies
  :: (MonadGen n, MonadIO m, MonadTest m) => Command n m DatabaseStateVar
cAddParsedDependencies =
  let
      -- given a value of the state, generate an input.
    gen (DatabaseStateVar depsToAdd database) =  Just $ AddDependency <$> Gen.element depsToAdd
    execute (AddDependency depToAdd) = do
      res <- liftIO . runEitherT $ insertAuditorDeps "temp.db" [depToAdd]
      case res of
        Right (AuditorDepsInserted [audVal]) -> pure audVal
        Left err ->
          error
            $  "Test.Audit.ModelProperties.cAddParsedDependencies: "
            ++ show err
  in
    Command
      gen
      execute
      [ -- Compare the parsed deps with Auditor values, make sure they are the same.
        Update $ \(DatabaseStateVar depsToAddDb database) _i out ->
            -- Get values from Auditor
        DatabaseStateVar depsToAddDb (database ++ [out])
      , Ensure
        $ \(DatabaseStateVar beforeDepToAddDb beforeDatabase) (DatabaseStateVar afterDepToAddDb afterDatabase) i out ->
            map concrete afterDatabase \\ map concrete beforeDatabase === [out]
      ]

newtype DeleteDependencies (v :: * -> *) =
    DeleteDependencies (Var Auditor v) deriving (Eq, Show)

instance HTraversable DeleteDependencies where
  htraverse f (DeleteDependencies (Var delDep)) =
    DeleteDependencies . Var <$> f delDep

-- Deleting dependencies flips the 'stillUsed' columnn to false.
cDeleteDependencies
  :: (MonadIO m, MonadTest m) => Command Gen m DatabaseStateVar
cDeleteDependencies =
  let
    -- Need to access the state to run the delete
    gen (DatabaseStateVar depsToAdd database) = if null database
      then Nothing
      else Just $ DeleteDependencies <$> Gen.element database
    execute (DeleteDependencies vDep) = do
      let depToDelete = (concrete vDep) { auditorStillUsed = False }
      res <- liftIO . runEitherT $ updateAuditorEntryDirect "temp.db"
                                                            depToDelete
      case res of
        Left err ->
          error $ "Test.Audit.ModelProperties.cDeleteDependencies: " ++ show err
        Right (UpdatedAuditorTableEntry val) -> return val
  in
    Command
      gen
      execute
      [ Update $ \(DatabaseStateVar depsToAdd database) input output ->
        DatabaseStateVar depsToAdd (database \\ [output])
      , Ensure
        $ \(DatabaseStateVar _ prevDatabase) (DatabaseStateVar _ upDatabase) _input output ->
              -- This should fail because you will randomly select an Entry that has already been deleted.
            map concrete upDatabase \\ map concrete prevDatabase === [output]
      ]


---------------------------------------------------------------------
-- Positive Properties
---------------------------------------------------------------------

-- Checks the number of added parsed dependencies is correct.
prop_AddParsedDependencies :: Property
prop_AddParsedDependencies = property $ do

  pDep <- forAll genDirectParsedDependency

  let commands     = [cDeleteDependencies, cAddParsedDependencies]
      -- Need to have another command to step the state machine forward.
      initialState = DatabaseStateVar [pDep] [] :: DatabaseStateVar v

  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialState commands
  evalIO $ clearAuditorTable "temp.db"
  executeSequential initialState actions

---------------------------------------------------------------------
-- Negative Properties
---------------------------------------------------------------------


---------------------------------------------------------------------
-- Helpers
---------------------------------------------------------------------


tests :: IO Bool
tests = and <$> sequence [checkSequential $$discover]
