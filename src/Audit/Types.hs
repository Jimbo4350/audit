module Audit.Types
  ( AnalysisStatus(..)
  , Command(..)
  , ConversionError(..)
  , DirectDependency
  , HashStatus(..)
  , IndirectDependency
  , NewDependency(..)
  , OperationError(..)
  , OperationResult(..)
  , ParsedRepo(..)
  , InitialDepVersions(..)
  , UpdatedDepVersions(..)
  , Package(..)
  , DependencyName
  , ParsedDependency(..)
  , RepoName
  , Version
  , report
  , reportMultiple
  , renderOperationError
  )
where

import           Audit.Database                 ( Auditor )

import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime(..) )
import           Data.Int                       ( Int32 )

data AnalysisStatus
  = ASGhcBoot   -- Library is a GHC boot library shipped with GHC
  | ASCommon    -- Well know Haskell library with a well know Haskell maintainer
  | ASCritical  -- A library critical to the security/functioning of Cardano
                -- (eg its network facing)
  | ASCrypto    -- A crypto related lib
  | ASUncategoried
  | ASNewDependency
  deriving (Eq, Ord, Show, Read)

newtype Command = Command String

newtype ConversionError = NewlyParsedDepDoesNotExistInDb { parsedDep :: ParsedDependency }
  deriving Show

type DirectDependency = String

type IndirectDependency = String

-- | Tells us if the hash of the existing .dot file matches
-- the hash of a newly generated .dot file or if the hash
-- exists in the db.
data HashStatus =
    HashMatches String
  | HashDoesNotMatch String
  | HashExists Int
  | HashNotFound
  deriving Show


data OperationError =
   ClearAuditorTableError String
 | DeleteAuditorEntryError String
 | DeleteHashError String
 | HashNotFoundError String
 | InsertHashError String
 | InsertAuditorDepsError String
 | InvalidCommand String
 | UpdateAuditorEntryError String
 | ConvError ConversionError
 | NotInAuditorTable String
 | ReadError String

 deriving Show

data OperationResult =
    AuditorDepsInserted
    -- ^ Dependencies successfully inserted into the Auditor table
  | AuditHashDoesNotMatch RepoName
    -- ^ The dependency tree has been updated and therefore the hash
    -- of the original dep tree does not match the hash of the updated
    -- dep tree.
  | AuditHashMatches RepoName
    -- ^ The dependency tree has not been updated, therefore
    -- the hashes of the original and updated dep tree are the
    -- same.
  | AuditHashNotFound
    -- ^ Hash of the dependency tree not found. Most likely
    -- because this is the first time the executable is being run.
  | CleanUpAndUpdateHash
    -- ^ Generate a new current state of the dependency tree.
    -- Delete the old dependency tree's hash in the db, take
    -- a new hash of the current dependency tree and insert it
    -- into the Hash table.
  | HashAlreadyPresent Int
    -- ^ Hash already exists in hash table.
  | HashInserted Int
    -- ^ Hash was not found in the database.
  | HashOperationResult HashStatus
    -- ^ Current hash status.
  | HashDeleted
    -- ^ Hash deleted from the hash table.
  | NoDependencyUpdatesDetected
    -- ^ Hash successfully deleted from the database.
  | SuccessfullyParsedRepos [ParsedRepo]
  | SuccessfullyUpdatedVersion
    -- ^ Updated the version of a particular dependency.
  | UpdatedAuditorTableEntry
    -- ^ Updated the 'stillUsed' flag for specific enteries in the
    -- auditor table
  | VersionExistsInDb [Auditor]
    -- Dependency version exists in the Auditor table.
  | VersionDoesNotExistInDb
    -- Dependency version does not exist in the Auditor table.
  deriving Show


newtype NewDependency =
  NewDependency { getNewDependency :: ParsedDependency }

newtype InitialDepVersions =
  InitialDepVersions { initDeps :: [(DependencyName, Version)] } deriving Show

data ParsedDependency = ParsedDependency
    { repoName   :: Text
    , depName    :: Text
    , depVersion :: Text
    , firstSeen  :: UTCTime
    , isDirect   :: Bool
    , inUse      :: Bool
    , aStatus    :: [AnalysisStatus]
    } deriving (Eq, Ord,Show)

data ParsedRepo = ParsedRepo
  { parsedDeps :: [ParsedDependency]
  , repoHash :: Int
  , parsedRepoName :: RepoName
  } deriving Show

data Package = Package
    { packageId      :: Int32
    , packageName    :: Text
    , packageVersion :: Text
    , dateFirstSeen  :: UTCTime
    , directDep      :: Bool
    , stillUsed      :: Bool
    , analysisStatus :: [AnalysisStatus]
    } deriving (Eq, Ord, Show)


type DependencyName = String
type RepoName = String
newtype UpdatedDepVersions =
  UpdatedDepVersions { updatedDeps :: [(DependencyName, Version)] } deriving Show

type Version = String


--------------------------------------------------------------------------------
-- Reporting
--------------------------------------------------------------------------------


reportMultiple :: Either OperationError [OperationResult] -> IO ()
reportMultiple (Right opRes) = mapM_ (print . show) opRes
reportMultiple (Left  e    ) = print $ renderOperationError e

report :: Either OperationError OperationResult -> IO ()
report (Right opRes) = print $ show opRes
report (Left  e    ) = print $ renderOperationError e

renderOperationError :: OperationError -> String
renderOperationError (ClearAuditorTableError err) =
  "ClearAuditorTableError: " <> show err
renderOperationError (ConvError err) = "ConversionError: " <> show err
renderOperationError (DeleteAuditorEntryError err) =
  "DeleteAuditorEntryError: " <> show err
renderOperationError (DeleteHashError err) = "DeleteHashError: " <> show err
renderOperationError (HashNotFoundError err) = "HashNotFoundError: " <> show err
renderOperationError (InsertHashError err) = "InsertHashError:" <> show err
renderOperationError (InsertAuditorDepsError err) =
  "InsertAuditorDepsError: " <> show err
renderOperationError (InvalidCommand err) = "InvalidCommand: " <> show err
renderOperationError (NotInAuditorTable err) =
  "NotInAuditorTable: " <> show err
renderOperationError (ReadError err) = "ReadError: " <> show err
renderOperationError (UpdateAuditorEntryError err) =
  "UpdateAuditorEntryError: " <> show err

