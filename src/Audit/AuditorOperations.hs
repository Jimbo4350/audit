module Audit.AuditorOperations
  ( clearAuditorTable
  , deleteAuditorEntry
  , insertAuditorDeps
  , queryAuditorDepNames
  )
where

import Audit.Conversion (parsedDepToAudQExpr, pkgToAuditor)
import Audit.Database (Auditor, AuditorDb(..), AuditorT(..), auditorDb)
import Audit.Types (Package(..), ParsedDependency(..))
import Database.Beam

import Database.Beam.Sqlite.Connection (runBeamSqlite)
import Database.SQLite.Simple (close, open)
import Data.Text (Text)

--------------------------------------------------------------------------------
-- Auditor Table Operations
--------------------------------------------------------------------------------

-- | Delete the dependencies in the auditor table.
clearAuditorTable :: String -> IO ()
clearAuditorTable fp = do
  conn           <- open fp
  allAuditorDeps <- queryAuditorDepNames fp
  mapM_
    (\x -> runBeamSqlite conn $ runDelete $ delete
      (auditor auditorDb)
      (\c -> auditorPackageName c ==. val_ x)
    )
    allAuditorDeps
  close conn


deleteAuditorEntry :: String -> Auditor -> IO ()
deleteAuditorEntry dbName aud = do
  conn <- open dbName
  runBeamSqlite conn $ runDelete $ delete
    (auditor auditorDb)
    (\table -> auditorPackageName table ==. val_ (auditorPackageName aud))
  close conn

-- | Inserts original direct & indirect dependencies
-- directly into auditor table.
insertAuditorDeps :: String -> [ParsedDependency] -> IO ()
insertAuditorDeps dbFilename pkgs = do
  let audExp = map parsedDepToAudQExpr pkgs
  conn <- open dbFilename
  runBeamSqlite conn
      $ runInsert
      $ insert (auditor auditorDb)
      $ insertExpressions audExp
  close conn

queryAuditorDepNames :: String -> IO [Text]
queryAuditorDepNames dbName  = do
    conn <- open dbName
    let allEntries = all_ (auditor auditorDb)
    entries <- runBeamSqlite conn $
        runSelectReturningList $ select allEntries
    close conn
    return $ map auditorPackageName entries
