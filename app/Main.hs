module Main where

import           Generate            (audit, commandHandler, createDB,
                                      createDeps, insertDB, insertNewDirectDB,
                                      insertNewIndirectDB, update)
import           Options.Applicative
import           Parser              (getCommand)
import           Sqlite              (queryAuditor)
import           Types               (Command (..), HashStatus (..))

-- | Usage example "stack exec -- audit-exe --audit audit"

main :: IO ()
main = commandHandler =<< execParser opts
  where
    opts = info (getCommand <**> helper)
      ( fullDesc
     <> header "audit - a dependency tracking tool" )

