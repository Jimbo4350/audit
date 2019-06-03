module Main where

import           Options.Applicative

import           Audit.Generate            (commandHandler)
import           Audit.Parser              (getCommand)

-- | Usage example "stack exec -- audit-exe --audit audit"

main :: IO ()
main = commandHandler =<< execParser opts
  where
    opts = info (getCommand <**> helper)
      ( fullDesc
     <> header "audit - a dependency tracking tool" )

