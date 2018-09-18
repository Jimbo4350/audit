module Main where

import           Generate            (commandHandler)
import           Options.Applicative
import           Parser              (getCommand)

-- | Usage example "stack exec -- audit-exe --audit audit"

main :: IO ()
main = commandHandler =<< execParser opts
  where
    opts = info (getCommand <**> helper)
      ( fullDesc
     <> header "audit - a dependency tracking tool" )

