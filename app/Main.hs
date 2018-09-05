module Main where

import           Generate (createDeps, streamDeps)

main :: IO ()
main = createDeps >> streamDeps


