import           Control.Exception (bracket_)
import           Control.Monad     (unless)
import           Hedgehog.Main     (defaultMain)
import           System.Exit       (exitFailure)
import           System.IO         (hSetEncoding, stderr, stdout, utf8)

import qualified Test.Audit.DepTree
import qualified Test.Audit.TempDatabase

main :: IO ()
main = do
    bracket_
        -- Creates a temporary database
        Test.Audit.TempDatabase.tempDb
        runTests
        -- Removes the temporaray database
        Test.Audit.TempDatabase.remTempDb

  where
    runTests :: IO ()
    runTests =
        defaultMain
            [ Test.Audit.DepTree.tests
            , Test.Audit.TempDatabase.tests
            ]
