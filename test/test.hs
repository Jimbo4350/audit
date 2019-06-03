import           Control.Monad     (unless)
import           System.Exit       (exitFailure)
import           System.IO         (hSetEncoding, stderr, stdout, utf8)

import qualified Test.Audit.DepTree
import qualified Test.Audit.TempDatabase

main :: IO ()
main = do
     -- ensure UTF-8 as that's what hedgehog needs.
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    -- Creates a temporary database
    Test.Audit.TempDatabase.tempDb
    let tests = [ Test.Audit.DepTree.tests
                , Test.Audit.TempDatabase.tests
                ]
    result <- and <$> sequence tests
    -- Removes the temporaray database
    Test.Audit.TempDatabase.remTempDb
    unless result exitFailure
