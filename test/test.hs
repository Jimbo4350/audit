import           Control.Monad     (unless)
import           System.Exit       (exitFailure)
import           System.IO         (hSetEncoding, stderr, stdout, utf8)
import qualified Test.DepTree
import qualified Test.TempDatabase

main :: IO ()
main = do
     -- ensure UTF-8 as that's what hedgehog needs.
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    -- Creates a temporary database
    Test.TempDatabase.tempDb
    let tests = [ Test.DepTree.tests
                , Test.TempDatabase.tests
                ]
    result <- and <$> sequence tests
    -- Removes the temporaray database
    Test.TempDatabase.remTempDb
    unless result exitFailure
