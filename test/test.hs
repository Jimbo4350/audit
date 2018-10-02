import           Control.Monad     (unless)
import           System.Exit       (exitFailure)
import           System.IO         (hSetEncoding, stderr, stdout, utf8)
import qualified Test.DepTree
import qualified Test.TempDatabase

main :: IO ()
main = do
     -- ensure UTF-8. As that's what hedgehog needs.
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    Test.TempDatabase.tempDb
    let tests = [ Test.DepTree.tests
                , Test.TempDatabase.tests
                ]
    result <- and <$> sequence tests
    Test.TempDatabase.remTempDb
    unless result exitFailure
