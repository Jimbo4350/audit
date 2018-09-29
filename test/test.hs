import           Control.Monad     (unless)
import           System.Exit       (exitFailure)
import           System.IO         (hSetEncoding, stderr, stdout, utf8)
import qualified Test.TempDatabase

main :: IO ()
main = do
     -- ensure UTF-8. As that's what hedgehog needs.
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    let tests = [ Test.TempDatabase.tests ]
    result <- and <$> sequence tests
    unless result
        exitFailure
