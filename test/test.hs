import           Control.Monad     (unless)
import           Hedgehog.Main     (defaultMain)
import           System.Exit       (exitFailure)
import           System.IO         (hSetEncoding, stderr, stdout, utf8)

import qualified Test.Audit.DepTree
import qualified Test.Audit.TempDatabase

main :: IO ()
main =
    Test.Audit.TempDatabase.withTempDB $
        defaultMain
            [ Test.Audit.DepTree.tests
            , Test.Audit.TempDatabase.tests
            ]
