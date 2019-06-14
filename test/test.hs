import           Control.Monad     (unless)
import           Hedgehog.Main     (defaultMain)
import           System.Exit       (exitFailure)
import           System.IO         (hSetEncoding, stderr, stdout, utf8)

import qualified Test.Audit.DepTree
import qualified Test.Audit.Properties

main :: IO ()
main =
    Test.Audit.Properties.withTempDB $
        defaultMain
            [ Test.Audit.DepTree.tests
            , Test.Audit.Properties.tests
            ]
