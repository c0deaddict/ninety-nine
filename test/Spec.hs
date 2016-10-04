import qualified Mod11to20           (runTests)
import qualified Mod1to10            (runTests)

import           Control.Monad
import           Test.QuickCheck.All

main :: IO ()
main = void $ Mod1to10.runTests >> Mod11to20.runTests
