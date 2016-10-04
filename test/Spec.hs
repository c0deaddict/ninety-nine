import qualified Mod11to20           (runTests)
import qualified Mod1to10            (runTests)
import qualified Mod21to30           (runTests)

import           Control.Monad
import           Test.QuickCheck.All

main :: IO ()
main = do
  Mod1to10.runTests
  Mod11to20.runTests
  void Mod21to30.runTests
