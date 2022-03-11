import Protolude
import ServerSpec (serverSpec1, serverSpec2)
import Test.Hspec (hspec)
import UnitTestsSpec (unitTestsSpec)

main :: IO ()
main = hspec $ do
  unitTestsSpec
  serverSpec1
  serverSpec2
