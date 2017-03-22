import Test.Tasty

import qualified Language.Lambda.Tests
import qualified Language.Materialization.Tests

main :: IO ()
main = putStrLn "" >> defaultMain tests

tests :: TestTree
tests = testGroup "All"
  [ Language.Lambda.Tests.tests
  , Language.Materialization.Tests.tests
  ]
