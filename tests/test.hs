import Test.Tasty
import Test.Tasty.HUnit

import qualified ScoreTests (tests)
import qualified UtilTests (tests)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ ScoreTests.tests
  , UtilTests.tests
  ]
