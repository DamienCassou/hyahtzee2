import Test.Tasty
import Test.Tasty.HUnit

import qualified UtilTests (tests)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [UtilTests.tests]
