import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main =
  defaultMain $
    testGroup
      "nix-haskell-test-test"
      [ expectFail $ testCase "war is peace" $ 2 + 2 @?= (5 :: Int)
      ]
