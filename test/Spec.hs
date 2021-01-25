import qualified Test.Tasty as Tasty
import           Test.Tasty.Hspec (shouldBe, describe, it, testSpec)
import           Data.List (sort)

import Lib

main = do
  tree <- testSpec "Tests" spec_dice
  Tasty.defaultMain tree

spec_dice = do
  describe "Face" $ do
    it "specifies an order between elements" $
      sort [Six, Five, Four, Three, Two, One] `shouldBe` faces
    it "can be printed" $
      map show faces `shouldBe` ["One", "Two", "Three", "Four", "Five", "Six"]
