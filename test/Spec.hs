-- import qualified Test.Tasty as Tasty
-- import           Test.Tasty.Hspec (shouldBe, describe, it, testSpec)
-- import           Data.List (sort)
import           Test.DocTest as DocTest (doctest)
import           Control.Exception (catch, throwIO)
import           System.Exit ( ExitCode(ExitSuccess) )

main = do
  -- tree <- testSpec "Tests" spec_dice
  -- Tasty.defaultMain tree
  -- `catch` (\e -> do
  --             if e == ExitSuccess
  --               then
                doctest ["-isrc", "src/Dice.hs", "src/Round.hs", "src/Score.hs", "src/ScoreCard.hs", "src/Types.hs"]
                -- else throwIO e)

-- spec_dice = do
--   describe "Face" $ do
--     it "specifies an order between elements" $
--       sort [Six, Five, Four, Three, Two, One] `shouldBe` faces
--     it "can be printed" $
--       map show faces `shouldBe` ["One", "Two", "Three", "Four", "Five", "Six"]
