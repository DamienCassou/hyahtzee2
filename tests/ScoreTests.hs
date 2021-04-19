module ScoreTests (tests) where

import Hyahtzee2.Figure as Figure
  ( Figure (LFigure),
    LowerFigure (SmallStraight),
  )
import Hyahtzee2.Score as Score (score)
import Test.Tasty
import Test.Tasty.HUnit

tests = testGroup "Score" [scoreSmallStraightTests]

scoreSmallStraightTests =
  testGroup
    "small straight"
    [ testCase "accepts [2,3,4,5] with a value appearing twice" $
        scoreSS [2, 3, 4, 4, 5] @?= 30,
      testCase "accepts [2,3,4,5] with another value" $
        scoreSS [1, 2, 3, 4, 5] @?= 30,
      testCase "accepts [3,4,5,6] with value 5 appearing twice" $
        scoreSS [3, 4, 5, 5, 6] @?= 30
    ]
  where
    scoreSS = score (LFigure SmallStraight)
