module ScoreTests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Hyahtzee2.Figure as Figure
  ( LowerFigure(SmallStraight)
  , Figure(LFigure)
  )

import Hyahtzee2.Score as Score (score)

tests = testGroup "Score" [scoreSmallStraightTests]

scoreSmallStraightTests = testGroup "small straight"
  [ testCase "accepts [2,3,4,5] with a value appearing twice" $
    scoreSS [2,3,4,4,5] @?= 30

  , testCase "accepts [2,3,4,5] with another value" $
    scoreSS [1,2,3,4,5] @?= 30

  , testCase "accepts [3,4,5,6] with value 5 appearing twice" $
    scoreSS [3,4,5,5,6] @?= 30
  ]
  where scoreSS = score (LFigure SmallStraight)
