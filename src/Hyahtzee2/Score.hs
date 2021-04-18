{-# LANGUAGE Safe #-}

module Hyahtzee2.Score (score) where

import qualified Data.List as List (isSubsequenceOf)

import qualified Hyahtzee2.Types as Types
  ( Figure (UFigure, LFigure)
  , UpperFigure (Aces, Twos, Threes, Fours, Fives, Sixes)
  , LowerFigure (ThreeOfAKind, FourOfAKind, SmallStraight, LargeStraight, FullHouse, Yahtzee, Chance)
  )

type Throw = [Int]
type Scoring = Throw -> Int

-- | Returns a count of the number of times the given element occured
-- in the given list.
count :: Eq a => a -> [a] -> Int
count item =
  foldl
    (\count' each -> if each == item then count' + 1 else count')
    0

-- | Return the score obtained if a throw is written in a line of the
-- upper section.
scoreUpperSection :: Int -> Throw -> Int
scoreUpperSection category throw =
  category * count category throw

scoreAces :: Scoring
scoreAces = scoreUpperSection 1

scoreTwos :: Scoring
scoreTwos = scoreUpperSection 2

scoreThrees :: Scoring
scoreThrees = scoreUpperSection 3

scoreFours :: Scoring
scoreFours = scoreUpperSection 4

scoreFives :: Scoring
scoreFives = scoreUpperSection 5

scoreSixes :: Scoring
scoreSixes = scoreUpperSection 6

-- Lower section

-- | Count three-of-a-kind score with provided dice
scoreThreeOfAKind :: Scoring
scoreThreeOfAKind throw@[d1, d2, d3, d4, d5]
  | d1 == d3 || d2 == d4 || d3 == d5 = sum throw
  | otherwise = 0
scoreThreeOfAKind _ = 0

-- | Count four-of-a-kind score with provided dice
scoreFourOfAKind :: Scoring
scoreFourOfAKind throw@[d1, d2, _, d4, d5]
  | d1 == d4 || d2 == d5 = sum throw
  | otherwise = 0
scoreFourOfAKind _ = 0

-- | Count full-house score with provided dice
scoreFullHouse :: Scoring
scoreFullHouse [d1, d2, d3, d4, d5]
  | d1 == d3 && d4 == d5 && d1 /= d5 = 25
  | d1 == d2 && d3 == d5 && d1 /= d5 = 25
  | otherwise = 0
scoreFullHouse _ = 0

scoreStraight :: Throw -> [[Int]] -> Bool
scoreStraight throw = any (`List.isSubsequenceOf` throw)

-- | Count small-straight score with provided dice
scoreSmallStraight :: Scoring
scoreSmallStraight throw
  | scoreStraight throw [[1, 2, 3, 4], [2, 3, 4, 5], [3, 4, 5, 6]] = 30
  | otherwise = 0

-- | Count large-straight score with provided dice
scoreLargeStraight :: Scoring
scoreLargeStraight throw
  | scoreStraight throw [[1, 2, 3, 4, 5], [2, 3, 4, 5, 6]] = 40
  | otherwise = 0

-- | Count yahtzee score with provided dice
scoreYahtzee :: Scoring
scoreYahtzee [d1, _, _, _, d5]
  | d1 == d5 = 50
  | otherwise = 0
scoreYahtzee _ = 0

-- | Count chance score with provided dice
scoreChance :: Scoring
scoreChance = sum

score :: Types.Figure -> [Int] -> Int
score (Types.UFigure Types.Aces) = scoreAces
score (Types.UFigure Types.Twos) = scoreTwos
score (Types.UFigure Types.Threes) = scoreThrees
score (Types.UFigure Types.Fours) = scoreFours
score (Types.UFigure Types.Fives) = scoreFives
score (Types.UFigure Types.Sixes) = scoreSixes
score (Types.LFigure Types.ThreeOfAKind) = scoreThreeOfAKind
score (Types.LFigure Types.FourOfAKind) = scoreFourOfAKind
score (Types.LFigure Types.SmallStraight) = scoreSmallStraight
score (Types.LFigure Types.LargeStraight) = scoreLargeStraight
score (Types.LFigure Types.FullHouse) = scoreFullHouse
score (Types.LFigure Types.Yahtzee) = scoreYahtzee
score (Types.LFigure Types.Chance) = scoreChance
