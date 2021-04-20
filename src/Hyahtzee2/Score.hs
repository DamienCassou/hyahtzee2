{-# LANGUAGE Safe #-}

-- |
-- Description : A function returning the score associated with a figure and an array of die values
-- Copyright   : (c) Damien Cassou, 2021
-- License     : BSD-3-Clause
module Hyahtzee2.Score (score) where

import qualified Data.List as List (isSubsequenceOf)
import qualified Hyahtzee2.Figure as Figure
  ( Figure (LFigure, UFigure),
    LowerFigure (Chance, FourOfAKind, FullHouse, LargeStraight, SmallStraight, ThreeOfAKind, Yahtzee),
    UpperFigure (Aces, Fives, Fours, Sixes, Threes, Twos),
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

-- | Returns the score (an integer) associated with a given `Figure.Figure`
-- and array of die values. For example, @[1, 2, 3, 4, 5]@ is worth a
-- score of 1 in `Figure.Aces` category, 30 in `Figure.SmallStraight`, and 40 in
-- `Figure.LargeStraight`.
score :: Figure.Figure -> [Int] -> Int
score (Figure.UFigure Figure.Aces) = scoreAces
score (Figure.UFigure Figure.Twos) = scoreTwos
score (Figure.UFigure Figure.Threes) = scoreThrees
score (Figure.UFigure Figure.Fours) = scoreFours
score (Figure.UFigure Figure.Fives) = scoreFives
score (Figure.UFigure Figure.Sixes) = scoreSixes
score (Figure.LFigure Figure.ThreeOfAKind) = scoreThreeOfAKind
score (Figure.LFigure Figure.FourOfAKind) = scoreFourOfAKind
score (Figure.LFigure Figure.SmallStraight) = scoreSmallStraight
score (Figure.LFigure Figure.LargeStraight) = scoreLargeStraight
score (Figure.LFigure Figure.FullHouse) = scoreFullHouse
score (Figure.LFigure Figure.Yahtzee) = scoreYahtzee
score (Figure.LFigure Figure.Chance) = scoreChance
