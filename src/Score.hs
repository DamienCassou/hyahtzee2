module Score where

import Data.List (elemIndex, isSubsequenceOf)

import Types (Scoring, Throw)
import Data.Maybe (fromMaybe)

-- | Returns a count of the number of times the given element occured
-- in the given list.
--
-- >>> count 1 [1, 1, 1, 3, 4]
-- 3
count :: Eq a => a -> [a] -> Int
count elem =
  foldl
    (\count each -> if each == elem then count + 1 else count)
    0

-- | Return the score obtained if a throw is written in a line of the
-- upper section.
--
-- >>> scoreUpperSection 1 [1, 2, 3, 4, 5]
-- 1
-- >>> scoreUpperSection 3 [1, 2, 3, 3, 3]
-- 9
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
--
-- >>> scoreThreeOfAKind [2, 2, 2, 4, 4]
-- 14
-- >>> scoreThreeOfAKind [2, 3, 4, 4, 4]
-- 17
-- >>> scoreThreeOfAKind [1, 4, 4, 4, 5]
-- 18
-- >>> scoreThreeOfAKind [2, 4, 4, 4, 4]
-- 18
-- >>> scoreThreeOfAKind [2, 3, 4, 4, 5]
-- 0
scoreThreeOfAKind :: Scoring
scoreThreeOfAKind throw@[d1, d2, d3, d4, d5]
  | d1 == d3 || d2 == d4 || d3 == d5 = sum throw
  | otherwise = 0

-- | Count four-of-a-kind score with provided dice
--
-- >>> scoreFourOfAKind [4, 4, 4, 4, 5]
-- 21
-- >>> scoreFourOfAKind [4, 5, 5, 5, 5]
-- 24
-- >>> scoreFourOfAKind [5, 5, 5, 5, 5]
-- 25
-- >>> scoreFourOfAKind [2, 3, 4, 4, 5]
-- 0
-- >>> scoreFourOfAKind [2, 2, 2, 4, 4]
-- 0
scoreFourOfAKind :: Scoring
scoreFourOfAKind throw@[d1, d2, d3, d4, d5]
  | d1 == d4 || d2 == d5 = sum throw
  | otherwise = 0

-- | Count full-house score with provided dice
--
-- >>> scoreFullHouse [2, 2, 2, 4, 4]
-- 25
-- >>> scoreFullHouse [2, 2, 4, 4, 4]
-- 25
-- >>> scoreFullHouse [2, 3, 4, 4, 5]
-- 0
-- >>> scoreFullHouse [4, 4, 4, 4, 5]
-- 0
-- >>> scoreFullHouse [4, 5, 5, 5, 5]
-- 0
-- >>> scoreFullHouse [5, 5, 5, 5, 5]
-- 0
scoreFullHouse :: Scoring
scoreFullHouse throw@[d1, d2, d3, d4, d5]
  | d1 == d3 && d4 == d5 && d1 /= d5 = 25
  | d1 == d2 && d3 == d5 && d1 /= d5 = 25
  | otherwise = 0

scoreStraight :: Throw -> [[Int]] -> Bool
scoreStraight throw = any (`isSubsequenceOf` throw)

-- | Count small-straight score with provided dice
--
-- >>> scoreSmallStraight [2, 3, 4, 4, 5]
-- 30
-- >>> scoreSmallStraight [1, 2, 3, 4, 5]
-- 30
-- >>> scoreSmallStraight [1, 2, 3, 4, 6]
-- 30
-- >>> scoreSmallStraight [1, 3, 4, 5, 6]
-- 30
-- >>> scoreSmallStraight [2, 2, 2, 4, 4]
-- 0
-- >>> scoreSmallStraight [2, 2, 4, 4, 4]
-- 0
scoreSmallStraight :: Scoring
scoreSmallStraight throw
  | scoreStraight throw [[1, 2, 3, 4], [2, 3, 4, 5], [3, 4, 5, 6]] = 30
  | otherwise = 0

-- | Count large-straight score with provided dice
--
-- >>> scoreLargeStraight [1, 2, 3, 4, 5]
-- 40
-- >>> scoreLargeStraight [2, 3, 4, 5, 6]
-- 40
-- >>> scoreLargeStraight [2, 2, 2, 4, 4]
-- 0
-- >>> scoreLargeStraight [2, 2, 4, 4, 4]
-- 0
-- >>> scoreLargeStraight [2, 3, 4, 4, 5]
-- 0
-- >>> scoreLargeStraight [1, 2, 3, 4, 6]
-- 0
-- >>> scoreLargeStraight [1, 3, 4, 5, 6]
-- 0
scoreLargeStraight :: Scoring
scoreLargeStraight throw
  | scoreStraight throw [[1, 2, 3, 4, 5], [2, 3, 4, 5, 6]] = 40
  | otherwise = 0

-- | Count yahtzee score with provided dice
--
-- >>> scoreYahtzee [1, 1, 1, 1, 1]
-- 50
-- >>> scoreYahtzee [2, 3, 4, 5, 6]
-- 0
-- >>> scoreYahtzee [2, 2, 2, 4, 4]
-- 0
-- >>> scoreYahtzee [2, 2, 4, 4, 4]
-- 0
-- >>> scoreYahtzee [2, 3, 4, 4, 5]
-- 0
-- >>> scoreYahtzee [1, 2, 3, 4, 6]
-- 0
-- >>> scoreYahtzee [1, 3, 4, 5, 6]
-- 0
scoreYahtzee :: Scoring
scoreYahtzee [d1, d2, d3, d4, d5]
  | d1 == d5 = 50
  | otherwise = 0

-- | Count chance score with provided dice
--
-- >>> scoreChance [1, 1, 1, 1, 1]
-- 5
-- >>> scoreChance [2, 3, 4, 5, 6]
-- 20
-- >>> scoreChance [2, 2, 2, 4, 4]
-- 14
-- >>> scoreChance [2, 2, 4, 4, 4]
-- 16
-- >>> scoreChance [2, 3, 4, 4, 5]
-- 18
scoreChance :: Scoring
scoreChance = sum
