module Score where

import Data.List (elemIndex, isSubsequenceOf)

import Types (Scoring, Throw, Score(NoScore, Aces, Twos, Threes, Fours, Fives, Sixes, ThreeOfAKind, FourOfAKind, FullHouse, SmallStraight, LargeStraight, Yahtzee, Chance))

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

first = head

second values = values !! 1

third values = values !! 2

fourth values = values !! 3

fifth values = values !! 4

-- | Upper section Scoring
--
-- >>> scoreAces [2, 2, 3, 3, 4]
-- NoScore
-- >>> scoreAces [1, 1, 1, 3, 4]
-- Aces 3
-- >>> scoreTwos [2, 2, 2, 5, 6]
-- Twos 6
-- >>> scoreThrees [3, 3, 3, 3, 4]
-- Threes 12
-- >>> scoreFours [4, 4, 5, 5, 5]
-- Fours 8
-- >>> scoreFives [1, 1, 2, 2, 5]
-- Fives 5
-- >>> scoreSixes [2, 3, 6, 6, 6]
-- Sixes 18
scoreUpperSection :: Int -> (Int -> Score) -> Throw -> Score
scoreUpperSection category constructor values =
  if score == 0 then NoScore else constructor score
  where
    score = category * count category values

scoreAces :: Scoring
scoreAces = scoreUpperSection 1 Aces

scoreTwos :: Scoring
scoreTwos = scoreUpperSection 2 Twos

scoreThrees :: Scoring
scoreThrees = scoreUpperSection 3 Threes

scoreFours :: Scoring
scoreFours = scoreUpperSection 4 Fours

scoreFives :: Scoring
scoreFives = scoreUpperSection 5 Fives

scoreSixes :: Scoring
scoreSixes = scoreUpperSection 6 Sixes

-- Lower section

-- | Count three-of-a-kind score with provided dice
--
-- >>> scoreThreeOfAKind [2, 2, 2, 4, 4]
-- ThreeOfAKind 14
-- >>> scoreThreeOfAKind [2, 3, 4, 4, 4]
-- ThreeOfAKind 17
-- >>> scoreThreeOfAKind [1, 4, 4, 4, 5]
-- ThreeOfAKind 18
-- >>> scoreThreeOfAKind [2, 4, 4, 4, 4]
-- ThreeOfAKind 18
-- >>> scoreThreeOfAKind [2, 3, 4, 4, 5]
-- NoScore
scoreThreeOfAKind :: Scoring
scoreThreeOfAKind throw@[d1, d2, d3, d4, d5]
  | d1 == d3 || d2 == d4 || d3 == d5 = ThreeOfAKind $ sum throw
  | otherwise = NoScore

-- | Count four-of-a-kind score with provided dice
--
-- >>> scoreFourOfAKind [4, 4, 4, 4, 5]
-- FourOfAKind 21
-- >>> scoreFourOfAKind [4, 5, 5, 5, 5]
-- FourOfAKind 24
-- >>> scoreFourOfAKind [5, 5, 5, 5, 5]
-- FourOfAKind 25
-- >>> scoreFourOfAKind [2, 3, 4, 4, 5]
-- NoScore
-- >>> scoreFourOfAKind [2, 2, 2, 4, 4]
-- NoScore
scoreFourOfAKind :: Scoring
scoreFourOfAKind throw@[d1, d2, d3, d4, d5]
  | d1 == d4 || d2 == d5 = FourOfAKind $ sum throw
  | otherwise = NoScore

-- | Count full-house score with provided dice
--
-- >>> scoreFullHouse [2, 2, 2, 4, 4]
-- FullHouse
-- >>> scoreFullHouse [2, 2, 4, 4, 4]
-- FullHouse
-- >>> scoreFullHouse [2, 3, 4, 4, 5]
-- NoScore
-- >>> scoreFullHouse [4, 4, 4, 4, 5]
-- NoScore
-- >>> scoreFullHouse [4, 5, 5, 5, 5]
-- NoScore
-- >>> scoreFullHouse [5, 5, 5, 5, 5]
-- NoScore
scoreFullHouse :: Scoring
scoreFullHouse throw@[d1, d2, d3, d4, d5]
  | d1 == d3 && d4 == d5 && d1 /= d5 = FullHouse
  | d1 == d2 && d3 == d5 && d1 /= d5 = FullHouse
  | otherwise = NoScore

scoreStraight :: Throw -> [[Int]] -> Bool
scoreStraight throw = any (`isSubsequenceOf` throw)

-- | Count small-straight score with provided dice
--
-- >>> scoreSmallStraight [2, 3, 4, 4, 5]
-- SmallStraight
-- >>> scoreSmallStraight [1, 2, 3, 4, 5]
-- SmallStraight
-- >>> scoreSmallStraight [1, 2, 3, 4, 6]
-- SmallStraight
-- >>> scoreSmallStraight [1, 3, 4, 5, 6]
-- SmallStraight
-- >>> scoreSmallStraight [2, 2, 2, 4, 4]
-- NoScore
-- >>> scoreSmallStraight [2, 2, 4, 4, 4]
-- NoScore
scoreSmallStraight :: Scoring
scoreSmallStraight throw
  | scoreStraight throw [[1, 2, 3, 4], [2, 3, 4, 5], [3, 4, 5, 6]] = SmallStraight
  | otherwise = NoScore

-- | Count large-straight score with provided dice
--
-- >>> scoreLargeStraight [1, 2, 3, 4, 5]
-- LargeStraight
-- >>> scoreLargeStraight [2, 3, 4, 5, 6]
-- LargeStraight
-- >>> scoreLargeStraight [2, 2, 2, 4, 4]
-- NoScore
-- >>> scoreLargeStraight [2, 2, 4, 4, 4]
-- NoScore
-- >>> scoreLargeStraight [2, 3, 4, 4, 5]
-- NoScore
-- >>> scoreLargeStraight [1, 2, 3, 4, 6]
-- NoScore
-- >>> scoreLargeStraight [1, 3, 4, 5, 6]
-- NoScore
scoreLargeStraight :: Scoring
scoreLargeStraight throw
  | scoreStraight throw [[1, 2, 3, 4, 5], [2, 3, 4, 5, 6]] = LargeStraight
  | otherwise = NoScore

-- | Count yahtzee score with provided dice
--
-- >>> scoreYahtzee [1, 1, 1, 1, 1]
-- Yahtzee
-- >>> scoreYahtzee [2, 3, 4, 5, 6]
-- NoScore
-- >>> scoreYahtzee [2, 2, 2, 4, 4]
-- NoScore
-- >>> scoreYahtzee [2, 2, 4, 4, 4]
-- NoScore
-- >>> scoreYahtzee [2, 3, 4, 4, 5]
-- NoScore
-- >>> scoreYahtzee [1, 2, 3, 4, 6]
-- NoScore
-- >>> scoreYahtzee [1, 3, 4, 5, 6]
-- NoScore
scoreYahtzee :: Scoring
scoreYahtzee [d1, d2, d3, d4, d5]
  | d1 == d5 = Yahtzee
  | otherwise = NoScore

-- | Count chance score with provided dice
--
-- >>> scoreChance [1, 1, 1, 1, 1]
-- Chance 5
-- >>> scoreChance [2, 3, 4, 5, 6]
-- Chance 20
-- >>> scoreChance [2, 2, 2, 4, 4]
-- Chance 14
-- >>> scoreChance [2, 2, 4, 4, 4]
-- Chance 16
-- >>> scoreChance [2, 3, 4, 4, 5]
-- Chance 18
scoreChance :: Scoring
scoreChance throw = Chance $ sum throw
