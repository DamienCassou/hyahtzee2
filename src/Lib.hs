module Lib where

import Data.List (elemIndex)

data Dice = One | Two | Three | Four | Five | Six
  deriving (Eq, Show, Ord)

faces = [One, Two, Three, Four, Five, Six]

-- | Return the numeric value of a Dice
--
-- >>> value One
-- 1
value :: Dice -> Int
value dice = case elemIndex dice faces of
  Just value -> value + 1
  Nothing -> -1 -- should not happen

-- | Returns a count of the number of times the given element occured
-- in the given list.
--
-- >>> count 1 [1, 1, 1, 3, 4]
-- 3
count :: Eq a => a -> [a] -> Int
count elem = foldl
             (\count each -> if each == elem then count + 1 else count)
             0

first = head
second  values = values  !! 1
third   values = values  !! 2
fourth  values = values  !! 3
fifth   values = values  !! 4

type Scoring = [Int] -> Int

-- | Upper section Scoring
--
-- >>> scoreAces [2, 2, 3, 3, 4]
-- 0
-- >>> scoreAces [1, 1, 1, 3, 4]
-- 3
-- >>> scoreTwos [2, 2, 2, 5, 6]
-- 6
-- >>> scoreThrees [3, 3, 3, 3, 4]
-- 12
-- >>> scoreFours [4, 4, 5, 5, 5]
-- 8
-- >>> scoreFives [1, 1, 2, 2, 5]
-- 5
-- >>> scoreSixes [2, 3, 6, 6, 6]
-- 18

scoreUpperSection :: Int -> [Int] -> Int
scoreUpperSection category values = category * count category values

scoreAces  :: Scoring
scoreAces   = scoreUpperSection 1

scoreTwos  :: Scoring
scoreTwos   = scoreUpperSection 2

scoreThrees:: Scoring
scoreThrees = scoreUpperSection 3

scoreFours :: Scoring
scoreFours  = scoreUpperSection 4

scoreFives :: Scoring
scoreFives  = scoreUpperSection 5

scoreSixes :: Scoring
scoreSixes  = scoreUpperSection 6

-- Lower section


-- | Count three-of-a-kind score with provided dices
--
-- >>> scoreThreeOfAKind [2, 3, 4, 4, 5]
-- 0
-- >>> scoreThreeOfAKind [2, 2, 2, 4, 4]
-- 14
-- >>> scoreThreeOfAKind [2, 3, 4, 4, 4]
-- 17
-- >>> scoreThreeOfAKind [1, 4, 4, 4, 5]
-- 18
-- >>> scoreThreeOfAKind [2, 4, 4, 4, 4]
-- 18
scoreThreeOfAKind :: Scoring
scoreThreeOfAKind throw@[d1, d2, d3, d4, d5]
  | d1 == d3 || d2 == d4 || d3 == d5 = sum throw
  | otherwise                        = 0

-- | Count four-of-a-kind score with provided dices
--
-- >>> scoreFourOfAKind [2, 3, 4, 4, 5]
-- 0
-- >>> scoreFourOfAKind [2, 2, 2, 4, 4]
-- 0
-- >>> scoreFourOfAKind [4, 4, 4, 4, 5]
-- 21
-- >>> scoreFourOfAKind [4, 5, 5, 5, 5]
-- 24
-- >>> scoreFourOfAKind [5, 5, 5, 5, 5]
-- 25
scoreFourOfAKind :: Scoring
scoreFourOfAKind throw@[d1, d2, d3, d4, d5]
  | d1 == d4 || d2 == d5 = sum throw
  | otherwise            = 0
