{-# LANGUAGE Safe #-}

module Hyahtzee2.ScoreCard
  ( newScoreCard
  , ScoreCard
  , value
  , allFigures
  , upperFigures
  , lowerFigures
  , upperScoreCard
  , lowerScoreCard
  , scoreBonus
  , isFinished
  , scoreUpperSection
  , scoreLowerSection
  , writeInBox
  ) where

import qualified Data.Map as Map (partitionWithKey, elems, insert, Map, empty, lookup, member, (!?))

import qualified Hyahtzee2.Types as Types
  ( Figure (UFigure, LFigure)
  , UpperFigure
  , LowerFigure
  )

-- $setup
-- >>> import Data.Map (fromList)
-- >>> import Types (UpperFigure(Aces,Twos,Threes,Fours,Fives,Sixes), LowerFigure(ThreeOfAKind))

type ScoreCard = Map.Map Types.Figure Int

newScoreCard :: ScoreCard
newScoreCard = Map.empty

-- | Return 2 half-score cards, with the upper section first.
--
-- >>> partitionScoreCard $ fromList [(UFigure Aces, 3), (LFigure ThreeOfAKind, 5)]
-- (fromList [(Aces,3)],fromList [(ThreeOfAKind,5)])
partitionScoreCard :: ScoreCard -> (ScoreCard, ScoreCard)
partitionScoreCard = Map.partitionWithKey (\figure _ ->
                                             case figure of
                                               Types.UFigure _ -> True
                                               Types.LFigure _ -> False)

-- | Return the upper part of a score card.
--
-- >>> upperScoreCard $ fromList [(UFigure Aces, 3), (LFigure ThreeOfAKind, 5)]
-- fromList [(Aces,3)]
upperScoreCard :: ScoreCard -> ScoreCard
upperScoreCard scoreCard = fst $ partitionScoreCard scoreCard

-- | Return the lower part of a score card.
--
-- >>> lowerScoreCard $ fromList [(UFigure Aces, 3), (LFigure ThreeOfAKind, 5)]
-- fromList [(ThreeOfAKind,5)]
lowerScoreCard :: ScoreCard -> ScoreCard
lowerScoreCard scoreCard = snd $ partitionScoreCard scoreCard

-- | Return the content of all boxes of a score card
--
-- >>> boxes $ fromList [(UFigure Aces, 3), (LFigure ThreeOfAKind, 5)]
-- [3,5]
boxes :: ScoreCard -> [Int]
boxes = Map.elems

-- | Return the sum of all numbers in the boxes of a score card.
--
-- >>> sumBoxes $ fromList [(UFigure Aces, 3), (LFigure ThreeOfAKind, 5)]
-- 8
sumBoxes :: ScoreCard -> Int
sumBoxes scoreCard = sum $ boxes scoreCard

-- | Return the bonus associated with the upper section.
--
-- >>> scoreBonus $ fromList [(UFigure Aces, 3)]
-- 0
-- >>> scoreBonus $ fromList [(UFigure Aces, 3), (UFigure Twos, 6), (UFigure Threes, 9), (UFigure Fours, 12), (UFigure Fives, 15), (UFigure Sixes, 18)]
-- 35
-- >>> scoreBonus $ fromList [(UFigure Fives, 30), (UFigure Sixes, 36)]
-- 35
scoreBonus :: ScoreCard -> Int
scoreBonus scoreCard = if sumBoxes (upperScoreCard scoreCard) >= 63 then 35 else 0

-- | Return the total of the upper section.
--
-- >>> scoreUpperSection $ fromList [(UFigure Aces, 3)]
-- 3
-- >>> scoreUpperSection $ fromList [(UFigure Fives, 30), (UFigure Sixes, 36)]
-- 101
scoreUpperSection :: ScoreCard -> Int
scoreUpperSection scoreCard = sumUpperBoxes + bonus
  where sumUpperBoxes = sumBoxes $ upperScoreCard scoreCard
        bonus = scoreBonus scoreCard

-- | Return the total of the lower section.
--
-- >>> scoreLowerSection $ fromList [(UFigure Aces, 3), (LFigure ThreeOfAKind, 17)]
-- 17
scoreLowerSection :: ScoreCard -> Int
scoreLowerSection scoreCard = sumBoxes $ lowerScoreCard scoreCard

-- | Return a score card after adding a new score in a box.
--
-- >>> writeInBox (UFigure Aces) 3 $ fromList [(LFigure ThreeOfAKind, 17)]
-- Just (fromList [(Aces,3),(ThreeOfAKind,17)])
-- >>> writeInBox (UFigure Aces) 3 $ fromList [(UFigure Aces, 17)]
-- Nothing
writeInBox :: Types.Figure -> Int -> ScoreCard -> Maybe ScoreCard
writeInBox figure value' scoreCard = case Map.lookup figure scoreCard of
  Just _ -> Nothing
  Nothing -> Just $ Map.insert figure value' scoreCard

-- | List all upper figures
--
-- >>> upperFigures
-- [Aces,Twos,Threes,Fours,Fives,Sixes]
upperFigures :: [Types.UpperFigure]
upperFigures = [minBound .. maxBound] :: [Types.UpperFigure]

-- | List all lower figures
--
-- >>> lowerFigures
-- [ThreeOfAKind,FourOfAKind,FullHouse,SmallStraight,LargeStraight,Yahtzee,Chance]
lowerFigures :: [Types.LowerFigure]
lowerFigures = [minBound .. maxBound] :: [Types.LowerFigure]

-- | List all upper and lower figures.
allFigures :: [Types.Figure]
allFigures = map Types.UFigure upperFigures ++ map Types.LFigure lowerFigures

-- | Return true iff the score card contains a value for the figure.
--
-- >>> hasValue (fromList [(UFigure Aces, 3)]) (UFigure Aces)
-- True
-- >>> hasValue (fromList [(UFigure Aces, 3)]) (UFigure Twos)
-- False
hasValue :: ScoreCard -> Types.Figure -> Bool
hasValue scoreCard figure = Map.member figure scoreCard

-- | Return the value associated with a figure or Nothing.
--
-- >>> value (fromList [(UFigure Aces, 3)]) (UFigure Aces)
-- Just 3
-- >>> value (fromList [(UFigure Aces, 3)]) (UFigure Twos)
-- Nothing
value :: ScoreCard -> Types.Figure -> Maybe Int
value scoreCard figure = scoreCard Map.!? figure

-- | Return true iff all figures have been written to in the score card.
--
isFinished :: ScoreCard -> Bool
isFinished scoreCard = all (hasValue scoreCard) allFigures
