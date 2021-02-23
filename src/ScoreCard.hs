{-# LANGUAGE Safe #-}

module ScoreCard (newScoreCard,
                  ScoreCard,
                  value,
                  allFigures,
                  upperFigures, lowerFigures,
                  upperScoreCard, lowerScoreCard,
                  scoreBonus, isFinished,
                  scoreUpperSection, scoreLowerSection,
                  writeInBox) where

import Data.Map (partitionWithKey, elems, insert, Map, empty, lookup, member, (!?))

import Types (Figure (UFigure, LFigure),
              UpperFigure, LowerFigure)

-- $setup
-- >>> import Data.Map (fromList)
-- >>> import Types (UpperFigure(Aces,Twos,Threes,Fours,Fives,Sixes), LowerFigure(ThreeOfAKind))

type ScoreCard = Map Figure Int

newScoreCard :: ScoreCard
newScoreCard = empty

-- | Return 2 half-score cards, with the upper section first.
--
-- >>> partitionScoreCard $ fromList [(UFigure Aces, 3), (LFigure ThreeOfAKind, 5)]
-- (fromList [(UFigure Aces,3)],fromList [(LFigure ThreeOfAKind,5)])
partitionScoreCard :: ScoreCard -> (ScoreCard, ScoreCard)
partitionScoreCard = partitionWithKey (\figure _ ->
                                          case figure of
                                            UFigure _ -> True
                                            LFigure _ -> False)

-- | Return the upper part of a score card.
--
-- >>> upperScoreCard $ fromList [(UFigure Aces, 3), (LFigure ThreeOfAKind, 5)]
-- fromList [(UFigure Aces,3)]
upperScoreCard :: ScoreCard -> ScoreCard
upperScoreCard scoreCard = fst $ partitionScoreCard scoreCard

-- | Return the lower part of a score card.
--
-- >>> lowerScoreCard $ fromList [(UFigure Aces, 3), (LFigure ThreeOfAKind, 5)]
-- fromList [(LFigure ThreeOfAKind,5)]
lowerScoreCard :: ScoreCard -> ScoreCard
lowerScoreCard scoreCard = snd $ partitionScoreCard scoreCard

-- | Return the content of all boxes of a score card
--
-- >>> boxes $ fromList [(UFigure Aces, 3), (LFigure ThreeOfAKind, 5)]
-- [3,5]
boxes :: ScoreCard -> [Int]
boxes = elems

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
-- Just (fromList [(UFigure Aces,3),(LFigure ThreeOfAKind,17)])
-- >>> writeInBox (UFigure Aces) 3 $ fromList [(UFigure Aces, 17)]
-- Nothing
writeInBox :: Figure -> Int -> ScoreCard -> Maybe ScoreCard
writeInBox figure value' scoreCard = case Data.Map.lookup figure scoreCard of
  Just _ -> Nothing
  Nothing -> Just $ insert figure value' scoreCard

-- | List all upper figures
--
-- >>> upperFigures
-- [Aces,Twos,Threes,Fours,Fives,Sixes]
upperFigures :: [UpperFigure]
upperFigures = [minBound .. maxBound] :: [UpperFigure]

-- | List all lower figures
--
-- >>> lowerFigures
-- [ThreeOfAKind,FourOfAKind,FullHouse,SmallStraight,LargeStraight,Yahtzee,Chance]
lowerFigures :: [LowerFigure]
lowerFigures = [minBound .. maxBound] :: [LowerFigure]

-- | List all upper and lower figures.
allFigures :: [Figure]
allFigures = map UFigure upperFigures ++ map LFigure lowerFigures

-- | Return true iff the score card contains a value for the figure.
--
-- >>> hasValue (fromList [(UFigure Aces, 3)]) (UFigure Aces)
-- True
-- >>> hasValue (fromList [(UFigure Aces, 3)]) (UFigure Twos)
-- False
hasValue :: ScoreCard -> Figure -> Bool
hasValue scoreCard figure = member figure scoreCard

-- | Return the value associated with a figure or Nothing.
--
-- >>> value (fromList [(UFigure Aces, 3)]) (UFigure Aces)
-- Just 3
-- >>> value (fromList [(UFigure Aces, 3)]) (UFigure Twos)
-- Nothing
value :: ScoreCard -> Figure -> Maybe Int
value scoreCard figure = scoreCard !? figure

-- | Return true iff all figures have been written to in the score card.
--
isFinished :: ScoreCard -> Bool
isFinished scoreCard = all (hasValue scoreCard) allFigures
