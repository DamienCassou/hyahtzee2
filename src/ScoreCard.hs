{-# LANGUAGE Safe #-}

module ScoreCard (newScoreCard,
                  ScoreCard,
                  upperScoreCard, lowerScoreCard,
                  upperFigures, lowerFigures,
                  scoreBonus,
                  scoreUpperSection, scoreLowerSection,
                  writeInBox) where

import Data.Map (partitionWithKey, elems, insert, Map, empty)

import Types (Figure (UFigure, LFigure),
              UpperFigure, LowerFigure)
import Data.Maybe (catMaybes)

-- $setup
-- >>> import Data.Map (fromList)
-- >>> import Types (UpperFigure(Aces,Twos,Threes,Fours,Fives,Sixes), LowerFigure(ThreeOfAKind))

type Box = Maybe Int
type ScoreCard = Map Figure Box

newScoreCard :: ScoreCard
newScoreCard = empty

-- | Return 2 half-score cards, with the upper section first.
--
-- >>> partitionScoreCard $ fromList [(UFigure Aces, Just 3), (LFigure ThreeOfAKind, Nothing)]
-- (fromList [(UFigure Aces,Just 3)],fromList [(LFigure ThreeOfAKind,Nothing)])
partitionScoreCard :: ScoreCard -> (ScoreCard, ScoreCard)
partitionScoreCard = partitionWithKey (\figure _ ->
                                          case figure of
                                            UFigure _ -> True
                                            LFigure _ -> False)

-- | Return the upper part of a score card.
--
-- >>> upperScoreCard $ fromList [(UFigure Aces, Just 3), (LFigure ThreeOfAKind, Nothing)]
-- fromList [(UFigure Aces,Just 3)]
upperScoreCard :: ScoreCard -> ScoreCard
upperScoreCard scoreCard = fst $ partitionScoreCard scoreCard

-- | Return the lower part of a score card.
--
-- >>> lowerScoreCard $ fromList [(UFigure Aces, Just 3), (LFigure ThreeOfAKind, Nothing)]
-- fromList [(LFigure ThreeOfAKind,Nothing)]
lowerScoreCard :: ScoreCard -> ScoreCard
lowerScoreCard scoreCard = snd $ partitionScoreCard scoreCard

-- | Return the content of all boxes of a score card
--
-- >>> boxes $ fromList [(UFigure Aces, Just 3), (LFigure ThreeOfAKind, Nothing)]
-- [Just 3,Nothing]
boxes :: ScoreCard -> [Box]
boxes = elems

-- | Return the sum of all numbers in the boxes of a score card.
--
-- >>> sumBoxes $ fromList [(UFigure Aces, Just 3), (LFigure ThreeOfAKind, Nothing)]
-- 3
sumBoxes :: ScoreCard -> Int
sumBoxes scoreCard = sum $ catMaybes $ boxes scoreCard

-- | Return the bonus associated with the upper section.
--
-- >>> scoreBonus $ fromList [(UFigure Aces, Just 3)]
-- 0
-- >>> scoreBonus $ fromList [(UFigure Aces, Just 3), (UFigure Twos, Just 6), (UFigure Threes, Just 9), (UFigure Fours, Just 12), (UFigure Fives, Just 15), (UFigure Sixes, Just 18)]
-- 35
-- >>> scoreBonus $ fromList [(UFigure Fives, Just 30), (UFigure Sixes, Just 36)]
-- 35
scoreBonus :: ScoreCard -> Int
scoreBonus scoreCard = if sumBoxes (upperScoreCard scoreCard) >= 63 then 35 else 0

-- | Return the total of the upper section.
--
-- >>> scoreUpperSection $ fromList [(UFigure Aces, Just 3)]
-- 3
-- >>> scoreUpperSection $ fromList [(UFigure Fives, Just 30), (UFigure Sixes, Just 36)]
-- 101
scoreUpperSection :: ScoreCard -> Int
scoreUpperSection scoreCard = sumUpperBoxes + bonus
  where sumUpperBoxes = sumBoxes $ upperScoreCard scoreCard
        bonus = scoreBonus scoreCard

-- | Return the total of the lower section.
--
-- >>> scoreLowerSection $ fromList [(UFigure Aces, Just 3), (LFigure ThreeOfAKind, Just 17)]
-- 17
scoreLowerSection :: ScoreCard -> Int
scoreLowerSection scoreCard = sumBoxes $ lowerScoreCard scoreCard

-- | Return a score card after adding a new score in a box.
--
-- >>> writeInBox (UFigure Aces) (Just 3) $ fromList [(LFigure ThreeOfAKind, Just 17)]
-- fromList [(UFigure Aces,Just 3),(LFigure ThreeOfAKind,Just 17)]
writeInBox :: Figure -> Box -> ScoreCard -> ScoreCard
writeInBox = insert

-- | List all upper figures
--
-- >>> upperFigures
-- [Aces,Twos,Threes,Fours,Fives,Sixes]
upperFigures :: [UpperFigure]
upperFigures = [minBound .. maxBound] :: [UpperFigure]

-- | List all lower figures
--
-- >>> lowerFigures
-- [ThreeOfAKind,FourOfAKind,SmallStraight,LargeStraight,Yahtzee,Chance]
lowerFigures :: [LowerFigure]
lowerFigures = [minBound .. maxBound] :: [LowerFigure]
