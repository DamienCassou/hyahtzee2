{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Safe #-}

-- |
-- Description : The `ScoreCard` type mapping a `ScoreCardLine` to the user's score for this line
-- Copyright   : (c) Damien Cassou, 2021
-- License     : BSD-3-Clause
module Hyahtzee2.ScoreCard
  ( newScoreCard,
    ScoreCard,
    ScoreCardLine (FigureLine, UpperBonusLine, TotalLine),
    allLines,
    valueAtLine,
    isFinished,
    writeInLine,
  )
where

import qualified Data.List as List (all)
import qualified Data.Map as Map (Map, elems, empty, insert, lookup, member, size, (!?))
import qualified Data.Maybe as Maybe (fromMaybe)
import qualified Hyahtzee2.Figure as Figure
  ( Figure (LFigure, UFigure),
    LowerFigure,
    UpperFigure,
  )

-- | Represents a line in the score card. The score for this line is
-- stored in a `ScoreCard` object. The line is either filled manually
-- by the user from its dice (the `FigureLine` constructor) or filled
-- automatically by the game when the lines above are filled (the
-- `UpperBonusLine` and `TotalLine` constructors).
data ScoreCardLine
  = FigureLine Figure.Figure
  | UpperBonusLine
  | TotalLine
  deriving stock (Ord, Eq)

instance Show ScoreCardLine where
  show (FigureLine figure) = show figure
  show UpperBonusLine = "Bonus"
  show TotalLine = "Total"

upperFigures :: [Figure.UpperFigure]
upperFigures = [minBound .. maxBound]

lowerFigures :: [Figure.LowerFigure]
lowerFigures = [minBound .. maxBound]

upperFigureLines :: [ScoreCardLine]
upperFigureLines = map (FigureLine . Figure.UFigure) upperFigures

lowerFigureLines :: [ScoreCardLine]
lowerFigureLines = map (FigureLine . Figure.LFigure) lowerFigures

-- | A top-down list of all lines a score card contains. This includes
-- the lines a user hasn't scored yet.
allLines :: [ScoreCardLine]
allLines = upperFigureLines ++ [UpperBonusLine] ++ lowerFigureLines ++ [TotalLine]

numberOfLines :: Int
numberOfLines = length allLines

-- | Stores the user's score on a given line.
type ScoreCard = Map.Map ScoreCardLine Int

-- | Return an empty `ScoreCard`.
newScoreCard :: ScoreCard
newScoreCard = Map.empty

-- | Return a score card after writing a score for a figure. If a
-- score has already been written for this figure, return Nothing.
writeInLine :: Figure.Figure -> Int -> ScoreCard -> Maybe ScoreCard
writeInLine figure value scoreCard =
  let line = FigureLine figure
   in case Map.lookup line scoreCard of
        Just _ -> Nothing -- error, there is already a number
        Nothing -> Just $ autoFillLines $ Map.insert line value scoreCard

autoFillLines :: ScoreCard -> ScoreCard
autoFillLines scoreCard =
  foldl
    (\result function -> function result)
    scoreCard
    autoFillFunctions

autoFillFunctions :: [ScoreCard -> ScoreCard]
autoFillFunctions =
  [ makeAutoFillFunction canBonusLineBeFilled UpperBonusLine computeBonusLineValue,
    makeAutoFillFunction canTotalLineBeFilled TotalLine computeTotalLineValue
  ]

makeAutoFillFunction :: (ScoreCard -> Bool) -> ScoreCardLine -> (ScoreCard -> Int) -> ScoreCard -> ScoreCard
makeAutoFillFunction canLineBeFilled scoreCardLine computeValue scoreCard =
  if canLineBeFilled scoreCard
    then Map.insert scoreCardLine (computeValue scoreCard) scoreCard
    else scoreCard

canBonusLineBeFilled :: ScoreCard -> Bool
canBonusLineBeFilled scoreCard = bonusLineIsEmpty && allUpperFiguresHaveScore
  where
    bonusLineIsEmpty = not $ hasValueAtLine scoreCard UpperBonusLine
    allUpperFiguresHaveScore = List.all (hasValueAtLine scoreCard) upperFigureLines

computeBonusLineValue :: ScoreCard -> Int
computeBonusLineValue scoreCard = if reachesThreshold then 35 else 0
  where
    reachesThreshold = sum upperFigureValues >= 63
    upperFigureValues = map valueAtLineOr0 upperFigureLines
    valueAtLineOr0 line = Maybe.fromMaybe 0 (valueAtLine scoreCard line)

canTotalLineBeFilled :: ScoreCard -> Bool
canTotalLineBeFilled scoreCard = Map.size scoreCard == (numberOfLines - 1)

computeTotalLineValue :: ScoreCard -> Int
computeTotalLineValue scoreCard = sum $ Map.elems scoreCard

hasValueAtLine :: ScoreCard -> ScoreCardLine -> Bool
hasValueAtLine scoreCard line = Map.member line scoreCard

-- | Return the value associated with a line or Nothing.
valueAtLine :: ScoreCard -> ScoreCardLine -> Maybe Int
valueAtLine scoreCard line = scoreCard Map.!? line

-- | Return true iff all figures have been written to in the score card.
isFinished :: ScoreCard -> Bool
isFinished scoreCard = Map.size scoreCard == numberOfLines
