{-# LANGUAGE Safe #-}

module Hyahtzee2.ScoreCard
  ( newScoreCard
  , ScoreCard
  , ScoreCardLine(FigureLine, UpperBonusLine, TotalLine)
  , allLines
  , valueAtLine
  , isFinished
  , writeInLine
  ) where

import qualified Data.List as List (all)
import qualified Data.Map as Map (Map, empty, lookup, insert, (!?), size, member, elems)
import qualified Data.Maybe as Maybe (fromMaybe)

import qualified Hyahtzee2.Types as Types
  ( Figure
  , UpperFigure
  , LowerFigure
  , Figure ( UFigure, LFigure )
  )

data ScoreCardLine
  = FigureLine Types.Figure
  | UpperBonusLine
  | TotalLine
  deriving (Ord, Eq)

instance Show ScoreCardLine where
  show (FigureLine figure) = show figure
  show UpperBonusLine = "Bonus"
  show TotalLine = "Total"

upperFigures :: [Types.UpperFigure]
upperFigures = [minBound .. maxBound]

lowerFigures :: [Types.LowerFigure]
lowerFigures = [minBound .. maxBound]

upperFigureLines :: [ScoreCardLine]
upperFigureLines = map (FigureLine . Types.UFigure) upperFigures

lowerFigureLines :: [ScoreCardLine]
lowerFigureLines = map (FigureLine . Types.LFigure) lowerFigures

allLines :: [ ScoreCardLine ]
allLines = upperFigureLines ++ [ UpperBonusLine ] ++ lowerFigureLines ++ [ TotalLine ]

numberOfLines :: Int
numberOfLines = length allLines

type ScoreCard = Map.Map ScoreCardLine Int

-- | Return an empty score card
newScoreCard :: ScoreCard
newScoreCard = Map.empty

-- | Return a score card after writing a score for a figure. If a
-- score has already been written for this figure, return Nothing.
writeInLine :: Types.Figure -> Int -> ScoreCard -> Maybe ScoreCard
writeInLine figure value scoreCard =
  let line = FigureLine figure
  in case Map.lookup line scoreCard of
       Just _ -> Nothing -- error, there is already a number
       Nothing -> Just $ autocompleteLines $ Map.insert line value scoreCard

autocompleteLines :: ScoreCard -> ScoreCard
autocompleteLines scoreCard = foldl
                              (\result function -> function result)
                              scoreCard
                              autocompleteFunctions

autocompleteFunctions :: [ScoreCard -> ScoreCard]
autocompleteFunctions = [autocompleteBonusLine, autocompleteTotalLine]

autocompleteBonusLine :: ScoreCard -> ScoreCard
autocompleteBonusLine scoreCard = if canFillBonusLine
                                  then scoreCardWithBonusLine
                                  else scoreCard
  where
    canFillBonusLine = bonusLineIsEmpty && allUpperFiguresHaveScore
    bonusLineIsEmpty = not $ hasValueAtLine scoreCard UpperBonusLine
    allUpperFiguresHaveScore = List.all (hasValueAtLine scoreCard) upperFigureLines
    scoreCardWithBonusLine = Map.insert UpperBonusLine bonusValue scoreCard
    bonusValue = if reachesThreshold then 35 else 0
    reachesThreshold = sum upperFigureValues >= 63
    upperFigureValues = map valueAtLineOr0 upperFigureLines
    valueAtLineOr0 line = Maybe.fromMaybe 0 (valueAtLine scoreCard line)

autocompleteTotalLine :: ScoreCard -> ScoreCard
autocompleteTotalLine scoreCard = if canFillTotalLine
                                  then scoreCardWithTotalLine
                                  else scoreCard
  where
    canFillTotalLine = Map.size scoreCard == (numberOfLines - 1)
    scoreCardWithTotalLine = Map.insert TotalLine totalValue scoreCard
    totalValue = sum $ Map.elems scoreCard

hasValueAtLine :: ScoreCard -> ScoreCardLine -> Bool
hasValueAtLine scoreCard line = Map.member line scoreCard

-- | Return the value associated with a line or Nothing.
valueAtLine :: ScoreCard -> ScoreCardLine -> Maybe Int
valueAtLine scoreCard line = scoreCard Map.!? line

-- | Return true iff all figures have been written to in the score card.
isFinished :: ScoreCard -> Bool
isFinished scoreCard = Map.size scoreCard == numberOfLines
