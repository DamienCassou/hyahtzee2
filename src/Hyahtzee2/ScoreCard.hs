{-# LANGUAGE Safe #-}

module Hyahtzee2.ScoreCard
  ( newScoreCard
  , ScoreCard
  , ScoreCardLine(FigureLine, UpperBonusLine, TotalLine)
  , allLines
  , valueAtLine
  , isFinished
  , writeInBox
  ) where

import qualified Data.Map as Map (Map, empty, lookup, insert, (!?), size)

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

allLines :: [ ScoreCardLine ]
allLines = upperFigureLines ++ [ UpperBonusLine ] ++ lowerFigureLines ++ [ TotalLine ]
  where upperFigureLines = map (FigureLine . Types.UFigure) upperFigures
        upperFigures = [minBound .. maxBound] :: [Types.UpperFigure]
        lowerFigureLines = map (FigureLine . Types.LFigure) lowerFigures
        lowerFigures = [minBound .. maxBound] :: [Types.LowerFigure]

numberOfLines :: Int
numberOfLines = length allLines

type ScoreCard = Map.Map ScoreCardLine Int

-- | Return an empty score card
newScoreCard :: ScoreCard
newScoreCard = Map.empty

-- | Return a score card after writing a score for a figure. If a
-- score has already been written for this figure, return Nothing.
writeInBox :: Types.Figure -> Int -> ScoreCard -> Maybe ScoreCard
writeInBox figure value scoreCard =
  let line = FigureLine figure
  in case Map.lookup line scoreCard of
       Just _ -> Nothing -- error, there is already a number
       Nothing -> Just $ Map.insert line value scoreCard

-- | Return the value associated with a line or Nothing.
valueAtLine :: ScoreCard -> ScoreCardLine -> Maybe Int
valueAtLine scoreCard line = scoreCard Map.!? line

-- | Return true iff all figures have been written to in the score card.
isFinished :: ScoreCard -> Bool
isFinished scoreCard = Map.size scoreCard == numberOfLines
