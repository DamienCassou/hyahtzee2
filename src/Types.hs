module Types where

import Data.Map

type Throw = [Int]
type Scoring = Throw -> Int

data UpperFigure = Aces | Twos | Threes | Fours | Fives | Sixes
  deriving (Eq, Show, Ord)

data LowerFigure = ThreeOfAKind | FourOfAKind | SmallStraight | LargeStraight | Yahtzee | Chance
  deriving (Eq, Show, Ord)

data Figure = UFigure UpperFigure | LFigure LowerFigure
  deriving (Eq, Show, Ord)

type Box = Maybe Int
type ScoreCard = Map Figure Box
