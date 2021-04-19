{-# LANGUAGE Safe #-}
{-# LANGUAGE DerivingStrategies #-}

module Hyahtzee2.Figure
  ( UpperFigure(Aces, Twos, Threes, Fours, Fives, Sixes)
  , LowerFigure(ThreeOfAKind, FourOfAKind, FullHouse, SmallStraight, LargeStraight, Yahtzee, Chance)
  , Figure(UFigure, LFigure)
  ) where

data UpperFigure = Aces | Twos | Threes | Fours | Fives | Sixes
  deriving stock (Eq, Show, Ord, Enum, Bounded)

data LowerFigure = ThreeOfAKind | FourOfAKind | FullHouse | SmallStraight | LargeStraight | Yahtzee | Chance
  deriving stock (Eq, Ord, Enum, Bounded)

instance Show LowerFigure where
  show ThreeOfAKind = "Three of a Kind"
  show FourOfAKind = "Four of a Kind"
  show FullHouse = "Full House"
  show SmallStraight = "Small Straight"
  show LargeStraight = "Large Straight"
  show Yahtzee = "Yahtzee"
  show Chance = "Chance"

data Figure = UFigure UpperFigure | LFigure LowerFigure
  deriving stock (Eq, Ord)

instance Show Figure where
  show (UFigure figure) = show figure
  show (LFigure figure) = show figure
