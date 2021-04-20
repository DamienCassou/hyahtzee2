{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Safe #-}

-- |
-- Description : The `Figure`, `UpperFigure` and `LowerFigure` types
--               representing the 13 categories a user scores to
-- Copyright   : (c) Damien Cassou, 2021
-- License     : BSD-3-Clause
module Hyahtzee2.Figure
  ( UpperFigure (Aces, Twos, Threes, Fours, Fives, Sixes),
    LowerFigure (ThreeOfAKind, FourOfAKind, FullHouse, SmallStraight, LargeStraight, Yahtzee, Chance),
    Figure (UFigure, LFigure),
  )
where

-- | The categories of the [score card's upper section](https://en.wikipedia.org/wiki/Yahtzee#Upper_section).
data UpperFigure = Aces | Twos | Threes | Fours | Fives | Sixes
  deriving stock (Eq, Show, Ord, Enum, Bounded)

-- | The categories of the [score card's lower section](https://en.wikipedia.org/wiki/Yahtzee#Lower_section).
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

-- | Represents all categories (both from the upper and lower sections).
data Figure = UFigure UpperFigure | LFigure LowerFigure
  deriving stock (Eq, Ord)

instance Show Figure where
  show (UFigure figure) = show figure
  show (LFigure figure) = show figure
