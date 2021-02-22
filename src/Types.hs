{-# LANGUAGE Safe #-}
{-# LANGUAGE DerivingStrategies #-}

-- TODO : rename this file to Figure.hs

module Types (UpperFigure(Aces, Twos, Threes, Fours, Fives, Sixes)
             , LowerFigure(ThreeOfAKind, FourOfAKind, FullHouse, SmallStraight, LargeStraight, Yahtzee, Chance)
             , Figure(UFigure, LFigure)) where

data UpperFigure = Aces | Twos | Threes | Fours | Fives | Sixes
  deriving stock (Eq, Show, Ord, Enum, Bounded)

data LowerFigure = ThreeOfAKind | FourOfAKind | FullHouse | SmallStraight | LargeStraight | Yahtzee | Chance
  deriving stock (Eq, Show, Ord, Enum, Bounded)

data Figure = UFigure UpperFigure | LFigure LowerFigure
  deriving stock (Eq, Show, Ord)
