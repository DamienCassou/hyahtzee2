{-# LANGUAGE Safe #-}
{-# LANGUAGE DerivingStrategies #-}

module Types (Throw
             , UpperFigure(Aces, Twos, Threes, Fours, Fives, Sixes)
             , LowerFigure(ThreeOfAKind, FourOfAKind, SmallStraight, LargeStraight, Yahtzee, Chance)
             , Figure(UFigure, LFigure)) where

type Throw = [Int]

data UpperFigure = Aces | Twos | Threes | Fours | Fives | Sixes
  deriving stock (Eq, Show, Ord, Enum, Bounded)

data LowerFigure = ThreeOfAKind | FourOfAKind | SmallStraight | LargeStraight | Yahtzee | Chance
  deriving stock (Eq, Show, Ord, Enum, Bounded)

data Figure = UFigure UpperFigure | LFigure LowerFigure
  deriving stock (Eq, Show, Ord)
