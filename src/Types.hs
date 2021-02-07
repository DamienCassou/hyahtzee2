module Types where

type Throw = [Int]
type Scoring = Throw -> Int

data UpperFigure = Aces | Twos | Threes | Fours | Fives | Sixes
  deriving (Eq, Show, Ord, Enum, Bounded)

data LowerFigure = ThreeOfAKind | FourOfAKind | SmallStraight | LargeStraight | Yahtzee | Chance
  deriving (Eq, Show, Ord, Enum, Bounded)

data Figure = UFigure UpperFigure | LFigure LowerFigure
  deriving (Eq, Show, Ord)

type Box = Maybe Int
