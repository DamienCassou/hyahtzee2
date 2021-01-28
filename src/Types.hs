module Types where

data Dice = One | Two | Three | Four | Five | Six
  deriving (Eq, Show, Ord)

type Throw = [Int]

data Score =
  NoScore
  -- Upper section
  | Aces Int | Twos Int | Threes Int | Fours Int | Fives Int | Sixes Int
  -- Lower section
  | ThreeOfAKind Int | FourOfAKind Int | FullHouse | SmallStraight | LargeStraight | Yahtzee | Chance Int
  deriving (Eq, Show)

type Scoring = [Int] -> Score
