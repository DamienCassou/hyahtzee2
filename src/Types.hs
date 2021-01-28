module Types where

type Throw = [Int]

data Score =
  NoScore
  -- Upper section
  | Aces Int | Twos Int | Threes Int | Fours Int | Fives Int | Sixes Int
  -- Lower section
  | ThreeOfAKind Int | FourOfAKind Int | FullHouse | SmallStraight | LargeStraight | Yahtzee | Chance Int
  deriving (Eq, Show)

type Scoring = [Int] -> Score
