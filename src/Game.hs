{-# LANGUAGE Safe #-}

module Game
  ( Game
  , newGame
  , scoreCard
  , round
  , toggleDie
  , rethrow
  , writeInBox
  , isFinished
  , setDice
  , dice
  , canThrowDice
  ) where

import Prelude hiding (round)
import qualified System.Random as Random (StdGen)

import qualified Round (Round, newRound, renewRound, toggleDie, rethrow, values, setDice, dice, canThrowDice)
import qualified ScoreCard (ScoreCard, newScoreCard, writeInBox, isFinished)
import qualified Types (Figure)
import qualified Score (score)
import qualified Dice (Dice)

data Game = Game { round :: Round.Round, scoreCard :: ScoreCard.ScoreCard }
  deriving Show

newGame :: Random.StdGen -> Game
newGame randomGen = Game { round = Round.newRound randomGen,
                           scoreCard = ScoreCard.newScoreCard }

-- | Return a new game by copying the one passed as parameter and changing its round.
setRound :: Game -> Round.Round -> Game
setRound game round' = Game { round = round', scoreCard = scoreCard game}

-- | Return a new game by copying the one passed as parameter and changing its dice.
setDice :: Game -> Dice.Dice -> Game
setDice game dice' = Game { round = Round.setDice (round game) dice', scoreCard = scoreCard game}

dice :: Game -> Dice.Dice
dice game = Round.dice $ round game

canThrowDice :: Game -> Bool
canThrowDice game = Round.canThrowDice $ round game

-- | Toggle selection of a die at a particular index.
toggleDie :: Int -> Game -> Game
toggleDie index game = setRound game $ Round.toggleDie index $ round game

-- | Throw non-selected dice.
rethrow :: Game -> Maybe Game
rethrow game = case Round.rethrow (round game) of
  Just round' -> Just $ setRound game round'
  Nothing -> Nothing

writeInBox :: Types.Figure -> Game -> Maybe Game
writeInBox figure game = case ScoreCard.writeInBox figure value (scoreCard game) of
  Just scoreCard' -> Just $ Game { round = Round.renewRound (round game), scoreCard = scoreCard' }
  Nothing -> Nothing
  where value = Score.score figure $ Round.values (round game)

isFinished :: Game -> Bool
isFinished game = ScoreCard.isFinished $ scoreCard game
