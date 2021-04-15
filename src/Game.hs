{-# LANGUAGE Safe #-}

module Game
  ( Game
  , newGame
  , scoreCard
  , Game.round
  , Game.toggleDie
  , Game.rethrow
  , Game.writeInBox
  , Game.isFinished
  , Game.setDice
  , Game.dice
  , Game.canThrowDice
  ) where

import System.Random (StdGen)

import Round (Round, newRound, renewRound, toggleDie, rethrow, values, setDice, dice, canThrowDice)
import ScoreCard (ScoreCard, newScoreCard, writeInBox, isFinished)
import Types (Figure)
import Score (score)
import Dice (Dice)

data Game = Game { round :: Round, scoreCard :: ScoreCard }
  deriving Show

newGame :: StdGen -> Game
newGame randomGen = Game { Game.round = newRound randomGen,
                           scoreCard = newScoreCard }

-- | Return a new game by copying the one passed as parameter and changing its round.
setRound :: Game -> Round -> Game
setRound game round' = Game { Game.round = round', scoreCard = scoreCard game}

-- | Return a new game by copying the one passed as parameter and changing its dice.
setDice :: Game -> Dice -> Game
setDice game dice' = Game { Game.round = Round.setDice (Game.round game) dice', scoreCard = scoreCard game}

dice :: Game -> Dice
dice game = Round.dice $ Game.round game

canThrowDice :: Game -> Bool
canThrowDice game = Round.canThrowDice $ Game.round game

-- | Return a new game by copying the one passed as parameter and changing its score card.
-- setScoreCard :: Game -> ScoreCard -> Game
-- setScoreCard game scoreCard' = Game { Game.round = Game.round game, scoreCard = scoreCard'}

-- | Toggle selection of a die at a particular index.
toggleDie :: Int -> Game -> Game
toggleDie index game = setRound game $ Round.toggleDie index $ Game.round game

-- | Throw non-selected dice.
rethrow :: Game -> Maybe Game
rethrow game = case Round.rethrow (Game.round game) of
  Just round' -> Just $ setRound game round'
  Nothing -> Nothing

writeInBox :: Figure -> Game -> Maybe Game
writeInBox figure game = case ScoreCard.writeInBox figure value (scoreCard game) of
  Just scoreCard' -> Just $ Game { Game.round = renewRound (Game.round game), Game.scoreCard = scoreCard' }
  Nothing -> Nothing
  where value = score figure $ values (Game.round game)

isFinished :: Game -> Bool
isFinished game = ScoreCard.isFinished $ scoreCard game
