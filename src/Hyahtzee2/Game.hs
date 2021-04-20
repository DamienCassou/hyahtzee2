{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE Safe #-}

-- |
-- Description : The `Game` type storing a `Round.Round` and a `ScoreCard.ScoreCard`
-- Copyright   : (c) Damien Cassou, 2021
-- License     : BSD-3-Clause
module Hyahtzee2.Game
  ( Game,
    newGame,
    scoreCard,
    round,
    toggleDie,
    rethrow,
    writeInLine,
    isFinished,
    canThrowDice,
  )
where

import qualified Hyahtzee2.Figure as Figure (Figure)
import qualified Hyahtzee2.Round as Round (Round, canThrowDice, newRound, renewRound, rethrow, toggleDie, values)
import qualified Hyahtzee2.Score as Score (score)
import qualified Hyahtzee2.ScoreCard as ScoreCard (ScoreCard, isFinished, newScoreCard, writeInLine)
import qualified System.Random as Random (StdGen)
import Prelude (Bool, Int, Maybe (Just, Nothing), Show, not, ($), (&&))

-- | A `Round.Round` and a `ScoreCard.ScoreCard`.
data Game = Game
  { -- | The game\'s current dice and iteration
    round :: Round.Round,
    -- | The mapping from lines to scores
    scoreCard :: ScoreCard.ScoreCard
  }
  deriving stock (Show)

-- | Create a new game with an empty score card and 5 dice already
-- thrown.
newGame :: Random.StdGen -> Game
newGame randomGen =
  Game {round = Round.newRound randomGen, scoreCard = ScoreCard.newScoreCard}

-- | Return a new game by copying the one passed as parameter and changing its round.
setRound :: Game -> Round.Round -> Game
setRound game round' = Game {round = round', scoreCard = scoreCard game}

-- | Return true iff the game is not finished yet and the user can
-- still throw the dice: i.e., the round\'s iteration is below 3.
canThrowDice :: Game -> Bool
canThrowDice game = gameIsNotFinished && roundCanThrowDice
  where
    gameIsNotFinished = not (isFinished game)
    roundCanThrowDice = Round.canThrowDice $ round game

-- | Toggle selection of a die at a particular index.
toggleDie :: Int -> Game -> Game
toggleDie index game = setRound game $ Round.toggleDie index $ round game

-- | Throw non-selected dice.
rethrow :: Game -> Maybe Game
rethrow game = case Round.rethrow (round game) of
  Just round' -> Just $ setRound game round'
  Nothing -> Nothing

-- | Return a game after writing a score for a `Figure.Figure` in the
-- game\'s `ScoreCard.ScoreCard`. A new round is created with a new
-- set of dice. If a score has already been written for this figure,
-- return `Nothing`.
writeInLine :: Figure.Figure -> Game -> Maybe Game
writeInLine figure game = case ScoreCard.writeInLine figure value (scoreCard game) of
  Just scoreCard' -> Just $ Game {round = Round.renewRound (round game), scoreCard = scoreCard'}
  Nothing -> Nothing
  where
    value = Score.score figure $ Round.values (round game)

-- | Return true iff the game\'s `ScoreCard.ScoreCard` is filled with scores:
-- i.e., no more score can be written.
isFinished :: Game -> Bool
isFinished game = ScoreCard.isFinished $ scoreCard game
