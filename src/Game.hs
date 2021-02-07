{-# LANGUAGE Safe #-}

module Game (newGame, Game.selectDie, Game.unselectDie, Game.rethrow, Game.writeInBox, Game.isFinished) where

import System.Random (StdGen)

import Round (Round, newRound, selectDie, unselectDie, rethrow)
import ScoreCard (ScoreCard, newScoreCard, writeInBox, isFinished)
import Types (Figure)

data Game = Game { round :: Round, scoreCard :: ScoreCard }

newGame :: StdGen -> Game
newGame randomGen = Game { Game.round = newRound randomGen,
                           scoreCard = newScoreCard }

-- | Return a new game by copying the one passed as parameter and changing its round.
setRound :: Game -> Round -> Game
setRound game round' = Game { Game.round = round', scoreCard = scoreCard game}

-- | Return a new game by copying the one passed as parameter and changing its score card.
setScoreCard :: Game -> ScoreCard -> Game
setScoreCard game scoreCard' = Game { Game.round = Game.round game, scoreCard = scoreCard'}

-- | Select one of the non-selected dice matching the given
-- value. Return `Nothing` if the value is not matching any
-- non-selected dice.
selectDie :: Game -> Int -> Maybe Game
selectDie game value = case Round.selectDie (Game.round game) value of
  Just round' -> Just $ setRound game round'
  Nothing -> Nothing

-- | Unselect one of the selected dice matching the given
-- value. Return `Nothing` if the value is not matching any
-- selected dice.
unselectDie :: Game -> Int -> Maybe Game
unselectDie game value = case Round.unselectDie (Game.round game) value of
  Just round' -> Just $ setRound game round'
  Nothing -> Nothing

-- | Throw non-selected dice.
rethrow :: Game -> Maybe Game
rethrow game = case Round.rethrow (Game.round game) of
  Just round' -> Just $ setRound game round'
  Nothing -> Nothing

writeInBox :: Game -> Figure -> Int -> Maybe Game
writeInBox game figure value = case ScoreCard.writeInBox figure value (scoreCard game) of
  Just scoreCard' -> Just $ setScoreCard game scoreCard'
  Nothing -> Nothing

isFinished :: Game -> Bool
isFinished game = ScoreCard.isFinished $ scoreCard game
