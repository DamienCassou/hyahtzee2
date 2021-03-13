{-# LANGUAGE Safe #-}

module Game (Game, newGame, scoreCard, Game.round, Game.selectDie, Game.selectDice, Game.unselectDie, Game.rethrow, Game.writeInBox, Game.isFinished, Game.setDice, Game.dice, Game.canThrowDice) where

import System.Random (StdGen)

import Round (Round, newRound, renewRound, selectDie, unselectDie, rethrow, values, setDice, dice, canThrowDice)
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

-- | Select one of the non-selected dice matching the given
-- value. Return `Nothing` if the value is not matching any
-- non-selected dice.
selectDie :: Int -> Game -> Maybe Game
selectDie value game = case Round.selectDie (Game.round game) value of
  Just round' -> Just $ setRound game round'
  Nothing -> Nothing

-- | Select non-selected dice matching the given
-- values. Return `Nothing` if one value is not matching any
-- non-selected dice.
selectDice :: [Int] -> Game -> Maybe Game
selectDice values' game = foldr (maybe Nothing . Game.selectDie) (Just game) values'

-- | Unselect one of the selected dice matching the given
-- value. Return `Nothing` if the value is not matching any
-- selected dice.
unselectDie :: Int -> Game -> Maybe Game
unselectDie value game = case Round.unselectDie (Game.round game) value of
  Just round' -> Just $ setRound game round'
  Nothing -> Nothing

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
