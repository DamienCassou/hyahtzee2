{-# LANGUAGE Safe #-}

module Game (newGame) where

import System.Random (StdGen)

import Round (Round, newRound)
import ScoreCard (ScoreCard, newScoreCard)

data Game = Game { round :: Round, scoreCard :: ScoreCard }

newGame :: StdGen -> Game
newGame randomGen = Game { Game.round = newRound randomGen,
                           scoreCard = newScoreCard }
