module Game where


import Round (Round, newRound)
import ScoreCard (ScoreCard, newScoreCard)
import System.Random (StdGen)

data Game = Game { round :: Round, scoreCard :: ScoreCard }

newGame :: StdGen -> Game
newGame randomGen = Game { Game.round = newRound randomGen,
                           scoreCard = newScoreCard }
