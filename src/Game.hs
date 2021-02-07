module Game where

import Round (Round)
import ScoreCard (ScoreCard)

data Game = Game { round :: Round, scoreCard :: ScoreCard }
