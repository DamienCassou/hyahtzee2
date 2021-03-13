module Main where

import System.Random (StdGen, mkStdGen)
import Game (newGame, selectDie, selectDice, rethrow, writeInBox)
import Types (Figure(LFigure, UFigure), LowerFigure(ThreeOfAKind), UpperFigure (Twos))

main :: IO ()
main =
  let game0 = newGame $ mkStdGen 1
      game1 = selectDice [2] game0
      game2 = rethrow =<< game1
      game3 = selectDice [2, 2] =<< game2
      game4 = rethrow =<< game3
      game5 = writeInBox (UFigure Twos) =<< game4
  in print game5
