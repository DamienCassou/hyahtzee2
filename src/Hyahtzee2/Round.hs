{-# LANGUAGE Safe #-}

-- |
-- Description : The `Round` type storing a `Dice.Dice` object, the current
--               iteration (from 1 to 3) and a random number generator
-- Copyright   : (c) Damien Cassou, 2021
-- License     : BSD-3-Clause
module Hyahtzee2.Round
  ( Round,
    newRound,
    showIteration,
    showDice,
    toggleDie,
    rethrow,
    values,
    renewRound,
    canThrowDice,
  )
where

import qualified Hyahtzee2.Dice as Dice (Dice, rethrow, throwDice, toggleDie, unselectAll, values)
import qualified System.Random as Random (StdGen)
import qualified Text.Printf as Printf (printf)

-- | A round stores the dice and the iteration number (i.e., the user
-- is allowed to throw the dice 3 times maximum).
data Round = Round
  { -- | The number of times the user threw the dice (from 1 to 3)
    iteration :: Int,
    -- | The 5 current dice
    dice :: Dice.Dice,
    -- | A number generator
    randomGen :: Random.StdGen
  }

maxIteration :: Int
maxIteration = 3

instance Show Round where
  show round' = Printf.printf "%s %s" (showDice round') (showIteration round')

-- | Return a string representation of the round's dice.
showDice :: Round -> String
showDice round' = show (dice round')

-- | Return a string representation of the round's iteration with the
-- current iteration and the maximum.
showIteration :: Round -> String
showIteration round' = Printf.printf "(throw %d/%d)" (iteration round') maxIteration

setDice :: Round -> Dice.Dice -> Round
setDice round' dice' = Round {iteration = iteration round', dice = dice', randomGen = randomGen round'}

-- | Return true iff the round is not at its 3rd iteration yet
canThrowDice :: Round -> Bool
canThrowDice round' = iteration round' < maxIteration

-- | Return true iff the round is not at its 3rd iteration yet
isPenultimateRound :: Round -> Bool
isPenultimateRound round' = iteration round' == maxIteration - 1

-- | Create a new instance of `Round`, iteration at 1 and random dice
newRound :: Random.StdGen -> Round
newRound randomGen' =
  let (dice', randomGen'') = Dice.throwDice randomGen'
   in Round {iteration = 1, dice = dice', randomGen = randomGen''}

-- | Create a new round by reusing the random number generator from
-- the round passed as argument.
renewRound :: Round -> Round
renewRound round' = newRound (randomGen round')

-- | Throw all dice but selected ones. Returns `Nothing` if round is
-- at iteration 3 already. After throwing the dice, if it is not
-- possible to throw dice again, unselect all dice.
rethrow :: Round -> Maybe Round
rethrow round'
  | not (canThrowDice round') = Nothing
  | otherwise =
    let (dice', randomGen') = Dice.rethrow (dice round') (randomGen round')
        penultimate = isPenultimateRound round'
     in Just $
          Round
            { iteration = iteration round' + 1,
              dice = if penultimate then Dice.unselectAll dice' else dice',
              randomGen = randomGen'
            }

-- | Select or unselect the die at given index.
toggleDie :: Int -> Round -> Round
toggleDie index round' = setDice round' $ Dice.toggleDie index (dice round')

-- | Return a sorted list of the value of every die regardless of
-- selection.
values :: Round -> [Int]
values round' = Dice.values (dice round')
