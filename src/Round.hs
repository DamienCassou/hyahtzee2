{-# LANGUAGE Safe #-}

module Round (Round, newRound, showIteration, showDice, Round.toggleDie, Round.rethrow, Round.values,renewRound, setDice, Round.dice, canThrowDice) where

import System.Random (StdGen)
import Text.Printf (printf)

import Dice (Dice, rethrow, toggleDie, values, throwDice, unselectAll)

-- $setup
-- >>> import System.Random (mkStdGen)

data Round = Round {
  -- The number of times the user threw the dice (from 1 to 3)
  iteration :: Int
  -- The 5 current dice
  , dice :: Dice
  -- A number generator
  , randomGen :: StdGen
  }

maxIteration :: Int
maxIteration = 3

-- | Distlay a round
--
-- >>> Round { iteration = 2, dice = fst (throwDice (mkStdGen 0)), randomGen = mkStdGen 0 }
-- [5, 1, 4, 6, 6] (throw 2/3)
instance Show Round where
  show round' = printf "%s %s" (showDice round') (showIteration round')

showDice :: Round -> String
showDice round' = show (dice round')

showIteration :: Round -> String
showIteration round' = printf "(throw %d/%d)" (iteration round') maxIteration

setDice :: Round -> Dice -> Round
setDice round' dice' = Round { iteration = iteration round', dice = dice', randomGen = randomGen round'}

-- | Return true iff the round is not at its 3rd iteration yet
--
-- >>> canThrowDice $ Round { iteration = 1, dice = fst $ throwDice $ mkStdGen 0, randomGen = mkStdGen 0 }
-- True
-- >>> canThrowDice $ Round { iteration = 2, dice = fst $ throwDice $ mkStdGen 0, randomGen = mkStdGen 0 }
-- True
-- >>> canThrowDice $ Round { iteration = 3, dice = fst $ throwDice $ mkStdGen 0, randomGen = mkStdGen 0 }
-- False
canThrowDice :: Round -> Bool
canThrowDice round' = iteration round' < maxIteration

-- | Return true iff the round is not at its 3rd iteration yet
--
-- >>> isPenultimateRound $ Round { iteration = 1, dice = fst $ throwDice $ mkStdGen 0, randomGen = mkStdGen 0 }
-- False
-- >>> isPenultimateRound $ Round { iteration = 2, dice = fst $ throwDice $ mkStdGen 0, randomGen = mkStdGen 0 }
-- True
-- >>> isPenultimateRound $ Round { iteration = 3, dice = fst $ throwDice $ mkStdGen 0, randomGen = mkStdGen 0 }
-- False
isPenultimateRound :: Round -> Bool
isPenultimateRound round' = iteration round' == maxIteration - 1

-- | Create a new instance of Round, iteration at 1 and random dice
--
-- >>> newRound $ mkStdGen 0
-- [5, 1, 4, 6, 6] (throw 1/3)
newRound :: StdGen -> Round
newRound randomGen' =
  let (dice', randomGen'') = throwDice randomGen' in
    Round {iteration = 1, dice = dice', randomGen = randomGen''}

renewRound :: Round -> Round
renewRound round' = newRound (randomGen round')

-- | Throw all dice but selected ones. Returns `Nothing` if round is
-- at iteration 3 already. After throwing dice, if it is not possible
-- to throw dice again, unselect all dice.
--
-- >>> Round.rethrow (Round {iteration = 1, dice = Dice.toggleDie 1 $ fst $ throwDice $ mkStdGen 0, randomGen = mkStdGen 0})
-- Just [[1], 1, 4, 6, 6] (throw 2/3)
-- >>> Round.rethrow (Round {iteration = 2, dice = Dice.toggleDie 1 $ fst $ throwDice $ mkStdGen 0, randomGen = mkStdGen 0})
-- Just [1, 1, 4, 6, 6] (throw 3/3)
-- >>> Round.rethrow (Round {iteration = 3, dice = Dice.toggleDie 1 $ fst $ throwDice $ mkStdGen 0, randomGen = mkStdGen 0})
-- Nothing
rethrow :: Round -> Maybe Round
rethrow round'
  | not (canThrowDice round') = Nothing
  | otherwise =
    let
      (dice', randomGen') = Dice.rethrow (dice round') (randomGen round')
      penultimate = isPenultimateRound round'
    in
      Just $ Round {
      iteration = iteration round' + 1,
      dice = if penultimate then unselectAll dice' else dice',
      randomGen = randomGen'}

-- | Select or unselect the die at given index.
--
-- >>> Round.toggleDie 1 (newRound $ mkStdGen 0)
-- [5, [1], 4, 6, 6] (throw 1/3)
-- >>> Round.toggleDie 3 (newRound $ mkStdGen 0)
-- [5, 1, 4, [6], 6] (throw 1/3)
toggleDie :: Int -> Round -> Round
toggleDie index round' = setDice round' $ Dice.toggleDie index (dice round')

values :: Round -> [Int]
values round' = Dice.values (dice round')
