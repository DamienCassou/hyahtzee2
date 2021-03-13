{-# LANGUAGE Safe #-}

module Round (Round, newRound, showIteration, showDice, Round.selectDie, Round.unselectDie, Round.rethrow, Round.values,renewRound) where

import System.Random (StdGen)
import Text.Printf (printf)

import Dice (Dice, rethrow, throwDice, selectDie, unselectDie, values)

-- $setup
-- >>> import System.Random (mkStdGen)
-- >>> import Dice (Dice(Dice, others, selection))

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
canThrowDice :: Round -> Bool
canThrowDice round' = iteration round' < maxIteration

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
-- at iteration 3 already.
--
-- >>> Round.rethrow $ newRound $ mkStdGen 0
-- Just [1, 2, 2, 4, 2] (throw 2/3)
-- >>> Round.rethrow (Round { iteration = 3, dice = Dice { others = [1, 2], selection = [3, 4, 5]}, randomGen = mkStdGen 0})
-- Nothing
rethrow :: Round -> Maybe Round
rethrow round'
  | not (canThrowDice round') = Nothing
  | otherwise =
    let (dice', randomGen') = Dice.rethrow (dice round') (randomGen round') in
      Just $ Round { iteration = iteration round' + 1, dice = dice', randomGen = randomGen'}

-- | Select one of the non-selected dice matching the given
-- value. Return `Nothing` if the value is not matching any
-- non-selected dice.
--
-- >>> Round.selectDie (newRound $ mkStdGen 0) 1
-- Just [[1], 5, 4, 6, 6] (throw 1/3)
-- >>> Round.selectDie (newRound $ mkStdGen 0) 3
-- Nothing
selectDie :: Round -> Int -> Maybe Round
selectDie round' value = case Dice.selectDie (dice round') value of
  Just dice' -> Just $ setDice round' dice'
  Nothing    -> Nothing

-- | Unselect one of the selected dice matching the given
-- value. Return `Nothing` if the value is not matching any
-- selected dice.
--
-- >>> Round.unselectDie (Round {iteration = 1, dice = Dice { others = [1, 2], selection = [3, 4, 5]}, randomGen = mkStdGen 0}) 3
-- Just [[4], [5], 3, 1, 2] (throw 1/3)
-- >>> Round.unselectDie (Round {iteration = 1, dice = Dice { others = [1, 2], selection = [3, 4, 5]}, randomGen = mkStdGen 0}) 1
-- Nothing
unselectDie :: Round -> Int -> Maybe Round
unselectDie round' value = case Dice.unselectDie (dice round') value of
  Just dice' -> Just $ setDice round' dice'
  Nothing    -> Nothing

values :: Round -> [Int]
values round' = Dice.values (dice round')
