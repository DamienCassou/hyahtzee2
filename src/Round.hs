module Round where

import Types (Throw)
import System.Random (StdGen, mkStdGen, randomR)
import Text.Printf (printf)
import Dice (Dice, rethrow, throwDice)

data Round = Round {
  -- The number of times the user threw the dice (from 1 to 3)
  iteration :: Int
  -- The 5 current dice
  , dice :: Dice
  -- A number generator
  , randomGen :: StdGen
  }

-- | Distlay a round
--
-- >>> Round { iteration = 2, dice = fst (throwDice (mkStdGen 0)), randomGen = mkStdGen 0 }
-- [5, 1, 4, 6, 6] (throw 2/3)
instance Show Round where
  show round = printf "%s (throw %d/3)" (show (dice round)) (iteration round)

-- | Return true iff the round is not at its 3rd iteration yet
--
canThrowDice :: Round -> Bool
canThrowDice round = iteration round < 3

-- | Create a new instance of Round, iteration at 1 and random dice
--
-- >>> newRound $ mkStdGen 0
-- [5, 1, 4, 6, 6] (throw 1/3)
newRound :: StdGen -> Round
newRound randomGen =
  let (dice, randomGen') = throwDice randomGen in
    Round {iteration = 1, dice = dice, randomGen = randomGen'}

-- | Throw all dice but selected ones. `selection` must be a valid
-- subset of `(throw round)`.n
--
-- >>> Round.rethrow $ newRound $ mkStdGen 0
-- [1, 2, 2, 4, 2] (throw 2/3)
rethrow :: Round -> Round
rethrow round =
  let (dice', randomGen') = Dice.rethrow (dice round) (randomGen round) in
  Round { iteration = iteration round + 1, dice = dice', randomGen = randomGen'}
