{-# LANGUAGE Safe #-}
{-# LANGUAGE TupleSections #-}

module Dice
  ( Dice
  , throwDice
  , rethrow
  , values
  , toggleDie
  , unselectAll
  ) where

import qualified Text.Printf as Printf (printf)
import qualified System.Random as Random (StdGen)
import qualified Data.Bifunctor as Bifunctor (second)
import qualified Data.List as List (intercalate)

import qualified Util (modifyNth, generateRandomValues)


-- $setup
-- >>> import System.Random(mkStdGen)

-- | An array of 5 dice. Each die is a pair of a value and a selection
-- state. The value is a number between 1 and 6. The selection state
-- is a boolean, with False, the default, indicating not selected.
newtype Dice = Dice [(Int, Bool)]

-- | Display dice.
--
-- >>> Dice [(6,True), (1,False), (6,True), (6,True), (2,False)]
-- [[6], 1, [6], [6], 2]
-- >>> Dice [(1,False), (2,False), (6,False), (6,False), (6,False)]
-- [1, 2, 6, 6, 6]
-- >>> Dice [(1,True), (2,True), (3,True), (4,True), (5,True)]
-- [[1], [2], [3], [4], [5]]
-- >>> Dice []
-- []
instance Show Dice where
  show (Dice dice) =
    "["
    ++ List.intercalate ", " (map (\(value, selected) -> Printf.printf (if selected then "[%d]" else "%d") value :: String) dice)
    ++ "]"

-- | Create an initial sequence of dice
--
-- >>> throwDice $ mkStdGen 0
-- ([5, 1, 4, 6, 6],732249858 652912057)
throwDice :: Random.StdGen -> (Dice, Random.StdGen)
throwDice randomGen =
  let (values', randomGen') = Util.generateRandomValues 5 randomGen in
    (Dice $ map (, False) values', randomGen')

-- | Throw non-selected dice.
--
-- >>> rethrow (Dice [(5, True), (5, True), (5, True), (1, False), (2, False)]) (mkStdGen 0)
-- ([[5], [5], [5], 6, 6],1601120196 1655838864)
rethrow :: Dice -> Random.StdGen -> (Dice, Random.StdGen)
rethrow (Dice dice) randomGen =
  let amountToThrow = length (filter (not . snd) dice)
      (values', randomGen') = Util.generateRandomValues amountToThrow randomGen
      keptDice = filter snd dice
      newDice = map (,False) values'
  in
    (Dice $ keptDice ++ newDice, randomGen')

-- TODO: use a State monad
-- (https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State#Pseudo-Random_Numbers)

-- | Select or unselect the die at given index.
--
-- >>> toggleDie 0 $ Dice [(3,True),(5,False)]
-- [3, 5]
-- >>> toggleDie 1 $ Dice [(3,True),(5,False)]
-- [[3], [5]]
toggleDie :: Int -> Dice -> Dice
toggleDie index (Dice values')= Dice $ Util.modifyNth index (Bifunctor.second not) values'

-- | Unselect all dice.
--
-- >>> unselectAll $ Dice [(3,True),(5,False),(6,False)]
-- [3, 5, 6]
unselectAll :: Dice -> Dice
unselectAll (Dice dice) = Dice $ map (Bifunctor.second (const False)) dice

-- | Return a sorted list of the value of all dice regardless of selection.
--
-- >>> values $ Dice [(1,True),(5,False),(6,True)]
-- [1,5,6]
values :: Dice -> [Int]
values (Dice dice) = map fst dice
