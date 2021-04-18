{-# LANGUAGE Safe #-}
{-# LANGUAGE TupleSections #-}

module Hyahtzee2.Dice
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
import qualified Data.List as List (intercalate, sort)

import qualified Hyahtzee2.Util as Util (modifyNth, generateRandomValues)

-- | An array of 5 dice. Each die is a pair of a value and a selection
-- state. The value is a number between 1 and 6. The selection state
-- is a boolean, with False, the default, indicating not selected.
newtype Dice = Dice [(Int, Bool)]

-- | Display dice.
instance Show Dice where
  show (Dice dice) =
    "["
    ++ List.intercalate ", " (map (\(value, selected) -> Printf.printf (if selected then "[%d]" else "%d") value :: String) dice)
    ++ "]"

-- | Create an initial sequence of dice
throwDice :: Random.StdGen -> (Dice, Random.StdGen)
throwDice randomGen =
  let (values', randomGen') = Util.generateRandomValues 5 randomGen in
    (Dice $ map (, False) values', randomGen')

-- | Throw non-selected dice.
rethrow :: Dice -> Random.StdGen -> (Dice, Random.StdGen)
rethrow (Dice dice) randomGen =
  let amountToThrow = length (filter (not . snd) dice)
      (values', randomGen') = Util.generateRandomValues amountToThrow randomGen
      keptDice = filter snd dice
      newDice = map (,False) values'
  in
    (Dice $ keptDice ++ newDice, randomGen')

-- | Select or unselect the die at given index.
toggleDie :: Int -> Dice -> Dice
toggleDie index (Dice values')= Dice $ Util.modifyNth index (Bifunctor.second not) values'

-- | Unselect all dice.
unselectAll :: Dice -> Dice
unselectAll (Dice dice) = Dice $ map (Bifunctor.second (const False)) dice

-- | Return a sorted list of the value of all dice regardless of selection.
values :: Dice -> [Int]
values (Dice dice) = List.sort $ map fst dice
