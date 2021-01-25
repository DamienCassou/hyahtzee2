module Lib where

import Data.List (elemIndex)

data Dice = One | Two | Three | Four | Five | Six
  deriving (Eq, Show, Ord)

faces = [One, Two, Three, Four, Five, Six]

-- | Return the numeric value of a Dice
--
-- >>> value One
-- 1
value :: Dice -> Int
value dice = case elemIndex dice faces of
  Just value -> value + 1
  Nothing -> -1 -- should not happen
