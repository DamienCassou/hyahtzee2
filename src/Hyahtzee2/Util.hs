{-# LANGUAGE Safe #-}

module Hyahtzee2.Util
  ( modifyNth,
    generateRandomValues,
  )
where

import qualified System.Random as Random (Random (randomR), StdGen)

-- | Return a new array with an element modified based on its index
-- and a modification function.
modifyNth :: Int -> (a -> a) -> [a] -> [a]
modifyNth _ _ [] = []
modifyNth n f (x : xs)
  | n == 0 = f x : xs
  | otherwise = x : modifyNth (n -1) f xs

-- | Return X randomly generated dice.
generateRandomValues :: Int -> Random.StdGen -> ([Int], Random.StdGen)
generateRandomValues number randomGen =
  foldl
    ( \(dice, randomGen') _ ->
        let (die, randomGen'') = generateRandomValue randomGen'
         in (die : dice, randomGen'')
    )
    ([], randomGen)
    [1 .. number]

-- | Return a randomly generated value for a die.
generateRandomValue :: Random.StdGen -> (Int, Random.StdGen)
generateRandomValue = Random.randomR (1, 6)
