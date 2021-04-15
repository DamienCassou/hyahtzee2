{-# LANGUAGE Safe #-}

module Util
  ( modifyNth
  , generateRandomValues
  ) where

import System.Random (StdGen, Random (randomR))

-- $setup
-- >>> import Text.Printf (printf)
-- >>> import System.Random(mkStdGen)

-- | Return a new array with an element modified based on its index
-- and a modification function.
--
-- >>> modifyNth 1 (\value -> value + 1) [10,11,12]
-- [10,12,12]
modifyNth :: Int -> (a -> a) -> [a] -> [a]
modifyNth _ _ [] = []
modifyNth n f (x:xs)
  | n == 0 = f x:xs
  | otherwise = x:modifyNth (n-1) f xs

-- | Return X randomly generated dice.
--
-- >>> generateRandomValues 5 $ mkStdGen 0
-- ([5,1,4,6,6],732249858 652912057)
generateRandomValues :: Int -> StdGen -> ([Int], StdGen)
generateRandomValues number randomGen = foldl
  (\(dice, randomGen') _ -> let (die, randomGen'') = generateRandomValue randomGen' in
      (die:dice, randomGen''))
  ([], randomGen)
  [1..number]

-- | Return a randomly generated value for a die.
--
-- >>> generateRandomValue $ mkStdGen 0
-- (6,40014 40692)
-- >>> generateRandomValue $ snd (generateRandomValue $ mkStdGen 1)
-- (5,1054756829 1655838864)
generateRandomValue :: StdGen -> (Int, StdGen)
generateRandomValue = randomR (1, 6)
