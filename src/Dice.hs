module Dice where

import Types (Throw)
import Text.Printf (printf)
import System.Random (StdGen, Random (randomR), mkStdGen)

type Selection = [Int]

data Dice = Dice {
  selection :: [Int]
  , others :: [Int]
  }

-- | Display dice.
--
-- >>> Dice { selection = [6, 6, 6], others = [1, 2]}
-- [[6], [6], [6], 1, 2]
-- >>> Dice { selection = [], others = [1, 2, 6, 6, 6]}
-- [1, 2, 6, 6, 6]
-- >>> Dice { selection = [1, 2, 3, 4, 5], others = []}
-- [[1], [2], [3], [4], [5]]
-- >>> Dice { selection = [], others = []}
-- []
instance Show Dice where
  show dice = let selectedDices = showArrayElements (printf "[%d]") (selection dice)
                  others' = showArrayElements (printf "%d") (others dice)
              in case (length selectedDices, length others') of
                   (0, 0) -> "[]"
                   (0, x) -> "[" ++ others' ++ "]"
                   (x, 0) -> "[" ++ selectedDices ++ "]"
                   (x, y) -> "[" ++ selectedDices ++ ", " ++ others' ++ "]"


-- | Return a string representing the elements of a list, comma separated.
--
-- >>> showArrayElements (printf "[%d]") [4,5,6]
-- "[4], [5], [6]"
showArrayElements :: (a -> String) -> [a] -> String
showArrayElements f (a:b:xs) = f a ++ ", " ++ showArrayElements f (b:xs)
showArrayElements f [a] = f a
showArrayElements _ [] = ""

throwDice :: StdGen -> (Dice, StdGen)
throwDice randomGen =
  let (values, randomGen') = generateRandomValues 5 randomGen in
    (Dice { selection = [], others = values }, randomGen')

-- | Throw non-selected dice.
--
-- >>> rethrow (Dice {selection = [], others = [1,2,3,4,5]}) (mkStdGen 0)
-- ([5, 1, 4, 6, 6],732249858 652912057)
rethrow :: Dice -> StdGen -> (Dice, StdGen)
rethrow dice randomGen =
  let amountToThrow = 5 - length (selection dice)
      (values, randomGen') = generateRandomValues amountToThrow randomGen
  in
    (Dice {selection = selection dice, others = values}, randomGen')

-- TODO: use a State monad
-- (https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State#Pseudo-Random_Numbers)

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
