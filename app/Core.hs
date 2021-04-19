{-# LANGUAGE Safe #-}

module Core
  ( GameUI
  , newGameUI
  , game
  , toggleDie
  , tryThrowingDice
  , writeInLine
  , figureForLetter
  , letterForFigure
  , allFigureLetters
  ) where

import qualified Data.List as List (find, sort, zip)

import qualified Hyahtzee2.Game as Game (Game, toggleDie, rethrow, writeInLine)

import qualified Hyahtzee2.Figure as Figure
  ( Figure ( UFigure, LFigure )
  , UpperFigure(Aces)
  , LowerFigure
  )

newtype GameUI = GameUI { game :: Game.Game}

newGameUI :: Game.Game -> GameUI
newGameUI game' = GameUI { game = game' }

-- | Select or unselect the die at given index.
toggleDie :: Int -> GameUI -> GameUI
toggleDie index gameUI =
  let game' = Game.toggleDie index $ game gameUI
  in GameUI {game = game'}

-- | Throw dice if it is still possible. Otherwise do nothing.
tryThrowingDice :: GameUI -> GameUI
tryThrowingDice gameUI =
  let maybeGame' = Game.rethrow $ game gameUI
  in case maybeGame' of
    Nothing -> gameUI
    Just game' -> GameUI {game = game'}

writeInLine :: Figure.Figure -> GameUI -> GameUI
writeInLine figure gameUI =
  let
    game' = game gameUI
    maybeGame = Game.writeInLine figure game'
  in
    maybe gameUI GameUI maybeGame

figuresAndLetters :: [(Figure.Figure, Char)]
figuresAndLetters = List.zip allFigures ['a'..'z']
  where allFigures = upperFigures ++ lowerFigures
        upperFigures = map Figure.UFigure ([minBound .. maxBound] :: [Figure.UpperFigure])
        lowerFigures = map Figure.LFigure ([minBound .. maxBound] :: [Figure.LowerFigure])

figureForLetter :: Char -> Figure.Figure
figureForLetter letter =
  let maybePair = List.find (\(_,char) -> char == letter) figuresAndLetters
  in maybe (Figure.UFigure Figure.Aces) fst maybePair

letterForFigure :: Figure.Figure -> Char
letterForFigure figure =
  let maybePair = List.find (\(figure',_) -> figure == figure') figuresAndLetters
  in maybe 'a' snd maybePair

allFigureLetters :: [Char]
allFigureLetters = List.sort $ map snd figuresAndLetters
