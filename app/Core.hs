module Core
  ( GameUI
  , newGameUI
  , game
  , throwDice
  , toggleDie
  , tryThrowingDice
  , writeInBox
  , figureForLetter
  , letterForFigure
  , allFigureLetters
  ) where

import qualified Data.List as List (find, sort, zip)

import qualified Hyahtzee2.Game as Game (Game, toggleDie, rethrow, writeInBox)

import qualified Hyahtzee2.Types as Types
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

-- | Shuffle all unselected dice.
throwDice :: GameUI -> GameUI
throwDice gameUI =
  let
    game' = game gameUI
    maybeGame = Game.rethrow game'
  in
    maybe gameUI GameUI maybeGame

-- | Throw dice if it is still possible. Otherwise do nothing.
tryThrowingDice :: GameUI -> GameUI
tryThrowingDice gameUI =
  let maybeGame' = Game.rethrow $ game gameUI
  in case maybeGame' of
    Nothing -> gameUI
    Just game' -> GameUI {game = game'}

writeInBox :: Types.Figure -> GameUI -> GameUI
writeInBox figure gameUI =
  let
    game' = game gameUI
    maybeGame = Game.writeInBox figure game'
  in
    maybe gameUI GameUI maybeGame

figuresAndLetters :: [(Types.Figure, Char)]
figuresAndLetters = List.zip allFigures ['a'..'z']
  where allFigures = upperFigures ++ lowerFigures
        upperFigures = map Types.UFigure ([minBound .. maxBound] :: [Types.UpperFigure])
        lowerFigures = map Types.LFigure ([minBound .. maxBound] :: [Types.LowerFigure])

figureForLetter :: Char -> Types.Figure
figureForLetter letter =
  let maybePair = List.find (\(_,char) -> char == letter) figuresAndLetters
  in maybe (Types.UFigure Types.Aces) fst maybePair

letterForFigure :: Types.Figure -> Char
letterForFigure figure =
  let maybePair = List.find (\(figure',_) -> figure == figure') figuresAndLetters
  in maybe 'a' snd maybePair

allFigureLetters :: [Char]
allFigureLetters = List.sort $ map snd figuresAndLetters
