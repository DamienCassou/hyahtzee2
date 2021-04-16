module Core (GameUI, newGameUI, game, throwDice, toggleDie, tryThrowingDice) where

import qualified Hyahtzee2.Game as Game (Game, toggleDie, rethrow)

newtype GameUI = GameUI { game :: Game.Game}

newGameUI :: Game.Game -> Core.GameUI
newGameUI game' = GameUI { game = game' }

-- | Select or unselect the die at given index.
toggleDie :: Int -> Core.GameUI -> Core.GameUI
toggleDie index gameUI =
  let game' = Game.toggleDie index $ game gameUI
  in GameUI {game = game'}


-- | Shuffle all unselected dice.
throwDice :: Core.GameUI -> Core.GameUI
throwDice gameUI =
  let
    game' = game gameUI
    maybeGame = Game.rethrow game'
  in
    maybe gameUI GameUI maybeGame

-- | Throw dice if it is still possible. Otherwise do nothing.
tryThrowingDice :: Core.GameUI -> Core.GameUI
tryThrowingDice gameUI =
  let maybeGame' = Game.rethrow $ game gameUI
  in case maybeGame' of
    Nothing -> gameUI
    Just game' -> GameUI {game = game'}
