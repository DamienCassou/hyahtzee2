{-# LANGUAGE Unsafe #-}

module Events (appEvent) where

import qualified Brick (BrickEvent (VtyEvent), EventM, Next, continue, halt)
import qualified Core (GameUI, figureForLetter, toggleDie, tryThrowingDice, writeInLine)
import qualified Graphics.Vty as Vty (Event (EvKey), Key (KChar))

-- | Handle a received event (usually a keyboard event) by returning a
-- copy of the game received as argument.
appEvent :: Core.GameUI -> Brick.BrickEvent () e -> Brick.EventM () (Brick.Next Core.GameUI)
appEvent game (Brick.VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = Brick.halt game
appEvent game (Brick.VtyEvent (Vty.EvKey (Vty.KChar 't') [])) = Brick.continue $ Core.tryThrowingDice game
appEvent game (Brick.VtyEvent (Vty.EvKey (Vty.KChar '1') [])) = Brick.continue $ Core.toggleDie 0 game
appEvent game (Brick.VtyEvent (Vty.EvKey (Vty.KChar '2') [])) = Brick.continue $ Core.toggleDie 1 game
appEvent game (Brick.VtyEvent (Vty.EvKey (Vty.KChar '3') [])) = Brick.continue $ Core.toggleDie 2 game
appEvent game (Brick.VtyEvent (Vty.EvKey (Vty.KChar '4') [])) = Brick.continue $ Core.toggleDie 3 game
appEvent game (Brick.VtyEvent (Vty.EvKey (Vty.KChar '5') [])) = Brick.continue $ Core.toggleDie 4 game
appEvent game (Brick.VtyEvent (Vty.EvKey (Vty.KChar '6') [])) = Brick.continue $ Core.toggleDie 5 game
appEvent game (Brick.VtyEvent (Vty.EvKey (Vty.KChar letter) [])) = Brick.continue $ Core.writeInLine (Core.figureForLetter letter) game
appEvent game _ = Brick.continue game
