{-# LANGUAGE Unsafe #-}

module Main (main) where

import qualified Brick
  ( App (..),
    appAttrMap,
    appChooseCursor,
    appDraw,
    appHandleEvent,
    appStartEvent,
    attrMap,
    defaultMain,
    neverShowCursor,
  )
import qualified Control.Monad (void)
import qualified Core (GameUI, newGameUI)
import qualified Draw (Name, drawUI)
import qualified Events (appEvent)
import qualified Graphics.Vty as Vty (defAttr)
import qualified Hyahtzee2.Game as Game (newGame)
import qualified System.Random as Random (getStdGen)

app :: Brick.App Core.GameUI e Draw.Name
app =
  Brick.App
    { Brick.appDraw = Draw.drawUI,
      Brick.appHandleEvent = Events.appEvent,
      Brick.appStartEvent = return,
      Brick.appAttrMap = const $ Brick.attrMap Vty.defAttr [],
      Brick.appChooseCursor = Brick.neverShowCursor
    }

main :: IO ()
main = Random.getStdGen >>= (Control.Monad.void . Brick.defaultMain app . Core.newGameUI . Game.newGame)
