module Main where

import qualified Brick (
  App, App(..),
  attrMap,
  appDraw, appHandleEvent, appStartEvent, appAttrMap, appChooseCursor,
  defaultMain,
  neverShowCursor)

import qualified Graphics.Vty as Vty (defAttr)

import qualified System.Random as Random (mkStdGen)
import qualified Control.Monad (void)

import qualified Core (GameUI, newGameUI)
import qualified Draw (drawUI, Name)
import qualified Events (appEvent)

import qualified Hyahtzee2.Game as Game (newGame)

app :: Brick.App Core.GameUI e Draw.Name
app =
    Brick.App { Brick.appDraw = Draw.drawUI
              , Brick.appHandleEvent = Events.appEvent
              , Brick.appStartEvent = return
              , Brick.appAttrMap = const $ Brick.attrMap Vty.defAttr []
              , Brick.appChooseCursor = Brick.neverShowCursor
              }

main :: IO ()
main = Control.Monad.void $ Brick.defaultMain app $ Core.newGameUI $ Game.newGame $ Random.mkStdGen 1
