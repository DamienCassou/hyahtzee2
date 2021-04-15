module Draw (drawUI, Name) where

import qualified Brick (Widget, Padding(Pad))
import qualified Brick.Widgets.Core as BrickC (hLimit, str, withBorderStyle, padTop, vBox, hBox, padAll)
import qualified Brick.Widgets.Border as BrickB (borderWithLabel)
import qualified Brick.Widgets.Border.Style as BrickBS (unicodeBold)
import qualified Brick.Widgets.Center as BrickCe (hCenter)
import qualified Brick.Widgets.Table as BrickT (renderTable, table)

import qualified Types (Figure)
import qualified ScoreCard (ScoreCard, allFigures, value)
import qualified Game (Game, scoreCard, round, canThrowDice)
import qualified Round (Round, showIteration, showDice)

import qualified Core (GameUI, game)

type Name = ()

drawRemainingThrows :: Round.Round -> Brick.Widget Name
drawRemainingThrows round = BrickC.str $ Round.showIteration round

drawRound :: Round.Round -> Brick.Widget Name
drawRound round = BrickC.hLimit 30
  $ BrickC.withBorderStyle BrickBS.unicodeBold
  $ BrickB.borderWithLabel (drawRemainingThrows round)
  $ BrickCe.hCenter
  $ BrickC.padAll 1
  $ BrickC.str (Round.showDice round)

figureValue :: ScoreCard.ScoreCard -> Types.Figure -> String
figureValue scoreCard figure = maybe "  -  " show (ScoreCard.value scoreCard figure)

drawFigure :: ScoreCard.ScoreCard -> Types.Figure -> [Brick.Widget Name]
drawFigure scoreCard figure = [
  -- Figure name:
  BrickC.str $ show figure
  -- Score:
  , BrickC.str $ figureValue scoreCard figure
  ]

drawScoreCard :: ScoreCard.ScoreCard -> Brick.Widget Name
drawScoreCard scoreCard = BrickT.renderTable $ BrickT.table $ map (drawFigure scoreCard) ScoreCard.allFigures

drawGame :: Core.GameUI -> Brick.Widget Name
drawGame gameUI = BrickC.hBox [
  drawScoreCard (Game.scoreCard $ Core.game gameUI)
  , BrickC.vBox [drawRound (Game.round $ Core.game gameUI), drawHelp gameUI]
  ]

drawHelp :: Core.GameUI -> Brick.Widget Name
drawHelp gameUI =
  let
    whenCanThrow = if Game.canThrowDice (Core.game gameUI)
      then "- Press `t` to throw dice again\n" ++ "- Press `1`-`6` to toogle die selection\n"
      else ""
    body = BrickC.str $ whenCanThrow ++ "- Press 'q' to quit\n"
  in BrickC.padTop (Brick.Pad 1) $ BrickB.borderWithLabel (BrickC.str "Help") body


drawUI :: Core.GameUI -> [Brick.Widget Name]
drawUI game = [drawGame game]
