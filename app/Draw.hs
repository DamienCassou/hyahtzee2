module Draw (drawUI, Name) where

import Prelude hiding (round)

import qualified Brick (Widget, Padding(Pad))
import qualified Brick.Widgets.Core as BrickC (hLimit, str, withBorderStyle, padTop, vBox, hBox, padAll)
import qualified Brick.Widgets.Border as BrickB (borderWithLabel)
import qualified Brick.Widgets.Border.Style as BrickBS (unicodeBold)
import qualified Brick.Widgets.Center as BrickCe (hCenter)
import qualified Brick.Widgets.Table as BrickT (renderTable, table)

import qualified Hyahtzee2.ScoreCard as ScoreCard (ScoreCard, ScoreCardLine(FigureLine), ScoreCardLine, valueAtLine, allLines)
import qualified Hyahtzee2.Game as Game (scoreCard, round, canThrowDice)
import qualified Hyahtzee2.Round as Round (Round, showIteration, showDice)

import qualified Core (GameUI, game, letterForFigure)

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

lineValue :: ScoreCard.ScoreCard -> ScoreCard.ScoreCardLine -> String
lineValue scoreCard line = maybe "  -  " show (ScoreCard.valueAtLine scoreCard line)

drawScoreCardLineName :: ScoreCard.ScoreCardLine -> Brick.Widget Name
drawScoreCardLineName (ScoreCard.FigureLine figure) = BrickC.str $ [Core.letterForFigure figure] ++ ". " ++ show figure
drawScoreCardLineName line = BrickC.str $ "   " ++ show line

drawScoreCardLine :: ScoreCard.ScoreCard -> ScoreCard.ScoreCardLine -> [Brick.Widget Name]
drawScoreCardLine scoreCard line = [drawScoreCardLineName line, drawScoreCardLineValue]
  where
    drawScoreCardLineValue = BrickC.str $ lineValue scoreCard line

drawScoreCard :: ScoreCard.ScoreCard -> Brick.Widget Name
drawScoreCard scoreCard = BrickT.renderTable $ BrickT.table $ map (drawScoreCardLine scoreCard) ScoreCard.allLines

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
