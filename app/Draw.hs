{-# LANGUAGE Unsafe #-}

module Draw (drawUI, Name) where

import qualified Brick (Padding (Pad), Widget)
import qualified Brick.Widgets.Border as BrickB (borderWithLabel)
import qualified Brick.Widgets.Border.Style as BrickBS (unicodeBold)
import qualified Brick.Widgets.Center as BrickCe (hCenter)
import qualified Brick.Widgets.Core as BrickC (hBox, hLimit, padAll, padTop, str, vBox, withBorderStyle)
import qualified Brick.Widgets.Table as BrickT (renderTable, table)
import qualified Core (GameUI, allFigureLetters, game, letterForFigure)
import qualified Hyahtzee2.Game as Game (canThrowDice, isFinished, round, scoreCard)
import qualified Hyahtzee2.Round as Round (Round, showDice, showIteration)
import qualified Hyahtzee2.ScoreCard as ScoreCard (ScoreCard, ScoreCardLine (FigureLine), allLines, valueAtLine)
import qualified Text.Printf as Printf (printf)
import Prelude (String, head, last, map, maybe, not, show, ($), (++))

type Name = ()

drawRemainingThrows :: Round.Round -> Brick.Widget Name
drawRemainingThrows round = BrickC.str $ Round.showIteration round

drawRound :: Round.Round -> Brick.Widget Name
drawRound round =
  BrickC.hLimit 30 $
    BrickC.withBorderStyle BrickBS.unicodeBold $
      BrickB.borderWithLabel (drawRemainingThrows round) $
        BrickCe.hCenter $
          BrickC.padAll 1 $
            BrickC.str (Round.showDice round)

lineValue :: ScoreCard.ScoreCard -> ScoreCard.ScoreCardLine -> String
lineValue scoreCard line = maybe "  -  " show (ScoreCard.valueAtLine scoreCard line)

drawScoreCardLineName :: ScoreCard.ScoreCardLine -> Brick.Widget Name
drawScoreCardLineName (ScoreCard.FigureLine figure) = BrickC.str $ [Core.letterForFigure figure] ++ ". " ++ show figure
drawScoreCardLineName line = BrickC.str $ "   " ++ show line

drawScoreCardLine :: ScoreCard.ScoreCard -> ScoreCard.ScoreCardLine -> [Brick.Widget Name]
drawScoreCardLine scoreCard line = [drawScoreCardLineName line, drawScoreCardLineValue]
  where
    drawScoreCardLineValue = BrickC.str $ lineValue scoreCard line :: Brick.Widget Name

drawScoreCard :: ScoreCard.ScoreCard -> Brick.Widget Name
drawScoreCard scoreCard = BrickT.renderTable $ BrickT.table $ map (drawScoreCardLine scoreCard) ScoreCard.allLines

drawGame :: Core.GameUI -> Brick.Widget Name
drawGame gameUI =
  BrickC.hBox
    [ drawScoreCard (Game.scoreCard $ Core.game gameUI),
      BrickC.vBox [drawRound (Game.round $ Core.game gameUI), drawHelp gameUI]
    ]

drawHelp :: Core.GameUI -> Brick.Widget Name
drawHelp gameUI =
  let whenCanThrow =
        if Game.canThrowDice (Core.game gameUI)
          then "- Press `t` to throw dice again\n" ++ "- Press `1`-`5` to toogle die selection\n"
          else ""
      whenCanWrite =
        if not $ Game.isFinished (Core.game gameUI)
          then
            Printf.printf
              "- Press `%c`-`%c` to write score in the score board\n"
              (head Core.allFigureLetters)
              (last Core.allFigureLetters)
          else ""
      body = BrickC.str $ whenCanThrow ++ whenCanWrite ++ "- Press `q` to quit\n" :: Brick.Widget Name
   in BrickC.padTop (Brick.Pad 1) $ BrickB.borderWithLabel (BrickC.str "Help") body

drawUI :: Core.GameUI -> [Brick.Widget Name]
drawUI game = [drawGame game]
