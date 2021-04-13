import Test.DocTest as DocTest (doctest)

main :: IO ()
main = do
  doctest ["-isrc"
          , "src/Dice.hs"
          , "src/Game.hs"
          , "src/Round.hs"
          , "src/Score.hs"
          , "src/ScoreCard.hs"
          , "src/Types.hs"
          , "src/Util.hs"
          ]
