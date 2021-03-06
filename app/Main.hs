module Main where

import Graphics.Gloss.Interface.IO.Game
import GHC.Float (int2Float)
import Control.Lens

import DataFunctions
import Loading
import Input
import Time2Update
import Render

-- exitWith
main :: IO ()
main = initialWorld >>= gameLoop >> print "Get Out!"

gameLoop :: World -> IO ()
gameLoop initialWorld = playIO (window initialWorld) background (fps initialWorld) initialWorld world2Picture event2Update time2Update


{-
showEvents =  play (InWindow "GameEvent" (700, 100) (10, 10))
              white
              100
              ""
              (\str     -> Translate (-340) 0 $ Scale 0.1 0.1 $ Text str)
              (\event _ -> show event)
              (\_ world -> world)
-}


window w = InWindow "COLLAPSE" (w ^. ui.windowSize) (300, 100)
background = white
fps w = w ^. ui.tps