module Main where

import Graphics.Gloss.Juicy
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Picture
import GHC.Float (int2Float)
import Control.Lens
import Control.DeepSeq

import GameData
import Loading
import Render
import Input
import Time2Update (time2Update)
import RunTimeData (fps)

-- exitWith
main :: IO ()
main = initialWorld >>= gameLoop >> print "Get Out!"

gameLoop :: World -> IO ()
gameLoop initialWorld = playIO window background fps initialWorld world2Picture event2Update time2Update


{-
showEvents =  play (InWindow "GameEvent" (700, 100) (10, 10))
              white
              100
              ""
              (\str     -> Translate (-340) 0 $ Scale 0.1 0.1 $ Text str)
              (\event _ -> show event)
              (\_ world -> world)
-}


window = InWindow "COLLAPSE" (xWindow, yWindow) (300, 100)
background = white