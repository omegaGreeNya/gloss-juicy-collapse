module Input where

import Control.Lens
import Graphics.Gloss.Interface.IO.Interact
import Data.Vector ((!))

import Loading
import GameData
import RunTimeData
import ActionsData

event2Update :: Event -> World -> IO World
event2Update (EventKey (SpecialKey KeyRight) Down  _ _) w = movePlayer (1, 0) w
event2Update (EventKey (SpecialKey KeyLeft)  Down  _ _) w = movePlayer (-1, 0) w
event2Update (EventKey (SpecialKey KeyUp)    Down  _ _) w = movePlayer (0, 1) w
event2Update (EventKey (SpecialKey KeyDown)  Down  _ _) w = movePlayer (0, -1) w
event2Update _ w = return w


movePlayer :: Position -> World -> IO World
movePlayer (x, y) w = return $ playerMoveMade $ w & level.levelEntities.ix playerLevelID.plannedActions .~ [updatePosAct]
   where playerLevelID = 0
         
         newPos = (\(px, py) -> (px + x, py + y)) pPos
         pPos = w ^. level.levelEntities & (! playerLevelID)
                                         & (^. status.position)
         
         updatePosAct = Action (EntityID 0) (\eStatus -> eStatus & position .~ newPos) id

playerMoveMade :: World -> World
playerMoveMade w = w & worldState.playerMadeMove .~ True