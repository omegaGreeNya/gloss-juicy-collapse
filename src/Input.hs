module Input where

import Control.Lens
import Graphics.Gloss.Interface.IO.Interact
import Data.Maybe (fromJust)
import System.Exit (exitWith, ExitCode(ExitSuccess))

import Loading
import GameData
import RunTimeData
import ActionsData
import HitData

event2Update :: Event -> World -> IO World
event2Update (EventKey (SpecialKey KeyRight) Down  _ _) w = movePlayer (1, 0) w
event2Update (EventKey (SpecialKey KeyLeft)  Down  _ _) w = movePlayer (-1, 0) w
event2Update (EventKey (SpecialKey KeyUp)    Down  _ _) w = movePlayer (0, -1) w
event2Update (EventKey (SpecialKey KeyDown)  Down  _ _) w = movePlayer (0, 1) w
event2Update (EventKey (SpecialKey KeyEsc)   Down  _ _) w = exitWith ExitSuccess
event2Update (EventKey (Char  'a'         )  Down  _ _) w = playerTestAttack w
event2Update _ w = return w


playerLevelID = 0

movePlayer :: PosShift -> World -> IO World
movePlayer posShift w = return $ playerMoveMade $ w & level.levelEntities.ix playerLevelID.plannedActions %~ updateAct
   where updateAct act = act & move .~ (Just posShift)

playerMoveMade :: World -> World
playerMoveMade w = w & worldState.playerMadeMove .~ True

playerTestAttack :: World -> IO World
playerTestAttack w = return $ playerMoveMade $ w & level.levelEntities.ix playerLevelID.plannedActions %~ setAttack
   where setAttack act = act & attacks .~ [testAttack pPos]
         
         pPos = fromJust $ w ^? level.levelEntities.ix playerLevelID.status.position

testAttack :: Position -> PlannedAttack
testAttack pPos = (fstHitBox, sndHitBox)
   where fstHitBox = SimpleHitBox (nwOf pPos) (nOf pPos) (10, 10)
         sndHitBox = CellBox [HitCell (neOf pPos) (1, 1) 0.5, HitCell (eOf pPos) (0, 1) 1] (10, 10)


-- move to another module
nwOf :: Position -> Position
nwOf (x, y) = (x - 1, y - 1)

nOf :: Position -> Position
nOf (x, y) = (x, y - 1)

neOf :: Position -> Position
neOf (x, y) = (x + 1, y - 1)

eOf :: Position -> Position
eOf (x, y) = (x + 1, y)

seOf :: Position -> Position
seOf (x, y) = (x + 1, y + 1)

sOf :: Position -> Position
sOf (x, y) = (x, y + 1)

swOf :: Position -> Position
swOf (x, y) = (x - 1, y + 1)

wOf :: Position -> Position
wOf (x, y) = (x - 1, y)
