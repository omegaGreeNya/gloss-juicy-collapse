module Input where

import Control.Lens
import Graphics.Gloss.Interface.IO.Interact
import Data.Maybe (fromJust)
import System.Exit (exitWith, ExitCode(ExitSuccess))
import qualified Data.Map.Strict as Map (Map, fromList, lookup)

import Loading
import DataFunctions
import ScreenUpdate

keyMapDown :: Map.Map Key (World -> IO World)
keyMapDown = Map.fromList [ (SpecialKey KeyEnd     , movePlayer (-1, 1))
                          , (SpecialKey KeyDown    , movePlayer (0, 1))
                          , (SpecialKey KeyPageDown, movePlayer (1, 1))
                          , (SpecialKey KeyLeft    , movePlayer (-1, 0))
                          , (Char '\f'             , movePlayer (0, 0))
                          , (SpecialKey KeyRight   , movePlayer (1, 0))
                          , (SpecialKey KeyHome    , movePlayer (-1, -1))
                          , (SpecialKey KeyUp      , movePlayer (0, -1))
                          , (SpecialKey KeyPageUp  , movePlayer (1, -1))
                          , (SpecialKey KeyEsc, \_ -> exitWith ExitSuccess)
                          , (Char  'a', playerTestAttack)
                          , (Char  '=', return . zoomIn)
                          , (Char  '-', return . zoomOut)
                          , (Char  'p', return . setTemp ultraFastTemp)
                          , (Char  '[', return . setTemp normalTemp)
                          , (Char  ']', return . setTemp slowTemp)
                          ]
keyMapUp :: Map.Map Key (World -> IO World)
keyMapUp = Map.fromList [(Char '\f', movePlayer (0, 0))]

event2Update_ :: Event -> World -> IO World
event2Update_ (EventKey pressedKey Down  _ _) w =
                                               case Map.lookup pressedKey keyMapDown of
                                                               Nothing -> return w
                                                               Just act -> act w
event2Update_ (EventKey pressedKey Up  _ _) w =
                                               case Map.lookup pressedKey keyMapUp of
                                                               Nothing -> return w
                                                               Just act -> act w
event2Update_ _ w = return w


playerLevelID = 0

movePlayer :: PosShift -> World -> IO World
movePlayer posShift w = return $ playerMoveMade $ w & unsafeThingByID pID.plannedAct %~ updateAct
   where updateAct act = act & move .~ (Just posShift)
         pID = w ^. player.playerEntityID

playerMoveMade :: World -> World
playerMoveMade w = w & worldState.playerMadeMove .~ True

playerTestAttack :: World -> IO World
playerTestAttack w = return $ playerMoveMade $ w & unsafeThingByID pID.plannedAct %~ setAttack
   where pID = w ^. player.playerEntityID
         setAttack act = act & attacks .~ [testAttack pPos]
         pPos = w ^. unsafeThingByID pID.status.position

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