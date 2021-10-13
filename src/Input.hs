-- | wtf with numpad keys??

module Input ( event2Update
             , pressedKeys2PlannedActions
             ) where

import Control.Lens
import Graphics.Gloss.Interface.IO.Interact
import Data.Maybe (fromJust, isJust)
import System.Exit (exitWith, ExitCode(ExitSuccess))
import qualified Data.Map.Strict as Map (Map, fromList, lookup, union, member)
import qualified Data.Set as Set (insert, delete, member)

import Loading
import DataFunctions
import ScreenUpdate

moveKeys :: Map.Map Key PosShift
moveKeys = Map.fromList moveKeysList

moveKeysList :: [(Key, PosShift)]
moveKeysList = [ (SpecialKey KeyEnd     , (-1, 1 ))
               , (SpecialKey KeyDown    , (0 , 1 ))
               , (SpecialKey KeyPageDown, (1 , 1 ))
               , (SpecialKey KeyLeft    , (-1, 0 ))
               , (Char '\f'             , (0 , 0 )) -- Num 5, need some workaround
               , (SpecialKey KeyRight   , (1 , 0 ))
               , (SpecialKey KeyHome    , (-1, -1))
               , (SpecialKey KeyUp      , (0 , -1))
               , (SpecialKey KeyPageUp  , (1 , -1))
               ]

systemKeys :: Map.Map Key (World -> IO World)
systemKeys = Map.fromList [ (SpecialKey KeyEsc, \_ -> exitWith ExitSuccess)
                          , (Char  '=', return . zoomIn)
                          , (Char  '-', return . zoomOut)
                          , (Char  'p', return . setTemp ultraFastTemp)
                          , (Char  '[', return . setTemp normalTemp)
                          , (Char  ']', return . setTemp slowTemp)
                          ]

attackKeys :: Map.Map Key (World -> IO World)--PlannedAttack
attackKeys = Map.fromList [(Char  'a', playerTestAttack)
                          ]

alterKeys ::  Map.Map Key (World -> IO World) -- A bit of kastili
alterKeys = Map.fromList [ (SpecialKey KeyAltL, return)
                         , (SpecialKey KeyCtrlL, return)
                         , (SpecialKey KeyShiftL, return)
                         ]

playerEntityKeysCheck :: Key -> Bool
playerEntityKeysCheck key = Map.member key moveKeys 
                         || Map.member key attackKeys

{-
keyMapUp :: Map.Map Key (World -> IO World)
keyMapUp = Map.fromList [(Char '\f', movePlayer (0, 0))]
-}

event2Update :: Event -> World -> IO World
event2Update (EventKey pressedKey  Down  _ _) w | isJust maybeSystemAct = (fromJust maybeSystemAct) w
                                                | itsPlayerKey          = return $ w & player.pressedKeys %~ Set.insert pressedKey
                                                | otherwise             = return w
   where maybeSystemAct = Map.lookup pressedKey systemKeys
         itsPlayerKey   = playerEntityKeysCheck pressedKey 

event2Update (EventKey releasedKey Up    _ _) w = return $ w & player.pressedKeys %~ Set.delete releasedKey
event2Update _ w = return w

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

-- <<< Parsing pressed keys to player plannedActions
pressedKeys2PlannedActions :: World -> World
pressedKeys2PlannedActions w = w & unsafeThingByID pTID.plannedAct .~ actions
   where actions = Action maybePosShift []
         
         maybePosShift = maybe Nothing (Just . snd) . safeHead
            $ dropWhile (\(key,_) -> not $ isKeyDown key w) moveKeysList
         
         pTID   = w ^. player.playerEntityID
         pThing = w ^. unsafeThingByID pTID
         
         
         
         
         
         altDown   = isKeyDown (SpecialKey KeyAltL  ) w 
         ctrlDown  = isKeyDown (SpecialKey KeyCtrlL ) w 
         shiftDown = isKeyDown (SpecialKey KeyShiftL) w 
         