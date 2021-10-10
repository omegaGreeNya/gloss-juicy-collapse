-- | Most Render just for debugging

module Render (world2Picture) where

import Control.Lens
import Data.Maybe (fromJust)
import GHC.Float (int2Float, int2Double, double2Float)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import Data.Foldable (foldl')

import Graphics.Gloss.Data.Picture (Picture(..))
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Juicy

import DataFunctions
import Logic

--world2Picture _ = return Blank

world2Picture :: World -> IO Picture
world2Picture w = return $ gridPic w
                        <> playerPlannedMove w
                        <> thingsMap2Pic w
                        <> tickRep w
                        <> timeLine w
                        <> playerAttackZones w -- rewrite hitbox data and then rewrite this, pls
                        <> hps w
                        <> test w
                        <> test2 w
                        <> test3 w
                        <> showPR w
                        <> showCamR w
                        -- <> debugINFO w
   
tickRep :: World -> Picture
tickRep w = Translate 0 200 $ Scale 0.1 0.1 $ Text $ show $ w ^. tick

-- <<
thingsMap2Pic :: World -> Picture
thingsMap2Pic w = withZone w zone thingID2Pic
   where zone = getRenderingZone w

thingID2Pic :: World -> ThingID -> Picture
thingID2Pic w tID = thing2Pic w $ w ^. unsafeThingByID tID

thing2Pic :: World -> Thing -> Picture
thing2Pic w t = Translate x y
              $ Scale scale scale
              $ tPic
   where dID = t ^. dataID
         tPic = w ^. unsafeThingDataByID dID.pic
         
         (x, y) = t ^. renderData.rPosition
         
         scale = w ^. camera.getScale
-- >>

-- <<
gridPic :: World -> Picture
gridPic w = Color black
          $ Pictures
          $ map makeYLine [minX, minX + cellS .. maxX]
          <> map makeXLine [maxY, maxY - cellS .. minY]
   where (xWindow, yWindow) = w ^. ui.windowSize
         minX = (int2Float xWindow)/(-2)
         maxX = negate minX
         minY = (int2Float yWindow)/(-2)
         maxY = negate minY
         
         makeYLine x = Line [(x, minY), (x, maxY)]
         makeXLine y = Line [(minX, y), (maxX, y)]
         
         cellS = int2Float $ w ^. camera.cellSize

playerPlannedMove :: World -> Picture
playerPlannedMove w = Color aquamarine $ Line [curPPosR, nxtPPosR]
   where pID   = w ^. player.playerEntityID
         p     = w ^. unsafeThingByID pID
         nextP = applyMove p
         
         curPPos = p ^. status.position
         curPPosR = mapPos2ScreenPos w curPPos
         
         nxtPPos = nextP ^. status.position
         nxtPPosR = mapPos2ScreenPos w nxtPPos

-- Rework this to get rid of ticks2time, pls?
timeLine :: World -> Picture
timeLine w = Translate lineWidthPos (lineHightPos w) (timeLine' w)

timeLine' :: World -> Picture
timeLine' w = case currentState of
                   PlayerThinkTime1 -> Color cyan $ Line linePath
                   PlayerThinkTime2 -> Color red  $ Line linePath
                   _                -> Blank
   where currentState = w ^. worldState.state
         
         maxTime = getStateTime currentState
         
         timeUsed = (maxTime -) . ticks2time w $ w ^. worldState.leftedPhaseTicks
         
         actualLineLength = (timeUsed / maxTime) * lineLength
         
         linePath = [(0,0), (actualLineLength, 0)]

playerAttackZones :: World -> Picture
playerAttackZones w = (preMoveZone pAttacks) <> (aftMoveZone pAttacks)
   where pID = w ^. player.playerEntityID
         pAttacks = w ^. unsafeEntityByID pID.plannedAct.attacks
         
         preMoveZone []          = Blank
         preMoveZone (pAttack:_) = let fstHit = fst pAttack
                                    in Color red $ hitBox2Picture w fstHit
         
         
         aftMoveZone []          = Blank
         aftMoveZone (pAttack:_) = let sndHit = snd pAttack
                                    in Color blue $ hitBox2Picture w sndHit
         
         
hitBox2Picture :: World -> HitBox -> Picture
hitBox2Picture _ None = Blank
hitBox2Picture w hit@(SimpleHitBox{}) = zoneWithPic w (_pos1 hit) (_pos2 hit) $ Scale 0.3 0.3 $ Text "x"
hitBox2Picture w hit@(CellBox{}) = foldMap (cell2HitPic w) (_cells hit)

cell2HitPic :: World -> HitCell -> Picture
cell2HitPic w (HitCell hitPos hitDir _) = Translate x y $ Rotate angle $ Scale 0.3 0.3 $ Text ">"
   where angle = int2Float . (45*) $ dir2Plan hitDir
         (x, y) = mapPos2ScreenPos w hitPos
   


zoneWithPic :: World -> Position -> Position -> Picture -> Picture
zoneWithPic w (x1, y1) (x2, y2) pic = zoneWithPic' w (Pictures []) xmin ymin xmin ymin xmax ymax pic
   where xmin = min x1 x2
         xmax = max x1 x2
         ymin = min y1 y2
         ymax = max y1 y2

zoneWithPic' :: World -> Picture -> Int -> Int -> Int -> Int -> Int -> Int -> Picture -> Picture
zoneWithPic' w pics x y xmin ymin xmax ymax pic | x < xmax = zoneWithPic' w updPics (x + 1) y xmin ymin xmax ymax pic
                                                | y < ymax = zoneWithPic' w updPics xmin (y + 1) xmin ymin xmax ymax pic
                                                | otherwise = updPics
   where updPics = pics <> placedPic
         placedPic = Translate x' y' pic
         (x',y') = mapPos2ScreenPos w (x, y)

hps :: World -> Picture
hps w = entities2hpPic w entitiesVec
   where entitiesVec = w ^. level.levelEntities

entities2hpPic :: Foldable t => World -> t Thing -> Picture
entities2hpPic w entities = foldMap (entity2hpPic w) entities

entity2hpPic :: World -> Thing -> Picture
entity2hpPic w e = Color green $ Translate x' y' $ Line linePath
   where (x, y) = e ^. renderData.rPosition
         
         cell = int2Float $ w ^. camera.cellSize
         
         x' = x - cell/2
         y' = y + cell/2 - 10
         
         dID = e ^. dataID
         
         mMaxHP = w ^? unsafeThingDataByID dID.stat.maxHP
         
         eMaxHP = maybe 0 int2Float mMaxHP
         eHP = e ^. status.hp
         
         hpLineLength = eHP/eMaxHP * cell
         linePath = [(0, 0), (hpLineLength, 0)]
         
         
         
-- Time for UI Data module?
lineLength = 100
lineHightPos w = (int2Float (w ^. ui.windowSize._2) / 2) - 10
lineWidthPos = -(lineLength / 2)

test :: World -> Picture
test w = Text $ show dummyHP
   where dummyHP = fromJust $ w ^? level.levelEntities.ix 1.status.hp

test2 w = Translate 0 (-100) $ Text (show pPos)
   where pPos = fromJust $ w  ^? unsafeEntityByID (EntityID 0).status.position

test3 w = Translate 0 (-200) $ Text (show ePos)
   where ePos = fromJust $ w  ^? unsafeEntityByID (EntityID 1).status.position

showPR :: World -> Picture
showPR w = Translate (-400) (-200) $ Scale 0.1 0.1 $ Text $ show pTR
   where pTID = w ^. player.playerEntityID
         pTR = w ^. unsafeThingByID pTID.renderData

showCamR :: World -> Picture
showCamR w = Translate (-400) (-220) $ Scale 0.1 0.1 $ Text $ show camInfo
   where camInfo = w ^. camera


debugINFO :: World -> Picture
debugINFO w = infoZone <> translate (worldStateINFO w)
   where (windowX, windowY) = w ^. ui.windowSize
         
         widthHalf  = 150
         heightHalf = 100
         leftUp    = (negate $ widthHalf, heightHalf)
         leftDown  = (negate $ widthHalf, negate $ heightHalf)
         rightDown = (widthHalf,          negate $ heightHalf)
         rightUp   = (widthHalf,          heightHalf)
         pPath = [leftUp, leftDown, rightDown, rightUp]
         
         (centerX, centerY) = ( int2Float windowX / 2 - widthHalf
                              , int2Float windowY / 2 - heightHalf)
         translate = Translate centerX centerY 
         
         infoZone_ = translate $ Color white $ Polygon pPath
         infoZone  = infoZone_ <> (translate $ Color black $ Line $ pPath <> [leftUp])

worldStateINFO :: World -> Picture
worldStateINFO w = (Translate 0 0 $ Scale 0.1 0.1 $ Text leftedTicks)
                <> (Translate 0 (-15) $ Scale 0.1 0.1 $ Text state_)
   where state_ = "state: " <> (show $ w ^. worldState.state)
         leftedTicks = "ticksLeft: " <> (show $ w ^. worldState.leftedPhaseTicks)