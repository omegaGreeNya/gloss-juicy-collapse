-- | Most Render just for debugging

module Render where

import Control.Lens
import Data.Maybe (fromJust)
import GHC.Float (int2Float, int2Double, double2Float)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import Data.Foldable (foldl')

import Graphics.Gloss.Data.Picture (Picture(..))
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Juicy

import Data.IORef

import GameData
import EntityObjectSpec
import RunTimeData
import Logic (applyMove)
import ActionsData
import HitData


mapPos2ScreenPos :: Position -> (Float, Float)
mapPos2ScreenPos (x, y) = ( fst leftUpperScreenPos + int2Float (x * xCell)
                          , snd leftUpperScreenPos - int2Float (y * yCell))

leftUpperScreenPos = (negate $ int2Float xWindow/2 - (int2Float xCell / 2), int2Float yWindow/2 - (int2Float yCell / 2))

world2Picture :: World -> IO Picture
world2Picture w = return $ gridPic <> (playerPlannedMove w) <> mapPic <> (tickRep w) <> (timeLine w) <> (playerAttackZones w) <> (hps w) <> (test w) <> (test2 w)<> (test3 w)
   where 
         mapPic = thingsMap2Pic w
         gridPic = grid (int2Float xCell) (int2Float yCell)
   
tickRep :: World -> Picture
tickRep w = Scale 0.1 0.1 $ Text $ show $ w ^. tick


thingsMap2Pic :: World -> Picture
thingsMap2Pic w = vectors2Pic
                $ fmap (fmap $ thingsID2Pic w) $ w ^. level.thingsMap

vectors2Pic :: Vector (Vector (Picture)) -> Picture
vectors2Pic vec = concatFoldableIOPic $ fmap concatFoldableIOPic vec

thingsID2Pic :: Foldable t => World -> t ThingID -> Picture
thingsID2Pic w things = foldMap (thingID2Pic w) things

thingID2Pic :: World -> ThingID -> Picture
thingID2Pic w (EntityID x) = entity2Pic w e
   where e = flip (!) x $ w ^. level.levelEntities
thingID2Pic w (ObjectID x) = object2Pic w o
   where o = flip (!) x $ w ^. level.levelObjects

entity2Pic :: World -> Entity -> Picture
entity2Pic w e = (Translate x y)
               $ ePic
   where (x, y) = mapPos2ScreenPos ePos
         ePos = e ^. status.position
         
         ePic = fromJust $ w ^? allData.entities.ix eID.pic
         eID = e ^. dataID

object2Pic = undefined

concatFoldableIOPic :: (Foldable t) => t Picture -> Picture
concatFoldableIOPic = foldr (<>) Blank

grid :: Float -> Float -> Picture
grid xCell yCell = Color black
                 $ Pictures
                 $ map makeXLine [minX, minX + xCell .. maxX]
                <> map makeYLine [minY, minY + yCell .. maxY]
   where minX = (int2Float xWindow)/(-2)
         maxX = (-1 * minX)
         minY = (int2Float yWindow)/(-2)
         maxY = (-1 * minY)
         
         makeXLine x = Line [(x, minY), (x, maxY)]
         makeYLine y = Line [(minX, y), (maxX, y)]

playerPlannedMove :: World -> Picture
playerPlannedMove w = Color aquamarine $ Line [curPPos', nxtPPos']
   where p     = flip (!) 0 $ w ^. level.levelEntities
         nextP = applyMove p
         
         curPPos = p ^. status.position
         curPPos' = mapPos2ScreenPos curPPos
         nxtPPos = nextP ^. status.position
         nxtPPos' = mapPos2ScreenPos nxtPPos
         
timeLine :: World -> Picture
timeLine w = Translate lineWidthPos lineHightPos (timeLine' w)

timeLine' :: World -> Picture
timeLine' w = if isNotExtraTime then Color cyan $ Line linePath
                                else Color red  $ Line linePath
   where isNotExtraTime = w ^. worldState.extraTimeLeft
         maxTime = if isNotExtraTime
                        then turnTime
                        else additionalTurnTime
         timeUsed = (maxTime -) $ w ^. worldState.leftedTurnTicks
         
         actualLineLength = (int2Float timeUsed / int2Float maxTime) * lineLength
         
         linePath = [(0,0), (actualLineLength, 0)]

playerAttackZones :: World -> Picture
playerAttackZones w = (preMoveZone pAttacks) <> (aftMoveZone pAttacks)
   where pAttacks = w ^? level.levelEntities.ix 0.plannedActions.attacks
         
         preMoveZone Nothing            = Blank
         preMoveZone (Just [])          = Blank
         preMoveZone (Just (pAttack:_)) = let fstHit = fst pAttack
                                           in Color red $ hitBox2Picture fstHit
         
         
         aftMoveZone Nothing            = Blank
         aftMoveZone (Just [])          = Blank
         aftMoveZone (Just (pAttack:_)) = let sndHit = snd pAttack
                                           in Color blue $ hitBox2Picture sndHit
         
         
hitBox2Picture :: HitBox -> Picture
hitBox2Picture None = Blank
hitBox2Picture hit@(SimpleHitBox{}) = zoneWithPic (_pos1 hit) (_pos2 hit) $ Scale 0.3 0.3 $ Text "x"
hitBox2Picture hit@(CellBox{}) = foldMap cells2HitPic (_cells hit)

cells2HitPic :: HitCell -> Picture
cells2HitPic (HitCell hitPos hitDir _) = Translate x y $ Rotate angle $ Scale 0.3 0.3 $ Text ">"
   where angle = int2Float . (45*) $ dir2Plan hitDir
         (x, y) = mapPos2ScreenPos hitPos
   


zoneWithPic :: Position -> Position -> Picture -> Picture
zoneWithPic (x1, y1) (x2, y2) pic = zoneWithPic' (Pictures []) x1 y1 x1 y1 x2 y2 pic

zoneWithPic' :: Picture -> Int -> Int -> Int -> Int -> Int -> Int -> Picture -> Picture
zoneWithPic' pics x y xmin ymin xmax ymax pic | x < xmax = zoneWithPic' updPics (x + 1) y xmin ymin xmax ymax pic
                                              | y < ymax = zoneWithPic' updPics xmin (y + 1) xmin ymin xmax ymax pic
                                              | otherwise = updPics
   where updPics = pics <> placedPic
         placedPic = Translate x' y' pic
         (x',y') = mapPos2ScreenPos (x, y)

hps :: World -> Picture
hps w = entities2hpPic w entitiesVec
   where entitiesVec = w ^. level.levelEntities

entities2hpPic :: Foldable t => World -> t Entity -> Picture
entities2hpPic w entities = foldMap (entity2hpPic w) entities

entity2hpPic :: World -> Entity -> Picture
entity2hpPic w e = Color green $ Translate x' y' $ Line linePath
   where ePos = e ^. status.position
         (x, y) = mapPos2ScreenPos ePos
         
         x' = x - (int2Float xCell)/2
         y' = y + (int2Float yCell)/2 - 10
         
         dID = e ^. dataID
         
         mMaxHP = w ^? allData.entities.ix dID.stats.maxHP
         
         eMaxHP = maybe 0 int2Float mMaxHP
         eHP = double2Float $ e ^. status.hp
         
         hpLineLength = eHP/eMaxHP * (int2Float xCell)
         linePath = [(0, 0), (hpLineLength, 0)]
         
         
         
-- Time for UI Data module?
lineLength = 100
lineHightPos = (int2Float yWindow / 2) - 10
lineWidthPos = -(lineLength / 2)

test :: World -> Picture
test w = Text $ show dummyHP
   where dummyHP = fromJust $ w ^? level.levelEntities.ix 1.status.hp

test2 w = Translate 0 (-100) $ Text (show pPos)
   where pPos = fromJust $ w  ^? unsafeEntityByID (EntityID 0).status.position

test3 w = Translate 0 (-200) $ Text (show ePos)
   where ePos = fromJust $ w  ^? unsafeEntityByID (EntityID 1).status.position
