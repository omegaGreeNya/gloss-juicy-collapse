{-# LANGUAGE RankNTypes #-}

module DataFunctions ( safeSucc
                     , unsafeEntityByID
                     , unsafeObjectByID
                     , unsafeThingByID
                     , unsafeEntityDataByID
                     , unsafeObjectDataByID
                     , unsafeThingDataByID
                     , allLevelEntities
                     , allLevelObjects
                     , getCameraPosition
                     , calcCameraPositionByPPos
                     , mapPos2ScreenPos
                     , getRenderingZone
                     , isInZone
                     , isOnScreen
                     , processZone
                     , withZone
                     , getStateTime
                     , timeLeftedForMove
                     , ticks2time
                     , module RunTimeData
                     , module ScreenData
                     , module GameData
                     , module ActionsData
                     , module HitData
                     , module EntityObjectSpec
                     ) where

import Control.Lens
import GHC.Float (int2Float)
import Data.Maybe (fromJust)
import Data.Foldable (foldl')

import GameData
import ActionsData
import RunTimeData
import ScreenData
import HitData
import EntityObjectSpec


-- <<
safeSucc :: (Eq a, Enum a, Bounded a) => a -> a
safeSucc a | a == maxBound = minBound
           | otherwise     = succ a
-- >>
-- <<
unsafeEntityByID :: ThingID -> Lens' World Thing
unsafeEntityByID (EntityID x) = lens getter setter
   where getter w = fromJust $ w ^? level.levelEntities.ix x
         setter w e = w & level.levelEntities.ix x .~ e

unsafeObjectByID :: ThingID -> Lens' World Thing
unsafeObjectByID (ObjectID x) = lens getter setter
   where getter w = fromJust $ w ^? level.levelObjects.ix x
         setter w o = w & level.levelObjects.ix x .~ o

unsafeThingByID :: ThingID -> Lens' World Thing
unsafeThingByID tID@(EntityID x) = unsafeEntityByID tID
unsafeThingByID tID@(ObjectID x) = unsafeObjectByID tID
-- >>

-- <<
unsafeEntityDataByID :: DataID -> Lens' World ThingData
unsafeEntityDataByID (EntityDataID x) = lens getter setter
   where getter w = fromJust $ w ^? allData.entities.ix x
         setter w e = w & allData.entities.ix x .~ e

unsafeObjectDataByID :: DataID -> Lens' World ThingData
unsafeObjectDataByID (ObjectDataID x) = lens getter setter
   where getter w = fromJust $ w ^? allData.objects.ix x
         setter w o = w & allData.objects.ix x .~ o

unsafeThingDataByID :: DataID -> Lens' World ThingData
unsafeThingDataByID dID@(EntityDataID x) = unsafeEntityDataByID dID
unsafeThingDataByID dID@(ObjectDataID x) = unsafeObjectDataByID dID
-- >>

-- <<
allLevelEntities :: Traversal' World Thing
allLevelEntities = level.levelEntities.traversed

allLevelObjects :: Traversal' World Thing
allLevelObjects = level.levelObjects.traversed
-- >>
-- <<< Screen Related Functions
getCameraPosition :: World -> RPosition
getCameraPosition w = w ^. camera.rPosition

calcCameraPositionByPPos :: World -> RPosition
calcCameraPositionByPPos w = (cameraX, cameraY)
   where cSize = w ^. camera.cellSize
   
         levelXRsize = int2Float $ w ^. level.levelSize._1 * cSize
         levelYRsize = int2Float $ w ^. level.levelSize._2 * cSize
         
         pTID = w ^. player.playerEntityID
         (pX, pY) = w ^. unsafeEntityByID pTID.status.position
         pRX = int2Float $ pX * cSize
         pRY = int2Float $ pY * cSize
         
         halfWindowX = int2Float $ w ^. ui.windowSize._1 `div` 2
         halfWindowY = int2Float $ w ^. ui.windowSize._2 `div` 2
         
         cameraX = min (max pRX halfWindowX) (levelXRsize - halfWindowX)
         cameraY = negate $ min (max pRY halfWindowY) (levelYRsize + halfWindowY)

-- | Ralatively to (0,0) cell
calcCellPosition :: World -> Position -> RPosition
calcCellPosition w (x, y) = (int2Float          $ cellSize' * x + halfCellSize
                            ,int2Float . negate $ cellSize' * y + halfCellSize)
   where cellSize' = w ^. camera.cellSize
         halfCellSize = cellSize' `div` 2

mapPos2ScreenPos :: World -> Position -> RPosition
mapPos2ScreenPos w p = (thngX - camX, thngY - camY)
   where (camX, camY) = w ^. camera.rPosition
         (thngX, thngY) = calcCellPosition w p

getRenderingZone :: World -> (Position, Position)
getRenderingZone w = ((xmin, ymin), (xmax, ymax))
   where cellSize' = w ^. camera.cellSize
         (wx, wy)  = w ^. ui.windowSize
         
         heightByCell = 1 + wy `div` cellSize'
         widthByCell  = 1 + wx `div` cellSize'
         
         pTID = w ^. player.playerEntityID
         (px, py) = w ^. unsafeEntityByID pTID.status.position
         
         (maxXPos, maxYPos) = w ^. level.levelSize
         
         xmin = max 0       (px - (widthByCell  `div` 2) - 2)
         ymin = max 0       (py - (heightByCell `div` 2) - 2)
         xmax = min maxXPos (px + (widthByCell  `div` 2) + 2)
         ymax = min maxYPos (py + (heightByCell `div` 2) + 2)

isInZone :: (Position, Position) -- Zone
         -> Position            -- Position to check entering in zone
         -> Bool
isInZone ((a, b), (c, d)) (x, y) = (((a <= x) && (x <= c)) || ((c <= x) && (x <= a)))
                                && (((b <= y) && (y <= d)) || ((d <= y) && (y <= b)))

isOnScreen :: Thing -> Bool
isOnScreen t = t ^. renderData.onScreen
-- >>>

-- <<
processZone :: World                       -- World to apply
            -> (Position, Position)        -- Zone
            -> (World -> ThingID -> World) -- updating world on ThingID
            -> World                       -- result of applying actions on every ThingID in zone
processZone w ((x1, y1), (x2, y2)) action = processZone' w xmin ymin xmin ymin xmax ymax action
   where xmin = min x1 x2
         xmax = max x1 x2
         ymin = min y1 y2
         ymax = max y1 y2

processZone' :: World                       -- World to apply
             -> Int                         -- Current width pos (x)
             -> Int                         -- Current height pos (y)
             -> Int                         -- min x
             -> Int                         -- min y
             -> Int                         -- max x
             -> Int                         -- max y
             -> (World -> ThingID -> World) -- updating world on ThingID
             -> World                       -- result of applying actions on every ThingID in zone
processZone' w x y xmin ymin xmax ymax action | x < xmax = processZone' newW (x + 1) y xmin ymin xmax ymax action
                                              | y < ymax = processZone' newW xmin (y + 1) xmin ymin xmax ymax action
                                              | otherwise = newW
   where newW = foldl' action w targetThingsIDs
         targetThingsIDs = w ^. level.thingsMap.ix x.ix y
-- >>

-- | apply function on Things IDs in some zone and concatinate the results
-- <<
withZone :: Monoid a
         => World
         -> (Position, Position)
         -> (World -> ThingID -> a)
         -> a
withZone w ((x1, y1), (x2, y2)) action = withZone' w xmin ymin xmin ymin xmax ymax action mempty
   where xmin = min x1 x2
         xmax = max x1 x2
         ymin = min y1 y2
         ymax = max y1 y2

withZone' :: Monoid a
          => World
          -> Int
          -> Int
          -> Int
          -> Int
          -> Int
          -> Int
          -> (World -> ThingID -> a)
          -> a
          -> a
withZone' w x y xmin ymin xmax ymax action result | x < xmax = withZone' w (x + 1) y xmin ymin xmax ymax action updatedResult
                                                  | y < ymax = withZone' w xmin (y + 1) xmin ymin xmax ymax action updatedResult
                                                  | otherwise = updatedResult
   where updatedResult = result <> foldMap (action w) targetThingsIDs
         targetThingsIDs = w ^. level.thingsMap.ix x.ix y
-- >>

-- << Time Related Functions
tBM, tM, tAM, tPT1, tPT2 :: Float -- state time (t - time, STATE)
tBM  = 0.4
tM   = 0.2
tAM  = 0.4
tPT1 = 0.5
tPT2 = 2

getStateTime :: State -> Float
getStateTime (BeforeMoves)      = tBM
getStateTime (Moves)            = tM
getStateTime (AfterMoves)       = tAM 
getStateTime (PlayerThinkTime1) = tPT1
getStateTime (PlayerThinkTime2) = tPT2

timeLeftedForMove :: State -> Float                                           -- Slow Motion multiplier
timeLeftedForMove (BeforeMoves)      = tBM                                    -- x1
timeLeftedForMove (Moves)            = tM + tAM + tPT1 / 4 + tPT2 / 8 + tBM   -- x1
timeLeftedForMove (AfterMoves)       = tAM + tPT1 / 4 + tPT2 / 8 + tBM        -- x1
timeLeftedForMove (PlayerThinkTime1) = tPT1 + tPT2 / 2 + tBM * 4              -- x4
timeLeftedForMove (PlayerThinkTime2) = tPT2 + tBM * 8                         -- x8


ticks2time :: World -> Tick -> Float
ticks2time w t = int2Float t / int2Float ticksPerSec
   where ticksPerSec = w ^. ui.tps
-- >>