module ScreenUpdate ( timeUpdate
                    , updateShiftings
                    , updateOnScreenStatuses
                    , zoomIn
                    , zoomOut
                    ) where

import Control.Lens
import GHC.Float (int2Float)

import DataFunctions


-- | Applies position shift of everything on the screen
-- <<< Time Position update
timeUpdate :: Float -> World -> World
timeUpdate time w = w & entitiesOnScreen %~ (updateScreenPosByTime time)
                      & objectsOnScreen  %~ (updateScreenPosByTime time)
                      & camera           %~ (updateCameraPosition time)

entitiesOnScreen :: Traversal' World ThingR
entitiesOnScreen = allLevelEntities.filtered isOnScreen.renderData

objectsOnScreen :: Traversal' World ThingR
objectsOnScreen  = allLevelObjects .filtered isOnScreen.renderData

updateScreenPosByTime :: Float -> ThingR -> ThingR
updateScreenPosByTime time tR@(ThingR rPos (xShift, yShift) isOnScreen) = 
      if isOnScreen 
         then tR & rPosition .~ (shiftRPos rPos (xShift * time, yShift * time))
         else tR

updateCameraPosition :: Float -> Camera -> Camera
updateCameraPosition time c@(Camera pos _ (xShift, yShift) _) = 
      let newPos = shiftRPos pos (xShift * time, yShift * time)
       in c & rPosition .~ newPos 
-- >>>

-- <<< Shift update
-- <<
updateShiftings :: Float -> World -> World
updateShiftings time = updateThingsShiftPerSec time . updateCameraShiftPerSec time

updateCameraShiftPerSec :: Float -> World -> World
updateCameraShiftPerSec leftedTime w = w & camera.shiftPerSec .~ newShiftPerSec
   where currentPos = getCameraPosition w
         targetPos  = calcCameraPositionByPPos w
         
         newShiftPerSec = calcShiftPerSec currentPos targetPos leftedTime
-- >>

-- <<
updateThingsShiftPerSec :: Float -> World -> World
updateThingsShiftPerSec leftedTime w = 
            w & allLevelEntities.filtered isOnScreen %~ updateThingShiftPerSec w leftedTime cameraShiftPerSec
              & allLevelObjects .filtered isOnScreen %~ updateThingShiftPerSec w leftedTime cameraShiftPerSec
   where cameraShiftPerSec = w ^. camera.shiftPerSec

updateThingShiftPerSec :: World -> Float -> RPositionShift -> Thing -> Thing
updateThingShiftPerSec w leftedTime cameraShiftPerSec t = t & renderData.shiftPerSec .~ newShiftPerSec
   where currentPos = t ^. renderData.rPosition 
         targetPos  = mapPos2ScreenPos w $ t ^. status.position
         
         newShiftPerSec = combineShiftWithCameraShift cameraShiftPerSec 
                        $ calcShiftPerSec currentPos targetPos leftedTime
-- >>
-- >>>


-- <<< OnScreen status functions
-- | Set OutOfScreen -> Check Screen Zone (-> set ShiftPerSec?)
updateOnScreenStatuses :: World -> World
updateOnScreenStatuses = updateOnScreenStatusZone . getOutOfScreen

-- << GET OUT
getOutOfScreen :: World -> World
getOutOfScreen w = w & allLevelEntities.filtered isOnScreen %~ getThingOutOfScreen w 
                     & allLevelObjects .filtered isOnScreen %~ getThingOutOfScreen w

getThingOutOfScreen :: World -> Thing -> Thing
getThingOutOfScreen w t = if inZone
                              then t
                              else t & renderData.onScreen .~ False
   where zonePositions = getRenderingZone w
         tPos = t ^. status.position
         inZone = isInZone zonePositions tPos
         
-- >>

-- << only adds things on screen (Watch out about SHIFTPERSEC!!!)
updateOnScreenStatusZone :: World -> World
updateOnScreenStatusZone w = processZone w screenZone setOnScreen
   where screenZone = getRenderingZone w
         
         setOnScreen w tID = let tR = w ^. unsafeThingByID tID.renderData
                              in w & unsafeThingByID tID %~ updateOnScreenStatus w tR

updateOnScreenStatus :: World -> ThingR -> Thing -> Thing
updateOnScreenStatus w tR@(ThingR _ _ isOnScreen) t = 
            if isOnScreen
               then t 
               else t & renderData .~ setOnScreen tRPos
   where tPos = t ^. status.position
         tRPos = mapPos2ScreenPos w tPos
-- >>

setOnScreen :: RPosition -> ThingR
setOnScreen rPos = ThingR rPos (0, 0) True -- RPositionShift should be assigned somethere else!!
-- >>>

-- <<< Camera related functions
-- <<
zoomIn :: World -> World
zoomIn w | zoom == maxZoom = w
         | otherwise       = updateShiftings turnTimeLeft . zoomRUpdate 2 $ w & camera.cellSize .~ maxZoom
                                                                              & camera.getScale .~ 1
   where zoom = w ^. camera.cellSize
         turnTimeLeft = w ^. worldState.leftedPhaseTime
-- >>

-- <<
zoomOut :: World -> World
zoomOut w | zoom == minZoom = w
          | otherwise       = updateShiftings turnTimeLeft . zoomRUpdate 0.5 $ w & camera.cellSize .~ minZoom
                                                                                 & camera.getScale .~ 0.5
   where zoom = w ^. camera.cellSize
         turnTimeLeft = w ^. worldState.leftedPhaseTime
-- >>

-- <<
-- ALЯRM! Camera CellSize MUST BE Updatete before call of this function
-- Im reaaly don't know why, but it's work (Maybe i should get at least 8 hrs of sleep? pls)
zoomRUpdate :: Float -> World -> World
zoomRUpdate scale w = processZone newCamPosW renderZone updateThingRPos
   where newCamPosW = w & camera.rPosition .~ newCamPos
         renderZone = getRenderingZone w
         
         currentCamPos = w ^. camera.rPosition
         newCamPos = calcCameraPositionByPPos w
         shiftForThingsOnScreen = rPosDiff currentCamPos newCamPos
         updateThingRPos w tID = w & unsafeThingByID tID.renderData.rPosition
                                   %~ flip shiftRPos shiftForThingsOnScreen . updateRPos
         
         vecToLeftUpCorner = (\(x, y) -> (negate x, negate y)) currentCamPos
         updateRPos rPos = rPosSum vecToLeftUpCorner . scaleRPosition scale $ rPosDiff rPos vecToLeftUpCorner
-- >>
-- >>>