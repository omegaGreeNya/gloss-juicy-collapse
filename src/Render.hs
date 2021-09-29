module Render where

import Control.Lens
import Data.Maybe (fromJust)
import GHC.Float (int2Float)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector

import Graphics.Gloss.Data.Picture (Picture(..))
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Juicy

import Data.IORef

import GameData
import EntityObjectSpec
import RunTimeData
import Logic (applyPlan)


mapPos2ScreenPos :: Position -> (Float, Float)
mapPos2ScreenPos (x, y) = ( fst leftUpperScreenPos + int2Float (x * xCell)
                          , snd leftUpperScreenPos + int2Float (y * yCell))

world2Picture :: World -> IO Picture
world2Picture w = do
   mapPic <- thingsMap2Pic w
   let gridPic = grid (int2Float xCell) (int2Float yCell)
   return $ gridPic <> (playerPlannedMove w)  <> mapPic <> (tickRep w) <> (timeLine w)
   
tickRep :: World -> Picture
tickRep w = Scale 0.1 0.1 $ Text $ show $ w ^. tick


thingsMap2Pic :: World -> IO Picture
thingsMap2Pic w = vectors2Pic
                $ fmap (fmap $ thingsListID2Pic w) $ w ^. level.thingsMap

vectors2Pic :: Vector (Vector (IO Picture)) -> IO Picture
vectors2Pic vec = concatFoldableIOPic $ fmap concatFoldableIOPic vec

thingsListID2Pic :: World -> [ThingID] -> IO Picture
thingsListID2Pic w things = concatFoldableIOPic
                          $ fmap (thingID2Pic w) things

thingID2Pic :: World -> ThingID -> IO Picture
thingID2Pic w (EntityID x) = entity2Pic w e
   where e = flip (!) x $ w ^. level.levelEntities
thingID2Pic w (ObjectID x) = object2Pic w o
   where o = flip (!) x $ w ^. level.levelObjects

entity2Pic :: World -> Entity -> IO Picture
entity2Pic w e = fmap (Translate x y)
               $ ePic
   where (x, y) = mapPos2ScreenPos ePos
         ePos = e ^. status.position
         
         ePic = fromJust $ w ^? allData.entities.ix eID.pic
         eID = e ^. dataID

object2Pic = undefined

concatFoldableIOPic :: (Foldable t) => t (IO Picture) -> IO Picture
concatFoldableIOPic = foldr (concatIOPic) (return Blank)

concatIOPic :: IO Picture -> IO Picture -> IO Picture
concatIOPic p1 p2 = (fmap (<>) p1) <*> p2

leftUpperScreenPos = (negate $ int2Float xWindow/2 - (int2Float xCell / 2), int2Float yWindow/2 - (int2Float yCell / 2))


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
   where wWithPlayerActed = applyPlan w pPlan
         
         pPlan = p ^. plannedActions
         p     = flip (!) 0 $ w ^. level.levelEntities
         nextP = flip (!) 0 $ wWithPlayerActed  ^. level.levelEntities
         
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
         

-- Time for UI Data module?
lineLength = 100
lineHightPos = (int2Float yWindow / 2) - 10
lineWidthPos = -(lineLength / 2)