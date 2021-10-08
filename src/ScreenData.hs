{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module ScreenData where

import Control.Lens

type RPosition = (Float, Float)
type RPositionShift = (Float, Float)

shiftRPos :: RPosition -> RPositionShift -> RPosition
shiftRPos (x, y) (dx, dy) = (x + dx, y + dy)

rPosDiff :: RPosition -> RPosition -> RPositionShift
rPosDiff (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

calcShiftPerSec :: RPosition        -- current RPos
                -> RPosition        -- target RPos
                -> Float            -- lefted time
                -> RPositionShift
calcShiftPerSec currentP targetP leftedTime = (xShift / leftedTime, yShift / leftedTime) 
   where (xShift, yShift) = rPosDiff targetP currentP

combineShiftWithCameraShift :: RPositionShift -- main shift
              -> RPositionShift -- shift to update
              -> RPositionShift -- updated shift
combineShiftWithCameraShift (cameraShiftX, cameraShiftY) (shiftX, shiftY) = (shiftX - cameraShiftX, shiftY - cameraShiftY)





data Camera = Camera { _rPosition   :: RPosition        -- center of screen relatively to (0,0) mapPos
                     , _cellSize    :: Int              -- size of cell in pixels
                     , _shiftPerSec :: RPositionShift   -- camera movement
                     }
makeFieldsNoPrefix ''Camera


data ThingR = ThingR { _rPosition   :: RPosition       -- Position on screen (0, 0) center of the screen
                     , _shiftPerSec :: RPositionShift  -- Shift of position per second
                     --, _layer       :: ????
                     , _onScreen :: Bool
                     } deriving Show
makeFieldsNoPrefix ''ThingR