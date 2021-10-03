{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HitData where

import Control.Lens

import RunTimeData (Position, PosShift, Scale, Direction)


-- For now for damage calculation used only fst number
type DamageRNG = (Int, Int)

data HitCell = HitCell { _pos   :: Position
                       , _dir   :: Direction
                       , _scale :: Scale
                       }

data HitBox = SimpleHitBox { _pos1 :: Position
                           , _pos2 :: Position
                           , _dmgRNG :: DamageRNG
                           }
            | CellBox { _cells :: [HitCell]
                      , _dmgRNG :: DamageRNG
                      }
            | None
            -- | I need to find a proper way of making hitbox scaling with directions
            {-
            HitBox { _pos1       :: Position  -- \
                     , _pos2       :: Position  -- |> hit zone
                     , _sourceVec  :: PosDir    -- hit derection norm. vector
                     , _shiftScale :: Int       -- how much position shift affects damage (1 = 25%) (0 - doesn't affect) (-1 = -25% ?)
                     , _dmgRNG   :: damageRNG -- damage interval
                     }
            -}
makeFieldsNoPrefix ''HitBox

type PlannedAttack = (HitBox, HitBox) -- hit before moving and after

{-
-- | Damage scales from fighters move.
damageCalc :: PosDir     -- Attack direction
           -> PosShift   -- defender move vector
           -> damageRNG 
           -> Int        -- resulting damage
damageCalc aShift dShidt dmgInterval = round $ scale * dmg
   where scale = dmgScale
         dmg = int2Float $ fst dmgInterval
   
dmgScale :: PosDir   -- Attack direction
         -> PosShift -- Defender shift
         -> Double
         -> Double
dmgScale aDir@(x1, y1) dShift@(x2, y2) scale = scale * 
   where resVec = shiftDiff aShift dShift
         
         shiftDiff (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)
         
         normalizedAShift = 
-}