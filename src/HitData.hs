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
makeFieldsNoPrefix ''HitBox

type PlannedAttack = (HitBox, HitBox) -- hit before moving and after