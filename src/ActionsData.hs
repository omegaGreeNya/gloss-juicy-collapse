{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ActionsData where

import Control.Lens

import RunTimeData
import HitData

data Action = Action { _move    :: Maybe PosShift
                     , _attacks :: [PlannedAttack] 
                     }
makeLenses ''Action

moveByShift :: Position -> PosShift -> Position
moveByShift (x, y) (x', y') = (x + x', y + y')

noneAct :: Action
noneAct = Action Nothing []