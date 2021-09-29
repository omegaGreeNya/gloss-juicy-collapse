{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ActionsData where

import Control.Lens

import RunTimeData


type UpdateEntity = EntityStatus -> EntityStatus

type UpdateObject = ObjectStatus -> ObjectStatus

data Action = Action { _targetsIDs     :: ThingID      -- Whith who?
                     , _actionOnEntity :: UpdateEntity -- Do what with ent?
                     , _actionOnObject :: UpdateObject -- Do what with obj?
                     } 
makeLenses ''Action

--type ActSpeed = Int --How fast action are?

type Actions = [Action] -- ActSpeed?

-- EXAMPLE ACTION CONSTRUCTOR
{-
move :: Int -> Position -> Action
move eID newPos = Action eID (\es -> es & position .~ newPos)
-}