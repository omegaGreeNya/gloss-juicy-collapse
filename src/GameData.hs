{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module GameData where

import Data.Text (Text)
--import Graphics.Gloss.Data.Picture
import Control.Lens
import Data.Vector (Vector)

import EntityObjectSpec
import RunTimeData
import ActionsData

data Entity = Entity { _dataID         :: Int
                     , _status         :: EntityStatus
                     , _plannedActions :: Actions
                     } 
makeFieldsNoPrefix ''Entity

data Object = Object { _dataID :: Int
                     , _status :: ObjectStatus
                     } 
makeFieldsNoPrefix ''Object

data Map = Map { _thingsMap     :: Vector (Vector [ThingID])  -- 2D array of !this level! things IDs
               , _levelEntities :: Vector Entity              -- AI/Player specIDs and statuses
               , _levelObjects  :: Vector Object              -- Walls/Floors/etc. - everything unactionable
               , _xSize         :: Int
               , _ySize         :: Int
               } 
makeLenses ''Map

data Player = Player { _playerEntity :: Entity
                     } 
makeLenses ''Player

data WorldState = WorldState { _leftedTurnTicks :: Tick
                             , _playerMadeMove  :: Bool
                             , _extraTimeLeft   :: Bool
                             }
makeLenses ''WorldState

data World = World { _player :: Player
                   , _level    :: Map
                   , _levelSet :: [Map]
                   , _allData :: AllData
                   , _tick :: Tick
                   , _worldState :: WorldState
                   } 
makeLenses ''World

-- Separate below???

xCell :: Int
xCell = 100
yCell :: Int
yCell = 100

xWindow :: Int
xWindow = 800
yWindow :: Int
yWindow = 600
