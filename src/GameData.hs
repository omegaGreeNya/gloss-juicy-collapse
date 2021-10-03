{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module GameData where

import Data.Text (Text)
--import Graphics.Gloss.Data.Picture
import Control.Lens
import Data.Vector (Vector)
import Data.Maybe (fromJust)
import Data.Set (Set)

import EntityObjectSpec
import RunTimeData
import ActionsData

data Entity = Entity { _dataID         :: Int
                     , _status         :: EntityStatus
                     , _plannedActions :: Action
                     } 
makeFieldsNoPrefix ''Entity

data Object = Object { _dataID :: Int
                     , _status :: ObjectStatus
                     } 
makeFieldsNoPrefix ''Object

data Map = Map { _thingsMap     :: Vector (Vector (Set ThingID))  -- 2D array of !this level! things IDs
               , _levelEntities :: Vector Entity                 -- AI/Player specIDs and statuses
               , _levelObjects  :: Vector Object                 -- Walls/Floors/etc. - everything unactionable
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


-- Class module? Naah..
class WithLevelID b where
   withIDStatus :: ThingID -> Lens' World b

instance WithLevelID EntityStatus where
   withIDStatus :: ThingID -> Lens' World EntityStatus
   withIDStatus (EntityID x) = lens getter setter 
      where getter w = fromJust $ w ^? level.levelEntities.ix x.status
            setter w es = w & level.levelEntities.ix x.status .~ es

instance WithLevelID ObjectStatus where
   withIDStatus :: ThingID -> Lens' World ObjectStatus
   withIDStatus (ObjectID x) = lens getter setter 
      where getter w = fromJust $ w ^? level.levelObjects.ix x.status
            setter w os = w & level.levelObjects.ix x.status .~ os


unsafeEntityByID :: ThingID -> Lens' World Entity
unsafeEntityByID (EntityID x) = lens getter setter
   where getter w = fromJust $ w ^? level.levelEntities.ix x
         setter w e = w & level.levelEntities.ix x .~ e

unsafeObjectByID :: ThingID -> Lens' World Object
unsafeObjectByID (ObjectID x) = lens getter setter
   where getter w = fromJust $ w ^? level.levelObjects.ix x
         setter w o = w & level.levelObjects.ix x .~ o

-- Separate below???

xCell :: Int
xCell = 100
yCell :: Int
yCell = 100

xWindow :: Int
xWindow = 800
yWindow :: Int
yWindow = 600
