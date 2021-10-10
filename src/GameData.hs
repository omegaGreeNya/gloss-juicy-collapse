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
import GHC.Float (int2Float)

import EntityObjectSpec
import RunTimeData
import ActionsData
import ScreenData

data Thing = Entity { _dataID         :: DataID
                    , _status         :: Status
                    , plannedActions  :: Action
                    , _renderData     :: ThingR
                    } 
           | Object { _dataID :: DataID
                    , _status :: Status
                    , _renderData :: ThingR
                    } 
makeLenses ''Thing

plannedAct :: Lens' Thing Action
plannedAct = lens getter setter
   where getter t@(Entity _ _ _ _)       = plannedActions t
         getter t@(Object _ _ _)         = noneAct
         setter (Entity dID st _ rD) act = Entity dID st act rD
         setter t@(Object _ _ _) _       = t

data Map = Map { _thingsMap     :: Vector (Vector (Set ThingID))  -- 2D array of !this level! things IDs
               , _levelEntities :: Vector Thing                   -- AI/Player specIDs and statuses
               , _levelObjects  :: Vector Thing                   -- Walls/Floors/etc. - everything unactionable
               , _levelSize     :: (Int, Int)
               }
makeLenses ''Map

data Player = Player { _playerEntityID :: ThingID
                     } 
makeLenses ''Player

data State = BeforeMoves
           | Moves
           | AfterMoves
           | PlayerThinkTime1
           | PlayerThinkTime2
            deriving (Show, Eq, Enum, Bounded)

data WorldState = WorldState { _leftedPhaseTicks :: Tick
                             , _leftedPhaseTime  :: Float
                             , _playerMadeMove   :: Bool
                             , _state            :: State
                             }
makeLenses ''WorldState

data UI = UI { _windowSize :: (Int, Int)
             , _tps        :: Int
             }
makeLenses ''UI

data World = World { _player :: Player
                   , _level    :: Map
                   , _levelSet :: [Map]
                   , _allData :: AllData
                   , _tick :: Tick
                   , _worldState :: WorldState
                   , _ui         :: UI
                   , _camera :: Camera
                   } 
makeLenses ''World