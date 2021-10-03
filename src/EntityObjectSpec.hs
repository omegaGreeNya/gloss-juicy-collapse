-- | What to load Exactly

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module EntityObjectSpec where

import Graphics.Gloss.Data.Picture (Picture)
import Control.Lens
import Control.Lens.Combinators (makeFieldsNoPrefix)
import Data.Vector (Vector)
import Data.Text (Text)
import Data.IORef

import ActionsData

-- << Ent/Obj description
data EntityStats = EntityStats { _getID       :: Int
                               , _defaultName :: Text
                               , _maxHP       :: Int
                               --, _entityStates      :: States
                               --, _aiID :: Int
                               } deriving Show
makeFieldsNoPrefix ''EntityStats

data ObjectStats = ObjectStats { _getID       :: Int
                               , _defaultName :: Text
                               , _maxHP       :: Int
                               --, _entityStates      :: States
                               } deriving Show
makeFieldsNoPrefix ''ObjectStats

-- >>


-- << Data Loading information
data EntitySpec = EntitySpec { _stats   :: EntityStats
                             , _picPath :: FilePath
                             } deriving Show
makeFieldsNoPrefix ''EntitySpec

data ObjectSpec = ObjectSpec { _stats   :: ObjectStats
                             , _picPath :: FilePath
                             } deriving Show
makeFieldsNoPrefix ''ObjectSpec
-- >>

-- << Fully loaded Data units
data EntityData = EntityData { _stats :: EntityStats
                             , _pic   :: Picture
                             } 
makeFieldsNoPrefix ''EntityData

data ObjectData = ObjectData { _stats :: ObjectStats
                             , _pic   :: Picture
                             } 
makeFieldsNoPrefix ''ObjectData
-- >>

-- << Representation of loading data
type EntitiesSpec = [EntitySpec]

type ObjectsSpec = [ObjectSpec]
-- >>

-- << Representation of loaded data
type EntitiesData = Vector EntityData

type ObjectsData = Vector ObjectData

data AllData = AllData { _entities :: EntitiesData
                       , _objects  :: ObjectsData
                       } 
makeLenses ''AllData
-- >>


-----------------------Loading Info----------------------------

entities2Load :: EntitiesSpec
entities2Load = [playerSpec, dummy]

objects2Load  :: ObjectsSpec
objects2Load  = []


-----------------------Loading Bits----------------------------

playerSpec :: EntitySpec
playerSpec = EntitySpec playerStats playerPic
   where playerStats = EntityStats 0 "Player" 100
         playerPic = "data/tiles/player/extraPain.png"

dummy :: EntitySpec
dummy = EntitySpec playerStats playerPic
   where playerStats = EntityStats 1 "Dummy" 100
         playerPic = "undefined"