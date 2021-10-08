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

data DataID = EntityDataID Int
            | ObjectDataID Int

-- << Ent/Obj description
data ThingStats = EntityStats { _getID       :: Int
                              , _defaultName :: Text
                              , _maxHP       :: Int
                              --, _entityStates      :: States
                              --, _aiID :: Int
                              } 
                | ObjectStats { _getID       :: Int
                              , _defaultName :: Text
                              , _maxHP       :: Int
                              --, _entityStates      :: States
                              }
makeLenses ''ThingStats

-- >>


-- << Data Loading information
data ThingSpec = EntitySpec { stats   :: ThingStats
                            , _picPath :: FilePath
                            }
               | ObjectSpec { stats   :: ThingStats
                            , _picPath :: FilePath
                            }
makeLenses ''ThingSpec
-- >>

-- << Fully loaded Data units
data ThingData = EntityData { stats :: ThingStats
                            , _pic   :: Picture
                            } 
               | ObjectData { stats :: ThingStats
                            , _pic   :: Picture
                            } 
makeLenses ''ThingData
-- >>

-- <<
class HasStats s a | s -> a where 
   stat :: Lens' s a

instance HasStats ThingSpec ThingStats where
   stat = lens getter setter
      where getter (EntitySpec st _) = st
            getter (ObjectSpec st _) = st
            setter (EntitySpec _  p) st = EntitySpec st p
            setter (ObjectSpec _  p) st = ObjectSpec st p

instance HasStats ThingData ThingStats where 
   stat = lens getter setter
      where getter (EntityData st _) = st
            getter (ObjectData st _) = st
            setter (EntityData _  p) st = EntityData st p
            setter (ObjectData _  p) st = ObjectData st p
-- >>

-- << Representation of loading data
type EntitiesSpec = [ThingSpec]

type ObjectsSpec = [ThingSpec]
-- >>

-- << Representation of loaded data
type EntitiesData = Vector ThingData

type ObjectsData = Vector ThingData

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

playerSpec :: ThingSpec
playerSpec = EntitySpec playerStats playerPic
   where playerStats = EntityStats 0 "Player" 100
         playerPic = "data/tiles/player/extraPain.png"

dummy :: ThingSpec
dummy = EntitySpec playerStats playerPic
   where playerStats = EntityStats 1 "Dummy" 100
         playerPic = "undefined"