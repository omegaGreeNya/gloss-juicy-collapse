{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module RunTimeData where

import GHC.Float (int2Float)


import Control.Lens



data ThingID = EntityID Int
             | ObjectID Int
             deriving Show

type Tick = Int

type Position = (Int, Int)

data EntityStatus = EntityStatus { --_entityName  :: Text
                                 --, _entityHP    :: Int
                                 --, _entityState :: EntityState
                                  _position   :: Position
                                 } deriving Show
makeFieldsNoPrefix ''EntityStatus

data ObjectStatus = ObjectStatus { --_objectName  :: Text
                                 --, _objectHP    :: Int
                                 --, _objectState :: ObjectState
                                  _position   :: Position
                                 } deriving Show
makeFieldsNoPrefix ''ObjectStatus

fps = 60

turnTime :: Int
turnTime  = fps `div` turnTime' -- 0.5 seconds
turnTime' = 2

additionalTurnTime :: Int
additionalTurnTime  = fps * additionalTurnTime' -- 2 second
additionalTurnTime' = 2