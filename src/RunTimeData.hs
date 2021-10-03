{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module RunTimeData where

import GHC.Float (int2Float, int2Double)


import Control.Lens



data ThingID = EntityID Int
             | ObjectID Int
             deriving Show

instance Eq ThingID where
   (EntityID x) == (EntityID y) = x == y
   (ObjectID x) == (ObjectID y) = x == y
   _            == _            = False

instance Ord ThingID where
   (EntityID _) <= (ObjectID _) = False
   (ObjectID _) <= (EntityID _) = True
   (ObjectID x) <= (ObjectID y) = x <= y
   (EntityID x) <= (EntityID y) = x <= y

type Tick = Int

type Position = (Int, Int) -- Point

type PosShift = (Int, Int) -- Vector

type Direction = (Int, Int) -- (x, y) }-> abs x, abs y = [0, 1]

type Scale = Double

shift2Dir :: PosShift -> Direction
shift2Dir (x, y) = (signum x, signum y)

mShift2Dir :: Maybe PosShift -> Direction
mShift2Dir Nothing = (0, 0)
mShift2Dir (Just shift) = shift2Dir shift

-- | flip dirScale == dirScale
dirScale :: Direction -- hit Direction
         -> Direction -- target Direction
         -> Scale
dirScale (0,  hy) (0,  ty) = int2Double . abs $ hy - ty
dirScale (hx, 0 ) (tx, 0 ) = int2Double . abs $ hx - tx
dirScale (hx, hy) (tx, ty) = (/2) . int2Double $ (abs $ hx - tx) + (abs $ hy - ty)

dir2Plan :: Direction
         -> Int        -- result*45 = angle degree
dir2Plan (1 , 0 ) = 0
dir2Plan (1 , 1) = 1
dir2Plan (0 , 1) = 2
dir2Plan (-1 , 1) = 3
dir2Plan (-1, 0) = 4
dir2Plan (-1, -1) = 5
dir2Plan (0 , -1) = 6
dir2Plan (1 , -1) = 7
dir2Plan _ = 0

data EntityStatus = EntityStatus { --_entityName  :: Text
                                   _hp    :: Double
                                 --, _entityState :: EntityState
                                 , _position   :: Position
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