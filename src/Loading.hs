module Loading where

import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Control.Lens


import GameData
import RunTimeData
import EntityObjectLoading
import EntityObjectSpec

initialWorld :: IO World 
initialWorld = loadAllData >>= \initWorld -> return $ World playerStart testLevel [] initWorld 0 initialWorldState

initialWorldState :: WorldState
initialWorldState = WorldState turnTime False True

testLevel = testLevel' `seq` testLevel'

testLevel' :: Map
testLevel' = Map levelMap ents objs 10 10
   where levelMap = list2map myMap
         ents = Vector.fromList [Entity 0 (EntityStatus (0, 0)) []]
         objs =  Vector.fromList []

playerStart :: Player
playerStart = Player $ Entity 0 (EntityStatus (0, 0)) []

myMap = [[[EntityID 0], [], [], [], [], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], []]]

-- to MapLoading module

list2map :: [[[ThingID]]] -> Vector (Vector [ThingID])
list2map = (fmap Vector.fromList) . Vector.fromList