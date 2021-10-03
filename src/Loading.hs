module Loading where

import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Data.Set (Set)
import qualified Data.Set as Set (fromList)
import Control.Lens


import GameData
import RunTimeData
import EntityObjectLoading
import EntityObjectSpec
import ActionsData

initialWorld :: IO World 
initialWorld = loadAllData >>= \initWorld -> return $ World playerStart testLevel [] initWorld 0 initialWorldState

initialWorldState :: WorldState
initialWorldState = WorldState turnTime False True

testLevel = testLevel' `seq` testLevel'

testLevel' :: Map
testLevel' = Map levelMap ents objs 10 10
   where levelMap = list2map myMap
         ents = Vector.fromList [Entity 0 (EntityStatus 100 (0, 0)) noneAct
                                ,Entity 1 (EntityStatus 100 (1, 1)) noneAct ]
         objs =  Vector.fromList []

playerStart :: Player
playerStart = Player $ Entity 0 (EntityStatus 100 (0, 0)) noneAct

myMap = [[[EntityID 0], [], [], [], [], [], [], [], [], []]
        ,[[], [EntityID 1], [], [], [], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], []]]

-- to MapLoading module

list2map :: [[[ThingID]]] -> Vector (Vector (Set ThingID))
list2map = (fmap (fmap Set.fromList)) . (fmap Vector.fromList) . Vector.fromList