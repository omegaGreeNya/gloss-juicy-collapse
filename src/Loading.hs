module Loading where

import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, empty)
import Control.Lens


import DataFunctions
import EntityObjectLoading

initialWorld :: IO World 
initialWorld = loadAllData >>= \thingsData -> return $ World playerStart testLevel [] thingsData 0 initialWorldState initialUI initialCamera

initialWorldState :: WorldState
initialWorldState = WorldState 1 1000 1000 False PlayerThinkTime2 normalTemp

testLevel :: Map
testLevel = Map levelMap ents objs (100,100)
   where levelMap = list2map myMap
         ents = Vector.fromList [Entity (EntityDataID 0) (EntityStatus 100 (0, 0)) noneAct noneThingR
                                ,Entity (EntityDataID 1) (EntityStatus 100 (1, 1)) noneAct noneThingR]
         objs =  Vector.fromList [Object (ObjectDataID 0) (ObjectStatus 100 (3, 1)) noneThingR
                                 ,Object (ObjectDataID 0) (ObjectStatus 100 (2, 2)) noneThingR
                                 ,Object (ObjectDataID 0) (ObjectStatus 100 (3, 2)) noneThingR
                                 ,Object (ObjectDataID 0) (ObjectStatus 100 (4, 2)) noneThingR
                                 ,Object (ObjectDataID 0) (ObjectStatus 100 (3, 3)) noneThingR
                                 ,Object (ObjectDataID 0) (ObjectStatus 100 (2, 4)) noneThingR
                                 ,Object (ObjectDataID 0) (ObjectStatus 100 (4, 4)) noneThingR
                                 ,Object (ObjectDataID 0) (ObjectStatus 100 (9, 9)) noneThingR
                                 ]

noneThingR :: ThingR
noneThingR = ThingR (25, 25) (0, 0) True

playerStart :: Player
playerStart = Player (EntityID 0) Set.empty


myMap = [[[EntityID 0], [], [], [], [], [], [], [], [], []]
        ,[[], [EntityID 1], [], [ObjectID 0], [], [], [], [], [], []]
        ,[[], [], [ObjectID 1], [ObjectID 2], [ObjectID 3], [], [], [], [], []]
        ,[[], [], [], [ObjectID 4], [], [], [], [], [], []]
        ,[[], [], [ObjectID 5], [], [ObjectID 6], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], []]
        ,[[], [], [], [], [], [], [], [], [], [ObjectID 7]]]


-- to MapLoading module

list2map :: [[[ThingID]]] -> Vector (Vector (Set ThingID))
list2map = (fmap (fmap Set.fromList)) . (fmap Vector.fromList) . Vector.fromList

initialUI :: UI
initialUI = UI (800, 600) 60

initialCamera :: Camera
initialCamera = Camera (400, -300) 64 (0, 0) 1