module Loading where

import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Data.Set (Set)
import qualified Data.Set as Set (fromList)
import Control.Lens


import DataFunctions
import EntityObjectLoading

initialWorld :: IO World 
initialWorld = loadAllData >>= \thingsData -> return $ World playerStart testLevel [] thingsData 0 initialWorldState initialUI initialCamera

initialWorldState :: WorldState
initialWorldState = WorldState 1 False PlayerThinkTime2

testLevel :: Map
testLevel = Map levelMap ents objs (10,10)
   where levelMap = list2map myMap
         ents = Vector.fromList [Entity (EntityDataID 0) (EntityStatus 100 (0, 0)) noneAct noneThingR
                                ,Entity (EntityDataID 1) (EntityStatus 100 (1, 1)) noneAct noneThingR]
         objs =  Vector.fromList []

noneThingR :: ThingR
noneThingR = ThingR (25, 25) (0, 0) True

playerStart :: Player
playerStart = Player (EntityID 0)

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

initialUI :: UI
initialUI = UI (800, 600) 60

initialCamera :: Camera
initialCamera = Camera (400, -300) 50 (0, 0)