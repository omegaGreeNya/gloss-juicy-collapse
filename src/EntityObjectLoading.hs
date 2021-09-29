module EntityObjectLoading where

import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Control.Lens

import EntityObjectSpec
import PictureLoading



loadAllData :: AllData
loadAllData = loadAllData'

loadAllData' :: AllData
loadAllData' = AllData (loadEntities entities2Load) (loadObjects objects2Load)

loadEntities :: EntitiesSpec -> EntitiesData
loadEntities esSpec = Vector.fromList $ map eSpec2Data esSpec
   where
         eSpec2Data :: EntitySpec -> EntityData
         eSpec2Data eSpec = EntityData eStats ePic
            where
               eStats = eSpec ^. stats
               ePic = loadPicPNG' ePicPath
               
               ePicPath = eSpec ^. picPath

loadObjects :: ObjectsSpec -> ObjectsData
loadObjects osSpec = Vector.fromList $ map oSpec2Data osSpec
   where
         oSpec2Data :: ObjectSpec -> ObjectData
         oSpec2Data oSpec = ObjectData oStats oPic
            where
               oPic = loadPicPNG' oPicPath
               
               oStats = oSpec ^. stats
               oPicPath = oSpec ^. picPath