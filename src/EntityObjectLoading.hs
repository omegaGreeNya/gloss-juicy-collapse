module EntityObjectLoading (loadAllData) where

import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Control.Lens

import EntityObjectSpec
import PictureLoading


loadAllData :: IO AllData
loadAllData = AllData <$> (loadEntities entities2Load) <*> (loadObjects objects2Load)

loadEntities :: EntitiesSpec -> IO EntitiesData
loadEntities esSpec = traverse eSpec2Data esSpec >>= return . Vector.fromList 
   where
         eSpec2Data :: ThingSpec -> IO ThingData
         eSpec2Data eSpec = ePic >>= (\ePic -> return $ EntityData eStats ePic)
            where
               eStats = eSpec ^. stat
               ePic = loadPicPNG' ePicPath
               
               ePicPath = eSpec ^. picPath

loadObjects :: ObjectsSpec -> IO ObjectsData
loadObjects osSpec = traverse oSpec2Data osSpec >>= return . Vector.fromList
   where
         oSpec2Data :: ThingSpec -> IO ThingData
         oSpec2Data oSpec = oPic >>= (\oPic -> return $ ObjectData oStats oPic)
            where
               oPic = loadPicPNG' oPicPath
               
               oStats = oSpec ^. stat
               oPicPath = oSpec ^. picPath