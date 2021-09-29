module PictureLoading where

import Graphics.Gloss.Juicy
import Graphics.Gloss.Data.Picture (Picture(..))
import Data.Maybe (fromJust, isJust)
import System.IO.Unsafe (unsafePerformIO)

missingPic :: IO Picture
missingPic = fmap fromJust $ loadJuicyPNG "data/Tiles/missing.png"

loadPicPNG' :: FilePath -> Picture
loadPicPNG' path = unsafePerformIO $ loadPicPNG path

loadPicPNG :: FilePath -> IO Picture
loadPicPNG path = checkmiss $ loadJuicyPNG path
   where checkmiss mPic = do
            picFound <- fmap isJust mPic
            if picFound then fmap fromJust mPic
                        else missingPic