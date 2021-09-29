module Time2Update where

import Control.Monad (when, unless)
import Control.Lens

import GameData
import RunTimeData
import Logic (execPlans)


time2Update :: Float -> World -> IO World
time2Update _ w = do
   let nextTickW = w & tick +~ 1
   
   return $ turnTicksUpdate nextTickW
   

turnTicksUpdate :: World -> World
turnTicksUpdate w = 
      case ticksLeft of
           0 -> zeroTicksUpdate w
           _ -> w & worldState.leftedTurnTicks -~ 1
   where ticksLeft = w ^. worldState.leftedTurnTicks

zeroTicksUpdate :: World -> World         
zeroTicksUpdate w | playerMoveNotReady && eTimeLeft = extraTimeUseW
                  | otherwise = execPlans nextTurnW
   where playerMoveNotReady = not $ w ^. worldState.playerMadeMove
         nextTurnW = w & worldState.playerMadeMove .~ False
                       & worldState.extraTimeLeft .~ True
                       & worldState.leftedTurnTicks .~ turnTime
         
         eTimeLeft = w ^. worldState.extraTimeLeft
         extraTimeUseW = w & worldState.extraTimeLeft .~ False
                           & worldState.leftedTurnTicks .~ additionalTurnTime
         

