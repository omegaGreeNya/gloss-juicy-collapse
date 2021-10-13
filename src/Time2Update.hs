module Time2Update (time2Update) where

import Control.Lens
import GHC.Float (int2Float)
import qualified Data.Set as Set (null)

import DataFunctions
import Logic
import ScreenUpdate
import Input (pressedKeys2PlannedActions)
-- | 1. Apply hits before moves
-- | 2. Apply Moves
-- | 3. Apply hits After moves
-- | 4. Player 1st think time
-- | 5. Player 2st think time
-- | repeat

-- | If phase is ends, world proceed to next one and update ShiftPerSecs.
-- | On each tick rPositions changes by time*shiftPerSec


time2Update :: Float -> World -> IO World
time2Update timeToProceed w = do
      let nextTickW = pressedKeys2PlannedActions $
                      w & tick +~ 1
                        & worldState.leftedPhaseTicks -~ 1
                        & worldState.leftedPhaseTime  -~ timeToProceed
                        & worldState.leftedTurnTime   -~ timeToProceed
                        

      case ticksLeft of
           0 -> return $ timeUpdate timeToProceed . zeroTicksUpdate $ nextTickW
           _ -> return $ timeUpdate timeToProceed nextTickW
   
   where ticksLeft = w ^. worldState.leftedPhaseTicks

-- | Case of next Phase
-- Reduce updateShiftings calls?
zeroTicksUpdate :: World -> World         
zeroTicksUpdate w =
      case nextState of
           BeforeMoves      -> updateShiftings turnTimeLeft . applyHitsBeforeMoves $ nextPhaseW
           Moves            -> updateShiftings turnTimeLeft . updateOnScreenStatuses . applyMoves $ nextPhaseW
           AfterMoves       -> updateShiftings turnTimeLeft . resetActions . applyHitsAfterMoves $ nextPhaseW
           PlayerThinkTime1 -> updateShiftings turnTimeLeft $ nextPhaseW
           PlayerThinkTime2 -> updateShiftings turnTimeLeft $ nextPhaseW
           
           
           
                  
   where currentState = w ^. worldState.phase
         
         nextState | (currentState == PlayerThinkTime1) 
                                    && playerMoveReady = BeforeMoves 
                   | otherwise                         = safeSucc currentState
         
         playerMoveReady = not . Set.null $ w ^. player.pressedKeys
         
         nextPhaseW = updatePlayerThinkPhase $ w & worldState.phase            .~ nextState
                                                 & worldState.leftedPhaseTicks .~ phaseTicks
                                                 & worldState.leftedTurnTime   .~ turnTimeLeft
                                                 & worldState.leftedPhaseTime  .~ phaseTimeLeft
         
         updatePlayerThinkPhase w = if nextState == BeforeMoves
                                       then w & worldState.playerMadeMove .~ False
                                       else w
         
         turnTimeLeft  = timeLeftedForTurn nextState w
         phaseTimeLeft = getPhaseTime nextState w
         phaseTicks = truncate $ phaseTimeLeft * (int2Float $ w ^. ui.tps)