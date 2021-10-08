module Time2Update (time2Update) where

import Control.Lens
import GHC.Float (int2Float)

import DataFunctions
import Logic
import ScreenUpdate
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
      let nextTickW = w & tick +~ 1
                        & worldState.leftedPhaseTicks -~ 1

      case ticksLeft of
           0 -> return $ timeUpdate timeToProceed . zeroTicksUpdate $ nextTickW
           _ -> return $ timeUpdate timeToProceed nextTickW
   
   where ticksLeft = w ^. worldState.leftedPhaseTicks

-- | Case of next Phase
zeroTicksUpdate :: World -> World         
zeroTicksUpdate w =
      case nextState of
           BeforeMoves      -> updateShiftings turnTimeLeft . applyHitsBeforeMoves $ nextPhaseW
           Moves            -> updateShiftings turnTimeLeft . updateOnScreenStatuses . applyMoves $ nextPhaseW
           AfterMoves       -> updateShiftings turnTimeLeft . resetActions . applyHitsAfterMoves $ nextPhaseW
           PlayerThinkTime1 -> updateShiftings turnTimeLeft $ nextPhaseW
           PlayerThinkTime2 -> updateShiftings turnTimeLeft $ nextPhaseW
           
           
           
                  
   where currentState = w ^. worldState.state
         
         nextState | (currentState == PlayerThinkTime1) 
                                    && playerMoveReady = BeforeMoves 
                   | otherwise                         = safeSucc currentState
         
         playerMoveReady = w ^. worldState.playerMadeMove
         
         nextPhaseW = updatePlayerThinkPhase $ w & worldState.state .~ nextState
                                                 & worldState.leftedPhaseTicks .~ phaseTicks
         
         updatePlayerThinkPhase w = if nextState == BeforeMoves
                                       then w & worldState.playerMadeMove .~ False
                                       else w
         
         turnTimeLeft = timeLeftedForMove nextState
         phaseTicks = truncate $ (getStateTime nextState) * (int2Float $ w ^. ui.tps)