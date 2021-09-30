module Logic where

import Control.Lens

import GameData
import RunTimeData
import ActionsData

execPlans :: World -> World
execPlans w = processPlans w plans
   where es = w ^. level.levelEntities
         plans = es ^.. traverse.plannedActions
         
         processPlans w [] = w
         processPlans w (p:ps) = processPlans (applyPlan w p) ps
   
   
-- Much better looks with 'withID' class.
applyPlan :: World -> Actions -> World
applyPlan w [] = w
applyPlan w ((Action tID eAct oAct):acts) = applyPlan (wUpdate tID) acts
   where wUpdate (EntityID _) = w & withIDStatus tID %~ eAct 
         wUpdate (ObjectID _) = w & withIDStatus tID %~ oAct
         