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
   
   
-- GOVNOKOD ALLERT
applyPlan :: World -> Actions -> World
applyPlan w [] = w
applyPlan w ((Action (EntityID x) eAct _):acts) = applyPlan (w & targetLens %~ eAct) acts
   where targetLens = level.levelEntities.ix x.status

applyPlan w ((Action (ObjectID x) _ oAct):acts) = applyPlan (w & targetLens %~ oAct) acts
   where targetLens = level.levelObjects.ix x.status
         