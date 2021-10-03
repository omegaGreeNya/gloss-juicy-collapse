-- | Probably, there is a bug with processZone!!!!!!!!!!!!!!!!!!!!!

module Logic where

import Control.Lens
import Data.Foldable (foldl')
import Data.Vector (Vector)
import qualified Data.Vector as Vector (length)
import qualified Data.Set as Set (delete, insert)
import Data.Maybe (fromJust)
import GHC.Float (int2Double)

import GameData
import RunTimeData
import ActionsData
import HitData

execPlans :: World -> World
execPlans = resetActions . applyHitsAfterMoves . applyMoves . applyHitsBeforeMoves
   
   
resetActions :: World -> World
resetActions w = w & level.levelEntities.traversed.plannedActions .~ noneAct

applyHitsBeforeMoves :: World -> World
applyHitsBeforeMoves w = foldl' applyHitBox w hitBoxes
   where hitBoxes = w ^.. level.levelEntities.folded.plannedActions.attacks.folded._1

applyMoves :: World -> World
applyMoves w = applyMoves' w 0 entNumber
   where entVec = w ^. level.levelEntities
         entNumber = Vector.length entVec

applyHitsAfterMoves :: World -> World
applyHitsAfterMoves w = foldl' applyHitBox w hitBoxes
   where hitBoxes = w ^.. level.levelEntities.folded.plannedActions.attacks.folded._2


applyMoves' :: World -> Int -> Int -> World
applyMoves' w i maxi | i == maxi = newW 
                     | otherwise = applyMoves' newW (i+1) maxi
   where maybeE = w ^? level.levelEntities.ix i
         e = fromJust maybeE
         
         maybeMove = e ^. plannedActions.move
         (x, y) = e ^. status.position
         
         applyMaybeMove (Nothing)    = e
         applyMaybeMove (Just shift) = e & status.position %~ moveByShift shift
         
         movedE = applyMaybeMove maybeMove
         (x', y') = movedE ^. status.position
         
         newW' (Nothing) = w 
         newW' (Just e)  | x == x' && y == y' = w
                         | otherwise          = w & level.thingsMap.ix x .ix y  %~ Set.delete (EntityID i)
                                                  & level.thingsMap.ix x'.ix y' %~ Set.insert (EntityID i)
                                                  & level.levelEntities.ix i .~ movedE
         
         newW = newW' maybeE

applyMove :: Entity -> Entity
applyMove e = applyMaybeMove maybeMove
   where maybeMove = e ^. plannedActions.move
         applyMaybeMove (Nothing)    = e
         applyMaybeMove (Just shift) = e & status.position %~ moveByShift shift
         
         

-- | For now just for Entities
--   HITBOX POSITION SHOULD BE NORMALIZED 
--   (e.g. (x1,y1) and (x2,y2) |-> x1 <= x2 && y1 <= y2)
applyHitBox :: World -> HitBox -> World
applyHitBox w None = w
applyHitBox w (SimpleHitBox p1 p2 dmgRNG) = applySimpleHitBox w p1 p2 dmgRNG
applyHitBox w (CellBox hcs dmgRNG) = applyCellBox w hcs dmgRNG

applySimpleHitBox :: World -> Position -> Position -> DamageRNG -> World
applySimpleHitBox w p1 p2 dmgRNG = processZone w p1 p2 hitAction
   where hitAction w tID@(EntityID _) = w & unsafeEntityByID tID.status %~ eDmg
         hitAction w _                = w
         eDmg :: EntityStatus -> EntityStatus
         eDmg es = es & hp -~ (getRNG dmgRNG)
-- | Im feeling bad about using withIDStatus above :( it's too ambiguous

applyCellBox :: Foldable t => World -> t HitCell -> DamageRNG -> World
applyCellBox w hcs dmgRNG = foldl' (applyHitCell dmgRNG) w hcs

applyHitCell :: DamageRNG -> World -> HitCell -> World
applyHitCell dmgRNG w (HitCell (hitX, hitY) hDir scale) = foldl' hitAction w targetsIDs
   where targetsIDs = w ^. level.thingsMap.ix hitX.ix hitY
         
         dmg = getRNG dmgRNG
         
         shiftDir :: Entity -> Direction
         shiftDir e = mShift2Dir $ e ^. plannedActions.move
         resultDirScale e = dirScale hDir (shiftDir e)
         finalDmg e = (1 + resultDirScale e) * scale * dmg                                    -- HIT SCALE FORMULA (sepparate to another module?)
         
         eDmg :: Entity -> Entity
         eDmg e = e & status.hp -~ finalDmg e
         
         hitAction w tID@(EntityID _) = w & unsafeEntityByID tID %~ eDmg
         hitAction w _ = w
         
   

processZone :: World                       -- World to apply
            -> Position                    -- Left upper corner of zone
            -> Position                    -- Max x and y
            -> (World -> ThingID -> World) -- updating world on ThingID
            -> World                       -- result of applying actions on every ThingID in zone
processZone w (xmin, ymin) (xmax, ymax) action = processZone' w xmin ymin xmin ymin xmax ymax action

processZone' :: World                       -- World to apply
             -> Int                         -- Current width pos (x)
             -> Int                         -- Current height pos (y)
             -> Int                         -- min x
             -> Int                         -- min y
             -> Int                         -- max x
             -> Int                         -- max y
             -> (World -> ThingID -> World) -- updating world on ThingID
             -> World                       -- result of applying actions on every ThingID in zone
processZone' w x y xmin ymin xmax ymax action | x < xmax = processZone' newW (x + 1) y xmin ymin xmax ymax action
                                              | y < ymax = processZone' newW xmin (y + 1) xmin ymin xmax ymax action
                                              | otherwise = newW
   where newW = foldl' action w targetThingsIDs
         targetThingsIDs = w ^. level.thingsMap.ix x.ix y

getRNG :: DamageRNG -> Double
getRNG (x, _) = int2Double x