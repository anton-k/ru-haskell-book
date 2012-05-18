module World where

import qualified Physics.Hipmunk as H
import qualified Graphics.UI.GLFW as G

import Data.StateVar

import Control.Monad
import Data.Maybe
import Types
import Utils   

import qualified IxMap as M

import Pure
import Dirty
import Inits
import Graphics

updateWorld :: World -> IO (World, Time)
updateWorld world = do
    t0 <- get G.time
    (sense, events) <- percept dirty
    let (pure', queries) = updatePure sense events pure
    dirty' <- react queries dirty    
    t1 <- get G.time
    return (World pure' dirty', t1 - t0)
    where dirty = worldDirty world
          pure  = worldPure  world  

percept :: Dirty -> IO (Sense, [Event])
percept a = do
    hero    <- obj2hero $ dirtyHero a
    balls   <- mapM (uncurry obj2ball) $ M.toList $ dirtyObjs a
    evts1   <- fmap maybeToList $ getTouch (dirtyTouchVar a) $ dirtyObjs a
    evts2   <- fmap maybeToList $ getClick $ dirtyMouse a
    return $ (Sense hero balls, evts1 ++ evts2)


updatePure :: Sense -> [Event] -> Pure -> (Pure, [Query])
updatePure s evts = updateEvents evts . updateSenses s 


react :: [Query] -> Dirty -> IO Dirty
react = foldr (<=<) return   
    . fmap (foldQuery removeBall heroVelocity 
            makeBall gameOver (stepDirty dt))


drawWorld :: World -> IO ()
drawWorld = draw . picture . worldPure


initWorld :: IO World
initWorld = do
    dirty   <- initDirty
    (sense, events) <- percept dirty
    return $ World (initPure sense events) dirty
