module Dirty where

import System.IO
import Data.Word

import Control.Applicative

import Data.List
import Control.Monad.IO.Class
import Data.IORef
import Data.StateVar

import qualified Physics.Hipmunk as H
import qualified Graphics.UI.GLFW as G
import qualified Graphics.Rendering.OpenGL as G
import Types
import Utils
import Inits

import qualified IxMap as M

import qualified Control.Monad.Random as R
import System.Random

stepDirty :: H.Time -> Dirty -> IO Dirty
stepDirty dt d = H.step (dirtySpace d) dt >> return d


obj2hero    :: Obj -> IO HeroBall
obj2ball    :: Id -> Obj -> IO Ball

obj2hero a  = do
    p <- get $ objPos a
    v <- get $ objVel a
    return $ HeroBall 
        { heroPos   = p
        , heroVel   = H.len v }

obj2ball i a = do
    p <- get $ objPos a
    return $ Ball 
        { ballId    = i
        , ballPos   = p
        , ballType  = objType a } 


objPos = H.position . objBody
objVel = H.velocity . objBody

getTouch    :: Sensor H.Shape -> M.IxMap Obj -> IO (Maybe Event)
getTouch touch objs = do
    val <- getSensor touch 
    maybe (return Nothing) handleJust val
    where hasShape s obj = s == objShape obj
          handleJust :: H.Shape -> IO (Maybe Event)
          handleJust x = maybe (return Nothing) 
                    (fmap (Just . Touch) . uncurry obj2ball)
                    $ find (hasShape x . snd) (M.toList objs)  

getClick    :: Sensor H.Position -> IO (Maybe Event)
getClick = fmap (fmap UserClick) . getSensor

getSensor :: Sensor a -> IO (Maybe a)
getSensor ref = do
    val <- get ref
    maybe (return Nothing) handleJust val
    where handleJust a = do
                ref $= Nothing
                return $ Just a


removeBall      :: Ball         -> Dirty -> IO Dirty
heroVelocity    :: H.Velocity   -> Dirty -> IO Dirty
makeBall        :: Freq         -> Dirty -> IO Dirty
gameOver        ::                 Dirty -> IO Dirty 

----------------------------

gameOver = const $ initDirty

removeBall b d = do
    
    H.spaceRemove space $ objBody obj
    H.spaceRemove space $ objShape obj
    return $ d{ dirtyObjs = M.delete k $ dirtyObjs d }
    where k     = ballId b
          obj   = dirtyObjs d M.! k
          space = dirtySpace d

heroVelocity vel a = ((objVel $ dirtyHero a) $= vel) >> return a

makeBall f a = do 
    obj <- genBall f (dirtySpace a)
    return $ a{ dirtyObjs = fst $ M.insert obj (dirtyObjs a) }

genBall :: Freq -> H.Space -> IO Obj
genBall f space = do
    bType   <- genBallType f
    (p, v)  <- genBallState
    ball    <- initObj bType p v space
    return ball
 


genBallType :: Freq -> IO BallType
genBallType f = R.evalRandIO $ R.fromList $ [
            (Good,  toRational $ freqGood f), 
            (Bad,   toRational $ freqBad f),
            (Bonus, toRational $ freqBonus f)]


genBallState :: IO (H.Position, H.Vector)
genBallState = do
    n   <- randomRIO (0, 5)
    dx  <- randomRIO (-1.0, 1.0)
    dy  <- randomRIO (-1.0, 1.0)
    dv  <- genBallVelocity 

    let p = holePoints !! n
    return (p, vel (getAngle dx dy p) dv)
    where vel a len = H.scale a len

          getAngle dx dy p = H.normalize $ (vec h2 w2) - p
            where w2 = dx * frameWidth/3
                  h2 = dy * frameHeight/3  


genBallVelocity :: IO H.CpFloat
genBallVelocity = randomRIO (minBallVel, maxBallVel)
    

------------------------------------------------------------
-- init

initDirty :: IO Dirty
initDirty = do
    space       <- initSpace
    hero        <- initHero space
    touchVar    <- initSensor (touchCallback space)
    mouseVar    <- initSensor mouseCallback

    return $ Dirty 
            { dirtyHero     = hero
            , dirtyObjs     = M.empty
            , dirtySpace    = space
            , dirtyTouchVar = touchVar 
            , dirtyMouse    = mouseVar
            }

initSensor :: (Sensor a -> IO ()) -> IO (Sensor a)
initSensor callback = do
    var <- newIORef Nothing
    callback var
    return var


initSpace :: IO H.Space
initSpace = do    
    space <- H.newSpace
    initWalls space
    return space

touchCallback :: H.Space -> Sensor H.Shape -> IO ()
touchCallback space touchVar = do
    H.addCollisionHandler space 
        (toCollisionType Hero) (toCollisionType Good) handler
    where handler = H.Handler Nothing Nothing (Just handle) Nothing

          handle = do
            (a, b) <- H.shapes
            liftIO $ touchVar $= (Just b)


mouseCallback :: Sensor H.Position -> IO ()
mouseCallback sensor = do      
    G.mouseButtonCallback $= (onMouse (updatePos sensor))
    where updatePos sensor =  do
                pos <- get G.mousePos
                size <- get G.windowSize
                sensor $= Just (mouse2canvas size pos)

onMouse act key keyState = case (key, keyState) of
    (G.ButtonLeft, G.Press)  -> act
    _                        -> return ()


mouse2canvas :: G.Size -> G.Position -> H.Vector
mouse2canvas (G.Size sx sy) (G.Position mx my) = H.Vector x y
    where d a b  = fromIntegral a / fromIntegral b
          x  = windowWidth * (d mx sx - 0.5)
          y  = windowHeight * (negate $ d my sy - 0.5)


initHero :: H.Space -> IO Obj
initHero = initObj Hero (vec 0 0) (vec 0 0)

initObj :: BallType -> H.Position -> H.Vector -> H.Space -> IO Obj
initObj ballType pos vel space = do
    b <- H.newBody mass $ H.momentForCircle mass (0, radius) 0
    s <- H.newShape b (H.Circle radius) 0
    H.position b $= pos
    H.velocity b $= vel
    
    H.elasticity s $= 0.9999
    H.collisionType s $= toCollisionType ballType

    H.spaceAdd space b 
    H.spaceAdd space s

    return $ (Obj ballType s b)
    where mass      = 20
          radius    = ballRadius


toCollisionType :: BallType -> Word32
toCollisionType a = case a of
    Hero    -> 1
    _       -> 2


initWalls :: H.Space -> IO ()
initWalls space = mapM_ f wallPoints
    where f (a, b) = initWall a b space


initWall :: H.Position -> H.Position -> H.Space -> IO ()
initWall pa pb space = do    
    b <- H.newBody H.infinity H.infinity
    s <- H.newShape b (H.LineSegment pa pb wallThickness) 0
    H.elasticity s $= 0.9999
    H.spaceAdd space b
    H.spaceAdd space s
    return ()
  

