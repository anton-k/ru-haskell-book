module Main where

import Control.Applicative

import Control.Applicative
import Data.StateVar
import Data.IORef
import Graphics.UI.GLFW
import System.Exit
import Control.Monad

import qualified Physics.Hipmunk  as H
import qualified Graphics.UI.GLFW as G
import qualified Graphics.Rendering.OpenGL as G
  
title = "in the box"

----------------------------
-- inits

type Time = Double

-- frames per second
fps :: Int
fps = 60

-- frame time in milliseconds
frameTime :: Time
frameTime = 1000 * ((1::Double) / fromIntegral fps)


nearOne = 0.9999
ballMass = 20
ballMoment = H.momentForCircle ballMass (0, ballRadius) 0
ballRadius = 10
    
initPos = H.Vector 0 0
initVel = H.Vector 0 0

wallThickness = 1

wallPoints = fmap (uncurry f) [
    ((-ow2, -oh2), (-ow2, oh2)),
    ((-ow2, oh2),  (ow2, oh2)),
    ((ow2, oh2),  (ow2, -oh2)),
    ((ow2, -oh2),   (-ow2, -oh2))]
    where f a b = (g a, g b) 
          g (a, b) = H.Vector a b  

dt :: Double
dt = 0.5

minVel :: Double
minVel = 10

width, height :: Double

height = 500
width = 700

w2, h2 :: Double

h2 = height / 2
w2 = width / 2

ow2, oh2 :: Double
    
ow2 = w2 - 50 
oh2 = h2 - 50  

data State = State 
    { stateBall     :: H.Body
    , stateSpace    :: H.Space
    }

ballPos :: State -> StateVar H.Position
ballPos = H.position . stateBall

ballVel :: State -> StateVar H.Velocity
ballVel = H.velocity . stateBall


main = do
    H.initChipmunk
    initGLFW
    state <- newIORef =<< initState
    loop state



loop :: IORef State -> IO ()
loop state = do    
    display state
    onMouse state
    sleep frameTime
    loop state


simulate :: State -> IO Time
simulate a = do
    t0 <- get G.time
    H.step (stateSpace a) dt 
    t1 <- get G.time
    return (t1 - t0)

initGLFW :: IO ()
initGLFW = do
    G.initialize
    G.openWindow (G.Size (d2gli width) (d2gli height)) [] G.Window
    G.windowTitle $= title
    G.windowCloseCallback $= exitWith ExitSuccess
    G.windowSizeCallback  $= (\size -> G.viewport $= (G.Position 0 0, size))
    G.clearColor $= G.Color4 1 1 1 1
    G.ortho (-dw2) (dw2) (-dh2) (dh2) (-1) 1
    where dw2 = realToFrac w2
          dh2 = realToFrac h2  

initState :: IO State
initState = do
    space <- H.newSpace 
    initWalls space
    ball <- initBall space initPos initVel
    return $ State ball space

initWalls :: H.Space -> IO ()
initWalls space = mapM_ (uncurry $ initWall space) wallPoints

initWall :: H.Space -> H.Position -> H.Position -> IO ()
initWall space a b = do
    body    <- H.newBody H.infinity H.infinity
    shape   <- H.newShape body (H.LineSegment a b wallThickness) 0
    H.elasticity shape $= nearOne
    H.spaceAdd space body
    H.spaceAdd space shape

initBall :: H.Space -> H.Position -> H.Velocity -> IO H.Body
initBall space pos vel = do
    body    <- H.newBody ballMass ballMoment 
    shape   <- H.newShape body (H.Circle ballRadius) 0
    H.position body $= pos
    H.velocity body $= vel
    H.elasticity shape $= nearOne
    H.spaceAdd space body
    H.spaceAdd space shape
    return body

-------------------------------
-- graphics

display state = do
    drawState =<< get state
    simTime <- simulate =<< get state    
    sleep (max 0 $ frameTime - simTime) 
    

drawState :: State -> IO ()
drawState st = do
    pos <- get $ ballPos st
    G.clear [G.ColorBuffer]
    drawWalls
    drawBall pos
    G.swapBuffers

drawBall :: H.Position -> IO ()
drawBall pos = do
    G.color red
    circle x y $ d2gl ballRadius
    where (x, y) = vec2gl pos

drawWalls :: IO ()
drawWalls = do
    G.color black
    line (-dow2) (-doh2) (-dow2) doh2
    line (-dow2) doh2    dow2    doh2
    line dow2    doh2    dow2    (-doh2)
    line dow2   (-doh2)  (-dow2) (-doh2)
    where dow2 = d2gl ow2 
          doh2 = d2gl oh2  


onMouse state = do    
    mb <- G.getMouseButton ButtonLeft
    when (mb == Press) (get G.mousePos >>= updateVel state)
   
updateVel state pos = do   
    size <- get G.windowSize
    st <- get state
    p0 <- get $ ballPos st
    v0 <- get $ ballVel st
    let p1 = mouse2canvas size pos
    ballVel st $= 
        H.scale (H.normalize $ p1 - p0) (max minVel $ H.len v0)

mouse2canvas :: G.Size -> G.Position -> H.Vector
mouse2canvas (G.Size sx sy) (G.Position mx my) = H.Vector x y
    where d a b  = fromIntegral a / fromIntegral b
          x  = width * (d mx sx - 0.5)
          y  = height * (negate $ d my sy - 0.5)


vertex2f :: G.GLfloat -> G.GLfloat -> IO ()
vertex2f a b = G.vertex (G.Vertex3 a b 0)


-- colors

white = G.Color4 (0::G.GLfloat)
black = G.Color4 (0::G.GLfloat) 0 0 1
red   = G.Color4 (1::G.GLfloat) 0 0 1

-- primitives

line :: G.GLfloat -> G.GLfloat -> G.GLfloat -> G.GLfloat -> IO ()
line ax ay bx by = G.renderPrimitive G.Lines $ do
    vertex2f ax ay
    vertex2f bx by


circle :: G.GLfloat -> G.GLfloat -> G.GLfloat -> IO ()
circle cx cy rad = 
    G.renderPrimitive G.Polygon $ mapM_ (uncurry vertex2f) points
    where n = 50
          points = zip xs ys
          xs = fmap (\x -> cx + rad * sin (2*pi*x/n)) [0 .. n]
          ys = fmap (\x -> cy + rad * cos (2*pi*x/n)) [0 .. n]

vec2gl :: H.Vector -> (G.GLfloat, G.GLfloat)
vec2gl (H.Vector x y) = (d2gl x, d2gl y)

d2gl :: Double -> G.GLfloat
d2gl = realToFrac

d2gli :: Double -> G.GLsizei
d2gli = toEnum . fromEnum . d2gl
