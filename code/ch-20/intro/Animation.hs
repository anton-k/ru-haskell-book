module Main where

import Control.Applicative
import Data.IORef
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL
import System.Exit
import Control.Monad

type Time = Double

title = "Hello OpenGL"

width, height :: GLsizei

fps :: Int
fps = 60

frameTime :: Time
frameTime = 1000 * ((1::Double) / fromIntegral fps)


width   = 700
height  = 600
    
w2, h2 :: GLfloat

w2 = (fromIntegral $ width) / 2
h2 = (fromIntegral $ height)  / 2

dw2, dh2 :: GLdouble

dw2 = fromRational $ toRational w2
dh2 = fromRational $ toRational h2

type Vec2d = (GLfloat, GLfloat)

data Ball = Ball
    { ballPos :: Vec2d
    , ballVel :: Vec2d
    }

initBall = Ball (0, 0) (0, 0)

dt :: GLfloat
dt = 0.3

minVel = 10

main = do
    initialize
    openWindow (Size width height) [] Window
    windowTitle $= title

    clearColor $= Color4 1 1 1 1
    ortho (-dw2) (dw2) (-dh2) (dh2) (-1) 1
    
    ball <- newIORef initBall

    windowCloseCallback $= exitWith ExitSuccess
    windowSizeCallback  $= (\size -> viewport $= (Position 0 0, size))

    loop ball


loop :: IORef Ball -> IO ()
loop ball = do    
    display ball
    onMouse ball
    sleep frameTime
    loop ball

display ball = do
    (px, py) <- ballPos <$> get ball
    (vx, vy) <- ballVel <$> get ball
    ball $= Ball (px + dt*vx, py + dt*vy) (vx, vy)

    clear [ColorBuffer]

    color black
    line (-ow2) (-oh2) (-ow2) oh2
    line (-ow2) oh2    ow2    oh2
    line ow2    oh2    ow2    (-oh2)
    line ow2   (-oh2)  (-ow2) (-oh2)

    color red
    circle px py 10

    swapBuffers
    where ow2 = w2 - 50 
          oh2 = h2 - 50  
    

onMouse ball = do
    mb <- getMouseButton ButtonLeft
    when (mb == Press) (get mousePos >>= updateVel ball)
   
updateVel ball pos = do
    (p0x, p0y) <- ballPos <$> get ball
    v0  <- ballVel <$> get ball
    size <- get windowSize
    let (p1x, p1y) = mouse2canvas size pos 
        v1 = scaleV (max minVel $ len v0) $ norm (p1x - p0x, p1y - p0y)
    ball $= Ball (p0x, p0y) v1
    where norm v@(x, y) = (x / len v, y / len v)
          len  (x, y) = sqrt (x*x + y*y) 
          scaleV k (x, y) = (k*x, k*y)


mouse2canvas :: Size -> Position -> (GLfloat, GLfloat)
mouse2canvas (Size sx sy) (Position mx my) = (x, y)
    where d a b  = fromIntegral a / fromIntegral b
          x  = fromIntegral width * (d mx sx - 0.5)
          y  = fromIntegral height * (negate $ d my sy - 0.5)



vertex2f :: GLfloat -> GLfloat -> IO ()
vertex2f a b = vertex (Vertex3 a b 0)


-- colors

white = Color4 (0::GLfloat)
black = Color4 (0::GLfloat) 0 0 1
red   = Color4 (1::GLfloat) 0 0 1

-- primitives

line :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
line ax ay bx by = renderPrimitive Lines $ do
    vertex2f ax ay
    vertex2f bx by


circle :: GLfloat -> GLfloat -> GLfloat -> IO ()
circle cx cy rad = 
    renderPrimitive Polygon $ mapM_ (uncurry vertex2f) points
    where n = 50
          points = zip xs ys
          xs = fmap (\x -> cx + rad * sin (2*pi*x/n)) [0 .. n]
          ys = fmap (\x -> cy + rad * cos (2*pi*x/n)) [0 .. n]


