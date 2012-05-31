module Main where 

import Data.StateVar
import Physics.Hipmunk

main = do
    initChipmunk

    space <- newSpace
    initWalls space
    ball <- initBall space initPos initVel
    loop 100 space ball


loop :: Int -> Space -> Body -> IO ()
loop 0 _     _    = return ()     
loop n space ball = do
    showPosition ball
    step space 0.5
    loop (n-1) space ball
    

showPosition :: Body -> IO ()
showPosition ball = do
    pos <- get $ position ball
    print pos

initWalls :: Space -> IO ()
initWalls space = mapM_ (uncurry $ initWall space) wallPoints

initWall :: Space -> Position -> Position -> IO ()
initWall space a b = do
    body    <- newBody infinity infinity
    shape   <- newShape body (LineSegment a b wallThickness) 0
    elasticity shape $= nearOne
    spaceAdd space body
    spaceAdd space shape

initBall :: Space -> Position -> Velocity -> IO Body
initBall space pos vel = do
    body    <- newBody ballMass ballMoment 
    shape   <- newShape body (Circle ballRadius) 0
    position body $= pos
    velocity body $= vel
    elasticity shape $= nearOne
    spaceAdd space body
    spaceAdd space shape
    return body
    
----------------------------
-- inits

nearOne = 0.9999
ballMass = 20
ballMoment = momentForCircle ballMass (0, ballRadius) 0
ballRadius = 10
    
initPos = Vector 0 0
initVel = Vector 10 5

wallThickness = 1

wallPoints = fmap (uncurry f) [
    ((-w2, -h2), (-w2, h2)),
    ((-w2, h2),  (w2, h2)),
    ((w2, h2),   (w2, -h2)),
    ((w2, -h2),  (-w2, -h2))]
    where f a b = (g a, g b) 
          g (a, b) = H.Vector a b  



h2 = 100
w2 = 100

