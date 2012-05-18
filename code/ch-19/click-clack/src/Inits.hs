module Inits where

import Types
import qualified Physics.Hipmunk as H
import Utils

title = "click-clack"


----------------------------
-- inits

-- frames per second
fps :: Int
fps = 60

-- frame time in milliseconds
frameTime :: Time
frameTime = 1000 * ((1::Double) / fromIntegral fps)

dt :: H.Time
dt = 0.2

minVel :: H.CpFloat
minVel = 9

creationPeriod :: Int
creationPeriod = 170

windowWidth, windowHeight, frameWidth, frameHeight, 
    frameOffset, wallOffset, w2, h2, o2 :: Double

frameOffset     = 50
frameWidth      = 700   
frameHeight     = 500
wallOffset      = 40

windowWidth     = frameWidth  + 2*frameOffset
windowHeight    = frameHeight + 2*frameOffset

w2 = frameWidth/2;  h2 = frameHeight/2;  o2 = wallOffset/2

ballRadius :: Double
ballRadius = 11

bonusRadius :: Double
bonusRadius = frameHeight / 2.7

bonusSleep :: Int
bonusSleep = 7


holePoints :: [H.Position]
holePoints = [
        vec (-w2+dt) (-h2+dt),
        vec 0     (-h2+dt),
        vec (w2-dt)    (-h2+dt),
        vec (-w2+dt) (h2-dt),
        vec 0     (h2-dt),
        vec (w2-dt)  (h2-dt)
    ]
    where dt  = 5

wallPoints :: [(H.Position, H.Position)]
wallPoints = [
    (vec (-w2+o2) (-h2), vec (-o2) (-h2)),
    (vec o2 (-h2), vec (w2-o2) (-h2)),
    
    (vec (-w2+o2) h2, vec (-o2) h2),
    (vec o2 h2, vec (w2-o2) h2),
    
    (vec (-w2) (h2-o2), vec (-w2) (-h2+o2)),
    (vec   w2  (h2-o2), vec   w2  (-h2+o2))]


goodBallCount, bonusBallCount, badBallCount :: Int

goodBallCount   = 2
bonusBallCount  = 4
badBallCount    = 13

livesNum :: Int
livesNum = 15

bonusNum :: Int
bonusNum = 40

minBallVel, maxBallVel :: H.CpFloat

minBallVel = 7
maxBallVel = 11


wallThickness :: Double
wallThickness = 1


scoresY1, scoresWidth, scoresX1, scoresX2, scoresHeight :: Double

scoresY1 = h2+30
scoresY2 = scoresY1 - scoresHeight

scoresX1 = -w2+o2
scoresX2 = w2 - scoresWidth-o2

scoresHeight = 20
scoresWidth = 100


livesRect :: Double -> (Point, Point)
livesRect k =  
    ((Point scoresX1 scoresY1), 
     (Point (scoresX1+k*scoresWidth) scoresY2))

bonusRect :: Double -> (Point, Point)
bonusRect k =  
    ((Point scoresX2 scoresY1), 
     (Point (scoresX2+k*scoresWidth) scoresY2))



resultPoint = Point (scoresX2-25) (scoresY2 )


-- level up' we make it harder
-- by rising number of balls

incrBad, incrBonus, incrGood :: Int -> Int -> Int

incrBad    = const (+3)
incrBonus level
    | even level = (+1)
    | otherwise  = (+2)
incrGood level  
    | even level    = succ 
    | otherwise     = id 


