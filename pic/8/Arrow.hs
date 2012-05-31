{-# LANGUAGe NoMonomorphismRestriction #-}

module Arrow(avrule, ahrule) where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

ahrule = xarrowDef
avrule = yarrowDef

data ArrowParam = ArrowParam
    { arrowAngle  :: Double
    , arrowLength :: Double }

defArrowParams = ArrowParam (pi/7) 0.5

--xarrow :: ArrowParam -> Double -> Diagram
xarrow a leng = centerXY $ (||| strutX 0.1) $
    stroke $ pathFromTrail $ flip Trail False $ fmap straight 
    [(leng - 0.1, 0), (-s * 0.02, 0), (s*vx, vy), (-s*vx, -vy), (s*vx, -vy)]
    where (vx, vy) = arrowVec a{ arrowAngle = angle - arrowAngle a }
          angle = s * pi   
          s = signum leng  
    
yarrow a = rotateBy (1/4) . xarrow a
{-
yarrow :: ArrowParam -> Double -> Diagram
yarrow a leng = fmap straight 
    [(0, leng), (0, -s * 0.7) , (vx, -s*vy), (-vx, s*vy), (-vx, -s*vy)]
    where (vx, vy) = arrowVec a{ arrowAngle = arrowAngle a + angle }
          angle = -pi/2 + pi * s 
          s = signum leng
-}
    

xarrowDef = xarrow defArrowParams
yarrowDef = yarrow defArrowParams


--arrowVec :: ArrowParam -> Vec
arrowVec a = (vx, vy)
    where vx = arrowLength a * cos (arrowAngle a)
          vy = arrowLength a * sin (arrowAngle a)


main = defaultMain $ ahrule 5 # showOrigin
        
c1 = circle 1 # lw 0.1
