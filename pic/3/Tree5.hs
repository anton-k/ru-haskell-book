{-# LANGUAGe NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.CmdLine

import Arrow

--beside' :: (HasOrigin a, Boundable a, Monoid a) => CircleFrac -> a -> a -> a 
beside' x = beside (cos t, sin t)
    where t = getCircleFrac $ tau * x

c n  = text (show n) <> circle 1 # lw 0.1 # fc white


d = 7

ah  = ahrule  d # lw 0.1
av  = avrule  d # lw 0.1

h  = hrule  d # lw 0.1
v  = vrule  d # lw 0.1

ad = rotateBy (1/2 + 1/8) ah 
au = rotateBy (1/2 + 1/8) av


tri n1 n2 n3 n4 = h1 === h2
    where m  = 3.2
          h2 = centerXY $ hcat [c n2, strutX m, c n3, strutX m, c n4]
          h1 = c n1 <> alignTR ad <> alignTL au 
                <> av'

av' = alignT (scale (cos $ tau/8) $ rotateBy (1/2) av) 
          
li = av' === c 4

label str x y  = (P (x, y), text str)

res = pad 1.15 $ centerXY $ strutX 2 ||| position [
        (origin, tri 1 3 5 6) ,
        (P (5.2, -5.95), freeze $ tri 6 2 7 8),
        (P (-5.2, -5.95), li)
    ]  

main = defaultMain $ res



