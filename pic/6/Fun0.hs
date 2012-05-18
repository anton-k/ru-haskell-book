{-# LANGUAGe NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.TwoD.Adjust
import Diagrams.Backend.Cairo.CmdLine


import Arrow

c n  = text n <> circle r # lw 0.1 # fc white

r = 1.5
d = 4

ah  = ahrule  d # lw 0.1
av  = avrule  d # lw 0.1


h  = hrule  d # lw 0.1
v  = vrule  d # lw 0.1

hv = scale 1.8 $ rotateBy (1/8) h 
ahv = scale 1.8 $ rotateBy (1/8) ah 


h1 a f b = pad 1.2 $ centerXY $ hcat 
    [text a, s, ah, c f, ah, s, text b]

s = strutX 0.5

res = h1 "a" "f" "b"


main = defaultMain $ apply (adjustSize (Height 50) (size2D res)) $
    pad 1.1 $ centerXY res

