{-# LANGUAGe NoMonomorphismRestriction #-}

import Diagrams.Prelude
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

h2 = pad 1.2 $ centerXY $  hcat
    [text "a", s, ah, c "g", ah, c "f", ah, s, text "c"] 
        

res = vcat [
        centerX $ hcat [h1 "a" "g" "b", strutX 3, h1 "b" "f" "c"],
        strutY 4,
        h2,
        strutY 4,
        h1 "a" "f . g" "c"]


main = defaultMain $ pad 1.1 $ centerXY res

