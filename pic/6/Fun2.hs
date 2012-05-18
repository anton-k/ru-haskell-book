{-# LANGUAGe NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Arrow
import Signs

c n  = (font "monospace" $ text n) <> circle r # lw 0.1 # fc white

r = 1.7
d = 4

ah  = ahrule  d # lw 0.1
av  = avrule  d # lw 0.1


h  = hrule  d # lw 0.1
v  = vrule  d # lw 0.1

hv = scale 1.8 $ rotateBy (1/8) h 
ahv = scale 1.8 $ rotateBy (1/8) ah 


h1 a f b = pad 1.2 $ centerXY $ hcat 
    [text a, s, ah, c f, ah, s, text b]

s = strutX 1

h2 = position [
        (origin, h2'),
        (P (0, 1), text "m b ? b")]


h2' = pad 1.2 $ centerXY $  hcat
    [text "a", s, ah, c "f", ah1, c "g", ah, s, text "m c"] 
    where ah1 = ahrule (1.8*d) # lw 0.1
        

res = vcat [
        centerX $ hcat [h1 "a" "f" "m b", strutX 3, h1 "b" "g" "m c"],
        strutY 4,
        h2,
        strutY 4,
        h1 "a" (sign specToSpec "f" "g") "m c"]


main = defaultMain $ pad 1.1 $ centerXY res

