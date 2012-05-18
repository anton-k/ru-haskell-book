{-# LANGUAGe NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Arrow
import Signs

import Codec.Binary.UTF8.String

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

s = strutX 1.2

h2 = position [
        (origin, h2'),
        (P (0, 1), text " ")]


h2' = pad 1.2 $ centerXY $  hcat
    [text "a", s, ah, c "f", ah, c "g", ah, s, text "IO c"] 
        

res' = centerXY $ vcat [
        centerX $ hcat [h1 "a" "f" "IO b", strutX 3, h1 "b" "g" "IO c"],
        strutY 4,
        h2,
        strutY 4,
        h1 "a" (sign arrToArr "f" "g") "IO c"]

res = position [
        (P (0,0), scaleY 2.5 $ vrule d # lw 0.2),
        (P (-7, 3), fc red $ text' "До"),
        (P (7, 3), fc blue $ text' "После"),
        (P (0,0), res')]

text' = scale 1.7 . text . encodeString

main = defaultMain $ pad 1.1 $ centerXY res

