{-# LANGUAGe NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Arrow
import Signs 

c n  = text n # font "monospace" <> circle r # lw 0.1 # fc white

r = 1.5
d = 4

ah  = ahrule  d # lw 0.1
av  = avrule  d # lw 0.1


h  = hrule  d # lw 0.1
v  = vrule  d # lw 0.1

hv = scale 1.8 $ rotateBy (1/8) h 
ahv = scale 1.8 $ rotateBy (1/8) ah 


h1 a f b = pad 1.2 $ centerXY $ position [
        (origin, x1),
        (P (0, -d/2.05), alignL (ah1 ||| s1 ||| text "msg"))]   
    where x0 = centerXY $ hcat 
                [text a, s, ah, c f, ah, s, text b]
          x1 = x0 === scaleY 0.5 v
          s  = strutX 0.5
          s1 = strutX 1.2
          ah1 = ahrule  (d*1.25) # lw 0.1

h2 = position [
        (origin, x0 === alignBy (-1, 0 ) 0.54 x1),
        (P (0, 2.2), text "b"),
        (P (-4.5,-4.5), text "MsgF"),
        (P (5.5,-1), text "MsgG")]
    where x0 = centerXY $ hcat
                    [text "a", s, ah, c "f", ah, c "g", ah, s, text "c"]
          s  = strutX 0.5  
          s1 = strutX 4.1
          v1 = alignT $ scaleY 0.7 v
          v2 = alignT $ scaleY 0.5 v
          av2 = alignT $ rotateBy 0.5 $ avrule (d/2) # lw 0.1
          x1 = alignL x3 === alignL x2
          ah1 = ahrule  (d+r) # lw 0.1
          ah2 = ahrule  (d-r) # lw 0.1
          x2 = hcat [v3, ah1, c "++", ah2, s1, text "MsgF ++ MsgG"] 
          x3 = hcat [v2, strutX (d+2*r - 0.15), av2]
          v3 = alignB $ vrule r # lw 0.1

res = vcat [
        centerX $ hcat [h1 "a" "f" "b", strutX 3, h1 "b" "g" "c"],
        strutY 4,
        h2,
        strutY 4,
        h1 "a" (sign specToSpec "f" "g") "c"]


main = defaultMain $ pad 1.1 $ centerXY res

