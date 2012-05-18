{-# LANGUAGe NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Arrow
import Signs

c n  = (font "monospace" $ text n) <> circle r # lw 0.1 # fc white

r = 1.5
d = 4

ah  = ahrule  d # lw 0.1
av  = avrule  d # lw 0.1


h  = hrule  d # lw 0.1
v  = vrule  d # lw 0.1

hv = scale 1.8 $ rotateBy (1/8) h 
ahv = scale 1.8 $ rotateBy (1/8) ah 


h1 a f b = pad 1.2 $ vcat [x0, x1]
    where x0 = centerXY $ hcat 
                [text a, s, ah, c f, ah, s, text b]
          s  = strutX 0.5
          av1 = avrule  (d*0.5) # lw 0.1
          x1 = vcat [av1, strutY 0.7, text "env"]


h11 = h1 "a" "f" "b"
h12 = hcat [text "b", s, ah, c "g", ah, s, text "c"]
    where s  = strutX 0.5


h2 = position [
        (origin, centerX x1),
        (P (-d/2-r,-2.5), centerX x2),
        (P (0, 1), text "b")]
    where x1 = hcat [text "a", s, ah, c "f", ah, c "g", ah, s, text "c"] 
          s  = strutX 0.5
          x2 = vcat [av1, strutY 0.7, text "env"]
          av1 = avrule  (d*0.5) # lw 0.1

          
          
    

res = vcat [
        centerX $ hcat [h11, strutX 2, h12],
        s,
        h2,
        s,
        h1 "a" (sign specToArr "f" "g") "c"]
    where s = strutY 2

main = defaultMain $ pad 1.1 $ centerXY res

