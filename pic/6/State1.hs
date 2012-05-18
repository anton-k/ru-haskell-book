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
                [text a, s, ah, c f, x01]
          s  = strutX 0.5
          av1 = avrule  (d*0.5) # lw 0.1
          x1 = vcat [av1, strutY 0.7, text "s"]          
          x01 = centerY $ vcat [af b, strutY 0.8, af "s"]
          af label = hcat [ah, s, text label]

h2 = position [
        (origin, centerX x0 === centerX x1),
        (P (0, 2), ah),
        (P (0, -2), x2),
        (P (0, 2.7), text "b")]
        
    where x0 = hcat [text "a", s, ah, c "f", strutX d, c "g", x01]
          x01 = centerY $ vcat [af "c", strutY 0.8, af "s"]
          af label = hcat [ah, s, text label]
          s  = strutX 0.5
          x1 = hcat [av1, strutX (d + 2*r - 0.2), av1]
          av1 = avrule  (d*0.5) # lw 0.1 === strutY 0.7 === text "s"

          x2 =(   scaleX 0.5 h # alignR 
               <> scaleY 0.7 v # alignT) # alignB 
               <> hrule (d/2 + r + 0.15) # lw 0.1 # alignL
         

res = vcat [
    centerX $ hcat [h1 "a" "f" "b", strutX 2, h1 "b" "g" "c"],
    s,
    centerX h2,
    s,
    h1 "a" (sign specToSpec "f" "g") "c"]
    where s = strutY 2


main = defaultMain $ pad 1.1 $ centerXY res

