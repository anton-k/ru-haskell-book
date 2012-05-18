{-# LANGUAGe NoMonomorphismRestriction #-}

import Data.Monoid

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


h13 a f b = pad 1.2 $ vcat [x0]
    where x0 = centerXY $ hcat 
                [text a, s, ah, c3 f, s, text b]
          s  = strutX 0.5

h12 a f b = pad 1.2 $ vcat [x0]
    where x0 = centerXY $ hcat 
                [text a, s, ah, c2 f, s, s, text b]
          s  = strutX 0.5



c2 n = c n ||| centerY rose
    where rose  = mconcat [
                    alignBL $ rotateBy a ah, 
                    alignTL $ rotateBy (-a) ah]
          s     = strutY 0.1 
          a     = 1/20

c3 n = c n ||| centerY rose
    where rose  = mconcat [
                    alignBL $ rotateBy a ah, 
                    alignL ah, 
                    alignTL $ rotateBy (-a) ah]
          s     = strutY 0.1 
          a     = 1/12

res = h13 "a" "f" "b"

main = defaultMain $ pad 1.1 $ centerXY res

