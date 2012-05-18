{-# LANGUAGe NoMonomorphismRestriction #-}

import Data.Monoid

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

rose _    _ 0 t = c t 
rose leng _ 1 t = c t ||| ahrule leng # lw 0.1
rose leng a n t = c t ||| centerY rose'
    where rose'   = mconcat $ fmap elem [-a, -a + (2*a / (n-1)) .. a]
          elem  x = align x $ rotateBy x $ ahrule leng # lw 0.1
          align x = case signum x of
                    -1  -> alignTL 
                    0   -> alignL
                    1   -> alignBL

rose1 = rose d

h1 ang n a f b = pad 1.2 $ vcat [x0]
    where x0 = centerXY $ hcat 
                [text a, s, ah, rose1 ang n f, s, text b]
          s  = strutX 0.5


h2 = position [
        (origin, x0),
        (P (a,b), x1),
        (P (a,-b),x3),
        (P (9, 3), labB),
        (P (10, -2.0), labB),
        (P (11,1), labB)]
    where x0    = hcat [text "a", s, ah, rose (1.5*d) (1/7) 3 "f", x2]
          s     = strutX 0.5
          x1    = f 1
          x2    = f 1
          x3    = f 1
          f n   = hcat [rose1 (1/12) n "g", s, text "c"]

          a = 14
          b = 5.7
          labB = text "b"
          


res = vcat [
        centerX $ hcat [h1 (1/12) 3 "a" "f" "b", h1 (1/20) 1 "b" "g" "c"],
        s,
        centerX h2,
        s,
        h1 (1/12) 3 "a" (sign specToArr "f" "g") "c"]
        where s = strutY 2


main = defaultMain $ pad 1.1 $ centerXY res

