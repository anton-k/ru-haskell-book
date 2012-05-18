{-# LANGUAGe NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.CmdLine

import Arrow

--beside' :: (HasOrigin a, Boundable a, Monoid a) => CircleFrac -> a -> a -> a 
beside' x = beside (cos t, sin t)
    where t = getCircleFrac $ tau * x

c n  = text n # font "monospace" <> circle 1.7 # lw 0.1 # fc white

wc n = lc n . fc n

d = 3.0
d1 = 1.7/2

ah  = ahrule  d # lw 0.1
av  = avrule  d # lw 0.1

h  = hrule  d # lw 0.1
v  = vrule  d # lw 0.1

ad = rotateBy (1/8) ah 
au = rotateBy (1/8) (strutY 1.7 === av)

h1 = centerXY $ vcat [c "Nat" # wc green, av, 
    c "Succ", av, c "Succ", av, c "Zero"]

tri n1 n2 n3 = h1 === h2
    where m  = 4.2
          h2 = centerXY $ hcat [c n2, strutX m, c n3]
          h1 = c n1 <> alignTR (beside' (1/8) ad $ strut w) 
                    <> alignTL au
          w  = (1.2, 1.2)
   
h20 = vcat [c "Expr" # wc green, av, c "Neg", av]
h21 = h20 === (centerX $ tri "Add" "One" "")
h22 = tri "Mul" "Six" "Ten"

h2 = centerXY $ position [
        (P (3.8,-10.05), h22),
        (origin, h21) ]

res = h1 ||| strutX 7 ||| h2

main = defaultMain $ pad 1.1 $ res
