{-# LANGUAGe NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.CmdLine

import Arrow

--beside' :: (HasOrigin a, Boundable a, Monoid a) => CircleFrac -> a -> a -> a 
beside' x = beside (cos t, sin t)
    where t = getCircleFrac $ tau * x

t n  = text n # font "monospace" <> circle rad # lw 0.1 # fc white 

c n  = text n # font "monospace" <> circle rad # lw 0.1 # fc white 
            # lc blue

s n  = text n # font "monospace" <> c rad <> c (rad + 0.3)
    where c r = circle r # lw 0.1 # fc white # lc green


wc n = lc n . fc n

d    = 3.0
d1   = 1.5/2
rad  = 1.5

ah  = ahrule  d # lw 0.1
av  = avrule  d # lw 0.1

h  = hrule  d # lw 0.1
v  = vrule  d # lw 0.1

ad = rotateBy (1/8) ah 
au = rotateBy (1/8) (strutY rad === av)

tri n1 n2 n3 = h1 === h2
    where m  = 4.0
          h2 = centerXY $ hcat [n2, strutX m, n3]
          h1 = n1 <> alignTR (beside' (1/8) ad $ strut w) 
                    <> alignTL au
          w  = (1.1, 1.1)

h0 = s "head"
h1 = vcat [t "[a]", av, tri (c ":") (s "x") (s "")]
h2 = text "="
h3 = vcat [t "a" # fc black, av,  s "x"]

delim = strutX 2
res = hcat [h0, h1, h2, delim, h3]

main = defaultMain $ pad 1.1 res
