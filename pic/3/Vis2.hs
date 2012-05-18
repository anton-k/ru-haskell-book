{-# LANGUAGe NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.CmdLine

import Arrow

--beside' :: (HasOrigin a, Boundable a, Monoid a) => CircleFrac -> a -> a -> a 
beside' x = beside (cos t, sin t)
    where t = getCircleFrac $ tau * x

t n  = text n # font "monospace" <> circle rad # lw 0.1 # fc white 
            # lc black

c n  = text n # font "monospace" <> circle rad # lw 0.1 # fc white 
            # lc blue

s n  = text n # font "monospace" <> c rad <> c (rad + 0.3)
    where c r = circle r # lw 0.1 # fc white # lc green


wc n = lc n . fc n

d    = 3.0
d1   = 1.7/2
rad  = 1.7

ah  = ahrule  d # lw 0.1
av  = avrule  d # lw 0.1

h  = hrule  d # lw 0.1
v  = vrule  d # lw 0.1

ad = rotateBy (1/8) ah 
au = rotateBy (1/8) (strutY 1.7 === av)

h1 = vcat [s "one"]
h2 = text "="
h3 = vcat [t "Nat", av, c "Succ", av, c "Zero"]

res = hcat [h1, strutX 2, h2, strutX 2, h3]

main = defaultMain $ pad 1.1 res
