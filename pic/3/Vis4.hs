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
rad  = 1.7

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

h0 = s "map"
h1 = vcat [t "a->b", av, s "f"]
h2 = vcat [t "[a]", av, c "[]"]
h3 = text "="
h4 = vcat [t "[b]",av,  c "[]"]

h2' = vcat [t "[a]", av, tri (c ":") (s "x") (s "xs")]

h4'  = position [
            (P (-7.7, -15.1), u3 "x"),
            (P (0.3, -15.1), u3 "f"),
            (P (7.7, -15.1), u4 "xs"),
            (origin, u1)] 
    where u1 = vcat [t "[b]", av, tri (c ":") (s "f") (s "map")]
          u2 = tri (s "map") (s "f") (s "xs")  
          u3 n = (alignBL $ rotateBy (-1/10) av) === s n
          u4 n = (alignBR $ rotateBy ( 1/10) av) === s n



delim = strutX 3
h11 = hcat [h0, delim, h1, strutX 4.5, h2, strutX 4.5, h3, strutX 7.5, h4]

h22 = hcat [h0, delim, h1, h2', h3, h4']
    where delim' = strutX 0.7

res = vcat [h11, strutY 2, h22]


main = defaultMain $ pad 1.1 res
