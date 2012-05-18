{-# LANGUAGe NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

c n  = text (show n) <> circle 1 # lw 0.1 # fc white

hleng = 7

dot = circle 0.2 # fc black # lw 0


dots = vcat' with {sep = 6} $ map centerX [
    dot,
    hcat' with {sep = 1.2} (replicate 3 dot),
    hcat' with {sep = 1.2} (replicate 7 dot),
    hcat' with {sep = 1.2} (replicate 11 dot)]

-----------

res = pad 1.1 $ lw 0.1 $ mconcat [dots,
    r  ~~ a1,
    r  ~~ a2,
    r  ~~ a3,
    a1 ~~ b1,
    a1 ~~ b2,
    a1 ~~ b3,
    a2 ~~ b4,
    a2 ~~ b5,
    a3 ~~ b6,
    a3 ~~ b7,

    b2 ~~ c1,
    b2 ~~ c2,
    b2 ~~ c3,
    b2 ~~ c4,
    b3 ~~ c5,

    b4 ~~ c6,
    b4 ~~ c7,
    b6 ~~ c8,
    b6 ~~ c9,
    b6 ~~ c10,
    b7 ~~ c11
    ] 
    where r  = P (0,0)
          a1 = P (-1.6,-6.4) 
          a2 = P (   0,-6.4)
          a3 = P ( 1.6,-6.4)
          b1 = P (-4.8, -12.8)   
          b2 = P (-3.2, -12.8)   
          b3 = P (-1.6, -12.8)   
          b4 = P (0, -12.8)  
          b5 = P (1.6, -12.8)  
          b6 = P (3.2, -12.8)  
          b7 = P (4.8, -12.8)

          [c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11] 
            = fmap (\a -> P (a, -19.2)) [-8, -6.4 .. 8]


            


        

            

main = defaultMain res

