{-# LANGUAGe NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Arrow

c n  = text (show n) <> circle 1 # lw 0.1 # fc white

wc color = (fc color . lc color)

hleng = 7

ah  = ahrule  7 # lw 0.1
av  = avrule  7 # lw 0.1

h  = hrule  7 # lw 0.1
v  = vrule  7 # lw 0.1

hv = scale 1.8 $ rotateBy (1/8) h 
ahv = scale 1.8 $ rotateBy (1/8) ah 

h1 = hcat [c 1  # wc green, strutX hleng, c 2]

h2 = hcat [c 3 # wc blue, h, c 4]

h5 = hcat [c 5, h]



h6 = c 6 # wc blue <> hv <> rotateBy (1/4) hv 


h11 = translate (0, 1) $ (vcat [h1, rotateBy 0.5 av, h2] # center (3.5, 0))

h7 = h6 === h11

h8 = hcat [c 8, strutX hleng, c 7]

h9 = centerXY h8 === h7

h10 = hcat [c 5 # wc blue ||| rotateBy 0.5 ah, translateY 1 $ centerXY h9]

label str x y  = (P (x, y), text str)

res = pad 1.15 $ (||| strutX 8)$ alignX 0.45 $ 
    position [(origin, centerXY h10), 
        label "a" 1.3 2.5,
        label "b" 8 2.5,
        label "c" 2.7 7.5,
        label "d" (-5) (-0.1),
     --   label "e" (5) (-0.1),
        label "f" 8  7,
        label "g" 1 (-5.5),
        label "h" 5 (-9),

        (translate (1.53, 1.53) $ origin, scale 0.5 ahv) 
        ]

main = defaultMain res

