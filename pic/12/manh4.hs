{-# Language NoMonomorphismRestriction #-}
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Arrow

d = 2
w = 0.1

sq n = square d # lw w <> text (show n)
emp  = square d # lw w 

state = centerXY $ vcat [
    hcat $ [emp, sq 4, emp, emp],
    hcat $ replicate 4 emp,
    hcat $ replicate 4 emp,
    hcat $ [emp, emp, sq 9, emp]]


res = pad 1.2 state

main = defaultMain res
