{-# Language NoMonomorphismRestriction #-}
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Arrow

d = 2
w = 0.1

sq n = square d # lw w <> text (show n)
emp  = square d # lw w 

randomState = centerXY $ vcat [
    hcat $ fmap sq [9,1,4,8],
    hcat [sq 13, emp, sq 11, sq 5],
    hcat $ fmap sq [2,10,7,3],
    hcat $ fmap sq [15,14,12,6]]

initState = centerXY $ vcat [
    hcat $ fmap sq [1,2,3,4],
    hcat $ fmap sq [5,6,7,8],
    hcat $ fmap sq [9,10,11,12],
    hcat [sq 13, sq 14,sq 15, emp]]


res = pad p randomState ||| ahrule 3 # lw (w) ||| pad p initState
    where p = 1.3

main = defaultMain res
