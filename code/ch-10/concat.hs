module Main where

concatR = {-# SCC "right" #-} foldr (++) [] 
concatL = {-# SCC "left" #-}  foldl (++) []

fun :: Double
fun = test concatL - test concatR
    where test f = last $ f $ map return [1 .. 1e4]

main = print fun
