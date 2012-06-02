module Main where

fun1 = test concatL - test concatR
fun2 = test concatL + test concatR

test f = last $ f $ map return [1 .. 1e4]

concatR = foldr (++) [] 
concatL = foldl (++) [] 

main = print fun1 >> print fun2
