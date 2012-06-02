module Main where

addEvens :: Int -> Int -> Int
addEvens a b 
    | even a && even b = a + b


q = zipWith addEvens [0, 2, 4, 6, 7, 8, 10] (repeat 0)

main = print q
