module Fib where

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib' :: Int -> Int
fib' n = fibs !! n
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

