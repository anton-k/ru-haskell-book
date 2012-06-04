module Inline(f, g) where

g :: Int -> Int
g x = x + 2

f :: Int -> Int
f x = g $ g x

