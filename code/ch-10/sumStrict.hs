module Main where

import Data.List(foldl')

sum' = foldl' (+) 0

main = print $ sum' [1 .. 1e5]

