{-# Language BangPatterns #-}
module Main where

import System.Environment(getArgs)

main = print . sum2 . xs . read =<< fmap head getArgs  
    where xs n = [1 .. 10 ^ n]

sum2 :: [Int] -> (Int, Int)
sum2 = iter (0, 0)
    where iter c  []     = c
          iter c  (x:xs) = iter (tick x c) xs

tick :: Int -> (Int, Int) -> (Int, Int)
tick x (!c0, !c1) | even x    = (c0, c1 + 1)
                  | otherwise = (c0 + 1, c1)
