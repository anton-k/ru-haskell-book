module Main where

import System.Environment

main = getArgs >>= mapM_ putStrLn . zipWith f [1 .. ]
    where f n a = show n ++ ": " ++ a
