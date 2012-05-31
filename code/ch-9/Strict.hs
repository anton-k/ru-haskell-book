{-# LANGUAGE BangPatterns, MagicHash #-}

module Strict where

import Data.List
import GHC.Prim

sum' :: Num a => [a] -> a
sum' = iter 0 
    where iter res []        = res
          iter res (a:as)    = let res' = a + res
                               in  res' `seq` iter res' as 

product' :: Num a => [a] -> a
product' = iter 1
    where iter res []        = res
          iter res (a:as)    = let res' = a * res
                               in  seq res' $ iter res' as 

mean :: [Double] -> Double
mean = division . foldl' count (0, 0)
    where count  (sum, leng) a = (sum+a, leng+1)
          division (sum, leng) = sum / fromIntegral leng


mean' :: [Double] -> Double
mean' = division . iter (0, 0)
    where iter res          []      = res
          iter (sum, leng)  (a:as)  = 
                let s = sum  + a
                    l = leng + 1
                in  s `seq` l `seq` iter (s, l) as
          
          division (sum, leng) = sum / fromIntegral leng


mean'' :: [Double] -> Double
mean'' = division . foldl' iter (0, 0)
    where iter (!sum, !leng) a = (sum  + a, leng + 1)
          division (sum, leng) = sum / fromIntegral leng


data P a b = P {-# UNPACK #-} !a {-# UNPACK #-} !b

mean''' :: [Double] -> Double
mean''' = division . foldl' iter (P 0 0)
    where iter (P sum leng) a = P (sum  + a) (leng + 1)
          division (P sum leng) = sum / fromIntegral leng


constBang :: a -> b -> a
constBang a !b = a


