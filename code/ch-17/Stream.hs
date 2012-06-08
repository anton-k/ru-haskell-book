module Stream where

import Nat    
   
data Stream n a = a :& Stream n a

dt :: (Nat n, Fractional a) => Stream n a -> a
dt xs = 1 / (fromIntegral $ toInt $ proxy xs)
    where proxy :: Stream n a -> n
          proxy = undefined

int :: (Nat n, Fractional a) => a -> Stream n a -> Stream n a
int x0 ~(f:&fs) = x0 :& int (x0 + dt fs * f) fs

