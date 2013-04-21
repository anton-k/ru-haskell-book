module Stream where 

import Control.Applicative((<$>))
import Data.List(foldl')

time :: (Enum a, Fractional a) => [a]
time = [0, dt .. ]

dt :: Fractional a => a
dt = 1e-3

-- метод Эйлера
int :: Fractional a => a -> [a] -> [a]
int x0 ~(f:fs) = x0 : int (x0 + dt * f) fs

dist :: Fractional a => Int -> [a] -> [a] -> a
dist n a b = ( / fromIntegral n) $ 
    foldl' (+) 0 $ take n $ map abs $ zipWith (-) a b


e = int 0 e

sinx = int 0 cosx
cosx = int 1 (negate <$> sinx)
