module Random where

import Types

type Random a = State Double a

next :: Random Double
next = State $ \s -> (s, nextRandom s)

nextRandom :: Double -> Double
nextRandom = snd . properFraction . (105.947 * )


addRandom :: Double -> Random Double
addRandom x = fmap (+x) next 

addRandom2 :: Double -> Double -> Random Double
addRandom2 a b = liftA2 (add a b) next next
    where add  a b = \x y -> diap a 1 x + diap b 1 y
          diap c r = \x   -> x * 2 * r - r + c


data Coin = Heads | Tails
    deriving (Show)

dropCoin :: Random Coin
dropCoin = fmap drop' next
    where drop' x 
            | x < 0.5   = Heads
            | otherwise = Tails  
