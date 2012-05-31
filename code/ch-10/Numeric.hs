module Numeric where

diverge :: (Ord a, Num a) => a -> [a] -> a
diverge eps (a:b:xs) 
    | abs (a - b) <= eps    = a
    | otherwise             = diverge eps (b:xs)


diff :: (Ord a, Fractional a) => a -> a -> (a -> a) -> a -> a
diff h0 eps f x = diverge eps $ map (easydiff f x) $ iterate (/2) h0
    where easydiff f x h = (f (x + h) - f x) / h


int :: (Ord a, Fractional a) => a -> (a -> a) -> a -> a -> a
int eps f a b = diverge eps $ integrate f a b

integrate :: Fractional a => (a -> a) -> a -> a -> [a]
integrate f a b = integ f a b (f a) (f b)
    where integ f a b fa fb = (fa+fb)*(b-a)/2 :
                zipWith (+) (integ f a m fa fm) 
                            (integ f m b fm fb)
                where m  = (a + b)/2
                      fm = f m 

