module Series(
    Ps(..), p0, ps, eval, 
    diff, int, 
    expx, sinx, cosx, tanx)
where

import Prelude

data Ps a = a :+: Ps a
    deriving (Show, Eq)

-- Конструкторы

p0 :: Num a => a -> Ps a
p0 x = x :+: p0 0

ps :: Num a => [a] -> Ps a
ps []     = p0 0
ps (a:as) = a :+: ps as

-- Вычисление

eval :: Num a => Int -> Ps a -> a -> a
eval 0 _         _ = 0
eval n (a :+: p) x = a + x * eval (n-1) p x

eval' :: Num a => Int -> Ps a -> a -> a
eval' n p x = sum $ zipWith (*) xs $ coeffs n p
    where coeffs 0 _         = []
          coeffs n (a :+: p) = a : coeffs (n-1) p
          xs = reverse $ take n $ iterate (x*) 1


-- Сложение и умножение 

instance Num a => Num (Ps a) where
    (f :+: fs) + (g :+: gs) = (f + g) :+: (fs + gs)
    (f :+: fs) * (g :+: gs) = (f * g) :+: (f .* gs + fs * (g :+: gs))
    negate (f :+: fs) = negate f :+: negate fs
    fromInteger n = p0 (fromInteger n)


(.*) :: Num a => a -> Ps a -> Ps a
k .* (f :+: fs) = (k * f) :+: (k .* fs)  

-- Деление 

instance Fractional a => Fractional (Ps a) where
    (0 :+: fs) / (0 :+: gs) = fs / gs
    (f :+: fs) / (g :+: gs) = q :+: ((fs - q .* gs)/(g :+: gs))
        where q = f/g

    fromRational x = p0 (fromRational x)

-- Производная и интеграл

diff :: Num a => Ps a -> Ps a
diff (f :+: fs) = diff' 1 fs
    where diff' n (g :+: gs) = (n * g) :+: (diff' (n+1) gs)

int :: Fractional a => Ps a -> Ps a
int fs = 0 :+: (int' 1 fs) 
    where int' n (g :+: gs) = (g/n) :+: (int' (n+1) gs)

-- Элементарные функции

expx, sinx, cosx, tanx :: Fractional a => Ps a

expx = 1 + int expx

sinx = int cosx
cosx = 1 - int sinx

tanx = sinx / cosx

test1 :: Rational -> Double
test1 a = abs $ sin (fromRational a) - fromRational (eval 30 sinx a)

