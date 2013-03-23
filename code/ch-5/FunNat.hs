module FunNat where

instance Num a => Num (t -> a) where
    (+) = fun2 (+)
    (*) = fun2 (*)
    (-) = fun2 (-)

    abs      = fun1 abs
    signum   = fun1 signum

    fromInteger = const . fromInteger


fun1 :: (a -> b) -> (t -> a) -> (t -> b)
fun1 = (.)

fun2 :: (a -> b -> c) -> (t -> a) -> (t -> b) -> (t -> c)
fun2 op a b = \t -> a t `op` b t


