module FunNat where

import Prelude(Show(..), Eq(..), Ord(..), Num(..), error)
import Data.Function(id, const, (.), ($), flip, on)
import Prelude(map, foldr, filter, zipWith)

instance Show (t -> a) where
    show _ = error "Sorry, no show. It's just for Num"

instance Eq a => Eq (t -> a) where
    (==) _ _ = error "Sorry, no Eq. It's just for Num"

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


