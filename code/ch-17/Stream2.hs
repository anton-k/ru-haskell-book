{-# Language TypeFamilies, EmptyDataDecls, UndecidableInstances #-}
{-# Language ScopedTypeVariables #-}
module Stream where

data Zero
data Succ a

type family Add a b :: *

type instance Add a Zero        = a
type instance Add a (Succ b)    = Succ (Add a b)


type family Mul a b :: *

type instance Mul a Zero        = Zero
type instance Mul a (Succ b)    = Add a (Mul a b)

class Nat a where
    toInt :: a -> Int

instance Nat Zero where
    toInt = const 0

instance Nat a => Nat (Succ a) where
    toInt x = 1 + toInt (undefined :: a)
        
 
data Stream n a = a :& Stream n a

dt :: forall n. (Nat n, Fractional a) => Stream n a -> a
dt xs = 1 / (fromIntegral $ toInt (undefined :: n))

int :: (Nat n, Fractional a) => a -> Stream n a -> Stream n a
int x0 ~(f:&fs) = x0 :& int (x0 + dt fs * f) fs

