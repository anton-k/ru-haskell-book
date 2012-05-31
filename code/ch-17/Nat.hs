{-# Language TypeFamilies, EmptyDataDecls, UndecidableInstances #-}
module Nat where

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
    toInt x = 1 + toInt (proxy x)
        
proxy :: f a -> a
proxy = undefined
