module Nat where

import Prelude (
        Bool(..), 
        Show(..), (++),
        Eq(..), Ord(..), Ordering(..),
        Num(..), Integer, error)

data Nat = Zero | Succ Nat
    deriving (Show)

-- класс Eq

instance Eq Nat where
    (==) Zero     Zero     = True
    (==) (Succ a) (Succ b) = a == b
    (==) _        _        = False

-- класс Ord

instance Ord Nat where
    compare Zero     Zero     = EQ
    compare Zero     (Succ a) = LT
    compare (Succ a) Zero     = GT
    compare (Succ a) (Succ b) = compare a b

-- класс Num

instance Num Nat where
    (+) a Zero     = a
    (+) a (Succ b) = Succ (a + b)

    (*) a Zero     = Zero
    (*) a (Succ b) = a + (a * b)   

    fromInteger 0 = Zero
    fromInteger n = Succ (fromInteger (n-1))

    abs    x    = x 
    signum Zero = Zero
    signum _    = Succ Zero

    negate _ = error "negate is undefined for Nat"
