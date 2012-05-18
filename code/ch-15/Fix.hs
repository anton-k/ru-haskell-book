{-# Language 
        FlexibleContexts, 
        UndecidableInstances,
        TypeSynonymInstances #-}
module Fix where

import Prelude hiding (succ, (>>), sum)
import Data.Maybe

newtype Fix f = Fix { unFix :: f (Fix f) }

fold :: Functor f => (f a -> a) -> (Fix f -> a)
fold f = f . fmap (fold f) . unFix

unfold :: Functor f => (a -> f a) -> (a -> Fix f)
unfold f = Fix . fmap (unfold f) . f

-- instance Eq 

instance Show (f (Fix f)) => Show (Fix f) where
    show x = "(" ++ show (unFix x) ++ ")"
  
instance Eq (f (Fix f)) => Eq (Fix f) where
    a == b = unFix a == unFix b


-- Рекурсия

cata :: Functor f => (f a -> a) -> (Fix f -> a)
cata f = f . fmap (cata f) . unFix

ana :: Functor f => (a -> f a) -> (a -> Fix f)
ana f = Fix . fmap (ana f) . f


fix :: (a -> a) -> a
fix = cata (\(Cons f a) -> f a) . ana (\a -> Cons a a)

hylo :: Functor f => (f b -> b) -> (a -> f a) -> (a -> b) 
hylo phi psi = cata phi . ana psi

(>>) :: Functor f => (a -> f a) -> (f b -> b) -> (a -> b) 
psi >> phi = phi . (fmap $ hylo phi psi) . psi

-----------------------------------------
-- Nat

data N a = Zero | Succ a
    deriving (Show, Eq)

instance Functor N where
    fmap f x = case x of
        Zero    -> Zero
        Succ a  -> Succ (f a)

type Nat = Fix N

zero :: Nat
zero = Fix Zero

succ :: Nat -> Nat
succ = Fix . Succ



instance Num Nat where
    (+) a = fold $ \x -> case x of
            Zero    -> a
            Succ x  -> succ x

    (*) a = fold $ \x -> case x of
            Zero    -> zero
            Succ x  -> a + x

    fromInteger = unfold $ \n -> case n of
            0   -> Zero
            n   -> Succ (n-1)

    abs = undefined
    signum = undefined


sumInt :: Int -> Int
sumInt = range >> sum
    
sum x = case x of
    Nil      -> 0 
    Cons a b -> a + b

range n 
    | n == 0    = Nil 
    | otherwise = Cons n (n-1)

---------------------------------------
-- List

data L a b = Nil | Cons a b
    deriving (Show, Eq)

instance Functor (L a) where
    fmap f x = case x of
        Nil         -> Nil
        Cons a b    -> Cons a (f b)

type List a = Fix (L a)

nil :: List a
nil = Fix Nil

infixr 5 `cons`

cons :: a -> List a -> List a
cons a = Fix . Cons a


headL :: List a -> a
headL x = case unFix x of
    Nil         -> error "empty list"
    Cons a _    -> a

tailL :: List a -> List a
tailL x = case unFix x of
    Nil         -> error "empty list"
    Cons a b    -> b

mapL :: (a -> b) -> List a -> List b
mapL f = fold $ \x -> case x of
    Nil         -> nil
    Cons a b    -> f a `cons` b

iterateL :: (a -> a) -> a -> List a
iterateL f = unfold $ \x -> Cons x (f x)

concatL :: List a -> List a -> List a
concatL a b = (fold $ \x -> case x of
    Nil         -> b
    Cons a b    -> a `cons` b) a

takeL :: Int -> List a -> List a
takeL = curry $ unfold $ \(n, xs) -> 
    if n == 0 then Nil
              else Cons (headL xs) (n-1, tailL xs)  

-----------------------------------------------
-- BTree

type BTree a = Fix (B a)

data B a b = Leaf a | Node b b
    deriving (Show, Eq)

instance Functor (B a) where
    fmap f x = case x of
        Leaf a      -> Leaf a
        Node a b    -> Node (f a) (f b)


leaf :: a -> BTree a
leaf = Fix . Leaf

node :: BTree a -> BTree a -> BTree a
node a b = Fix (Node a b)

swapB :: BTree a -> BTree a
swapB = cata $ \x  -> case x of
        Leaf a      -> leaf a
        Node a b    -> node b a

flatten :: BTree a -> List a
flatten = cata $ \x -> case x of
        Leaf a      -> a `cons` nil
        Node a b    -> a `concatL` b

-----------------------------------------------
-- Stream

type Stream a = Fix (S a)

data S a b = a :& b
    deriving (Show, Eq)

instance Functor (S a) where
    fmap f (a :& b) = a :& f b  


headS :: Stream a -> a
headS x = case unFix x of
    (a :& _) -> a


tailS :: Stream a -> Stream a
tailS x = case unFix x of
    (_ :& b) -> b


mapS :: (a -> b) -> Stream a -> Stream b
mapS f = ana $ \xs -> (f $ headS xs) :& tailS xs

dropWhileS :: (a -> Bool) -> Stream a -> Stream a
dropWhileS p = psi >> phi 
    where phi ((b, xs) :& next) = if b then next else xs
          psi xs = (p $ headS xs, xs) :& tailS xs

iterateS :: (a -> a) -> a -> Stream a
iterateS f = ana $ \x -> x :& f x

zipS :: Stream a -> Stream b -> Stream (a, b)
zipS = curry $ ana $ 
    \(a, b) -> (headS a, headS b) :& (tailS a, tailS b)

getElem :: Int -> Stream a -> a
getElem = curry (enum >> elem) 
    where elem ((n, a) :& next) 
                | n == 0    = a
                | otherwise = next
          enum (a, st) = (a, headS st) :& (a-1, tailS st)



fibs :: Stream Int
fibs = ana (\(a, b) -> a :& (b, a+b)) (0, 1)


fib :: Int -> Int
fib = flip getElem fibs 


-- primes

primes :: Stream Int
primes = ana erato nums

erato xs = n :& erase n ys
    where n  = fromJust $ headS ys    
          ys = dropWhileS isNothing xs  
            

nums :: Stream (Maybe Int)
nums = mapS Just $ iterateS (+1) 2 

findFirst :: Stream (Maybe a) -> a
findFirst = cata $ \(a :& b) -> maybe b id a


erase :: Int -> Stream (Maybe a) -> Stream (Maybe a)
erase n xs = ana phi (0, xs)
    where phi (a, xs) 
            | a == 0    = Nothing  :& (a', tailS xs)
            | otherwise = headS xs :& (a', tailS xs)
            where a' = if a == n-1 then 0 else (a+1)    
