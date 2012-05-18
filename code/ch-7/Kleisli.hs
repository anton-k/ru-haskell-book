module Kleisli where

import Prelude hiding (id, (>>), pred)
import Nat

infixr 0 +$, *$

class Category cat where
    id   :: cat a a
    (>>) :: cat a b -> cat b c -> cat a c


class Kleisli m where
    idK  :: a -> m a
    (*>) :: (a -> m b) -> (b -> m c) -> (a -> m c)    

(+>) :: Kleisli m => (a -> m b) -> (b -> c) -> (a -> m c)
f +> g = f *> (g >> idK)

-- применение в терминах Kleisli

(*$) :: Kleisli m => (a -> m b) -> m a -> m b
(+$) :: Kleisli m => (a -> b)   -> m a -> m b

f *$ a = (const a *> f) ()
f +$ a = (const a +> f) ()


-- Экземпляр для функций

instance Category (->) where
    id      = \x -> x
    f >> g  = \x -> g (f x)

-- Maybe


instance Kleisli Maybe where
    idK    = Just
    f *> g = f >> maybe Nothing g


pred :: Nat -> Maybe Nat
pred Zero       = Nothing
pred (Succ a)   = Just a


-- []

instance Kleisli [] where
    idK     = \a -> [a]
    f *> g  = f >> map g >> concat


next :: Char -> String
next 'a' = "ab"
next 'b' = "a"

generate :: Int -> (a -> [a]) -> (a -> [a])
generate n f = iterate (*> f) f !! n


roots :: Floating a => a -> a -> [a]
roots b a = ((-b/2) + ) +$    
    case signum d of
        1   -> [sqrt d, -(sqrt d)]
        0   -> [0]
        -1  -> []        
    where d = (b/2)^2 - a   


sq :: Num a => a -> a -> a -> a
sq b a x = x^2 + b * x + a 

test :: Floating a => a -> a -> [Bool]
test a b = testRoot a b +$ roots a b

testRoot :: Floating a => a -> a -> a -> Bool
testRoot b a root = sq b a root == 0

-- State

data State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State f) = f

instance Kleisli (State s) where
    idK     = \a -> State $ \s -> (a, s)
    f *> g  = \a -> State $ \s0 ->
                        let (b, s1) = runState (f a) s0
                            (c, s2) = runState (g b) s1
                        in  (c, s2) 


iter :: Int -> State Int Int
iter n = State $ \s -> (s + n, s + 1)

sumInts :: Int -> Int
sumInts n = fst $ runState (f n 0) 0
    where f n = iterate (*> iter) iter !! n


-- Reader


-- Writer


