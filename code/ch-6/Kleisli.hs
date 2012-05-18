module Kleisli where

import Prelude hiding (id, (>>), pred, sequence)
import Nat

infixr 0 *$
infixr 1 +$
infixl 0 $$

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

ap :: Kleisli m => m (a -> b) -> m a -> m b
ap mf ma = ( +$ ma) *$ mf

($$) :: Kleisli m => m (a -> b) -> m a -> m b
($$) = ap

lift1 :: Kleisli m => (a -> b)   -> m a -> m b
lift1 = (+$)

lift2 :: Kleisli m => (a -> b -> c) -> m a -> m b -> m c
lift2 f a b = lift1 f a `ap` b

lift3 :: Kleisli m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3 f a b c = lift2 f a b `ap` c

sequence :: Kleisli m => [m a] -> m [a]
sequence = foldr (lift2 (:)) (idK []) 

mapK :: Kleisli m => (a -> m b) -> [a] -> m [b]
mapK f a = sequence $ (map f a)

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


