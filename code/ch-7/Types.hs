module Types(
    State(..), Reader(..), Writer(..),
    runState, runWriter, runReader,
    tell, execWriter,
    module Control.Applicative,
    module Control.Monad,
    module Data.Monoid)    
where

import Data.Monoid
import Control.Applicative
import Control.Monad

-------------------------------------------------
-- Функции с состоянием
--
--      a -> State s b

data State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State f) = f

instance Functor (State s) where
    fmap = liftM

instance Applicative (State s) where
    pure    = return
    (<*>)   = ap

instance Monad (State s) where
    return a  = State $ \s -> (a, s)
    ma >>= mf = State $ \s0 -> 
                    let (b, s1) = runState ma s0
                    in   runState (mf b) s1

---------------------------------------------------
-- Функции с окружением
--
--      a -> Reader env b

data Reader env a = Reader (env -> a)

runReader :: Reader env a -> env -> a
runReader (Reader f) = f

instance Functor (Reader env) where
    fmap = liftM

instance Applicative (Reader env) where
    pure    = return
    (<*>)   = ap

instance Monad (Reader env) where
    return a    = Reader $ const a
    ma >>= mf   = Reader $ \env -> 
                    let b = runReader ma env
                    in  runReader (mf b) env 
                    
---------------------------------------------------
-- Функции-накопители
--
--      Monoid msg => a -> Writer msg b

data Writer msg a = Writer (a, msg)

runWriter :: Writer msg a -> (a, msg)
runWriter (Writer f) = f

instance Monoid msg => Functor (Writer msg) where
    fmap = liftM

instance Monoid msg => Applicative (Writer msg) where
    pure    = return
    (<*>)   = ap

instance Monoid msg => Monad (Writer msg) where
    return a    = Writer (a, mempty)
    ma >>= mf   = Writer (c, msgA `mappend` msgF)
        where (b, msgA) = runWriter ma
              (c, msgF) = runWriter $ mf b


tell :: Monoid a => a -> Writer a () 
tell a = Writer ((), a)

execWriter :: Writer msg a -> msg
execWriter (Writer (a, msg)) = msg



