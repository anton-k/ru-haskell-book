module Loop where

import Control.Monad

import Data.STRef
import Control.Monad.ST

forLoop ::  i -> (i -> Bool) -> (i -> i) -> (i -> s -> s) -> s -> s
forLoop i0 pred next update s0 = runST $ do
    refI <- newSTRef i0
    refS <- newSTRef s0
    iter refI refS
    readSTRef refS
    where iter refI refS = do
            i <- readSTRef refI
            s <- readSTRef refS
            when (pred i) $ do
                writeSTRef refI $ next i
                writeSTRef refS $ update i s
                iter refI refS


whileLoop :: (s -> Bool) -> (s -> s) -> s -> s
whileLoop pred update s0 = runST $ do
    ref <- newSTRef s0
    iter ref 
    readSTRef ref
    where iter ref = do
            s <- readSTRef ref
            when (pred s) $ do
                writeSTRef ref $ update s
                iter ref


