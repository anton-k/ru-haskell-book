module Qsort where

import Control.Monad

import Data.STRef
import Control.Monad.ST

import Data.Array
import Data.Array.ST
import Data.Array.MArray

import Data.Array.Base (unsafeRead, unsafeWrite)

import Loop

swapArray :: Ix i => i -> i -> STArray s i e -> ST s ()
swapArray i j arr = do
     vi <- readArray arr i
     vj <- readArray arr j

     writeArray arr i vj
     writeArray arr j vi    


test :: Int -> Int -> [a] -> [a]
test i j xs = elems $ runSTArray $ do
    arr <- newListArray (0, length xs - 1) xs
    swapArray i j arr
    return arr

qsort :: Ord a => [a] -> [a]
qsort xs = elems $ runSTArray $ do
    arr <- newListArray (left, right) xs
    qsortST left right arr
    return arr
    where left  = 0
          right = length xs - 1
 


qsortST :: Ord a => Int -> Int -> STArray s Int a -> ST s ()
qsortST left right arr = do
    when (left <= right) $ do
        swapArray left (div (left + right) 2) arr
        vLeft <- readArray arr left 
        (last, _) <- forLoop (left + 1) (<= right) succ 
                            (update vLeft) (return (left, arr))
        swapArray left last arr
        qsortST left (last - 1) arr
        qsortST (last + 1) right arr
    where update vLeft i st = do
            (last, arr) <- st
            vi <- readArray arr i
            if (vi < vLeft) 
                then do
                    swapArray (succ last) i arr
                    return (succ last, arr)
                else do
                    return (last, arr)


{-
slow sort too:

qsortST :: Ord a => Int -> Int -> STArray s Int a -> ST s (STArray s Int a)
qsortST left right arr 
    | left >= right = return arr
    | otherwise     = do
        last <- partition left right arr
        qsortST left (pred last) arr
        qsortST (succ last) right arr

partition :: Ord a => Int -> Int -> STArray s Int a -> ST s Int
partition left right arr = do
    let i = div (left + right) 2
    vi <- readArray arr i
    swapArray left i arr 
    last <- foldM (swaps vi) left [succ left .. right]
    swapArray last left arr
    return last
    where swaps vi j k = do
            vk <- readArray arr k
            if (vk < vi)
                then do
                    swapArray (succ j) k arr
                    return (succ j)
                else 
                    return j
-}

