module Main where

import Criterion.Main
import Control.DeepSeq

import Metro

instance NFData Station where
    rnf (St a b) = rnf (rnf a, rnf b)

instance NFData Way  where
instance NFData Name where


pair1 = (St Orange DnoBolota, St Green Prizrak)
pair2 = (St Red Lao, St Blue De)

test name search = bgroup name $ [
            bench "1" $ nf (uncurry search) pair1,
            bench "2" $ nf (uncurry search) pair2]



main = defaultMain [        
        test "Set"  connectSet,
        test "Hash" connectHashSet]
