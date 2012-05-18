module Main where

import Control.Applicative

import Criterion.Main
import Test.QuickCheck
import Metro

instance Arbitrary Station where
    arbitrary = ($ s0) . foldr (.) id . fmap select <$> ints
        where ints = vector 30
              s0 = St Blue De

select :: Int -> Station -> Station
select i s = as !! mod i (length as)
    where as = fst <$> distMetroMap s

prop :: (Station -> Station -> Maybe [Station]) 
	-> Station -> Station -> Bool
prop search a b = search a b == (reverse <$> search b a)


main = defaultMain [
	bench "Set"  $ quickCheck (prop connectSet),
	bench "Hash" $ quickCheck (prop connectHashSet)]


