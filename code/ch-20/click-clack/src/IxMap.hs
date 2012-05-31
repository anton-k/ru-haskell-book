module IxMap(
        IxMap, empty, 
        insert, delete, (!),
        fromList, toList) 
where

import qualified Data.Map as M

data IxMap a = IxMap 
    { ixMapData      :: M.Map Int a
    , ixMapFreshIds  :: [Int]
    }


instance Functor IxMap where
    fmap f a = a{ ixMapData = M.map f $ ixMapData a}
    

empty :: IxMap a
empty = IxMap M.empty [0 ..]


insert :: a -> IxMap a -> (IxMap a, Int)
insert a m = (IxMap (M.insert k a (ixMapData m)) ks, k)
    where k:ks = ixMapFreshIds m

delete :: Int -> IxMap a -> IxMap a
delete k m = IxMap (M.delete k $ ixMapData m) (k : ixMapFreshIds m)


fromList :: [a] -> IxMap a
fromList as = IxMap (M.fromList  (zip [0..] as)) [n..]
    where n = length as
            

(!) :: IxMap a -> Int -> a
(!) m k = ixMapData m M.! k


toList :: IxMap a -> [(Int, a)]
toList = M.toList . ixMapData


