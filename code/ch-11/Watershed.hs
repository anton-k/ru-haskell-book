module Watershed where

import Data.Array
import Data.Function
import Data.List
import qualified Data.Map as M

type Coord = (Int, Int)
type HeightMap = Array Coord Int
type SinkMap   = Array Coord Coord
type LabelMap  = Array Coord Char

flow :: HeightMap -> SinkMap
flow arr = fix $ \result -> listArray (bounds arr) $ 
    map (\x -> maybe x (result !) $ getSink arr x) $ 
    range $ bounds arr 

getSink :: HeightMap -> Coord -> Maybe Coord
getSink arr (x, y) 
    | null sinks = Nothing
    | otherwise  = Just $ snd $ minimum $ map (\i -> (arr!i, i)) sinks
    where sinks = filter p [(x+1, y), (x-1, y), (x, y-1), (x, y+1)]
          p i   = inRange (bounds arr) i && arr ! i < arr ! (x, y)


label :: SinkMap -> LabelMap
label a = fmap (m M.! ) a 
    where m = M.fromList $ flip zip ['a' .. ] $ nub $ elems a


ex = listArray ((0, 0), (4, 5)) 
        [1,2,3,4,5,6,
         7,8,9,2,4,5,
         3,5,3,3,6,7,
         6,4,5,5,3,1,
         2,2,4,5,3,7]

main = print $ label $ flow ex








