module Metro where

import Data.Tree
import Data.Maybe
import qualified AstarSet       as S
import qualified AstarHashSet   as H

import Data.Hashable

data Station = St Way Name
    deriving (Show, Eq, Ord)

instance Hashable Station where
    hash (St a b) = hash (fromEnum a, fromEnum b)


data Way = Blue | Black | Green | Red | Orange
    deriving (Show, Eq, Ord, Enum)

data Name = Kosmodrom | UlBylichova | Zvezda 
          | Zapad | Ineva | De | Krest | Rodnik | Vostok 
          | Yug | Sirius | Til | TrollevMost | Prizrak | TainstvenniyLes 
          | DnoBolota | PlBakha | Lao | Sever
          | PlShekspira
    deriving (Show, Eq, Ord, Enum)


data Point = Point 
    { px :: Double
    , py :: Double
    } deriving (Show, Eq)

place :: Name -> Point
place x = uncurry Point $ case x of
    Kosmodrom           -> (-3,7)
    UlBylichova         -> (-2,4) 
    Zvezda              -> (0,1)
    Zapad               -> (1,7)
    Ineva               -> (0.5, 4)
    De                  -> (0,-1)
    Krest               -> (0,-3)
    Rodnik              -> (0,-5)
    Vostok              -> (-1,-7)
    Yug                 -> (-7,-1)
    Sirius              -> (-3,0)
    Til                 -> (3,2)
    TrollevMost         -> (5,4)
    Prizrak             -> (8,6)
    TainstvenniyLes     -> (11,7)
    DnoBolota           -> (-7,-4)
    PlBakha             -> (-3,-3)
    Lao                 -> (3.5,0)
    Sever               -> (6,1)
    PlShekspira         -> (3,-3)


dist :: Point -> Point -> Double
dist a b = sqrt $ (px a - px b)^2 + (py a - py b)^2

stationDist :: Station -> Station -> Double
stationDist (St n a) (St m b)
    | n /= m && a == b  = penalty
    | otherwise         = dist (place a) (place b)
    where penalty = 1



metroMap :: Station -> [Station]
metroMap x = case x of
    St Black Kosmodrom          -> [St Black UlBylichova]
    St Black UlBylichova        -> 
            [St Black Kosmodrom, St Black Zvezda, St Red UlBylichova]  
    St Black  Zvezda            -> 
            [St Black UlBylichova, St Blue  Zvezda, St Green Zvezda]
    
    St Blue Zapad               -> [St Blue Ineva]
    St Blue Ineva               ->
            [St Blue Zapad, St Red Ineva, St Blue Zvezda]
    St Blue Zvezda              ->
            [St Black Zvezda, St Green Zvezda, St Blue Ineva, St Blue De]
    St Blue De                  ->
            [St Blue Zvezda, St Blue Krest]
    St Blue Krest               ->
            [St Orange Krest, St Blue Rodnik, St Blue De]
    St Blue Rodnik              ->
            [St Blue Krest, St Blue Vostok, St Red Rodnik]
    St Blue Vostok              -> [St Blue Rodnik]
    
    St Orange DnoBolota         -> [St Orange PlBakha] 
    St Orange PlBakha           ->
            [St Red PlBakha, St Orange Krest, St Orange DnoBolota]
    St Orange Krest             ->
            [St Blue Krest, St Orange PlBakha, St Orange Lao]
    St Orange Lao               ->
            [St Orange Krest, St Red Lao, St Orange Sever]
    St Orange Sever             -> [St Orange Lao]

    
    St Green TainstvenniyLes    -> [St Green Prizrak]
    St Green Prizrak            -> 
            [St Green TainstvenniyLes, St Green TrollevMost]
    St Green TrollevMost        ->
            [St Green Til, St Green Prizrak]
    St Green Til                ->
            [St Green TrollevMost, St Green Zvezda, St Red Til]
    St Green Zvezda             ->
            [St Black Zvezda, St Blue Zvezda, St Green Til, St Green Sirius]
    St Green Sirius             ->
            [St Green Zvezda, St Green Yug, St Red Sirius]
    St Green Yug                -> [St Green Sirius]


    St Red UlBylichova          ->
            [St Black UlBylichova, St Red Ineva, St Red Sirius]
    St Red Ineva                ->
            [St Blue Ineva, St Red UlBylichova, St Red Til]
    St Red Til                  ->
            [St Green Til, St Red Ineva, St Red Lao]
    St Red Lao                  ->
            [St Orange Lao, St Red Til, St Red PlShekspira]
    St Red PlShekspira          ->
            [St Red Lao, St Red Rodnik]
    St Red Rodnik               ->
            [St Blue Rodnik, St Red PlShekspira, St Red PlBakha]
    St Red PlBakha              ->
            [St Orange PlBakha, St Red Rodnik, St Red Sirius]
    St Red Sirius               ->
            [St Green Sirius, St Red PlBakha, St Red UlBylichova]

        
distMetroMap :: Station -> [(Station, Double)]
distMetroMap x = fmap (\a -> (a, stationDist x a)) $ metroMap x
    where dist (St a _) (St b _)
            | a == b    = 10
            | otherwise = 5


metroTree :: Station -> Station -> Tree (Station, Double)
metroTree init goal = S.astarTree distMetroMap (stationDist goal) init

connectSet :: Station -> Station -> Maybe [Station]
connectSet a b = S.search (== b) $ metroTree a b

connectHashSet :: Station -> Station -> Maybe [Station]
connectHashSet a b = H.search (== b) $ metroTree a b

--main = print $ connect (St Red Sirius) (St Green Prizrak)
