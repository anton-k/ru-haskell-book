module Test where

import Control.Applicative 

import Metro

import Test.QuickCheck

instance Arbitrary Station where
    arbitrary = ($ s0) . foldr (.) id . fmap select <$> ints
        where ints = vector =<< choose (0, 100)
              s0 = St Blue De

select :: Int -> Station -> Station
select i s = as !! mod i (length as)
    where as = fst <$> distMetroMap s
            

prop1 :: Station -> Station -> Bool
prop1 a b = connect a b == (fmap reverse $ connect b a)

prop2 :: Station -> Station -> Bool
prop2 a b = maybe True (all (uncurry near) . pairs) $ connect a b

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (drop 1 xs)

near :: Station -> Station -> Bool
near a b = a `elem` (fst <$> distMetroMap b)


fakeProp :: Station -> Station -> Bool
fakeProp (St a _) (St b _) = a == b

notBlueAndBlack a b = cond a && cond b ==> prop1 a b 
    where cond (St a _) = a /= Blue && a /= Black


testFor = forAll (liftA2 (,) gen gen) $ uncurry prop1
    where gen = elements [St Blue De, St Red Lao, 
                    St Green Til, St Orange Sever]


newtype OnlyOrange = OnlyOrange Station   deriving (Show)
newtype Only4      = Only4      Station   deriving (Show)  

instance Arbitrary OnlyOrange where
    arbitrary = OnlyOrange . St Orange <$> 
        elements [DnoBolota, PlBakha, Krest, Lao, Sever]


instance Arbitrary Only4 where
    arbitrary = Only4 <$> elements [St Blue De, St Red Lao, 
                    St Green Til, St Orange Sever]


prop3 :: Station -> Station -> Property
prop3 a@(St wa _) b@(St wb _) = 
    classify (wa == Orange || wb == Orange) "Orange" $
    classify (wa == Black  || wb == Black)  "Black"  $
    classify (wa == Red    || wb == Red)    "Red"    $ prop1 a b


