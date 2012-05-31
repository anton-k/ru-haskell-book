module Laziest where

{-

lazySafeHead :: [a] -> Maybe a
lazySafeHead ~(x:xs) = Just x
lazySafeHead []      = Nothing

-}


lazySafeHead :: [a] -> Maybe a
lazySafeHead []      = Nothing
lazySafeHead ~(x:xs) = Just x

