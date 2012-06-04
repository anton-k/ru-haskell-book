module Main where

data List a = Nil | Cons a (List a)
    deriving (Show)


{-# INLINE [1] foldrL #-}
foldrL :: (a -> b -> b) -> b -> List a -> b
foldrL cons nil x = case x of
    Nil         -> nil
    Cons a as   -> cons a (foldrL cons nil as) 

{-# INLINE [1] mapL #-}
mapL :: (a -> b) -> List a -> List b
mapL f = undefined

{-# RULES  
"mapL"  forall f xs.
    mapL f xs  =  foldrL (Cons . f) Nil xs
  #-}


main = print $ mapL (+100) $ Cons 1 $ Cons 2 $ Cons 3 Nil
