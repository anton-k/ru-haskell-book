{-# Language TypeFamilies #-}
module Point2D where

class AdditiveGroup v where
    zeroV   :: v
    (^+^)   :: v -> v -> v
    negateV :: v -> v


class AdditiveGroup v => VectorSpace v where
    type Scalar v   :: *
    (*^)            :: Scalar v -> v -> v


data V2 = V2 Int Int
    deriving (Show, Eq)

instance AdditiveGroup V2 where
    zeroV       = V2 0 0
    (V2 x y)  ^+^ (V2 x' y')  = V2 (x+x') (y+y')
    negateV (V2 x y)   = V2 (-x) (-y)

instance VectorSpace V2 where
    type Scalar V2 = Int
    s *^ (V2 x y) = V2 (s*x) (s*y)
