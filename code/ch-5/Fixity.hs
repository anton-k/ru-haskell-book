module Fixity where

import Prelude(Num(..))

infixl 4 *** 
infixl 5 +++
infixr 5 `neg`

(***) = (*)
(+++) = (+)
neg   = (-)

