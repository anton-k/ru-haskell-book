module Inits where

import Data.Char(isDigit)

bookPrefix = "" -- "ru-haskell-book-"

    
isBookChapter :: String -> Bool
isBookChapter a = 
        a == "preface" || a == "appendix" 
    ||  all isDigit a
