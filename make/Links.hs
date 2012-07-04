module Links where

import Data.List    (isPrefixOf)

import Text.Pandoc 
import Inits(bookPrefix)

import Debug.Trace(trace)

echo' :: Show a => a -> a
echo' a = trace (show a) a


prefixLinks :: Pandoc -> Pandoc
prefixLinks = bottomUp prefixLinks'

prefixLinks' :: Inline -> Inline
prefixLinks' x = case x of
    Link text target    -> Link text (tfmLink bookPrefix target)
    _                   -> x


tfmLink prefix (url, title) = (tfmUrl prefix url, title)
    where tfmUrl prefix a
            | isLocal a     = prefix ++ a
            | otherwise     = a 


isLocal = all ( /= '/')

