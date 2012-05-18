module Main where

import Control.Applicative
import System.Environment

hi :: String -> String
hi name = "Hi! My name is " ++ name ++ ".\n"

reply :: String -> [String] -> String
reply name (x:_) = hi name ++ case x of
    "happy"     -> "What a lovely day. What's up?"
    "sad"       -> "Ooohh. Have you got some news for me?"
    "neutral"   -> "How are you?"  
reply name _     = reply name ["neutral"]


main = putStrLn =<< reply <$> getProgName <*> getArgs
