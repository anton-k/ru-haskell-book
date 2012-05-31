module File where

import Control.Applicative
import Control.Monad

main = msg1 >> getLine >>= read >>= append
    where read   file = (readFile file >>= putStrLn) >> return file
          append file = msg2 >> getLine >>= appendFile file
          msg1        = putStr "input file: "
          msg2        = putStr "input text: "

