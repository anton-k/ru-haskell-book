module FileSafe where

import Control.Applicative
import Control.Monad

main = try `catch` const main

try = msg1 >> getLine >>= read >>= append
    where read   file = readFile file >>= putStrLn >> return file
          append file = msg2 >> getLine >>= appendFile file
          msg1        = putStr "input file: "
          msg2        = putStr "input text: "
