module Main where

import Data.IORef
import Data.StateVar

main = do
    var <- newIORef 2
    x <- readIORef var
    print x
    writeIORef var 4
    x <- readIORef var
    print x


main' = var >>= (\v -> 
       readIORef v >>= print 
    >> writeIORef v 4 
    >> readIORef v >>= print)
    where var = newIORef 2    


main'' = do
    var <- newIORef 2
    x   <- get var
    print x
    var $= 4
    x   <- get var
    print x

            
