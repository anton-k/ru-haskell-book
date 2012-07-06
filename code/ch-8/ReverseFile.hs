module Main where

import qualified System.IO.Strict as StrictIO

main :: IO ()
main = inFile reverse "test"

inFile :: (String -> String) -> FilePath -> IO ()
inFile fun file = writeFile file . fun =<< StrictIO.readFile file
    
