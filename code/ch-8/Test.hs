module Test where
    
import System.IO

main = do
    hPutStrLn stdout "HelloHelloHello"
    hSeek stdout AbsoluteSeek 4
    hPutChar stdout '*'
    

