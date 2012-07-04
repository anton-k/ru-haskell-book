module Main where

import System.Environment
import System.Cmd

import Data.List

main = do
    str <- readFile "main.tex"
    getArgs >>= make str . head



make :: String -> String -> IO ()
make body name = do
    writeFile "test.tex" $ texMain body name
    system "pdflatex test.tex"
    system "latex test.tex"
    system  $ convert name
    system $ mv name 
    system $ dvips name
    system $ mv2 name
    return ()    
    
  
mv2 str = "mv "++ str ++ ".ps " ++ str ++ ".png " ++ str ++ ".pdf ../pic/15"

mv str = "mv test.pdf " ++ str ++ ".pdf"

dvips str = "dvips test.dvi -o " ++ str ++ ".ps"

convert str = "convert -density 600x600 test.pdf -quality 90 -resize 700x300 " 
    ++ str ++ ".png"


texMain :: String -> String -> String
texMain body str = pre 
    ++ "\\begin{document}\n\\input{" ++ str ++ "}\n\\end{document}"
    where pre = unlines 
                $ takeWhile (not . isPrefixOf "\\begin{document}") 
                $ lines body
    
