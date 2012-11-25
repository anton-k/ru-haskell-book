{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Prelude hiding (init, FilePath)

import Data.String(fromString)
import Shelly

import Data.Text.Lazy as LT hiding (init, filter, all, find)
import Filesystem.Path hiding ((</>))
import Text.Pandoc

import Common
import Colour(colourLatex)

default (LT.Text)

isPic x = isPng x || isJpg x || isJpeg x

txtFiles  = lsBy isTxt "."
cssFiles  = lsBy isCss "."
htmlFiles = lsBy isHtml "."

main :: IO ()
main = shelly $ do
    goRoot 
    clearResDir
    copyTexHeaders
    mapM_ runGpp =<< srcFileNames
    cd "target" 
    tfmPandocs
    makeTex
--    makePdf
--    free

clearResDir = remake "target/html/book"
free = mapM_ rm_f =<< txtFiles

goRoot = cd ".."

srcFileNames = fmap (fmap filename) $ chdir "pandoc" txtFiles

runGpp file = chdir "gpp-macros" $ do
    cp ("../pandoc" </> file) "tmp.txt"
    gpp "tmp.txt" ("../target" </> file)
    where gpp from to = run_ "./gpp-tex" $ fmap toTextIgnore [from, to]

copyTexHeaders = copyFromTo "tex" "target"

makeTex = txtFiles >>= mapM_ pandoc_

pandoc_ a = run_ "pandoc" (flags a ++ [toTextIgnore $ filename a]) 

flags a = outFile a ++ commonFlags
    where commonFlags = ["-S", "-t", "latex", "--latex-engine=xelatex", "-f", "native", "--chapters", "--no-highlight"]


outFile = ("-o" :) . return . toTextIgnore . filename
    . flip replaceExtension "tex" 


makePdf = act >> act
    where act = run_ "xelatex" ["--shell-escape", "notes-all"]


tfmPandocs = mapM_ tfmPandoc =<< txtFiles


tfmPandoc :: FilePath -> Sh ()
tfmPandoc a = do
    a' <- LT.unpack . toTextIgnore <$> absPath a
    liftIO $ inFile tfm a' a'
    where tfm = colourLatex 

