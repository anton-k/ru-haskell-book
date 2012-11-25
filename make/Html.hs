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

import Colour
import Links
import Template
import Inits

import Common

default (LT.Text)

isPic x = isPng x || isJpg x || isJpeg x

txtFiles  = lsBy isTxt "."
cssFiles  = lsBy isCss "."
htmlFiles = lsBy isHtml "."

main :: IO ()
main = shellyNoDir $ do
    goRoot 
    clearResDir
    copyHtmlHeaders
    mapM_ runGpp =<< srcFileNames
    cd "target/html/book" 
    tfmPandocs
    makeHtml
    free

clearResDir = remake "target/html/book"
free = mapM_ rm_f =<< txtFiles

goRoot = cd ".."

srcFileNames = fmap (fmap filename) $ chdir "pandoc" txtFiles

runGpp file = chdir "gpp-macros" $ do
    cp ("../pandoc" </> file) "tmp.txt"
    gpp "tmp.txt" ("../target/html/book" </> file)
    where gpp from to = run_ "./gpp-html" $ fmap toTextIgnore [from, to]

copyHtmlHeaders = copyFromTo "html" "target/html/book"

makeHtml = phi =<< cssFiles
    where phi css = txtFiles >>= mapM_ (makeFile css)

makeFile css a = pandoc_ (flagsByName css a) a

pandoc_ flags a = run_ "pandoc" ((toTextIgnore a) : flags)

flagsByName css a = outFile a ++ [mathFlag a] 
    ++ tocFlag a ++ fmap cssStyle css ++ commonFlags
    where commonFlags = ["-sS", "-f", "native", "-t", "html"]


outFile = ("-o" :) . return . toTextIgnore . filename
    . flip replaceExtension "html" 


cssStyle = fromString . ("--css=" ++) . LT.unpack . toTextIgnore 

mathFlag a 
    | isCategoryChapter a'  = "--mathjax"
    | otherwise             = "--latexmathml"
    where a' = basename a
          isCategoryChapter a = a == "15" || a == "16"

tocFlag a
    | isBookChapter a'  = ["--toc"]
    | otherwise         = []
    where a' = LT.unpack $ toTextIgnore $ basename a
          

tfmPandocs =  do
    toc <- fmap (parseToc . LT.unpack) $ readfile "toc.txt"
    mapM_ (tfmPandoc toc) =<< txtFiles


tfmPandoc :: Toc -> FilePath -> Sh ()
tfmPandoc toc a = do
    a' <- LT.unpack . toTextIgnore <$> absPath a
    liftIO $ inFile tfm a' a'
    where tfm = colourHtml . prefixLinks . template toc bname 
          bname = LT.unpack $ toTextIgnore $ basename a  

