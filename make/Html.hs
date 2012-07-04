{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Prelude hiding (init, FilePath)

import qualified System.IO.Strict as StrictIO

import Data.String(fromString)
-- import Shelly hiding ((</>))
-- import Shelly.Find
import Sh

import Data.Text.Lazy as LT hiding (init, filter, all, find)
import Filesystem.Path
import Filesystem.Path.CurrentOS

import Text.Pandoc

import Colour
import Links
import Template
import Inits

import Common

default (LT.Text)

isPic x = isPng x || isJpg x || isJpeg x

txtFiles  = lsBy isTxt "html"
cssFiles  = lsBy isCss "html"
htmlFiles = lsBy isHtml "html"

main :: IO ()
main = sh $ do
    init
    tfmSrc
    makeHtml
    free

init = remake "html" >> mkdir "html/book" >> copySrc >> copyPictures
free = rmSrc >> mvHtml >> rmNotUsedPictures

mvHtml   = mv' htmlFiles >> mv' cssFiles
    where mv' = ( >>= act)
          act x = cp x "html/book" >> rm_f x

copySrc  = copyPandocs >> copyHtmlHeaders

copyPandocs     = copyFromTo "../pandoc" "html"
copyHtmlHeaders = copyFromTo "../html" "html"
copyPictures    = mkdir "html/pic" >> cp_r "../pic" "html/pic"

rmSrc = txtFiles >>= rm_f

rmNotUsedPictures = lsBy (not . isPic) "html/pic" >>= rm_f


makeHtml = phi =<< unRoll cssFiles
    where phi css = txtFiles >>= makeFile css

makeFile css a = pandoc_ (flagsByName css a) a

pandoc_ flags a = run_ "pandoc" ((fromString $ encodeString a) : flags)

flagsByName css a = outFile a ++ [mathFlag a] 
    ++ tocFlag a ++ fmap cssStyle css ++ commonFlags
    where commonFlags = ["-sS", "-f", "native", "-t", "html"]


outFile = ("-o" :) . return . fromString . encodeString 
    . flip replaceExtension "html" . appendPrefix


appendPrefix a = dirname a </> fromString (bookPrefix ++ (encodeString $ filename a))

cssStyle = fromString . ("--css=" ++) . encodeString . filename 

mathFlag a 
    | isCategoryChapter a'  = "--mathjax"
    | otherwise             = "--latexmathml"
    where a' = basename a
          isCategoryChapter a = a == "15" || a == "16"

tocFlag a
    | isBookChapter a'  = ["--toc"]
    | otherwise         = []
    where a' = encodeString $ basename a
          
tfmSrc = appendPrefixToCss >> tfmPandocs 


appendPrefixToCss = cssFiles >>= mv'
    where mv' a = mv a (appendPrefix a)

tfmPandocs = do
    toc <- liftIO $ fmap parseToc $ readFile "html/toc.txt"
    txtFiles >>= tfmPandoc toc


tfmPandoc :: Toc -> FilePath -> Sh ()
tfmPandoc toc a = liftIO $ inFile tfm a' a'
    where tfm = colourHtml . prefixLinks . template toc bname 
          bname = encodeString $ basename a  
          a' = encodeString a

