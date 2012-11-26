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
    mapM_ runGpp =<< srcFileNames
    makeEpub

goRoot = cd ".."

srcFileNames = fmap (fmap filename) $ chdir "pandoc" txtFiles

runGpp file = chdir "gpp-macros" $ do
    cp ("../pandoc" </> file) "tmp.txt"
    gpp "tmp.txt" ("../target" </> file)
    where gpp from to = run_ "./gpp-html" $ fmap toTextIgnore [from, to]


makeEpub = cd "epub" >> run_ "./make.sh" []

