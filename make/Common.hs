{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Common where

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


default (LT.Text)

lol = undefined

isTex = hasExt "tex"
isTxt = hasExt "txt"
isPng = hasExt "png"
isJpg = hasExt "jpg"
isJpeg = hasExt "jpeg"
isPs = hasExt "ps"
isPdf = hasExt "pdf"
isCss = hasExt "css"
isHtml = hasExt "html"

lsBy pred dir = findWhen dir (return . pred) 

remake dir =  rm_rf dir >> mkdir dir
copyFromTo from to = ls from >>= flip cp to

-- pandoc to pandoc

inFile :: (Pandoc -> Pandoc) -> String -> String -> IO ()
inFile with from to = 
      writeFile to
    . writeNative writerState     
    . with
    . readMarkdown parserState 
    =<< StrictIO.readFile from


parserState = defaultParserState { stateSmart = True } 
writerState = defaultWriterOptions { writerColumns = 121 }






