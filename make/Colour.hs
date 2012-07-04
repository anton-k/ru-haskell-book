module Colour(
    colourHtml, colourLatex,
    highlightInlineHtml, highlightCodeHtml
)
where

import qualified Codec.Binary.UTF8.String as Utf8

import qualified Language.Haskell.HsColour.HTML as H
import qualified Language.Haskell.HsColour.LaTeX as L
import Language.Haskell.HsColour.Colourise

import Text.Pandoc(
        bottomUp, Pandoc(..), Block(..), Inline(..), 
        Format, nullAttr,
        readHtml, defaultParserState)

data Target = ToHtml | ToLatex

-- html
colourHtml  = highlightPandoc ToHtml

highlightInlineHtml, highlightCodeHtml :: String -> String

highlightInlineHtml = highlightInline ToHtml
highlightCodeHtml   = highlightCode ToHtml


-- latex
colourLatex = highlightPandoc ToLatex
    

-----------------------------
-- pandoc to pandoc

highlightPandoc :: Target -> Pandoc -> Pandoc
highlightPandoc to = bottomUp (tfmCode to) . bottomUp (tfmInline to)


getHscolour :: Target -> Hscolour
getHscolour x = case x of
    ToHtml  -> H.hscolour (getColourPrefs x) False
    ToLatex -> L.hscolour (getColourPrefs x)

getFormat :: Target -> Format
getFormat x = case x of
    ToHtml  -> "html"
    ToLatex -> "tex"

-- Mixing bold with regular font breaks alignment in html, 
-- so we use non-bold in html and mixed in latex
getColourPrefs :: Target -> ColourPrefs 
getColourPrefs x = case x of
    ToHtml  -> noBoldWoodsPrefs
    ToLatex -> woodsPrefs


isHs = ( == nullAttr)

tfmCode :: Target -> Block -> Block
tfmCode to x = case x of
    CodeBlock attrs str -> phi x attrs str
    _                   -> x
    where phi x attrs str   
            | isHs attrs    = rawBlock to $ highlightCode to str
            | otherwise     = x


tfmInline :: Target -> Inline -> Inline
tfmInline to x = case x of
    (Code attrs str)    -> phi x attrs str
    _                   -> x
    where phi x attrs str 
            | isHs attrs    = rawInline to $ highlightInline to str
            | otherwise     = x

rawInline = RawInline . getFormat
rawBlock  = RawBlock  . getFormat


-----------------------------
-- string to string

type Hscolour = String -> String

highlightInline :: Target -> String -> String
highlightInline to = addCode . stripPre . getHscolour to


highlightCode :: Target -> String -> String
highlightCode to x 
    | isSh x    = addPre x
    | otherwise = getHscolour to x

isSh :: String -> Bool
isSh = phi . filter (not . (`elem` ['\n', ' ']))
    where phi x 
            | null x = True
            | otherwise = ( == '$') $ head x
        

addCode x = "<code>" ++ x ++ "</code>"
addPre  x = "<pre>" ++ x ++ "</pre>"

stripPre x = drop (length "<pre>") $ take (n - length "</pre>") x
    where n = length x
            


woodsPrefs = ColourPrefs 
  { keyword  = [Foreground ground, Bold]
  , keyglyph = [Foreground ground, Bold]
  , layout   = [Foreground Black]
  , comment  = [Foreground skyBlue]
  , conid    = [Foreground Green, Bold]
  , varid    = [Normal]
  , conop    = [Foreground ground,Bold]
  , varop    = [Foreground Black]
  , string   = [Foreground Black]
  , char     = [Foreground Black]
  , number   = [Foreground almostBlue, Bold]
  , cpp      = [Foreground Black,Dim]
  , selection = [Bold, Foreground Black]
  , variantselection = [Dim, Foreground Red]
  , definition = [Foreground Black]
  }


-- Something is wrong with rendering of bold fonts in html.
-- Browser is source of the problem. If you mix bold and non bold, 
-- the font is no longer monospace. Turning off all bolds makes it aligned.
-- So we use non-bold in html and mixed in latex
noBoldWoodsPrefs = ColourPrefs 
  { keyword  = [Foreground ground]
  , keyglyph = [Foreground ground]
  , layout   = [Foreground Black]
  , comment  = [Foreground skyBlue]
  , conid    = [Foreground Green]
  , varid    = [Normal]
  , conop    = [Foreground ground]
  , varop    = [Foreground Black]
  , string   = [Foreground Black]
  , char     = [Foreground Black]
  , number   = [Foreground almostBlue]
  , cpp      = [Foreground Black,Dim]
  , selection = [Bold, Foreground Black]
  , variantselection = [Dim, Foreground Red]
  , definition = [Foreground Black]
  }




skyBlue = Rgb 33 73 193
almostBlue = Rgb 0 0 238
ground = Rgb 178 89 15


