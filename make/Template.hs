-- | Append header and footer
module Template(
      Toc
    , template
    , parseToc) 
where

import Control.Monad.Trans.Writer
import Text.Pandoc hiding (Writer) 
import Data.Default

import Inits

import Debug.Trace(trace)

echo' :: Show a => a -> a
echo' a = trace (show a) a



type Toc  = [(Name, Url)] 

type Name = String
type Url  = String

template :: Toc -> Name -> Pandoc -> Pandoc
template toc name doc  
    | isBookChapter name    = template' toc name doc
    | otherwise             = doc


template' :: Toc -> Name -> Pandoc -> Pandoc
template' toc name (Pandoc meta blocks) = Pandoc meta $
    header prev next ++ blocks ++ footer prev next ++ license 
    where (prev, next) = fileInfo toc name


type Prev = (Name, Url)
type Next = (Name, Url)

header :: Prev -> Next -> [Block]
header (_, prev) (_, next) = getBlocks $ headerTemplate prev next

footer :: Prev -> Next -> [Block]
footer prev next = getBlocks $ footerTemplate prev next

license :: [Block]
license = getBlocks licenseTemplate

headerTemplate :: Url -> Url -> String
headerTemplate prev next = inDiv "header" $ navTemplate prev next

footerTemplate :: Prev -> Next -> String
footerTemplate (prevName, prevUrl) (nextName, nextUrl) =
    inDiv "footer" $ 
        navTemplate prevUrl nextUrl 
        ++ "\n" ++
        chaptersTemplate prevName nextName
           


fileInfo :: Toc -> Name -> (Prev, Next)
fileInfo toc x = (getToc $ pred $ n, getToc $ succ n)
    where n = fileNum x
          getToc = (toc !! ) . flip mod (length toc)

fileNum :: Name -> Int
fileNum x = case x of
    "preface"   -> 0
    "appendix"  -> 22
    _           -> read $ echo' x


getBlocks str = case parse str of
    Pandoc _ blocks     -> blocks        
    

-- get toc

parseToc :: String -> Toc
parseToc = tail .  -- first link is a link to logo
    execWriter . bottomUpM parseToc' . parse 
    where parseToc' :: Inline -> Writer Toc Inline
          parseToc' x = case x of
                Link text (url, title)  -> tell [(name text,url)] >> return x
                _                       -> return x
          name x = write $ Pandoc emptyMeta [Plain x]
          emptyMeta = Meta [] [] []



parse :: String -> Pandoc
parse = readMarkdown def

write :: Pandoc -> String
write = writeMarkdown def

-- templates (bydlokod, oh my)

inDiv name str = 
    "<div id=\"" ++ name ++ "\"> \
\ \n" ++ str ++ " \
\ \n </div> "


navTemplate :: Url -> Url -> String
navTemplate prev next = 
 "<div id=\"nav\">\
\<a id=\"prev\" class=\"button\" href=\"" 
    ++ bookPrefix ++ prev ++ "\">**`<-`**</a>\ 
\<a id=\"next\" class=\"button\" href=\"" 
    ++ bookPrefix ++ next ++ "\">**`->`**</a>\
\   <div id=\"home\">[`::` Содержание `::`](toc.html)</div>\
\</div>"


chaptersTemplate :: Name -> Name -> String
chaptersTemplate prev next = 
 "<div id=\"nav\">\
\ <div id=\"prev-name\">" ++ prev ++ "</div>\
\ <div id=\"wrap-next-name\"><div id=\"next-name\">" ++ next ++ "</div></div>\
\</div>"


licenseTemplate :: String
licenseTemplate = 
   "<div id=\"license\">Зарегистрировано под лицензией \
\ [Creative commons Attribution-NonCommercial-NoDerivs](http://creativecommons.org/licenses/by-nc-nd/3.0/) \
\ 3.0 Generic (CC BY-NC-ND 3.0)</div>" 
 
