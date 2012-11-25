-- | Tex to Pandoc (semi-automate)
module Main where

import Codec.Binary.UTF8.String

import Control.Monad.State
import Data.List.Split
import Data.List
import Data.Char

import System.Environment

main = do
    [from, to] <- getArgs
    readFile from >>= writeFile to . process
    
    
data Unit = Plain String | Slash String String | Code String | Verb String
    deriving (Show)

type Tex = [Unit]


process :: String -> String
process = decodeString . printTex . fmap subs . parse' . encodeString

-------------------------------------------------
-- parse

type Parse = String -> State [Unit] String

dropOn :: String -> String -> String
dropOn a = drop (length a)

parse' :: String -> Tex
parse' str = fmap rev $ reverse $ execState (parse str) []
    where rev x = case x of
            Plain str   -> Plain $ reverse str
            _           -> x

parse :: Parse
parse xs 
    | codePref `isPrefixOf` xs  = parseCode $ dropOn codePref xs
    | verbPref `isPrefixOf` xs  = parseVerb $ dropOn verbPref xs
    | commPref `isPrefixOf` xs  = parseCommand $ dropOn commPref xs
    | otherwise                 = next xs
    where codePref = "\\begin{code}" 
          commPref = "\\"  
          verbPref = "\\verb"


next :: Parse
next []      = return []
next (x:xs)  = parse =<< state phi
    where phi s = (\a -> (xs, a)) $ case s of
            []              -> [Plain [x]]
            Plain a : rest  -> Plain (x:a) : rest
            a               -> Plain [x] : a


parseCode :: Parse
parseCode str = do
    s <- get
    put (Code body : s)
    parse $ dropOn (body ++ endCode) str
    where body = head $ splitOn endCode str
          endCode = "\\end{code}"  


parseCommand :: Parse 
parseCommand str = parse =<< state phi
    where (a, b) = span (`elem` letters) str
          (b1, b2) = getComBody b            
          phi :: [Unit] -> (String, [Unit])
          phi s = (b2, Slash a b1 : s)
        
letters = (['a' .. 'z'] ++ ['A' .. 'Z']) 

getComBody :: String -> (String, String)
getComBody str = case str of
    '{' : rest  -> let (a, b) = span (/= '}') rest
                   in  (a, tail b)  
    _           -> ([], str)
    

parseVerb :: Parse
parseVerb (x:xs) = parse =<< state phi
    where (a, b) = span (/= x) xs
          phi :: Tex -> (String, Tex)  
          phi s  = (tail b, Verb a : s)
    


-------------------------------------------
-- substitute

subs :: Unit -> Unit
subs x 
    | isIn x        = Plain $ subIn x
    | isEmph x      = Plain $ subEmph x
    | isTtt x       = Plain $ subTtt x
    | isBf x        = Plain $ subBf x
    | isChapter x   = Plain $ subChapter x
    | isSection x   = Plain $ subSection x
    | isSubsection x= Plain $ subSubsection x
    | isSubsubsection x = Plain $ subSubsubsection x
    | isParagraph x = Plain $ subParagraph x
    | isCode x      = Plain $ subCode x
    | isVerb x      = Plain $ subVerb x
    | isQuote x     = Plain $ subQuote x
    | isUrl x       = Plain $ subUrl x
    | isDots x      = Plain $ subDots x    
    | isItemize x   = Plain []
    | isItem x      = Plain $ subItem x
    | otherwise     = x

isItemize (Slash x y) = y == "itemize" && (x == "begin" || x == "end")
isItemize _           = False

isItem = isSlash "item"

isIn x = isSlash "In" x || isSlash "InS" x
isEmph = isSlash "emph"
isTtt  = isSlash "texttt"
isBf  = isSlash "textbf"
isChapter = isSlash "chapter"
isSection = isSlash "section"
isSubsection = isSlash "subsection"
isSubsubsection = isSlash "subsubsection"
isParagraph = isSlash "paragraph"
isQuote = isSlash "Quote"
isUrl = isSlash "url"
isDots = isSlash "dots"

isCode x = case x of
    (Code _)    -> True
    _           -> False

isSlash str x = case x of
    (Slash a _)  -> a == str
    _            -> False

isVerb x = case x of
    (Verb x)    -> True
    _           -> False
    

subIn (Slash _ a) 
    | noBackTicks a = "`" ++ a ++ "`"
    | otherwise     = "``" ++ a ++ "``" 
    where noBackTicks = all (/= '`')

subEmph (Slash _ a) = "*" ++ a ++ "*"
subTtt  = subIn
subBf   (Slash _ a) = "**" ++ a ++ "**"

subQuote (Slash _ a) = "\"" ++ a ++ "\""
subUrl (Slash _ a) = "<" ++ a ++ ">"
subDots _ = "..."

subHead n str = replicate n '#' ++ " " ++ str
subChapter (Slash _ a) = subHead 1 a
subSection (Slash _ a) = subHead 2 a
subSubsection (Slash _ a) = subHead 3 a
subSubsubsection (Slash _ a) = subHead 4 a
subParagraph (Slash _ a) = subHead 5 a

subCode (Code str) = "~~~" ++ str ++ "~~~"
subVerb (Verb str) = "`" ++ str ++ "`"

subItem _ = "*  "


----------------------------------------------
-- print

printTex :: [Unit] -> String
printTex = foldr (\a b -> printUnit a ++ b) []

printUnit x = case x of
    Plain s -> s
    Slash a b -> if null b
        then '\\' : (a ++ b)
        else '\\' : (a ++ "{" ++ b ++ "}")




