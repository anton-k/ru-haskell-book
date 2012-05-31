import Control.Applicative

f :: Char -> Char -> Char -> String
f a b c = reverse $ [a,b,c]

main :: IO ()
main = print =<< f <$> getChar <*> getChar <*> getChar

