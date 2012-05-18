module Exp where

import qualified Data.Map as M(Map, lookup, fromList)
import Types

-- Тип для отложенных вычислений.
data Exp    = Var String
            | Lit Int
            | Neg Exp
            | Add Exp Exp
            | Mul Exp Exp
            deriving (Show, Eq)


instance Num Exp where
    negate  = Neg
    (+)     = Add
    (*)     = Mul

    fromInteger = Lit . fromInteger

    abs     = undefined
    signum  = undefined


-- Окружение
type Env = M.Map String Int

value :: Env -> String -> Int
value env name = maybe errorMsg id $ M.lookup name env
    where errorMsg = error $ "value is undefined for " ++ name


-- Составляем переменные
--
var :: String -> Exp
var = Var

n :: Int -> Exp
n = var . show


-- Вычисляем значения

eval :: Exp -> Reader Env Int
eval (Lit n)    = pure n
eval (Neg n)    = negateA $ eval n
eval (Add a b)  = eval a `addA` eval b
eval (Mul a b)  = eval a `mulA` eval b
eval (Var name) = Reader $ \env -> value env name
    
    
addA      = liftA2 (+)
mulA      = liftA2 (*)  
negateA   = liftA negate


runExp :: Exp -> [(String, Int)] -> Int
runExp a env = runReader (eval a) $ M.fromList env

-- Собираем статистику
countBiFuns :: Exp -> Int
countBiFuns = getSum . execWriter . countBiFuns'

countBiFuns' :: Exp -> Writer (Sum Int) ()
countBiFuns' x = case x of
    Add a b -> tell (Sum 1) *> bi a b
    Mul a b -> tell (Sum 1) *> bi a b
    Neg a   -> un a
    _       -> return ()
    where bi a b = countBiFuns' a *> countBiFuns' b  
          un     = countBiFuns'

countArgs :: Exp -> Int
countArgs = getSum . execWriter . countArgs'

countArgs' :: Exp -> Writer (Sum Int) ()
countArgs' x = case x of
    Add a b -> tell (Sum 2) *> bi a b
    Mul a b -> tell (Sum 2) *> bi a b
    Neg a   -> tell (Sum 1) *> un a
    Lit a   -> tell (Sum 1)
    Var a   -> tell (Sum 1)
    where bi a b = countArgs' a *> countArgs' b  
          un     = countArgs'


noNeg :: Exp -> Bool
noNeg = not . getAny . execWriter . anyNeg

anyNeg :: Exp -> Writer Any ()
anyNeg x = case x of
    Neg _   -> tell (Any True)
    Add a b -> bi a b
    Mul a b -> bi a b
    _       -> pure ()
    where bi a b = anyNeg a *> anyNeg b   


