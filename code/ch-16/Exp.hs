{-# LANGUAGE GADTs #-}

module Exp where

{-
data Exp = ValTrue
         | ValFalse
         | If Exp Exp Exp
         | Val Int
         | Add Exp Exp
         | Mul Exp Exp
         deriving (Show)
-}

data Exp a where
    ValTrue     :: Exp Bool
    ValFalse    :: Exp Bool
    If          :: Exp Bool -> Exp a -> Exp a -> Exp a
    Val         :: Int -> Exp Int
    Add         :: Exp Int -> Exp Int -> Exp Int
    Mul         :: Exp Int -> Exp Int -> Exp Int


eval :: Exp a -> a
eval x = case x of
    ValTrue     -> True
    ValFalse    -> False
    If p t e    -> if eval p then eval t else eval e
    Val n       -> n
    Add a b     -> eval a + eval b
    Mul a b     -> eval a * eval b


class (Log exp, Arith exp) => E exp

class Log exp where
    true    :: exp Bool
    false   :: exp Bool
    iff     :: exp Bool -> exp a -> exp a -> exp a

class Arith exp where
    val     :: Int -> exp Int
    add     :: exp Int -> exp Int -> exp Int
    mul     :: exp Int -> exp Int -> exp Int


notE :: Log exp => exp Bool -> exp Bool
notE x = iff x false true

squareE :: Arith exp => exp Int -> exp Int
squareE x = mul x x

e1 :: E exp => exp Int
e1 = squareE $ iff (notE true) (val 1) (val 2)

e2 :: E exp => exp Bool
e2 = notE true


newtype Eval a = Eval { runEval :: a }

instance Log Eval where
    true    = Eval True
    false   = Eval False
    iff p t e = if runEval p then t else e

instance Arith Eval where
    val     = Eval
    add a b = Eval $ runEval a + runEval b
    mul a b = Eval $ runEval a * runEval b

instance E Eval
    

newtype Print a = Print { runPrint :: String }

instance Log Print where
    true    = Print "True" 
    false   = Print "False"
    iff p t e = Print $ "if (" ++ runPrint p ++ ") {" 
            ++ runPrint t ++ "}"
            ++ "{" ++ runPrint e ++ "}"

instance Arith Print where
    val n   = Print $ show n
    add a b = Print $ "(" ++ runPrint a ++ ")+(" ++ runPrint b ++ ")"
    mul a b = Print $ "(" ++ runPrint a ++ ")*(" ++ runPrint b ++ ")"

instance E Print 

