module Tree where

import Types
import Data.Tree
import Random

type Diap a = (a, a)

inDiap :: Ord a => Diap a -> Tree a -> [a]
inDiap d = execWriter . inDiap' d

inDiap' :: Ord a => Diap a -> Tree a -> Writer [a] ()
inDiap' d@(a, b) (Node v xs) 
    | (a <= v) && (v <= b)  = tell [v] *> rest  
    | otherwise             = rest 
    where rest = mapM_ (inDiap' d) xs  

list a = Node a []
bi v a b = Node v [a, b]
un v a   = Node v [a]

tree1 = bi 10 (un 2 $ un 6 $ list 7) (list 5)
tree2 = bi 12 tree1 (bi 8 tree1 tree1)

t0 = Node 0 $ map list [1 .. 7]
t1 n = foldr1 (>>)  $ take n $ repeat t0
tt = t1 6
ttt = t1 7

