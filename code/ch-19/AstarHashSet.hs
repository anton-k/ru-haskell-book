module AstarHashSet where

import Control.Applicative

import Data.List(find)
import Data.Tree

import qualified Data.HashSet as S
import Data.Hashable

import Data.Maybe

import qualified Data.PriorityQueue.FingerTree as Q

astarTree :: (Num h, Ord h) 
    => (a -> [(a, h)]) -> (a -> h) -> a -> Tree (a, h)
astarTree alts distToGoal s0 = unfoldTree f (s0, 0)
    where f (s, h) = ((s, heur h s), next h <$> alts s)
          heur h s = h + distToGoal s  
          next h (a, d) = (a, d + h)

search :: (Ord h, Hashable a, Eq a) 
	=> (a -> Bool) -> Tree (a, h) -> Maybe [a]
search isGoal =  findPath isGoal . flattenTree . addPath 

un = undefined

findPath :: (a -> Bool) -> [Path a] -> Maybe [a]
findPath isGoal =  fmap path . find (isGoal . pathEnd)


type PTree a h = Tree (Path a, h)

flattenTree :: (Ord h, Hashable a, Eq a) => Tree (Path a, h) -> [Path a]
flattenTree a = ping none (singleton a) 

ping :: (Ord h, Hashable a, Eq a) => Visited a -> ToVisit a h -> [Path a]
ping visited toVisit 
    | isEmpty toVisit = []
    | otherwise       = pong visited toVisit' a
    where (a, toVisit') = next toVisit


pong :: (Ord h, Hashable a, Eq a) 
    => Visited a -> ToVisit a h -> Tree (Path a, h) -> [Path a]
pong visited toVisit a 
    | inside a visited  = ping visited toVisit
    | otherwise         = getPath a : 
        ping (insert a visited) (schedule (subForest a) toVisit)


getPath :: Tree (Path a, h) -> Path a
getPath = fst . rootLabel

type ToVisit a h = Q.PQueue h (Tree (Path a, h))

priority t = (snd $ rootLabel t, t)

singleton :: Ord h => Tree (Path a, h) -> ToVisit a h
singleton = uncurry Q.singleton . priority 

next :: Ord h => ToVisit a h -> (Tree (Path a, h), ToVisit a h)
next = fromJust . Q.minView

isEmpty :: Ord h => ToVisit a h -> Bool
isEmpty = Q.null

schedule :: Ord h => [Tree (Path a, h)] -> ToVisit a h -> ToVisit a h
schedule = Q.union . Q.fromList . fmap priority

type Visited a   = S.HashSet a

none :: Hashable a => Visited a
none = S.empty

insert :: (Hashable a, Eq a) => Tree (Path a, h) -> Visited a -> Visited a
insert = S.insert . pathEnd . getPath

inside :: (Eq a, Hashable a) => Tree (Path a, h) -> Visited a -> Bool
inside = S.member . pathEnd . getPath



addPath :: Tree (a, h) -> Tree (Path a, h)
addPath = iter []
    where iter ps t = Node (Path val (reverse ps'), h) $ 
            iter ps' <$> subForest t
            where (val, h)  = rootLabel t
                  ps'       = val : ps


data Path a = Path 
	{ pathEnd	:: a
	, path		:: [a]
	} 


