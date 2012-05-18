{-# Language TypeFamilies #-}
module Track(
    Event(..), Track(..),
    silence, 
    Temporal(..),
    (+:+), (=:=), chord, line, loop) 
where

import Data.Monoid

data Event t a = Event {
    eventStart      :: t,
    eventDur        :: t,
    eventContent    :: a
    } deriving (Show, Eq)


data Track t a = Track {
    trackDur        :: t,
    trackEvents     :: [Event t a]
    }

silence :: t -> Track t a
silence t = Track t []

-- Temporal

class Temporal a where
    type Dur a :: *
    dur     :: a -> Dur a
    delay   :: Dur a -> a -> a
    stretch :: Dur a -> a -> a

instance Num t => Temporal (Event t a) where
    type Dur (Event t a) = t
    dur = eventDur
    delay   = delayEvent
    stretch = stretchEvent


instance Num t => Temporal (Track t a) where
    type Dur (Track t a) = t
    dur = trackDur
    delay   = delayTrack
    stretch = stretchTrack   

delayEvent :: Num t => t -> Event t a -> Event t a
delayEvent d e = e{ eventStart = d + eventStart e }

stretchEvent :: Num t => t -> Event t a -> Event t a
stretchEvent s e = e{ 
    eventStart  = s * eventStart e, 
    eventDur    = s * eventDur   e }


delayTrack :: Num t => t -> Track t a -> Track t a
delayTrack d (Track t es) = Track (t + d) (map (delayEvent d) es) 

stretchTrack :: Num t => t -> Track t a -> Track t a
stretchTrack s (Track t es) = Track (t * s) (map (stretchEvent s) es) 


-- Composition

(=:=) :: Ord t => Track t a -> Track t a -> Track t a
Track t es =:= Track t' es' = Track (max t t') (es ++ es')

(+:+) :: (Ord t, Num t) => Track t a -> Track t a -> Track t a
(+:+) a b = a =:= delay (dur a) b

chord :: (Num t, Ord t) => [Track t a] -> Track t a
chord = foldr (=:=) (silence 0)

line :: (Num t, Ord t) => [Track t a] -> Track t a
line = foldr (+:+) (silence 0)

loop :: (Num t, Ord t) => Int -> Track t a -> Track t a
loop n t = line $ replicate n t

-- Standart classes

instance Functor (Event t) where
    fmap f e = e{ eventContent = f (eventContent e) }

instance Functor (Track t) where
    fmap f t = t{ trackEvents = fmap (fmap f) (trackEvents t) }


instance (Ord t, Num t) => Monoid (Track t a) where
    mappend = (=:=)
    mempty  = silence 0
