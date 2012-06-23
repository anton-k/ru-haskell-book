module FSMt where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import Data.Monoid

type FSM s = StateT s (Writer [String]) s

fsm :: Show ev => (ev -> s -> s) -> (ev -> FSM s)
fsm transition e = log e >> run e
    where run e = StateT $ \s -> return (s, transition e s)  
          log e = lift $ tell [show e] 


type Speaker = (SpeakerState, Level)

data SpeakerState = Sleep | Work
    deriving (Show)

data Level  = Level Int
    deriving (Show)

quieter :: Level -> Level
quieter (Level n) = Level $ max 0 (n-1)

louder :: Level -> Level
louder (Level n) = Level $ min 10 (n+1)


data User = Button | Quieter | Louder
    deriving (Show)


speaker :: User -> FSM Speaker
speaker = fsm $ trans
    where trans Button    (Sleep, n) = (Work, n)
          trans Button    (Work,  n) = (Sleep, n)
          trans Louder    (s,     n) = (s, louder n)
          trans Quieter   (s,     n) = (s, quieter n)


safeSpeaker :: User -> FSM Speaker
safeSpeaker = fsm $ trans
    where trans Button  (Sleep, _) = (Work,  Level 0)
          trans Button  (Work,  _) = (Sleep, Level 0)
          trans Quieter (Work,  n) = (Work,  quieter n)
          trans Louder  (Work,  n) = (Work,  louder n)
          trans _       (Sleep, n) = (Sleep, n)


session :: [User]
session = [Button, Louder, Quieter, Button, Louder]
