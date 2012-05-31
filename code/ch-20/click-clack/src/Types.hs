module Types where

import Data.Monoid

import IxMap
import Data.IORef
import qualified Physics.Hipmunk as H

type Time = Double

-- actions

data GameState = On | WaitForClick Int

data Query 
    = Remove Ball 
    | HeroVelocity H.Velocity | MakeBall Freq
    | GameOver | Step 

-- senses

data Event = Touch Ball | UserClick H.Position

data Sense = Sense 
    { senseHero     :: HeroBall
    , senseBalls    :: [Ball]  }

-- graphics
    
data Picture = EmptyPicture
             | Prim Color Primitive
             | Join Picture Picture

foldPicture :: a -> (Color -> Primitive -> a) -> (a -> a -> a) 
    -> Picture -> a
foldPicture emptyPic primPic joinPic p = case p of
    EmptyPicture    -> emptyPic
    Prim col a      -> primPic col a
    Join a b        -> joinPic (f a) (f b)
    where f = foldPicture emptyPic primPic joinPic


instance Monoid Picture where
    mempty  = EmptyPicture
    mappend = Join

data Primitive = Line Point Point 
               | Circle Point Radius 
               | Rect Point Point
               | CircleSolid Point Radius 
               | RectSolid Point Point
               | Text Point String


data Point  = Point Double Double
type Radius = Double   

data Color = Color Double Double Double

------------------------------------------------------

data World = World 
    { worldPure   :: Pure
    , worldDirty  :: Dirty }

-- pure

data Pure = Pure
    { pureScores     :: Scores
    , pureHero       :: HeroBall
    , pureBalls      :: [Ball]
    , pureCreation   :: Creation
    , pureTouch      :: Maybe (Int, [Ball])
    , pureGameState  :: GameState
    }

data HeroBall = HeroBall
    { heroPos   :: H.Position
    , heroVel   :: H.CpFloat
    }


data Ball = Ball
    { ballType      :: BallType
    , ballPos       :: H.Position
    , ballId        :: Id
    }

data BallType = Hero | Good | Bad | Bonus
    deriving (Show, Eq, Enum)


type Id = Int

data Scores = Scores 
    { scoresLevel :: Int
    , scoresLives :: Int
    , scoresBonus :: Int
    } deriving (Show)

data Creation = Creation 
    { creationStat      :: Stat
    , creationGoalStat  :: Stat
    , creationTick      :: Int
    }

data Stat = Stat
    { goodCount     :: Int
    , badCount      :: Int
    , bonusCount    :: Int
    } deriving (Eq, Show, Read)

data Freq = Freq 
    { freqGood      :: Float
    , freqBad       :: Float
    , freqBonus     :: Float
    } 

-- dirty

data Dirty = Dirty 
    { dirtyHero     :: Obj
    , dirtyObjs     :: IxMap Obj
    , dirtySpace    :: H.Space
    , dirtyTouchVar :: Sensor H.Shape
    , dirtyMouse    :: Sensor H.Position
    }

data Obj = Obj 
    { objType     :: BallType
    , objShape    :: H.Shape
    , objBody     :: H.Body
    } deriving (Eq)

type Sensor a = IORef (Maybe a)


foldQuery :: (Ball -> a) -> (H.Velocity -> a) -> (Freq -> a) 
    -> a -> a 
    -> Query -> a
foldQuery rm hv mb go step q = case q of
    Remove ball         -> rm ball
    HeroVelocity vel    -> hv vel
    MakeBall freq       -> mb freq
    GameOver            -> go
    Step                -> step

foldEvent :: (Ball -> a) -> (H.Position -> a) 
    -> (Event -> a)
foldEvent touch click a = case a of
    Touch ball  -> touch ball
    UserClick p -> click p



