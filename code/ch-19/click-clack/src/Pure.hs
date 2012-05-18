module Pure where

import Data.Monoid
import Data.Maybe
import qualified Physics.Hipmunk as H
import Control.Monad.State

import Control.Applicative
import Data.Tuple(swap)

import Data.List
import Types
import Utils
import Inits
import Graphics

pureGameOver :: Pure -> Bool
pureGameOver s = case pureGameState s of
    On      -> False
    _       -> True

processClick :: Pure -> (Pure, [Query])
processClick p = case pureGameState p of
    WaitForClick 0      -> (reset p, [GameOver])
    WaitForClick n      -> (p{ pureGameState = WaitForClick (n-1) }, [])
    On                  -> (p, [])

------------------------------------------------------------------
-- inits

initPure :: Sense -> [Event] -> Pure
initPure sense _ = Pure 
    { pureScores    = initScores 
    , pureHero      = senseHero sense
    , pureBalls     = senseBalls sense
    , pureCreation  = initCreation
    , pureTouch     = Nothing
    , pureGameState = On
    }

initScores = Scores 
    { scoresLevel   = 0
    , scoresLives   = livesNum
    , scoresBonus   = 0     
    }

initCreation = Creation 
    { creationStat      = Stat 0 0 0
    , creationGoalStat  = Stat goodBallCount badBallCount bonusBallCount
    , creationTick      = 0
    }

-------------------------------------------------------------------
-- events

-- continuous

updateSenses :: Sense -> Pure -> Pure
updateSenses sense pure 
    | pureGameOver pure = pure
    | otherwise         = pure
    { pureHero    = senseHero sense
    , pureBalls   = senseBalls sense
    , pureCreation= creation{ creationStat = getStat $ senseBalls sense }
    , pureTouch   = updTouch =<< pureTouch pure  
    }
    where creation = pureCreation pure
          getStat = foldl' phi (Stat 0 0 0) . fmap ballType
          phi stat b = case b of 
            Good    -> stat{ goodCount  = succ $ goodCount  stat }
            Bad     -> stat{ badCount   = succ $ badCount   stat }
            Bonus   -> stat{ bonusCount = succ $ bonusCount stat }
            Hero    -> stat
          updTouch (n, xs) = case n of
            0   -> Nothing
            a   -> Just (pred a, xs)


-- discrete



updateEvents :: [Event] -> Pure -> (Pure, [Query])
updateEvents evts pure 
    | pureGameOver pure = onGameOver evts pure
    | otherwise         = upd pure
    where upd = swap . runState 
            (concat <$> liftA2 (:) onNothing (mapM onEvent evts)) 

onGameOver :: [Event] -> Pure -> (Pure, [Query])
onGameOver evts pure  
    | any isUserClick evts = processClick pure
    | otherwise            = (pure, [])
    where isUserClick evt = case evt of
                (UserClick _)   -> True
                _               -> False
        

onEvent :: Event -> State Pure [Query]
onEvent = foldEvent onTouch onUserClick

onUserClick :: H.Position -> State Pure [Query]
onUserClick p1 = state $ \s -> 
        let hero = pureHero s
            v  = heroVel $ pureHero s
            p0 = heroPos $ pureHero s
            v1 = H.scale (H.normalize (p1 - p0)) (max v minVel)
            s' = s{ pureHero = hero{ heroVel = H.len v1 }}
        in  ([HeroVelocity v1], s')

onNothing :: State Pure [Query]
onNothing = state $ phi
    where phi s  
            | isGameOver s           = ([], setGameOver s)       
            | pureGameOver s         = ([], s)
            | tick == creationPeriod = 
                    (Step : def ++ createBall s, setTick 0 s)
            | otherwise              = 
                    (Step : def, setTick (succ tick) s)
            where create = pureCreation s
                  tick = creationTick create
                  setTick a s = s{ pureCreation = create{ creationTick = a } }  
                  def = removeOutliners s


setGameOver a = a{ pureGameState = WaitForClick 1 }

isGameOver :: Pure -> Bool
isGameOver s 
    = (not $ insideFrame $ heroPos $ pureHero s)
    || 0 > (scoresLives $ pureScores s)    

reset :: Pure -> Pure
reset a = a
    { pureScores   = initScores
    , pureCreation = initCreation
    , pureGameState= On }


createBall :: Pure -> [Query]
createBall st = maybeToList $ fmap MakeBall $ freq goal cur
    where goal = creationGoalStat   $ pureCreation st
          cur  = creationStat       $ pureCreation st


freq :: Stat -> Stat -> Maybe Freq
freq goal cur 
    | isLess s      = Just res
    | otherwise     = Nothing
    where dt g = max 0 $ g goal - g cur
          f  g = toEnum (g s) / toEnum (total s)
          isLess s = s /= (Stat 0 0 0)           

          s   = Stat (dt goodCount) (dt badCount) (dt bonusCount) 
          res = Freq (f goodCount)  (f badCount)  (f bonusCount)
          total (Stat a b c) = a + b + c   


removeOutliners :: Pure -> [Query]
removeOutliners = 
    fmap Remove . filter (not . insideFrame . ballPos) . pureBalls

insideFrame :: H.Position -> Bool
insideFrame (H.Vector x y) = abs x <= w2 && abs y <= h2
    where w2 = windowWidth / 2 
          h2 = windowHeight / 2   
          


onTouch :: Ball -> State Pure [Query]
onTouch b = state $ \s -> case ballType b of
        Hero    -> ([], s)
        Good    -> ([Remove b], updLive (min livesNum . succ) s)
        Bad     -> ([], updLive pred s)
        Bonus   -> let  xs = removeBonus (ballPos b) s
                   in   (fmap Remove xs, setTouch xs $ 
                            updBonus (length xs) s) 

setTouch xs s = s{ pureTouch = Just (bonusSleep, xs) }

updScores f s = s{ pureScores = f $ pureScores s }
          
updLive  f = updScores (\a -> a{ scoresLives = f $ scoresLives a }) 

updCreation f = \a -> a{ pureCreation = f $ pureCreation a } 

updGoalStat f = updCreation $ 
    \a -> a{ creationGoalStat = f $ creationGoalStat a }


updBonus :: Int -> Pure -> Pure
updBonus n a    
    | isLevelUp level' level    = updGoalStat (makeItHarder level') a'
    | otherwise                 = a'
    where (bonus', level') = countBonus n bonus level
          bonus = scoresBonus sco   
          level = scoresLevel sco  
          a' = a{ pureScores = sco
                      { scoresBonus = bonus'
                      , scoresLevel = level'} }
          sco = pureScores a
          isLevelUp new old = new > old
          makeItHarder level a = a
            { badCount   = incrBad level $ badCount a
            , bonusCount = incrBonus level $ bonusCount a
            , goodCount  = incrGood level $ goodCount a
            }



{-
updBonus f = updScores (\a -> 
    let (newBonus, newLevel) = f (scoresBonus a) (scoresLevel a)
    in  a{ scoresBonus = newBonus
         , scoresLevel = newLevel
        }) 
-}
countBonus n bonus level  
    | bonus' > bonusNum = (bonus' - bonusNum, level + 1)
    | otherwise         = (bonus', level)
    where bonus' = bonus + (fromEnum $ count $ fromIntegral $ n - 1) 
          count x = 0.5*x*(x + 1)
            
removeBonus :: H.Position -> Pure -> [Ball]
removeBonus pos = filter (closeTo pos . ballPos) . pureBalls
    where closeTo p1 p2 = H.len (p1 - p2) < bonusRadius


----------------------------------------------------------
-- drawing

picture :: Pure -> Picture
picture a = mconcat [
    drawScores $ pureScores a, 
    drawWalls, 
    drawHero $ pureHero a, 
    mconcat $ fmap drawBall $ pureBalls a,
    maybe mempty (drawTouch $ heroPos $ pureHero a) $ pureTouch a
    ]


drawHero :: HeroBall -> Picture
drawHero = drawObj Hero . heroPos 

drawBall :: Ball -> Picture
drawBall b = drawObj (ballType b) (ballPos b)

drawObj objType objPos = 
    Prim (type2color objType) (CircleSolid (vec2point objPos) ballRadius)
    where type2color a = case a of
                Hero    -> red
                Good    -> green
                Bad     -> blue
                Bonus   -> orange

drawWalls :: Picture
drawWalls = mconcat $ map f wallPoints 
    where f (a, b) = Prim (Color 0 0 0) $ Line (vec2point a) (vec2point b)
          
vec2point :: H.Vector -> Point
vec2point (H.Vector x y) = Point x y

scorePosition = Point (-frameWidth/2) (frameHeight/2 + 20)


drawScores :: Scores -> Picture
drawScores a = mconcat [
    drawBonus a, 
    drawLives (scoresLives a)]


drawLives :: Int -> Picture
drawLives n = drawScoresRect livesRect livesNum n (col1 n) (col2 n)
    where col1 n
            | n <= 1    = red
            | otherwise = blue
          col2 n
            | n <= 3    = red
            | otherwise = green

drawScoresRect f lim val col1 col2 = mconcat [
    Prim col2 (uncurry RectSolid $ f $ 
            fromIntegral (max 0 val) / fromIntegral lim),
    Prim col1 (uncurry Rect $ f 1) ]

    
drawTouch :: H.Position -> (Int, [Ball]) -> Picture
drawTouch center = (rim <>) . mconcat . fmap (drawBall . toBonus) . snd
    where toBonus b = b{ ballType = Bonus }
          rim = Prim orange $ Circle (vec2point center) bonusRadius 


drawBonus :: Scores -> Picture
drawBonus a = Prim black 
    (Text resultPoint $ "Bonus: " ++ (show $ totalBonus a))

totalBonus :: Scores -> Int
totalBonus a = scoresLevel a * bonusNum + scoresBonus a
