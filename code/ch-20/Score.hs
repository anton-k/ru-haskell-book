module Score where

import Track
import qualified Codec.Midi as M

import Control.Arrow(first, second)
import Data.List(sortBy, groupBy, partition)
import Data.Function (on)

type Score = Track Double Note

data Note = Note {
    noteInstr   :: Instr,
    noteVolume  :: Volume,
    notePitch   :: Pitch,
    isDrum      :: Bool  }

type Instr  = Int
type Volume = Int
type Pitch  = Int

---------------------------------------------------------
-- Pitch

note :: Int -> Score
note n = Track 1 [Event 0 1 (Note 0 64 (60+n) False)]


a, b, c, d, e, f, g,
    as, bs, cs, ds, es, fs, gs,
    af, bf, cf, df, ef, ff, gf :: Score


c   = note 0
cs  = note 1
d   = note 2
ds  = note 3
e   = note 4
f   = note 5
fs  = note 6
g   = note 7
gs  = note 8
a   = note 9
as  = note 10
b   = note 11

es  = f
bs  = c

cf  = b
df  = cs
ef  = ds
ff  = e
gf  = fs
af  = gs
bf  = as

-- Octave

higher :: Int -> Score -> Score
higher n = fmap (\a -> a{ notePitch = 12*n + notePitch a })

lower :: Int -> Score -> Score 
lower n = higher (-n)

high :: Score -> Score
high = higher 1

low :: Score -> Score 
low = lower 1

-------------------------------------------
-- Time

bn, hn, qn, en, sn :: Score -> Score

-- двойная длительность (brewis note)
bn = stretch 2

-- половина (half note)
hn = stretch 0.5

-- четверть (quater note)
qn = stretch 0.25

-- восьмая (eighth note)
en = stretch 0.125

-- шестнадцатая (sixth note)
sn = stretch 0.0625

-------------------------------------------
-- Volume

louder :: Int -> Score -> Score
louder n = fmap $ \a -> a{ noteVolume = n + noteVolume a }

quieter :: Int -> Score -> Score
quieter n = louder (-n)

-------------------------------------------
-- Instrument

instr :: Int -> Score -> Score
instr n = fmap $ \a -> a{ noteInstr = n, isDrum = False }

drum :: Int -> Score -> Score
drum n = fmap $ \a -> a{ notePitch = n, isDrum = True }

bam :: Int -> Score
bam n = Track 1 [Event 0 1 (Note 0 n 0 True)]

-------------------------------------------
-- Rest

rest :: Double -> Score
rest = silence

wnr = rest 1

bnr = bn wnr
hnr = hn wnr
qnr = qn wnr
enr = en wnr
snr = sn wnr

--------------------------------------------
-- Render

render :: Score -> M.Midi
render s = M.Midi M.SingleTrack timeDiv [toTrack s]

timeDiv :: M.TimeDiv
timeDiv = M.TicksPerBeat 96

toTrack :: Score -> M.Track M.Ticks
toTrack = addEndMsg . tfmTime . mergeInstr . groupInstr

addEndMsg :: M.Track M.Ticks -> M.Track M.Ticks
addEndMsg = (++ [(0, M.TrackEnd)])

tfmTime :: M.Track Double -> M.Track M.Ticks
tfmTime = M.fromAbsTime . M.fromRealTime timeDiv . 
     sortBy (compare `on` fst)

type MidiEvent = Event Double Note

groupInstr :: Score -> ([[MidiEvent]], [MidiEvent])
groupInstr = first groupByInstrId . 
    partition (not . isDrum . eventContent) . 
    alignEvents . trackEvents
    where groupByInstrId = groupBy ((==) `on` instrId) . 
                           sortBy  (compare `on` instrId)

mergeInstr :: ([[MidiEvent]], [MidiEvent]) -> M.Track Double
mergeInstr (instrs, drums) = concat $ drums' : instrs'
    where instrs' = zipWith setChannel ([0 .. 8] ++ [10 .. 15]) instrs
          drums'  = setDrumChannel drums  

setChannel :: M.Channel -> [MidiEvent] -> M.Track Double
setChannel ch ms = case ms of
    []      -> []
    x:xs    -> (0, M.ProgramChange ch (instrId x)) : (fromEvent ch =<< ms)
    

setDrumChannel :: [MidiEvent] -> M.Track Double
setDrumChannel ms = fromEvent drumChannel =<< ms 
    where drumChannel = 9
                                                      
instrId = noteInstr . eventContent


fromEvent :: M.Channel -> MidiEvent -> M.Track Double
fromEvent ch e = [
    (eventStart e, noteOn n), 
    (eventStart e + eventDur e, noteOff n)]
    where n = clipToMidi $ eventContent e
          noteOn  n = M.NoteOn  ch (notePitch n) (noteVolume n)
          noteOff n = M.NoteOff ch (notePitch n) 0

clipToMidi :: Note -> Note
clipToMidi n = n { 
    notePitch   = clip $ notePitch n,
    noteVolume  = clip $ noteVolume n }
    where clip = max 0 . min 127


alignEvents :: [MidiEvent] -> [MidiEvent]
alignEvents es 
    | d < 0     = map (delay (abs d)) es
    | otherwise = es
    where d = minimum $ map eventStart es
