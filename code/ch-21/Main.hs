module Main where

import System

import Track
import Score
import Codec.Midi

out = (>> system "timidity tmp.mid") . exportFile "tmp.mid" . render


closedHiHat = drum 42
rideCymbal  = drum 59
cabasa      = drum 69
maracas     = drum 70
tom         = drum 45

flute        = instr 73

b1 = bam 100
b0 = bam 84

drums1 = loop 80 $ chord [
    tom   $ line [qn b1, qn b0, hnr],
    maracas $ line [hnr, hn b0] 
    ]
   
drums2 = quieter 20 $ cabasa $ loop 120 $ en $ line [b1, b0, b0, b0, b0]

drums3 = closedHiHat $ loop 50 $ en (line [b1, loop 12 wnr])

drums = drums1 =:= drums2 =:= drums3

c7  = chord [c, e, b]
gs7 = chord [low af, c, g]
g7  = chord [low g, low bf, f]

harmony = loop 12 $ lower 1 $ bn $ line [bn c7, gs7, g7]

ac = louder 5

mel1 = bn $ line [bnr, subMel, ac $ stretch (1+1/8) e, c,
    subMel, enr]
    where subMel = line [g, stretch 1.5 $ qn g, qn f, qn g]

mel2 = loop 2 $ qn $ line [subMel, ac $ bn ds, c, d, ac $ bn c, c, c, wnr,
     subMel, ac $ bn g, f, ds, ac $ bn f, ds, ac $ bn c]
    where subMel = line [ac ds, c, d, ac $ bn c, c, c]

mel3 = loop 2 $ line [pat1 (high c) as g, pat1 g f d] 
    where pat1 a b c = line [pat a, loop 3 qnr, wnr, 
                pat b, qnr, hnr, pat c, qnr, hnr]
          pat  x     = en (x +:+ x)  

--mel = flute $ line [mel3]
mel = flute $ line [mel1, mel2, mel3]

cha = delay (dur mel1 + dur mel2) $ loop 10 $ rideCymbal $ delay 1 b1

res = chord [
    drums,
    harmony,
    high $ mel,
    louder 40 $ cha,
    rest 0]

main = out res
