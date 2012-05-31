{-# LANGUAGe NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Data.List
import Codec.Binary.UTF8.String

-- colors
ciColor = red
alColor = black
arColor = blue
axColor = blue
luColor = green 
ldColor = orange

station c = scale 0.5 $ circle 0.5 # lc c # lw 0.2 # fc white
    <> circle 0.9 # lw 0 # fc white

-- circle line
-- ci

-- points :

ci  = station ciColor
ciL = seg ciColor

ciPs = [(0.5,4), (3,2), (3.5, 0), (3.4, -3.4), 
          (0,-5), (-3,-3), (-3,0), (-2,4), (0.5,4)]
    
-- axis line
-- al

al  = station alColor
alL = seg alColor
    
alPs = [(-3,7), (-2,4), (0,1)]

-- axis line
-- ar
ar = station arColor
arL = seg arColor

arPs = [(1,7), (0.5,4), (0,1)]

-- axis line
-- ax
ax = station axColor
axL = seg axColor

axPs = [(0,1), (0,-1), (0,-3), (0,-5), (-1,-7)]

-- legs upper line
-- lu
lu = station luColor
luL = seg luColor

luPs = [(-7,-1), (-3,0), (0,1), (3,2), (5,4), (8,6), (11,7)]

-- legs lower line
-- ld 
ld = station ldColor
ldL = seg ldColor

ldPs = [(-7,-4), (-3,-3), (0,-3), (3.5,0), (6,1)]


seg c ps = lc c $ lw 0.4 $ stroke $ fromVertices $ fmap P ps

res' = mconcat $ mconcat $ 
    [(fmap (\v -> translate v ci) ciPs),
    (fmap (\v -> translate v ar) arPs),
    (fmap (\v -> translate v al) alPs),
    (fmap (\v -> translate v ax) axPs),
    (fmap (\v -> translate v lu) luPs),
    (fmap (\v -> translate v ld) ldPs),
    [ciL ciPs, alL alPs, arL arPs, axL axPs, luL luPs, ldL ldPs]    
    ]
    
text' = scale 0.7 . text . encodeString

res = pad 1.2 $  position [
    (P (-5, 8), text' "Космодром"),
    (P (1, 8), text' "Запад"),
    (P (-2, -8), text' "Восток"),
    (P (-8, -1), text' "Юг"),
    (P (8, 0.5), text' "Север"),
    (P (6.5,-3), text' "пл.Шекспира"),
    (P (2,-5.5), text' "Родник"),
    (P (1.7,-3.2), text' "Крест"),
    (P (1.2,-1.0), text' "Де"),
    
    (P (10,8), text' "Таинственный"),
    (P (12.5,7), text' "лес"),

    (P (5.5,6), text' "Призрак"),

    (P (8.5, 3.7), text' "Троллев мост"),
    (P (-6, -5), text' "Дно болота"),

    (P (-5, 4), text' "ул.Булычёва"),

    (P (1.5, 0.5), text' "Звезда"),
    (P (-4.5, 0.8), text' "Сириус"),
    (P (-5, -2.5), text' "пл.Баха"),
    (P (4.7, -0.5), text' "Лао"),
    (P (2.3, 4.3), text' "Инева"),
    (P (4.7, 2), text' "Тилль"),

    (P (0, 0), res')
    ]


main = defaultMain res
