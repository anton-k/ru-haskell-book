module Calendar where

import Prelude (Int, String, Char, Show(..), (++))

-- Дата
data Date = Date Year Month Day

-- Год
data Year  = Year Int       -- Int это целые числа

-- Месяц
data Month =  January       -- Январь
            | February      -- Февраль
            | March         -- Март
            | April         -- Апрель
            | May           -- Май
            | June          -- Июнь
            | July          -- Июль
            | August        -- Август
            | September     -- Сентябрь
            | October       -- Октябрь
            | November      -- Ноябрь            
            | December      -- Декабрь

data Day = Day Int

-- Неделя
data Week =   Monday        -- Понедельник
            | Tuesday       -- Вторник
            | Wednesday     -- Среда
            | Thursday      -- Четверг
            | Friday        -- Пятница
            | Saturday      -- Суббота
            | Sunday        -- Воскресенье

-- Время
data Time = Time Hour Minute Second

data Hour   = Hour   Int    -- Час
data Minute = Minute Int    -- Минута
data Second = Second Int    -- Секунда



------------------------------------------------------
-- Экземпляры для Show


instance Show Week where
    show Monday     = "Mon"
    show Tuesday    = "Tue"
    show Wednesday  = "Wed"
    show Thursday   = "Thu"  
    show Friday     = "Fri" 
    show Saturday   = "Sat"
    show Sunday     = "Sun"



instance Show Time where
    show (Time h m s) = show h ++ ":" ++ show m ++ ":" ++ show s

instance Show Hour where
    show (Hour h) = addZero (show h)

instance Show Minute where
    show (Minute m) = addZero (show m)

instance Show Second where
    show (Second s) = addZero (show s)

addZero :: String -> String
addZero (a:[]) = '0' : a : []
addZero as     = as
