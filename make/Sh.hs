{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Sh
    ( Sh, sh
    , roll, unRoll
    , run, run_
    , ls, mv, cp, cp_r, mkdir
    , rm, rm_f, rm_rf 
    , touch
    , hasExt
    , readfile, writefile, appendfile
    , find, findWhen     

    , module Control.Monad.IO.Class
    , module Filesystem.Path
    ) where

import Prelude hiding (FilePath)

import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad

import qualified Shelly as S
import qualified Shelly.Find as S

import Filesystem.Path(FilePath)
import qualified Filesystem.Path as Fs

import Data.Text.Lazy as LT hiding (concat, all, find)

default (LT.Text)

newtype Sh a = Sh { unSh :: S.ShIO [a] }

instance Functor Sh where
    fmap f = Sh . fmap (fmap f) . unSh    

instance Monad Sh where
    return =  Sh . return . return 
    a >>= f = Sh $ fmap concat $ mapM (unSh . f) =<< unSh a
    a >> b = Sh $ unSh a >> unSh b

instance Applicative Sh where
    pure = return
    (<*>) = ap

instance MonadPlus Sh where
    mzero = Sh $ return []
    mplus a b = Sh $ liftA2 (++) (unSh a) (unSh b)

instance MonadIO Sh where
    liftIO = sh1 . liftIO

------------------------------------------

sh1 = Sh . fmap return

sh :: Sh () -> IO ()
sh = (>> return ()) . shelly 

shelly :: MonadIO m => Sh a -> m [a]
shelly = S.shelly . unSh


unRoll :: Sh a -> Sh [a]
unRoll = Sh . fmap return . unSh 

roll :: Sh [a] -> Sh a
roll = Sh . fmap concat . unSh

ls :: FilePath -> Sh FilePath
ls = Sh . S.ls

absPath :: FilePath -> Sh FilePath
absPath = sh1 . S.absPath

mv :: FilePath -> FilePath -> Sh ()
mv a b = sh1 $ S.mv a b

rm :: FilePath -> Sh ()
rm = sh1 . S.rm

rm_f :: FilePath -> Sh ()
rm_f = sh1 . S.rm_f

rm_rf :: FilePath -> Sh ()
rm_rf = sh1 . S.rm_rf

cp :: FilePath -> FilePath -> Sh ()
cp a b = sh1 $ S.cp a b

cp_r :: FilePath -> FilePath -> Sh ()
cp_r a b = sh1 $ S.cp_r a b

mkdir :: FilePath -> Sh ()
mkdir = sh1 . S.mkdir

-----------------

run :: FilePath -> [Text] -> Sh Text
run a b = sh1 $ S.run a b

run_ :: FilePath -> [Text] -> Sh ()
run_ a b = sh1 $ S.run_ a b

----------------

touch :: FilePath -> Sh ()
touch = flip writefile ""

readfile :: FilePath -> Sh Text
readfile = sh1 . S.readfile

writefile :: FilePath -> Text -> Sh ()
writefile a b = sh1 $ S.writefile a b

appendfile :: FilePath -> Text -> Sh ()
appendfile a b = sh1 $ S.appendfile a b

---------------
-- filesystem extension

hasExt :: Text -> FilePath -> Bool
hasExt = flip Fs.hasExtension . toStrict

------------------
-- finds

find :: FilePath -> Sh FilePath
find = Sh . S.find

findWhen :: FilePath -> (FilePath -> Sh Bool) -> Sh FilePath
findWhen a p = Sh $ S.findWhen a (fmap and . unSh . p)


