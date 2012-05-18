-- | Little example
module Hello(
    -- * Introduction
    -- | Here is the little example to show you
    -- how to make docs with Haddock
    
    -- * Types
    -- | The types.
    T(..),
    -- * Classes
    -- | The classes.
    C(..),
    -- * Functions
    helloWorld
    
) where

import Utility.Hello(hello)
import Utility.World(world)

-- | Here is the comment
data T = A      -- ^ constructor A
       | B      -- ^ constructor B
       | C      -- ^ and so on

-- | C-class
class C a where
    -- | f-function
    f :: a -> a
    -- | g-function
    g :: a -> a


-- | Here is the comment
helloWorld :: String
helloWorld = hello ++ ", " ++ world ++ "!"
