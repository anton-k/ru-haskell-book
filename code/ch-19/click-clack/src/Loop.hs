module Loop where

import Data.IORef
import Data.StateVar
import qualified Graphics.UI.GLFW as G
import qualified Graphics.Rendering.OpenGL as G
import qualified Physics.Hipmunk as H

import System.Exit

import World 
import Types
import Inits
import Utils

gameLoop :: IO ()
gameLoop = do
    initGame
    loop =<< newIORef =<< initWorld


loop :: IORef World -> IO ()
loop worldRef = do
    world <- get worldRef
    drawWorld world
    (world, dt) <- updateWorld world
    worldRef $= world
    G.sleep (max 0 $ frameTime - dt) 
    loop worldRef
 

initGame :: IO ()
initGame = do 
    H.initChipmunk
    initGLFW

-- initGLFW

initGLFW :: IO ()
initGLFW = do
    G.initialize
    G.openWindow (G.Size (d2gli windowWidth) (d2gli windowHeight)) 
        [] G.Window
    G.windowTitle $= title
    G.windowCloseCallback $= exitWith ExitSuccess
    G.windowSizeCallback  $= (\size -> G.viewport $= (G.Position 0 0, size))
    G.clearColor $= G.Color4 1 1 1 1
    G.blend      $= G.Enabled
    G.blendFunc  $= (G.SrcAlpha, G.OneMinusSrcAlpha)
    G.ortho (-dw2) (dw2) (-dh2) (dh2) (-1) 1
    where dw2 = realToFrac $ windowWidth / 2
          dh2 = realToFrac $ windowHeight / 2
