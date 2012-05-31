module Main where

import Graphics.UI.GLFW           
import Graphics.Rendering.OpenGL

import System.Exit

title = "Hello OpenGL"

width   = 700
height  = 600

main = do
    initialize
    openWindow (Size width height) [] Window
    windowTitle $= title
      
    windowCloseCallback $= exitWith ExitSuccess
    clearColor $= Color4 1 1 1 1
    loop

loop = do
    display
    loop

display = do
    clear [ColorBuffer]
    swapBuffers

