module Main where

import Graphics.UI.GLFW           
import Graphics.Rendering.OpenGL

import System.Exit

title = "Hello OpenGL"

width, height :: GLsizei

width   = 700
height  = 600
    
w2, h2 :: GLfloat

w2 = (fromIntegral $ width) / 2
h2 = (fromIntegral $ height)  / 2

dw2, dh2 :: GLdouble

dw2 = fromRational $ toRational w2
dh2 = fromRational $ toRational h2

main = do
    initialize
    openWindow (Size width height) [] Window
    windowTitle $= title

    clearColor $= Color4 1 1 1 1
    ortho (-dw2-50) (dw2+50) (-dh2-50) (dh2+50) (-1) 1
   
    windowCloseCallback $= exitWith ExitSuccess
    windowSizeCallback  $= (\size -> viewport $= (Position 0 0, size))

    loop


loop = do
    display
    loop


display = do
    clear [ColorBuffer]

    color black
    line (-w2) (-h2) (-w2) h2
    line (-w2) h2    w2    h2
    line w2    h2    w2    (-h2)
    line w2   (-h2)  (-w2) (-h2)

    color red
    circle 0 0 10

    swapBuffers
    

vertex2f :: GLfloat -> GLfloat -> IO ()
vertex2f a b = vertex (Vertex3 a b 0)


-- colors

white = Color4 (0::GLfloat)
black = Color4 (0::GLfloat) 0 0 1
red   = Color4 (1::GLfloat) 0 0 1

-- primitives

line :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
line ax ay bx by = renderPrimitive Lines $ do
    vertex2f ax ay
    vertex2f bx by


circle :: GLfloat -> GLfloat -> GLfloat -> IO ()
circle cx cy rad = 
    renderPrimitive Polygon $ mapM_ (uncurry vertex2f) points
    where n = 50
          points = zip xs ys
          xs = fmap (\x -> cx + rad * sin (2*pi*x/n)) [0 .. n]
          ys = fmap (\x -> cy + rad * cos (2*pi*x/n)) [0 .. n]


