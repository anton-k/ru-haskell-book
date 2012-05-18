module Graphics where

import Prelude hiding (lines)

import qualified Graphics.Rendering.OpenGL as G
import qualified Graphics.UI.GLFW as G

import Types
import Utils
   
black = Color 0 0 0
red   = Color 1 0 0
green  = Color 0 1 0
blue  = Color 0 0 1
orange = Color 1 0.5 0
yellow = Color 1 1 0 
skyBlue = Color 0 1 1
violet = Color 1 0 1

raduga = [red, orange, yellow, green, skyBlue, blue, violet]

draw :: Picture -> IO ()
draw a = do
    G.clear [G.ColorBuffer]
    foldPicture (return ()) drawPrim (>>) a
    G.swapBuffers
    where drawPrim (Color r g b) prim = do
            G.color $ G.Color4 (d2gl r) (d2gl g) (d2gl b) 1
            drawPrimitive prim    


drawPrimitive :: Primitive -> IO ()
drawPrimitive a = case a of
    Line pa pb          -> lines [glPoint pa, glPoint pb]
    Circle pc rad       -> lineLoop $ circlePoints (glPoint pc) (d2gl rad)  
    Rect pa pb          -> lineLoop $ rectPoints (glPoint pa) (glPoint pb) 
    CircleSolid pc rad  -> polygon  $ circlePoints (glPoint pc) (d2gl rad)
    RectSolid pa pb     -> polygon  $ rectPoints (glPoint pa) (glPoint pb)
    Text pa msg         -> drawText (glPoint pa) msg
    where glPoint (Point x y) = (d2gl x, d2gl y)


type GLpoint = (G.GLfloat, G.GLfloat)

circlePoints (cx, cy) rad = zip xs ys
    where n = 50
          xs = fmap (\x -> cx + rad * sin (2*pi*x/n)) [0 .. n]
          ys = fmap (\x -> cy + rad * cos (2*pi*x/n)) [0 .. n]

rectPoints (ax, ay) (bx, by) = 
    [(ax, ay), (ax, by), (bx, by), (bx, ay), (ax, ay)]


lines       = primitive G.Lines
lineLoop    = primitive G.LineLoop
polygon     = primitive G.Polygon 

primitive primType = 
    G.renderPrimitive primType . mapM_ (uncurry vertex2f) 



drawText :: GLpoint -> String -> IO ()
drawText (a, b) msg = G.preservingMatrix $ 
    G.translate (G.Vector3 a b 0) >>
    G.scale (1.5::G.GLfloat) 1.5 1 >> G.renderString G.Fixed8x16 msg
    

    

