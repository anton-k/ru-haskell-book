module Utils where

import qualified Physics.Hipmunk as H
import qualified Graphics.Rendering.OpenGL as G
import Types

vec :: H.CpFloat -> H.CpFloat -> H.Vector
vec = H.Vector

un :: a 
un = undefined

d2gl :: Double -> G.GLfloat
d2gl = realToFrac

d2gli :: Double -> G.GLsizei
d2gli = toEnum . fromEnum . d2gl


vertex2f :: G.GLfloat -> G.GLfloat -> IO ()
vertex2f a b = G.vertex (G.Vertex3 a b 0)

