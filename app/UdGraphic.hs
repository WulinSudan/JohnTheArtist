module UdGraphic (
    Comanda(..),
    Distancia,
    Angle
    )
    where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT
    ( mainLoop,
      displayCallback,
      reshapeCallback,
      getArgsAndInitialize,
      initialDisplayMode,
      initialWindowSize,
      createWindow,
      swapBuffers,
      loadIdentity,
      viewport,
      clear,
      DisplayMode(DoubleBuffered),
      Position(Position),
      Size(..),
      ClearBuffer(ColorBuffer),
      HasSetter(($=)) )
import Data.IORef ()
import Data.List ()
import Control.Monad( liftM, liftM2, liftM3 )
import System.Random ()
import Test.QuickCheck
    ( Arbitrary(arbitrary), elements, oneof, sized )

infixr 5 :#:

-- Punts

data Pnt = Pnt Float Float
  deriving (Eq,Ord,Show)

instance Num Pnt where
  Pnt x y + Pnt x' y'  =  Pnt (x+x') (y+y')
  Pnt x y - Pnt x' y'  =  Pnt (x-x') (y-y')
  Pnt x y * Pnt x' y'  =  Pnt (x*x') (y*y')
  fromInteger          =  scalar . fromInteger
  abs (Pnt x y)        =  Pnt (abs x) (abs y)
  signum (Pnt x y)     =  Pnt (signum x) (signum y)

instance Fractional Pnt where
  Pnt x y / Pnt x' y'  =  Pnt (x/x') (y/y')
  fromRational         =  scalar . fromRational

scalar :: Float -> Pnt
scalar x  =  Pnt x x

scalarMin :: Pnt -> Pnt
scalarMin (Pnt x y)  =  scalar (x `min` y)

scalarMax :: Pnt -> Pnt
scalarMax (Pnt x y)  =  scalar (x `max` y)

dimensions :: Pnt -> (Int,Int)
dimensions (Pnt x y)  =  (ceiling x, ceiling y)

lub :: Pnt -> Pnt -> Pnt
Pnt x y `lub` Pnt x' y'  =  Pnt (x `max` x') (y `max` y')

glb :: Pnt -> Pnt -> Pnt
Pnt x y `glb` Pnt x' y'  =  Pnt (x `min` x') (y `min` y')

pointToSize :: Pnt -> Size
pointToSize (Pnt x y) = Size (ceiling x) (ceiling y)

sizeToPoint :: Size -> Pnt
sizeToPoint (Size x y) = Pnt (fromIntegral x) (fromIntegral y)

-- Colors

data Llapis = Color' GL.GLfloat GL.GLfloat GL.GLfloat
            | Transparent
            deriving (Eq, Ord, Show)

pencilToRGB :: Llapis -> GL.Color3 GL.GLfloat
pencilToRGB (Color' r g b)  =  GL.Color3 r g b
pencilToRGB Transparent  =  error "pencilToRGB: transparent"

blanc, negre, vermell, verd, blau :: Llapis
blanc   = Color' 1.0 1.0 1.0
negre   = Color' 0.0 0.0 0.0
vermell = Color' 1.0 0.0 0.0
verd    = Color' 0.0 1.0 0.0
blau    = Color' 0.0 0.0 1.0

-- Lines

data Ln = Ln Llapis Pnt Pnt
  deriving (Eq,Ord,Show)


-- Window parameters

theCanvas :: Pnt
theCanvas  =  Pnt 800 800

theBGcolor :: GL.Color3 GL.GLfloat
theBGcolor = pencilToRGB blanc



-- Main drawing and window functions

display :: Comanda -> IO ()
display c = do
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize  $= pointToSize theCanvas
  getArgsAndInitialize
  w <- createWindow "pencilcil Graphics"
  displayCallback $= draw c
  reshapeCallback $= Just (\x -> (viewport $= (Position 0 0, x)))
  --actionOnWindowClose $= ContinueExectuion
  draw c
  mainLoop

draw :: Comanda -> IO ()
draw c = do clear [ColorBuffer]
            loadIdentity
            background
            toGraphic $ rescale $ execute c
            swapBuffers

toGraphic :: [Ln] -> IO ()
toGraphic lines  = sequence_ (map f lines)
  where
  f (Ln pencil startP endP)  =
    GL.color (pencilToRGB pencil) >>
    GL.renderPrimitive GL.LineStrip (toVertex startP >> toVertex endP)

background :: IO ()
background = do GL.color theBGcolor
                GL.renderPrimitive GL.Polygon $ mapM_ GL.vertex
                      [GL.Vertex3 (-1) (-1) 0,
                       GL.Vertex3   1  (-1) 0,
                       GL.Vertex3   1    1  0,
                       GL.Vertex3 (-1)   1 (0::GL.GLfloat) ]


toVertex (Pnt x y)  =  GL.vertex $ GL.Vertex3
 (realToFrac x) (realToFrac y) (0::GL.GLfloat)



-- Definició de les comandes per moure el llapis

                
type Angle     = Float
type Distancia = Float
data Comanda   = Avança Distancia
               | Gira Angle
               | Para
               | Comanda :#: Comanda
               | CanviarColor Llapis 
               | Branca Comanda deriving Show

  
-- Problema 8
-- Pas de comandes a lines a pintar per GL graphics

execute' :: Comanda -> Pnt -> Angle -> Llapis -> [Ln] -> ([Ln], Pnt, Angle)
execute' (Avança dst) pnt angle llapis lines =
  let newPnt = pnt + vectorDesplaçament angle dst
      newLine = Ln llapis pnt newPnt
  in (newLine : lines, newPnt, angle)

execute' (Gira angle') pnt currAngle llapis lines =
  let newAngle = currAngle + angle'
  in (lines, pnt, newAngle)

execute' (c1 :#: c2) pnt angle llapis lines =
  let (lines1, pnt1, angle1) = execute' c1 pnt angle llapis lines
      (lines2, pnt2, angle2) = execute' c2 pnt1 angle1 llapis lines1
  in (lines2, pnt2, angle2)

execute' Para pnt angle llapis lines = (lines, pnt, angle)

vectorDesplaçament :: Angle -> Distancia -> Pnt
vectorDesplaçament angle dst = Pnt (dst * cos (-rads)) (dst * sin (-rads))
  where rads = degToRad angle

-- Graus a radians 
degToRad :: Floating a => a -> a
degToRad deg = deg * pi / 180

-- Modifica també la funció execute per treure els resultats correctament:

execute :: Comanda -> [Ln]
execute c = let (lines, _, _) = execute' c (Pnt 0 0) 0 negre []
            in reverse lines

-- >>> execute $ (Avança 30 :#: Para :#: Gira 10) :#: Avança 20
-- [Ln (Color' 0.0 0.0 0.0) (Pnt 0.0 0.0) (Pnt 30.0 0.0),Ln (Color' 0.0 0.0 0.0) (Pnt 30.0 0.0) (Pnt 49.696156 (-3.4729638))]



-- Rescales all points in a list of lines
--  from an arbitrary scale
--  to (-1.-1) - (1.1)

rescale :: [Ln] -> [Ln]
rescale lines | points == [] = []
              | otherwise    = map f lines
  where
  f (Ln pencil p q)  =  Ln pencil (g p) (g q)
  g p             =  swap ((p - p0) / s)
  points          =  [ r | Ln pencil p q <- lines, r <- [p, q] ]
  hi              =  foldr1 lub points
  lo              =  foldr1 glb points
  s               =  scalarMax (hi - lo) * scalar (0.55)
  p0              =  (hi + lo) * scalar (0.5)
  swap (Pnt x y)  =  Pnt y x


-- Generators for QuickCheck

instance Arbitrary Llapis where
    arbitrary  =  sized pencil
        where
          pencil n  =  elements [negre,vermell,verd,blau,blanc,Transparent]


instance Arbitrary Comanda where
    arbitrary  =  sized cmd
        where
          cmd n  |  n <= 0     =  oneof [liftM (Avança . abs) arbitrary,
                                         liftM Gira arbitrary ]
                 |  otherwise  =  liftM2 (:#:) (cmd (n `div` 2)) (cmd (n `div`2))
