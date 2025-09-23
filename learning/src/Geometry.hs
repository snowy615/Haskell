module Geometry
  ( sphereVolume
  , sphereArea
  , cubeVolume
  , cubeArea
  , Shape(..)
  ) where

  
sphereVolume :: Float -> Float
sphereVolume r = (4.0 / 3.0) * pi * r ^ 3

sphereArea :: Float -> Float
sphereArea r = 4.0 * pi * r ^ 2

cubeVolume :: Float -> Float
cubeVolume a = a ^ 3

cubeArea :: Float -> Float
cubeArea a = 6 * a ^ 2

surface ::Shape -> Float
surface (Circle _ _ r) = pi * r ^2
surface (Rectangle x1 y1 x2 y2) = 2 * (x1 * y1 + x1 * y2 + x2 * y1)

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
    deriving (Show)