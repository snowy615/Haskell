module Geometry
  ( sphereVolume
  , sphereArea
  , cubeVolume
  , cubeArea
  , Shape(..)
  , Point(..)
  , surface
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
surface (Circle _ r) = pi * r ^2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs(x2 - x1) * abs(y2 - y1)

data Point = Point Float Float
    deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point
    deriving (Show)