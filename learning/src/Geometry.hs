module Geometry
  ( sphereVolume
  , sphereArea
  , cubeVolume
  , cubeArea
  ) where

  
sphereVolume :: Float -> Float
sphereVolume r = (4.0 / 3.0) * pi * r ^ 3

sphereArea :: Float -> Float
sphereArea r = 4.0 * pi * r ^ 2

cubeVolume :: Float -> Float
cubeVolume a = a ^ 3

cubeArea :: Float -> Float
cubeArea a = 6 * a ^ 2