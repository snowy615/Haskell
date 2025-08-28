module Types where 
circumference :: Float -> Float
circumference r = 2 * pi * r

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference' :: Double -> Double
circumference' r = 2 * pi * r