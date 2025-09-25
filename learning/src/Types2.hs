module Types2 where 

import Data.List
-- import Data.List(nub, sort)       =import only nub and sort
-- import Data.List hiding (nub)     =import everything except nub
import qualified Data.Map as Map
import Data.Char
-- import Data.Set as Set
-- import Geometry

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = 
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

encode2 :: Int -> String -> String
encode2 shift msg = 
    let ords = map ord msg
        shifted = map (+ shift) ords
    in map chr shifted

decode2 :: Int -> String -> String
decode2 shift msg = encode2 (negate shift) msg

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

-- record syntax
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

infixr 5 .++
(.++) :: [a] -> [a] -> [a]
[] .++ ys = ys
(x:xs) .++ ys = x : (xs .++ ys)

solveRPN :: (Num a) => String -> a
solveRPN = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:ys) "*" = (x * y):ys
          foldingFunction (x:y:ys) "+" = (x + y):ys
          foldingFunction (x:y:ys) "-" = (y - x):ys
          foldingFunction (x:y:ys) "/" = (y `div` x):ys
          foldingFunction xs numberString = read numberString : xs