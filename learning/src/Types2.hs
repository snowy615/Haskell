module Types2 where 

import Data.List
-- import Data.List(nub, sort)       =import only nub and sort
-- import Data.List hiding (nub)     =import everything except nub
import qualified Data.Map as Map

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = 
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)