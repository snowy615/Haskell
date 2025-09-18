import Data.List

module Types2 where 

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub