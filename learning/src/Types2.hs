module Types2 where 

import Data.List
-- import Data.List(nub, sort)       =import only nub and sort
-- import Data.List hiding (nub)     =import everything except nub
import qualified Data.Map as Map

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub