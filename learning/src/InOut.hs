module InOut where

import Data.Char
main = do
    putStrLn "Hello, what is your name?"
    name <- getLine
    let capName = map toUpper name
    putStrLn ("Hey " ++ capName ++ ", you rock!")