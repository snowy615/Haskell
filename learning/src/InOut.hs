module InOut where

import Data.Char
main = do
    putStrLn "Hello, what is your name?"
    name <- getLine
    feeling <- return "Awesome"
    let capName = map toUpper name
    putStrLn ("Hey " ++ capName ++ ", you rock! " ++ feeling ++ "!")