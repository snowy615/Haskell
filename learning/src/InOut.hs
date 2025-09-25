module InOut where

import System.IO
import Data.Char
main = do
    -- putStr "Hi! "
    -- putStrLn "What is your name?"
    -- name <- getLine
    -- feeling <- return "Awesome"
    -- let capName = map toUpper name
    -- putStrLn ("Hey " ++ capName ++ ", you rock! " ++ feeling ++ "!")
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

    -- alternative
    withFile "input.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)

    -- easier
    contents <- readFile "input.txt"
    putStr contents
--writeFile, appendFile

