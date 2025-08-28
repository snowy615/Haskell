module HelloWorld where

helloWorld :: IO ()
helloWorld = putStrLn "Hello World"

greetPerson :: String -> IO ()
greetPerson name = putStrLn ("Hello, " ++ name ++ "!")