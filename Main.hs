module Main where
import System.Environment

main :: IO()
main = do
    putStrLn "What's your name, what's your number?"
    name <- getLine
    putStrLn $ "I would like to get to know you " ++ name ++ "."
