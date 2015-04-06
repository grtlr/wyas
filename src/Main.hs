module Main where

import System.Environment
import Control.Monad (liftM)

import Wyas.Parser
import Wyas.Evaluator
import Wyas.Error

main :: IO()
main = do args <- getArgs
          evaled <- return $ liftM show $ readExpr (head args) >>= eval
          putStrLn $ extractValue $ trapError evaled
