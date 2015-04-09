module Main where

import System.Environment
import Control.Monad (liftM)

import Wyas.Parser
import Wyas.Evaluator
import Wyas.Error
import Wyas.Repl

main :: IO()
main = do args <- getArgs
          case length args of
               0 -> runRepl
               1 -> evalAndPrint $ head args
               _ -> putStrLn "Program takes only 0 or 1 argument"
