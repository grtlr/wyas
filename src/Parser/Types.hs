module Parser.Types where

import Data.Complex (Complex)
import Data.Vector

data LispVal = Nil ()
             | Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (Vector LispVal)
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             deriving (Eq, Show)
