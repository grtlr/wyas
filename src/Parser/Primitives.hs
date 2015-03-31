{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parser.Primitives where

import Parser.Types

import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readOct, readHex)
import Data.Ratio ((%))
import Data.Complex (Complex (..))
import Control.Monad (liftM)

parseBool :: Parser LispVal
parseBool = do char '#'
               b <- parseTrue <|> parseFalse
               return $ Bool b
               where parseTrue  = char 't' >> return True
                     parseFalse = char 'f' >> return False

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               return $ Atom (first:rest)

--
-- Char & String
--

parseChar :: Parser LispVal
parseChar = liftM Character (string "#\\" >> parseChar')
          where parseWS = (string " " <|> string "space") >> return ' '
                parseNewline = string "newline" >> return '\n'
                parseChar' = parseWS <|> parseNewline <|> anyChar

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (escapedChars <|> noneOf "\\\"")
                 char '"'
                 (return . String) x

--
-- Parsing Integers
--

parseNumber :: Parser LispVal
parseNumber = parsePlainNumber <|> parseRadixNumber

parsePlainNumber :: Parser LispVal
parsePlainNumber = liftM (Number . read) (many1 digit)

--
-- Radix Numbers
--

parseRadixNumber :: Parser LispVal
parseRadixNumber = char '#' >>
                   (
                        parseDecimal
                        <|> parseBinary
                        <|> parseOct
                        <|> parseHex
                   )

parseDecimal :: Parser LispVal
parseDecimal = do char 'd'
                  n <- many1 digit
                  (return . Number . read) n

parseBinary :: Parser LispVal
parseBinary = do char 'b'
                 n <- many $ oneOf "01"
                 (return . Number . bin2int) n

parseHex :: Parser LispVal
parseHex = do char 'x'
              n <- many $ oneOf "0123456789abcdefABCDEF"
              return . Number . readWith readHex $ n
              where readWith f s = fst $ head (f s)

parseOct :: Parser LispVal
parseOct = do char 'o'
              n <- many $ oneOf "01234567"
              return . Number . readWith readOct $ n
              where readWith f s = fst $ head (f s)

--
-- Numeric Tower
--

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (read (x ++ "." ++y))

parseRational :: Parser LispVal
parseRational = do n <- many1 digit
                   char '/'
                   d <- many1 digit
                   return $ Ratio $ read n % read d

parseComplex :: Parser LispVal
parseComplex = do x <- try parseFloat <|> parsePlainNumber
                  char '+'
                  y <- try parseFloat <|> parsePlainNumber
                  char 'i'
                  return $ Complex (toDouble x :+ toDouble y)

--
-- Helpers
--

escapedChars :: Parser Char
escapedChars = char '\\' >>
               (
                    char '\\' <|> char '"' <|>
                    parseNewline <|>
                    parseReturn <|>
                    parseTab
               )
               where parseNewline = char 'n' >> return '\n'
                     parseReturn  = char 'r' >> return '\r'
                     parseTab     = char 't' >> return '\t'

symbol :: Parser Char
symbol = oneOf "!$%|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

bin2int :: String -> Integer
bin2int = bin2int' 0
        where bin2int' digint "" = digint
              bin2int' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1)
                                       in bin2int' old xs

-- TODO clean up
toDouble :: LispVal -> Double
toDouble (Float f) = f
toDouble (Number n) = fromIntegral n
toDouble _ = error "ERROR: not a double"
