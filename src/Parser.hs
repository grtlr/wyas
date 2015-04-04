{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parser
    ( readExpr'
    , readExpr
    , parseExpr
    , module Parser.Types
    ) where

import Parser.Types
import Parser.Primitives
import Error

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad (liftM)
import Control.Monad.Error (throwError)
import Data.Vector (fromList)

readExpr' :: String -> String
readExpr' input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseQuoted
         <|> parseQuasiQuoted
         <|> parseUnQuote
         <|> parseAnyList
         <|> try parseVector
         <|> try parseChar
         <|> try parseComplex
         <|> try parseFloat
         <|> try parseRational
         <|> try parseNumber
         <|> try parseBool

--
-- Lists, dotted lists, quoted datums
--

parseAnyList :: Parser LispVal
parseAnyList = do
    char '('
    optional spaces
    h <- sepEndBy parseExpr spaces
    t <- (char '.' >> spaces >> parseExpr) <|> return (Nil ())
    optional spaces
    char ')'
    return $ case t of
               (Nil ()) -> List h
               _ -> DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do char '`'
                      x <- parseExpr
                      return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do char ','
                  x <- parseExpr
                  return $ List [Atom "unquote", x]

parseVector :: Parser LispVal
parseVector = do string "#("
                 (List x) <- liftM List $ sepBy parseExpr spaces
                 char ')'
                 return . Vector $ fromList x
