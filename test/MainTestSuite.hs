{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Parser

import Test.Tasty
import Test.Tasty.HUnit

import Data.Ratio ((%))
import Data.Complex (Complex((:+)))
import Data.Vector (fromList)

testTypeParsing = testGroup "type parsing tests"
  [ testCase "parse decimal point number" $ readExpr "3" @?= Number 3
  , testCase "parse floating point number" $ readExpr "42.0" @?= Float 42.0
  , testCase "parse rational number" $ readExpr "3/4" @?= (Ratio $ 3 % 4)
  , testCase "parse complex number (decimal)" $ readExpr "3+1i" @?= (Complex $ 3.0 :+ 1.0)
  , testCase "parse complex number (floating)" $ readExpr "3.0+1.0i" @?= (Complex $ 3.0 :+ 1.0)
  , testCase "parse number (binary prefix)" $ readExpr "#b1000" @?= Number 8
  , testCase "parse number (hex prefix)" $ readExpr "#xFF" @?= Number 255
  , testCase "parse number (oct prefix)" $ readExpr "#o7" @?= Number 7
  , testCase "parse char" $ readExpr "#\\a" @?= Character 'a'
  , testCase "parse space char" $ readExpr "#\\ "  @?= Character ' '
  , testCase "parse special newline char" $ readExpr "#\\newline" @?= Character '\n'
  , testCase "parse special space char" $ readExpr "#\\space" @?= Character ' '
  , testCase "parse string" $ readExpr "\"test 123\"" @?= String "test 123"
  , testCase "parse escaped quote in string" $ readExpr "\"\\\"\"" @?= String "\""
  , testCase "parse quote in string" $ readExpr "\"test \\\"123\\\" test\"" @?= String "test \"123\" test"
  , testCase "parse backslash in string" $ readExpr "\"te\\\\st\"" @?= String "te\\st"
  , testCase "parse newline in string" $ readExpr "\"te\nst\"" @?= String "te\nst"
  , testCase "parse atom" $ readExpr "test_func123" @?= Atom "test_func123"
  , testCase "parse bool (true)" $ readExpr "#t" @?= Bool True
  , testCase "parse bool (false)" $ readExpr "#f" @?= Bool False
  ]

testListParsing = testGroup "list parsing tests"
  [ testCase "parse simple list"
        $ readExpr "(a b)"
        @?= List [Atom "a", Atom "b"]

  , testCase "parse nested list"
        $ readExpr "(a (b) c)"
        @?= List [Atom "a",
                  List [Atom "b"],
                  Atom "c"]

  , testCase "parse dotted list"
        $ readExpr "(a (b . c) d)"
        @?= List [Atom "a",
                  DottedList [(Atom "b")] (Atom "c"),
                  Atom "d"]

  , testCase "parse quoted dotted"
        $ readExpr "(a '(b (c . d)) e)"
        @?= List [Atom "a",
                  List [Atom "quote",
                        List [Atom "b",
                              DottedList [(Atom "c")] (Atom "d")]],
                  Atom "e"]

  , testCase "parse quasiquoted list"
        $ readExpr "`(a (b c))"
        @?= List [Atom "quasiquote",
                  List [Atom "a",
                        List [Atom "b", Atom "c"]]]

  , testCase "parse quasiquoted list (unquote)"
        $ readExpr "`(a ,(b c) d)"
        @?= List [Atom "quasiquote",
                  List [Atom "a",
                        List [Atom "unquote",
                              List [Atom "b",
                                    Atom "c"]],
                        Atom "d"]]
  , testCase "parse vector"
        $ readExpr "#(1 a 3)"
        @?= Vector (fromList [Number 1, Atom "a", Number 3]) 
  ]

tests :: TestTree
tests = testGroup "Parser Tests" [testTypeParsing, testListParsing]

main = defaultMain tests

