{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import Wyas.Parser
import Wyas.Evaluator
import Wyas.Types
import Wyas.Error

import Test.Tasty
import Test.Tasty.HUnit

import Data.Ratio ((%))
import Data.Complex (Complex((:+)))
import Data.Vector (fromList)

testTypeParsing = testGroup "type parsing tests"
  [ testCase "parse decimal point number"
        $ (extractValue . readExpr) "3"
        @?= Number 3

  , testCase "parse floating point number"
        $ (extractValue . readExpr) "42.0"
        @?= Float 42.0

  , testCase "parse rational number"
        $ (extractValue . readExpr) "3/4"
        @?= (Ratio $ 3 % 4)

  , testCase "parse complex number (decimal)"
        $ (extractValue . readExpr) "3+1i"
        @?= (Complex $ 3.0 :+ 1.0)

  , testCase "parse complex number (floating)"
        $ (extractValue . readExpr) "3.0+1.0i"
        @?= (Complex $ 3.0 :+ 1.0)

  , testCase "parse number (binary prefix)"
        $ (extractValue . readExpr) "#b1000"
        @?= Number 8

  , testCase "parse number (hex prefix)"
        $ (extractValue . readExpr) "#xFF"
        @?= Number 255

  , testCase "parse number (oct prefix)"
        $ (extractValue . readExpr) "#o7"
        @?= Number 7

  , testCase "parse char"
        $ (extractValue . readExpr) "#\\a"
        @?= Character 'a'

  , testCase "parse space char"
        $ (extractValue . readExpr) "#\\ "
        @?= Character ' '

  , testCase "parse special newline char"
        $ (extractValue . readExpr) "#\\newline"
        @?= Character '\n'

  , testCase "parse special space char"
        $ (extractValue . readExpr) "#\\space"
        @?= Character ' '

  , testCase "parse string"
        $ (extractValue . readExpr) "\"test 123\""
        @?= String "test 123"

  , testCase "parse escaped quote in string"
        $ (extractValue . readExpr) "\"\\\"\""
        @?= String "\""

  , testCase "parse quote in string"
        $ (extractValue . readExpr) "\"test \\\"123\\\" test\""
        @?= String "test \"123\" test"

  , testCase "parse backslash in string"
        $ (extractValue . readExpr) "\"te\\\\st\""
        @?= String "te\\st"

  , testCase "parse newline in string"
        $ (extractValue . readExpr) "\"te\nst\""
        @?= String "te\nst"

  , testCase "parse atom"
        $ (extractValue . readExpr) "test_func123"
        @?= Atom "test_func123"

  , testCase "parse bool (true)"
        $ (extractValue . readExpr) "#t"
        @?= Bool True

  , testCase "parse bool (false)"
        $ (extractValue . readExpr) "#f"
        @?= Bool False
  ]

testListParsing = testGroup "list parsing tests"
  [ testCase "parse simple list"
        $ (extractValue . readExpr) "(a b)"
        @?= List [Atom "a", Atom "b"]

  , testCase "parse nested list"
        $ (extractValue . readExpr) "(a (b) c)"
        @?= List [Atom "a",
                  List [Atom "b"],
                  Atom "c"]

  , testCase "parse dotted list"
        $ (extractValue . readExpr) "(a (b . c) d)"
        @?= List [Atom "a",
                  DottedList [(Atom "b")] (Atom "c"),
                  Atom "d"]

  , testCase "parse quoted dotted"
        $ (extractValue . readExpr) "(a '(b (c . d)) e)"
        @?= List [Atom "a",
                  List [Atom "quote",
                        List [Atom "b",
                              DottedList [(Atom "c")] (Atom "d")]],
                  Atom "e"]

  , testCase "parse quasiquoted list"
        $ (extractValue . readExpr) "`(a (b c))"
        @?= List [Atom "quasiquote",
                  List [Atom "a",
                        List [Atom "b", Atom "c"]]]

  , testCase "parse quasiquoted list (unquote)"
        $ (extractValue . readExpr) "`(a ,(b c) d)"
        @?= List [Atom "quasiquote",
                  List [Atom "a",
                        List [Atom "unquote",
                              List [Atom "b",
                                    Atom "c"]],
                        Atom "d"]]
  , testCase "parse vector"
        $ (extractValue . readExpr) "#(1 a 3)"
        @?= Vector (fromList [Number 1, Atom "a", Number 3])
  ]


testEvaluation = testGroup "evaluation tests"
  [ testCase "simple addition"
        $ extractValue (readExpr "(+ 2 2)" >>= eval)
        @?= Number 4
  , testCase "negative number"
        $ extractValue (readExpr "(+ 2 (-4 1))" >>= eval)
        @?= Number 2
  , testCase "nested calls"
        $ extractValue (readExpr "(+ 2 (- 4 1))" >>= eval)
        @?= Number 5
  , testCase "chained calls"
        $ extractValue (readExpr "(- (+ 4 6 3) 3 5 2)" >>= eval)
        @?= Number 3
  , testCase "check symbol?"
        $ extractValue (readExpr "(symbol? test)" >>= eval)
        @?= Bool True
  , testCase "check number?"
        $ extractValue (readExpr "(number? 3)" >>= eval)
        @?= Bool True
  , testCase "check string?"
        $ extractValue (readExpr "(string? \"test\")" >>= eval)
        @?= Bool True
  , testCase "symbol to string"
        $ extractValue (readExpr "(symbol->string test)" >>= eval)
        @?= String "test"
  ]

testBinop = testGroup "evaluate binops"
  [ testCase "less than (numbers)"
        $ extractValue (readExpr "(< 2 3)" >>= eval)
        @?= Bool True
  , testCase "greater than (numbers)"
        $ extractValue (readExpr "(> 2 3)" >>= eval)
        @?= Bool False
  , testCase "greater or equal than (numbers)"
        $ extractValue (readExpr "(>= 3 3)" >>= eval)
        @?= Bool True
  , testCase "string equality"
        $ extractValue (readExpr "(string=? \"test\" \"test\")" >>= eval)
        @?= Bool True
  , testCase "less than (strings)"
        $ extractValue (readExpr "(string<? \"abc\" \"bba\")" >>= eval)
        @?= Bool True
  , testCase "equal on lists"
        $ extractValue (readExpr "(equal? \'(1 \"2\") \'(1 2))" >>= eval)
        @?= Bool True
  ]

tests :: TestTree
tests = testGroup "Parser Tests" [ testTypeParsing
                                 , testListParsing
                                 , testEvaluation
                                 , testBinop
                                 ]

main = defaultMain tests

