module Language.Lambda.Parser.Tests where

import Data.Maybe
import Text.Megaparsec (runParser)
import Text.Megaparsec.Error (parseErrorPretty)

import Test.Tasty
import Test.Tasty.HUnit

import Language.Lambda.Core
import Language.Lambda.Parser

tests :: TestTree
tests = testGroup "Parser"
  [ testCase "Term" case_term
  , testCase "Application" case_application
  , testCase "Identity" case_identity
  , testCase "Constant True" case_constTrue
  ]

shouldParse :: String -> Expression
shouldParse = either (error . parseErrorPretty) id . runParser expression "<???>"

case_term :: Assertion
case_term = Term "x" @=? shouldParse "x"

case_application :: Assertion
case_application = Application (Term "f") (Term "x") @=? shouldParse "f x"

case_identity :: Assertion
case_identity = Abstraction "x" (Term "x") @=? shouldParse "\\x. x"

case_constTrue :: Assertion
case_constTrue = Abstraction "x" (Abstraction "y" (Term "x")) @=? shouldParse "\\x. \\y. x"
