module Language.Lambda.Tests (tests) where

import Test.Tasty

import qualified Language.Lambda.Parser.Tests

tests :: TestTree
tests = testGroup "Lambda Calculus"
  [ Language.Lambda.Parser.Tests.tests
  ]
