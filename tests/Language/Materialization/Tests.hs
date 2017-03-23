module Language.Materialization.Tests (tests) where

import Test.Tasty

import qualified Language.Materialization.Interpreter.Tests
import qualified Language.Materialization.Parser.Tests

tests :: TestTree
tests = testGroup "Materialization"
  [ Language.Materialization.Parser.Tests.tests
  , Language.Materialization.Interpreter.Tests.tests
  ]
