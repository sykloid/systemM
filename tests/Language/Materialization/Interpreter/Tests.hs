{-# LANGUAGE QuasiQuotes #-}
module Language.Materialization.Interpreter.Tests (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Materialization.Core
import Language.Materialization.Interpreter
import Language.Materialization.Quoter

tests :: TestTree
tests = testGroup "Interpreter"
  [ testCase "Synchronization of Non-Resolving Names" case_syncNonResolving
  ]

(@!?) :: Program -> (InterpretationResult Store -> Assertion) -> Assertion
(@!?) p f = case runInterpretation (cfgToEnd (p :/: nil)) mempty of
  ((Left err, _), _) -> assertFailure $ show err
  result -> f result

(@!!*) :: Program -> InterpretationError -> Assertion
(@!!*) p f = case runInterpretation (cfgToEnd (p :/: nil)) mempty of
  ((Right _, _), _) -> assertFailure "It actually succeeded."
  ((Left e, _), _) -> e @?= f

case_syncNonResolving :: Assertion
case_syncNonResolving = [mP|x ?|]  @!!* ResolutionError (Unqualified (Name "x"))
