{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Materialization.Interpreter.Tests (tests) where

import Control.Exception

import Test.Tasty
import Test.Tasty.HUnit

import Language.Common.Pretty
import Language.Materialization.Core
import Language.Materialization.Interpreter
import Language.Materialization.Quoter

tests :: TestTree
tests = testGroup "Interpreter"
  [ testCase "Synchronization of Non-Resolving Names" case_syncNonResolving
  , testCase "Initialization of Small Values" case_initializeSmallValue
  , testCase "Initialization of Large Values" case_initializeLargeValue
  , testCase "Initialization of Function Values" case_initializeFunctionValue
  , testCase "Capture of Small Values" case_smallCapture
  , testCase "Capture of Multiple Values" case_multiCapture
  ]

(@!?) :: Configuration -> (InterpretationResult Store -> Assertion) -> Assertion
(@!?) p f = case runInterpretation (cfgToEnd p) mempty of
  ((Left err, _), _) -> assertFailure $ show err
  result -> f result

(@!!*) :: Configuration -> InterpretationError -> Assertion
(@!!*) p f = case runInterpretation (cfgToEnd p) mempty of
  ((Right _, _), _) -> assertFailure "It actually succeeded."
  ((Left e, _), _) -> e @?= f

(@!=?) :: LeftExpression -> ShallowValue -> (Store -> Interpretation Assertion)
(@!=?) lExpr sVal s = do
  lIdentAddr <- fromShare <$> resolve lExpr s !? AllocationError lExpr
  lValue <- inspect lIdentAddr s
  return $ lValue @=? sVal

infix 2 @!?
infix 2 @!!*

-- Run a configuration, get a store, run a query on the store to get an assertion, and assert it.
(~=>) :: Configuration -> (Store -> Interpretation Assertion) -> Assertion
(~=>) c f = case runInterpretation (cfgToEnd c >>= \s -> f s >>= \r -> return (s, r)) mempty of
  ((Right (s, a), w), t) -> a
    `onException` putStrLn (render $ pretty (((Right s, w), t) :: InterpretationResult Store))
  ((Left e, w), t) -> assertFailure (render $ pretty e)
    `onException` putStrLn (render $ pretty (((Left e, w), t) :: InterpretationResult Store))

infix 1 ~=>

case_syncNonResolving :: Assertion
case_syncNonResolving = [mP|x?|] :/: nil @!!* NameResolutionError (Unqualified (Name "x"))

case_initializeSmallValue :: Assertion
case_initializeSmallValue = ([mP|x = small-1|] :/: nil) ~=> [mL|x|] @!=? SmallValue "1"

case_initializeLargeValue :: Assertion
case_initializeLargeValue = ([mP|x = large-1|] :/: nil) ~=> [mL|x|] @!=? LargeValue "1"

case_initializeFunctionValue :: Assertion
case_initializeFunctionValue = ([mP|x = \[]. \y. {} -> (y? * C)|] :/: nil) ~=>
  [mL|x|] @!=? (FunctionValue $ Abstraction "y" []
                (BidExpression $ Bid (Synchronizing $ Unqualified $ Name "y") Copy))

case_smallCapture :: Assertion
case_smallCapture = [mP| q = small-1;
                         x = \[z = (q? * C)]. \y. {} -> \[]. \w. {} -> (z? * C)
                        |] :/: nil ~=> [mL|x.z|] @!=? SmallValue "1"

case_multiCapture :: Assertion
case_multiCapture = [mP| q1 = small-1; q2 = large-1;
                         f = \[w1 = (q1? * C), w2 = (q2? * C)]. \y. {} -> (w2 * C)
                       |] :/: nil ~=> [mL|f.w2|] @!=? LargeValue "1"
