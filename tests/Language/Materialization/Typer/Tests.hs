{-# LANGUAGE OverloadedStrings #-}

module Language.Materialization.Typer.Tests (tests) where

import Control.Lens
import qualified Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit

import Language.Materialization.Core
import Language.Materialization.Typer

tests :: TestTree
tests = testGroup "Typer"
  [ testCase "Single Atom" case_singleAtom
  , testCase "Single Blob" case_singleBlob
  , testCase "Identity Function" case_identityFn
  , testCase "Identity Application" case_identityApp
  , testCase "Const Function" case_constFn
  , testCase "Const Application (1 Args)" case_constApp1
  , testCase "Const Application (2 Args)" case_constApp2
  ]

(@:>>=) :: Program -> (TypingState -> Assertion) -> Assertion
(@:>>=) p f = case runTyping (program p) mempty of
  ((Right _, _), s) -> f s
  ((Left er, _), _) -> assertFailure (show er)

(@:?) :: Name -> Type -> TypingState -> Assertion
(@:?) n t ts = M.lookup n (ts ^. typeMap) @=? Just t

infix 6 @:>>=
infix 7 @:?

case_singleAtom :: Assertion
case_singleAtom = [Initialization (Name "x") AtomValue] @:>>= "x" @:? AtomType

case_singleBlob :: Assertion
case_singleBlob = [Initialization (Name "x") BlobValue] @:>>= "x" @:? BlobType

case_identityFn :: Assertion
case_identityFn =
  [ Initialization "id"
     (CapturingValue [] (Abstraction "x" AtomType [] "x"))
  ] @:>>= "id" @:? ClosureType AtomType AtomType []

case_identityApp :: Assertion
case_identityApp =
  [ Initialization "id"
     (CapturingValue [] (Abstraction "x" AtomType [] "x"))
  , Initialization "x" AtomValue
  , Materialization "y" (Application "id" "x" Copy) Copy
  ] @:>>= "y" @:? AtomType

constFnInit :: Clause
constFnInit =
  Initialization "const"
   (CapturingValue []
    (Abstraction "x" AtomType
     [Initialization "t"
      (CapturingValue [("x'", "x", Copy)]
       (Abstraction "y" AtomType [] "x'"))] "t"))

case_constFn :: Assertion
case_constFn =
  [ constFnInit
  ] @:>>= "const" @:? ClosureType AtomType (ClosureType AtomType AtomType [("x'", AtomType)]) []

case_constApp1 :: Assertion
case_constApp1 =
  [ constFnInit
  , Initialization "a" AtomValue
  , Materialization "z" (Application "const" "a" Copy) Copy
  ] @:>>= "z" @:? ClosureType AtomType AtomType [("x'", AtomType)]

case_constApp2 :: Assertion
case_constApp2 =
  [ constFnInit
  , Initialization "a" AtomValue
  , Materialization "z" (Application "const" "a" Copy) Copy
  , Materialization "w" (Application "z" "a" Copy) Copy
  ] @:>>= "w" @:? AtomType
