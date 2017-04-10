module Language.Materialization.Parser.Tests where

import Text.Megaparsec (runParser)
import Text.Megaparsec.String (Parser)
import Text.Megaparsec.Error (parseErrorPretty)

import Test.Tasty
import Test.Tasty.HUnit

import Language.Materialization.Core
import Language.Materialization.Parser

tests :: TestTree
tests = testGroup "Parser"
  [ testGroup "Programs"
    [ testCase "Empty Program" case_emptyProgram
    ]
  , testGroup "Left Expressions"
    [ testCase "Vanilla Names" case_vanillaNames
    , testCase "Unqualified Names" case_unqualifiedNames
    , testCase "Qualified Names" case_qualifiedNames
    ]
  , testGroup "Right Expressions"
    [ testGroup "Bid Expressions"
      [ testCase "Unqualified Copy" case_unqualifiedCopyBid
      ]
    ]
  , testGroup "Bid Types"
    [ testCase "Copy" case_copyBidType
    , testCase "Move" case_moveBidType
    , testCase "Refr" case_refrBidType
    ]
  ]

case_emptyProgram :: Assertion
case_emptyProgram = program `forceParser` "" @=? []

case_vanillaNames :: Assertion
case_vanillaNames = name `forceParser` "x" @=? Name "x"

case_unqualifiedNames :: Assertion
case_unqualifiedNames = leftExpression `forceParser` "x" @=? (Unqualified $ Name "x")

case_qualifiedNames :: Assertion
case_qualifiedNames = leftExpression `forceParser` "x.y" @=? Qualified (Unqualified (Name "x")) (Name "y")

case_unqualifiedCopyBid :: Assertion
case_unqualifiedCopyBid =
  rightExpression `forceParser` "(x * C)" @=? BidExpression (Bid (NonSynchronizing $ Unqualified (Name "x")) Copy)

case_copyBidType :: Assertion
case_copyBidType = bidType `forceParser` "C" @=? Copy

case_moveBidType :: Assertion
case_moveBidType = bidType `forceParser` "M" @=? Move

case_refrBidType :: Assertion
case_refrBidType = bidType `forceParser` "R" @=? Refr
