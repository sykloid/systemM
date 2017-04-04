{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Materialization.Core where

import Data.Data
import Data.String

import Text.PrettyPrint.Annotated

import Language.Common.Pretty (Pretty(..))

-- | Programs are sequences of clauses, parameterized by a name type.
type Program = [Clause]

instance Pretty Program where
  pretty b = vcat (map pretty b)

-- | Clauses are purely side-effecting statements, typically targeting a single name.
data Clause
  -- | Assignment
  = Assignment LeftExpression RightExpression

  -- | Synchronize the contents of a name.
  | Synchronization LeftExpression

  -- | Return from a function application, performing all necessary clean-up activities.
  | Return
 deriving (Data, Eq, Ord, Read, Show, Typeable)

instance Pretty Clause where
  pretty c = case c of
    Assignment lExpr rExpr -> pretty lExpr <+> "=" <+> pretty rExpr
    Synchronization lExpr -> pretty (Synchronizing lExpr)
    Return -> "!"

-- | An expression which may appear on the left-hand side of an assignment.
data LeftExpression = Unqualified Name | Qualified LeftExpression Name
 deriving (Data, Eq, Ord, Read, Show, Typeable)

instance Pretty LeftExpression where
  pretty lExpr = case lExpr of
    Unqualified n -> pretty n
    Qualified lExpr' n -> pretty lExpr' <> "." <> pretty n

data SyncExpression = NonSynchronizing LeftExpression | Synchronizing LeftExpression
 deriving (Data, Eq, Ord, Read, Show, Typeable)

instance Pretty SyncExpression where
  pretty sExpr = case sExpr of
    NonSynchronizing lExpr -> pretty lExpr
    Synchronizing lExpr -> pretty lExpr <> "?"

-- | An expression which may appear on the right-hand side of an assignment.
data RightExpression
  -- | Plain values, wrapped with an intent on how to transfer resources.
  = BidExpression Bid

  -- | Function application.
  | Application SyncExpression Bid

  -- | Expressions which necessarily create new values.
  | LiteralExpression Literal
 deriving (Data, Eq, Ord, Read, Show, Typeable)

instance Pretty RightExpression where
  pretty rExpr = case rExpr of
    BidExpression b -> pretty b
    Application lExpr b -> pretty lExpr <+> pretty b
    LiteralExpression literal -> pretty literal

-- | Bids convey an intent to transfer resources.
data Bid = Bid SyncExpression BidType
 deriving (Data, Eq, Ord, Read, Show, Typeable)

instance Pretty Bid where
  pretty (Bid lExpr bType) = parens (pretty lExpr <+> "*" <+> pretty bType)

-- | Literal values, categorized by memory characteristics.
data Literal
  -- | Small literals only have a stack component.
  = SmallLiteral ValueSentinel

  -- | Large literals have a stack and a heap component.
  | LargeLiteral ValueSentinel

  -- | Capturing literals (really only functions) have only a stack component to themselves (a code
  -- pointer), but also have a number of dependents, which may be any kind of value.
  | CaptureExpression CaptureSpec Abstraction
 deriving (Data, Eq, Ord, Read, Show, Typeable)

instance Pretty Literal where
  pretty literal = case literal of
    SmallLiteral i -> "small" <> "-" <> pretty i
    LargeLiteral i -> "large-" <> "-" <> pretty i
    CaptureExpression cSpec abstraction -> "\\" <> pretty cSpec <> "." <+> pretty abstraction

data Abstraction = Abstraction Name Program RightExpression
 deriving (Data, Eq, Ord, Read, Show, Typeable)

instance Pretty Abstraction where
  pretty (Abstraction name body right) = "\\" <> pretty name <> "." <+> pretty body <+> "->" <+> pretty right

-- | Specification of how names are captured -- their names on the inside and the outside, as well
-- as the materialization method used.
type CaptureSpec = [(Name, Bid)]

instance Pretty CaptureSpec where
  pretty cSpec = brackets $ hsep $ punctuate comma $ map prettyCapture cSpec
   where prettyCapture (iName, bid) = pretty (Assignment (Unqualified iName) (BidExpression bid))

-- | Methods of materialization.
data BidType = Copy | Move | Refr
 deriving (Data, Eq, Ord, Read, Show, Typeable)

instance Pretty BidType where
  pretty m = case m of
    Copy -> "C"
    Move -> "M"
    Refr -> "R"

-- | Program-level names.
newtype Name = Name String
 deriving (Data, Eq, Ord, Read, Show, Typeable)

instance Pretty Name where
  pretty (Name n) = text n

instance IsString Name where
  fromString = Name

-- | Value sentinels serve to distinguish different values of the same type -- e.g. different small
-- values, different large values, etc.
newtype ValueSentinel = ValueSentinel String
 deriving (Data, Eq, Ord, Read, Show, Typeable)

instance Pretty ValueSentinel where
  pretty (ValueSentinel i) = text i
