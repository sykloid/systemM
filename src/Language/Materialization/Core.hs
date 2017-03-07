{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Materialization.Core where

import Text.PrettyPrint.Annotated
import Language.Common.Pretty (Pretty(..))

-- | Programs are sequences of clauses, parameterized by a name type.
type Program = [Clause]

instance Pretty Program where
  pretty b = vcat (map pretty b)

-- | Clauses are purely side-effecting statements, typically targeting a single name.
data Clause
  -- | Initialize a name from a value expression.
  = Initialization Name ValueExpression

  -- | Materialize a name from data stored under another, either directly, or through function
  -- application.
  | Materialization Name NameExpression Method

  -- | Synchronize the contents of a name.
  | Synchronization Name

  -- | Return from a function application, performing all necessary clean-up activities.
  | Return
 deriving (Eq, Ord, Read, Show)

instance Pretty Clause where
  pretty c = case c of
    Initialization name vexpr -> pretty name <+> "=" <+> pretty vexpr
    Materialization name lexpr m -> pretty name <+> "<" <> pretty m <> "-" <+> pretty lexpr
    Synchronization name -> pretty name <> "?"
    Return -> "!"

-- | Expressions resulting in names.
data NameExpression
  -- | Just a plain name.
  = PrimitiveName Name

  -- | An application of a closure stored under one name to an argument stored under another,
  -- materializing the formal parameter by the designated materialization method.
  | Application Name Name Method
 deriving (Eq, Ord, Read, Show)

instance Pretty NameExpression where
  pretty nexpr = case nexpr of
    PrimitiveName name -> pretty name
    Application fName xName m -> pretty fName <+> parens (pretty m) <+> pretty xName

-- | Expressions resulting in values.
data ValueExpression
  -- | Just a plain value.
  = AtomValue
  | BlobValue

  -- | A capture expression; a data constructor with a designated capture specification.
  | CapturingVExpr CaptureExpression
 deriving (Eq, Ord, Read, Show)

instance Pretty ValueExpression where
  pretty vexpr = case vexpr of
    AtomValue -> "a"
    BlobValue -> "b"
    CapturingVExpr c -> pretty c

-- | Expressions which make use of captured names; typically data constructors such as functions.
data CaptureExpression
  -- | Function abstractions, with a formal parameter, body and return parameter.
  = CaptureExpression CaptureSpec DataConstructor
 deriving (Eq, Ord, Read, Show)

instance Pretty CaptureExpression where
  pretty (CaptureExpression cSpec dCon) = "\\" <> pretty cSpec <> "." $+$ pretty dCon

-- | Specification of how names are captured -- their names on the inside and the outside, as well
-- as the materialization method used.
newtype CaptureSpec = CaptureSpec [(Name, Name, Method)]
 deriving (Eq, Ord, Read, Show)

instance Pretty CaptureSpec where
  pretty (CaptureSpec cSpec) = brackets $ hsep $ punctuate comma $ map prettyCapture cSpec
   where prettyCapture (iName, oName, m) = pretty iName <+> "<" <> pretty m <> "-" <+> pretty oName

data DataConstructor = Abstraction Name Program Name
 deriving (Eq, Ord, Read, Show)

instance Pretty DataConstructor where
  pretty (Abstraction xName pBody rName) = "\\" <> pretty xName <> "." $+$ pretty pBody <+> "~>" <+> pretty rName

-- | Methods of materialization.
data Method = Copy | Move | Refr
 deriving (Eq, Ord, Read, Show)

instance Pretty Method where
  pretty m = case m of
    Copy -> "C"
    Move -> "M"
    Refr -> "R"

-- | Program-level names.
newtype Name = Name String
 deriving (Eq, Ord, Read, Show)

instance Pretty Name where
  pretty (Name n) = text n

-- | Types serve to distinguish between small values, big values and closure values.
data Type = AtomType | BlobType | ClosureType Type Type CaptureType
 deriving (Eq, Ord, Read, Show)

instance Pretty Type where
  pretty t = case t of
    AtomType -> "A"
    BlobType -> "B"
    ClosureType ta tb tc -> pretty ta <+> "-" <> pretty tc <> ">" <+> pretty tb

-- | The type of a capture is the set of associations between captured names (on the inside of a
-- captured value) and their corresponding types.
type CaptureType = [(Name, Type)]

instance Pretty CaptureType where
  pretty nts = brackets $ hsep $ punctuate comma $ map prettyCaptureType nts
   where prettyCaptureType (n, t) = pretty n <> colon <+> pretty t
