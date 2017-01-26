{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Materialization.Core where

import Text.PrettyPrint.Annotated
import Language.Common.Pretty (Pretty(..))

-- | Programs are blocks with designated return locations.
data Program = Program { programBody :: Block, programReturn :: Location }
 deriving (Eq, Ord, Read, Show)

instance Pretty Program where
  pretty (Program {..}) = lbrace $+$ nest 2 (pretty programBody) $+$ rbrace <+> "~>" <+> pretty programReturn

-- | Blocks are sequences of clauses.
newtype Block = Block { blockBody :: [Clause] }
 deriving (Eq, Ord, Read, Show)

instance Pretty Block where
  pretty (Block b) = vcat (map pretty b)

data Clause
  -- | Introduction of a new location into the environment.
  = Allocate Location

  -- | Initialization of a location with a (static) value expression.
  | Initialize Location ValueExpression

  -- | Materialization of a value in one location into another, according to a designated
  -- materialization method.
  | Materialize Location LocationExpression Method

  -- | Deallocation of an existing location from the environment.
  | Deallocate Location
 deriving (Eq, Ord, Read, Show)

instance Pretty Clause where
  pretty c = case c of
    Allocate loc -> "+" <> pretty loc <> semi
    Initialize loc vexpr -> pretty loc <+> "=" <+> pretty vexpr <> semi
    Materialize loc lexpr m -> pretty loc <+> "<" <> pretty m <> "-" <+> pretty lexpr <> semi
    Deallocate loc -> "-" <> pretty loc <> semi

instance Pretty LocationExpression where
  pretty lexpr = case lexpr of
    LPrimitive loc -> pretty loc
    Application floc xloc m -> pretty floc <+> parens (pretty m) <+> pretty xloc

-- | Expressions resulting in values.
data ValueExpression
  -- | Just a plain value.
  = VPrimitive Value

  -- | A function abstraction, with a formal parameter, closure specification and body.
  | Abstraction Location CaptureSpec Program
 deriving (Eq, Ord, Read, Show)

-- | Expressions resulting in locations.
data LocationExpression
  -- | Just a plain location.
  = LPrimitive Location

  -- | An application of a closure stored in one location to an argument stored in another location,
  -- materializing the formal parameter by the designated materialization method.
  | Application Location Location Method
 deriving (Eq, Ord, Read, Show)

instance Pretty ValueExpression where
  pretty vexpr = case vexpr of
    VPrimitive v -> pretty v
    Abstraction xloc cspec body -> "\\" <> pretty xloc <+> pretty cspec $+$ pretty body

-- | Specification of how closure locations are captured --- the materialization method, and the
-- inner and outer locations.
newtype CaptureSpec = CaptureSpec [(Location, Location, Method)]
 deriving (Eq, Ord, Read, Show)

instance Pretty CaptureSpec where
  pretty (CaptureSpec cspec) = brackets $ hsep $ punctuate comma $ map prettyCapture cspec
   where
    prettyCapture (iLoc, oLoc, m) = pretty iLoc <+> "<" <> pretty m <> "-" <+> pretty oLoc

-- | Methods of materialization.
data Method = Copy | Move | Refr
 deriving (Eq, Ord, Read, Show)

instance Pretty Method where
  pretty m = case m of
    Copy -> "C"
    Move -> "M"
    Refr -> "R"

-- | Named locations, accessible from the program.
newtype Location = Location String
 deriving (Eq, Ord, Read, Show)

instance Pretty Location where
  pretty (Location loc) = text loc

-- | Values, stored in locations.
data Value
  = Atom
  | Closure Location ClosureSpec Program
 deriving (Eq, Ord, Read, Show)

instance Pretty Value where
  pretty v = case v of
    Atom -> "atom"
    Closure xloc clspec body -> "\\" <> pretty xloc <+> pretty clspec $+$ pretty body

newtype ClosureSpec = ClosureSpec [Location]
 deriving (Eq, Ord, Read, Show)

instance Pretty ClosureSpec where
  pretty (ClosureSpec ls) = brackets (hsep $ punctuate comma $ map pretty ls)
