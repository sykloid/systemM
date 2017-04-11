{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Common.PrimitiveValues where

import Data.Data
import Data.String

import Language.Common.Pretty

-- | Value sentinels serve to distinguish different values of the same type -- e.g. different small
-- values, different large values, etc.
newtype ValueSentinel = ValueSentinel String
 deriving (Data, Eq, Ord, Read, Show, Typeable)

instance Pretty ValueSentinel where
  pretty (ValueSentinel i) = text i

instance IsString ValueSentinel where
  fromString = ValueSentinel

data PrimitiveValue = SmallPrimitive ValueSentinel | LargePrimitive ValueSentinel
 deriving (Data, Eq, Ord, Read, Show, Typeable)

instance Pretty PrimitiveValue where
  pretty pv = case pv of
    SmallPrimitive vs -> "small" <> "-" <> pretty vs
    LargePrimitive vs -> "large" <> "-" <> pretty vs
