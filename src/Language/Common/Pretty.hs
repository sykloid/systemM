module Language.Common.Pretty (Pretty(..), MDoc, module Text.PrettyPrint.Annotated) where

import Text.PrettyPrint.Annotated

type MAnn = ()
type MDoc = Doc MAnn

class Pretty a where
  pretty :: a -> MDoc
