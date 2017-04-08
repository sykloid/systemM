module Language.Common.Pretty (
  Pretty(..),
  MDoc,
  module Text.PrettyPrint.Annotated,

  block
) where

import Text.PrettyPrint.Annotated

type MAnn = ()
type MDoc = Doc MAnn

class Pretty a where
  pretty :: a -> MDoc

block :: Char -> String -> MDoc -> MDoc
block c t d = text (replicate 3 c) <+> text t <+> text (replicate 3 c) $+$ d
