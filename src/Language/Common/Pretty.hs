{-# LANGUAGE FlexibleInstances #-}
module Language.Common.Pretty (
  Pretty(..),
  MDoc,
  module Text.PrettyPrint.Annotated,

  block,
  guardMP
) where

import qualified Data.Map as M
import Text.PrettyPrint.Annotated

type MAnn = ()
type MDoc = Doc MAnn

class Pretty a where
  pretty :: a -> MDoc

block :: Char -> MDoc -> MDoc -> MDoc
block c t d = text (replicate 3 c) <+> t <+> text (replicate 3 c) $+$ d

instance Pretty Int where
  pretty = int

instance (Pretty k, Pretty v) => Pretty (M.Map k (Maybe v)) where
  pretty m = foldl ($+$) mempty $ map prettyMaplet (M.toAscList m)
   where prettyMaplet (n, nsi) = pretty n <+> text "->" <+> case nsi of
           Just nsi' -> pretty nsi'
           Nothing -> text "<deleted>"

guardMP :: (Pretty k, Pretty v) => MDoc -> M.Map k (Maybe v) -> MDoc
guardMP h m = h <+> if M.null m then text "<empty>" else pretty m
