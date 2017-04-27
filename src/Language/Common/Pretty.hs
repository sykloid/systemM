{-# LANGUAGE FlexibleInstances #-}
module Language.Common.Pretty (
  Pretty(..),
  MDoc,
  module Text.PrettyPrint.Annotated,

  block,
  guardMP,
  renderPretty
) where

import qualified Data.Map as M
import Text.PrettyPrint.Annotated

type MAnn = ()
type MDoc = Doc MAnn

class Pretty a where
  pretty :: a -> MDoc

block :: Char -> MDoc -> MDoc -> MDoc
block c t d = leftP <+> t <+> rightP $+$ d
 where
  ribbon = 80
  commonWidth = max 1 (ribbon - length (render t)) `div` 2 - 2
  leftP = padding (commonWidth + 1)
  rightP = (if even (length (render t)) then empty else char ' ') <> padding commonWidth
  padding w = text (replicate w c)

instance Pretty Int where
  pretty = int

instance (Pretty k, Pretty v) => Pretty (M.Map k (Maybe v)) where
  pretty m = foldl ($+$) mempty $ map prettyMaplet (M.toAscList m)
   where prettyMaplet (n, nsi) = pretty n <+> text "->" <+> case nsi of
           Just nsi' -> pretty nsi'
           Nothing -> text "<deleted>"

guardMP :: (Pretty k, Pretty v) => MDoc -> M.Map k (Maybe v) -> MDoc
guardMP h m = h <+> if M.null m then text "<empty>" else pretty m

renderPretty :: MDoc -> String
renderPretty = renderStyle style { lineLength = 80, ribbonsPerLine = 1.0 }
