{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Lambda.Core (
  Expression(..),
  toSystemM,

  lambdaChars,
  arrowChars
) where

import Control.Monad.State.Strict

import qualified Data.List as L

import Language.Common
import Language.Common.Pretty

import qualified Language.Materialization.Core as M

data Expression
  = Term String
  | Application Expression Expression
  | Abstraction String Expression
 deriving (Eq, Ord, Read, Show)

instance Pretty Expression where
  pretty e = case e of
    Term s -> text s
    Application f x ->
      let pf = case f of
                 (Abstraction _ _) -> parens (pretty f)
                 _ -> pretty f
          px = case x of
                 (Abstraction _ _) -> parens (pretty x)
                 (Application _ _) -> parens (pretty x)
                 _ -> pretty x
      in pf <+> px
    Abstraction x b -> text (head lambdaChars) <> text x <+> text (head arrowChars) <+> pretty b

toSystemM :: Expression -> (M.Program, M.LeftExpression)
toSystemM expr = flip evalState (0 :: Int) $ recursiveToSystemM expr
 where
   recursiveToSystemM e = case e of
     Term i -> return ([], M.Unqualified $ M.Name i)
     Application f x -> do
       result <- M.Unqualified <$> genSym
       (mfp, mfl) <- recursiveToSystemM f
       (mxp, mxl) <- recursiveToSystemM x
       return (mfp ++ mxp ++ [M.Assignment result (M.Application mfl (M.Bid mxl M.Copy))], result)
     Abstraction x b -> do
       (mep, mel) <- recursiveToSystemM b
       let captureSpec = [(M.Name inside, M.Bid (M.Unqualified $ M.Name inside) M.Copy) | inside <- L.delete x (freeVars b)]
       fName <- M.Unqualified <$> genSym
       return ([M.Assignment fName $
                  M.LiteralExpression (
                   M.CaptureExpression captureSpec (M.Abstraction (M.Name x) mep (M.BidExpression (M.Bid mel M.Copy))))]
              , fName)
    where
      genSym = do
        i <- get
        modify succ
        return $ M.Name $ "_" ++ show i

      freeVars :: Expression -> [String]
      freeVars (Term i) = [i]
      freeVars (Application f x) = freeVars f ++ freeVars x
      freeVars (Abstraction x b) = [i | i <- freeVars b, i /= x]
