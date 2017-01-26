{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Lambda.Core (
  Expression(..),
  toSystemM,

  lambdaChars,
  arrowChars
) where

import Control.Monad.State.Strict

import qualified Data.List as L

import Language.Common.Pretty

import qualified Language.Materialization.Core as M

data Expression
  = Term String
  | Application Expression Expression
  | Abstraction String Expression
 deriving (Eq, Ord, Read, Show)

lambdaChars :: [String]
lambdaChars = ["λ", "\\"]

arrowChars :: [String]
arrowChars = ["->", "."]

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

toSystemM :: Expression -> M.Program
toSystemM expr = flip evalState (0 :: Int) $ do
  (body, result) <- recursiveToSystemM expr
  return $ M.Program (M.Block body) result
 where
   recursiveToSystemM e = case e of
     Term i -> return ([], (M.Location i))
     Application f x -> do
       result <- genSym
       (mfp, mfv) <- recursiveToSystemM f
       (mxp, mxv) <- recursiveToSystemM x
       return (mfp ++ mxp ++ [M.Materialize result (M.Application mfv mxv M.Copy) M.Copy], result)
     Abstraction x b -> do
       (mep, mev) <- recursiveToSystemM b
       let captureSpec = M.CaptureSpec [(M.Location inside, M.Location inside, M.Copy) | inside <- L.delete x (freeVars b)]
       fLoc <- genSym
       return ([M.Initialize fLoc (M.Abstraction (M.Location x) captureSpec (M.Program (M.Block mep) mev))], fLoc)
    where
      genSym = do
        i <- get
        modify succ
        return $ M.Location $ "_" ++ show i

      freeVars :: Expression -> [String]
      freeVars (Term i) = [i]
      freeVars (Application f x) = freeVars f ++ freeVars x
      freeVars (Abstraction x b) = [i | i <- freeVars b, i /= x]
