{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Inserts reasonable synchronization hints.
module Language.Materialization.Transforms.AutoSync (AutoSync(..)) where

import Language.Materialization.Core

class AutoSync t where
  autoSync :: t -> t

instance AutoSync Program where
  autoSync = map autoSync

instance AutoSync Clause where
  autoSync = \case
    Assignment lExpr rExpr -> Assignment lExpr (autoSync rExpr)
    c -> c

instance AutoSync RightExpression where
  autoSync = \case
    BidExpression b -> BidExpression (autoSync b)
    Application se b -> Application (autoSync se) (autoSync b)
    LiteralExpression lit -> LiteralExpression (autoSync lit)

instance AutoSync Bid where
  autoSync = \case
    Bid se t -> Bid (autoSync se) t

instance AutoSync Literal where
  autoSync = \case
    CaptureExpression cSpec a -> CaptureExpression [(lExpr, autoSync b) | (lExpr, b) <- cSpec] (autoSync a)
    lit -> lit

instance AutoSync Abstraction where
  autoSync = \case
    Abstraction n p rExpr -> Abstraction n (autoSync p) (autoSync rExpr)

instance AutoSync SyncExpression where
  autoSync se = case se of
    NonSynchronizing lExpr -> Synchronizing lExpr
    _ -> se
