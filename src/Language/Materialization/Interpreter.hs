{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Materialization.Interpreter where

import Control.Applicative
import Control.Error
import qualified Control.Monad.Identity as I
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Semigroup (Max)
import Data.String

import Language.Materialization.Core

-- * The Interpreter Monad

type Interpretation a = ExceptT InterpretationError (WriterT [InterpretationEvent] (StateT InterpretationState I.Identity)) a
type InterpretationResult a = ((Either InterpretationError a, [InterpretationEvent]), InterpretationState)

runInterpretation :: Interpretation a
                  -> InterpretationState
                  -> ((Either InterpretationError a, [InterpretationEvent]), InterpretationState)
runInterpretation i s = I.runIdentity (runStateT (runWriterT (runExceptT i)) s)

-- ** Interpreter Error Reporting

data InterpretationError
  = InterpretationError String
  | ResolutionError LeftExpression
 deriving (Eq, Ord, Read, Show)

instance IsString InterpretationError where
  fromString = InterpretationError

-- ** Interpreter Event Logging

data InterpretationEvent

-- ** Interpreter State

data InterpretationState = InterpretationState { _counter :: Max Address }

instance Monoid InterpretationState where
  mempty = InterpretationState { _counter = mempty }
  mappend (InterpretationState c1) (InterpretationState c2) = InterpretationState (c1 <> c2)

class Patchable a where
  nil :: a
  isConcrete :: a -> Bool
  (<<>) :: a -> a -> a
  (<+>) :: a -> a -> a
  (<<*>) :: a -> [a] -> a
  (<<*>) = foldl (<<>)

newtype Mapping k v = Mapping (M.Map k (Maybe v))
 deriving (Eq, Functor, Foldable, Ord, Read, Show)

instance Ord k => Patchable (Mapping k v) where
  nil = Mapping []
  isConcrete (Mapping m) = all isJust m -- Haha
  (<<>) (Mapping c) (Mapping d) = Mapping $ M.foldrWithKey patch c d
   where
    patch k mv = M.alter (const $ Just <$> mv) k
  (<+>) (Mapping c) (Mapping d) = Mapping $ M.union c d

-- * Configuration

data Configuration = Program :/: Store

data Store = Store
  { _environment :: Environment
  , _identities :: Mapping IdentityAddress Identity
  , _memory :: Memory
  }

instance Patchable Store where
  nil = Store { _environment = nil, _identities = nil, _memory = nil }
  isConcrete (Store e i m) = isConcrete e && isConcrete i && isConcrete m
  (<<>) (Store e1 i1 m1) (Store e2 i2 m2) = Store (e1 <<> e2) (i1 <<> i2) (m1 <<> m2)
  (<+>) (Store e1 i1 m1) (Store e2 i2 m2) = Store (e1 <+> e2) (i1 <+> i2) (m1 <+> m2)

data Environment = Environment { _stack :: Stack, _globals :: Mapping Name IdentityAddress }

instance Patchable Environment where
  nil = Environment { _stack = nil, _globals = nil }
  isConcrete Environment { .. } = isConcrete _stack && isConcrete _globals
  (<<>) (Environment s1 gs1) (Environment s2 gs2) = Environment (s1 <<> s2) (gs1 <<> gs2)
  (<+>) (Environment s1 gs1) (Environment s2 gs2) = Environment (s1 <+> s2) (gs1 <+> gs2)

data Identity = Identity
  { _dependents :: Mapping Name IdentityAddress
  , _stackAddress :: StackAddress
  , _heapAddress :: HeapAddress
  }

data Memory = Memory
  { _stackMS :: Mapping StackAddress StackValue
  , _heapMS :: Mapping HeapAddress HeapValue
  }
 deriving (Eq, Ord, Read, Show)

instance Patchable Memory where
  nil = Memory { _stackMS = nil, _heapMS = nil }
  isConcrete Memory { .. } = isConcrete _stackMS && isConcrete _heapMS
  (<<>) (Memory s1 h1) (Memory s2 h2) = Memory (s1 <<> s2) (h1 <<> h2)
  (<+>) (Memory s1 h1) (Memory s2 h2) = Memory (s1 <+> s2) (h1 <+> h2)

type Stack = [Frame]

instance Patchable a => Patchable [a] where
  nil = []
  isConcrete = all isConcrete
  (<<>) = zipWith (<<>)
  (<+>) = zipWith (<+>)

data Frame = Frame { _locals :: Mapping Name IdentityAddress, _closure :: Mapping Name IdentityAddress }

instance Patchable Frame where
  nil = Frame { _locals = nil, _closure = nil }
  isConcrete Frame { .. } = isConcrete _locals && isConcrete _closure
  (<<>) (Frame ls1 cs1) (Frame ls2 cs2) = Frame (ls1 <<> ls2) (cs1 <<> cs2)
  (<+>) (Frame ls1 cs1) (Frame ls2 cs2) = Frame (ls1 <+> ls2) (cs1 <+> cs2)

type Address = Int
type IdentityAddress = Address
type StackAddress = Address
type HeapAddress = Address

data Value = SmallValue ValueSentinel | LargeValue ValueSentinel | FunctionValue Abstraction
 deriving (Eq, Ord, Read, Show)

data StackValue = SmallStackValue ValueSentinel | LargeStackValue ValueSentinel | FunctionStackValue Abstraction
 deriving (Eq, Ord, Read, Show)

newtype HeapValue = LargeHeapValue ValueSentinel
 deriving (Eq, Ord, Read, Show)

type ShallowValue = Value
newtype DeepValue = DeepValue (Mapping Name DeepValue, ShallowValue)

-- ** Lenses

-- ** Helpers

-- *** Error Reporting Operators

(!??) :: Maybe a -> InterpretationError -> Interpretation a
(!??) ma e = maybe (throwE e) return ma

(!!?) :: Interpretation (Maybe a) -> InterpretationError -> Interpretation a
(!!?) m e = m >>= (!?? e)

-- *** Value Decomposition/Recomposition

decompose :: Value -> (Maybe StackValue, Maybe HeapValue)
decompose lit = case lit of
  SmallValue v -> (Just (SmallStackValue v), Nothing)
  LargeValue v -> (Just (LargeStackValue v), Just (LargeHeapValue v))
  FunctionValue a -> (Just (FunctionStackValue a), Nothing)

recompose :: (Maybe StackValue, Maybe HeapValue) -> Maybe Value
recompose (msv, mhv) = case (msv, mhv) of
  (Just (SmallStackValue v), Nothing) -> Just $ SmallValue v
  (Just (LargeStackValue v), Just (LargeHeapValue v')) | v == v' -> Just $ LargeValue v
  (Just (FunctionStackValue a), Nothing) -> Just $ FunctionValue a
  _ -> Nothing

-- | Resolve the given @LeftExpression@ to a potential @IdentityAddress@ in a given @Store@.
resolve :: LeftExpression -> Store -> Interpretation (Maybe IdentityAddress)
resolve lExpr s = case lExpr of
  Qualified prefix suffix -> do
    intermediate <- resolve prefix s !!? ResolutionError prefix
    dependents <- dependentsOf intermediate s
    return (look suffix dependents)
  Unqualified name -> do
    let resolveInStack n s' = case s' of
          [] -> Nothing
          (Frame {..}:fs) ->
            let ls = look n _locals
                cs = look n _closure
                rs = resolveInStack n fs
            in ls <|> cs <|> rs
        resolveInEnvironment n Environment {..} =
          let ss = resolveInStack n _stack
              gs = look n _globals
          in ss <|> gs
    return (resolveInEnvironment name (_environment s))

look :: Ord k => k -> Mapping k v -> Maybe v
look k (Mapping m) = fromJust <$> M.lookup k m

dependentsOf :: IdentityAddress -> Store -> Interpretation (Mapping Name IdentityAddress)
dependentsOf i s = _dependents <$> (look i (_identities s) !?? "Bojo")

-- * Interpreters

cfgToEnd :: Configuration -> Interpretation Store
cfgToEnd (p :/: s) = case p of
  [] -> return s
  cs -> stepOnce (cs :/: s) >>= cfgToEnd

cfgToSync :: Configuration -> Interpretation (Either Store (LeftExpression, Configuration))
cfgToSync (p :/: s) = case p of
  [] -> return (Left s)
  (Synchronization n:cs) -> return (Right (n, cs :/: s))
  cs -> stepOnce (cs :/: s) >>= cfgToSync

stepOnce :: Configuration -> Interpretation Configuration
stepOnce ([] :/: s) = return ([] :/: s)
stepOnce ((c:cs) :/: s) = case c of
  Assignment _ _ -> _
  Synchronization lExpr -> do
    _ <- resolve lExpr s !!? ResolutionError lExpr
    return (cs :/: s)
  Return -> _
