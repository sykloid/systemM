{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | A System-M Interpreter.
module Language.Materialization.Interpreter (
  -- * Interpretation Machinery
  Interpretation,
  InterpretationResult,
  InterpretationError(..),
  InterpretationEvent(..),
  runInterpretation,

  -- * Wrappers
  Nillable(..),
  Patchable(..),
  Nullable(..),
  Shareable(..),

  -- * Simulation Types
  Store(..),
  Environment(..),
  Memory(..),
  Ident(..),
  Stack,
  Frame(..),

  Address,
  IdentAddress,
  StackAddress,
  HeapAddress,

  ShallowValue(..),
  DeepValue(..),
  StackValue(..),
  HeapValue(..),

  -- * Interpretations
  Configuration(..),
  cfgToEnd,
  cfgToSync,
)  where

import Control.Applicative
import Control.Error hiding ((!?), (??))
import qualified Control.Error as Error
import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict hiding ((<>))
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Semigroup
import Data.String

import Language.Materialization.Core
import Language.Common.Pretty hiding ((<>))

-- * Interpretation Machinery

type Interpretation a = ExceptT InterpretationError (WriterT [InterpretationEvent] (StateT InterpretationState Identity)) a
type InterpretationResult a = ((Either InterpretationError a, [InterpretationEvent]), InterpretationState)

runInterpretation :: Interpretation a
                  -> InterpretationState
                  -> InterpretationResult a
runInterpretation i s = runIdentity (runStateT (runWriterT (runExceptT i)) s)

instance (Show a, Pretty a) => Pretty (InterpretationResult a) where
  pretty ((r, w), _) = foldl ($+$) mempty blocks
   where
    blocks =
       [ block '=' "Result" pr
       , block '=' "Event Log" (foldl ($+$) mempty $ map pretty w)
       ] :: [MDoc]
    pr = case r of
      Left e -> pretty e
      Right a -> fromString $ show a

-- ** Error Reporting

data InterpretationError
  = InterpretationError String
  | NameResolutionError LeftExpression
  | AllocationError LeftExpression
  | IdentResolutionError IdentAddress
  | StackResolutionError StackAddress
  | HeapResolutionError HeapAddress
  | RecompositionError (Nullable StackValue) (Nullable HeapValue)
 deriving (Eq, Ord, Read, Show)

instance IsString InterpretationError where
  fromString = InterpretationError

instance Pretty InterpretationError where
  pretty e = case e of
    InterpretationError s -> "Interpretation Error:" <+> fromString s
    NameResolutionError lExpr -> "Name Resolution Error:" <+> pretty lExpr
    AllocationError lExpr -> "Allocation Error:" <+> pretty lExpr
    IdentResolutionError iAddr -> "Ident Resolution Error:" <+> int iAddr
    StackResolutionError sAddr -> "Stack Resolution Error:" <+> int sAddr
    HeapResolutionError hAddr -> "Heap Resolution Error:" <+> int hAddr
    RecompositionError nsValue nhValue -> "Recomposition Error:" <+> pretty nsValue <+> "and" <+> pretty nhValue

hushE :: Interpretation a -> InterpretationError -> Interpretation (Maybe a)
hushE m e = catchE (Just <$> m) (\e' -> if e == e' then return Nothing else throwE e')

(??) :: MaybeLike a => a -> InterpretationError -> Interpretation (MaybeType a)
(??) x e = toMaybe x Error.?? e

(!?) :: MaybeLike a => Interpretation a -> InterpretationError -> Interpretation (MaybeType a)
(!?) m e = m >>= (?? e)

-- ** Event Logging

data InterpretationEvent
  = ClauseEvent Clause String
  | SynchronizationEvent LeftExpression ShallowValue
 deriving (Eq, Ord, Read, Show)

instance Pretty InterpretationEvent where
  pretty ie = case ie of
    ClauseEvent c comment -> pretty c <+> ":" <+> fromString comment
    SynchronizationEvent lExpr v -> pretty lExpr <+> "synchronized as" <+> pretty v

-- ** Interpreter State

newtype InterpretationState = InterpretationState { counter :: Max Address }
 deriving (Eq, Ord, Read, Show)

instance Monoid InterpretationState where
  mempty = InterpretationState { counter = 0 }
  mappend (InterpretationState c1) (InterpretationState c2) = InterpretationState (c1 <> c2)

-- * Wrappers

-- | Types that have a distinguished @nil@ value.
class Nillable a where
  nil :: a

newtype ChangesTo a = ChangesTo { fromChanges :: a}

-- | Nillable types which can be patched (i.e. modified from a delta).
class Nillable a => Patchable a where
  isConcrete :: a -> Bool
  (<<>) :: a -> ChangesTo a -> a
  (<++>) :: ChangesTo a -> ChangesTo a -> ChangesTo a
  (<<*>) :: a -> [ChangesTo a] -> a
  (<<*>) = foldl (<<>)

-- | A maybe-like wrapper, denoting data which might be null, or junk.
data Nullable x = Null | Valid x
  deriving (Eq, Ord, Read, Show)

instance Pretty a => Pretty (Nullable a) where
  pretty na = case na of
    Null -> "Null"
    Valid a -> "Valid" <+> pretty a

instance Nillable (Nullable a) where
  nil = Null

class MaybeLike a where
  type MaybeType a :: *
  toMaybe :: a -> Maybe (MaybeType a)

instance MaybeLike (Nullable a) where
  type MaybeType (Nullable a) = a
  toMaybe na = case na of
    Null -> Nothing
    Valid a -> Just a

instance MaybeLike (Maybe a) where
  type MaybeType (Maybe a) = a
  toMaybe = id

-- | An either-like wrapper, denoting an entity (typically an address) which may be owned or
-- borrowed. If sharing status is irrelevant, eliminate with @fromShare@.
data Shareable x = Owned { fromShare :: x } | Borrowed { fromShare :: x }
  deriving (Eq, Ord, Read, Show)

-- * Configuration

type Mapping k v = M.Map k (Maybe v)

-- | Construct a delta-mapping, discarding null keys.
prepareChanges :: Ord k => Mapping (Nullable k) v -> Mapping k v
prepareChanges m = M.fromList [ (k, v) | (Valid k, v) <- M.toList m ]

instance Ord k => Nillable (Mapping k v) where
  nil = []

instance Ord k => Patchable (Mapping k v) where
  isConcrete = all isJust -- Haha
  (<<>) c (ChangesTo d) = M.foldrWithKey patch c d
   where patch k mv = M.alter (const $ Just <$> mv) k
  (<++>) (ChangesTo c) (ChangesTo d) = ChangesTo $ M.union c d

type Namespace = Mapping Name (Nullable (Shareable IdentAddress))

data Configuration = Program :/: Store
  deriving (Eq, Ord, Read, Show)

infix 3 :/:

data Store = Store
  { environment :: Environment
  , idents :: Mapping IdentAddress Ident
  , memory :: Memory
  }
 deriving (Eq, Ord, Read, Show)

instance Pretty Store where
  pretty Store {..} = foldl ($+$) mempty blocks
   where
     blocks :: [MDoc]
     blocks = [ block '-' "Environment" mempty
              , block '-' "Idents" mempty
              , block '-' "Memory" mempty
              ]

instance Nillable Store where
  nil = Store { environment = nil, idents = nil, memory = nil }

instance Patchable Store where
  isConcrete (Store e i m) = isConcrete e && isConcrete i && isConcrete m
  (<<>) (Store e1 i1 m1) (ChangesTo (Store e2 i2 m2)) = Store (e1 <<> ChangesTo e2) (i1 <<> ChangesTo i2) (m1 <<> ChangesTo m2)
  (<++>) (ChangesTo (Store e1 i1 m1)) (ChangesTo (Store e2 i2 m2))
    = ChangesTo $ Store (fromChanges $ ChangesTo e1 <++> ChangesTo e2)
                        (fromChanges $ ChangesTo i1 <++> ChangesTo i2)
                        (fromChanges $ ChangesTo m1 <++> ChangesTo m2)

data Environment = Environment { stack :: Stack, globals :: Namespace }
 deriving (Eq, Ord, Read, Show)

instance Nillable Environment where
  nil = Environment { stack = nil, globals = nil }

instance Patchable Environment where
  isConcrete Environment { .. } = isConcrete stack && isConcrete globals
  (<<>) (Environment s1 gs1) (ChangesTo (Environment s2 gs2))
    = Environment (s1 <<> ChangesTo s2) (gs1 <<> ChangesTo gs2)
  (<++>) (ChangesTo (Environment s1 gs1)) (ChangesTo (Environment s2 gs2))
    = ChangesTo $ Environment (fromChanges $ ChangesTo s1 <++> ChangesTo s2)
                              (fromChanges $ ChangesTo gs1 <++> ChangesTo gs2)

data Ident = Ident
  { dependents :: Namespace
  , stackAddress :: Nullable StackAddress
  , heapAddress :: Nullable HeapAddress
  }
 deriving (Eq, Ord, Read, Show)

instance Nillable Ident where
  nil = Ident { dependents = nil, stackAddress = nil, heapAddress = nil }

data Memory = Memory
  { stackMS :: Mapping StackAddress (Nullable StackValue)
  , heapMS :: Mapping HeapAddress (Nullable HeapValue)
  }
 deriving (Eq, Ord, Read, Show)

instance Nillable Memory where
  nil = Memory { stackMS = nil, heapMS = nil }

instance Patchable Memory where
  isConcrete Memory { .. } = isConcrete stackMS && isConcrete heapMS
  (<<>) (Memory s1 h1) (ChangesTo (Memory s2 h2)) = Memory (s1 <<> ChangesTo s2) (h1 <<> ChangesTo h2)
  (<++>) (ChangesTo (Memory s1 h1)) (ChangesTo (Memory s2 h2))
    = ChangesTo $ Memory (fromChanges $ ChangesTo s1 <++> ChangesTo s2)
                         (fromChanges $ ChangesTo h1 <++> ChangesTo h2)

type Stack = [Frame]

instance Nillable [a] where
  nil = []

instance Patchable a => Patchable [a] where
  isConcrete = all isConcrete
  (<<>) xs (ChangesTo cxs) = [x <<> ChangesTo cx | x <- xs | cx <- cxs]
  (<++>) (ChangesTo cxs1) (ChangesTo cxs2)
    = ChangesTo [fromChanges $ ChangesTo cx1 <++> ChangesTo cx2 | cx1 <- cxs1 | cx2 <- cxs2]

data Frame = Frame { locals :: Namespace, closure :: Namespace }
 deriving (Eq, Ord, Read, Show)

instance Nillable Frame where
  nil = Frame { locals = nil, closure = nil }

instance Patchable Frame where
  isConcrete Frame { .. } = isConcrete locals && isConcrete closure
  (<<>) (Frame ls1 cs1) (ChangesTo (Frame ls2 cs2)) = Frame (ls1 <<> ChangesTo ls2) (cs1 <<> ChangesTo cs2)
  (<++>) (ChangesTo (Frame ls1 cs1)) (ChangesTo (Frame ls2 cs2))
    = ChangesTo $ Frame (fromChanges $ ChangesTo ls1 <++> ChangesTo ls2)
                        (fromChanges $ ChangesTo cs1 <++> ChangesTo cs2)

type Address = Int
type IdentAddress = Address
type StackAddress = Address
type HeapAddress = Address

-- *** Values

-- | Shallow values do not include captured values; determining a name's shallow value does not
-- require forcing all of its components.
data ShallowValue
  -- | Small values are statically sized, and can be stack allocated in their entirety.
  = SmallValue ValueSentinel

  -- | Large values have a statically sized component which can be stack allocated, as well as a
  -- dynamically sized component which must be heap allocated.
  | LargeValue ValueSentinel

  -- | Function values have a statically sized code component (think function pointer); and do not
  -- themselves have a dynamically sized heap component.
  | FunctionValue Abstraction
 deriving (Eq, Ord, Read, Show)

instance Pretty ShallowValue where
  pretty v = case v of
    SmallValue vs -> "small-" <> pretty vs
    LargeValue vs -> "large-" <> pretty vs
    FunctionValue a -> "function-" <> braces (pretty a)

-- | Deep values consist of a shallow value, as well as a mapping of captured dependent names to
-- their own corresponding deep values. Inspecting a deep value forces all associated captures,
-- which is necessary for synchronization, but not for application (for example).
newtype DeepValue = DeepValue (ShallowValue, Mapping Name DeepValue)

data StackValue = SmallStackValue ValueSentinel | LargeStackValue ValueSentinel | FunctionStackValue Abstraction
 deriving (Eq, Ord, Read, Show)

instance Pretty StackValue where
  pretty sv = case sv of
    SmallStackValue vs -> "small-" <> pretty vs <> "-stack"
    LargeStackValue vs -> "large-" <> pretty vs <> "-stack"
    FunctionStackValue as -> "stack-abstraction" <> parens (pretty as)

newtype HeapValue = LargeHeapValue ValueSentinel
 deriving (Eq, Ord, Read, Show)

instance Pretty HeapValue where
  pretty hv = case hv of
    LargeHeapValue vs -> "large-" <> pretty vs <> "-heap"

-- ** Helpers

-- *** Error Reporting Operators

-- *** Value Decomposition/Recomposition

-- | Decompose a value into its stack and heap components.
decompose :: ShallowValue -> Interpretation (Nullable StackValue, Nullable HeapValue)
decompose lit = return $ case lit of
  SmallValue v -> (Valid (SmallStackValue v), Null)
  LargeValue v -> (Valid (LargeStackValue v), Valid (LargeHeapValue v))
  FunctionValue a -> (Valid (FunctionStackValue a), Null)

-- | Recompose a pair of stack/heap values into (possibly) a regular value.
recompose :: (Nullable StackValue, Nullable HeapValue) -> Interpretation ShallowValue
recompose (msv, mhv) = case (msv, mhv) of
  (Valid (SmallStackValue v), Null) -> return $ SmallValue v
  (Valid (LargeStackValue v), Valid (LargeHeapValue v')) | v == v' -> return $ LargeValue v
  (Valid (FunctionStackValue a), Null) -> return $ FunctionValue a
  _ -> throwE $ RecompositionError msv mhv

freshA :: Interpretation Address
freshA = getMax <$> gets counter <* modify (\s -> s { counter = succ (counter s)})

gFreshA :: Nullable a -> Interpretation (Nullable Address)
gFreshA ma = case ma of { Null -> return Null; _ -> Valid <$> freshA }

-- *** Resolution

resolve :: LeftExpression -> Store -> Interpretation (Nullable (Shareable IdentAddress))
resolve lExpr s = case lExpr of
  Qualified prefix suffix -> do
    intermediate <- fromShare <$> resolve prefix s !? AllocationError prefix
    ds <- dependentsOf intermediate s
    lookupC suffix ds ?? NameResolutionError lExpr
  Unqualified name -> do
    let resolveInStack n s' = case s' of
          [] -> Nothing
          (Frame {..}:fs) ->
            let ls = lookupC n locals
                cs = lookupC n closure
                rs = resolveInStack n fs
            in ls <|> cs <|> rs
        resolveInEnvironment n Environment {..} =
          let ss = resolveInStack n stack
              gs = lookupC n globals
          in ss <|> gs
    resolveInEnvironment name (environment s) ?? NameResolutionError lExpr

inspect :: IdentAddress -> Store -> Interpretation ShallowValue
inspect i s = do
  Ident _ msa mha <- lookupC i (idents s) ?? IdentResolutionError i
  msv <- case msa of
    Null -> return Null
    Valid sa -> lookupC sa (stackMS $ memory s) ?? StackResolutionError sa
  mhv <- case mha of
    Null -> return Null
    Valid ha -> lookupC ha (heapMS $ memory s) ?? HeapResolutionError ha
  recompose (msv, mhv)

declare :: LeftExpression -> Store -> Interpretation Store
declare lExpr s = case lExpr of
  Qualified prefix suffix -> do
    pIdentAddr <- fromShare <$> resolve prefix s !? AllocationError prefix
    pIdent <- lookupC pIdentAddr (idents s) ?? IdentResolutionError pIdentAddr
    let dependentsChanges = ChangesTo [(suffix, Just Null)]
    let storeChanges =
          ChangesTo $ nil { idents = [ ( pIdentAddr
                                       , Just pIdent { dependents = dependents pIdent <<> dependentsChanges
                                                     })]
                          }
    return $ s <<> storeChanges
  Unqualified name -> do
    let environmentChanges = case stack $ environment s of
          [] -> nil { globals = [(name, Just Null)] }
          _ -> nil { stack = [Frame [(name, Just Null)] nil] }
    return $ s <<> ChangesTo nil { environment = environmentChanges }

allocate :: LeftExpression -> Store -> Interpretation Store
allocate lExpr s = do
  lIdentAddr <- resolve lExpr s
  when (lIdentAddr /= Null) $ throwE "Allocating for already allocated lExpr."
  nIdentAddr <- freshA
  nChanges <- case lExpr of
    Qualified prefix suffix -> do
      pIdentAddr <- fromShare <$> resolve prefix s !? AllocationError prefix
      pIdent <- lookupC pIdentAddr (idents s) ?? IdentResolutionError pIdentAddr
      return $ nil { idents = [ ( pIdentAddr
                                , Just pIdent { dependents = dependents pIdent
                                                <<> [(suffix, Just $ Valid $ Owned nIdentAddr)]
                                              }
                                )
                              ]
                   }
    Unqualified name -> do
      let environmentChanges = case stack $ environment s of
            [] -> nil { globals = [(name, Just $ Valid $ Owned nIdentAddr)] }
            _ -> nil  { stack = [Frame [(name, Just $ Valid $ Owned nIdentAddr)] nil] }
      return $ nil { environment = environmentChanges }
  return $ s <<> nChanges
             <<> nil { idents = [(nIdentAddr, Just nil)]
                     }

lookupC :: Ord k => k -> Mapping k v -> Maybe v
lookupC k m = if isConcrete m then fromJust <$> M.lookup k m else error "Concrete lookup on non-concrete map."

lookM :: Ord k => Nullable k -> Mapping k v -> Maybe v
lookM k m = case k of
  Null -> Nothing
  Valid k' -> lookupC k' m

dependentsOf :: IdentAddress -> Store -> Interpretation Namespace
dependentsOf i s = dependents <$> (lookupC i (idents s) ?? "Bojo")

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
  -- Synchronizing assignments are assignments which as part of their interpretation, force some of
  -- the lExprs mentioned within their rExpr to be synchronized.
  Assignment lExpr (BidExpression (Bid (Synchronizing x) m)) ->
    return $ [ Synchronization x
             , Assignment lExpr (BidExpression (Bid (NonSynchronizing x) m))
             ] ++ cs :/: s
  Assignment lExpr (Application (Synchronizing f) x) ->
    return $ [ Synchronization f
             , Assignment lExpr (Application (NonSynchronizing f) x)
             ] ++ cs :/: s

  -- Function application resolves and inspects the function, and generates the subsequent clauses
  -- necessary to 1) materialize the formal argument, 2) run the body, 3) materialize the return
  -- value, and 3) return.

  -- Any non-synchronizing assignment _requires_ that the LHS be declared, at the very least. This
  -- establishes the current scope as the LHS' defining scope.
  Assignment lExpr rExpr -> resolve lExpr s `hushE` NameResolutionError lExpr >>= \case

    -- If the LHS doesn't resolve, declare it now, and redo the assignment.
    Nothing -> tell [ClauseEvent c "Declaration"] >> ((c:cs) :/:) <$> declare lExpr s
    -- If the LHS doesn't resolve, it needs to be declared _immediately_, at the very least. This
    -- establishes the LHS' defining scope, allowing subsequent allocation (immediately or
    -- otherwise).
    -- If the LHS resolves, it may still resolve to a @Null@ identity address, indicating that it
-- has been declared but not allocated. Whether this is acceptable depends on the RHS.
-- an assignment by any
  --   other bid, or of a literal value _requires_ that the LHS be allocated; an application doesn't
--   care (it defers the requirement to the return value assignment).

    -- Some RHS clauses only require that the LHS resolve, not that it necessarily resolves to
    -- anything meaningful.
    Just nlIdentAddr -> case rExpr of
      -- Function application delegates the allocation (or lack thereof) of the LHS to the
      -- assignment of the return value.
      Application (NonSynchronizing f) x -> do
        i <- fromShare <$> resolve f s !? NameResolutionError f

        -- Function application technically only requires _shallow_ inspection, which would allow
        -- closure to be inconsistent. This however, is far to complicated to handle, so deep
        -- synchronization it is.
        inspect i s >>= \case
          FunctionValue (Abstraction formalName body returnExpr) ->
            return $ [ Assignment (Unqualified formalName) (BidExpression x)
                     ] ++ body ++
                     [ Assignment lExpr returnExpr
                     , Return
                     ] :/: s

          -- This would be more disciplined as a type error.
          _ -> throwE "Application of a non-function."

      -- Even if the LHS does resolve, it may resolve to a @Null@ identity address, which means it
      -- isn't associated with any identity, owned or otherwise.
      _ -> case nlIdentAddr of

        Null -> case rExpr of
          -- An assignment of a bid by reference _requires_ that the LHS resolve to a @Null@ identity
          -- address, so that it may associate the LHS with the identity address of the RHS by
          -- borrowing it.
          BidExpression (Bid _ Refr) -> _

          -- Any other assignment would produce a value to be stored in the identity held by the
          -- LHS, thereby requiring that it be allocated. Allocate it in this case, then redo the assignment.
          _ -> tell [ClauseEvent c "Allocation"] >> ((c:cs) :/:) <$> allocate lExpr s

        -- For all remaining assignment forms, the ownership status of the LHS identity is
        -- irrelevant, so we may safely unwrap it.
        Valid (fromShare -> lIdentAddr) -> do

          -- The LHS identity _must_ exist; this lookup cannot fail.
          lIdent <- lookupC lIdentAddr (idents s) ?? IdentResolutionError lIdentAddr

          case rExpr of
            BidExpression (Bid (Synchronizing _) _) -> error "Unreachable"
            Application _ _ -> error "Unreachable"
            -- Reference assignments are ill-formed when the LHS resolves to a non-null identity address.
            BidExpression (Bid (NonSynchronizing _) Refr) -> throwE "Referencing to an existing identity."
            BidExpression (Bid (NonSynchronizing _) Move) -> _
            BidExpression (Bid (NonSynchronizing rName) Copy) -> do
              rIdentAddr <- fromShare <$> resolve rName s !? AllocationError rName
              rIdent <- lookupC rIdentAddr (idents s) ?? IdentResolutionError rIdentAddr
              let mrStackValue = lookM (stackAddress rIdent) (stackMS $ memory s)
              let mrHeapValue = lookM (heapAddress rIdent) (heapMS $ memory s)
              newStackAddr <- gFreshA (stackAddress rIdent)
              newHeapAddr <- gFreshA (heapAddress rIdent)
              let storeChanges = nil { idents = [ ( lIdentAddr
                                                  , Just Ident { dependents = nil
                                                               , stackAddress = newStackAddr
                                                               , heapAddress = newHeapAddr
                                                               }
                                                  )
                                                ]
                                     , memory = nil { stackMS = prepareChanges [ (newStackAddr, mrStackValue)
                                                                               , (stackAddress lIdent, Nothing)
                                                                               ]
                                                    , heapMS = prepareChanges [(newHeapAddr, mrHeapValue)
                                                                              , (heapAddress lIdent, Nothing)
                                                                              ]
                                                    }
                                     }
              return (cs :/: s <<> storeChanges)

            LiteralExpression (SmallLiteral i) -> do
              tell [ClauseEvent c "Small Literal Asignment"]
              (msv, Null) <- decompose (SmallValue i)
              newStackAddr <- freshA
              let storeChanges = nil { idents = [ ( lIdentAddr
                                                  , Just Ident { dependents = nil
                                                               , stackAddress = Valid newStackAddr
                                                               , heapAddress = nil
                                                               }
                                                  )
                                                ]
                                     , memory = nil {stackMS = prepareChanges [(Valid newStackAddr, Just msv)]}
                                     }
              return (cs :/: s <<> storeChanges)
            LiteralExpression (LargeLiteral _) -> _
            LiteralExpression (CaptureExpression _ _) -> _
  Synchronization lExpr -> do
    i <- fromShare <$> resolve lExpr s !? AllocationError lExpr
    v <- inspect i s
    tell [SynchronizationEvent lExpr v]
    return (cs :/: s)
  Return -> _
