{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Topology.Free.Arrow
-- Description : Reusable free arrow + monad bind + lineage framework
--
-- This module factors out the /algebraic structure/ that
-- "Kafka.Streams.Topology.Free" was built on top of. The
-- structure is independent of Kafka — it's a generic
-- 'FreeArrow' parameterised over a /primitive/ type @p@ that
-- supplies the domain-specific operations.
--
-- Concretely, the existing 'Kafka.Streams.Topology.Free.Topology'
-- could be re-expressed as
--
-- @
-- type Topology = 'FreeArrow' Prim
-- @
--
-- where @Prim@ is a closed GADT carrying the ~65 Kafka-specific
-- operations ('Source', 'Sink', 'MapValues', joins, …). All the
-- framework operations (composition, parallel, fanout, lineage,
-- monad bind, the 'Category' \/ 'Arrow' \/ 'ArrowChoice' \/
-- 'Applicative' \/ 'Monad' \/ 'Semigroup' \/ 'Monoid' instances,
-- the generic interpreter, and the optimisation laws that don't
-- inspect primitive shape) would live here, /once/, reusable for
-- any DSL that wants the same shape.
--
-- == What's reusable
--
--   * The 'FreeArrow' GADT with its Category, Arrow, ArrowChoice,
--     Applicative, Monad, Semigroup, and Monoid instances, plus
--     the 'Data.Profunctor.Profunctor' \/ 'Data.Profunctor.Strong'
--     \/ 'Data.Profunctor.Choice' hierarchy.
--   * The profunctor-shaped helpers ('lmapFA', 'rmapFA', 'dimapFA').
--   * The reader-shaped helpers ('askInputFA', 'localInputFA',
--     'applyValueFA').
--   * A generic 'interpret' that walks the structure given an
--     interpretation for primitives.
--   * A generic 'inspect' / 'inspectDeep' that produces token
--     listings given a primitive labeller.
--   * Generic framework-level rewrites ('Arr' fusion, identity
--     collapse, right-associate 'Compose', push-pure-through
--     'First' \/ 'Second' \/ 'Parallel' \/ 'Fanout' \/ 'Fork') —
--     run automatically by 'simplifyFA'.
--
-- == What stays domain-specific
--
--   * The 'Prim' GADT itself — its constructors are the
--     primitives the DSL exposes.
--   * The Prim-specific optimisations (e.g. fusing two
--     @MapValues@ into one) — implemented by pattern-matching
--     on @Lift (Prim_X _) `Compose` Lift (Prim_Y _)@.
--   * The Prim interpreter — what each primitive /does/ when
--     run against the host effect monad.
--   * Smart constructors — they just call 'Lift' on the
--     appropriate Prim constructor.
--
-- == Migration status
--
-- "Kafka.Streams.Topology.Free" now /is/ a consumer of this
-- framework: @type 'Topology' = 'FreeArrow' 'Prim'@. The
-- ~80 GADT constructors that used to live in @Topology@ have
-- been split — 15 framework constructors moved here, the
-- remaining 63 Kafka-specific ones became @Prim@. The public
-- API for 'Topology' is unchanged.
module Kafka.Streams.Topology.Free.Arrow
  ( -- * The free arrow
    FreeArrow (..)
  , lift

    -- * Lineage helpers
  , fork
  , forkN
  , tap

    -- * Profunctor-shaped helpers
  , lmapFA
  , rmapFA
  , dimapFA

    -- * Reader-shaped helpers
  , askInputFA
  , localInputFA
  , applyValueFA

    -- * Generic interpreter
  , interpret
  , interpretIO
  , interpretTraced

    -- * Generic introspection
  , inspectFA
  , inspectFADeep
  , prettyPrintFA
  , countNodesFA

    -- * Generic framework-level optimisation
  , simplifyFA
  ) where

import Prelude hiding (id, (.))

import Control.Arrow (Arrow (..), ArrowChoice (..))
import Control.Category (Category (..))
import qualified Control.Category as Cat
import Control.Monad ((>=>))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Profunctor (Choice (..), Profunctor (..), Strong (..))
import Data.Text (Text)
import qualified Data.Text as T

----------------------------------------------------------------------
-- The free arrow GADT
----------------------------------------------------------------------

-- | A free /arrow/ over a primitive type @p@.
--
-- @'FreeArrow' p i o@ is the type of programs that:
--
--   * Take an input of type @i@.
--   * Produce an output of type @o@.
--   * Compose primitives @p :: * -> * -> *@ via the
--     'Category' / 'Arrow' / 'ArrowChoice' combinators plus
--     lineage extras ('Fork', 'ForkN', 'Tap') and a free
--     reader-monad 'Bind'.
--
-- The /interpretation/ of the programs is supplied externally
-- (via 'interpret'): callers decide what each primitive
-- /means/ when run against their effect monad. The same
-- 'FreeArrow' value can be inspected, optimised at the
-- framework level ('simplifyFA'), and interpreted into
-- different host monads.
data FreeArrow p i o where
  -- ------------------------------------------------------------------
  -- Category
  -- ------------------------------------------------------------------

  -- | The identity arrow.
  Id      :: FreeArrow p a a

  -- | Right-to-left composition: @'Compose' g f@ runs @f@ then @g@.
  Compose :: FreeArrow p b c -> FreeArrow p a b -> FreeArrow p a c

  -- ------------------------------------------------------------------
  -- Arrow
  -- ------------------------------------------------------------------

  -- | Lift a pure function. Pure functions don't visit a
  -- primitive — they fuse with adjacent 'Arr's and push
  -- through 'First' \/ 'Second' \/ 'Parallel' \/ 'Fanout' \/
  -- 'Fork' via 'simplifyFA'.
  Arr     :: (a -> b) -> FreeArrow p a b

  -- | Operate on the left of a pair.
  First   :: FreeArrow p a b -> FreeArrow p (a, c) (b, c)

  -- | Operate on the right of a pair.
  Second  :: FreeArrow p a b -> FreeArrow p (c, a) (c, b)

  -- | Operate on both halves of a pair independently.
  Parallel :: FreeArrow p a b -> FreeArrow p c d -> FreeArrow p (a, c) (b, d)

  -- | Feed one input to two sub-arrows and pair their outputs.
  Fanout  :: FreeArrow p a b -> FreeArrow p a c -> FreeArrow p a (b, c)

  -- ------------------------------------------------------------------
  -- ArrowChoice
  -- ------------------------------------------------------------------

  -- | Operate on the @Left@ side of a sum.
  LeftT   :: FreeArrow p a b -> FreeArrow p (Either a c) (Either b c)

  -- | Operate on the @Right@ side of a sum.
  RightT  :: FreeArrow p a b -> FreeArrow p (Either c a) (Either c b)

  -- | Operate on both sides of a sum independently.
  Plus    :: FreeArrow p a b -> FreeArrow p c d -> FreeArrow p (Either a c) (Either b d)

  -- | Collapse a sum into one output.
  Fanin   :: FreeArrow p a c -> FreeArrow p b c -> FreeArrow p (Either a b) c

  -- ------------------------------------------------------------------
  -- Lineage extras
  -- ------------------------------------------------------------------

  -- | Duplicate the input wire.
  Fork    :: FreeArrow p a (a, a)

  -- | N-way fan-out: apply each sub-arrow to the same input
  -- and collect their outputs.
  ForkN   :: !(NonEmpty (FreeArrow p a b)) -> FreeArrow p a (NonEmpty b)

  -- | Run a side-effecting sub-arrow (a @'FreeArrow' p a ()@)
  -- and pass the wire through unchanged.
  Tap     :: FreeArrow p a () -> FreeArrow p a a

  -- ------------------------------------------------------------------
  -- Monad bind (free reader)
  -- ------------------------------------------------------------------

  -- | The classical monad bind, threading the /input/ value
  -- as the environment. The bind continuation is an opaque
  -- Haskell function — 'inspectFA' can't see past it, but
  -- 'inspectFADeep' (which runs the interpreter) can.
  Bind    :: FreeArrow p i a -> (a -> FreeArrow p i b) -> FreeArrow p i b

  -- ------------------------------------------------------------------
  -- The primitive
  -- ------------------------------------------------------------------

  -- | Embed a domain-specific primitive into the free arrow.
  -- Everything domain-specific that isn't a pure function or
  -- a structural combinator lives behind 'Lift'.
  Lift    :: p i o -> FreeArrow p i o

----------------------------------------------------------------------
-- Smart constructor for 'Lift'
----------------------------------------------------------------------

-- | Synonym for the 'Lift' constructor. Provided so callers
-- don't have to import the GADT internals just to wrap a
-- primitive.
lift :: p i o -> FreeArrow p i o
lift = Lift

----------------------------------------------------------------------
-- Lineage helpers
----------------------------------------------------------------------

-- | Same as @'id' '&&&' 'id'@ but a dedicated constructor so the
-- optimiser can recognise it and so 'inspect' shows a clear
-- token.
fork :: FreeArrow p a (a, a)
fork = Fork

-- | N-way fan-out.
forkN :: NonEmpty (FreeArrow p a b) -> FreeArrow p a (NonEmpty b)
forkN = ForkN

-- | Run a closed-output side pipeline; pass the wire through.
tap :: FreeArrow p a () -> FreeArrow p a a
tap = Tap

----------------------------------------------------------------------
-- Category / Arrow / ArrowChoice instances
----------------------------------------------------------------------

instance Category (FreeArrow p) where
  id = Id
  Id . f  = f
  g  . Id = g
  g  . f  = Compose g f

instance Arrow (FreeArrow p) where
  arr     = Arr
  first   = First
  second  = Second
  (***)   = Parallel
  (&&&)   = Fanout

instance ArrowChoice (FreeArrow p) where
  left    = LeftT
  right   = RightT
  (+++)   = Plus
  (|||)   = Fanin

----------------------------------------------------------------------
-- Functor / Applicative / Monad / Semigroup / Monoid
----------------------------------------------------------------------

instance Functor (FreeArrow p a) where
  fmap f t = Arr f `Compose` t

instance Applicative (FreeArrow p i) where
  pure x = Arr (const x)
  tf <*> tx = Compose (Arr (uncurry ($))) (Fanout tf tx)

instance Monad (FreeArrow p i) where
  return = pure
  (>>=) = Bind

instance Semigroup o => Semigroup (FreeArrow p i o) where
  t1 <> t2 = Compose (Arr (uncurry (<>))) (Fanout t1 t2)

instance Monoid o => Monoid (FreeArrow p i o) where
  mempty = Arr (const mempty)

----------------------------------------------------------------------
-- Profunctor / Strong / Choice
----------------------------------------------------------------------

-- | A 'FreeArrow' is a profunctor: contravariant in its input,
-- covariant in its output. 'dimap' \/ 'lmap' \/ 'rmap' are the
-- pure-function pre\/post-composition helpers (see 'dimapFA',
-- 'lmapFA', 'rmapFA') exposed under their canonical class names
-- so callers can program against the @profunctors@ vocabulary
-- (and the optics built on top of it) when wiring topology
-- fragments together.
instance Profunctor (FreeArrow p) where
  dimap = dimapFA
  lmap  = lmapFA
  rmap  = rmapFA

-- | 'Strong' threads an untouched component alongside the wire,
-- reusing the 'Arrow' product combinators 'First' \/ 'Second'.
-- @'first'' = 'first'@ and @'second'' = 'second'@; spelling them
-- out as the profunctor methods lets 'FreeArrow' be used with
-- strength-based optics (lenses).
instance Strong (FreeArrow p) where
  first'  = First
  second' = Second

-- | 'Choice' routes one side of a sum through the arrow and
-- passes the other through untouched, reusing the 'ArrowChoice'
-- combinators 'LeftT' \/ 'RightT'. @'left'' = 'left'@ and
-- @'right'' = 'right'@; this is what lets 'FreeArrow' drive
-- prism-based optics.
instance Choice (FreeArrow p) where
  left'  = LeftT
  right' = RightT

----------------------------------------------------------------------
-- Profunctor / Reader helpers (standalone — no class deps)
----------------------------------------------------------------------

-- | Pre-compose with a pure function. Profunctor 'lmap'.
lmapFA :: (a -> b) -> FreeArrow p b c -> FreeArrow p a c
lmapFA f t = Compose t (Arr f)

-- | Post-compose with a pure function. Profunctor 'rmap'.
rmapFA :: (c -> d) -> FreeArrow p b c -> FreeArrow p b d
rmapFA g t = Compose (Arr g) t

-- | Pre- and post-compose with pure functions. Profunctor
-- 'dimap'.
dimapFA :: (a -> b) -> (c -> d) -> FreeArrow p b c -> FreeArrow p a d
dimapFA f g = rmapFA g . lmapFA f

-- | The input wire as a free arrow. Reader-monad analogue.
askInputFA :: FreeArrow p i i
askInputFA = Id

-- | Pre-transform the input before running an arrow.
localInputFA :: (i' -> i) -> FreeArrow p i a -> FreeArrow p i' a
localInputFA f t = Compose t (Arr f)

-- | Pre-feed a fixed value into an arrow. Powers monad-bind
-- ergonomics: @op \`applyValueFA\` x@ runs @op@ with @x@ as its
-- input regardless of the caller's input type.
applyValueFA :: FreeArrow p a b -> a -> FreeArrow p i b
applyValueFA t a = Compose t (Arr (const a))

----------------------------------------------------------------------
-- Generic interpreter
----------------------------------------------------------------------

-- | The generic interpreter. Walks a 'FreeArrow' against a
-- primitive-interpreter @runPrim@ that explains what each
-- primitive does in the host monad @m@.
--
-- Each framework constructor is interpreted polymorphically;
-- only the 'Lift' case dispatches to @runPrim@. This is the
-- one-line theorem that gives 'FreeArrow' its name: the
-- structure is /free/ over the choice of primitives.
--
-- @
-- 'interpret' runPrim 'Id'           = 'pure'
-- 'interpret' runPrim ('Compose' g f) = 'interpret' runPrim f '>=>' 'interpret' runPrim g
-- 'interpret' runPrim ('Arr' f)       = 'pure' '.' f
-- 'interpret' runPrim ('Lift' p)      = runPrim p
-- ... and so on for every framework constructor.
-- @
interpret
  :: forall p m i o
   . Monad m
  => (forall a b. p a b -> a -> m b)
  -> FreeArrow p i o -> i -> m o
interpret runPrim = go
  where
    go :: forall x y. FreeArrow p x y -> x -> m y
    -- Category / Arrow
    go Id            = pure
    go (Compose g f) = go f >=> go g
    go (Arr f)       = pure . f
    go (First t)     = \(a, c) -> do
      !b <- go t a
      pure (b, c)
    go (Second t)    = \(c, a) -> do
      !b <- go t a
      pure (c, b)
    go (Parallel p q) = \(a, c) -> do
      !b <- go p a
      !d <- go q c
      pure (b, d)
    go (Fanout p q)   = \a -> do
      !b <- go p a
      !c <- go q a
      pure (b, c)
    -- ArrowChoice
    go (LeftT t)     = \case
      Left  a -> Left  <$> go t a
      Right c -> pure (Right c)
    go (RightT t)    = \case
      Left  c -> pure (Left c)
      Right a -> Right <$> go t a
    go (Plus p q)    = \case
      Left  a -> Left  <$> go p a
      Right c -> Right <$> go q c
    go (Fanin p q)   = \case
      Left  a -> go p a
      Right c -> go q c
    -- Lineage
    go Fork          = \a -> pure (a, a)
    go (ForkN ts)    = \a -> traverse (`go` a) ts
    go (Tap t)       = \a -> do
      !() <- go t a
      pure a
    -- Bind
    go (Bind t k)    = \i -> do
      !a <- go t i
      go (k a) i
    -- The primitive
    go (Lift p)      = runPrim p

-- | 'interpret' specialised to @m = IO@. Convenient when the
-- host effect monad is IO (the common case for streaming /
-- topology DSLs that want to register nodes against a
-- mutable builder).
interpretIO
  :: (forall a b. p a b -> a -> IO b)
  -> FreeArrow p i o -> i -> IO o
interpretIO = interpret

----------------------------------------------------------------------
-- Generic introspection
----------------------------------------------------------------------

-- | Walk a 'FreeArrow' and emit a flat token listing, given a
-- caller-supplied labeller for primitives. Mirrors
-- 'Kafka.Streams.Topology.Free.inspect' but parameterised
-- over the primitive type.
--
-- The framework constructors emit their canonical tokens:
-- @\"Id\"@, @\"Arr\"@, @\"First<…>\"@, @\"Parallel<…|…>\"@,
-- etc. The 'Lift' case calls @labelPrim@; the result is
-- inserted as a single token.
--
-- 'Bind' is opaque past the continuation — emits one
-- @\"Bind<…opaque>\"@ marker plus the visible left side. See
-- 'inspectFADeep' for the version that runs the interpreter
-- to walk through binds.
inspectFA
  :: forall p i o
   . (forall a b. p a b -> Text)   -- ^ primitive labeller
  -> FreeArrow p i o
  -> [Text]
inspectFA labelPrim = go
  where
    go :: forall x y. FreeArrow p x y -> [Text]
    go Id              = ["Id"]
    go (Compose g f)   = go f ++ go g
    go (Arr _)         = ["Arr"]
    go (First t)       = "First<" : go t ++ [">"]
    go (Second t)      = "Second<" : go t ++ [">"]
    go (Parallel p q)  = "Parallel<" : go p ++ "|" : go q ++ [">"]
    go (Fanout p q)    = "Fanout<"   : go p ++ "|" : go q ++ [">"]
    go (LeftT t)       = "Left<"     : go t ++ [">"]
    go (RightT t)      = "Right<"    : go t ++ [">"]
    go (Plus p q)      = "Plus<"     : go p ++ "|" : go q ++ [">"]
    go (Fanin p q)     = "Fanin<"    : go p ++ "|" : go q ++ [">"]
    go Fork            = ["Fork"]
    go (ForkN ts)      = "ForkN<" : concatMap go (NE.toList ts) ++ [">"]
    go (Tap t)         = "Tap<" : go t ++ [">"]
    go (Bind t _)      = "Bind<" : go t ++ ["…opaque>"]
    go (Lift p)        = [labelPrim p]

-- | Like 'inspectFA' but walks /through/ 'Bind' continuations
-- by actually running the supplied 'interpret' to materialise
-- the wire value and pass it to the continuation. Only works
-- for closed-input @'FreeArrow' p ()_open o@-style topologies
-- where the caller has supplied a /seed input/ to feed the
-- walk.
--
-- The supplied @runPrim@ should be the same interpreter the
-- caller would use to compile the topology — so the wire
-- values seen by bind continuations match what they'd see at
-- run time.
inspectFADeep
  :: forall p i o
   . (forall a b. p a b -> Text)
  -> (forall a b. p a b -> a -> IO b)
  -> FreeArrow p i o
  -> i
  -> IO [Text]
inspectFADeep labelPrim runPrim t seed = do
  ref <- newIORef ([] :: [[Text]])
  let trace toks = modifyIORef' ref (toks :)
  _ <- interpretTraced trace labelPrim runPrim t seed
  collected <- readIORef ref
  pure (concat (reverse collected))

-- | 'interpret' with a tracer callback invoked at each leaf
-- and structural-bracket point. Used internally by
-- 'inspectFADeep' to produce a token list whose semantics
-- match 'inspectFA' but where binds are walked past.
interpretTraced
  :: forall p i o
   . ([Text] -> IO ())
  -> (forall a b. p a b -> Text)
  -> (forall a b. p a b -> a -> IO b)
  -> FreeArrow p i o
  -> i
  -> IO o
interpretTraced tracer labelPrim runPrim = go
  where
    trace = tracer
    go :: forall x y. FreeArrow p x y -> x -> IO y
    go Id              i = trace ["Id"] >> pure i
    go (Compose g f)   i = do
      !mid <- go f i
      go g mid
    go (Arr f)         i = trace ["Arr"] >> pure (f i)
    go (First t)   (a,c) = do
      trace ["First<"]; !b' <- go t a; trace [">"]; pure (b', c)
    go (Second t)  (c,a) = do
      trace ["Second<"]; !b' <- go t a; trace [">"]; pure (c, b')
    go (Parallel p q) (a,c) = do
      trace ["Parallel<"]; !b' <- go p a; trace ["|"]
      !d' <- go q c; trace [">"]; pure (b', d')
    go (Fanout p q)    a = do
      trace ["Fanout<"]; !b' <- go p a; trace ["|"]
      !c' <- go q a; trace [">"]; pure (b', c')
    go (LeftT t) e = case e of
      Left  a -> do
        trace ["Left<"]; !b' <- go t a; trace [">"]; pure (Left b')
      Right c -> trace ["Left<", ">"] >> pure (Right c)
    go (RightT t) e = case e of
      Left  c -> trace ["Right<", ">"] >> pure (Left c)
      Right a -> do
        trace ["Right<"]; !b' <- go t a; trace [">"]; pure (Right b')
    go (Plus p q) e = case e of
      Left a  -> do
        trace ["Plus<"]; !b' <- go p a; trace ["|", ">"]; pure (Left b')
      Right c -> do
        trace ["Plus<", "|"]; !d' <- go q c; trace [">"]; pure (Right d')
    go (Fanin p q) e = case e of
      Left a  -> do
        trace ["Fanin<"]; !b' <- go p a; trace ["|", ">"]; pure b'
      Right c -> do
        trace ["Fanin<", "|"]; !d' <- go q c; trace [">"]; pure d'
    go Fork a            = trace ["Fork"] >> pure (a, a)
    go (ForkN ts) a      = do
      trace ["ForkN<"]; !ys <- traverse (`go` a) ts
      trace [">"]; pure ys
    go (Tap t) a         = do
      trace ["Tap<"]; !_ <- go t a; trace [">"]; pure a
    go (Bind t k) i      = do
      trace ["Bind<"]; !a <- go t i; trace [">~>"]
      !out <- go (k a) i; trace ["</Bind>"]; pure out
    go (Lift p) i        = do
      trace [labelPrim p]
      runPrim p i

-- | 'inspectFA' joined with whitespace.
prettyPrintFA
  :: (forall a b. p a b -> Text) -> FreeArrow p i o -> Text
prettyPrintFA labelPrim = T.intercalate " " . inspectFA labelPrim

-- | Count the constructors in a 'FreeArrow' AST. Mirrors
-- 'Kafka.Streams.Topology.Free.countNodes' for the framework
-- part; primitives count as one each.
countNodesFA :: FreeArrow p i o -> Int
countNodesFA = go
  where
    go :: forall p' x y. FreeArrow p' x y -> Int
    go Id              = 1
    go (Compose g f)   = 1 + go g + go f
    go (Arr _)         = 1
    go (First t)       = 1 + go t
    go (Second t)      = 1 + go t
    go (Parallel p q)  = 1 + go p + go q
    go (Fanout p q)    = 1 + go p + go q
    go (LeftT t)       = 1 + go t
    go (RightT t)      = 1 + go t
    go (Plus p q)      = 1 + go p + go q
    go (Fanin p q)     = 1 + go p + go q
    go Fork            = 1
    go (ForkN ts)      = 1 + sum (NE.map go ts)
    go (Tap t)         = 1 + go t
    go (Bind t _)      = 1 + go t
    go (Lift _)        = 1

----------------------------------------------------------------------
-- Generic framework-level rewrites
----------------------------------------------------------------------

-- | A single bottom-up pass applying the framework-level
-- algebraic laws. /Doesn't/ inspect primitives — those
-- rewrites are domain-specific and live in the DSL on top.
--
-- Applies, in order:
--
--   * Identity collapse: @'Id' '.' f = f@, @f '.' 'Id' = f@.
--   * 'Arr' fusion: @'Arr' g '.' 'Arr' f = 'Arr' (g . f)@.
--   * Push pure functions through 'First' \/ 'Second' \/
--     'Parallel' \/ 'Fanout' \/ 'Fork'.
--   * Right-associate 'Compose' to surface adjacent
--     leaves for the per-DSL fusion pass to spot.
--   * Collapse 'First' 'Id' \/ 'Second' 'Id' \/
--     'Parallel' 'Id' 'Id' \/ 'Plus' 'Id' 'Id' \/ 'Tap' 'Id'
--     to 'Id'.
--
-- The implementation is recursive but bounded: each pass is
-- strictly node-count-non-increasing.
simplifyFA :: forall p i o. FreeArrow p i o -> FreeArrow p i o
simplifyFA = go
  where
    go :: forall x y. FreeArrow p x y -> FreeArrow p x y
    go (Compose g f)   = smartCompose (go g) (go f)
    go (First t)       = collapseFirst (go t)
    go (Second t)      = collapseSecond (go t)
    go (Parallel p q)  = collapseParallel (go p) (go q)
    go (Fanout p q)    = collapseFanout   (go p) (go q)
    go (LeftT t)       = collapseLeft  (go t)
    go (RightT t)      = collapseRight (go t)
    go (Plus p q)      = collapsePlus  (go p) (go q)
    go (Fanin p q)     = Fanin (go p) (go q)
    go (ForkN ts)      = ForkN (NE.map go ts)
    go (Tap t)         = collapseTap (go t)
    go (Bind t k)      = Bind (go t) k
    go x               = x

    collapseFirst :: FreeArrow p a b -> FreeArrow p (a, c) (b, c)
    collapseFirst Id      = Id
    collapseFirst (Arr f) = Arr (\(a, c) -> (f a, c))
    collapseFirst t       = First t

    collapseSecond :: FreeArrow p a b -> FreeArrow p (c, a) (c, b)
    collapseSecond Id      = Id
    collapseSecond (Arr f) = Arr (\(c, a) -> (c, f a))
    collapseSecond t       = Second t

    collapseParallel
      :: FreeArrow p a b -> FreeArrow p c d -> FreeArrow p (a, c) (b, d)
    collapseParallel Id      Id      = Id
    collapseParallel (Arr f) (Arr g) = Arr (\(a, c) -> (f a, g c))
    collapseParallel p       q       = Parallel p q

    collapseFanout
      :: FreeArrow p a b -> FreeArrow p a c -> FreeArrow p a (b, c)
    collapseFanout (Arr f) (Arr g) = Arr (\a -> (f a, g a))
    collapseFanout p       q       = Fanout p q

    collapseLeft :: FreeArrow p a b -> FreeArrow p (Either a c) (Either b c)
    collapseLeft Id = Id
    collapseLeft t  = LeftT t

    collapseRight :: FreeArrow p a b -> FreeArrow p (Either c a) (Either c b)
    collapseRight Id = Id
    collapseRight t  = RightT t

    collapsePlus
      :: FreeArrow p a b -> FreeArrow p c d -> FreeArrow p (Either a c) (Either b d)
    collapsePlus Id Id = Id
    collapsePlus p  q  = Plus p q

    collapseTap :: FreeArrow p a () -> FreeArrow p a a
    collapseTap Id = Id
    collapseTap t  = Tap t

    smartCompose
      :: forall a b c
       . FreeArrow p b c -> FreeArrow p a b -> FreeArrow p a c
    -- Identity
    smartCompose Id f  = f
    smartCompose g  Id = g
    -- Pure-function fusion
    smartCompose (Arr g) (Arr f) = Arr (g Cat.. f)
    -- Push 'Arr' through 'Fork'
    smartCompose (Arr g) Fork    = Arr (\a -> g (a, a))
    -- Right-associate: (h . i) . f -> h . (i . f) so adjacent
    -- leaves surface along the right spine for DSL-specific
    -- fusion to spot.
    smartCompose (Compose h i) f = smartCompose h (smartCompose i f)
    -- Try to fuse with the head of an already-right-associated
    -- spine.
    smartCompose g (Compose h i) =
      case smartCompose g h of
        Compose g' h' -> Compose g' (Compose h' i)
        gh            -> smartCompose gh i
    smartCompose g f = Compose g f
