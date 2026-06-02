{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Kafka.Streams.Pipeline
-- Description : First-class composable topology fragments
--
-- A 'Pipeline a b' is a /value/ that describes a topology
-- transformation from a stream of type @a@ to a stream of
-- type @b@. Under the hood it's a 'Control.Arrow.Kleisli'
-- arrow over 'IO': @Pipeline a b ≃ a -> IO b@.
--
-- Pipelines compose with the usual 'Control.Category' /
-- 'Control.Arrow' vocabulary:
--
-- @
-- import Control.Category ((>>>))
--
-- normalise :: 'Pipeline' ('KStream' Text Text) ('KStream' Text Text)
-- normalise = 'pmapValues' T.toUpper
--         >>> 'pfilter'    (\\r -> recordValue r \/= \"\")
-- @
--
-- And you /apply/ a pipeline to a stream with 'applyPipeline'
-- (or just call the function directly):
--
-- @
-- src <- streamFromTopic b \"in\" (consumed textSerde textSerde)
-- out <- applyPipeline normalise src
-- toTopic \"out\" (produced textSerde textSerde) out
-- @
--
-- This module sits alongside (not on top of) the existing
-- imperative DSL — that API stays for users who prefer the
-- Java-style fluent shape.
--
-- 'Pipeline' also has 'Control.Arrow.Arrow' and
-- 'Control.Arrow.ArrowChoice' instances, so the full Kleisli
-- vocabulary (@first@, @second@, @***@, @&&&@, @left@, @right@,
-- @+++@, @|||@) works on it. Combined with the typed smart
-- constructors below this is enough to express most static
-- topology fragments as pure values.
--
-- A 'Pipeline' is exactly @'Data.Profunctor.Star' IO@, so it
-- carries the profunctor hierarchy that an @IO@-effected 'Star'
-- supports: 'Data.Profunctor.Profunctor' \/
-- 'Data.Profunctor.Strong' \/ 'Data.Profunctor.Choice' \/
-- 'Data.Profunctor.Traversing.Traversing', plus
-- 'Data.Profunctor.Sieve.Sieve' and
-- 'Data.Profunctor.Rep.Representable' (@'Rep' = IO@). So
-- @dimap@ \/ @lmap@ \/ @rmap@ (and the optics built on them)
-- re-shape its types, and @traverse'@ \/ @wander@ lift it over
-- a 'Traversable' or a van-Laarhoven traversal. Over its output
-- it is 'Functor' \/ 'Applicative' \/ 'Monad' and, inheriting
-- 'IO''s instances, 'Control.Applicative.Alternative' \/
-- 'Control.Monad.MonadPlus' (exception-fallback semantics).
-- This is the complete set of @'Data.Profunctor.Star' IO@
-- instances; the only ones a generic 'Star' has that 'Pipeline'
-- lacks are those needing 'IO' to be 'Distributive'
-- ('Data.Profunctor.Closed' \/ 'Data.Profunctor.Mapping') or
-- 'Traversable' ('Data.Profunctor.Cochoice'), which it is not.
module Kafka.Streams.Pipeline
  ( -- * Pipeline
    Pipeline (..)
  , applyPipeline

    -- * Smart constructors over 'KStream'
    -- ** Stateless transforms
  , pmapValues
  , pmapKeyValue
  , pfilter
  , pfilterNot
  , pconcatMapValues
  , pconcatMapKeyValue
  , ppeek
  , pselectKey
  , pvalues
    -- ** Composition / branching
  , pmerge
  , pmergeAll
  , pbranch
    -- ** Sinks
  , psink
  , psinkWith
  , pthrough
    -- ** Conversions
  , ptoTable
  , ptoStream
  , prepartition

    -- * Lifting / lowering
  , liftIOAction
  , liftPure
  ) where

import Control.Applicative (Alternative (..))
import Control.Arrow (Arrow (..), ArrowChoice (..))
import Control.Category (Category (..))
import Control.Monad (MonadPlus (..), (>=>))
import Data.Profunctor (Choice (..), Profunctor (..), Strong (..))
import Data.Profunctor.Rep (Representable (..))
import Data.Profunctor.Sieve (Sieve (..))
import Data.Profunctor.Traversing (Traversing (..))
import Prelude hiding (id, (.))

import qualified Kafka.Streams.KStream as KS
import Kafka.Streams.KTable (KTable)
import Kafka.Streams.Materialized (Materialized)
import Kafka.Streams.Produced (Produced, produced)
import Kafka.Streams.Serde (HasSerde, Serde)
import Kafka.Streams.Types (Record, TopicName, topicName)
import qualified Data.Text as T

----------------------------------------------------------------------
-- Pipeline
----------------------------------------------------------------------

-- | A composable topology fragment. Isomorphic to
-- 'Control.Arrow.Kleisli' @IO a b@; we use a dedicated
-- newtype so error messages and Haddock both refer to
-- @Pipeline@ instead of @Kleisli IO@.
newtype Pipeline a b = Pipeline
  { runPipeline :: a -> IO b
  }

-- | Category-style composition: 'id' is the no-op pipeline,
-- @q '.' p@ runs @p@ first then @q@. The same shape as
-- function composition; '(.)' is right-to-left, '(>>>)' is
-- left-to-right.
instance Category Pipeline where
  id :: Pipeline a a
  id = Pipeline pure

  (.) :: Pipeline b c -> Pipeline a b -> Pipeline a c
  Pipeline g . Pipeline f = Pipeline (f >=> g)

-- | 'Arrow' parallels the Kleisli IO arrow: 'arr' lifts a pure
-- function and 'first' / 'second' / @***@ / @&&&@ work as
-- expected. This is what enables 'proc' notation on
-- 'Pipeline' values.
instance Arrow Pipeline where
  arr :: forall a b. (a -> b) -> Pipeline a b
  arr f = Pipeline (pure . f)

  first :: forall a b c. Pipeline a b -> Pipeline (a, c) (b, c)
  first (Pipeline f) = Pipeline $ \(a, c) -> do
    !b <- f a
    pure (b, c)

  second :: forall a b c. Pipeline a b -> Pipeline (c, a) (c, b)
  second (Pipeline f) = Pipeline $ \(c, a) -> do
    !b <- f a
    pure (c, b)

  (***) :: Pipeline a b -> Pipeline c d -> Pipeline (a, c) (b, d)
  Pipeline f *** Pipeline g = Pipeline $ \(a, c) -> do
    !b <- f a
    !d <- g c
    pure (b, d)

  (&&&) :: Pipeline a b -> Pipeline a c -> Pipeline a (b, c)
  Pipeline f &&& Pipeline g = Pipeline $ \a -> do
    !b <- f a
    !c <- g a
    pure (b, c)

-- | 'ArrowChoice' for branching pipelines over @Either@. Useful
-- when 'splitStream' yields multiple branches and the per-branch
-- transforms differ in shape.
instance ArrowChoice Pipeline where
  left :: forall a b c. Pipeline a b -> Pipeline (Either a c) (Either b c)
  left (Pipeline f) = Pipeline $ \case
    Left a  -> Left <$> f a
    Right c -> pure (Right c)

  right :: forall a b c. Pipeline a b -> Pipeline (Either c a) (Either c b)
  right (Pipeline f) = Pipeline $ \case
    Left c  -> pure (Left c)
    Right a -> Right <$> f a

  (+++) :: Pipeline a b -> Pipeline c d -> Pipeline (Either a c) (Either b d)
  Pipeline f +++ Pipeline g = Pipeline $ \case
    Left a  -> Left <$> f a
    Right c -> Right <$> g c

  (|||) :: Pipeline a c -> Pipeline b c -> Pipeline (Either a b) c
  Pipeline f ||| Pipeline g = Pipeline $ \case
    Left a  -> f a
    Right b -> g b

-- | 'Profunctor' over @('a' -> IO 'b')@: 'lmap' adjusts the
-- input with a pure function, 'rmap' the output. Lets pipelines
-- be re-shaped to fit a surrounding composition without leaving
-- the 'Pipeline' vocabulary (and lets them be driven by the
-- optics built on @profunctors@).
instance Profunctor Pipeline where
  dimap f g (Pipeline h) = Pipeline (fmap g . h . f)
  lmap f (Pipeline h)    = Pipeline (h . f)
  rmap g (Pipeline h)    = Pipeline (fmap g . h)

-- | 'Strong' — carry a component past the pipeline untouched.
-- Agrees with the 'Arrow' product combinators: @'first'' =
-- 'first'@, @'second'' = 'second'@.
instance Strong Pipeline where
  first'  = first
  second' = second

-- | 'Choice' — run the pipeline on one branch of a sum and pass
-- the other through. Agrees with 'ArrowChoice': @'left'' =
-- 'left'@, @'right'' = 'right'@.
instance Choice Pipeline where
  left'  = left
  right' = right

-- | 'Traversing' — lift a pipeline over any 'Traversable'
-- container. @'traverse'' p@ runs @p@ on every element
-- (left-to-right, in 'IO') and rebuilds the structure; @wander@
-- runs @p@ at every focus of an arbitrary van-Laarhoven
-- traversal. This is what lets a single-record fragment be
-- applied to a batch, or driven by a lens-style @Traversal@.
--
-- Viable because the underlying effect is 'IO', which is
-- 'Applicative' (the only requirement 'Star' places on
-- 'Traversing'). The dual 'Data.Profunctor.Cochoice' \/
-- 'Data.Profunctor.Closed' \/ 'Data.Profunctor.Mapping' are
-- /not/ available: they need 'IO' to be 'Traversable' \/
-- 'Distributive', which it is not.
instance Traversing Pipeline where
  traverse' (Pipeline f) = Pipeline (traverse f)
  wander w (Pipeline f)  = Pipeline (w f)

-- | 'Sieve' — a 'Pipeline' /is/ its underlying @a -> IO b@
-- (it's @'Data.Profunctor.Star' IO@), so @'sieve' =
-- 'runPipeline'@.
instance Sieve Pipeline IO where
  sieve = runPipeline

-- | 'Representable' — the representation is 'IO', and
-- 'tabulate' is just the 'Pipeline' constructor. Together with
-- the 'Sieve' instance this records that 'Pipeline' is exactly
-- the representable profunctor on 'IO', so generic
-- 'Sieve' \/ 'Representable' code works on it unchanged.
instance Representable Pipeline where
  type Rep Pipeline = IO
  tabulate = Pipeline

-- | 'Functor' over the output type.
instance Functor (Pipeline a) where
  fmap f (Pipeline g) = Pipeline (fmap f . g)

-- | 'Applicative' parallel application: @f \<*\> x@ runs both
-- pipelines on the same input and combines their results.
instance Applicative (Pipeline a) where
  pure x = Pipeline (\_ -> pure x)
  Pipeline f <*> Pipeline x = Pipeline $ \a -> do
    !g <- f a
    !v <- x a
    pure (g v)

-- | 'Monad' over the output, threading the same input through
-- both steps (the @ReaderT a IO@ monad). Completes the
-- 'Functor' \/ 'Applicative' \/ 'Monad' trio; the continuation
-- sees the original input, so @p >>= k@ is "run @p@, then run
-- @k result@ on the same input".
instance Monad (Pipeline a) where
  Pipeline m >>= k = Pipeline $ \a -> do
    !b <- m a
    runPipeline (k b) a

-- | 'Alternative' inherited pointwise from 'IO''s instance:
-- 'empty' is a pipeline that always fails (an 'IO' exception),
-- and @p \<|\> q@ runs @p@ on the input and, /if it throws an
-- 'IOError'/, runs @q@ on the same input (the 'IO' fallback
-- semantics from @GHC.Base@). Mirrors @'Alternative' ('Star' IO
-- a)@.
instance Alternative (Pipeline a) where
  empty = Pipeline (const empty)
  Pipeline f <|> Pipeline g = Pipeline (\a -> f a <|> g a)

-- | 'MonadPlus' with the same exception-fallback semantics as
-- the 'Alternative' instance (@'mzero' = 'empty'@,
-- @'mplus' = '(<|>)'@). Mirrors @'MonadPlus' ('Star' IO a)@.
instance MonadPlus (Pipeline a)

-- | 'applyPipeline' is the canonical way to run a pipeline
-- against a stream. It's a synonym for 'runPipeline' chosen
-- for readability at call sites:
--
-- @
-- result <- applyPipeline pipeline source
-- @
applyPipeline :: Pipeline a b -> a -> IO b
applyPipeline = runPipeline

----------------------------------------------------------------------
-- Smart constructors — stateless transforms
----------------------------------------------------------------------

-- | Pure value-only transform.
pmapValues
  :: HasSerde v'
  => (v -> v')
  -> Pipeline (KS.KStream k v) (KS.KStream k v')
pmapValues f = Pipeline (KS.mapValues f)

-- | Pure key+value transform.
pmapKeyValue
  :: (HasSerde k', HasSerde v')
  => (k -> v -> (k', v'))
  -> Pipeline (KS.KStream k v) (KS.KStream k' v')
pmapKeyValue f = Pipeline (KS.mapKeyValue f)

-- | Predicate filter. Records the predicate rejects are
-- dropped from the downstream stream.
pfilter
  :: (Record k v -> Bool)
  -> Pipeline (KS.KStream k v) (KS.KStream k v)
pfilter p = Pipeline (KS.filterStream p)

-- | Inverse of 'pfilter'.
pfilterNot
  :: (Record k v -> Bool)
  -> Pipeline (KS.KStream k v) (KS.KStream k v)
pfilterNot p = Pipeline (KS.filterNotStream p)

-- | One-to-many value transform.
pconcatMapValues
  :: HasSerde v'
  => (v -> [v'])
  -> Pipeline (KS.KStream k v) (KS.KStream k v')
pconcatMapValues f = Pipeline (KS.concatMapValues f)

-- | One-to-many key+value transform.
pconcatMapKeyValue
  :: (HasSerde k', HasSerde v')
  => (k -> v -> [(k', v')])
  -> Pipeline (KS.KStream k v) (KS.KStream k' v')
pconcatMapKeyValue f = Pipeline (KS.concatMapKeyValue f)

-- | Side-effecting observer; doesn't change the stream.
ppeek
  :: (Record k v -> IO ())
  -> Pipeline (KS.KStream k v) (KS.KStream k v)
ppeek f = Pipeline (KS.peekStream f)

-- | Re-key the stream from the full record.
pselectKey
  :: HasSerde k'
  => (Record k v -> k')
  -> Pipeline (KS.KStream k v) (KS.KStream k' v)
pselectKey f = Pipeline (KS.selectKey f)

-- | Drop the key (mirrors @KStream.values()@).
pvalues
  :: Pipeline (KS.KStream k v) (KS.KStream () v)
pvalues = Pipeline KS.valuesStream

----------------------------------------------------------------------
-- Composition / branching
----------------------------------------------------------------------

-- | Merge a stream with a /captured/ companion stream. Useful
-- inside an 'arr'-heavy section of a 'Pipeline' to interleave
-- two named upstream sources without leaving the 'Pipeline'
-- vocabulary.
pmerge
  :: KS.KStream k v
  -> Pipeline (KS.KStream k v) (KS.KStream k v)
pmerge other = Pipeline (KS.mergeStreams other)

-- | Merge a stream with all captured companions.
pmergeAll
  :: [KS.KStream k v]
  -> Pipeline (KS.KStream k v) (KS.KStream k v)
pmergeAll others = Pipeline $ \s -> KS.mergeStreamsN (s : others)

-- | Predicate-routed branch. Mirrors the pre-KIP-418
-- @KStream.branch@ shape: returns one substream per
-- predicate, in evaluation order. Records that match no
-- predicate are dropped.
pbranch
  :: [Record k v -> Bool]
  -> Pipeline (KS.KStream k v) [KS.KStream k v]
pbranch preds = Pipeline (KS.branchStream preds)

----------------------------------------------------------------------
-- Sinks
----------------------------------------------------------------------

-- | Publish to a topic with default 'Produced'. Mirrors the
-- 'Kafka.Streams.DSL.sink' / @KStream.to@ shape but lifted to
-- a 'Pipeline' value with @()@ as the output type so it can
-- terminate a chain.
psink
  :: T.Text
  -> Serde k
  -> Serde v
  -> Pipeline (KS.KStream k v) ()
psink t ks vs = Pipeline (KS.toTopic (topicName t) (produced ks vs))

-- | Publish to a topic with a fully-specified 'Produced'.
psinkWith
  :: TopicName
  -> Produced k v
  -> Pipeline (KS.KStream k v) ()
psinkWith t p = Pipeline (KS.toTopic t p)

-- | Through a topic and back into a fresh source. Mirrors
-- @KStream.through@.
pthrough
  :: T.Text
  -> Serde k
  -> Serde v
  -> Pipeline (KS.KStream k v) (KS.KStream k v)
pthrough t ks vs = Pipeline (KS.throughTopic (topicName t) (produced ks vs))

----------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------

-- | Materialise a stream into a 'KTable'.
ptoTable
  :: Ord k
  => Materialized k v
  -> Pipeline (KS.KStream k v) (KTable k v)
ptoTable m = Pipeline (KS.toTable m)

-- | Convert a 'KTable' back into a 'KStream' of changes.
ptoStream :: Pipeline (KTable k v) (KS.KStream k v)
ptoStream = Pipeline KS.toKStreamFromKTable

-- | Force a repartition with the given topic-name prefix.
prepartition
  :: T.Text -> Pipeline (KS.KStream k v) (KS.KStream k v)
prepartition prefix = Pipeline (KS.repartition prefix)

----------------------------------------------------------------------
-- Lifting / lowering
----------------------------------------------------------------------

-- | Lift an arbitrary @a -> IO b@ action into a 'Pipeline'.
-- Useful when you've already written the topology-mutating
-- function in the imperative style and want to splice it
-- into a Category-style composition without re-writing.
liftIOAction :: (a -> IO b) -> Pipeline a b
liftIOAction = Pipeline

-- | Lift a pure function into a 'Pipeline'. Alias for
-- 'Control.Arrow.arr' specialised to 'Pipeline'.
liftPure :: (a -> b) -> Pipeline a b
liftPure = arr
