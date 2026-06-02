{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Kafka.Streams.DSL
-- Description : Haskell-native, builder-implicit façade over the Streams DSL
--
-- This module is a re-skinned façade on top of the existing
-- @Kafka.Streams.KStream@ / @Kafka.Streams.KTable@ API. The
-- underlying primitives are unchanged; what's new here is:
--
-- 1. A 'Streams' /reader/ monad over the 'StreamsBuilder' so users
--    don't thread it through every source / sink call.
--
-- 2. Shorter operator names that collide with @Prelude@ (e.g.
--    'Kafka.Streams.DSL.map', 'filter', 'foldr'-shaped fragments).
--    Import this module qualified:
--
--    @
--    import qualified Kafka.Streams.DSL as S
--
--    topology :: IO Topology
--    topology = S.build $ do
--      src <- S.source \"in\"  textSerde textSerde
--      out <- src S.|> S.map T.toUpper
--                 S.|> S.filter (\\r -> recordValue r /= \"\")
--      S.sink \"out\" textSerde textSerde out
--    @
--
-- 3. A pipe operator '|>' for left-to-right chains. The left
--    side is either a pure value @a@ or an action @Streams a@
--    (dispatched via the 'Pipe' class), so the same operator
--    works on the head and tail of a chain. The right side is
--    any 'PipeInto' target — a @b -> Streams c@ continuation or
--    a first-class 'Kafka.Streams.Pipeline.Pipeline' fragment.
--
-- 4. An alternative 'into' / 'with' vocabulary for sinks
--    that reads naturally at call sites: @src `into` \"out\"@.
--
-- The original imperative API (@streamFromTopic@, @filterStream@,
-- @mapValues@…) is still available unchanged from
-- "Kafka.Streams"; pick whichever style fits the topology.
module Kafka.Streams.DSL
  (     -- * Topology builder
    Streams (..)
  , runStreams
  , runStreamsWith
  , build
  , builder
  , liftIO

    -- * Re-exports for convenience
  , KStream
  , KTable
  , GlobalKTable
  , Record (..)
  , TopicName
  , topicName
  , Serde
  , Consumed
  , Produced
  , Materialized
  , Joined
  , Grouped
  , KGroupedStream
  , KGS.CountedTableLocal
  , Timestamp (..)

    -- * Sources
  , source
  , sourceWith
  , table
  , tableWith
  , globalTable

    -- * Sinks
  , sink
  , sinkWith
  , sinkTo
  , into
  , through

    -- * Stateless transforms
  , map
  , mapWithKey
  , mapM
  , mapWithKeyM
  , filter
  , filterNot
  , concatMap
  , concatMapWithKey
  , peek
  , foreach
  , selectKey
  , values

    -- * Combine
  , merge
  , mergeAll
  , branch

    -- * Conversions
  , toTable
  , toStream
  , repartition

    -- * Aggregations
  , groupByKey
  , groupBy
  , count
  , reduce
  , aggregate

    -- * Joins
  , join
  , leftJoin
  , outerJoin
  , joinTable

    -- * Pipe operators
  , (|>)
  , (<|)
  , Pipe (..)
  , PipeInto (..)
  , Pipeline
  ) where

import Prelude hiding (concatMap, filter, map, mapM)

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader (..))
import Data.Int (Int64)
import Data.Text (Text)

import Kafka.Streams.Consumed (Consumed, consumed)
import Kafka.Streams.Grouped (Grouped (..))
import Kafka.Streams.GlobalKTable (GlobalKTable)
import qualified Kafka.Streams.GlobalKTable as GT
import Kafka.Streams.Joined (Joined)
import qualified Kafka.Streams.Joined as JW
import qualified Kafka.Streams.KGroupedStream as KGS
import Kafka.Streams.KGroupedStream (KGroupedStream)
import qualified Kafka.Streams.KStream as KS
import Kafka.Streams.KStream (KStream)
import qualified Kafka.Streams.KTable as KT
import Kafka.Streams.KTable (KTable)
import Kafka.Streams.Materialized (Materialized)
import qualified Kafka.Streams.Materialized as Mat
import Kafka.Streams.Pipeline (Pipeline, runPipeline)
import Kafka.Streams.Produced (Produced, produced)
import Kafka.Streams.Serde (HasSerde, Serde)
import Kafka.Streams.StreamsBuilder
  ( StreamsBuilder
  , buildTopology
  , newStreamsBuilder
  )
import Kafka.Streams.Time (Timestamp (..))
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Types (Record (..), TopicName, topicName)

----------------------------------------------------------------------
-- Streams monad
----------------------------------------------------------------------

-- | A topology under construction. Carries the 'StreamsBuilder'
-- implicitly so the user doesn't pass it to every source / sink
-- call. Operations register a new node in the underlying
-- mutable builder; the 'Streams' value records the work to do.
--
-- Structurally a @ReaderT StreamsBuilder IO@. Rather than wrap
-- the transformer we keep the explicit newtype (so error
-- messages and Haddock say @Streams@) and provide the standard
-- @mtl@ instances by hand: 'MonadIO' for 'liftIO' and
-- 'MonadReader' 'StreamsBuilder' for 'ask' \/ 'local' \/
-- 'reader'. That means @Streams@ inter-operates with any
-- @mtl@-polymorphic helper a caller already has.
newtype Streams a = Streams { unStreams :: StreamsBuilder -> IO a }

instance Functor Streams where
  fmap f (Streams g) = Streams $ \b -> fmap f (g b)
  {-# INLINE fmap #-}

instance Applicative Streams where
  pure x = Streams (\_ -> pure x)
  {-# INLINE pure #-}
  Streams f <*> Streams x = Streams $ \b -> f b <*> x b
  {-# INLINE (<*>) #-}

instance Monad Streams where
  Streams m >>= k = Streams $ \b -> do
    !a <- m b
    unStreams (k a) b
  {-# INLINE (>>=) #-}

-- | Lift an arbitrary 'IO' action into 'Streams'. This is the
-- real @mtl@ 'Control.Monad.IO.Class.liftIO' (re-exported for
-- convenience), so a value built with @mtl@-polymorphic
-- 'MonadIO' code drops straight into a topology builder.
instance MonadIO Streams where
  liftIO m = Streams (\_ -> m)
  {-# INLINE liftIO #-}

-- | The reader environment is the implicit 'StreamsBuilder'.
-- 'ask' hands it back, 'local' runs a sub-action against a
-- transformed builder, and 'reader' projects a value out of it.
instance MonadReader StreamsBuilder Streams where
  ask = Streams pure
  {-# INLINE ask #-}
  local f (Streams g) = Streams (g . f)
  {-# INLINE local #-}
  reader f = Streams (pure . f)
  {-# INLINE reader #-}

-- | Run a topology-builder action and return the assembled
-- 'Topo.Topology'. Equivalent to @newStreamsBuilder@ +
-- @buildTopology@ with the body in between.
build :: Streams a -> IO Topo.Topology
build m = fmap snd (runStreams m)

-- | Run a topology-builder action, returning both the result
-- of the action /and/ the assembled 'Topo.Topology'.
runStreams :: Streams a -> IO (a, Topo.Topology)
runStreams m = do
  b <- newStreamsBuilder
  a <- unStreams m b
  topo <- buildTopology b
  pure (a, topo)

-- | Variant that runs against a pre-existing 'StreamsBuilder'.
-- Use when interleaving DSL-style fragments with imperative
-- builder operations from the original API.
runStreamsWith :: StreamsBuilder -> Streams a -> IO a
runStreamsWith b m = unStreams m b

-- | Yield the current 'StreamsBuilder' inside a 'Streams' action.
-- Useful when dropping down to the original API for an operation
-- not yet covered here.
builder :: Streams StreamsBuilder
builder = Streams pure

-- | Internal helper.
withBuilder :: (StreamsBuilder -> IO a) -> Streams a
withBuilder = Streams

----------------------------------------------------------------------
-- Sources
----------------------------------------------------------------------

-- | Subscribe to a topic with the default 'Consumed' (record
-- timestamp, earliest offset reset, no explicit node name).
--
-- /JVM equivalent:/ @StreamsBuilder.stream(topic, Consumed.with(...))@.
source
  :: Text                                 -- ^ topic name
  -> Serde k                              -- ^ key serde
  -> Serde v                              -- ^ value serde
  -> Streams (KStream k v)
source t ks vs = sourceWith (topicName t) (consumed ks vs)

-- | Subscribe to a topic with a fully-specified 'Consumed'.
sourceWith :: TopicName -> Consumed k v -> Streams (KStream k v)
sourceWith t c = withBuilder $ \b -> KS.streamFromTopic b t c

-- | Materialise a topic as a 'KTable' (default in-memory store).
--
-- /JVM equivalent:/ @StreamsBuilder.table(topic, Consumed, Materialized)@.
table
  :: Ord k
  => Text -> Serde k -> Serde v -> Streams (KTable k v)
table t ks vs =
  tableWith (topicName t) (consumed ks vs)
    (Mat.withValueSerde vs (Mat.withKeySerde ks Mat.materialized))

-- | Materialise a topic as a 'KTable' with a caller-supplied
-- 'Consumed' and 'Materialized'.
tableWith
  :: Ord k
  => TopicName
  -> Consumed k v
  -> Materialized k v
  -> Streams (KTable k v)
tableWith t c m = withBuilder $ \b -> KT.tableFromTopic b t c m

-- | Cluster-replicated 'GlobalKTable'.
--
-- /JVM equivalent:/ @StreamsBuilder.globalTable(topic, ...)@.
globalTable
  :: Ord k
  => Text -> Serde k -> Serde v -> Streams (GlobalKTable k v)
globalTable t ks vs = withBuilder $ \b ->
  GT.globalTable b (topicName t) (consumed ks vs)
    (Mat.withValueSerde vs (Mat.withKeySerde ks Mat.materialized))

----------------------------------------------------------------------
-- Sinks
----------------------------------------------------------------------

-- | Publish records to a topic with default 'Produced'.
--
-- /JVM equivalent:/ @KStream.to(topic, Produced.with(ks, vs))@.
sink
  :: Text                                 -- ^ topic name
  -> Serde k                              -- ^ key serde
  -> Serde v                              -- ^ value serde
  -> KStream k v
  -> Streams ()
sink t ks vs s = sinkWith (topicName t) (produced ks vs) s

-- | Publish records to a topic with a fully-specified 'Produced'.
sinkWith :: TopicName -> Produced k v -> KStream k v -> Streams ()
sinkWith t p s = liftIO (KS.toTopic t p s)

-- | Flipped 'sink' — reads as @src `into` "out"@.
--
-- @
-- src `into` "out" textSerde textSerde
-- @
into
  :: KStream k v
  -> Text -> Serde k -> Serde v
  -> Streams ()
into s t ks vs = sink t ks vs s

-- | Per-record dynamic-topic sink. Mirrors
-- @KStream.to(TopicNameExtractor, Produced)@.
sinkTo
  :: KS.TopicNameExtractor k v
  -> Produced k v
  -> KStream k v
  -> Streams ()
sinkTo ext p s = liftIO (KS.toExtracted ext p s)

-- | Sink to a topic and re-subscribe from it.
-- Mirrors @KStream.through@.
through
  :: Text -> Serde k -> Serde v
  -> KStream k v
  -> Streams (KStream k v)
through t ks vs s = liftIO (KS.throughTopic (topicName t) (produced ks vs) s)

----------------------------------------------------------------------
-- Stateless transforms
----------------------------------------------------------------------

-- | Pure value-only map. Equivalent to @KStream.mapValues@.
map :: HasSerde v' => (v -> v') -> KStream k v -> Streams (KStream k v')
map f s = liftIO (KS.mapValues f s)

-- | Pure (key, value) map. Equivalent to @KStream.map@.
mapWithKey
  :: (HasSerde k', HasSerde v')
  => (k -> v -> (k', v')) -> KStream k v -> Streams (KStream k' v')
mapWithKey f s = liftIO (KS.mapKeyValue f s)

-- | Effectful value-only map. Equivalent to @KStream.mapValues@
-- with an embedded @IO@.
mapM :: HasSerde v' => (v -> IO v') -> KStream k v -> Streams (KStream k v')
mapM f s = liftIO (KS.mapValuesM f s)

-- | Effectful (key, value) map. Equivalent to @KStream.map@ with
-- an embedded @IO@.
mapWithKeyM
  :: (HasSerde k', HasSerde v')
  => (k -> v -> IO (k', v')) -> KStream k v -> Streams (KStream k' v')
mapWithKeyM f s = liftIO (KS.mapKeyValueM f s)

-- | Keep records satisfying the predicate. Mirrors @KStream.filter@.
filter
  :: (Record k v -> Bool) -> KStream k v -> Streams (KStream k v)
filter p s = liftIO (KS.filterStream p s)

-- | Drop records satisfying the predicate. Mirrors @KStream.filterNot@.
filterNot
  :: (Record k v -> Bool) -> KStream k v -> Streams (KStream k v)
filterNot p s = liftIO (KS.filterNotStream p s)

-- | One-to-many value-only expansion. Mirrors
-- 'Kafka.Streams.KStream.concatMapValues' (the JVM
-- @KStream.flatMapValues@). The name follows Haskell's
-- 'Data.List.concatMap' convention rather than Scala's
-- @flatMap@.
concatMap
  :: HasSerde v'
  => (v -> [v']) -> KStream k v -> Streams (KStream k v')
concatMap f s = liftIO (KS.concatMapValues f s)

-- | One-to-many @(key, value)@ expansion. Mirrors
-- 'Kafka.Streams.KStream.concatMapKeyValue' (the JVM
-- @KStream.flatMap@).
concatMapWithKey
  :: (HasSerde k', HasSerde v')
  => (k -> v -> [(k', v')]) -> KStream k v -> Streams (KStream k' v')
concatMapWithKey f s = liftIO (KS.concatMapKeyValue f s)

-- | Side-effecting observer (record is unchanged). Mirrors @KStream.peek@.
peek
  :: (Record k v -> IO ()) -> KStream k v -> Streams (KStream k v)
peek f s = liftIO (KS.peekStream f s)

-- | Terminal side-effect; the stream is dropped. Mirrors @KStream.foreach@.
foreach :: (Record k v -> IO ()) -> KStream k v -> Streams ()
foreach f s = liftIO (KS.foreachStream f s)

-- | Re-key the stream from the full record. Mirrors @KStream.selectKey@.
selectKey
  :: HasSerde k'
  => (Record k v -> k') -> KStream k v -> Streams (KStream k' v)
selectKey f s = liftIO (KS.selectKey f s)

-- | Drop the key. Mirrors @KStream.values@.
values :: KStream k v -> Streams (KStream () v)
values s = liftIO (KS.valuesStream s)

----------------------------------------------------------------------
-- Composition
----------------------------------------------------------------------

-- | Merge two streams of the same key/value type. Mirrors
-- @KStream.merge@.
merge :: KStream k v -> KStream k v -> Streams (KStream k v)
merge a b = liftIO (KS.mergeStreams a b)

-- | Merge a non-empty list of streams. Mirrors fold of @merge@.
mergeAll :: [KStream k v] -> Streams (KStream k v)
mergeAll = liftIO . KS.mergeStreamsN

-- | Predicate-routed split. Mirrors @KStream.branch@ (the pre-KIP-418
-- shape that just returns @[KStream]@; for the named-branches DSL use
-- 'KS.splitStream' from the original API).
branch
  :: [Record k v -> Bool]
  -> KStream k v
  -> Streams [KStream k v]
branch ps s = liftIO (KS.branchStream ps s)

----------------------------------------------------------------------
-- KStream ↔ KTable
----------------------------------------------------------------------

-- | Convert a 'KStream' into a 'KTable' by materialising the
-- latest value per key. Mirrors @KStream.toTable@.
toTable
  :: Ord k
  => Materialized k v -> KStream k v -> Streams (KTable k v)
toTable m s = liftIO (KS.toTable m s)

-- | Convert a 'KTable' to a 'KStream' carrying every changelog
-- event. Mirrors @KTable.toStream@.
toStream :: KTable k v -> Streams (KStream k v)
toStream kt = liftIO (KS.toKStreamFromKTable kt)

-- | Force a repartition with the given topic-name prefix.
-- Mirrors @KStream.repartition@.
repartition :: Text -> KStream k v -> Streams (KStream k v)
repartition prefix s = liftIO (KS.repartition prefix s)

----------------------------------------------------------------------
-- Aggregations
----------------------------------------------------------------------

-- | Group by the existing key. Mirrors @KStream.groupByKey@.
--
-- @KGroupedStream@ is a pure-handle type, so unlike most DSL
-- combinators 'groupByKey' doesn't need to register a new
-- topology node and is therefore non-effectful in 'Streams'.
groupByKey
  :: Grouped k v -> KStream k v -> Streams (KGroupedStream k v)
groupByKey g s = pure (KGS.groupByKey g s)

-- | Group by a derived key. Mirrors @KStream.groupBy@.
groupBy
  :: (Record k v -> k')
  -> Grouped k' v
  -> KStream k v
  -> Streams (KGroupedStream k' v)
groupBy f g s = liftIO (KGS.groupByStream f g s)

-- | Cardinality per key. Mirrors @KGroupedStream.count@.
count
  :: Ord k
  => Materialized k Int64
  -> KGroupedStream k v
  -> Streams (KGS.CountedTableLocal k Int64)
count m g = liftIO (KGS.countStream m g)

-- | Same-shape reducer. Mirrors @KGroupedStream.reduce@.
reduce
  :: Ord k
  => (v -> v -> v)
  -> Materialized k v
  -> KGroupedStream k v
  -> Streams (KGS.CountedTableLocal k v)
reduce f m g = liftIO (KGS.reduceStream f m g)

-- | General-shape aggregator. Mirrors @KGroupedStream.aggregate@.
-- The initialiser is an 'IO' to mirror Java's @Initializer<A>@
-- (a fresh accumulator per key); supply @pure x@ for pure seeds.
aggregate
  :: Ord k
  => IO agg
  -> (k -> v -> agg -> agg)
  -> Materialized k agg
  -> KGroupedStream k v
  -> Streams (KGS.CountedTableLocal k agg)
aggregate z step m g = liftIO (KGS.aggregateStream z step m g)

----------------------------------------------------------------------
-- Joins
----------------------------------------------------------------------

-- | Stream-table inner join. Mirrors
-- @KStream.join(KTable, ValueJoiner, Joined)@.
join
  :: (Ord k, HasSerde v')
  => (v -> vt -> v')
  -> Joined k v vt
  -> KStream k v
  -> KTable k vt
  -> Streams (KStream k v')
join j jo s t = liftIO (KS.joinKStreamKTable j jo s t)

-- | Stream-table left join.
leftJoin
  :: (Ord k, HasSerde v')
  => (v -> Maybe vt -> v')
  -> Joined k v vt
  -> KStream k v
  -> KTable k vt
  -> Streams (KStream k v')
leftJoin j jo s t = liftIO (KS.leftJoinKStreamKTable j jo s t)

-- | KTable-KTable inner join.
joinTable
  :: Ord k
  => (v1 -> v2 -> v')
  -> Materialized k v'
  -> KTable k v1
  -> KTable k v2
  -> Streams (KTable k v')
joinTable j m a b = liftIO (KT.joinKTableKTable j m a b)

----------------------------------------------------------------------
-- Imports used only by signatures above
----------------------------------------------------------------------

-- | Stream-stream window outer-join. Mirrors
-- @KStream.outerJoin(other, ValueJoiner, JoinWindows, StreamJoined)@.
outerJoin
  :: Ord k
  => (Maybe v1 -> Maybe v2 -> v')
  -> JW.JoinWindows
  -> Joined k v1 v2
  -> KStream k v1
  -> KStream k v2
  -> Streams (KStream k v')
outerJoin j w jo a b = liftIO (KS.outerJoinKStreamKStream j w jo a b)

----------------------------------------------------------------------
-- Pipe operator
----------------------------------------------------------------------

-- | The /source/ side of a pipe: lift either a pure value @a@
-- or a 'Streams' action producing @a@ into the 'Streams' monad.
-- Used so the same '|>' operator works on the head and the tail
-- of a chain.
--
-- The catch-all instance is 'pure' (a pure value); the
-- 'Streams' instance is the identity. The functional dependency
-- @a -> b@ recovers the carried type so the rest of the chain
-- type-checks.
class Pipe a b | a -> b where
  toStreams :: a -> Streams b

instance Pipe (Streams a) a where
  toStreams = id

instance {-# OVERLAPPABLE #-} (a ~ b) => Pipe a b where
  toStreams = pure

-- | The /target/ side of a pipe: anything that can consume a
-- @b@ and produce a @'Streams' c@. The functional dependency
-- @t -> b c@ pins both ends down from the step itself, so the
-- two instances never overlap (they have distinct outermost
-- type constructors, @(->)@ vs 'Pipeline') and no
-- @OVERLAPPING@ \/ @OVERLAPPABLE@ pragmas are needed.
--
-- Instances:
--
--   * @b -> 'Streams' c@ — a plain Kleisli continuation, the
--     classic right-hand side (e.g. @'map' f@, @'filter' p@,
--     @'sink' …@).
--   * @'Pipeline' b c@ — a first-class, reusable topology
--     fragment (see "Kafka.Streams.Pipeline"). It runs in 'IO',
--     so it's lifted with 'liftIO'. This lets pre-built
--     'Pipeline' values be spliced straight into a @|>@ chain:
--
--     @
--     normalise :: 'Pipeline' ('KStream' Text Text) ('KStream' Text Text)
--     src |> normalise |> 'sink' \"out\" textSerde textSerde
--     @
class PipeInto t b c | t -> b c where
  pipeInto :: t -> b -> Streams c

instance PipeInto (b -> Streams c) b c where
  pipeInto = id

instance PipeInto (Pipeline b c) b c where
  pipeInto p = liftIO . runPipeline p

-- | Left-to-right pipe. The left-hand side is either a pure
-- value or another 'Streams' action (dispatched via 'Pipe'); the
-- right-hand side is any 'PipeInto' target — a @b -> 'Streams' c@
-- continuation or a first-class 'Pipeline'.
--
-- @
-- src |> map T.toUpper |> filter (\\r -> recordValue r /= \"\")
-- src |> myReusablePipeline |> sink \"out\" ks vs
-- @
(|>) :: (Pipe a b, PipeInto t b c) => a -> t -> Streams c
a |> t = toStreams a >>= pipeInto t
infixl 1 |>

-- | Right-to-left form of '|>'.
(<|) :: (Pipe a b, PipeInto t b c) => t -> a -> Streams c
t <| a = a |> t
infixr 1 <|
