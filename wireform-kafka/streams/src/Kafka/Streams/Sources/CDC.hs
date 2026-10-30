{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.Sources.CDC
Description : Change-data-capture source primitive (Riffle §5)

Most upstream Kafka Streams topologies consume from a Kafka
topic. Riffle adds a /source primitive/ for change-data-capture
(CDC) feeds, where each record is structured as
@Insert / Update / Delete@ with a before-image and an
after-image of the row. CDC sources are how Debezium, AWS DMS,
and direct database tail-readers publish to Kafka in practice.

This module provides:

  * 'CDCEvent' — the canonical event ADT.
  * 'CDCSource' — a record producing 'CDCEvent's that the
    runtime can poll. The contract is similar in shape to the
    mock consumer but is independent of the Kafka wire protocol
    so non-Kafka transports (binlog tail, WAL stream, etc.) can
    plug in directly.
  * 'cdcToKTableProcessor' — wires a 'CDCSource' into a
    materialised KTable, applying the standard CDC-to-KTable
    mapping (Insert/Update → @put@, Delete → @delete@).
  * 'inMemoryCDCSource' — deterministic in-process test
    source. The chaos suite drives this.
-}
module Kafka.Streams.Sources.CDC (
  -- * Events
  CDCEvent (..),
  CDCOp (..),
  CDCPhase (..),
  SchemaChange (..),
  cdcKey,
  cdcAfter,
  cdcBefore,

  -- * Source contract
  CDCSource (..),
  CDCPoll (..),

  -- * In-memory test source
  inMemoryCDCSource,
  pushCDC,
  pushSchemaChange,
  setPhase,

  -- * KTable wiring
  cdcToKTableStep,
  applyCDCToKVStore,

  -- * Key-aware compaction
  compactCDCBatch,
) where

import Control.Concurrent.STM (
  TQueue,
  TVar,
  atomically,
  flushTQueue,
  newTQueueIO,
  newTVarIO,
  readTVar,
  readTVarIO,
  writeTQueue,
  writeTVar,
 )
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Kafka.Streams.State.Store (
  KeyValueStore (..),
 )
import Kafka.Streams.Time (Timestamp)


----------------------------------------------------------------------
-- Events
----------------------------------------------------------------------

-- | Kind of change captured.
data CDCOp = CDCInsert | CDCUpdate | CDCDelete
  deriving stock (Eq, Show, Generic)


{- | Which phase the upstream CDC connector is currently in.
Debezium and similar tools distinguish between the initial
/snapshot/ (a one-shot dump of every existing row) and the
/streaming/ phase (the WAL / binlog tail that captures live
changes). Phase transitions are surfaced to downstream
operators so they can:

  * Defer windowing during the snapshot (the snapshot is
    usually a flood of records with stale timestamps).
  * Reset 'WatermarkStrategy' idleness on the transition so
    a long snapshot doesn't trigger idle detection on the
    downstream.
  * Switch from \"all rows are inserts\" to
    \"insert/update/delete\" semantics where applicable.
-}
data CDCPhase
  = -- | The connector is dumping existing rows.
    SnapshotPhase
  | {- | The connector has caught up and is now reading the WAL
    tail.
    -}
    StreamingPhase
  deriving stock (Eq, Show, Generic)


{- | A schema-change announcement from the upstream connector.
Surfaced as a /side record/ alongside data records so a
topology that materialises the CDC feed can adapt its
downstream serdes or drop / re-key affected columns.
-}
data SchemaChange = SchemaChange
  { scTable :: !Text
  -- ^ Fully-qualified table name as the connector spells it.
  , scVersion :: !Int
  {- ^ Monotonic version of the schema. Incremented on every
  ALTER TABLE event.
  -}
  , scStmt :: !Text
  {- ^ The DDL statement the connector observed (or a
  best-effort textualisation, depending on the
  connector).
  -}
  , scAt :: !Timestamp
  }
  deriving stock (Eq, Show, Generic)


{- | One CDC event. The @before@ image is present on 'CDCUpdate'
and 'CDCDelete'; the @after@ image is present on 'CDCInsert'
and 'CDCUpdate'.

We don't fold these into separate constructors because every
downstream processor wants the same set of fields and the
'CDCOp' tag is enough to discriminate. The 'Maybe v' fields
match the Debezium / DMS wire schema 1:1.
-}
data CDCEvent k v = CDCEvent
  { cdcOp :: !CDCOp
  , cdcKey' :: !k
  , cdcBefore' :: !(Maybe v)
  , cdcAfter' :: !(Maybe v)
  , cdcSrcOffset :: !Int64
  {- ^ Source-system offset / LSN / SCN. Monotonically
  increasing within one source partition; the runtime uses
  this for commit-offset bookkeeping.
  -}
  , cdcTs :: !Timestamp
  {- ^ Event-time timestamp. Used as the record's
  'recordTimestamp' downstream.
  -}
  }
  deriving stock (Eq, Show, Generic)


-- Field accessors avoid the awkward primes when callers don't
-- want to enable @DuplicateRecordFields@.
cdcKey :: CDCEvent k v -> k
cdcKey = cdcKey'


cdcBefore :: CDCEvent k v -> Maybe v
cdcBefore = cdcBefore'


cdcAfter :: CDCEvent k v -> Maybe v
cdcAfter = cdcAfter'


----------------------------------------------------------------------
-- Source contract
----------------------------------------------------------------------

{- | Result of one poll on a 'CDCSource'. Records and schema
changes are surfaced separately so the downstream topology
can route them onto different operators (data records into
the materialised KTable; schema changes into a side
diagnostic stream).
-}
data CDCPoll k v = CDCPoll
  { cdcPollEvents :: ![CDCEvent k v]
  , cdcPollSchema :: ![SchemaChange]
  , cdcPollPhase :: !CDCPhase
  {- ^ Phase observed at poll time. The runtime can read this
  to gate windowing or idleness handling.
  -}
  }
  deriving stock (Generic)


{- | A 'CDCSource' is a non-Kafka source the runtime can poll.
The shape extends what a consumer
provides with CDC-specific bits: phase awareness, schema
changes, and the standard commit-cursor checkpointing.

The runtime calls 'cdcPoll' on each tick. An empty 'CDCPoll'
(no events, no schema changes) means "no new data"; the
runtime backs off as it would on a Kafka consumer.
'cdcCommitTo' is called after a successful commit cycle; the
source uses this to advance its read cursor on the upstream
system (e.g. update a tail-reader checkpoint).

'cdcClose' is invoked on shutdown; the source should release
any underlying connection / file handle.
-}
data CDCSource k v = CDCSource
  { cdcSourceName :: !String
  , cdcPoll :: !(IO (CDCPoll k v))
  , cdcCommitTo :: !(Int64 -> IO ())
  , cdcClose :: !(IO ())
  }


----------------------------------------------------------------------
-- In-memory reference source
----------------------------------------------------------------------

{- | In-process CDC source handle. The 'TQueue's are exposed so
tests can stage events / schema changes through 'pushCDC' /
'pushSchemaChange'.
-}
data InMemoryCDCSource k v = InMemoryCDCSource
  { imEvents :: !(TQueue (CDCEvent k v))
  , imSchema :: !(TQueue SchemaChange)
  , imPhase :: !(TVar CDCPhase)
  }


{- | Deterministic in-process CDC source. Starts in
'SnapshotPhase'; tests can transition via 'setPhase'.
-}
inMemoryCDCSource :: String -> IO (CDCSource k v, InMemoryCDCSource k v)
inMemoryCDCSource nm = do
  evQ <- newTQueueIO
  scQ <- newTQueueIO
  phaseV <- newTVarIO SnapshotPhase
  let h = InMemoryCDCSource evQ scQ phaseV
  pure
    ( CDCSource
        { cdcSourceName = nm
        , cdcPoll = atomically $ do
            es <- flushTQueue evQ
            ss <- flushTQueue scQ
            phaseNow <- readTVar phaseV
            pure
              CDCPoll
                { cdcPollEvents = es
                , cdcPollSchema = ss
                , cdcPollPhase = phaseNow
                }
        , cdcCommitTo = \_ -> pure ()
        , cdcClose = pure ()
        }
    , h
    )


{- | Push a data event into the source's queue. Tests use this
to drive workloads; production source plug-ins fill the queue
from their transport thread.
-}
pushCDC :: InMemoryCDCSource k v -> CDCEvent k v -> IO ()
pushCDC h e = atomically (writeTQueue (imEvents h) e)


-- | Push a 'SchemaChange' into the source's side channel.
pushSchemaChange :: InMemoryCDCSource k v -> SchemaChange -> IO ()
pushSchemaChange h s = atomically (writeTQueue (imSchema h) s)


{- | Update the source's reported phase. Tests typically call
@setPhase h StreamingPhase@ once the snapshot batch has been
pushed through.
-}
setPhase :: InMemoryCDCSource k v -> CDCPhase -> IO ()
setPhase h p = atomically (writeTVar (imPhase h) p)


----------------------------------------------------------------------
-- KTable wiring
----------------------------------------------------------------------

{- | Apply a single CDC event to a 'KeyValueStore' the way a
canonical CDC-to-KTable bridge would: Insert / Update become
'kvsPut'; Delete becomes 'kvsDelete'. An Insert / Update with a
'Nothing' after-image is treated as a logical delete (matches
the Debezium tombstone convention).
-}
applyCDCToKVStore
  :: KeyValueStore k v
  -> CDCEvent k v
  -> IO ()
applyCDCToKVStore kvs e = case cdcOp e of
  CDCInsert -> case cdcAfter e of
    Just v -> kvsPut kvs (cdcKey e) v
    Nothing -> () <$ kvsDelete kvs (cdcKey e)
  CDCUpdate -> case cdcAfter e of
    Just v -> kvsPut kvs (cdcKey e) v
    Nothing -> () <$ kvsDelete kvs (cdcKey e)
  CDCDelete -> () <$ kvsDelete kvs (cdcKey e)


{- | Drain one batch of events from a 'CDCSource' onto a
'KeyValueStore'. Returns the count of events applied. The
runtime would call this from the topology's source-processor;
this helper makes the loop easy to test in isolation.

Schema-change records in the poll are not applied to the KV
store — they're surfaced via 'cdcToKTableStep''s return value
so the caller can route them onto a side stream.
-}
cdcToKTableStep
  :: forall k v
   . Ord k
  => CDCSource k v
  -> KeyValueStore k v
  -> IO (Int, [SchemaChange], CDCPhase)
cdcToKTableStep src kvs = do
  poll <- cdcPoll src
  let compacted = compactCDCBatch (cdcPollEvents poll)
  mapM_ (applyCDCToKVStore kvs) compacted
  pure (length compacted, cdcPollSchema poll, cdcPollPhase poll)


----------------------------------------------------------------------
-- Key-aware compaction
----------------------------------------------------------------------

{- | Compact a batch of CDC events on the way to a KTable: for
each key, keep only the /last/ event in source order. Drops
intermediate updates that would just be overwritten anyway.
The runtime applies this when 'cdcToKTableStep' loads a
materialised view because:

  * Snapshot phase often produces many records per key in
    quick succession (an initial INSERT then a series of
    UPDATEs).
  * Downstream subscribers only see the final state of each
    key; intermediate updates are wasted writes.

The compactor is order-preserving: keys retain the relative
order of their last event; within a key, all earlier events
are dropped. A trailing 'CDCDelete' is preserved so the
KTable correctly observes deletes.
-}
compactCDCBatch
  :: forall k v
   . Ord k
  => [CDCEvent k v] -> [CDCEvent k v]
compactCDCBatch events =
  let pairsByKey =
        foldl
          (\m e -> Map.insert (cdcKey e) e m)
          (Map.empty :: Map k (CDCEvent k v))
          events
      firstAppearance = go events (Map.empty :: Map k ()) []
  in [pairsByKey Map.! k | k <- firstAppearance]
  where
    go [] _ acc = reverse acc
    go (e : rest) seen acc
      | Map.member (cdcKey e) seen = go rest seen acc
      | otherwise =
          go
            rest
            (Map.insert (cdcKey e) () seen)
            (cdcKey e : acc)
