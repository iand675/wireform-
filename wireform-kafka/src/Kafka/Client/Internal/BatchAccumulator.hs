{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Kafka.Client.Internal.BatchAccumulator
Description : Producer batch accumulation and management
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module implements the batch accumulator for the Kafka producer.

The batch accumulator:
- Maintains one queue per topic-partition
- Accumulates records into batches
- Makes batches "ready" based on size or linger time
- Preserves ordering within each partition
- Allows concurrent access from multiple threads

Design follows patterns from Java Kafka client and librdkafka:
- Per-partition Deques hold batches
- Current batch per partition is being filled
- Batches become ready when full or linger time expires
- Sender thread(s) drain ready batches
-}
module Kafka.Client.Internal.BatchAccumulator (
  -- * Batch Accumulator
  BatchAccumulator,
  createBatchAccumulator,
  closeBatchAccumulator,

  -- * Adding Records
  appendRecord,
  appendRecordWithCallback,
  appendRecordStamped,
  appendRecordStampedUnsafe,
  appendRecordStampedAt,
  appendRecordsStamped,

  -- * Direct-mode hot path (skip partition lookups)
  BatchAccumulating,
  PartitionQueue,
  baHotState,
  baTopicPartition,
  currentBatchOf,
  appendDirect,
  BatchStamp (..),
  noStamp,
  TopicPartition (..),

  -- * Draining Batches
  drainReadyBatches,
  hasReadyBatches,
  purgePendingBatches,

  -- * Batch Types
  ProducerBatch (..),
  BatchState (..),
  RecordCallback (..),
  runRecordCallback,
  BatchAck (..),
  flushPendingBatches,
) where

import Control.Concurrent.STM
import Control.Monad (forM, forM_, when)
import Data.Atomics (
  Ticket,
  casIORef,
  peekTicket,
  readForCAS,
 )
import Data.ByteString qualified as BS
import Data.Foldable (foldl', toList)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.IORef (
  IORef,
  atomicModifyIORef',
  modifyIORef',
  newIORef,
  readIORef,
  writeIORef,
 )
import Data.Int
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import GHC.Generics (Generic)
import Kafka.Compression.Types qualified as Compression
import Kafka.Protocol.RecordBatch qualified as RB
import Kafka.Time qualified as KafkaTime


-- | Topic-partition identifier
data TopicPartition = TopicPartition
  { tpTopic :: !Text
  , tpPartition :: !Int32
  }
  deriving (Eq, Show, Ord, Generic)


instance Hashable TopicPartition


-- | State of a batch
data BatchState
  = -- | Currently being filled with records
    Filling
  | -- | Ready to be sent
    Ready
  | -- | Currently being sent
    Sending
  | -- | Successfully sent
    Complete
  | -- | Send failed with error
    Failed !Text
  deriving (Eq, Show, Generic)


{- | Per-record acknowledgement metadata handed to the sender's
ack-callback path. Replaces the previous
@(Text, Int32, Int64, Int64)@ 4-tuple so that:

  * the three machine-word fields ('ackPartition' / 'ackOffset'
    / 'ackTimestamp') are 'UNPACK'-pragma'd into the
    constructor, eliminating one boxed-'Int32' and two
    boxed-'Int64' allocations per acked record;
  * the strict-product representation lets the
    constructed-product-result optimiser elide the 'Right'
    wrapper when the immediate consumer is a 'case' on the
    'Either' (the producer's adapter callback).

Heap-profile note: the previous tuple shape showed up in
@ghc-prim:GHC.Tuple.Prim.(,,,)@ allocations on the sender ack
path; this 4-field strict record is allocated /and/ consumed
inside the same case branch in 'processProduceResponse' so
GHC can fuse the construction away on the no-op-callback path
the bench harness uses.
-}
data BatchAck = BatchAck
  { ackTopic :: !Text
  , ackPartition :: {-# UNPACK #-} !Int32
  , ackOffset :: {-# UNPACK #-} !Int64
  , ackTimestamp :: {-# UNPACK #-} !Int64
  }
  deriving (Eq, Show, Generic)


{- | Callback for record completion. Either no-callback
(sender skips per-record metadata construction entirely) or a
real callback that receives 'Left' with an error message or
'Right' with the per-record 'BatchAck'.

Why a sum and not a function-only type:
the perf-critical code paths ('sendMessageUnsafe_',
'sendMessageFastest_', 'sendMessages_') pass the no-op
callback. With a function-only type the sender would still
have to /allocate/ and /strictly construct/ the per-record
'BatchAck' (UNPACK'd strict fields → all evaluated at
construction) and then immediately throw it away. Tagging the
"no callback" case lets the sender's dispatch skip the
construction with one branch instead — preserves the strict
'BatchAck' shape for callers that /do/ use the metadata
without burning cycles for callers that don't.
-}
data RecordCallback
  = NoRecordCallback
  | RecordCallback !(Either Text BatchAck -> IO ())


{- | Invoke a 'RecordCallback'. The 'NoRecordCallback' branch is
a one-instruction tag check (no allocation, no closure
dispatch); the 'RecordCallback' branch is a single function
application. Used by the sender's per-record dispatch on both
the success and error paths so it can skip the per-record
'BatchAck' / error-message construction entirely when no
caller cares.
-}
runRecordCallback :: RecordCallback -> Either Text BatchAck -> IO ()
runRecordCallback NoRecordCallback _ = pure ()
runRecordCallback (RecordCallback f) e = f e
{-# INLINE runRecordCallback #-}


-- | A batch of records for a single partition
data ProducerBatch = ProducerBatch
  { batchTopicPartition :: !TopicPartition
  -- ^ Which topic-partition this batch is for
  , batchRecords :: !(V.Vector RB.Record)
  {- ^ Records in this batch.

  Stored as a frozen 'V.Vector' rather than a 'Seq' so the
  sender's 'buildRecordBatch' can hand it straight to the
  wire encoder (which already wants a 'V.Vector RB.Record')
  with no shape conversion. Pre-Vector this was @Seq Record@
  and the sender did @V.fromList \. toList@ per batch — that
  pass was visible in the heap profile as
  @containers:Data.Sequence.Internal.Deep@ + @.Three@
  residency dominating per-batch allocation.
  -}
  , batchSizeBytes :: !Int
  -- ^ Current size in bytes (approximate)
  , batchCreateTime :: !Int64
  -- ^ Timestamp when batch was created (milliseconds)
  , batchBaseTimestamp :: !Int64
  -- ^ Base timestamp for the batch
  , batchState :: !BatchState
  -- ^ Current state of this batch
  , batchCompression :: !Compression.CompressionCodec
  -- ^ Compression codec to use
  , batchCompressionLevel :: !Compression.CompressionLevel
  -- ^ Compression level (KIP-353/776/909)
  , batchCallbacks :: !(V.Vector RecordCallback)
  {- ^ Completion callbacks for each record, in order. Same
  'V.Vector' shape as 'batchRecords' for the same reason
  (the sender's ack dispatch walks both with the same
  index).
  -}
  , batchAttempts :: !Int
  {- ^ Number of retry attempts already taken on this batch.
  Bumped each time the sender re-enqueues the batch after a
  retriable produce error. The 'shouldRetry' predicate
  compares this against 'retryMaxAttempts' from the sender's
  'RetryConfig'; 'retryBatch' passes it to 'nextRetryBackoffMs'
  so successive attempts back off exponentially. 0 means the
  batch hasn't been retried yet.
  -}
  , batchProducerId :: !Int64
  {- ^ Idempotent / transactional producer id stamped onto the
  serialised RecordBatch. 'noProducerId' (= -1) for
  non-idempotent producers; populated from
  'Kafka.Client.Internal.ProducerSender.SenderState' when
  the producer config has @producerIdempotent = True@ or
  @producerTransactional = Just _@.
  -}
  , batchProducerEpoch :: !Int16
  {- ^ Producer epoch from @InitProducerId@. Pairs with
  'batchProducerId'; together they fence stale producers.
  -}
  , batchBaseSequence :: !Int32
  {- ^ Sequence number assigned to the first record in this
  batch. Each successive batch on the same (topic, partition)
  gets the previous batch's @batchBaseSequence + count@.
  'noSequence' (= -1) for non-idempotent producers.
  -}
  , batchIsTransactional :: !Bool
  {- ^ Whether this batch is part of an open transaction. When
  'True', 'Kafka.Client.Internal.ProducerSender.buildRecordBatch'
  sets 'attrIsTransactional' on the wire-level
  'RecordBatch.Attributes' so the broker treats the batch as a
  transactional write (consumed by read-committed consumers
  only after 'EndTxn(commit)'). Always 'False' for
  non-transactional / idempotent-only producers.
  -}
  }


{- | Per-partition batch queue.

Per-partition usage is single-producer (the user thread that
calls 'sendMessage' for this partition; partition assignment is
deterministic per record so concurrent producer threads land in
different 'PartitionQueue' instances) and single-consumer (the
sender thread that calls 'drainReadyBatches'). The two refs were
'TVar' pre-Tier-2 of the STM-replacement work; the 'atomically'
block on every 'appendRecordStamped' was paying STM commit
overhead (~150–200 ns/record at the bench) for an SPSC swap.
'IORef' + 'atomicModifyIORef\'' provides the same single-ref
visibility guarantees, with the multi-thread-on-one-partition
safety 'atomicModifyIORef\'' still gives us. See
@docs/STM_REPLACEMENT_SPEC.md@ Tier 2 for the full analysis.
-}
data PartitionQueue = PartitionQueue
  { queueBatches :: !(IORef (Seq ProducerBatch))
  {- ^ Queue of /sealed/ batches for this partition (oldest
  first). Producer thread appends ready batches via
  'atomicModifyIORef\''; sender thread drains via
  'atomicModifyIORef\'' with @Seq.spanl isReady@.
  -}
  , queueCurrentBatch :: !(IORef (Maybe BatchAccumulating))
  {- ^ Batch currently being filled. The accumulating batch
  has its hot mutable fields (records, size, callbacks)
  under a single inner 'IORef' so the per-record append
  path mutates them in place rather than rebuilding the
  whole batch struct on every call. When sealing we
  snapshot the inner state into an immutable
  'ProducerBatch' (so the rest of the producer keeps the
  record-style interface) and push it onto
  'queueBatches'.
  -}
  }


{- | In-progress batch held in 'queueCurrentBatch' while records
are being appended.

Splitting the batch into "fixed metadata" (this struct) and
"hot mutable state" (the inner 'baHotState' ref) lets the
per-record 'appendRecordStamped' / 'appendRecordStampedUnsafe'
hot path do one 'atomicModifyIORef\'' on the small
'BatchHotState' (3 fields = 24 B allocation per call) instead
of rebuilding the 12-field 'ProducerBatch' struct on every
record (~96 B + tuple allocation).

The fixed fields mirror the constant parts of 'ProducerBatch';
when the batch is sealed (size limit hit / linger expired /
flush) we snapshot the hot state and the fixed fields into a
normal 'ProducerBatch' so the writer thread + every existing
consumer of the type sees the same immutable shape they used
to.
-}
data BatchAccumulating = BatchAccumulating
  { baTopicPartition :: !TopicPartition
  , baCreateTime :: !Int64
  , baBaseTimestamp :: !Int64
  , baCompression :: !Compression.CompressionCodec
  , baCompressionLevel :: !Compression.CompressionLevel
  , baProducerId :: !Int64
  , baProducerEpoch :: !Int16
  , baBaseSequence :: !Int32
  , baIsTransactional :: !Bool
  , baHotState :: !(IORef BatchHotState)
  }


{- | Hot mutable state of an in-progress batch. Held under a
single 'IORef' so the per-record append is one CAS that
updates the entire state atomically.

Records and callbacks are accumulated in /reversed/ singly-
linked lists so the per-record append is one ':' cons (one
cell allocation per record per list) rather than one 'Seq'
snoc. The 'snapshotBatch' helper reverses and converts to the
@Seq@ shape the rest of the producer expects; the O(n)
reverse runs once per sealed batch and is negligible compared
to the per-record win.

/Seal interlock/: the seal bit is packed into the sign of
'bhSizeRaw' — non-negative is "open, accumulated size = N",
'sealedSentinel' (a fixed negative value) is "sealed". Packing
the bit instead of carrying a separate @!Bool@ field keeps
'BatchHotState' at three pointer-sized slots, so the per-CAS
struct rebuild on the append hot path stays at the same
allocation footprint as the pre-fix shape.

The append CAS reads @bhSizeRaw@ as part of every
'atomicModifyIORef\'' on this 'IORef' and bails to the slow
path if it's negative; the sealer flips it inside its own
'atomicModifyIORef\'' that simultaneously captures the final
state for the snapshot. Because both sides contend on the
/same/ 'IORef', the CAS protocol is total: any append whose
CAS lands /before/ the seal CAS is incorporated into the
snapshot; any append whose CAS lands /after/ the seal CAS
sees the negative sentinel and routes itself to a fresh
batch via 'slowAppendIO'. There is no window in which a
successful append can be silently dropped.
-}
data BatchHotState = BatchHotState
  { bhRecordsRev :: ![RB.Record]
  , bhSizeRaw :: !Int
  , bhCallbacksRev :: ![RecordCallback]
  }


{- | Sentinel value of 'bhSizeRaw' meaning "this batch has been
sealed; no more appends will be accepted." Negative so it can
never collide with a real accumulated size (which is always
>= 0). 'minBound :: Int' is used so any append that races and
adds its record size to it stays negative — defensive belt &
braces in case a CAS reads the sentinel and tries to add to
it before the @< 0@ guard short-circuits.
-}
sealedSentinel :: Int
sealedSentinel = minBound
{-# INLINE sealedSentinel #-}


{- | The accumulated size in bytes of the records observed by
this hot state. Returns 0 for the sealed sentinel — callers
that need the real size on a sealed batch should keep the
pre-seal value separately (see 'snapshotBatch', which captures
the size /before/ flipping to the sentinel).
-}
sizeOf :: BatchHotState -> Int
sizeOf st = let !n = bhSizeRaw st in if n < 0 then 0 else n
{-# INLINE sizeOf #-}


-- | Batch accumulator configuration
data BatchAccumulatorConfig = BatchAccumulatorConfig
  { accumulatorBatchSize :: !Int
  -- ^ Maximum batch size in bytes
  , accumulatorLingerMs :: !Int
  -- ^ Time to wait for batching in milliseconds
  , accumulatorCompression :: !Compression.CompressionCodec
  -- ^ Compression codec to use
  , accumulatorCompressionLevel :: !Compression.CompressionLevel
  -- ^ Compression level (KIP-353/776/909)
  }


{- | Batch accumulator that manages batches for all partitions.

Pre-Tier-2 the partition map was an 'StmMap.Map' so insertion of
a new partition could compose with the per-partition queue swap
inside one STM transaction. Tier 2 replaced both the per-partition
queue refs and the partition map with 'IORef'-backed equivalents;
new-partition insertion is now an 'atomicModifyIORef\'' CAS that
preserves the "first inserter wins, the loser walks away" race
semantics @StmMap.insert@ already gave us.

'accumulatorClosed' stays a 'TVar' (Tier 1's recommendation:
"leave the booleans alone in Tier 1, revisit if profiling
indicates it"). The producer / sender / close paths only read it
once per call so the per-tick STM cost is negligible compared to
the per-record win in Tier 2.
-}
data BatchAccumulator = BatchAccumulator
  { accumulatorConfig :: !BatchAccumulatorConfig
  -- ^ Configuration
  , accumulatorPartitions :: !(IORef (HashMap TopicPartition PartitionQueue))
  {- ^ Per-partition queues. New partitions are inserted via
  'atomicModifyIORef\'' CAS with a re-check inside the
  modify so concurrent inserters of the same partition see
  the same 'PartitionQueue'.
  -}
  , accumulatorClosed :: !(TVar Bool)
  {- ^ Whether accumulator is closed. Kept as a 'TVar' to keep
  'closeBatchAccumulator's flip-then-drain semantics
  visible to the rest of the producer (notably the sender
  thread's shutdown signal); the cost is one 'readTVarIO'
  per 'appendRecord' which is dominated by the IORef work
  below.
  -}
  }


-- | Create a new batch accumulator
createBatchAccumulator
  :: Int
  -- ^ Batch size in bytes
  -> Int
  -- ^ Linger time in milliseconds
  -> Compression.CompressionCodec
  -- ^ Compression codec
  -> Compression.CompressionLevel
  -- ^ Compression level
  -> IO BatchAccumulator
createBatchAccumulator batchSize lingerMs compression compressionLevel = do
  let config =
        BatchAccumulatorConfig
          { accumulatorBatchSize = batchSize
          , accumulatorLingerMs = lingerMs
          , accumulatorCompression = compression
          , accumulatorCompressionLevel = compressionLevel
          }
  partitions <- newIORef HashMap.empty
  closed <- newTVarIO False
  return
    BatchAccumulator
      { accumulatorConfig = config
      , accumulatorPartitions = partitions
      , accumulatorClosed = closed
      }


{- | Close the batch accumulator. Marks all pending batches as
ready for draining.

Pre-Tier-2 the close-flag flip and the per-partition drain ran
inside a single STM transaction, so a producer thread that had
already entered 'appendRecordStamped's STM block before the
close committed would either see closed=True (and bail out) or
complete its append before close ran. Tier 2 splits these into
two operations: the 'TVar' flip happens first under STM, then
the partition snapshot drain happens via 'atomicModifyIORef\''
per partition. The race window for "producer appends after
closed flag is True but its append still lands" widens
slightly, but the late-append outcome is identical to the
pre-Tier-2 behaviour: the record sits in a partition's ready
queue that nobody is draining, exactly as a stray late append
under STM would (the sender thread has been told to stop). See
@docs/STM_REPLACEMENT_SPEC.md@ Tier 2 "closed-flag race" note.
-}
closeBatchAccumulator :: BatchAccumulator -> IO ()
closeBatchAccumulator BatchAccumulator {..} = do
  atomically $ writeTVar accumulatorClosed True
  parts <- readIORef accumulatorPartitions
  forM_ (HashMap.elems parts) markCurrentBatchReadyIO


{- | Mark every partition's filling batch as 'Ready' /without/
closing the accumulator.  Used by 'Kafka.Client.Producer.
flushProducer' so subsequent sends keep working — the JVM
client + librdkafka behave the same way: 'flush()' is a
drain-checkpoint, not a destructor.

Tier 2: was an STM transaction over every partition; now folds
'atomicModifyIORef\'' over a snapshot of the partition map.
Loses cross-partition atomicity which is fine — flush is a
drain-checkpoint, not a barrier (mid-flush appends to other
partitions were always allowed; the JVM client's @flush()@
behaves the same way).
-}
flushPendingBatches :: BatchAccumulator -> IO ()
flushPendingBatches BatchAccumulator {..} = do
  parts <- readIORef accumulatorPartitions
  forM_ (HashMap.elems parts) markCurrentBatchReadyIO


{- | Drop every locally queued batch without closing the accumulator.

This is the accumulator-level analogue of librdkafka's
@RD_KAFKA_PURGE_F_QUEUE@: records that have already been drained
by the sender thread are not touched, but records still sitting in
the per-partition ready queue or current filling batch are removed
and their callbacks receive the supplied local error.

The returned map counts records purged per topic-partition so the
producer can repair sequence bookkeeping for idempotent producers:
this implementation assigns sequence numbers before enqueue, while
a local purge means those sequence numbers never reached a broker.
-}
purgePendingBatches :: BatchAccumulator -> Text -> IO (HashMap TopicPartition Int)
purgePendingBatches BatchAccumulator {..} reason = do
  parts <- readIORef accumulatorPartitions
  fmap (foldl' (HashMap.unionWith (+)) HashMap.empty) $
    forM (HashMap.elems parts) purgePartitionQueue
  where
    purgePartitionQueue :: PartitionQueue -> IO (HashMap TopicPartition Int)
    purgePartitionQueue PartitionQueue {..} = do
      queued <- atomicModifyIORef' queueBatches $ \s -> (Seq.empty, s)
      current <- atomicModifyIORef' queueCurrentBatch $ \mb -> (Nothing, mb)
      currentCounts <- case current of
        Nothing -> pure HashMap.empty
        Just ba -> do
          batch <- snapshotBatch ba (Failed reason)
          failBatch batch
          pure (countBatch HashMap.empty batch)
      forM_ queued failBatch
      pure (foldl' countBatch currentCounts queued)

    countBatch :: HashMap TopicPartition Int -> ProducerBatch -> HashMap TopicPartition Int
    countBatch counts batch =
      HashMap.insertWith
        (+)
        (batchTopicPartition batch)
        (V.length (batchRecords batch))
        counts

    failBatch :: ProducerBatch -> IO ()
    failBatch batch =
      V.mapM_ (\cb -> runRecordCallback cb (Left reason)) (batchCallbacks batch)


{- | Promote a partition's in-flight accumulating batch onto the
ready 'queueBatches', if any. Snapshots the 'BatchAccumulating'
into an immutable 'ProducerBatch' marked 'Ready' before
pushing.
-}
markCurrentBatchReadyIO :: PartitionQueue -> IO ()
markCurrentBatchReadyIO PartitionQueue {..} = do
  promoted <- atomicModifyIORef' queueCurrentBatch $ \mb -> case mb of
    Nothing -> (Nothing, Nothing)
    Just ba -> (Nothing, Just ba)
  case promoted of
    Nothing -> pure ()
    Just ba -> do
      sealed <- snapshotBatch ba Ready
      atomicModifyIORef' queueBatches $ \s -> (s |> sealed, ())


{- | Append a record to the accumulator
Returns True if the record was added, False if accumulator is closed
-}
appendRecord
  :: BatchAccumulator
  -> TopicPartition
  -> RB.Record
  -> IO Bool
appendRecord accumulator tp record = do
  -- Use appendRecordWithCallback with a no-op callback
  appendRecordWithCallback accumulator tp record NoRecordCallback


{- | Producer-id / epoch / sequence triple stamped onto each new
batch when the producer is idempotent (KIP-98) or transactional
(KIP-98 / KIP-447). Carries an 'isTransactional' flag so the
record-batch encoder can flip the corresponding bit in the
attributes word.

Use 'noStamp' for non-idempotent producers; the accumulator will
leave 'batchProducerId' / 'batchProducerEpoch' / 'batchBaseSequence'
at the @no…@ sentinels.
-}
data BatchStamp = BatchStamp
  { stampProducerId :: !Int64
  , stampProducerEpoch :: !Int16
  , stampBaseSequence :: !Int32
  {- ^ Sequence number to stamp on a /freshly created/ batch.
  Records appended to a batch that's already filling inherit
  that batch's sequence base; the producer-side counter
  should therefore be advanced /after/ a batch is sealed.
  -}
  , stampIsTransactional :: !Bool
  }
  deriving (Eq, Show)


{- | Stamp value used when no idempotent / transactional state is
in scope.
-}
noStamp :: BatchStamp
noStamp =
  BatchStamp
    { stampProducerId = RB.noProducerId
    , stampProducerEpoch = RB.noProducerEpoch
    , stampBaseSequence = RB.noSequence
    , stampIsTransactional = False
    }


{- | Append a record carrying an explicit 'BatchStamp'. If the
record creates a new batch, the stamp is recorded on the batch;
if it joins an existing /filling/ batch, the existing stamp is
preserved and the call asserts (in debug mode) that the
producer-id + epoch match.

The producer is responsible for ensuring that consecutive
'stampBaseSequence' values across batches on the same
(topic, partition) form a gapless sequence; the accumulator only
records what it's given.
| One CAS attempt on a batch's hot state. Either
'AppendedSize' (record went in; new accumulated size returned
so the caller can decide whether to seal) or 'SealedRace'
(sealer's CAS landed first; record did /not/ go in, caller
must retry on a fresh batch).

This is the single point where every append site contends
with the sealer, and it's the reason late appends can no
longer be silently dropped: the seal CAS in 'snapshotBatch'
contends on the /same/ 'baHotState' 'IORef', so any append
whose CAS lands before the seal CAS is observed by the
snapshot, and any append whose CAS lands after sees
@bhSizeRaw < 0@ and bails here.
| One CAS attempt on a batch's hot state. Returns the new
accumulated size in bytes on success, or a negative value
(the seal sentinel that can never appear as a real size) if
the seal CAS in 'snapshotBatch' got there first.

/Implementation/: hand-rolled @readForCAS \/ casIORef@ from
'Data.Atomics' instead of 'atomicModifyIORef\''. The two
buy us the same atomicity guarantee (sequenced-consistent
CAS on the 'IORef') but @casIORef@ doesn't go through the
@(state, result)@ tuple closure that 'atomicModifyIORef\''
allocates per call. With ~3 M records/sec that's
~24 B × 3 M = ~72 MB/s of pure-overhead allocation removed
from the steady-state heap profile.

The retry loop is the standard ticket-then-CAS pattern:
read the current ticket, build the candidate state, attempt
the CAS; on failure ('peekTicket' on the returned ticket
gives us the current value without re-reading the 'IORef')
recompute against the new value and retry.
-}
{-# INLINE casAppend #-}
casAppend
  :: BatchAccumulating
  -> RB.Record
  -> RecordCallback
  -> Int
  -- ^ approximate record size
  -> IO Int
casAppend ba record callback rs = do
  tkt <- readForCAS (baHotState ba)
  go tkt
  where
    {-# INLINE go #-}
    go !tkt = do
      let !st = peekTicket tkt
          !raw = bhSizeRaw st
      if raw < 0
        -- Sealed: nothing to update. Return the negative
        -- sentinel so the caller routes the record through
        -- 'slowAppendIO'.
        then pure raw
        else do
          let !ns = raw + rs
              !st' =
                BatchHotState
                  { bhRecordsRev = record : bhRecordsRev st
                  , bhSizeRaw = ns
                  , bhCallbacksRev = callback : bhCallbacksRev st
                  }
          (ok, !tkt') <- casIORef (baHotState ba) tkt st'
          if ok
            then pure ns
            else go tkt'


{-# INLINE appendRecordStamped #-}
appendRecordStamped
  :: BatchAccumulator
  -> TopicPartition
  -> RB.Record
  -> RecordCallback
  -> BatchStamp
  -> IO Bool
appendRecordStamped acc@BatchAccumulator {..} tp record callback stamp = do
  -- The closed-flag check moved out of the per-record STM
  -- transaction; see the data-decl note above. We accept the
  -- widened race because 'closeBatchAccumulator' drains every
  -- partition after flipping the flag, so a stray late append
  -- ends up in a partition's ready queue that the sender thread
  -- is no longer servicing — same outcome the STM version
  -- produced for an interleaved late append.
  isClosed <- readTVarIO accumulatorClosed
  if isClosed
    then pure False
    else do
      mq <- HashMap.lookup tp <$> readIORef accumulatorPartitions
      case mq of
        Nothing -> slowAppendIO tp record callback stamp acc
        Just queue -> do
          mba <- readIORef (queueCurrentBatch queue)
          case mba of
            Nothing -> slowAppendIO tp record callback stamp acc
            Just ba -> do
              let !rs = approximateRecordSize record
                  !limit = accumulatorBatchSize accumulatorConfig
              !ns <- casAppend ba record callback rs
              if ns < 0
                -- Sealer's CAS landed first. The append did
                -- NOT go in; route to 'slowAppendIO' so the
                -- record lands in a fresh batch (which the
                -- sealer just left @queueCurrentBatch@ empty
                -- for, or which the slow path will install).
                then slowAppendIO tp record callback stamp acc
                else do
                  when (ns >= limit) $ sealCurrent acc queue ba
                  pure True


{- | Variant of 'appendRecordStamped' kept around for callers
that hold the single-producer-per-partition contract.

/History/: this used to bypass 'atomicModifyIORef\'' on
'baHotState' for a one-read-one-write fast path. That was
/unsound/: the sender thread (which is /not/ a producer
thread) runs 'drainReadyBatches' \/ 'sealCurrent' concurrently
with the producer, so the seal's snapshot read could land
between the producer's 'readIORef' and 'writeIORef' and miss
the about-to-be-written record entirely. 'appendRecordStamped'
now uses the seal-race-safe CAS protocol, and this function
is just an alias so existing call sites keep building.

The single-producer-per-partition contract is still useful for
partitioner-state lock-freeness in the producer above us; we
just don't get the @readIORef + writeIORef@ shortcut here.
| Like 'appendRecordStamped' but accepts an /absolute/
record timestamp override. The accumulator translates it to
the right 'recordTimestampDelta' relative to whichever batch
the record ends up landing in:

  * Fast path (current batch already filling) -- delta is
    computed against the in-progress batch's 'baBaseTimestamp'
    before the CAS, so the wire-level absolute timestamp will
    be exactly @ts@. The result of this is race-free because
    the batch's base timestamp is set once at creation and
    never mutates.
  * Slow path (no current batch yet) -- a fresh batch is
    created with 'baBaseTimestamp' = @ts@ and the record's
    delta = 0, so again the wire timestamp is exactly @ts@.

Pass 'Nothing' for the override to fall back to the legacy
behaviour (wall-clock base, delta = 0).
-}
{-# INLINE appendRecordStampedAt #-}
appendRecordStampedAt
  :: BatchAccumulator
  -> TopicPartition
  -> RB.Record
  -> RecordCallback
  -> BatchStamp
  -> Maybe Int64
  -- ^ absolute record timestamp
  -> IO Bool
appendRecordStampedAt acc@BatchAccumulator {..} tp record callback stamp mTs = do
  isClosed <- readTVarIO accumulatorClosed
  if isClosed
    then pure False
    else do
      mq <- HashMap.lookup tp <$> readIORef accumulatorPartitions
      case mq of
        Nothing -> slowAppendIOAt tp record callback stamp mTs acc
        Just queue -> do
          mba <- readIORef (queueCurrentBatch queue)
          case mba of
            Nothing -> slowAppendIOAt tp record callback stamp mTs acc
            Just ba -> do
              let !record' = case mTs of
                    Just ts ->
                      record
                        { RB.recordTimestampDelta =
                            ts - baBaseTimestamp ba
                        }
                    Nothing -> record
                  !rs = approximateRecordSize record'
                  !limit = accumulatorBatchSize accumulatorConfig
              !ns <- casAppend ba record' callback rs
              if ns < 0
                then slowAppendIOAt tp record callback stamp mTs acc
                else do
                  when (ns >= limit) $ sealCurrent acc queue ba
                  pure True


{- | Slow-path variant of 'slowAppendIO' that uses the
caller-supplied absolute timestamp as the new batch's
'baBaseTimestamp' instead of the wall clock. Falls back to the
wall clock when 'Nothing' is supplied.
-}
{-# INLINE slowAppendIOAt #-}
slowAppendIOAt
  :: TopicPartition
  -> RB.Record
  -> RecordCallback
  -> BatchStamp
  -> Maybe Int64
  -> BatchAccumulator
  -> IO Bool
slowAppendIOAt tp record callback stamp mTs acc@BatchAccumulator {..} = do
  baseTime <- case mTs of
    Just ts -> pure ts
    Nothing -> getCurrentTimeMillis
  queue <- getOrCreatePartitionQueue acc tp
  candidate <- newAccumulating accumulatorConfig tp baseTime stamp
  installed <- atomicModifyIORef' (queueCurrentBatch queue) $ \mc ->
    case mc of
      Just b -> (Just b, b)
      Nothing -> (Just candidate, candidate)
  -- If the slot was already populated by a concurrent caller
  -- (or by an earlier slow-append for this same partition that
  -- our own retry collides with), recompute the delta against
  -- the /installed/ batch's actual base. We are guaranteed
  -- 'installed' is the live batch at this point.
  let !record' = case mTs of
        Just ts ->
          record {RB.recordTimestampDelta = ts - baBaseTimestamp installed}
        Nothing -> record
      !rs = approximateRecordSize record'
      !limit = accumulatorBatchSize accumulatorConfig
  !ns <- casAppend installed record' callback rs
  if ns < 0
    then slowAppendIOAt tp record callback stamp mTs acc
    else do
      when (ns >= limit) $ sealCurrent acc queue installed
      pure True


{-# INLINE appendRecordStampedUnsafe #-}
appendRecordStampedUnsafe
  :: BatchAccumulator
  -> TopicPartition
  -> RB.Record
  -> RecordCallback
  -> BatchStamp
  -> IO Bool
appendRecordStampedUnsafe = appendRecordStamped


{- | Append a /sequence/ of records to one (topic, partition) in
a single 'atomicModifyIORef\'' call on the partition's
@queueCurrentBatch@. Amortises the per-call overhead the
single-record 'appendRecordStamped' pays on its hot path
(closed check, partition map lookup, queue-current swap) so
high-throughput producers that already have a list of records
to publish can hand them all over in one go.

Per-record work (size accounting, batch sealing on size limit,
sequence promotion to the ready queue) is folded inside the
one atomic-modify; if the records overflow the configured batch
size we may emit multiple ready batches at once. Callbacks
attach to the same record vector (the caller passes one
callback list keyed by record position).

Returns 'False' if the accumulator is closed; 'True' on
successful enqueue regardless of how many internal batches
were sealed.
-}
appendRecordsStamped
  :: BatchAccumulator
  -> TopicPartition
  -> Seq RB.Record
  -- ^ records to append, in order
  -> Seq RecordCallback
  -- ^ one callback per record (same order; same length)
  -> BatchStamp
  -> IO Bool
appendRecordsStamped acc@BatchAccumulator {..} tp recs cbs stamp = do
  isClosed <- readTVarIO accumulatorClosed
  if isClosed
    then pure False
    else do
      queue <- getOrCreatePartitionQueue acc tp
      go queue (Seq.zip recs cbs)
      pure True
  where
    !batchSizeLimit = accumulatorBatchSize accumulatorConfig

    -- Walk the input records, folding them into the current
    -- accumulating batch. Each iteration:
    --   * ensures there's a current batch (creating one if not),
    --   * atomically appends the next record to its hot state,
    --   * seals if the batch hit the size limit.
    -- This is the canonical multi-record flow; it shares the
    -- single-record code paths above.
    go :: PartitionQueue -> Seq (RB.Record, RecordCallback) -> IO ()
    go queue s = case Seq.viewl s of
      Seq.EmptyL -> pure ()
      (r, cb) Seq.:< rest -> do
        ba <- ensureCurrent queue
        let !rs = approximateRecordSize r
        !ns <- casAppend ba r cb rs
        if ns < 0
          -- Sealer raced us; route this single record through
          -- the slow path (which will install a fresh batch if
          -- needed) and continue the bulk fold against the
          -- partition's now-current batch.
          then do
            _ <- slowAppendIO tp r cb stamp acc
            go queue rest
          else do
            when (ns >= batchSizeLimit) $ sealCurrent acc queue ba
            go queue rest

    ensureCurrent :: PartitionQueue -> IO BatchAccumulating
    ensureCurrent queue = do
      mba <- readIORef (queueCurrentBatch queue)
      case mba of
        Just b -> pure b
        Nothing -> do
          ct <- getCurrentTimeMillis
          candidate <- newAccumulating accumulatorConfig tp ct stamp
          atomicModifyIORef' (queueCurrentBatch queue) $ \mc ->
            case mc of
              Just b -> (Just b, b)
              Nothing -> (Just candidate, candidate)


{- | Slow path: read the clock, ensure a 'PartitionQueue' exists,
create a fresh 'BatchAccumulating' if needed, then fall back
to the regular 'appendRecordStamped' to handle the actual
append (and any seal it triggers).
-}
{-# INLINE slowAppendIO #-}
slowAppendIO
  :: TopicPartition
  -> RB.Record
  -> RecordCallback
  -> BatchStamp
  -> BatchAccumulator
  -> IO Bool
slowAppendIO tp record callback stamp acc@BatchAccumulator {..} = do
  currentTime <- getCurrentTimeMillis
  queue <- getOrCreatePartitionQueue acc tp
  -- CAS in a fresh 'BatchAccumulating' if one isn't there yet.
  -- Race semantics: if a concurrent caller installs first, we
  -- discard our candidate and use theirs.
  candidate <- newAccumulating accumulatorConfig tp currentTime stamp
  installed <- atomicModifyIORef' (queueCurrentBatch queue) $ \mc ->
    case mc of
      Just b -> (Just b, b)
      Nothing -> (Just candidate, candidate)
  -- The regular CAS-with-sealed-check append. Even on the slow
  -- path 'installed' might race a sealer (rare, but possible if
  -- the sealer fires on linger between our 'queueCurrentBatch'
  -- swap and this CAS). On 'SealedRace' we recurse to install
  -- another fresh candidate.
  let !rs = approximateRecordSize record
      !limit = accumulatorBatchSize accumulatorConfig
  !ns <- casAppend installed record callback rs
  if ns < 0
    then slowAppendIO tp record callback stamp acc
    else do
      when (ns >= limit) $ sealCurrent acc queue installed
      pure True


{- | Snapshot the in-progress 'BatchAccumulating' into an
immutable 'ProducerBatch' marked 'Ready', swap
'queueCurrentBatch' to 'Nothing' (so future appends create a
fresh accumulating batch), and push the sealed batch onto
'queueBatches'.
-}
sealCurrent
  :: BatchAccumulator
  -> PartitionQueue
  -> BatchAccumulating
  -> IO ()
sealCurrent _acc queue ba = do
  -- Best-effort swap: if a concurrent caller has already swapped
  -- this same accumulating batch out (e.g. another thread sealed
  -- on its own atomic-modify-then-seal path), we just bail.
  swapped <- atomicModifyIORef' (queueCurrentBatch queue) $ \mc ->
    case mc of
      Just _ -> (Nothing, True)
      Nothing -> (Nothing, False)
  when swapped $ do
    sealed <- snapshotBatch ba Ready
    atomicModifyIORef' (queueBatches queue) $ \s -> (s |> sealed, ())


-- | Allocate a fresh 'BatchAccumulating' with empty hot state.
newAccumulating
  :: BatchAccumulatorConfig
  -> TopicPartition
  -> Int64
  -- ^ current time millis
  -> BatchStamp
  -> IO BatchAccumulating
newAccumulating BatchAccumulatorConfig {..} tp currentTime BatchStamp {..} = do
  hot <-
    newIORef
      BatchHotState
        { bhRecordsRev = []
        , bhSizeRaw = 0
        , bhCallbacksRev = []
        }
  pure
    BatchAccumulating
      { baTopicPartition = tp
      , baCreateTime = currentTime
      , baBaseTimestamp = currentTime
      , baCompression = accumulatorCompression
      , baCompressionLevel = accumulatorCompressionLevel
      , baProducerId = stampProducerId
      , baProducerEpoch = stampProducerEpoch
      , baBaseSequence = stampBaseSequence
      , baIsTransactional = stampIsTransactional
      , baHotState = hot
      }


{- | Snapshot a 'BatchAccumulating' into an immutable
'ProducerBatch' with the supplied 'BatchState'.

The read of the hot state is the /seal CAS/: it atomically
flips @bhSizeRaw@ to 'sealedSentinel' and captures the final hot-state
value. Because every append goes through 'casAppend' on the
same 'IORef', the protocol is total — see 'BatchHotState'\'s
haddock for the linearisation argument. The snapshot we
return contains exactly the records whose append CAS landed
before this seal CAS; any append CAS that lands later sees
@bhSizeRaw < 0@ and routes itself to a fresh batch.

The CAS is idempotent on already-sealed batches (the closure
returns the same state with the seal bit re-set), so calling
'snapshotBatch' twice on the same 'BatchAccumulating' is
safe — both calls produce the same 'ProducerBatch'. (The
seal-then-snapshot sites in this module never do that, but the
property keeps the protocol robust against future callers.)
-}
snapshotBatch :: BatchAccumulating -> BatchState -> IO ProducerBatch
snapshotBatch BatchAccumulating {..} state = do
  -- Seal CAS via 'Data.Atomics.casIORef': flip 'bhSizeRaw' to
  -- the negative 'sealedSentinel' and capture the pre-seal
  -- state in one CAS. Any append CAS that arrives after this
  -- one observes the negative sentinel and bails to
  -- 'slowAppendIO'; any append CAS that landed before this one
  -- is included in the captured state.
  --
  -- The 'casIORef' (rather than 'atomicModifyIORef\'') matches
  -- the append-side hot path so neither the per-record nor the
  -- per-batch CAS pays the @(state, result)@ tuple closure
  -- overhead.
  st <- sealCAS
  -- The hot-state lists are accumulated in REVERSE (cons-on-
  -- left, O(1) per append). The forward-order 'V.Vector' is
  -- written by walking the reversed list and storing each
  -- element at a /descending/ slot index inside an 'MV.MVector'
  -- via 'MV.unsafeWrite', then 'V.unsafeFreeze'-ing it.
  --
  -- Why this beats @V.fromListN nRec (reverse xs)@: 'reverse'
  -- on a singly-linked list allocates one fresh cons cell per
  -- element (~24 B / record) for the reversed spine that
  -- 'V.fromListN' then walks once and discards. The
  -- mutable-vector + 'unsafeFreeze' shape skips that
  -- intermediate spine entirely — the only per-element
  -- allocation is the slot in the final (immutable) 'V.Vector',
  -- which both shapes already pay. Saves ~24 B / record at
  -- seal time on top of dropping one full O(n) walk of the
  -- list, which the heap profile flagged as a 'reverse' / cons
  -- hotspot per sealed batch.
  let !nRec = length (bhRecordsRev st)
  !records <- vecFromRev nRec (bhRecordsRev st)
  !callbacks <- vecFromRev nRec (bhCallbacksRev st)
  pure
    ProducerBatch
      { batchTopicPartition = baTopicPartition
      , batchRecords = records
      , batchSizeBytes = sizeOf st
      , batchCreateTime = baCreateTime
      , batchBaseTimestamp = baBaseTimestamp
      , batchState = state
      , batchCompression = baCompression
      , batchCompressionLevel = baCompressionLevel
      , batchCallbacks = callbacks
      , batchAttempts = 0
      , batchProducerId = baProducerId
      , batchProducerEpoch = baProducerEpoch
      , batchBaseSequence = baBaseSequence
      , batchIsTransactional = baIsTransactional
      }
  where
    -- Loop a 'casIORef' that flips 'bhSizeRaw' to the seal
    -- sentinel and returns the captured /pre-seal/ state. On
    -- collision (concurrent appender or concurrent sealer), use
    -- the ticket's view of the current state to retry without
    -- a second 'IORef' read.
    --
    -- Idempotent on already-sealed batches: if the captured
    -- state already has @bhSizeRaw < 0@, we just hand it back
    -- without writing — so two concurrent seals produce the
    -- same 'ProducerBatch'.
    sealCAS :: IO BatchHotState
    sealCAS = do
      tkt <- readForCAS baHotState
      sealLoop tkt
    sealLoop :: Ticket BatchHotState -> IO BatchHotState
    sealLoop !tkt = do
      let !s = peekTicket tkt
      if bhSizeRaw s < 0
        then pure s
        else do
          let !s' = s {bhSizeRaw = sealedSentinel}
          (ok, !tkt') <- casIORef baHotState tkt s'
          if ok then pure s else sealLoop tkt'


{- | Build an immutable 'V.Vector' of @n@ elements from a
reversed singly-linked list, in /forward/ order, without
materialising the reversed cons spine that
@V.fromListN n (reverse xs)@ would produce.

Walks the list once, writing each element at a descending
slot index inside an 'MV.MVector' allocated up-front at the
known length. The final 'V.unsafeFreeze' is a constant-time
handoff (no copy). Caller passes the precomputed length so
we don't pay a separate O(n) 'length' walk.
-}
{-# INLINE vecFromRev #-}
vecFromRev :: Int -> [a] -> IO (V.Vector a)
vecFromRev !n xs = do
  v <- MV.unsafeNew n
  let go !_ [] = pure ()
      go !i (y : ys) = do
        MV.unsafeWrite v i y
        go (i - 1) ys
  go (n - 1) xs
  V.unsafeFreeze v


{- | Append a record with a completion callback
Returns True if the record was added, False if accumulator is closed
The callback will be invoked when the broker acknowledges the record
-}
appendRecordWithCallback
  :: BatchAccumulator
  -> TopicPartition
  -> RB.Record
  -> RecordCallback
  -- ^ Callback invoked on completion
  -> IO Bool
appendRecordWithCallback acc tp record callback =
  appendRecordStamped acc tp record callback noStamp


{- | Drain all ready batches from the accumulator. This includes
batches that are full or have exceeded linger time.

Tier 2 rewrites this from one big STM transaction over every
partition into a fold of per-partition 'atomicModifyIORef\''
calls. Cross-partition atomicity is lost, which matches the
pre-Tier-2 producer's expectations: the sender's drain
iteration was already a snapshot in time, and a partition that
becomes ready /during/ the drain just gets picked up on the
sender's next iteration.
-}
drainReadyBatches :: BatchAccumulator -> IO [ProducerBatch]
drainReadyBatches BatchAccumulator {..} = do
  currentTime <- getCurrentTimeMillis
  parts <- readIORef accumulatorPartitions
  fmap concat $ forM (HashMap.elems parts) $ \PartitionQueue {..} -> do
    -- Linger check: if the current /filling/ batch has been open
    -- longer than 'lingerMs', promote it to the ready queue
    -- before draining.
    promoted <- atomicModifyIORef' queueCurrentBatch $ \mc ->
      case mc of
        Just ba
          | currentTime - baCreateTime ba >= lingerMs ->
              (Nothing, Just ba)
        _ -> (mc, Nothing)
    case promoted of
      Nothing -> pure ()
      Just ba -> do
        sealed <- snapshotBatch ba Ready
        atomicModifyIORef' queueBatches $ \s -> (s |> sealed, ())
    atomicModifyIORef' queueBatches $ \s ->
      let (ready, remaining) = Seq.spanl isReadyBatch s
      in (remaining, toList ready)
  where
    lingerMs = fromIntegral $ accumulatorLingerMs accumulatorConfig

    isReadyBatch :: ProducerBatch -> Bool
    isReadyBatch batch = batchState batch == Ready


{- | Check if there are any ready batches.

Tier 2: walks the IORef-backed partition snapshot and reads
each partition's queue + current batch with plain
'readIORef'. Both reads acquire their own visibility window;
since the only state transition we care about is "is there a
ready batch?" and a producer that promotes a batch to ready
between the queue read and the current-batch read just gets
noticed on the next call, the racy view is fine.
-}
hasReadyBatches :: BatchAccumulator -> IO Bool
hasReadyBatches BatchAccumulator {..} = do
  currentTime <- getCurrentTimeMillis
  parts <- readIORef accumulatorPartitions
  go currentTime (HashMap.elems parts)
  where
    lingerMs = fromIntegral $ accumulatorLingerMs accumulatorConfig

    go _ [] = pure False
    go now (PartitionQueue {..} : rest) = do
      batches <- readIORef queueBatches
      if any (\b -> batchState b == Ready) batches
        then pure True
        else do
          currentM <- readIORef queueCurrentBatch
          case currentM of
            Just ba | now - baCreateTime ba >= lingerMs -> pure True
            _ -> go now rest


{- | Get or create a partition queue.

Tier 2: was an STM 'StmMap.lookup' / 'StmMap.insert' pair;
now an 'atomicModifyIORef\'' CAS over the partition map. The
CAS is in three steps:

1. Cheap fast path: 'readIORef' the map and look up the
   partition. If present, we're done — no allocation, no CAS.
2. Otherwise allocate fresh 'IORef's for the partition queue.
3. CAS the partition map: if some other inserter beat us to
   the same 'TopicPartition' we throw our candidate away and
   return theirs (matches @StmMap.insert@'s "first writer
   wins" semantics so all subsequent producers on this
   partition see the same 'PartitionQueue').
-}
getOrCreatePartitionQueue
  :: BatchAccumulator
  -> TopicPartition
  -> IO PartitionQueue
getOrCreatePartitionQueue BatchAccumulator {..} tp = do
  existing <- HashMap.lookup tp <$> readIORef accumulatorPartitions
  case existing of
    Just q -> pure q
    Nothing -> do
      newBatchesRef <- newIORef Seq.empty
      newCurrentRef <- newIORef Nothing
      let !candidate = PartitionQueue newBatchesRef newCurrentRef
      atomicModifyIORef' accumulatorPartitions $ \m ->
        case HashMap.lookup tp m of
          Just q' -> (m, q')
          Nothing -> (HashMap.insert tp candidate m, candidate)


-- | Create a new empty batch
createBatch
  :: BatchAccumulatorConfig
  -> TopicPartition
  -> Int64
  -- ^ Current time
  -> ProducerBatch
createBatch BatchAccumulatorConfig {..} tp currentTime =
  ProducerBatch
    { batchTopicPartition = tp
    , batchRecords = V.empty
    , batchSizeBytes = 0
    , batchCreateTime = currentTime
    , batchBaseTimestamp = currentTime
    , batchState = Filling
    , batchCompression = accumulatorCompression
    , batchCompressionLevel = accumulatorCompressionLevel
    , batchCallbacks = V.empty
    , batchAttempts = 0
    , batchProducerId = RB.noProducerId
    , batchProducerEpoch = RB.noProducerEpoch
    , batchBaseSequence = RB.noSequence
    , batchIsTransactional = False
    }


{- | Look up the partition's currently-in-progress
'BatchAccumulating' (or 'Nothing' if there isn't one yet).
Used by 'Kafka.Client.Producer.sendMessageFastest_' to
cache the in-progress batch handle in producer-local state and
skip the per-record partition-map + queue-current lookups.
-}
currentBatchOf
  :: BatchAccumulator
  -> TopicPartition
  -> IO (Maybe (BatchAccumulating, PartitionQueue))
currentBatchOf BatchAccumulator {..} tp = do
  mq <- HashMap.lookup tp <$> readIORef accumulatorPartitions
  case mq of
    Nothing -> pure Nothing
    Just queue -> do
      mba <- readIORef (queueCurrentBatch queue)
      case mba of
        Nothing -> pure Nothing
        Just ba -> pure (Just (ba, queue))


{- | Append a record directly to a known 'BatchAccumulating',
skipping the closed-flag check, the partition-map lookup, and
the 'queueCurrentBatch' read 'appendRecordStamped' would do.
Returns 'True' if the batch is still the partition's current
batch after the append; 'False' if this append filled the
batch and triggered a seal (so the caller should refresh its
cached handle).

/Safety/: caller must ensure no other thread concurrently
appends to this same 'BatchAccumulating'. The default sticky
partitioner + single producer thread is the canonical safe
shape.
-}
{-# INLINE appendDirect #-}
appendDirect
  :: BatchAccumulator
  -> PartitionQueue
  -> BatchAccumulating
  -> RB.Record
  -> RecordCallback
  -> IO Bool
appendDirect acc@BatchAccumulator {..} queue ba record callback = do
  let !rs = approximateRecordSize record
      !limit = accumulatorBatchSize accumulatorConfig
  !ns <- casAppend ba record callback rs
  if ns < 0
    -- The cached 'BatchAccumulating' was sealed by another
    -- thread between the producer's cache lookup and our CAS.
    -- Route the record through 'slowAppendIO' so it lands in the
    -- partition's now-current (or about-to-be-installed) batch,
    -- and signal the caller to invalidate its cached handle by
    -- returning 'False'.
    then do
      _ <- slowAppendIO (baTopicPartition ba) record callback (stampOf ba) acc
      pure False
    else
      if ns >= limit
        then do
          sealCurrent acc queue ba
          pure False
        else pure True


{- | Reconstruct the 'BatchStamp' a 'BatchAccumulating' was born
with so 'appendDirect' can hand it to 'slowAppendIO' on the
seal-race retry path. We can't reach back to the producer for
a fresh stamp from inside the accumulator, so we reuse the
in-progress batch's stamp; for the non-idempotent / non-
transactional case this is just 'noStamp', and for the
idempotent / transactional case the next sequence number is
already assigned to this batch so reusing the stamp is what
the producer would have done anyway.
-}
{-# INLINE stampOf #-}
stampOf :: BatchAccumulating -> BatchStamp
stampOf BatchAccumulating {..} =
  BatchStamp
    { stampProducerId = baProducerId
    , stampProducerEpoch = baProducerEpoch
    , stampBaseSequence = baBaseSequence
    , stampIsTransactional = baIsTransactional
    }


{- | Get current time in milliseconds.

Uses 'Kafka.Time.currentTimeMillis' which on Linux reads the
vDSO-mapped @CLOCK_REALTIME_COARSE@ (~8 ns per call) and on
macOS/BSD reads the regular vDSO 'CLOCK_REALTIME'. The
accumulator's @tryFastAppend@ STM-only path skips this call
entirely; only the @slowAppend@ path (one in N records) needs
the timestamp to seed a fresh batch.
-}
getCurrentTimeMillis :: IO Int64
getCurrentTimeMillis = KafkaTime.currentTimeMillis


{- | Approximate size of a record in bytes
This is a rough estimate for batch size tracking
-}
{-# INLINE approximateRecordSize #-}
approximateRecordSize :: RB.Record -> Int
approximateRecordSize RB.Record {..} =
  let keySize = maybe 0 BS.length recordKey
      valueSize = maybe 0 BS.length recordValue
      headerSize = sum $ map (\h -> BS.length (RB.headerKey h) + maybe 0 BS.length (RB.headerValue h)) recordHeaders
      -- Add overhead for VarInt encoding and metadata
      overhead = 20
  in keySize + valueSize + headerSize + overhead
