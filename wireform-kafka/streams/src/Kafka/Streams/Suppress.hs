{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

{- |
Module      : Kafka.Streams.Suppress
Description : Suppress operator

Buffers updates per (windowed) key and emits each key's final
value only after the window has closed (i.e. stream time has
advanced past @windowEnd + gracePeriod@). Mirrors
@KTable.suppress(Suppressed.untilWindowCloses(...))@ over a
windowed KTable.

== Design

The suppress processor is wired as an additional node downstream
of a windowed aggregation. It holds a 'KeyValueStore' of buffered
records keyed by 'WindowedKey'. On every input record:

  1. The buffer is updated with the latest value for that
     windowed key.
  2. The processor iterates the buffer and forwards (then removes)
     every entry whose window has closed.

The driver advances stream time as records arrive; the
'TopologyTestDriver' user can additionally call
'advanceDriverStreamTime' to trigger emissions without sending
records.

For non-windowed KTables, 'suppressUntilTimeLimit' offers a
coarser per-key debounce: emit at most one update per key per
time-limit window.
-}
module Kafka.Streams.Suppress (
  suppressWindowed,
  suppressWindowedWith,
  suppressUntilTimeLimit,
  streamFromWindowedHandle,
  streamFromSessionWindowedHandle,
  suppressWindowedHandle,
  SuppressBufferFullException (..),

  -- * Suppressed builder
  Suppressed (..),
  untilWindowCloses,
  untilTimeLimit,
  suppressKStream,

  -- * Buffer config
  BufferConfig (..),
  BufferOverflowPolicy (..),
  unboundedBufferConfig,
  maxBytesBufferConfig,
  maxRecordsBufferConfig,
  shutDownWhenFull,
  emitEarlyWhenFull,
  dropOldestSilently,

  -- * Dead-letter shelf (KIP-1033-style overflow routing)

  --
  -- Routes the oldest over-cap entries to a side topic instead of
  -- throwing or emitting them early. The Riffle spec calls this
  -- the @shed@ policy alongside @drop@ / @fail@ /
  -- @spill-to-durable-state@; @drop@ ships as 'DropOldestSilently',
  -- @fail@ as 'ShutdownWhenFull', and @shed@ as the dedicated
  -- 'suppressWindowedShed' operator below. The spill policy is
  -- deferred until snapshot-aware state backends land.
  DeadLetterShelf (..),
  suppressWindowedShed,
) where

import Control.Exception (Exception, throwIO)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BSL
import Data.IORef
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Control.Monad (forM_, when)
import GHC.Generics (Generic)
import Kafka.Streams.KStream (KStream (..))
import Kafka.Streams.Processor (
  Processor (..),
  ProcessorContext (..),
  PunctuationType (..),
  Punctuator (..),
  effectiveTime,
  forwardRecord,
  getStateStore,
  processorName,
  schedule,
 )
import Kafka.Streams.Processor qualified
import Kafka.Streams.Serde (Serde (..))
import Kafka.Streams.SessionWindowedKStream qualified as SWKS
import Kafka.Streams.State.KeyValue.InMemory (
  inMemoryKeyValueStoreBuilder,
 )
import Kafka.Streams.State.Store (
  withStoreSerdesKV,
  AnyStateStore (..),
  KeyValueStore (..),
  StoreBuilderKV,
  SessionKey (..),
  StoreName,
  WindowedKey (..),
  kvIteratorToList,
 )
import Kafka.Streams.Serde.Windowed (sessionKeySerde, windowedSerde)
import Kafka.Streams.StreamsBuilder (
  freshNodeName,
  freshStoreName,
  withTopology_,
 )
import Kafka.Streams.Time (Duration, Timestamp (..), durationMillis, millis)
import Kafka.Streams.TimeWindowedKStream (
  EmitStrategy (..),
  WindowedTableHandle (..),
 )
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Types (Record (..), TopicName, emptyHeaders, unTopicName)
import Kafka.Streams.Window (windowsGracePeriod, windowsSize)
import Unsafe.Coerce qualified as Unsafe


----------------------------------------------------------------------
-- WindowedTableHandle -> KStream
----------------------------------------------------------------------

{- | Tap a 'WindowedTableHandle' as a 'KStream' of (windowed-key,
value) records. Each downstream record corresponds to one update
of the underlying window store.
-}
streamFromWindowedHandle
  :: forall k v
   . Ord k
  => WindowedTableHandle k v
  -> Serde k
  -- ^ key serde for downstream
  -> Serde v
  -- ^ value serde for downstream
  -> IO (KStream (WindowedKey k) v)
streamFromWindowedHandle h kserde vserde = case wthEmit h of
  OnWindowClose -> do
    -- Defer the (WindowedKey k, v) emission until the window has
    -- fully closed. We tap the windowed handle as an emit-on-update
    -- stream first, then attach the suppress operator on top —
    -- routing through 'suppressWindowedHandle' here would recurse
    -- back through this function and loop.
    s <- doStreamFromWindowedHandle h kserde vserde
    suppressWindowed
      (millis (windowsGracePeriod (wthWindows h)))
      (windowsSize (wthWindows h))
      s
  OnWindowUpdate -> doStreamFromWindowedHandle h kserde vserde


{- | Every update of the window store as a record: the aggregation
node forwards them keyed by window already.
-}
doStreamFromWindowedHandle
  :: forall k v
   . WindowedTableHandle k v
  -> Serde k
  -> Serde v
  -> IO (KStream (WindowedKey k) v)
doStreamFromWindowedHandle h kserde vserde =
  pure
    KStream
      { kstreamBuilder = wthBuilder h
      , kstreamParent = wthNode h
      , kstreamKeyChanged = False
      , kstreamKeySerde = windowedSerde kserde
      , kstreamValueSerde = vserde
      }


{- | Suppress all updates to a windowed KTable until the window has
closed (i.e. stream time has advanced past @windowEnd +
gracePeriod@). Each window emits exactly once with its final
value.

The window length is taken from the supplied 'WindowedTableHandle'.
The @gracePeriod@ is added to the window end before flushing, so
late records arriving within the grace can still update the
buffered value.
-}
suppressWindowed
  :: forall k v
   . Ord k
  => Duration
  -- ^ grace period
  -> Int64
  -- ^ window size (ms)
  -> KStream (WindowedKey k) v
  -> IO (KStream (WindowedKey k) v)
suppressWindowed grace windowSize =
  suppressWindowedWith grace windowSize unboundedBufferConfig


{- | Bounded @suppress(untilWindowCloses(...))@. Like
'suppressWindowed' but enforces the supplied
'BufferConfig': when the buffer exceeds the configured
record / byte limit the runtime either emits the oldest
buffered windows early ('EmitEarlyWhenFull') or throws
'SuppressBufferFullException' which propagates to the
KIP-1033 / 671 handlers ('ShutdownWhenFull').

Byte counting is approximate (per-record, not per-byte)
because the processor doesn't see the serialised value;
'bufMaxBytes' and 'bufMaxRecords' are treated as the same
soft cap. The cap is a count-of-buffered-windowed-keys.
-}
suppressWindowedWith
  :: forall k v
   . Ord k
  => Duration
  -- ^ grace period
  -> Int64
  -- ^ window size (ms)
  -> BufferConfig
  -> KStream (WindowedKey k) v
  -> IO (KStream (WindowedKey k) v)
suppressWindowedWith grace windowSize bufCfg s = do
  let b = kstreamBuilder s
  bufNm <- freshStoreName b "SUPPRESS-BUFFER"
  procNm <- freshNodeName b "SUPPRESS"
  let bufBuilder =
        withStoreSerdesKV
          (kstreamKeySerde s)
          (bufferedSerde (kstreamValueSerde s))
          (inMemoryKeyValueStoreBuilder bufNm)
          :: StoreBuilderKV (WindowedKey k) (Buffered v)
  withTopology_ b $ \t ->
    let !t1 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = procNm
              , Topo.processorSpecParents = [kstreamParent s]
              , Topo.processorSpecSupplier =
                  Topo.AnyProcessor
                    ( suppressWindowedProc @k @v
                        bufNm
                        (durationMillis grace)
                        windowSize
                        bufCfg
                    )
              , Topo.processorSpecStores = [bufNm]
              }
            t
        !t2 = Topo.addStateStoreKV bufBuilder [procNm] t1
    in t2
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = procNm
      , kstreamKeyChanged = kstreamKeyChanged s
      , kstreamKeySerde = kstreamKeySerde s
      , kstreamValueSerde = kstreamValueSerde s
      }


{- | Thrown when a 'ShutdownWhenFull' suppress buffer
overflows. Caught by the streams runtime's KIP-1033 /
KIP-671 handler stack and routed like any other processing
exception.
-}
data SuppressBufferFullException = SuppressBufferFullException
  { store :: !StoreName
  , cap :: !Int
  , at :: !Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)


----------------------------------------------------------------------
-- DeadLetterShelf (shed-to-DLQ overflow routing)
--
-- The Riffle spec's @shed@ policy: when a bounded suppress buffer
-- exceeds its record cap, the oldest entries are serialised and
-- pushed to a configured side topic instead of being dropped, emitted
-- early, or failing the task. Downstream of the suppress operator
-- /does not/ see the shed records.
----------------------------------------------------------------------

{- | Dead-letter routing for over-cap suppress entries. The cap +
topic + serdes are bundled because they must agree on the wire
type of the side stream.
-}
data DeadLetterShelf k v = DeadLetterShelf
  { dlsTopic :: !TopicName
  -- ^ Side topic that receives the shed records.
  , dlsKeySerde :: !(Serde (WindowedKey k))
  -- ^ Serde used to encode the windowed key on the wire.
  , dlsValueSerde :: !(Serde v)
  -- ^ Serde used to encode the buffered value on the wire.
  , dlsRecordCap :: !Int
  {- ^ Record budget. When the buffer holds more than this, the
  oldest entries (by 'WindowedKey' start timestamp) are
  shed in order until the buffer is back under the cap.
  -}
  }


{- | Bounded @suppressUntilWindowCloses@ with a /dead-letter
shelf/. When the buffer exceeds 'dlsRecordCap', the oldest
entries are serialised through 'dlsKeySerde' /
'dlsValueSerde' and routed to 'dlsTopic' via the engine's
record collector. Downstream of this operator never sees the
shed records.

@
shed <- DeadLetterShelf
  { dlsTopic       = topicName \"suppress-dlq\"
  , dlsKeySerde    = windowedSerde textSerde
  , dlsValueSerde  = int64Serde
  , dlsRecordCap   = 1024
  }
supressedStream <-
  suppressWindowedShed (millis 5_000) 60_000 shed inputStream
@

'suppressWindowedShed' is the typed analogue of the @shed@
entry in the Riffle bounded-buffer policy enumeration. The
other three policies (@fail@ \/ @drop@ \/ @emit-early@) are
carried via 'BufferOverflowPolicy' on 'suppressWindowedWith'.
-}
suppressWindowedShed
  :: forall k v
   . Ord k
  => Duration
  -- ^ grace period
  -> Int64
  -- ^ window size (ms)
  -> DeadLetterShelf k v
  -> KStream (WindowedKey k) v
  -> IO (KStream (WindowedKey k) v)
suppressWindowedShed grace windowSize shelf s = do
  let b = kstreamBuilder s
  bufNm <- freshStoreName b "SUPPRESS-SHED-BUFFER"
  procNm <- freshNodeName b "SUPPRESS-SHED"
  let bufBuilder =
        withStoreSerdesKV
          (kstreamKeySerde s)
          (bufferedSerde (kstreamValueSerde s))
          (inMemoryKeyValueStoreBuilder bufNm)
          :: StoreBuilderKV (WindowedKey k) (Buffered v)
  withTopology_ b $ \t ->
    let !t1 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = procNm
              , Topo.processorSpecParents = [kstreamParent s]
              , Topo.processorSpecSupplier =
                  Topo.AnyProcessor
                    ( suppressWindowedShedProc @k @v
                        bufNm
                        (durationMillis grace)
                        windowSize
                        shelf
                    )
              , Topo.processorSpecStores = [bufNm]
              }
            t
        !t2 = Topo.addStateStoreKV bufBuilder [procNm] t1
    in t2
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = procNm
      , kstreamKeyChanged = kstreamKeyChanged s
      , kstreamKeySerde = kstreamKeySerde s
      , kstreamValueSerde = kstreamValueSerde s
      }


suppressWindowedShedProc
  :: forall k v
   . Ord k
  => StoreName
  -> Int64 -- grace ms
  -> Int64 -- window size ms
  -> DeadLetterShelf k v
  -> IO (Processor (WindowedKey k) v)
suppressWindowedShedProc sn graceMs winMs shelf =
  untilWindowClosesProc "SUPPRESS-SHED" sn graceMs winMs (Just shelf.dlsRecordCap) $ \ctx (wk, _) ->
    -- over the cap: the oldest windows go to the side topic
    \bv ->
      ctxEmitToTopic
        ctx
        Kafka.Streams.Processor.SinkEmit
          { Kafka.Streams.Processor.seTopic = unTopicName shelf.dlsTopic
          , Kafka.Streams.Processor.seKey = Just (serialize shelf.dlsKeySerde wk)
          , Kafka.Streams.Processor.seValue = serialize shelf.dlsValueSerde bv.bValue
          , Kafka.Streams.Processor.seTimestamp = let WindowedKey _ wstart = wk in wstart
          , Kafka.Streams.Processor.seHeaders = emptyHeaders
          }


suppressWindowedProc
  :: forall k v
   . Ord k
  => StoreName
  -> Int64 -- grace ms
  -> Int64 -- window size ms
  -> BufferConfig
  -> IO (Processor (WindowedKey k) v)
suppressWindowedProc sn graceMs winMs bufCfg =
  untilWindowClosesProc "SUPPRESS" sn graceMs winMs (bufferConfigCap bufCfg) $ \ctx (wk, n) bv ->
    case bufCfg.overflow of
      ShutdownWhenFull -> throwIO (SuppressBufferFullException sn (maybe 0 id (bufferConfigCap bufCfg)) n)
      EmitEarlyWhenFull -> forwardRecord ctx (Record (Just wk) bv.bValue (Timestamp bv.bTs) emptyHeaders)
      DropOldestSilently -> pure ()


{- | @suppress(untilWindowCloses)@: each window's latest value waits in
the buffer until stream time reaches its end plus the grace period
(when its aggregation stops taking updates), then goes downstream at
the timestamp of its last update, oldest window first. Over the cap,
@overCap@ decides what becomes of the oldest windows (it is handed
the window and the buffer's size).
-}
untilWindowClosesProc
  :: forall k v
   . Ord k
  => T.Text
  -> StoreName
  -> Int64 -- grace ms
  -> Int64 -- window size ms
  -> Maybe Int
  -> (ProcessorContext -> (WindowedKey k, Int) -> Buffered v -> IO ())
  -> IO (Processor (WindowedKey k) v)
untilWindowClosesProc nm sn graceMs winMs cap overCap = do
  ref <- newIORef Nothing
  let flushDue ctx tb = do
        Timestamp now <- effectiveTime ctx
        tbEvictWhile tb (\end -> end + graceMs <= now) $ \(wk, bv) ->
          forwardRecord ctx (Record (Just wk) bv.bValue (Timestamp bv.bTs) emptyHeaders)
      enforce ctx tb = forM_ cap $ \c -> do
        n <- tbSize tb
        when (n > c) $ tbEvictOldest tb (n - c) $ \(wk, bv) -> overCap ctx (wk, n) bv
  pure
    Processor
      { procName = processorName nm
      , procInit = \ctx -> do
          tb <-
            getStateStore ctx sn >>= \case
              Just (AnyKeyValueStore kvs) -> newTimeBuffer (Unsafe.unsafeCoerce kvs)
              _ -> error $ "suppress: buffer store missing: " <> show sn
          writeIORef ref (Just (ctx, tb))
          -- like the JVM's suppress, flush on stream time alone, so a
          -- window closes even when its key sees no more input
          _ <- schedule ctx 1 StreamTimePunctuation (Punctuator (\_ -> flushDue ctx tb))
          pure ()
      , procClose = pure ()
      , procProcess = \r -> do
          m <- readIORef ref
          case (m, recordKey r) of
            (Just (ctx, tb), Just wk@(WindowedKey _ (Timestamp wstart))) -> do
              let Timestamp ts = recordTimestamp r
              -- buffered by the window's end
              tbPut tb wk (\_ -> Buffered (wstart + winMs) ts (recordValue r))
              flushDue ctx tb
              enforce ctx tb
            _ -> pure ()
      }


{- | Effective record cap for a 'BufferConfig'. We treat
'bufMaxBytes' and 'bufMaxRecords' as the same approximate
record-count limit; the user gets soft enforcement either
way.
-}
bufferConfigCap :: BufferConfig -> Maybe Int
bufferConfigCap b = case (b.maxRecords, b.maxBytes) of
  (Just n, _) -> Just n
  (_, Just n) -> Just n
  _ -> Nothing


----------------------------------------------------------------------
-- Suppress (until-time-limit)
----------------------------------------------------------------------

{- | Per-key debounce: at most one emission per key per
@timeLimit@. Updates received within the limit overwrite the
buffered value; the buffered value flushes at the next time-limit
boundary or when stream time has advanced past
@lastFlush + timeLimit@.
-}
suppressUntilTimeLimit
  :: forall k v
   . Ord k
  => Duration
  -> KStream k v
  -> IO (KStream k v)
suppressUntilTimeLimit timeLimit s = do
  let b = kstreamBuilder s
  bufNm <- freshStoreName b "SUPPRESS-DEBOUNCE-BUFFER"
  procNm <- freshNodeName b "SUPPRESS-DEBOUNCE"
  let bufBuilder =
        withStoreSerdesKV
          (kstreamKeySerde s)
          (bufferedSerde (kstreamValueSerde s))
          (inMemoryKeyValueStoreBuilder bufNm)
          :: StoreBuilderKV k (Buffered v)
  withTopology_ b $ \t ->
    let !t1 =
          Topo.addProcessorWith
            Topo.ProcessorSpec
              { Topo.processorSpecName = procNm
              , Topo.processorSpecParents = [kstreamParent s]
              , Topo.processorSpecSupplier =
                  Topo.AnyProcessor
                    ( suppressTimeLimitProc @k @v
                        bufNm
                        (durationMillis timeLimit)
                    )
              , Topo.processorSpecStores = [bufNm]
              }
            t
        !t2 = Topo.addStateStoreKV bufBuilder [procNm] t1
    in t2
  pure
    KStream
      { kstreamBuilder = b
      , kstreamParent = procNm
      , kstreamKeyChanged = kstreamKeyChanged s
      , kstreamKeySerde = kstreamKeySerde s
      , kstreamValueSerde = kstreamValueSerde s
      }


{- | @suppress(untilTimeLimit)@: a key's first buffered record starts
its wait; later ones replace the value (not the wait) until stream
time reaches the start plus @limitMs@, when the latest goes downstream
at its own timestamp (the JVM's time-ordered buffer).
-}
suppressTimeLimitProc
  :: forall k v
   . Ord k
  => StoreName
  -> Int64
  -> IO (Processor k v)
suppressTimeLimitProc sn limitMs = do
  ref <- newIORef Nothing
  let flushDue ctx tb = do
        Timestamp now <- effectiveTime ctx
        tbEvictWhile tb (\first -> first + limitMs <= now) $ \(k, bv) ->
          forwardRecord ctx (Record (Just k) bv.bValue (Timestamp bv.bTs) emptyHeaders)
  pure
    Processor
      { procName = processorName "SUPPRESS-DEBOUNCE"
      , procInit = \ctx -> do
          tb <-
            getStateStore ctx sn >>= \case
              Just (AnyKeyValueStore kvs) -> newTimeBuffer (Unsafe.unsafeCoerce kvs)
              _ -> error $ "suppress-debounce: buffer missing: " <> show sn
          writeIORef ref (Just (ctx, tb))
          _ <- schedule ctx 1 StreamTimePunctuation (Punctuator (\_ -> flushDue ctx tb))
          pure ()
      , procClose = pure ()
      , procProcess = \r -> do
          m <- readIORef ref
          case (m, recordKey r) of
            (Just (ctx, tb), Just k) -> do
              let Timestamp ts = recordTimestamp r
              tbPut tb k (\prev -> Buffered (maybe ts (.bTime) prev) ts (recordValue r))
              flushDue ctx tb
            _ -> pure ()
      }


----------------------------------------------------------------------
-- The time-ordered buffer
----------------------------------------------------------------------

-- | A buffered value: when it is due from, the timestamp it goes out at, and the value.
data Buffered v = Buffered
  { bTime :: {-# UNPACK #-} !Int64
  , bTs :: {-# UNPACK #-} !Int64
  , bValue :: !v
  }


{- | A suppress buffer: the store (logged, so it survives a restart)
and an index of its keys by buffer time, built from the store on first
use (after any restore), so that eviction costs the evicted entries,
not a scan of the buffer.
-}
data TimeBuffer k v = TimeBuffer
  { tbStore :: !(KeyValueStore k (Buffered v))
  , tbIndex :: !(IORef (Maybe (Set.Set (Int64, k), Map.Map k Int64)))
  }


newTimeBuffer :: KeyValueStore k (Buffered v) -> IO (TimeBuffer k v)
newTimeBuffer kvs = TimeBuffer kvs <$> newIORef Nothing


indexOf :: Ord k => TimeBuffer k v -> IO (Set.Set (Int64, k), Map.Map k Int64)
indexOf tb =
  readIORef tb.tbIndex >>= \case
    Just ix -> pure ix
    Nothing -> do
      rows <- kvsAll tb.tbStore >>= kvIteratorToList
      let !ix = (Set.fromList (map (\(k, bv) -> (bv.bTime, k)) rows), Map.fromList (map (\(k, bv) -> (k, bv.bTime)) rows))
      writeIORef tb.tbIndex (Just ix)
      pure ix


-- | Buffer @k@'s new entry, computed from its current one.
tbPut :: Ord k => TimeBuffer k v -> k -> (Maybe (Buffered v) -> Buffered v) -> IO ()
tbPut tb k f = do
  (order, times) <- indexOf tb
  prev <- kvsGet tb.tbStore k
  let !bv = f prev
  kvsPut tb.tbStore k bv
  let order' = Set.insert (bv.bTime, k) (maybe order (\t -> Set.delete (t, k) order) (Map.lookup k times))
  writeIORef tb.tbIndex (Just (order', Map.insert k bv.bTime times))


tbSize :: Ord k => TimeBuffer k v -> IO Int
tbSize tb = Map.size . snd <$> indexOf tb


-- | Take out, oldest first, the entries whose buffer time satisfies @due@.
tbEvictWhile :: Ord k => TimeBuffer k v -> (Int64 -> Bool) -> ((k, Buffered v) -> IO ()) -> IO ()
tbEvictWhile tb due emit = go
  where
    go = do
      (order, _) <- indexOf tb
      case Set.lookupMin order of
        Just (t, k) | due t -> evictOne tb k emit >> go
        _ -> pure ()


-- | Take out the @n@ oldest entries.
tbEvictOldest :: Ord k => TimeBuffer k v -> Int -> ((k, Buffered v) -> IO ()) -> IO ()
tbEvictOldest tb n emit
  | n <= 0 = pure ()
  | otherwise = do
      (order, _) <- indexOf tb
      forM_ (Set.lookupMin order) $ \(_, k) -> evictOne tb k emit >> tbEvictOldest tb (n - 1) emit


evictOne :: Ord k => TimeBuffer k v -> k -> ((k, Buffered v) -> IO ()) -> IO ()
evictOne tb k emit = do
  (order, times) <- indexOf tb
  mbv <- kvsDelete tb.tbStore k
  writeIORef tb.tbIndex (Just (maybe order (\t -> Set.delete (t, k) order) (Map.lookup k times), Map.delete k times))
  mapM_ (\bv -> emit (k, bv)) mbv


{- | A buffered entry's changelog bytes: its buffer time and timestamp
(8 bytes big-endian each), then the value's.
-}
bufferedSerde :: Serde v -> Serde (Buffered v)
bufferedSerde vs =
  Serde
    { serialize = \(Buffered t ts v) ->
        BSL.toStrict (BB.toLazyByteString (BB.int64BE t <> BB.int64BE ts)) <> serialize vs v
    , deserialize = \bs ->
        if BS.length bs < 16
          then Left "suppress buffer entry shorter than its 16-byte header"
          else
            let (tb, rest) = BS.splitAt 8 bs
                (tsb, vb) = BS.splitAt 8 rest
                be = BS.foldl' (\acc w -> acc * 256 + fromIntegral w) 0
            in Buffered (be tb) (be tsb) <$> deserialize vs vb
    , serializeHeaders = serializeHeaders vs . (.bValue)
    }


----------------------------------------------------------------------
-- Suppressed builder (Java's Suppressed.untilWindowCloses /
-- .untilTimeLimit)
----------------------------------------------------------------------

{- | Mirrors Java's @Suppressed<K>@: a configuration value the
caller hands to 'suppressKStream'.
-}
data Suppressed
  = SuppressUntilWindowCloses
      { suppressGrace :: !Duration
      , suppressWindowSize :: !Int64
      }
  | SuppressUntilTimeLimit
      { suppressLimit :: !Duration
      }


untilWindowCloses :: Duration -> Int64 -> Suppressed
untilWindowCloses g sz = SuppressUntilWindowCloses g sz


untilTimeLimit :: Duration -> Suppressed
untilTimeLimit = SuppressUntilTimeLimit


{- | Apply a 'Suppressed' configuration to a 'KStream'. For
'SuppressUntilWindowCloses' the input must be a stream of
windowed-key records (see 'streamFromWindowedHandle'); for
'SuppressUntilTimeLimit' any keyed stream works.

We expose two type-distinct call sites because the underlying
key shapes differ; this convenience function picks the right
backend based on the 'Suppressed' value.
-}
suppressKStream
  :: forall k v
   . Ord k
  => Suppressed
  -> Kafka.Streams.KStream.KStream k v
  -> IO (Kafka.Streams.KStream.KStream k v)
suppressKStream s =
  case s of
    SuppressUntilWindowCloses {} ->
      error
        "suppressKStream: untilWindowCloses requires a windowed-key \
        \stream; use 'suppressWindowed' directly."
    SuppressUntilTimeLimit {suppressLimit = lim} ->
      suppressUntilTimeLimit lim


----------------------------------------------------------------------
-- High-level: KTable.suppress on a windowed aggregation
----------------------------------------------------------------------

{- | Convenience: take a 'WindowedTableHandle' from a windowed
aggregation and apply 'suppressWindowed' to its change stream.
Mirrors @KTable.suppress(Suppressed.untilWindowCloses(...))@.
-}
suppressWindowedHandle
  :: forall k v
   . Ord k
  => Duration -- grace
  -> Int64 -- window size
  -> Serde k -- inner key serde
  -> Serde v
  -> WindowedTableHandle k v
  -> IO (KStream (WindowedKey k) v)
suppressWindowedHandle grace winMs ks vs h = do
  -- Always tap the handle as an emit-on-update stream here.
  -- Routing through 'streamFromWindowedHandle' would loop for
  -- 'OnWindowClose' handles, since that branch delegates back
  -- to this same function.
  s <- doStreamFromWindowedHandle h ks vs
  suppressWindowed grace winMs s


{- | A 'SWKS.SessionWindowedTableHandle' as the stream of its updates,
keyed by session: each session's new aggregate, and 'Nothing' for a
session merged into a larger one (the JVM's
@SessionWindowedKStream...toStream()@, nulls included).
-}
streamFromSessionWindowedHandle
  :: forall k v
   . SWKS.SessionWindowedTableHandle k v
  -> Serde k
  -- ^ key serde for downstream
  -> Serde v
  -- ^ value serde for downstream
  -> IO (KStream (SessionKey k) (Maybe v))
streamFromSessionWindowedHandle h kserde vserde =
  pure
    KStream
      { kstreamBuilder = SWKS.swthBuilder h
      , kstreamParent = SWKS.swthNode h
      , kstreamKeyChanged = False
      , kstreamKeySerde = sessionKeySerde kserde
      , -- like 'Kafka.Streams.KStream.toChangelogStream': nullable values go out through toTopicNullable
        kstreamValueSerde = const (error "streamFromSessionWindowedHandle: write it with toTopicNullable (its values are Maybe v)") vserde
      }


----------------------------------------------------------------------
-- BufferConfig (KIP-328)
----------------------------------------------------------------------

{- | Buffer-size advisory for 'Suppressed.untilWindowCloses'.
'unboundedBufferConfig' is the only mode the suppress
processor currently /enforces/ at runtime, but the bounded
variants accept the same JVM Suppressed.BufferConfig
vocabulary so callers can author topologies declaratively.
-}
data BufferConfig = BufferConfig
  { maxBytes :: !(Maybe Int)
  , maxRecords :: !(Maybe Int)
  , overflow :: !BufferOverflowPolicy
  }
  deriving (Eq, Show)


{- | What to do when a bounded suppress buffer overflows. Mirrors
Java's 'BufferConfig.shutDownWhenFull' /
'emitEarlyWhenFull' (KIP-328) and extends with the
Riffle-spec @drop@ policy ('DropOldestSilently'). The @shed@
policy is exposed separately via 'suppressWindowedShed'
because it carries typed-serde + topic config that doesn't fit
a plain enum.
-}
data BufferOverflowPolicy
  = {- | JVM default for bounded buffers: tear the stream down
    when the budget is exceeded ('Fail' in the Riffle spec).
    -}
    ShutdownWhenFull
  | {- | Emit the buffered windows downstream even though grace
    hasn't elapsed yet, freeing space for new entries.
    Trades correctness (downstream sees premature emissions)
    for liveness.
    -}
    EmitEarlyWhenFull
  | {- | Discard the oldest over-cap entries without emitting them.
    Trades correctness (downstream never sees the dropped
    records) for both memory bound and liveness. The Riffle
    spec's @drop@ policy.
    -}
    DropOldestSilently
  deriving (Eq, Show)


unboundedBufferConfig :: BufferConfig
unboundedBufferConfig = BufferConfig Nothing Nothing EmitEarlyWhenFull


maxBytesBufferConfig :: Int -> BufferConfig
maxBytesBufferConfig n = BufferConfig (Just n) Nothing ShutdownWhenFull


maxRecordsBufferConfig :: Int -> BufferConfig
maxRecordsBufferConfig n = BufferConfig Nothing (Just n) ShutdownWhenFull


{- | Set the overflow policy on a 'BufferConfig'. Mirrors
Java's @BufferConfig.shutDownWhenFull()@ /
@emitEarlyWhenFull()@.
-}
shutDownWhenFull :: BufferConfig -> BufferConfig
shutDownWhenFull b = b {overflow = ShutdownWhenFull}


emitEarlyWhenFull :: BufferConfig -> BufferConfig
emitEarlyWhenFull b = b {overflow = EmitEarlyWhenFull}


-- | Set the overflow policy to 'DropOldestSilently'.
dropOldestSilently :: BufferConfig -> BufferConfig
dropOldestSilently b = b {overflow = DropOldestSilently}
