{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.Suppress
-- Description : Suppress operator
--
-- Buffers updates per (windowed) key and emits each key's final
-- value only after the window has closed (i.e. stream time has
-- advanced past @windowEnd + gracePeriod@). Mirrors
-- @KTable.suppress(Suppressed.untilWindowCloses(...))@ over a
-- windowed KTable.
--
-- == Design
--
-- The suppress processor is wired as an additional node downstream
-- of a windowed aggregation. It holds a 'KeyValueStore' of buffered
-- records keyed by 'WindowedKey'. On every input record:
--
--   1. The buffer is updated with the latest value for that
--      windowed key.
--   2. The processor iterates the buffer and forwards (then removes)
--      every entry whose window has closed.
--
-- The driver advances stream time as records arrive; the
-- 'TopologyTestDriver' user can additionally call
-- 'advanceDriverStreamTime' to trigger emissions without sending
-- records.
--
-- For non-windowed KTables, 'suppressUntilTimeLimit' offers a
-- coarser per-key debounce: emit at most one update per key per
-- time-limit window.
module Kafka.Streams.Suppress
  ( suppressWindowed
  , suppressWindowedWith
  , suppressUntilTimeLimit
  , streamFromWindowedHandle
  , streamFromSessionWindowedHandle
  , suppressWindowedHandle
  , SuppressBufferFullException (..)
    -- * Suppressed builder
  , Suppressed (..)
  , untilWindowCloses
  , untilTimeLimit
  , suppressKStream
    -- * Buffer config
  , BufferConfig (..)
  , BufferOverflowPolicy (..)
  , unboundedBufferConfig
  , maxBytesBufferConfig
  , maxRecordsBufferConfig
  , shutDownWhenFull
  , emitEarlyWhenFull
  , dropOldestSilently
    -- * Dead-letter shelf (KIP-1033-style overflow routing)
  --
  -- Routes the oldest over-cap entries to a side topic instead of
  -- throwing or emitting them early. The Riffle spec calls this
  -- the @shed@ policy alongside @drop@ / @fail@ /
  -- @spill-to-durable-state@; @drop@ ships as 'DropOldestSilently',
  -- @fail@ as 'ShutdownWhenFull', and @shed@ as the dedicated
  -- 'suppressWindowedShed' operator below. The spill policy is
  -- deferred until snapshot-aware state backends land.
  , DeadLetterShelf (..)
  , suppressWindowedShed
  ) where

import Data.IORef
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Unsafe.Coerce as Unsafe

import qualified Kafka.Streams.KStream
import Kafka.Streams.KStream (KStream (..))
import Kafka.Streams.StreamsBuilder
  ( StreamsBuilder
  , freshNodeName
  , freshStoreName
  , withTopology_
  )
import Control.Exception (Exception, throwIO)
import qualified Data.List as List
import GHC.Generics (Generic)

import Kafka.Streams.Time (millis)
import Kafka.Streams.Window (windowsGracePeriod, windowsSize)
import Kafka.Streams.TimeWindowedKStream
  ( EmitStrategy (..)
  , WindowedTableHandle (..)
  )
import qualified Kafka.Streams.SessionWindowedKStream as SWKS
import qualified Kafka.Streams.Processor
import Kafka.Streams.Processor
  ( Processor (..)
  , ProcessorContext (..)
  , PunctuationType (..)
  , Punctuator (..)
  , effectiveTime
  , forwardRecord
  , getStateStore
  , processorName
  , schedule
  )
import Kafka.Streams.Serde (Serde, serialize)
import Kafka.Streams.State.KeyValue.InMemory
  ( inMemoryKeyValueStoreBuilder
  )
import Kafka.Streams.State.Store
  ( AnyStateStore (..)
  , KeyValueStore (..)
  , StoreBuilderKV
  , StoreName
  , WindowStore (..)
  , WindowedKey (..)
  , kvIteratorToList
  )
import Kafka.Streams.Time (Duration, Timestamp (..), durationMillis)
import qualified Kafka.Streams.Topology as Topo
import Kafka.Streams.Types (Record (..), TopicName, emptyHeaders, unTopicName)

----------------------------------------------------------------------
-- WindowedTableHandle -> KStream
----------------------------------------------------------------------

-- | Tap a 'WindowedTableHandle' as a 'KStream' of (windowed-key,
-- value) records. Each downstream record corresponds to one update
-- of the underlying window store.
streamFromWindowedHandle
  :: forall k v
   . (Ord k, Eq v)
  => WindowedTableHandle k v
  -> Serde k                          -- ^ key serde for downstream
  -> Serde v                          -- ^ value serde for downstream
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

-- | Original behaviour — emit on every update to the window
-- store. Kept as a separate helper so the
-- 'streamFromWindowedHandle' dispatch above can route by
-- 'wthEmit'.
doStreamFromWindowedHandle
  :: forall k v
   . (Ord k, Eq v)
  => WindowedTableHandle k v
  -> Serde k
  -> Serde v
  -> IO (KStream (WindowedKey k) v)
doStreamFromWindowedHandle h kserde vserde = do
  let b = wthBuilder h
  -- The handle's @wthNode@ is the aggregation processor itself,
  -- which forwards the latest aggregate /value/ each time. It
  -- doesn't forward the windowed key in our model — the windowed
  -- key only lives in the store. To produce a (WindowedKey k, v)
  -- stream we attach a small adapter processor that reads the
  -- store and emits the most recent (key, value) on every input.
  --
  -- This is admittedly not as efficient as Java's "out-of-band
  -- emitter" but it's a faithful single-task driver model.
  nm <- freshNodeName b "WINDOWED-AS-STREAM"
  withTopology_ b $
    Topo.addProcessorWith
      Topo.ProcessorSpec
        { Topo.processorSpecName     = nm
        , Topo.processorSpecParents  = [wthNode h]
        , Topo.processorSpecSupplier =
            Topo.AnyProcessor (windowedAsStreamProc @k @v (wthStore h))
        , Topo.processorSpecStores   = [wthStore h]
        }
  -- 'kserde' is retained only conceptually; downstream sinks will
  -- typically supply a WindowedKey-aware composite serde via
  -- 'Produced' rather than relying on the KStream-carried one,
  -- hence the explicit error placeholder.
  let _ = kserde
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = nm
    , kstreamKeySerde   = error
        "streamFromWindowedHandle: WindowedKey serde unset; supply one to to/through"
    , kstreamValueSerde = vserde
    }

windowedAsStreamProc
  :: forall (k :: *) (v :: *) (origIn :: *)
   . (Ord k, Eq v)
  => StoreName
  -> IO (Processor k origIn)
windowedAsStreamProc sn = do
  ctxRef   <- newIORef Nothing
  storeRef <- newIORef (Nothing :: Maybe (WindowStore k v))
  -- Tracks the value most recently forwarded for each
  -- @(k, windowStart)@ pair so we never re-emit unchanged
  -- entries when the windowed aggregator forwards through us
  -- for an update to a /different/ window of the same key.
  lastFwdRef <- newIORef (Map.empty :: Map.Map (k, Timestamp) v)
  pure Processor
    { procName = processorName "WINDOWED-AS-STREAM"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        getStateStore ctx sn >>= \case
          Just (AnyWindowStore ws) ->
            writeIORef storeRef (Just (Unsafe.unsafeCoerce ws))
          _ -> error "windowedAsStreamProc: store missing"
    , procClose = pure ()
    , procProcess = \r -> do
        mctx <- readIORef ctxRef
        mst  <- readIORef storeRef
        case (mctx, mst, recordKey r) of
          (Just ctx, Just ws, Just k) -> do
            it <- wsFetchRange ws k (Timestamp minBound) (Timestamp maxBound)
            entries <- kvIteratorToList it
            mapM_ (\(ts, v) -> do
                     prior <- Map.lookup (k, ts) <$> readIORef lastFwdRef
                     case prior of
                       Just v' | v' == v -> pure ()
                       _ -> do
                         modifyIORef' lastFwdRef (Map.insert (k, ts) v)
                         forwardRecord ctx
                           (Record (Just (WindowedKey k ts)) v
                             (recordTimestamp r) (recordHeaders r)
                             :: Record (WindowedKey k) v))
                  entries
          _ -> pure ()
    }

----------------------------------------------------------------------
-- Suppress (windowed)
----------------------------------------------------------------------

-- | Suppress all updates to a windowed KTable until the window has
-- closed (i.e. stream time has advanced past @windowEnd +
-- gracePeriod@). Each window emits exactly once with its final
-- value.
--
-- The window length is taken from the supplied 'WindowedTableHandle'.
-- The @gracePeriod@ is added to the window end before flushing, so
-- late records arriving within the grace can still update the
-- buffered value.
suppressWindowed
  :: forall k v
   . Ord k
  => Duration                              -- ^ grace period
  -> Int64                                 -- ^ window size (ms)
  -> KStream (WindowedKey k) v
  -> IO (KStream (WindowedKey k) v)
suppressWindowed grace windowSize =
  suppressWindowedWith grace windowSize unboundedBufferConfig

-- | Bounded @suppress(untilWindowCloses(...))@. Like
-- 'suppressWindowed' but enforces the supplied
-- 'BufferConfig': when the buffer exceeds the configured
-- record / byte limit the runtime either emits the oldest
-- buffered windows early ('EmitEarlyWhenFull') or throws
-- 'SuppressBufferFullException' which propagates to the
-- KIP-1033 / 671 handlers ('ShutdownWhenFull').
--
-- Byte counting is approximate (per-record, not per-byte)
-- because the processor doesn't see the serialised value;
-- 'bufMaxBytes' and 'bufMaxRecords' are treated as the same
-- soft cap. The cap is a count-of-buffered-windowed-keys.
suppressWindowedWith
  :: forall k v
   . Ord k
  => Duration                              -- ^ grace period
  -> Int64                                 -- ^ window size (ms)
  -> BufferConfig
  -> KStream (WindowedKey k) v
  -> IO (KStream (WindowedKey k) v)
suppressWindowedWith grace windowSize bufCfg s = do
  let b = kstreamBuilder s
  bufNm <- freshStoreName b "SUPPRESS-BUFFER"
  procNm <- freshNodeName b "SUPPRESS"
  let bufBuilder = inMemoryKeyValueStoreBuilder bufNm
                     :: StoreBuilderKV (WindowedKey k) v
  withTopology_ b $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = procNm
                  , Topo.processorSpecParents  = [kstreamParent s]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor
                        (suppressWindowedProc @k @v
                           bufNm (durationMillis grace) windowSize bufCfg)
                  , Topo.processorSpecStores   = [bufNm]
                  } t
        !t2 = Topo.addStateStoreKV bufBuilder [procNm] t1
     in t2
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = procNm
    , kstreamKeySerde   = kstreamKeySerde s
    , kstreamValueSerde = kstreamValueSerde s
    }

-- | Thrown when a 'ShutdownWhenFull' suppress buffer
-- overflows. Caught by the streams runtime's KIP-1033 /
-- KIP-671 handler stack and routed like any other processing
-- exception.
data SuppressBufferFullException = SuppressBufferFullException
  { store :: !StoreName
  , cap   :: !Int
  , at    :: !Int
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

-- | Dead-letter routing for over-cap suppress entries. The cap +
-- topic + serdes are bundled because they must agree on the wire
-- type of the side stream.
data DeadLetterShelf k v = DeadLetterShelf
  { dlsTopic       :: !TopicName
    -- ^ Side topic that receives the shed records.
  , dlsKeySerde    :: !(Serde (WindowedKey k))
    -- ^ Serde used to encode the windowed key on the wire.
  , dlsValueSerde  :: !(Serde v)
    -- ^ Serde used to encode the buffered value on the wire.
  , dlsRecordCap   :: !Int
    -- ^ Record budget. When the buffer holds more than this, the
    --   oldest entries (by 'WindowedKey' start timestamp) are
    --   shed in order until the buffer is back under the cap.
  }

-- | Bounded @suppressUntilWindowCloses@ with a /dead-letter
-- shelf/. When the buffer exceeds 'dlsRecordCap', the oldest
-- entries are serialised through 'dlsKeySerde' /
-- 'dlsValueSerde' and routed to 'dlsTopic' via the engine's
-- record collector. Downstream of this operator never sees the
-- shed records.
--
-- @
-- shed <- DeadLetterShelf
--   { dlsTopic       = topicName \"suppress-dlq\"
--   , dlsKeySerde    = windowedSerde textSerde
--   , dlsValueSerde  = int64Serde
--   , dlsRecordCap   = 1024
--   }
-- supressedStream <-
--   suppressWindowedShed (millis 5_000) 60_000 shed inputStream
-- @
--
-- 'suppressWindowedShed' is the typed analogue of the @shed@
-- entry in the Riffle bounded-buffer policy enumeration. The
-- other three policies (@fail@ \/ @drop@ \/ @emit-early@) are
-- carried via 'BufferOverflowPolicy' on 'suppressWindowedWith'.
suppressWindowedShed
  :: forall k v
   . Ord k
  => Duration                              -- ^ grace period
  -> Int64                                 -- ^ window size (ms)
  -> DeadLetterShelf k v
  -> KStream (WindowedKey k) v
  -> IO (KStream (WindowedKey k) v)
suppressWindowedShed grace windowSize shelf s = do
  let b = kstreamBuilder s
  bufNm <- freshStoreName b "SUPPRESS-SHED-BUFFER"
  procNm <- freshNodeName b "SUPPRESS-SHED"
  let bufBuilder = inMemoryKeyValueStoreBuilder bufNm
                     :: StoreBuilderKV (WindowedKey k) v
  withTopology_ b $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = procNm
                  , Topo.processorSpecParents  = [kstreamParent s]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor
                        (suppressWindowedShedProc @k @v
                           bufNm (durationMillis grace) windowSize shelf)
                  , Topo.processorSpecStores   = [bufNm]
                  } t
        !t2 = Topo.addStateStoreKV bufBuilder [procNm] t1
     in t2
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = procNm
    , kstreamKeySerde   = kstreamKeySerde s
    , kstreamValueSerde = kstreamValueSerde s
    }

suppressWindowedShedProc
  :: forall k v
   . Ord k
  => StoreName
  -> Int64                                 -- grace ms
  -> Int64                                 -- window size ms
  -> DeadLetterShelf k v
  -> IO (Processor (WindowedKey k) v)
suppressWindowedShedProc sn graceMs winMs shelf = do
  ctxRef  <- newIORef Nothing
  bufRef  <- newIORef (Nothing :: Maybe (KeyValueStore (WindowedKey k) v))
  sizeRef <- newIORef (0 :: Int)
  let !cap_ = shelf.dlsRecordCap
  pure Processor
    { procName = processorName "SUPPRESS-SHED"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        getStateStore ctx sn >>= \case
          Just (AnyKeyValueStore kvs) ->
            writeIORef bufRef (Just (Unsafe.unsafeCoerce kvs))
          _ -> error $ "suppress-shed: buffer store missing: " <> show sn
        -- Same flush-on-stream-time-advance contract as
        -- 'suppressWindowedProc'; see comment there.
        _ <- schedule ctx 1 StreamTimePunctuation
               (Punctuator $ \_ -> do
                  mbuf' <- readIORef bufRef
                  case mbuf' of
                    Just buf_ -> flushDueShed ctx buf_ sizeRef
                    Nothing   -> pure ())
        pure ()
    , procClose = pure ()
    , procProcess = \r -> case recordKey r of
        Nothing -> pure ()
        Just wk -> do
          mctx <- readIORef ctxRef
          mbuf <- readIORef bufRef
          case (mctx, mbuf) of
            (Just ctx, Just buf_) -> do
              existing <- kvsGet buf_ wk
              kvsPut buf_ wk (recordValue r)
              case existing of
                Just _  -> pure ()
                Nothing -> modifyIORef' sizeRef (+ 1)
              flushDueShed ctx buf_ sizeRef
              enforceShed ctx buf_ sizeRef cap_
            _ -> pure ()
    }
  where
    flushDueShed ctx buf_ sizeRef = do
      Timestamp now <- effectiveTime ctx
      it <- kvsAll buf_
      entries <- kvIteratorToList it
      mapM_
        (\(wk@(WindowedKey _ (Timestamp wstart)), v) -> do
            let !winEnd = wstart + winMs
            if now > winEnd + graceMs
              then do
                _ <- kvsDelete buf_ wk
                modifyIORef' sizeRef (\n -> max 0 (n - 1))
                forwardRecord ctx
                  (Record (Just wk) v (Timestamp winEnd) emptyHeaders
                    :: Record (WindowedKey k) v)
              else pure ())
        entries
    enforceShed ctx buf_ sizeRef cap = do
      cur <- readIORef sizeRef
      if cur <= cap
        then pure ()
        else do
          it <- kvsAll buf_
          entries <- kvIteratorToList it
          let !ordered =
                List.sortOn (\(WindowedKey _ ts, _) -> ts) entries
          shedUntil ctx buf_ sizeRef cap ordered
    shedUntil _ _ _ _ [] = pure ()
    shedUntil ctx buf_ sizeRef cap ((wk, v) : rest) = do
      cur <- readIORef sizeRef
      if cur <= cap
        then pure ()
        else do
          let !keyBytes = Just (serialize shelf.dlsKeySerde wk)
              !valBytes = serialize shelf.dlsValueSerde v
              !ts       = let WindowedKey _ wstart = wk in wstart
          _ <- kvsDelete buf_ wk
          modifyIORef' sizeRef (\n -> max 0 (n - 1))
          ctxEmitToTopic ctx
            (Kafka.Streams.Processor.SinkEmit
              { Kafka.Streams.Processor.seTopic     = unTopicName shelf.dlsTopic
              , Kafka.Streams.Processor.seKey       = keyBytes
              , Kafka.Streams.Processor.seValue     = valBytes
              , Kafka.Streams.Processor.seTimestamp = ts
              })
          shedUntil ctx buf_ sizeRef cap rest

suppressWindowedProc
  :: forall k v
   . Ord k
  => StoreName
  -> Int64                                 -- grace ms
  -> Int64                                 -- window size ms
  -> BufferConfig
  -> IO (Processor (WindowedKey k) v)
suppressWindowedProc sn graceMs winMs bufCfg = do
  ctxRef <- newIORef Nothing
  bufRef <- newIORef (Nothing :: Maybe (KeyValueStore (WindowedKey k) v))
  -- Live count of records currently buffered. Bumped on
  -- 'kvsPut', decremented on each flush. Used to enforce the
  -- BufferConfig cap.
  sizeRef <- newIORef (0 :: Int)
  let !cap_ = bufferConfigCap bufCfg
      !policy = bufCfg.overflow
  pure Processor
    { procName = processorName "SUPPRESS"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        getStateStore ctx sn >>= \case
          Just (AnyKeyValueStore kvs) ->
            writeIORef bufRef (Just (Unsafe.unsafeCoerce kvs))
          _ -> error $ "suppress: buffer store missing: " <> show sn
        -- Mirror Java's @Suppressed@: the suppress operator
        -- registers a stream-time punctuator so windows whose
        -- close time has been crossed get flushed even when no
        -- new input arrives for the affected key. We fire on
        -- every stream-time advance (@interval = 1@) because the
        -- punctuator framework only fires once per advance, so
        -- a larger interval would let advances slip past closed
        -- windows in between fires.
        let !punInterval = 1 :: Int
        _ <- schedule ctx punInterval StreamTimePunctuation
               (Punctuator $ \_ -> do
                  mbuf' <- readIORef bufRef
                  case mbuf' of
                    Just buf_ -> flushDue ctx buf_ sizeRef
                    Nothing   -> pure ())
        pure ()
    , procClose = pure ()
    , procProcess = \r -> case recordKey r of
        Nothing -> pure ()
        Just wk -> do
          mctx <- readIORef ctxRef
          mbuf <- readIORef bufRef
          case (mctx, mbuf) of
            (Just ctx, Just buf_) -> do
              -- If the key is already in the buffer the put
              -- is an in-place update (no size growth).
              existing <- kvsGet buf_ wk
              kvsPut buf_ wk (recordValue r)
              case existing of
                Just _  -> pure ()
                Nothing -> modifyIORef' sizeRef (+ 1)
              -- Run the normal due-window flush first so
              -- naturally-due windows leave before we have to
              -- consider overflow.
              flushDue ctx buf_ sizeRef
              -- Now enforce the cap, if any.
              enforceCap ctx buf_ sizeRef cap_ policy
            _ -> pure ()
    }
  where
    flushDue ctx buf_ sizeRef = do
      Timestamp now <- effectiveTime ctx
      it <- kvsAll buf_
      entries <- kvIteratorToList it
      mapM_
        (\(wk@(WindowedKey _ (Timestamp wstart)), v) -> do
            let !winEnd = wstart + winMs
            if now > winEnd + graceMs
              then do
                _ <- kvsDelete buf_ wk
                modifyIORef' sizeRef (\n -> max 0 (n - 1))
                forwardRecord ctx
                  (Record (Just wk) v (Timestamp winEnd) emptyHeaders
                    :: Record (WindowedKey k) v)
              else pure ())
        entries
    enforceCap _ _ _ Nothing _ = pure ()
    enforceCap ctx buf_ sizeRef (Just cap) policy = do
      cur <- readIORef sizeRef
      if cur <= cap
        then pure ()
        else case policy of
          ShutdownWhenFull ->
            throwIO (SuppressBufferFullException sn cap cur)
          EmitEarlyWhenFull -> do
            ordered <- orderedEntries buf_
            evictUntil EvictEmit ctx buf_ sizeRef cap ordered
          DropOldestSilently -> do
            ordered <- orderedEntries buf_
            evictUntil EvictDrop ctx buf_ sizeRef cap ordered
    -- Eviction mode: should the evicted record be forwarded
    -- downstream, or silently dropped?
    --
    -- 'EmitEarlyWhenFull' uses 'EvictEmit'; 'DropOldestSilently'
    -- uses 'EvictDrop'. The shed-to-DLQ operator
    -- ('suppressWindowedShed') has its own variant in
    -- 'suppressWindowedShedProc'.
    orderedEntries buf_ = do
      it <- kvsAll buf_
      entries <- kvIteratorToList it
      pure (List.sortOn (\(WindowedKey _ ts, _) -> ts) entries)
    evictUntil _ _ _ _ _ [] = pure ()
    evictUntil mode ctx buf_ sizeRef cap
               ((wk@(WindowedKey _ (Timestamp wstart)), v) : rest) = do
      cur <- readIORef sizeRef
      if cur <= cap
        then pure ()
        else do
          let !winEnd = wstart + winMs
          _ <- kvsDelete buf_ wk
          modifyIORef' sizeRef (\n -> max 0 (n - 1))
          case mode of
            EvictEmit ->
              forwardRecord ctx
                (Record (Just wk) v (Timestamp winEnd) emptyHeaders
                  :: Record (WindowedKey k) v)
            EvictDrop -> pure ()
          evictUntil mode ctx buf_ sizeRef cap rest

-- | Per-call eviction mode for the 'enforceCap' helper.
data EvictMode = EvictEmit | EvictDrop

-- | Effective record cap for a 'BufferConfig'. We treat
-- 'bufMaxBytes' and 'bufMaxRecords' as the same approximate
-- record-count limit; the user gets soft enforcement either
-- way.
bufferConfigCap :: BufferConfig -> Maybe Int
bufferConfigCap b = case (b.maxRecords, b.maxBytes) of
  (Just n, _)      -> Just n
  (_,      Just n) -> Just n
  _                -> Nothing

----------------------------------------------------------------------
-- Suppress (until-time-limit)
----------------------------------------------------------------------

-- | Per-key debounce: at most one emission per key per
-- @timeLimit@. Updates received within the limit overwrite the
-- buffered value; the buffered value flushes at the next time-limit
-- boundary or when stream time has advanced past
-- @lastFlush + timeLimit@.
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
  let bufBuilder = inMemoryKeyValueStoreBuilder bufNm
                     :: StoreBuilderKV k (Int64, v)
  withTopology_ b $ \t ->
    let !t1 = Topo.addProcessorWith
                Topo.ProcessorSpec
                  { Topo.processorSpecName     = procNm
                  , Topo.processorSpecParents  = [kstreamParent s]
                  , Topo.processorSpecSupplier =
                      Topo.AnyProcessor
                        (suppressTimeLimitProc @k @v
                           bufNm (durationMillis timeLimit))
                  , Topo.processorSpecStores   = [bufNm]
                  } t
        !t2 = Topo.addStateStoreKV bufBuilder [procNm] t1
     in t2
  pure KStream
    { kstreamBuilder    = b
    , kstreamParent     = procNm
    , kstreamKeySerde   = kstreamKeySerde s
    , kstreamValueSerde = kstreamValueSerde s
    }

suppressTimeLimitProc
  :: forall k v
   . Ord k
  => StoreName
  -> Int64
  -> IO (Processor k v)
suppressTimeLimitProc sn limitMs = do
  ctxRef <- newIORef Nothing
  bufRef <- newIORef (Nothing :: Maybe (KeyValueStore k (Int64, v)))
  pure Processor
    { procName = processorName "SUPPRESS-DEBOUNCE"
    , procInit = \ctx -> do
        writeIORef ctxRef (Just ctx)
        getStateStore ctx sn >>= \case
          Just (AnyKeyValueStore kvs) ->
            writeIORef bufRef (Just (Unsafe.unsafeCoerce kvs))
          _ -> error $ "suppress-debounce: buffer missing: " <> show sn
        -- Same flush-on-stream-time-advance contract as
        -- 'suppressWindowedProc'; without it, a debounced key
        -- whose source goes silent for longer than 'limitMs'
        -- would sit in the buffer forever even though the
        -- runtime's clock has crossed @firstSeen + limitMs@.
        _ <- schedule ctx 1 StreamTimePunctuation
               (Punctuator $ \_ -> do
                  mbuf' <- readIORef bufRef
                  case mbuf' of
                    Just buf_ -> do
                      Timestamp now <- effectiveTime ctx
                      flushExpired ctx buf_ now
                    Nothing   -> pure ())
        pure ()
    , procClose = pure ()
    , procProcess = \r -> case recordKey r of
        Nothing -> pure ()
        Just k -> do
          mctx <- readIORef ctxRef
          mbuf <- readIORef bufRef
          case (mctx, mbuf) of
            (Just ctx, Just buf_) -> do
              Timestamp now <- effectiveTime ctx
              -- Step 1: flush any expired buffered entries (the
              -- existing buffered value, NOT the just-arrived one).
              flushExpired ctx buf_ now
              -- Step 2: re-read this key and decide whether to
              -- start a new debounce window or extend the current.
              mPrev <- kvsGet buf_ k
              let !firstSeen = case mPrev of
                                 Just (ts, _) -> ts
                                 Nothing      -> now
              kvsPut buf_ k (firstSeen, recordValue r)
            _ -> pure ()
    }
  where
    flushExpired ctx buf_ now = do
      it <- kvsAll buf_
      entries <- kvIteratorToList it
      mapM_
        (\(k, (firstSeen, v)) ->
           if now >= firstSeen + limitMs
             then do
               _ <- kvsDelete buf_ k
               forwardRecord ctx
                 (Record (Just k) v (Timestamp now) emptyHeaders :: Record k v)
             else pure ())
        entries

----------------------------------------------------------------------
-- Suppressed builder (Java's Suppressed.untilWindowCloses /
-- .untilTimeLimit)
----------------------------------------------------------------------

-- | Mirrors Java's @Suppressed<K>@: a configuration value the
-- caller hands to 'suppressKStream'.
data Suppressed
  = SuppressUntilWindowCloses
      { suppressGrace      :: !Duration
      , suppressWindowSize :: !Int64
      }
  | SuppressUntilTimeLimit
      { suppressLimit :: !Duration
      }

untilWindowCloses :: Duration -> Int64 -> Suppressed
untilWindowCloses g sz = SuppressUntilWindowCloses g sz

untilTimeLimit :: Duration -> Suppressed
untilTimeLimit = SuppressUntilTimeLimit

-- | Apply a 'Suppressed' configuration to a 'KStream'. For
-- 'SuppressUntilWindowCloses' the input must be a stream of
-- windowed-key records (see 'streamFromWindowedHandle'); for
-- 'SuppressUntilTimeLimit' any keyed stream works.
--
-- We expose two type-distinct call sites because the underlying
-- key shapes differ; this convenience function picks the right
-- backend based on the 'Suppressed' value.
suppressKStream
  :: forall k v
   . Ord k
  => Suppressed
  -> Kafka.Streams.KStream.KStream k v
  -> IO (Kafka.Streams.KStream.KStream k v)
suppressKStream s =
  case s of
    SuppressUntilWindowCloses{} ->
      error
        "suppressKStream: untilWindowCloses requires a windowed-key \
        \stream; use 'suppressWindowed' directly."
    SuppressUntilTimeLimit{ suppressLimit = lim } ->
      suppressUntilTimeLimit lim

----------------------------------------------------------------------
-- High-level: KTable.suppress on a windowed aggregation
----------------------------------------------------------------------

-- | Convenience: take a 'WindowedTableHandle' from a windowed
-- aggregation and apply 'suppressWindowed' to its change stream.
-- Mirrors @KTable.suppress(Suppressed.untilWindowCloses(...))@.
suppressWindowedHandle
  :: forall k v
   . (Ord k, Eq v)
  => Duration                              -- grace
  -> Int64                                 -- window size
  -> Serde k                               -- inner key serde
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

-- | Tap a 'SWKS.SessionWindowedTableHandle' as a 'KStream' of
-- @(k, v)@ records pinned at the session-aggregator's emit node.
--
-- The aggregator forwards plain-keyed records (the inner @k@,
-- not 'SWKS.SessionKey k') so the downstream stream is keyed by
-- @k@. The supplied serdes are stamped into the resulting
-- 'KStream' so downstream sinks / joins have them available.
streamFromSessionWindowedHandle
  :: forall k v
   . Ord k
  => SWKS.SessionWindowedTableHandle k v
  -> Serde k                          -- ^ key serde for downstream
  -> Serde v                          -- ^ value serde for downstream
  -> IO (KStream k v)
streamFromSessionWindowedHandle h kserde vserde =
  pure KStream
    { kstreamBuilder    = SWKS.swthBuilder h
    , kstreamParent     = SWKS.swthNode h
    , kstreamKeySerde   = kserde
    , kstreamValueSerde = vserde
    }

----------------------------------------------------------------------
-- BufferConfig (KIP-328)
----------------------------------------------------------------------

-- | Buffer-size advisory for 'Suppressed.untilWindowCloses'.
-- 'unboundedBufferConfig' is the only mode the suppress
-- processor currently /enforces/ at runtime, but the bounded
-- variants accept the same JVM Suppressed.BufferConfig
-- vocabulary so callers can author topologies declaratively.
data BufferConfig = BufferConfig
  { maxBytes   :: !(Maybe Int)
  , maxRecords :: !(Maybe Int)
  , overflow   :: !BufferOverflowPolicy
  }
  deriving (Eq, Show)

-- | What to do when a bounded suppress buffer overflows. Mirrors
-- Java's 'BufferConfig.shutDownWhenFull' /
-- 'emitEarlyWhenFull' (KIP-328) and extends with the
-- Riffle-spec @drop@ policy ('DropOldestSilently'). The @shed@
-- policy is exposed separately via 'suppressWindowedShed'
-- because it carries typed-serde + topic config that doesn't fit
-- a plain enum.
data BufferOverflowPolicy
  = ShutdownWhenFull
    -- ^ JVM default for bounded buffers: tear the stream down
    --   when the budget is exceeded ('Fail' in the Riffle spec).
  | EmitEarlyWhenFull
    -- ^ Emit the buffered windows downstream even though grace
    --   hasn't elapsed yet, freeing space for new entries.
    --   Trades correctness (downstream sees premature emissions)
    --   for liveness.
  | DropOldestSilently
    -- ^ Discard the oldest over-cap entries without emitting them.
    --   Trades correctness (downstream never sees the dropped
    --   records) for both memory bound and liveness. The Riffle
    --   spec's @drop@ policy.
  deriving (Eq, Show)

unboundedBufferConfig :: BufferConfig
unboundedBufferConfig = BufferConfig Nothing Nothing EmitEarlyWhenFull

maxBytesBufferConfig :: Int -> BufferConfig
maxBytesBufferConfig n = BufferConfig (Just n) Nothing ShutdownWhenFull

maxRecordsBufferConfig :: Int -> BufferConfig
maxRecordsBufferConfig n = BufferConfig Nothing (Just n) ShutdownWhenFull

-- | Set the overflow policy on a 'BufferConfig'. Mirrors
-- Java's @BufferConfig.shutDownWhenFull()@ /
-- @emitEarlyWhenFull()@.
shutDownWhenFull :: BufferConfig -> BufferConfig
shutDownWhenFull b = b { overflow = ShutdownWhenFull }

emitEarlyWhenFull :: BufferConfig -> BufferConfig
emitEarlyWhenFull b = b { overflow = EmitEarlyWhenFull }

-- | Set the overflow policy to 'DropOldestSilently'.
dropOldestSilently :: BufferConfig -> BufferConfig
dropOldestSilently b = b { overflow = DropOldestSilently }
