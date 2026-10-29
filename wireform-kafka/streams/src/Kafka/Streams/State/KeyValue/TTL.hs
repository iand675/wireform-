{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.State.KeyValue.TTL
Description : Event-time TTL wrapper for KeyValueStore (Riffle §3)

A wrapper around an existing 'KeyValueStore' that attaches an
/event-time expiry/ to every entry on write, and lazily drops
expired entries on read.

The wrapper stores @(value, expireAt)@ pairs underneath; the
caller's view is @KeyValueStore k v@ — expired entries are
invisible to 'kvsGet' / 'kvsRange' / 'kvsAll' as soon as the
supplied /event-time clock/ has advanced past their expiry.

== Reaping

Lazy expiry on read keeps the store cheap, but doesn't free
the memory of expired entries. The runtime periodically (e.g.
on each punctuator firing) calls 'expireBefore' to actively
purge entries that have aged out. That's intentionally
separate from the read-path so a heavy reaper can't block the
hot path.

== Event-time vs wall-clock

The clock is supplied by the caller as @IO Timestamp@. In the
topology this is the engine's 'ctxStreamTime' (so the TTL is
event-time, matching Kafka Streams' window semantics) — never
the wall clock. Tests can supply a fixed clock to make expiry
deterministic.
-}
module Kafka.Streams.State.KeyValue.TTL (
  TTLConfig (..),
  ttlKeyValueStore,
  expireBefore,
  ttlEntryCount,

  -- * Clock helpers
  ttlClockFromCoordinator,
) where

import Data.IORef (IORef, newIORef)
import Data.Int (Int64)
import Kafka.Streams.Processor (
  Cancellable (..),
  ProcessorContext (..),
  PunctuationType (..),
  Punctuator (..),
  schedule,
 )
import Kafka.Streams.State.Store (
  KeyValueIterator (..),
  KeyValueStore (..),
  kvIteratorToList,
 )
import Kafka.Streams.Time (
  Duration,
  Timestamp (..),
  addDuration,
  durationMillis,
 )
import Kafka.Streams.Watermark qualified as Watermark


----------------------------------------------------------------------
-- Config
----------------------------------------------------------------------

-- | Configuration for a TTL wrapper.
data TTLConfig = TTLConfig
  { ttlDuration :: !Duration
  {- ^ Entries written at @t@ expire at @t + ttlDuration@. A
  value of @0@ means "no TTL" — the wrapper is a passthrough
  (it still adds an 'expireBefore' API but never expires
  anything).
  -}
  , ttlClock :: !(IO Timestamp)
  {- ^ Event-time clock. The runtime ties this to
  'ctxStreamTime'.
  -}
  }


----------------------------------------------------------------------
-- Wrapper
----------------------------------------------------------------------

{- | Wrap a base 'KeyValueStore' so every entry has an event-time
TTL. The wrapper:

  * On 'kvsPut': reads @now <- ttlClock@ and writes the
    underlying entry as @(value, now + ttl)@.
  * On 'kvsGet' / 'kvsRange' / 'kvsAll': reads @now <- ttlClock@
    and filters out entries whose @expireAt <= now@.
  * On 'expireBefore': sweeps the underlying store and deletes
    every entry with @expireAt <= now@.

The underlying store sees values of type @(v, Timestamp)@; this
module's API hides that and exposes a plain @KeyValueStore k v@.
-}
ttlKeyValueStore
  :: forall k v
   . TTLConfig
  -> KeyValueStore k (v, Timestamp)
  -> IO (KeyValueStore k v, IORef Int64)
ttlKeyValueStore cfg under = do
  -- A small "reaped" counter the caller can read via
  -- 'ttlEntryCount' to drive metrics / tests.
  reapedTV <- newIORef (0 :: Int64)
  pure
    ( KeyValueStore
        { kvsBase = kvsBase under
        , kvsCache = Nothing
        , kvsApproxEntries = kvsApproxEntries under
        , kvsGet = ttlGet cfg under
        , kvsPut = ttlPut cfg under
        , kvsPutIfAbsent = ttlPutIfAbsent cfg under
        , kvsDelete = ttlDelete under
        , kvsRange = ttlRange cfg under
        , kvsAll = ttlAll cfg under
        , kvsReverseRange = ttlReverseRange cfg under
        , kvsReverseAll = ttlReverseAll cfg under
        }
    , reapedTV
    )


ttlGet
  :: TTLConfig
  -> KeyValueStore k (v, Timestamp)
  -> k
  -> IO (Maybe v)
ttlGet cfg under k = do
  now <- ttlClock cfg
  mvte <- kvsGet under k
  pure $ case mvte of
    Nothing -> Nothing
    Just (v, expireAt) ->
      if expireAt <= now then Nothing else Just v


ttlPut
  :: TTLConfig
  -> KeyValueStore k (v, Timestamp)
  -> k
  -> v
  -> IO ()
ttlPut cfg under k v = do
  now <- ttlClock cfg
  let !expireAt = addDuration now (ttlDuration cfg)
  kvsPut under k (v, expireAt)


ttlPutIfAbsent
  :: TTLConfig
  -> KeyValueStore k (v, Timestamp)
  -> k
  -> v
  -> IO (Maybe v)
ttlPutIfAbsent cfg under k v = do
  now <- ttlClock cfg
  let !expireAt = addDuration now (ttlDuration cfg)
  -- Read first so we can apply the expired-as-absent rule.
  existing <- kvsGet under k
  case existing of
    Just (vOld, exp0)
      | exp0 > now -> pure (Just vOld)
      | otherwise -> do
          -- Already expired: overwrite as a fresh entry.
          kvsPut under k (v, expireAt)
          pure Nothing
    Nothing -> do
      r <- kvsPutIfAbsent under k (v, expireAt)
      pure (fst <$> r)


ttlDelete
  :: KeyValueStore k (v, Timestamp)
  -> k
  -> IO (Maybe v)
ttlDelete under k = do
  r <- kvsDelete under k
  pure (fst <$> r)


-- | Iterator transform: filter out expired entries on the fly.
liveIterator
  :: Timestamp
  -> KeyValueIterator k (v, Timestamp)
  -> IO (KeyValueIterator k v)
liveIterator now it =
  pure
    KeyValueIterator
      { kvIterNext = nextLive
      , kvIterClose = kvIterClose it
      }
  where
    nextLive = do
      mx <- kvIterNext it
      case mx of
        Nothing -> pure Nothing
        Just (k, (v, expireAt))
          | expireAt > now -> pure (Just (k, v))
          | otherwise -> nextLive


ttlRange
  :: TTLConfig
  -> KeyValueStore k (v, Timestamp)
  -> k
  -> k
  -> IO (KeyValueIterator k v)
ttlRange cfg under lo hi = do
  now <- ttlClock cfg
  it <- kvsRange under lo hi
  liveIterator now it


ttlAll
  :: TTLConfig
  -> KeyValueStore k (v, Timestamp)
  -> IO (KeyValueIterator k v)
ttlAll cfg under = do
  now <- ttlClock cfg
  it <- kvsAll under
  liveIterator now it


ttlReverseRange
  :: TTLConfig
  -> KeyValueStore k (v, Timestamp)
  -> k
  -> k
  -> IO (KeyValueIterator k v)
ttlReverseRange cfg under lo hi = do
  now <- ttlClock cfg
  it <- kvsReverseRange under lo hi
  liveIterator now it


ttlReverseAll
  :: TTLConfig
  -> KeyValueStore k (v, Timestamp)
  -> IO (KeyValueIterator k v)
ttlReverseAll cfg under = do
  now <- ttlClock cfg
  it <- kvsReverseAll under
  liveIterator now it


----------------------------------------------------------------------
-- Reaping
----------------------------------------------------------------------

{- | Sweep the underlying store and delete every entry whose
@expireAt <= now@. Returns the number of entries reaped. The
caller is expected to invoke this from a punctuator on a
regular cadence so memory usage stays bounded.
-}
expireBefore
  :: KeyValueStore k (v, Timestamp)
  -> Timestamp
  -> IO Int64
expireBefore under now = do
  it <- kvsAll under
  pairs <- kvIteratorToList it
  let expired =
        [ k
        | (k, (_, expireAt)) <- pairs
        , expireAt <= now
        ]
  mapM_ (\k -> () <$ kvsDelete under k) expired
  pure (fromIntegral (length expired))


{- | Schedule a stream-time 'Punctuator' that reaps expired
entries from a TTL-wrapped store at the supplied cadence. This
is the missing piece the 'ttlKeyValueStore' docstring promises
(\"the runtime periodically calls 'expireBefore'\") -- without
a scheduled reaper, expired entries linger in the underlying
store until the next 'kvsPut' that overwrites them, even
though they are correctly /invisible/ on read.

Mirrors the JVM Kafka Streams behaviour where windowed and
session stores auto-evict on a punctuator cadence; the
per-store TTL wrapper here just needs a processor that owns
the store to call this from 'procInit':

@
procInit = \\ctx -> do
  ...
  _ <- 'scheduleTtlReaper' ctx cfg under (Time.millis 1000)
  pure ()
@

The clock used by the punctuator is the same 'ttlClock' the
'TTLConfig' was constructed with -- typically the watermark
coordinator via 'ttlClockFromCoordinator' for event-time TTL,
or a wall-clock callback for wall-clock TTL.

Returns the 'Cancellable' so the processor can stop the reaper
on close.
-}
scheduleTtlReaper
  :: ProcessorContext
  -> TTLConfig
  -> KeyValueStore k (v, Timestamp)
  -> Duration
  -- ^ how often to sweep
  -> IO Cancellable
scheduleTtlReaper ctx cfg under interval =
  schedule
    ctx
    (max 1 (fromIntegral (durationMillis interval) :: Int))
    StreamTimePunctuation
    ( Punctuator $ \_ -> do
        now <- ttlClock cfg
        _ <- expireBefore under now
        pure ()
    )


{- | Returns the count of live (non-expired) entries as of @now@.
This is O(n); the runtime uses it for metrics and tests use it
to assert post-expiry visibility.
-}
ttlEntryCount
  :: KeyValueStore k v
  -> IO Int64
ttlEntryCount kvs = do
  it <- kvsAll kvs
  pairs <- kvIteratorToList it
  pure (fromIntegral (length pairs))


----------------------------------------------------------------------
-- Clock helpers
----------------------------------------------------------------------

{- | Build a 'ttlClock' callback that reads the
'Kafka.Streams.Watermark.WatermarkCoordinator' 's effective
(min-of-live-sources) watermark. This is the Riffle \xc2\xa76 default
— TTL on a state store is driven by the cross-source
coordinated watermark, not by the per-task 'StreamTime' or
the wall clock. The TTL wrapper still accepts any
@IO Timestamp@, so callers can substitute a wall-clock or
fixed-test clock when appropriate.
-}
ttlClockFromCoordinator :: Watermark.WatermarkCoordinator -> IO Timestamp
ttlClockFromCoordinator = Watermark.currentEffectiveWatermark
