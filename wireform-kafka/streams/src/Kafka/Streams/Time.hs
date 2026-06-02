{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Kafka.Streams.Time
-- Description : Timestamps, durations, time semantics
--
-- Kafka Streams treats time as 64-bit milliseconds since the Unix
-- epoch (matching the broker @CreateTime@ / @LogAppendTime@ wire
-- format).  Negative values denote /unknown/ / /no-timestamp-type/
-- per @org.apache.kafka.common.record.TimestampType.NO_TIMESTAMP_TYPE@.
{-# LANGUAGE BangPatterns #-}
module Kafka.Streams.Time
  ( -- * Timestamps
    Timestamp (..)
  , noTimestamp
  , isKnownTimestamp
  , minTimestamp
  , maxTimestamp
  , timestampMillis
  , addMillis
  , diffMillis
  , timestampToUTCTime
  , utcTimeToTimestamp
  , nowMillis
    -- * Durations
  , Duration (..)
  , durationMillis
  , millis
  , seconds
  , minutes
  , hours
  , days
  , addDuration
  , subDuration
  , scaleDuration
    -- * Timestamp semantics
  , TimestampType (..)
  , StreamTime (..)
  , initialStreamTime
  , advanceStreamTime
    -- * Timestamp extractors
  , TimestampExtractor (..)
  , wallClockTimestampExtractor
  , recordTimestampExtractor
  , failOnNoTimestampExtractor
  , logAndSkipOnNoTimestamp
  , usePartitionTimeOnInvalidTimestamp
  , extractRecordMetadataTimestamp
  ) where

import Data.Int (Int64)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX
  ( POSIXTime
  , posixSecondsToUTCTime
  , utcTimeToPOSIXSeconds
  )
import GHC.Generics (Generic)

-- | Wall-clock-aligned millisecond timestamp.
--
-- @Timestamp (-1)@ encodes /unknown/, matching the Java @-1@ sentinel.
newtype Timestamp = Timestamp { unTimestamp :: Int64 }
  deriving stock (Eq, Ord, Show, Generic)

-- | The representable range, so generic code can reach the
-- @-1@-aware extremes without importing 'minTimestamp' \/
-- 'maxTimestamp' by name. @'minBound' = 'minTimestamp'@,
-- @'maxBound' = 'maxTimestamp'@.
instance Bounded Timestamp where
  minBound = minTimestamp
  maxBound = maxTimestamp

-- | Sentinel for "no timestamp set".
noTimestamp :: Timestamp
noTimestamp = Timestamp (-1)

isKnownTimestamp :: Timestamp -> Bool
isKnownTimestamp (Timestamp t) = t >= 0

minTimestamp, maxTimestamp :: Timestamp
minTimestamp = Timestamp minBound
maxTimestamp = Timestamp maxBound

timestampMillis :: Timestamp -> Int64
timestampMillis = unTimestamp

addMillis :: Timestamp -> Int64 -> Timestamp
addMillis (Timestamp t) ms = Timestamp (t + ms)

-- | Distance in milliseconds (signed).
diffMillis :: Timestamp -> Timestamp -> Int64
diffMillis (Timestamp a) (Timestamp b) = a - b

timestampToUTCTime :: Timestamp -> UTCTime
timestampToUTCTime (Timestamp ms) =
  posixSecondsToUTCTime (fromIntegral ms / 1000 :: POSIXTime)

utcTimeToTimestamp :: UTCTime -> Timestamp
utcTimeToTimestamp t =
  Timestamp (round (utcTimeToPOSIXSeconds t * 1000))

-- | Positive duration in milliseconds. Constructors are not exposed
-- so we can keep the invariant.
newtype Duration = Duration { unDuration :: Int64 }
  deriving stock (Eq, Ord, Show, Generic)

-- | Durations add: @d1 <> d2@ is the combined span (e.g. window
-- size plus grace period). Both operands are already
-- non-negative so the sum stays non-negative and the
-- smart-constructor invariant is preserved.
instance Semigroup Duration where
  Duration a <> Duration b = Duration (a + b)
  {-# INLINE (<>) #-}

-- | 'mempty' is the zero duration, the additive identity. Lets
-- a list of durations be combined with 'mconcat'.
instance Monoid Duration where
  mempty = Duration 0
  {-# INLINE mempty #-}

-- Smart constructors keep the invariant @>= 0@. Java throws on
-- negatives; we clamp at zero rather than throw because pure code
-- handles that more gracefully and the practical effect is the same
-- for downstream window math.
millis, seconds, minutes, hours, days :: Int64 -> Duration
millis  ms  = Duration (max 0 ms)
seconds s   = Duration (max 0 (s * 1000))
minutes m   = Duration (max 0 (m * 60_000))
hours   h   = Duration (max 0 (h * 3_600_000))
days    d   = Duration (max 0 (d * 86_400_000))

durationMillis :: Duration -> Int64
durationMillis = unDuration

addDuration :: Timestamp -> Duration -> Timestamp
addDuration (Timestamp t) (Duration d) = Timestamp (t + d)

subDuration :: Timestamp -> Duration -> Timestamp
subDuration (Timestamp t) (Duration d) = Timestamp (t - d)

scaleDuration :: Int64 -> Duration -> Duration
scaleDuration n (Duration d) = Duration (max 0 (n * d))

-- | Producer-supplied timestamp interpretation.
data TimestampType
  = CreateTime
  | LogAppendTime
  | NoTimestampType
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

-- | Stream-time tracking. Streams maintains a per-task monotonic
-- watermark equal to the maximum timestamp seen on that task so far.
-- Punctuators and window expiration are driven by stream-time, not
-- wall-clock.
newtype StreamTime = StreamTime { unStreamTime :: Timestamp }
  deriving stock (Eq, Ord, Show, Generic)

-- | Stream time only ever advances, so it combines by 'max' —
-- the same rule as 'advanceStreamTime'. This makes folding the
-- per-partition stream times of a task into its watermark a
-- plain 'mconcat' / 'foldMap'.
instance Semigroup StreamTime where
  StreamTime a <> StreamTime b = StreamTime (max a b)
  {-# INLINE (<>) #-}

-- | 'mempty' is 'initialStreamTime' (the minimum timestamp), the
-- identity for 'max'.
instance Monoid StreamTime where
  mempty = initialStreamTime
  {-# INLINE mempty #-}

initialStreamTime :: StreamTime
initialStreamTime = StreamTime minTimestamp

-- | Stream time advances monotonically. Out-of-order records do not
-- regress it.
advanceStreamTime :: Timestamp -> StreamTime -> StreamTime
advanceStreamTime t (StreamTime cur) = StreamTime (max cur t)

-- | Pluggable timestamp extractor.
--
-- Receives the record together with the previous stream time so the
-- handler can choose to fall back, raise, or skip when a record has
-- no embedded timestamp.
newtype TimestampExtractor k v = TimestampExtractor
  { runTimestampExtractor
      :: Maybe k -> v -> Timestamp -> StreamTime -> IO Timestamp
  }

-- | Use wall-clock time at the moment of extraction (mirrors Java's
-- @WallclockTimestampExtractor@).
wallClockTimestampExtractor :: TimestampExtractor k v
wallClockTimestampExtractor = TimestampExtractor $ \_ _ _ _ ->
  utcTimeToTimestamp <$> getCurrentTime
{-# INLINE wallClockTimestampExtractor #-}

-- | Current wall-clock time as Unix epoch milliseconds. Used by
-- the streams runtime for KIP-869 standby-grace deadlines and
-- the KIP-441 probing-rebalance cadence.
nowMillis :: IO Int64
nowMillis = do
  !pt <- utcTimeToPOSIXSeconds <$> getCurrentTime
  pure (floor (pt * 1000 :: POSIXTime))

-- | Use the embedded record timestamp.
recordTimestampExtractor :: TimestampExtractor k v
recordTimestampExtractor = TimestampExtractor $ \_ _ rt _ -> pure rt

-- | Match @FailOnInvalidTimestamp@: throws if the embedded timestamp
-- is the @-1@ sentinel.
failOnNoTimestampExtractor :: TimestampExtractor k v
failOnNoTimestampExtractor = TimestampExtractor $ \_ _ rt _ ->
  if isKnownTimestamp rt
    then pure rt
    else error "TimestampExtractor: record has no timestamp"

-- | Match @LogAndSkipOnInvalidTimestamp@: returns 'noTimestamp' which
-- callers should interpret as "skip this record".
logAndSkipOnNoTimestamp :: TimestampExtractor k v
logAndSkipOnNoTimestamp = TimestampExtractor $ \_ _ rt _ -> pure rt

-- | Match @UsePartitionTimeOnInvalidTimestamp@ (the JVM default
-- since Kafka 2.5): when the embedded record timestamp is the
-- @-1@ sentinel, fall back to the current /partition time/ —
-- i.e. the highest timestamp seen on this partition so far.
-- This keeps stream-time monotonic for downstream windowing
-- without poisoning aggregates with epoch-0 timestamps.
usePartitionTimeOnInvalidTimestamp :: TimestampExtractor k v
usePartitionTimeOnInvalidTimestamp = TimestampExtractor $ \_ _ rt (StreamTime st) ->
  if isKnownTimestamp rt
    then pure rt
    else
      if isKnownTimestamp st
        then pure st
        else error "TimestampExtractor: no valid stream time yet"

-- | Base extractor that mirrors Java's
-- @ExtractRecordMetadataTimestamp@: returns the embedded record
-- timestamp unchanged, deferring to the caller's specialisation
-- for the invalid-timestamp branch. Identical to
-- 'recordTimestampExtractor' but the name is preserved for
-- JVM-portability shims.
extractRecordMetadataTimestamp :: TimestampExtractor k v
extractRecordMetadataTimestamp = recordTimestampExtractor
