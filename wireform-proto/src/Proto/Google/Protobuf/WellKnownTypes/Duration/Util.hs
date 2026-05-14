{-# LANGUAGE ScopedTypeVariables #-}
-- | Utility functions for @google.protobuf.Duration@.
--
-- Provides conversions to\/from 'NominalDiffTime', construction from
-- milliseconds\/microseconds\/nanoseconds, arithmetic, an 'Ord' instance,
-- and validation — mirroring utilities found in Go (@durationpb@),
-- Java (@com.google.protobuf.util.Durations@), and Rust (@prost-types@).
module Proto.Google.Protobuf.WellKnownTypes.Duration.Util
  ( -- * Conversions
    durationFromNominalDiffTime
  , durationToNominalDiffTime

    -- * Construction
  , durationFromSeconds
  , durationFromMillis
  , durationFromMicros
  , durationFromNanos

    -- * Extraction
  , durationToSeconds
  , durationToMillis
  , durationToMicros
  , durationToNanos

    -- * Arithmetic
  , addDurations
  , negateDuration
  , absDuration

    -- * Validation
  , isValidDuration
  , normalizeDuration

    -- * Comparison
  , compareDuration
  ) where

import Data.Int (Int32, Int64)
import Data.Time.Clock (NominalDiffTime)

import Proto.Google.Protobuf.WellKnownTypes.Duration (Duration(..), defaultDuration)

nanosPerSecond :: Int64
nanosPerSecond = 1000000000

-- | Convert a 'NominalDiffTime' to a 'Duration'.
--
-- Fractional seconds are preserved at nanosecond granularity.
durationFromNominalDiffTime :: NominalDiffTime -> Duration
durationFromNominalDiffTime ndt =
  let totalNanos = round (ndt * fromIntegral nanosPerSecond) :: Int64
      (s, n) = totalNanos `quotRem` nanosPerSecond
  in defaultDuration
    { durationSeconds = s
    , durationNanos = fromIntegral n
    }

-- | Convert a 'Duration' to a 'NominalDiffTime'.
durationToNominalDiffTime :: Duration -> NominalDiffTime
durationToNominalDiffTime dur =
  let s = fromIntegral (durationSeconds dur) :: NominalDiffTime
      n = fromIntegral (durationNanos dur) / fromIntegral nanosPerSecond :: NominalDiffTime
  in s + n

-- | Construct a 'Duration' from whole seconds.
durationFromSeconds :: Int64 -> Duration
durationFromSeconds s = defaultDuration { durationSeconds = s }

-- | Construct a 'Duration' from milliseconds.
durationFromMillis :: Int64 -> Duration
durationFromMillis ms =
  let (s, rem') = ms `quotRem` 1000
  in defaultDuration
    { durationSeconds = s
    , durationNanos = fromIntegral (rem' * 1000000)
    }

-- | Construct a 'Duration' from microseconds.
durationFromMicros :: Int64 -> Duration
durationFromMicros us =
  let (s, rem') = us `quotRem` 1000000
  in defaultDuration
    { durationSeconds = s
    , durationNanos = fromIntegral (rem' * 1000)
    }

-- | Construct a 'Duration' from nanoseconds.
durationFromNanos :: Int64 -> Duration
durationFromNanos ns =
  let (s, n) = ns `quotRem` nanosPerSecond
  in defaultDuration
    { durationSeconds = s
    , durationNanos = fromIntegral n
    }

-- | Extract total seconds (truncating nanos).
durationToSeconds :: Duration -> Int64
durationToSeconds = durationSeconds

-- | Convert to total milliseconds (truncating sub-millisecond part).
durationToMillis :: Duration -> Int64
durationToMillis dur =
  durationSeconds dur * 1000 + fromIntegral (durationNanos dur) `quot` 1000000

-- | Convert to total microseconds (truncating sub-microsecond part).
durationToMicros :: Duration -> Int64
durationToMicros dur =
  durationSeconds dur * 1000000 + fromIntegral (durationNanos dur) `quot` 1000

-- | Convert to total nanoseconds.
durationToNanos :: Duration -> Int64
durationToNanos dur =
  durationSeconds dur * nanosPerSecond + fromIntegral (durationNanos dur)

-- | Add two 'Duration' values.
addDurations :: Duration -> Duration -> Duration
addDurations a b =
  let totalNanos = durationToNanos a + durationToNanos b
      (s, n) = totalNanos `quotRem` nanosPerSecond
  in defaultDuration
    { durationSeconds = s
    , durationNanos = fromIntegral n
    }

-- | Negate a 'Duration'.
negateDuration :: Duration -> Duration
negateDuration dur = defaultDuration
  { durationSeconds = negate (durationSeconds dur)
  , durationNanos = negate (durationNanos dur)
  }

-- | Absolute value of a 'Duration'.
absDuration :: Duration -> Duration
absDuration dur
  | durationSeconds dur < 0 || (durationSeconds dur == 0 && durationNanos dur < 0) =
      negateDuration dur
  | otherwise = dur

-- | Normalize a Duration so that seconds and nanos have the same sign
-- and nanos is in @(-999999999, 999999999)@.
--
-- The proto spec requires that for valid Durations, nanos have the same
-- sign as seconds (or be zero) and @|nanos| < 10^9@.
normalizeDuration :: Duration -> Duration
normalizeDuration dur =
  let totalNanos = durationToNanos dur
      (s, n) = totalNanos `quotRem` nanosPerSecond
  in defaultDuration
    { durationSeconds = s
    , durationNanos = fromIntegral n
    }

-- | A 'Duration' is valid when:
--
-- * seconds is in @[-315576000000, 315576000000]@ (roughly +/- 10000 years)
-- * nanos is in @[-999999999, 999999999]@
-- * seconds and nanos have the same sign (or either is zero)
isValidDuration :: Duration -> Bool
isValidDuration dur =
  abs (durationSeconds dur) <= 315576000000
  && abs (fromIntegral (durationNanos dur) :: Int64) <= 999999999
  && signsAgree (durationSeconds dur) (fromIntegral (durationNanos dur))
  where
    signsAgree s n
      | s == 0 || n == 0 = True
      | otherwise = (s > 0) == (n > 0)

-- | Compare two 'Duration' values by total nanoseconds.
compareDuration :: Duration -> Duration -> Ordering
compareDuration a b = compare (durationToNanos a) (durationToNanos b)

instance Ord Duration where
  compare = compareDuration
