{-# LANGUAGE ScopedTypeVariables #-}

{- | Utility functions for @google.protobuf.Timestamp@.

Provides conversions to\/from standard Haskell time types ('UTCTime',
'POSIXTime'), arithmetic with 'Duration', a current-time helper,
an 'Ord' instance, and validation — mirroring utilities found in
Go (@timestamppb@), Java (@com.google.protobuf.util.Timestamps@),
and Rust (@prost-types@).
-}
module Proto.Google.Protobuf.Timestamp.Util (
  -- * Conversions
  timestampFromUTCTime,
  timestampToUTCTime,
  timestampFromPOSIXTime,
  timestampToPOSIXTime,

  -- * Current time
  getCurrentTimestamp,

  -- * Arithmetic
  addDuration,
  subtractTimestamps,

  -- * Validation
  isValidTimestamp,

  -- * Comparison
  compareTimestamp,
) where

import Data.Int (Int64)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Proto.Google.Protobuf.Duration (Duration (..), defaultDuration)
import Proto.Google.Protobuf.Timestamp (Timestamp (..), defaultTimestamp)


nanosPerSecond :: Integer
nanosPerSecond = 1000000000


-- | Convert a 'UTCTime' to a 'Timestamp'.
timestampFromUTCTime :: UTCTime -> Timestamp
timestampFromUTCTime = timestampFromPOSIXTime . utcTimeToPOSIXSeconds


-- | Convert a 'Timestamp' to a 'UTCTime'.
timestampToUTCTime :: Timestamp -> UTCTime
timestampToUTCTime = posixSecondsToUTCTime . timestampToPOSIXTime


{- | Convert a 'POSIXTime' to a 'Timestamp'.

Fractional seconds are preserved at nanosecond granularity.
-}
timestampFromPOSIXTime :: POSIXTime -> Timestamp
timestampFromPOSIXTime pt = timestampFromTotalNanos (round (pt * fromIntegral nanosPerSecond))


-- Timestamp carries a nonnegative fractional part, including before 1970.
timestampFromTotalNanos :: Integer -> Timestamp
timestampFromTotalNanos total =
  let (seconds, nanos) = total `divMod` nanosPerSecond
  in defaultTimestamp
       { timestampSeconds = checkedSeconds seconds
       , timestampNanos = fromIntegral nanos
       }


timestampTotalNanos :: Timestamp -> Integer
timestampTotalNanos ts = toInteger (timestampSeconds ts) * nanosPerSecond + toInteger (timestampNanos ts)


checkedSeconds :: Integer -> Int64
checkedSeconds seconds
  | seconds < toInteger (minBound :: Int64) || seconds > toInteger (maxBound :: Int64) =
      error "Protobuf time: seconds do not fit in Int64"
  | otherwise = fromInteger seconds


-- | Convert a 'Timestamp' to a 'POSIXTime'.
timestampToPOSIXTime :: Timestamp -> POSIXTime
timestampToPOSIXTime ts =
  let s = fromIntegral (timestampSeconds ts) :: POSIXTime
      n = fromIntegral (timestampNanos ts) / fromIntegral nanosPerSecond :: POSIXTime
  in s + n


-- | Get the current system time as a 'Timestamp'.
getCurrentTimestamp :: IO Timestamp
getCurrentTimestamp = timestampFromPOSIXTime <$> getPOSIXTime


-- | Add a 'Duration' to a 'Timestamp', producing a new 'Timestamp'.
addDuration :: Timestamp -> Duration -> Timestamp
addDuration ts dur =
  timestampFromTotalNanos (timestampTotalNanos ts + toInteger (durationSeconds dur) * nanosPerSecond + toInteger (durationNanos dur))


-- | Compute the 'Duration' between two 'Timestamp' values (@a - b@).
subtractTimestamps :: Timestamp -> Timestamp -> Duration
subtractTimestamps a b =
  let (seconds, nanos) = (timestampTotalNanos a - timestampTotalNanos b) `quotRem` nanosPerSecond
  in defaultDuration
       { durationSeconds = checkedSeconds seconds
       , durationNanos = fromIntegral nanos
       }


{- | 'Timestamp' is valid when seconds is in @[0001-01-01T00:00:00Z, 9999-12-31T23:59:59Z]@
and nanos is in @[0, 999999999]@.

These are the bounds from the proto spec.
-}
isValidTimestamp :: Timestamp -> Bool
isValidTimestamp ts =
  timestampSeconds ts >= minTimestampSeconds
    && timestampSeconds ts <= maxTimestampSeconds
    && timestampNanos ts >= 0
    && timestampNanos ts <= 999999999
  where
    minTimestampSeconds = -62135596800 -- 0001-01-01T00:00:00Z
    maxTimestampSeconds = 253402300799 -- 9999-12-31T23:59:59Z


-- | Compare two 'Timestamp' values. Compares seconds first, then nanos.
compareTimestamp :: Timestamp -> Timestamp -> Ordering
compareTimestamp a b =
  compare (timestampSeconds a) (timestampSeconds b)
    <> compare (timestampNanos a) (timestampNanos b)


instance Ord Timestamp where
  compare = compareTimestamp
