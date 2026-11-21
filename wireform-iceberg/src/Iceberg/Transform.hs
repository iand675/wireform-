{- | Apply Iceberg partition\/sort transforms to typed source values.

Each 'Transform' has an associated /result type/ and an /evaluation/ function.
For example, @bucket[N]@ over an @int@ source produces an @int@ in @[0, N)@,
and @year@ over a @date@ produces an @int@ year-since-epoch.

Time transforms work on millisecond, microsecond, or nanosecond units:

- @date@      values are days since the Unix epoch.
- @timestamp@ values are microseconds since the Unix epoch.
- @timestamp_ns@ values are nanoseconds since the Unix epoch.

This module mirrors the Java @TransformUtil@ helpers and is byte-compatible
with the engine implementations used by Spark and PyIceberg.
-}
module Iceberg.Transform (
  -- * Result type
  transformResultType,

  -- * Evaluation
  applyTransform,
  TransformError (..),

  -- * Year-month-day-hour helpers
  dateToYears,
  dateToMonths,
  dateToDays,
  microsToYears,
  microsToMonths,
  microsToDays,
  microsToHours,
  nanosToYears,
  nanosToMonths,
  nanosToDays,
  nanosToHours,
) where

import Avro.Value qualified as AV
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Iceberg.Murmur3 (bucketBytes, bucketInt, bucketLong, bucketString)
import Iceberg.SingleValue qualified as SV
import Iceberg.Types


{- | What type does the partition\/sort transform produce when applied to a
value of the given source type? Returns 'Nothing' if the combination is
invalid (e.g. @year@ on a string column).
-}
transformResultType :: Transform -> IcebergType -> Maybe IcebergType
transformResultType Identity t = Just t
transformResultType (Bucket _) _ = Just TInt
transformResultType (Truncate _) t = case t of
  TInt -> Just TInt
  TLong -> Just TLong
  TString -> Just TString
  TBinary -> Just TBinary
  TDecimal _ _ -> Just t
  _ -> Nothing
transformResultType Year t = if isTime t then Just TInt else Nothing
transformResultType Month t = if isTime t then Just TInt else Nothing
transformResultType Day t = if isTime t then Just TDate else Nothing
transformResultType Hour t = if isTimestamp t then Just TInt else Nothing
transformResultType Void _ = Just TUnknown
transformResultType (UnknownTransform _) _ = Nothing


isTime :: IcebergType -> Bool
isTime TDate = True
isTime TTimestamp = True
isTime TTimestampTz = True
isTime TTimestampNs = True
isTime TTimestampTzNs = True
isTime _ = False


isTimestamp :: IcebergType -> Bool
isTimestamp TTimestamp = True
isTimestamp TTimestampTz = True
isTimestamp TTimestampNs = True
isTimestamp TTimestampTzNs = True
isTimestamp _ = False


data TransformError
  = TEUnsupported !Text !IcebergType
  | TEUnknownTransform !Text
  | TENullValue
  deriving (Show, Eq)


{- | Apply a transform to a source value of a given source type.

The output is a typed Avro value that matches the result type from
'transformResultType' (e.g. 'AV.Int' or 'AV.Long').
-}
applyTransform
  :: Transform
  -> IcebergType
  -- ^ Source type.
  -> AV.Value
  -- ^ Source value (must match the source type).
  -> Either TransformError AV.Value
applyTransform Identity _ v = Right v
applyTransform Void _ _ = Right AV.Null
applyTransform (UnknownTransform n) _ _ = Left (TEUnknownTransform n)
applyTransform (Bucket n) src v = case src of
  TInt -> withInt v $ \x -> AV.Int (fromIntegral (bucketInt n x))
  TDate -> withInt v $ \x -> AV.Int (fromIntegral (bucketInt n x))
  TTime -> withLong v $ \x -> AV.Int (fromIntegral (bucketLong n x))
  TLong -> withLong v $ \x -> AV.Int (fromIntegral (bucketLong n x))
  TTimestamp -> withLong v $ \x -> AV.Int (fromIntegral (bucketLong n x))
  TTimestampTz -> withLong v $ \x -> AV.Int (fromIntegral (bucketLong n x))
  TTimestampNs -> withLong v $ \x -> AV.Int (fromIntegral (bucketLong n x))
  TTimestampTzNs -> withLong v $ \x -> AV.Int (fromIntegral (bucketLong n x))
  TString -> withText v $ \t -> AV.Int (fromIntegral (bucketString n t))
  TBinary -> withBytes v $ \bs -> AV.Int (fromIntegral (bucketBytes n bs))
  TFixed _ -> withBytes v $ \bs -> AV.Int (fromIntegral (bucketBytes n bs))
  TUuid -> withBytes v $ \bs -> AV.Int (fromIntegral (bucketBytes n bs))
  TDecimal _ _ -> withBytes v $ \bs -> AV.Int (fromIntegral (bucketBytes n bs))
  other -> Left (TEUnsupported "bucket" other)
applyTransform (Truncate w) src v = case src of
  TInt -> withInt v $ \x -> AV.Int (truncateInt32 w x)
  TLong -> withLong v $ \x -> AV.Long (truncateInt64 w x)
  TString -> withText v $ \t -> AV.String (T.take w t)
  TBinary -> withBytes v $ \bs -> AV.Bytes (BS.take w bs)
  TFixed _ -> withBytes v $ \bs -> AV.Bytes (BS.take w bs)
  TDecimal _ _ -> case v of
    AV.Bytes bytes -> truncateDecimal bytes
    AV.Fixed bytes -> truncateDecimal bytes
    AV.Null -> Left TENullValue
    _ -> Left (TEUnsupported "truncate" src)
  other -> Left (TEUnsupported "truncate" other)
  where
    truncateDecimal bytes
      | w > 0, Right value <- SV.decodeDecimal bytes =
          Right (AV.Bytes (SV.encodeDecimal (value - value `mod` toInteger w)))
      | otherwise = Left (TEUnsupported "truncate" src)
applyTransform Year src v = case src of
  TDate -> withInt v $ \d -> AV.Int (dateToYears d)
  TTimestamp -> withLong v $ \t -> AV.Int (microsToYears t)
  TTimestampTz -> withLong v $ \t -> AV.Int (microsToYears t)
  TTimestampNs -> withLong v $ \t -> AV.Int (nanosToYears t)
  TTimestampTzNs -> withLong v $ \t -> AV.Int (nanosToYears t)
  other -> Left (TEUnsupported "year" other)
applyTransform Month src v = case src of
  TDate -> withInt v $ \d -> AV.Int (dateToMonths d)
  TTimestamp -> withLong v $ \t -> AV.Int (microsToMonths t)
  TTimestampTz -> withLong v $ \t -> AV.Int (microsToMonths t)
  TTimestampNs -> withLong v $ \t -> AV.Int (nanosToMonths t)
  TTimestampTzNs -> withLong v $ \t -> AV.Int (nanosToMonths t)
  other -> Left (TEUnsupported "month" other)
applyTransform Day src v = case src of
  TDate -> Right v
  TTimestamp -> withLong v $ \t -> AV.Int (microsToDays t)
  TTimestampTz -> withLong v $ \t -> AV.Int (microsToDays t)
  TTimestampNs -> withLong v $ \t -> AV.Int (nanosToDays t)
  TTimestampTzNs -> withLong v $ \t -> AV.Int (nanosToDays t)
  other -> Left (TEUnsupported "day" other)
applyTransform Hour src v = case src of
  TTimestamp -> withLong v $ \t -> AV.Int (microsToHours t)
  TTimestampTz -> withLong v $ \t -> AV.Int (microsToHours t)
  TTimestampNs -> withLong v $ \t -> AV.Int (nanosToHours t)
  TTimestampTzNs -> withLong v $ \t -> AV.Int (nanosToHours t)
  other -> Left (TEUnsupported "hour" other)


-- ============================================================
-- Numeric helpers
-- ============================================================

truncateInt32 :: Int -> Int32 -> Int32
truncateInt32 w x = x - mod32 x (fromIntegral w)
  where
    mod32 a b = let r = a `mod` b in if r < 0 then r + b else r


truncateInt64 :: Int -> Int64 -> Int64
truncateInt64 w x = x - mod64 x (fromIntegral w)
  where
    mod64 a b = let r = a `mod` b in if r < 0 then r + b else r


-- ============================================================
-- Time conversions
--
-- All Iceberg time partitions are days/months/hours/years since the
-- Unix epoch (1970-01-01). Negative results indicate dates before 1970.
-- ============================================================

dateToYears :: Int32 -> Int32
dateToYears days = year - 1970
  where
    (year, _, _) = daysToYMD days


dateToMonths :: Int32 -> Int32
dateToMonths days =
  let (y, m, _) = daysToYMD days
  in 12 * (y - 1970) + (m - 1)


dateToDays :: Int32 -> Int32
dateToDays = id


microsToYears :: Int64 -> Int32
microsToYears us = dateToYears (microsToDays us)


microsToMonths :: Int64 -> Int32
microsToMonths us = dateToMonths (microsToDays us)


microsToDays :: Int64 -> Int32
microsToDays us = fromIntegral (us `divFloor` (86400 * 1_000_000))


microsToHours :: Int64 -> Int32
microsToHours us = fromIntegral (us `divFloor` (3600 * 1_000_000))


nanosToYears :: Int64 -> Int32
nanosToYears ns = microsToYears (ns `divFloor` 1000)


nanosToMonths :: Int64 -> Int32
nanosToMonths ns = microsToMonths (ns `divFloor` 1000)


nanosToDays :: Int64 -> Int32
nanosToDays ns = microsToDays (ns `divFloor` 1000)


nanosToHours :: Int64 -> Int32
nanosToHours ns = microsToHours (ns `divFloor` 1000)


{- | Floor-division that matches Java/Spark semantics for negative numerators
(so @-1 \`divFloor\` 86400000000@ rounds towards more negative, not zero).
-}
divFloor :: Int64 -> Int64 -> Int64
divFloor a b =
  let q = a `quot` b
      r = a - q * b
  in if (r /= 0) && ((r < 0) /= (b < 0)) then q - 1 else q


{- | Civil-from-days algorithm by Howard Hinnant (well known and BSD-licensed).
Returns @(year, month, day)@ for a date encoded as days since 1970-01-01.
-}
daysToYMD :: Int32 -> (Int32, Int32, Int32)
daysToYMD daysSinceEpoch =
  let z = fromIntegral daysSinceEpoch + 719468 :: Int
      era = (if z >= 0 then z else z - 146096) `div` 146097
      doe = z - era * 146097
      yoe = (doe - doe `div` 1460 + doe `div` 36524 - doe `div` 146096) `div` 365
      y = yoe + era * 400
      doy = doe - (365 * yoe + yoe `div` 4 - yoe `div` 100)
      mp = (5 * doy + 2) `div` 153
      d = doy - (153 * mp + 2) `div` 5 + 1
      m = if mp < 10 then mp + 3 else mp - 9
      yReal = if m <= 2 then y + 1 else y
  in (fromIntegral yReal, fromIntegral m, fromIntegral d)


-- ============================================================
-- Internal Avro extractors
-- ============================================================

withInt :: AV.Value -> (Int32 -> AV.Value) -> Either TransformError AV.Value
withInt (AV.Int n) f = Right (f n)
withInt AV.Null _ = Left TENullValue
withInt v _ = Left (TEUnsupported "int" (avSummary v))


withLong :: AV.Value -> (Int64 -> AV.Value) -> Either TransformError AV.Value
withLong (AV.Long n) f = Right (f n)
withLong AV.Null _ = Left TENullValue
withLong v _ = Left (TEUnsupported "long" (avSummary v))


withText :: AV.Value -> (Text -> AV.Value) -> Either TransformError AV.Value
withText (AV.String t) f = Right (f t)
withText AV.Null _ = Left TENullValue
withText v _ = Left (TEUnsupported "text" (avSummary v))


withBytes :: AV.Value -> (ByteString -> AV.Value) -> Either TransformError AV.Value
withBytes (AV.Bytes b) f = Right (f b)
withBytes (AV.Fixed b) f = Right (f b)
withBytes AV.Null _ = Left TENullValue
withBytes v _ = Left (TEUnsupported "bytes" (avSummary v))


avSummary :: AV.Value -> IcebergType
avSummary (AV.Bool _) = TBoolean
avSummary (AV.Int _) = TInt
avSummary (AV.Long _) = TLong
avSummary (AV.Float _) = TFloat
avSummary (AV.Double _) = TDouble
avSummary (AV.String _) = TString
avSummary (AV.Bytes _) = TBinary
avSummary (AV.Fixed _) = TBinary
avSummary _ = TUnknown
