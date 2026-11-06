{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedSums #-}

{- | Lossless timestamp storage and checked projections. The extension names
below belong to wireform; they are not canonical Apache Arrow extensions.
-}
module Parquet.Timestamp.Internal where

import Arrow.Column qualified as C
import Arrow.Types qualified as A
import Control.Monad (unless)
import Control.Monad.ST (runST)
import Data.Bits (shiftL, shiftR, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as U
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as S
import Data.Vector.Storable.Mutable qualified as M
import Data.Word (Word64)
import Parquet.Types qualified as P


{- | A signed two's-complement 96-bit integer, with a signed high word and
unsigned low word. Every constructor value is valid. Units belong to the
column's 'TimestampEncoding', not to this integer.
-}
data Timestamp96 = Timestamp96
  { timestamp96High :: {-# UNPACK #-} !Int32
  , timestamp96Low :: {-# UNPACK #-} !Word64
  }
  deriving (Show, Eq, Ord)


{- | Distinct 12-byte timestamp formats. 'SignedTimestamp96' counts the named
units since the Unix epoch. 'JulianInt96' stores a signed Julian day and
signed nanoseconds, as used by legacy Parquet writers. Non-normalized
nanoseconds are preserved; scalar/projection helpers normalize arithmetically.
-}
data TimestampEncoding = SignedTimestamp96 !Bool !P.LtTimeUnit | JulianInt96
  deriving (Show, Eq)


-- | Construct a timestamp count without truncation.
timestamp96FromInteger :: Integer -> Either String Timestamp96
timestamp96FromInteger value
  | value < negate (2 ^ (95 :: Int)) || value >= 2 ^ (95 :: Int) = Left "Parquet.Timestamp: count exceeds signed96"
  | otherwise = Right (Timestamp96 (fromInteger (value `shiftR` 64)) (fromInteger value))


{- | Recover the full signed count. This scalar convenience function uses
arbitrary-precision arithmetic; column projection uses concrete word loops.
-}
timestamp96ToInteger :: Timestamp96 -> Integer
timestamp96ToInteger (Timestamp96 high low) = (toInteger high `shiftL` 64) + toInteger low


-- | The modern TIMESTAMP fixed12 representation: signed little-endian counts.
encodeTimestamp96 :: Timestamp96 -> ByteString
encodeTimestamp96 (Timestamp96 high low) = BL.toStrict (B.toLazyByteString (B.word64LE low <> B.int32LE high))


-- | Decode exactly 12 bytes; every signed96 bit pattern is a valid count.
decodeTimestamp96 :: ByteString -> Either String Timestamp96
decodeTimestamp96 bytes
  | BS.length bytes /= 12 = Left "Parquet.Timestamp: expected12bytes"
  | otherwise = Right (Timestamp96 (fromIntegral (wordAt bytes 8 4)) (wordAt bytes 0 8))


{- | Decode legacy INT96 into a full signed96 count of nanoseconds since the
Unix epoch. Signed/non-normalized nanoseconds are included without wrapping.
-}
decodeJulianInt96 :: ByteString -> Either String Timestamp96
decodeJulianInt96 bytes
  | BS.length bytes /= 12 = Left "Parquet.Timestamp: expected12bytes"
  | otherwise = timestamp96FromInteger (toInteger days * 86400000000000 + toInteger nanos)
  where
    nanos = fromIntegral (wordAt bytes 0 8) :: Int64
    days = fromIntegral (fromIntegral (wordAt bytes 8 4) :: Int32) - 2440588 :: Int64


{- | Make a lossless Arrow field backed by FixedSizeBinary(12). Modern storage
uses extension @wireform.parquet.timestamp96@ with metadata
@millis:utc@, @millis:local@, @micros:utc@, @micros:local@,
@nanos:utc@ or @nanos:local@. Legacy storage uses
@wireform.parquet.int96@ with empty metadata. The 12 payload bytes retain
the original Parquet representation, independently of Arrow endianness.
-}
timestampField :: Text -> Bool -> TimestampEncoding -> A.Field
timestampField name nullable encoding =
  (A.defaultField name nullable (A.AFixedSizeBinary 12) V.empty)
    { A.fieldMetadata = V.fromList [("ARROW:extension:name", extensionName encoding), ("ARROW:extension:metadata", parameters encoding)]
    }


extensionName :: TimestampEncoding -> Text
extensionName SignedTimestamp96 {} = "wireform.parquet.timestamp96"
extensionName JulianInt96 = "wireform.parquet.int96"


parameters :: TimestampEncoding -> Text
parameters JulianInt96 = ""
parameters (SignedTimestamp96 adjusted unit) = unitName unit <> if adjusted then ":utc" else ":local"


unitName :: P.LtTimeUnit -> Text
unitName P.LtMillis = "millis"
unitName P.LtMicros = "micros"
unitName P.LtNanos = "nanos"


isTimestampField :: A.Field -> Bool
isTimestampField = V.any (\entry -> entry == ("ARROW:extension:name", "wireform.parquet.timestamp96") || entry == ("ARROW:extension:name", "wireform.parquet.int96")) . A.fieldMetadata


{- | Inspect and validate a wireform timestamp extension. Other fields return
Nothing; duplicate names, malformed parameters and incompatible storage fail.
-}
timestampEncoding :: A.Field -> Either String (Maybe TimestampEncoding)
timestampEncoding field
  | not (isTimestampField field) = Right Nothing
  | otherwise = do
      encoding <- case (V.toList (values "ARROW:extension:name"), V.toList (values "ARROW:extension:metadata")) of
        (["wireform.parquet.int96"], []) -> Right JulianInt96
        (["wireform.parquet.int96"], [""]) -> Right JulianInt96
        (["wireform.parquet.timestamp96"], [text]) -> case text of
          "millis:utc" -> Right (SignedTimestamp96 True P.LtMillis)
          "millis:local" -> Right (SignedTimestamp96 False P.LtMillis)
          "micros:utc" -> Right (SignedTimestamp96 True P.LtMicros)
          "micros:local" -> Right (SignedTimestamp96 False P.LtMicros)
          "nanos:utc" -> Right (SignedTimestamp96 True P.LtNanos)
          "nanos:local" -> Right (SignedTimestamp96 False P.LtNanos)
          _ -> invalid
        _ -> invalid
      unless (A.fieldType field == A.AFixedSizeBinary 12 && A.fieldDictionary field == Nothing && V.null (A.fieldChildren field)) invalid
      Right (Just encoding)
  where
    values key = V.map snd (V.filter ((== key) . fst) (A.fieldMetadata field))
    invalid = Left "Parquet.Timestamp: invalid timestamp extension/storage"


schemaEncoding :: P.SchemaElement -> Either String (Maybe TimestampEncoding)
schemaEncoding field = case (P.seType field, P.seLogicalType field) of
  (Just P.PTFixedLenByteArray, Just (P.LTTimestamp adjusted unit))
    | P.seTypeLength field == Just 12 -> Right (Just (SignedTimestamp96 adjusted unit))
    | otherwise -> Left "Parquet.Timestamp: TIMESTAMP fixed storage must have length12"
  (_, Just P.LTTimestamp {}) | P.seType field /= Just P.PTInt64 -> Left "Parquet.Timestamp: unsupported TIMESTAMP storage"
  (Just P.PTInt96, _) -> Right (Just JulianInt96)
  _ -> Right Nothing


{- | Decide a typed projection before reading values. Removing the extension
explicitly permits raw FixedSizeBinary(12) access. Retaining it requires
exactly the same encoding and parameters. Signed96 narrows only to the same
unit and UTC/local semantics; INT96 supports exact second/ms/us/ns conversion.
-}
fieldsCompatible :: A.Field -> A.Field -> Either String Bool
fieldsCompatible source target = do
  actual <- timestampEncoding source
  wanted <- timestampEncoding target
  Right $ case (actual, wanted, A.fieldType target) of
    (Just encoding, Just same, _) -> encoding == same
    (Just _, Nothing, A.AFixedSizeBinary 12) -> not (V.any ((== "ARROW:extension:name") . fst) (A.fieldMetadata target))
    (Just (SignedTimestamp96 adjusted unit), Nothing, A.ATimestamp targetUnit zone) -> noExtension target && arrowUnit unit == targetUnit && adjusted == maybe False (const True) zone
    (Just JulianInt96, Nothing, A.ATimestamp _ Nothing) -> noExtension target
    _ -> False


noExtension :: A.Field -> Bool
noExtension = not . V.any ((== "ARROW:extension:name") . fst) . A.fieldMetadata


arrowUnit :: P.LtTimeUnit -> A.TimeUnit
arrowUnit P.LtMillis = A.Millisecond
arrowUnit P.LtMicros = A.Microsecond
arrowUnit P.LtNanos = A.Nanosecond


{- | Convert an extension column to a checked Arrow timestamp, or retain its
exact storage. Null payloads are never inspected. Overflow and sub-unit
precision loss fail, including below nested nullable ancestors after assembly.
-}
projectTimestampColumn :: A.Field -> A.Field -> C.ColumnArray -> Either String C.ColumnArray
projectTimestampColumn source target column = do
  compatible <- fieldsCompatible source target
  unless compatible (Left "Parquet.Timestamp: incompatible timestamp projection")
  encoding <- timestampEncoding source >>= maybe (Left "Parquet.Timestamp: source is not a timestamp extension") Right
  unless (A.fieldNullable target || C.nullCount column == 0) (Left "Parquet.Timestamp: non-nullable target contains nulls")
  case (A.fieldType target, column) of
    (A.AFixedSizeBinary 12, C.ColFixedSizeBinary 12 _ _ _) -> validateTimestampColumn encoding column
    (A.ATimestamp unit _, C.ColFixedSizeBinary 12 count validity bytes) -> do
      let project = case encoding of
            SignedTimestamp96 {} -> signed64At# bytes
            JulianInt96 -> julian64At# unit bytes
      values <- runST $ do
        output <- M.unsafeNew count
        let loop !index
              | index == count = Right <$> S.unsafeFreeze output
              | not (C.isValidAt validity index) = M.unsafeWrite output index 0 >> loop (index + 1)
              | otherwise = case project (index * 12) of
                  (# message | #) -> pure (Left message)
                  (# | value #) -> M.unsafeWrite output index value >> loop (index + 1)
        loop 0
      C.mkPrim C.PTimestamp validity values
    _ -> Left "Parquet.Timestamp: expected FixedSizeBinary(12) column"


validateTimestampColumn :: TimestampEncoding -> C.ColumnArray -> Either String C.ColumnArray
validateTimestampColumn _ column = case column of
  C.ColFixedSizeBinary 12 _ _ _ -> Right column
  _ -> Left "Parquet.Timestamp: expected FixedSizeBinary(12) column"


wordAt :: ByteString -> Int -> Int -> Word64
wordAt bytes offset width = loop 0 0
  where
    loop !index !value
      | index == width = value
      | otherwise = loop (index + 1) (value .|. (fromIntegral (U.unsafeIndex bytes (offset + index)) `shiftL` (index * 8)))
{-# INLINE wordAt #-}


signed64At# :: ByteString -> Int -> (# String | Int64 #)
signed64At# bytes offset =
  let low = fromIntegral (wordAt bytes offset 8) :: Int64
      high = fromIntegral (wordAt bytes (offset + 8) 4) :: Int32
  in if high == (if low < 0 then -1 else 0)
       then (# | low #)
       else (# "Parquet.Timestamp: timestamp exceeds Arrow Int64 range" | #)
{-# INLINE signed64At# #-}


julian64At# :: A.TimeUnit -> ByteString -> Int -> (# String | Int64 #)
julian64At# unit bytes offset =
  let factor = case unit of A.Second -> 1000000000; A.Millisecond -> 1000000; A.Microsecond -> 1000; A.Nanosecond -> 1
      unitsPerDay = 86400000000000 `quot` factor
      nanos = fromIntegral (wordAt bytes offset 8) :: Int64
      (carry, normalized) = nanos `divMod` 86400000000000
      day = fromIntegral (fromIntegral (wordAt bytes (offset + 8) 4) :: Int32) - 2440588 + carry :: Int64
      (fraction, rest) = normalized `quotRem` factor
      (lowDay, lowFraction) = (minBound :: Int64) `divMod` unitsPerDay
      (highDay, highFraction) = (maxBound :: Int64) `divMod` unitsPerDay
  in if rest /= 0
       then (# "Parquet.Timestamp: timestamp projection loses sub-unit precision" | #)
       else
         if day < lowDay || day > highDay || (day == lowDay && fraction < lowFraction) || (day == highDay && fraction > highFraction)
           then (# "Parquet.Timestamp: timestamp exceeds Arrow Int64 range" | #)
           else (# | day * unitsPerDay + fraction #)
{-# INLINE julian64At# #-}
