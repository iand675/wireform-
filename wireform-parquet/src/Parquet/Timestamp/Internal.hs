{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Lossless timestamp storage and checked projections. The extension names
below belong to wireform; they are not canonical Apache Arrow extensions.
-}
module Parquet.Timestamp.Internal where

import Arrow.Column qualified as C
import Arrow.Types qualified as A
import Control.Monad (unless)
import Data.Bits (complement, finiteBitSize, shiftL, shiftR, (.|.))
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
import Data.Word (Word8, byteSwap32, byteSwap64)
import Foreign.Ptr (castPtr)
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Exts (Int (I#), Int64#, indexWord8OffAddrAsWord32#, indexWord8OffAddrAsWord64#, quotRemWord2#, quotRemWord#, word64ToWord#, wordToWord64#)
import GHC.Int (Int64 (I64#))
import GHC.Ptr (Ptr (..))
import GHC.Word (Word32 (W32#), Word64 (W64#))
import Parquet.Types qualified as P
import System.IO.Unsafe (unsafeDupablePerformIO)


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
exactly the same encoding and parameters. Timestamp projections can change
units exactly, and preserve the UTC/local semantics of signed96 storage.
-}
fieldsCompatible :: A.Field -> A.Field -> Either String Bool
fieldsCompatible source target = do
  actual <- timestampEncoding source
  wanted <- timestampEncoding target
  Right $ case (actual, wanted, A.fieldType target) of
    (Just encoding, Just same, _) -> encoding == same
    (Just _, Nothing, A.AFixedSizeBinary 12) -> not (V.any ((== "ARROW:extension:name") . fst) (A.fieldMetadata target))
    (Just (SignedTimestamp96 adjusted _), Nothing, A.ATimestamp _ zone) -> noExtension target && adjusted == maybe False (const True) zone
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
    (A.ATimestamp unit _, C.ColFixedSizeBinary 12 count validity bytes)
      | count <= BS.length bytes `quot` 12 ->
          unsafeDupablePerformIO $ U.unsafeUseAsCString bytes $ \raw -> do
            let ptr = castPtr raw
                convert = convertTimestamps count validity
            values <- case encoding of
              SignedTimestamp96 _ sourceUnit ->
                let sourceNanos = nanosPerUnit (arrowUnit sourceUnit)
                    targetNanos = nanosPerUnit unit
                in case compare sourceNanos targetNanos of
                     EQ -> convert (signed64At# ptr)
                     GT ->
                       let !factor = sourceNanos `quot` targetNanos
                           !lower = minBound `quot` factor
                           !upper = maxBound `quot` factor
                       in convert $ \offset -> case signed64At# ptr offset of
                            (# | value #)
                              | let n = I64# value
                              , lower <= n && n <= upper -> unboxTimestamp (n * factor)
                            _ -> (# timestampOverflow | #)
                     LT ->
                       let !divisor = fromIntegral (targetNanos `quot` sourceNanos)
                       in convert (divideSigned96# divisor ptr)
              JulianInt96 -> convert (julian64At# unit ptr)
            pure (values >>= C.mkPrim C.PTimestamp validity)
    _ -> Left "Parquet.Timestamp: expected FixedSizeBinary(12) column"


convertTimestamps
  :: Int
  -> Maybe C.Validity
  -> (Int -> (# String | Int64# #))
  -> IO (Either String (S.Vector Int64))
convertTimestamps count validity project = do
  output <- M.unsafeNew count
  let loop !index
        | index == count = Right <$> S.unsafeFreeze output
        | not (C.isValidAt validity index) = M.unsafeWrite output index 0 >> loop (index + 1)
        | otherwise = case project (index * 12) of
            (# message | #) -> pure (Left message)
            (# | value #) -> M.unsafeWrite output index (I64# value) >> loop (index + 1)
  loop 0
{-# INLINE convertTimestamps #-}


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


-- Each address read is unaligned and confined to one validated 12-byte
-- cell. The input remains alive inside unsafeUseAsCString.
lowAt :: Ptr Word8 -> Int -> Word64
lowAt (Ptr ptr) (I# offset) =
  let word = W64# (indexWord8OffAddrAsWord64# ptr offset)
  in case targetByteOrder of
       LittleEndian -> word
       BigEndian -> byteSwap64 word
{-# INLINE lowAt #-}


highAt :: Ptr Word8 -> Int -> Int32
highAt (Ptr ptr) offset =
  case offset + 8 of
    I# at ->
      let word :: Word32
          word = W32# (indexWord8OffAddrAsWord32# ptr at)
      in fromIntegral $ case targetByteOrder of
           LittleEndian -> word
           BigEndian -> byteSwap32 word
{-# INLINE highAt #-}


signed64At# :: Ptr Word8 -> Int -> (# String | Int64# #)
signed64At# bytes offset =
  case fromIntegral (lowAt bytes offset) of
    low@(I64# value)
      | highAt bytes offset == (if low < 0 then -1 else 0) -> (# | value #)
      | otherwise -> (# timestampOverflow | #)
{-# INLINE signed64At# #-}


-- Convert the two limbs to a magnitude. The high limb must be below the
-- divisor for the machine division to return a quotient in one word.
divideSigned96# :: Word64 -> Ptr Word8 -> Int -> (# String | Int64# #)
divideSigned96# divisor bytes offset
  | finiteBitSize (0 :: Word) == 64 =
      if magnitudeHigh >= divisor
        then (# timestampOverflow | #)
        else case (magnitudeHigh, magnitudeLow, divisor) of
          (W64# h, W64# l, W64# d) ->
            case if magnitudeHigh == 0
                   then quotRemWord# (word64ToWord# l) (word64ToWord# d)
                   else quotRemWord2# (word64ToWord# h) (word64ToWord# l) (word64ToWord# d) of
              (# q, r #) -> finish (W64# (wordToWord64# q)) (W64# (wordToWord64# r))
  | otherwise =
      let (q, r) = timestamp96ToInteger (Timestamp96 high low) `quotRem` toInteger divisor
      in if q < toInteger (minBound :: Int64) || q > toInteger (maxBound :: Int64)
           then (# timestampOverflow | #)
           else if r /= 0 then (# timestampPrecisionLoss | #) else unboxTimestamp (fromInteger q)
  where
    !low = lowAt bytes offset
    high = highAt bytes offset
    !negative = high < 0
    magnitudeLow = if negative then negate low else low
    magnitudeHigh
      | not negative = fromIntegral high
      | low == 0 = negate (fromIntegral high)
      | otherwise = fromIntegral (complement high)
    finish q r
      | r /= 0 = (# timestampPrecisionLoss | #)
      | q > (if negative then 0x8000000000000000 else 0x7fffffffffffffff) = (# timestampOverflow | #)
      | otherwise = unboxTimestamp (fromIntegral (if negative then negate q else q))
{-# INLINE divideSigned96# #-}


nanosPerUnit :: A.TimeUnit -> Int64
nanosPerUnit A.Second = 1000000000
nanosPerUnit A.Millisecond = 1000000
nanosPerUnit A.Microsecond = 1000
nanosPerUnit A.Nanosecond = 1


timestampOverflow, timestampPrecisionLoss :: String
timestampOverflow = "Parquet.Timestamp: timestamp exceeds Arrow Int64 range"
timestampPrecisionLoss = "Parquet.Timestamp: timestamp projection loses sub-unit precision"


julian64At# :: A.TimeUnit -> Ptr Word8 -> Int -> (# String | Int64# #)
julian64At# unit bytes offset =
  let factor = nanosPerUnit unit
      unitsPerDay = 86400000000000 `quot` factor
      nanos = fromIntegral (lowAt bytes offset) :: Int64
      (carry, normalized) = nanos `divMod` 86400000000000
      day = fromIntegral (highAt bytes offset) - 2440588 + carry :: Int64
      (fraction, rest) = normalized `quotRem` factor
      (lowDay, lowFraction) = (minBound :: Int64) `divMod` unitsPerDay
      (highDay, highFraction) = (maxBound :: Int64) `divMod` unitsPerDay
  in if rest /= 0
       then (# timestampPrecisionLoss | #)
       else
         if day < lowDay || day > highDay || (day == lowDay && fraction < lowFraction) || (day == highDay && fraction > highFraction)
           then (# timestampOverflow | #)
           else unboxTimestamp (day * unitsPerDay + fraction)
{-# INLINE julian64At# #-}


unboxTimestamp :: Int64 -> (# String | Int64# #)
unboxTimestamp (I64# value) = (# | value #)
{-# INLINE unboxTimestamp #-}
