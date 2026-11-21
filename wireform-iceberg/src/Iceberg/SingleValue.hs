{-# LANGUAGE BangPatterns #-}

{- | Iceberg single-value binary serialisation.

The Iceberg manifest format stores @lower_bounds@ \/ @upper_bounds@ as
"single-value" byte strings. Each Iceberg primitive type has a fixed
canonical encoding documented in Appendix D of the spec (and in the
Java @SingleValueParser@). This module covers the cases needed to
round-trip Parquet column statistics and to evaluate predicates.

Encodings:

- @int@\/@date@        -> 4-byte little-endian
- @long@\/@timestamp@  -> 8-byte little-endian
- @float@              -> 4-byte little-endian IEEE 754
- @double@             -> 8-byte little-endian IEEE 754
- @boolean@            -> single byte 0\/1
- @string@             -> UTF-8 bytes (no length prefix)
- @uuid@\/@fixed@      -> raw bytes
- @binary@             -> raw bytes
- @decimal(P,S)@       -> minimum two's-complement big-endian unscaled

The V3 @geometry@ and @geography@ types use a different encoding
(WKB POINT, 21 bytes) - see "Iceberg.Geometry".
-}
module Iceberg.SingleValue (
  -- * Encoders
  encodeBool,
  encodeInt32,
  encodeInt64,
  encodeFloat,
  encodeDouble,
  encodeString,
  encodeBytes,
  encodeDecimal,

  -- * Decoders
  decodeBool,
  decodeInt32,
  decodeInt64,
  decodeFloat,
  decodeDouble,
  decodeString,
  decodeDecimal,

  -- * Comparison
  compareSingleValueBy,
) where

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word32, Word64)
import GHC.Float qualified as GF
import Iceberg.Types (IcebergType (..))
import Wireform.Builder qualified as BB
import Prelude hiding (decodeFloat, encodeFloat)


-- ============================================================
-- Encoders
-- ============================================================

encodeBool :: Bool -> ByteString
encodeBool b = BS.singleton (if b then 1 else 0)


encodeInt32 :: Int32 -> ByteString
encodeInt32 = BL.toStrict . BB.toLazyByteString . BB.int32LE


encodeInt64 :: Int64 -> ByteString
encodeInt64 = BL.toStrict . BB.toLazyByteString . BB.int64LE


encodeFloat :: Float -> ByteString
encodeFloat = BL.toStrict . BB.toLazyByteString . BB.floatLE


encodeDouble :: Double -> ByteString
encodeDouble = BL.toStrict . BB.toLazyByteString . BB.doubleLE


encodeString :: Text -> ByteString
encodeString = TE.encodeUtf8


encodeBytes :: ByteString -> ByteString
encodeBytes = id


{- | Encode an unscaled decimal value as the minimum-width two's-complement
big-endian byte string. Iceberg writers and Parquet decimals follow this
representation.
-}
encodeDecimal :: Integer -> ByteString
encodeDecimal = BS.pack . go []
  where
    -- Stop once the remaining bits equal the sign extension of this byte.
    go acc value
      | (rest == 0 && byte < 0x80) || (rest == -1 && byte >= 0x80) = byte : acc
      | otherwise = go (byte : acc) rest
      where
        byte = fromIntegral value
        rest = value `shiftR` 8


-- ============================================================
-- Decoders
-- ============================================================

decodeBool :: ByteString -> Either String Bool
decodeBool bs = case BS.uncons bs of
  Just (0, _) -> Right False
  Just (_, _) -> Right True
  Nothing -> Left "decodeBool: empty"


decodeInt32 :: ByteString -> Either String Int32
decodeInt32 bs
  | BS.length bs == 4 = Right $! readInt32LE bs
  | otherwise = Left "decodeInt32: expected 4 bytes"


decodeInt64 :: ByteString -> Either String Int64
decodeInt64 bs
  | BS.length bs == 8 = Right $! readInt64LE bs
  | otherwise = Left "decodeInt64: expected 8 bytes"


decodeFloat :: ByteString -> Either String Float
decodeFloat bs
  | BS.length bs == 4 = Right (GF.castWord32ToFloat (fromIntegral (readInt32LE bs) :: Word32))
  | otherwise = Left "decodeFloat: expected 4 bytes"


decodeDouble :: ByteString -> Either String Double
decodeDouble bs
  | BS.length bs == 8 = Right (GF.castWord64ToDouble (fromIntegral (readInt64LE bs) :: Word64))
  | otherwise = Left "decodeDouble: expected 8 bytes"


decodeString :: ByteString -> Either String Text
decodeString bs = case TE.decodeUtf8' bs of
  Right t -> Right t
  Left e -> Left ("decodeString: " ++ show e)


decodeDecimal :: ByteString -> Either String Integer
decodeDecimal bs = case BS.uncons bs of
  Nothing -> Left "decodeDecimal: empty"
  Just (h, _) ->
    let w :: Integer
        w = BS.foldl' (\acc b -> (acc `shiftL` 8) .|. fromIntegral b) 0 bs
        len = BS.length bs
        signed
          | h .&. 0x80 /= 0 = w - (1 `shiftL` (8 * len))
          | otherwise = w
    in Right signed


readInt32LE :: ByteString -> Int32
readInt32LE bs =
  let b0 = fromIntegral (BS.index bs 0) :: Word32
      b1 = fromIntegral (BS.index bs 1) :: Word32
      b2 = fromIntegral (BS.index bs 2) :: Word32
      b3 = fromIntegral (BS.index bs 3) :: Word32
  in fromIntegral (b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24))


readInt64LE :: ByteString -> Int64
readInt64LE bs =
  let b0 = fromIntegral (BS.index bs 0) :: Word64
      b1 = fromIntegral (BS.index bs 1) :: Word64
      b2 = fromIntegral (BS.index bs 2) :: Word64
      b3 = fromIntegral (BS.index bs 3) :: Word64
      b4 = fromIntegral (BS.index bs 4) :: Word64
      b5 = fromIntegral (BS.index bs 5) :: Word64
      b6 = fromIntegral (BS.index bs 6) :: Word64
      b7 = fromIntegral (BS.index bs 7) :: Word64
      w =
        b0
          .|. (b1 `shiftL` 8)
          .|. (b2 `shiftL` 16)
          .|. (b3 `shiftL` 24)
          .|. (b4 `shiftL` 32)
          .|. (b5 `shiftL` 40)
          .|. (b6 `shiftL` 48)
          .|. (b7 `shiftL` 56)
  in fromIntegral w


-- ============================================================
-- Type-aware comparison
-- ============================================================

{- | Compare two single-value byte strings as values of the given Iceberg type.
Used by 'Iceberg.Expression' to reason about lower\/upper bounds without
decoding into full 'Avro.Value' trees.
-}
compareSingleValueBy :: IcebergType -> ByteString -> ByteString -> Either String Ordering
compareSingleValueBy ty a b = case ty of
  TBoolean -> liftCompare decodeBool a b
  TInt -> liftCompare decodeInt32 a b
  TDate -> liftCompare decodeInt32 a b
  TLong -> liftCompare decodeInt64 a b
  TTimestamp -> liftCompare decodeInt64 a b
  TTimestampTz -> liftCompare decodeInt64 a b
  TTime -> liftCompare decodeInt64 a b
  TTimestampNs -> liftCompare decodeInt64 a b
  TTimestampTzNs -> liftCompare decodeInt64 a b
  TFloat -> liftCompare Iceberg.SingleValue.decodeFloat a b
  TDouble -> liftCompare Iceberg.SingleValue.decodeDouble a b
  TString -> liftCompare decodeString a b
  TBinary -> Right (compare a b)
  TFixed _ -> Right (compare a b)
  TUuid -> Right (compare a b)
  TDecimal _ _ -> liftCompare decodeDecimal a b
  _ -> Left "compareSingleValueBy: unsupported type"
  where
    liftCompare :: Ord c => (ByteString -> Either String c) -> ByteString -> ByteString -> Either String Ordering
    liftCompare dec x y = do
      x' <- dec x
      y' <- dec y
      Right (compare x' y')
