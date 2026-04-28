{-# LANGUAGE BangPatterns #-}
-- | Avro schema fingerprinting using Parsing Canonical Form.
--
-- Implements CRC-64-AVRO and MD5 fingerprints per the Avro specification.
-- The Parsing Canonical Form (PCF) normalizes a schema to a deterministic
-- JSON representation that strips doc, aliases, default, order, and
-- extra whitespace.
module Avro.Fingerprint
  ( avroFingerprint
  , avroFingerprintMD5
  , parsingCanonicalForm
  ) where

import Data.Bits (shiftR, xor, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word8, Word64)

import qualified Crypto.Hash.MD5 as MD5

import Avro.Schema (AvroType(..), AvroSchema(..), AvroField(..))

-- | Compute CRC-64-AVRO fingerprint of a schema's Parsing Canonical Form.
-- Returns an 8-byte ByteString.
avroFingerprint :: AvroType -> ByteString
avroFingerprint ty = crc64Avro (parsingCanonicalForm ty)

-- | Compute MD5 fingerprint of a schema's Parsing Canonical Form.
-- Returns a 16-byte ByteString.
avroFingerprintMD5 :: AvroType -> ByteString
avroFingerprintMD5 ty = MD5.hash (parsingCanonicalForm ty)

-- | Get Parsing Canonical Form of a schema (normalized JSON as bytes).
parsingCanonicalForm :: AvroType -> ByteString
parsingCanonicalForm ty = BSC.pack (pcf ty)

pcf :: AvroType -> String
pcf (AvroPrimitive s) = pcfSchema s
pcf (AvroRecord{avroRecordName = name, avroRecordFields = fields}) =
  "{\"name\":\"" ++ T.unpack name ++ "\",\"type\":\"record\",\"fields\":["
  ++ intercalate "," (map pcfField (V.toList fields))
  ++ "]}"
pcf (AvroEnum{avroEnumName = name, avroEnumSymbols = syms}) =
  "{\"name\":\"" ++ T.unpack name ++ "\",\"type\":\"enum\",\"symbols\":["
  ++ intercalate "," (map (\s -> "\"" ++ T.unpack s ++ "\"") (V.toList syms))
  ++ "]}"
pcf (AvroArray{avroArrayItems = items}) =
  "{\"type\":\"array\",\"items\":" ++ pcf items ++ "}"
pcf (AvroMap{avroMapValues = vals}) =
  "{\"type\":\"map\",\"values\":" ++ pcf vals ++ "}"
pcf (AvroUnion{avroUnionBranches = branches}) =
  "[" ++ intercalate "," (map pcf (V.toList branches)) ++ "]"
pcf (AvroFixed{avroFixedName = name, avroFixedSize = sz}) =
  "{\"name\":\"" ++ T.unpack name ++ "\",\"type\":\"fixed\",\"size\":" ++ show sz ++ "}"
pcf (AvroLogical{avroLogicalBase = base}) = pcf base

pcfSchema :: AvroSchema -> String
pcfSchema AvroNull   = "\"null\""
pcfSchema AvroBool   = "\"boolean\""
pcfSchema AvroInt    = "\"int\""
pcfSchema AvroLong   = "\"long\""
pcfSchema AvroFloat  = "\"float\""
pcfSchema AvroDouble = "\"double\""
pcfSchema AvroBytes  = "\"bytes\""
pcfSchema AvroString = "\"string\""
pcfSchema (AvroSchemaRef n) = "\"" ++ T.unpack n ++ "\""

pcfField :: AvroField -> String
pcfField fld =
  "{\"name\":\"" ++ T.unpack (avroFieldName fld) ++ "\",\"type\":" ++ pcf (avroFieldType fld) ++ "}"

-- | Avro CRC-64 (a 64-bit Rabin fingerprint as defined in the Avro
-- specification, single-object encoding section). The algorithm uses
-- the @EMPTY@ constant as both the initial value /and/ the
-- polynomial; this is a deliberate departure from the ECMA-182 CRC-64.
--
-- > fp[0] = EMPTY
-- > fp[i+1] = (fp[i] >>> 8) xor TABLE[(fp[i] xor buf[i]) & 0xff]
-- > TABLE[k] = iterate (\\v -> (v >>> 1) xor (EMPTY .&. -(v .&. 1)))
-- >                    8 (fromIntegral k)
crc64Avro :: ByteString -> ByteString
crc64Avro !bs =
  let !crc = BS.foldl' crc64Update emptyConst bs
  in word64ToBS crc

emptyConst :: Word64
emptyConst = 0xc15d213aa4d7a795

crc64Update :: Word64 -> Word8 -> Word64
crc64Update !crc !b =
  let !idx = (crc `xor` fromIntegral b) .&. 0xff
      !tableEntry = crc64Step8 idx
  in (crc `shiftR` 8) `xor` tableEntry

-- | One @TABLE[k]@ entry computed inline (no static array). Following
-- the spec literally: 8 iterations of @(v >>> 1) xor (EMPTY & -(v & 1))@.
{-# INLINE crc64Step8 #-}
crc64Step8 :: Word64 -> Word64
crc64Step8 !k0 = step (step (step (step (step (step (step (step k0)))))))
  where
    step !v =
      let !lsb = v .&. 1
          !mask = negate lsb
      in (v `shiftR` 1) `xor` (emptyConst .&. mask)

word64ToBS :: Word64 -> ByteString
word64ToBS !w = BS.pack
  [ fromIntegral (w .&. 0xFF)
  , fromIntegral ((w `shiftR` 8) .&. 0xFF)
  , fromIntegral ((w `shiftR` 16) .&. 0xFF)
  , fromIntegral ((w `shiftR` 24) .&. 0xFF)
  , fromIntegral ((w `shiftR` 32) .&. 0xFF)
  , fromIntegral ((w `shiftR` 40) .&. 0xFF)
  , fromIntegral ((w `shiftR` 48) .&. 0xFF)
  , fromIntegral ((w `shiftR` 56) .&. 0xFF)
  ]
