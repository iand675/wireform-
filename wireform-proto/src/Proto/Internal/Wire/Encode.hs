{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Low-level, high-performance wire format encoding primitives.

__Stability:__ exposed for use by wireform-proto-generated code; not
part of the stable public API.

All encoding is done via 'Wireform.Builder' for zero-copy
concatenation and efficient output. Varint encoding is unrolled for
common small values.
-}
module Proto.Internal.Wire.Encode (
  -- * Varint encoding
  putVarint,
  putVarint32,
  putVarintSigned,
  putSVarint32,
  putSVarint64,

  -- * Fixed-width encoding
  putFixed32,
  putFixed64,
  putFloat,
  putDouble,

  -- * Length-delimited
  putLengthDelimited,
  putByteString,
  putText,

  -- * Tags
  putTag,

  -- * Size calculation (avoids double-encoding for submessages)
  varintSize,
  varintSize32,
  tagSize,
  fieldVarintSize,
  fieldSVarint32Size,
  fieldSVarint64Size,
  fieldFixed32Size,
  fieldFixed64Size,
  fieldFloatSize,
  fieldDoubleSize,
  fieldBoolSize,
  fieldBytesSize,
  fieldTextSize,
  fieldMessageSize,

  -- * Pre-computed tags (for generated code)
  precomputeTag,
  putPrecomputedTag,

  -- * Helpers
  zigZag32,
  zigZag64,
) where

import Data.Bits (countLeadingZeros, shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text.Array qualified as TA
import Data.Text.Foreign qualified as TF
import Data.Text.Internal qualified as TI
import Data.Word (Word32, Word64)
import GHC.Exts (ByteArray#, Int (..), Int#, copyByteArrayToAddr#, plusAddr#)
import Proto.Internal.Wire (WireType, fieldTag)
import Wireform.Builder (Builder)
import Wireform.Builder qualified as B
import Wireform.Builder.FastBuilder qualified as FBI


{- | Get the UTF-8 byte length of a Text without allocating a ByteString.
On text >= 2.0 the internal representation is already UTF-8, so
'Data.Text.Foreign.lengthWord8' just reads the length slot of the
'Text' record -- O(1), no allocation.
-}
textUtf8Length :: Text -> Int
textUtf8Length = TF.lengthWord8
{-# INLINE textUtf8Length #-}


{- | Encode a varint (unsigned). Unrolled for values that fit in 1-3 bytes
(covers field tags up to ~250k and values up to 2^21).
-}
putVarint :: Word64 -> Builder
putVarint !n
  | n < 0x80 =
      B.word8 (fromIntegral n)
  | n < 0x4000 =
      B.word8 (fromIntegral (n .&. 0x7F) .|. 0x80)
        <> B.word8 (fromIntegral (n `shiftR` 7))
  | n < 0x200000 =
      B.word8 (fromIntegral (n .&. 0x7F) .|. 0x80)
        <> B.word8 (fromIntegral ((n `shiftR` 7) .&. 0x7F) .|. 0x80)
        <> B.word8 (fromIntegral (n `shiftR` 14))
  | otherwise = putVarintSlow n
{-# INLINE putVarint #-}


putVarintSlow :: Word64 -> Builder
putVarintSlow = go
  where
    go !n
      | n < 0x80 = B.word8 (fromIntegral n)
      | otherwise = B.word8 (fromIntegral (n .&. 0x7F) .|. 0x80) <> go (n `shiftR` 7)


{- | Encode a 32-bit unsigned varint without promoting to Word64.
A Word32 needs at most 5 varint bytes.
-}
putVarint32 :: Word32 -> Builder
putVarint32 !n
  | n < 0x80 =
      B.word8 (fromIntegral n)
  | n < 0x4000 =
      B.word8 (fromIntegral (n .&. 0x7F) .|. 0x80)
        <> B.word8 (fromIntegral (n `shiftR` 7))
  | otherwise = putVarint32Slow n
{-# INLINE putVarint32 #-}


putVarint32Slow :: Word32 -> Builder
putVarint32Slow = go
  where
    go !n
      | n < 0x80 = B.word8 (fromIntegral n)
      | otherwise = B.word8 (fromIntegral (n .&. 0x7F) .|. 0x80) <> go (n `shiftR` 7)


-- | Encode a signed varint (two's complement, always 10 bytes for negatives).
putVarintSigned :: Int64 -> Builder
putVarintSigned n = putVarint (fromIntegral n)
{-# INLINE putVarintSigned #-}


-- | Encode a sint32 using zigzag encoding.
putSVarint32 :: Int32 -> Builder
putSVarint32 n = putVarint (fromIntegral (zigZag32 n))
{-# INLINE putSVarint32 #-}


-- | Encode a sint64 using zigzag encoding.
putSVarint64 :: Int64 -> Builder
putSVarint64 n = putVarint (zigZag64 n)
{-# INLINE putSVarint64 #-}


{- | ZigZag encoding for 32-bit signed integers.
Maps signed integers to unsigned: 0 -> 0, -1 -> 1, 1 -> 2, -2 -> 3, ...
-}
zigZag32 :: Int32 -> Word32
zigZag32 n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 31))
{-# INLINE zigZag32 #-}


-- | ZigZag encoding for 64-bit signed integers.
zigZag64 :: Int64 -> Word64
zigZag64 n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 63))
{-# INLINE zigZag64 #-}


-- | Encode a 32-bit fixed value (little-endian).
putFixed32 :: Word32 -> Builder
putFixed32 = B.word32LE
{-# INLINE putFixed32 #-}


-- | Encode a 64-bit fixed value (little-endian).
putFixed64 :: Word64 -> Builder
putFixed64 = B.word64LE
{-# INLINE putFixed64 #-}


-- | Encode a float (little-endian).
putFloat :: Float -> Builder
putFloat = B.floatLE
{-# INLINE putFloat #-}


-- | Encode a double (little-endian).
putDouble :: Double -> Builder
putDouble = B.doubleLE
{-# INLINE putDouble #-}


-- | Encode a length-delimited field: varint length prefix + payload.
putLengthDelimited :: ByteString -> Builder
putLengthDelimited bs =
  putVarint (fromIntegral (BS.length bs)) <> B.byteStringCopy bs
{-# INLINE putLengthDelimited #-}


-- | Encode a bytes field.
putByteString :: ByteString -> Builder
putByteString = putLengthDelimited
{-# INLINE putByteString #-}


{- | Write a slice of an unpinned 'ByteArray#' directly into the
fast-builder's current buffer chunk.

This avoids the intermediate pinned 'BS.ByteString' allocation that
@'B.byteStringCopy' . 'TE.encodeUtf8'@ would force. We use
fast-builder's 'ensureBytes' to guarantee buffer space, then
construct a raw 'Builder' that 'copyByteArrayToAddr#' 's the slice
from the text's unpinned backing store into the builder's pinned
buffer. Net cost is one buffer write, no heap allocation.
-}
byteArraySliceBuilder :: ByteArray# -> Int# -> Int# -> Builder
byteArraySliceBuilder arr# off# len# =
  FBI.ensureBytes (I# len#)
    <> FBI.Builder
      ( \_ (# cur#, end#, s# #) ->
          case copyByteArrayToAddr# arr# off# cur# len# s# of
            s'# -> (# plusAddr# cur# len#, end#, s'# #)
      )
{-# INLINE byteArraySliceBuilder #-}


{- | Encode a string field (UTF-8).

Avoids the intermediate pinned 'BS.ByteString' allocation that the
naive @'putVarint' (length bs) <> 'B.byteStringCopy' bs@ form forces
via 'TE.encodeUtf8'. Instead, the length prefix is read from the
'Text' record's length slot in O(1) and the payload bytes are
streamed straight from the 'Text' 's underlying 'ByteArray#' into
the builder buffer via 'byteArraySliceBuilder'.
-}
putText :: Text -> Builder
putText (TI.Text (TA.ByteArray arr#) (I# off#) lenI@(I# len#)) =
  putVarint (fromIntegral lenI) <> byteArraySliceBuilder arr# off# len#
{-# INLINE putText #-}


{- | Encode a field tag (field number + wire type) as a varint.

For field numbers 1-15 (the vast majority in practice), the tag
fits in a single byte. We emit B.word8 directly, avoiding the
putVarint branch chain entirely.
-}
putTag :: Int -> WireType -> Builder
putTag fn wt
  | tagVal < 0x80 = B.word8 (fromIntegral tagVal)
  | otherwise = putVarint tagVal
  where
    !tagVal = fieldTag fn wt
{-# INLINE putTag #-}


{- | Pre-compute the tag bytes for a field at definition time.
Returns a strict ByteString containing the varint-encoded tag.
Use with 'B.byteString' in generated code to avoid re-encoding
the tag on every call — the tag bytes are a compile-time constant
baked into the .data section.
-}
precomputeTag :: Int -> WireType -> ByteString
precomputeTag fn wt =
  B.toStrictByteString $ putVarint (fieldTag fn wt)


{- | Emit a pre-computed tag. This is a single memcpy of 1-2 bytes
rather than the varint encoding arithmetic on every encode call.
-}
putPrecomputedTag :: ByteString -> Builder
putPrecomputedTag = B.byteStringCopy
{-# INLINE putPrecomputedTag #-}


-- Size calculation functions: compute the encoded size of values without
-- actually encoding them. Critical for submessage encoding where we need
-- the size prefix before the payload.

{- | Size of a varint encoding in bytes.

Uses CLZ (count leading zeros) for a branchless computation:
each varint byte encodes 7 bits, so size = ceil((64 - clz(n|1)) / 7).
The (n .|. 1) ensures clz is 63 for n=0 (not undefined).
-}
varintSize :: Word64 -> Int
varintSize !n =
  let !bits = 64 - countLeadingZeros (n .|. 1)
      -- ceiling division by 7: (bits + 6) / 7
      !sz = (bits + 6) `quot` 7
  in sz
{-# INLINE varintSize #-}


-- | Size of a 32-bit varint encoding in bytes.  Max 5 bytes.
varintSize32 :: Word32 -> Int
varintSize32 !n
  | n < 0x80 = 1
  | n < 0x4000 = 2
  | n < 0x200000 = 3
  | n < 0x10000000 = 4
  | otherwise = 5
{-# INLINE varintSize32 #-}


-- | Size of a tag encoding.
tagSize :: Int -> Int
tagSize fn = varintSize (fromIntegral fn `shiftL` 3)
{-# INLINE tagSize #-}


-- | Size of a varint field (tag + value).
fieldVarintSize :: Int -> Word64 -> Int
fieldVarintSize fn val = tagSize fn + varintSize val
{-# INLINE fieldVarintSize #-}


-- | Size of a sint32 field.
fieldSVarint32Size :: Int -> Int32 -> Int
fieldSVarint32Size fn val = tagSize fn + varintSize (fromIntegral (zigZag32 val))
{-# INLINE fieldSVarint32Size #-}


-- | Size of a sint64 field.
fieldSVarint64Size :: Int -> Int64 -> Int
fieldSVarint64Size fn val = tagSize fn + varintSize (zigZag64 val)
{-# INLINE fieldSVarint64Size #-}


-- | Size of a fixed32 field (tag + 4 bytes).
fieldFixed32Size :: Int -> Int
fieldFixed32Size fn = tagSize fn + 4
{-# INLINE fieldFixed32Size #-}


-- | Size of a fixed64 field (tag + 8 bytes).
fieldFixed64Size :: Int -> Int
fieldFixed64Size fn = tagSize fn + 8
{-# INLINE fieldFixed64Size #-}


-- | Size of a float field.
fieldFloatSize :: Int -> Int
fieldFloatSize = fieldFixed32Size
{-# INLINE fieldFloatSize #-}


-- | Size of a double field.
fieldDoubleSize :: Int -> Int
fieldDoubleSize = fieldFixed64Size
{-# INLINE fieldDoubleSize #-}


-- | Size of a bool field.
fieldBoolSize :: Int -> Int
fieldBoolSize fn = tagSize fn + 1
{-# INLINE fieldBoolSize #-}


-- | Size of a bytes field (tag + varint length + payload).
fieldBytesSize :: Int -> ByteString -> Int
fieldBytesSize fn bs =
  let len = BS.length bs
  in tagSize fn + varintSize (fromIntegral len) + len
{-# INLINE fieldBytesSize #-}


{- | Size of a text field (tag + varint length + UTF-8 payload).
Uses textUtf8Length to get the byte count.
-}
fieldTextSize :: Int -> Text -> Int
fieldTextSize fn t =
  let !len = textUtf8Length t
  in tagSize fn + varintSize (fromIntegral len) + len
{-# INLINE fieldTextSize #-}


{- | Size of a submessage field given its pre-computed payload size.
This is the key optimization: compute the submessage size first,
then encode tag + length + payload in one pass.
-}
fieldMessageSize :: Int -> Int -> Int
fieldMessageSize fn payloadSize =
  tagSize fn + varintSize (fromIntegral payloadSize) + payloadSize
{-# INLINE fieldMessageSize #-}
