{-# LANGUAGE BangPatterns #-}

{- | Archetype-specialised encode functions for maximum performance.

__Stability:__ exposed for use by wireform-proto-generated code; not
part of the stable public API.

Each function is specialised for a specific
@(field_number, wire_type, field_type)@ combination, with the wire
tag byte baked in as a compile-time constant. Generated code calls
into these directly rather than going through the
@encodeField*@ / @putTag@ wrapper chain.

== Sized builder return shape

Every archetype here returns a 'SizedBuilder', not a raw 'Builder'.
That means each fragment carries the exact wire byte count of what
it will emit, so the parent message can:

1. allocate its output buffer in one shot at exactly the right size,
   and
2. write a submessage's varint length prefix without a /separate/
   @messageSize@ traversal of the inner message.

The old "build a 'Builder', then do a separate size traversal to
prefix the length, then emit" two-pass shape is gone — it was
'O(N²)' on nested-message trees because each level re-traversed
its subtree to compute the inner size, and the cost grew
quadratically with nesting depth. With 'SizedBuilder' the size
flows up the tree alongside the builder fragments in 'O(N)' total.
See "Proto.Internal.SizedBuilder" for the underlying machinery.
-}
module Proto.Internal.Encode.Archetype (
  -- * Singular field archetypes (tag byte baked in)
  archVarint,
  archSVarint32,
  archSVarint64,
  archFixed32,
  archFixed64,
  archFloat,
  archDouble,
  archBool,
  archString,
  archBytes,
  archSubmessage,

  -- * Repeated field archetypes
  archRepeatedString,
  archRepeatedSubmessage,

  -- * Packed repeated archetypes
  archPackedVarints,

  -- * Legacy size-pass helpers (kept so already-generated modules
  -- whose import lists still mention them keep compiling; new
  -- generated code does not use these — the size is carried by
  -- 'SizedBuilder' instead).
  archVarintSize,
  archStringSize,
  archBytesSize,
  archBoolSize,
  archFixed32Size,
  archFixed64Size,
  archSubmessageSize,
) where

import Data.Bits (shiftL, shiftR, xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text.Foreign qualified as TF
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32, Word64, Word8)
import Proto.Internal.SizedBuilder (SizedBuilder, sized, withSubMessage)
import Proto.Internal.Wire.Encode (
  putSVarint32,
  putSVarint64,
  putText,
  putVarint,
  varintSize,
 )
import Wireform.Builder qualified as B


-- | Varint field. Tag byte + varint-encoded value.
archVarint :: Word64 -> Word64 -> SizedBuilder
archVarint !tag !val =
  let !sz = varintSize tag + varintSize val
  in sized sz (putVarint tag <> putVarint val)
{-# INLINE archVarint #-}


-- | ZigZag-encoded sint32 field. Tag byte + zigzag-varint.
archSVarint32 :: Word64 -> Int32 -> SizedBuilder
archSVarint32 !tag !val =
  -- ZigZag preserves varint width for the worst case; @putSVarint32@
  -- writes the same number of bytes that @varintSize@ would
  -- report for the post-zigzag value.
  let !zz = zigZagW32 val
      !sz = varintSize tag + varintSize (fromIntegral zz)
  in sized sz (putVarint tag <> putSVarint32 val)
{-# INLINE archSVarint32 #-}


-- | ZigZag-encoded sint64 field. Tag byte + zigzag-varint.
archSVarint64 :: Word64 -> Int64 -> SizedBuilder
archSVarint64 !tag !val =
  let !zz = zigZagW64 val
      !sz = varintSize tag + varintSize zz
  in sized sz (putVarint tag <> putSVarint64 val)
{-# INLINE archSVarint64 #-}


-- | fixed32 field (little-endian).
archFixed32 :: Word64 -> Word32 -> SizedBuilder
archFixed32 !tag !val =
  sized (varintSize tag + 4) (putVarint tag <> B.word32LE val)
{-# INLINE archFixed32 #-}


-- | fixed64 field (little-endian).
archFixed64 :: Word64 -> Word64 -> SizedBuilder
archFixed64 !tag !val =
  sized (varintSize tag + 8) (putVarint tag <> B.word64LE val)
{-# INLINE archFixed64 #-}


-- | IEEE 754 float field.
archFloat :: Word64 -> Float -> SizedBuilder
archFloat !tag !val =
  sized (varintSize tag + 4) (putVarint tag <> B.floatLE val)
{-# INLINE archFloat #-}


-- | IEEE 754 double field.
archDouble :: Word64 -> Double -> SizedBuilder
archDouble !tag !val =
  sized (varintSize tag + 8) (putVarint tag <> B.doubleLE val)
{-# INLINE archDouble #-}


-- | bool field. Tag + single varint byte (0 or 1).
archBool :: Word64 -> Bool -> SizedBuilder
archBool !tag True = sized (varintSize tag + 1) (putVarint tag <> B.word8 1)
archBool !tag False = sized (varintSize tag + 1) (putVarint tag <> B.word8 0)
{-# INLINE archBool #-}


{- | UTF-8 string field. Tag + length varint + UTF-8 bytes.

The size pass reads the 'Text' record's byte length in O(1) via
'TF.lengthWord8' (no allocation). The build pass streams the
'Text' 's bytes straight from its unpinned 'ByteArray#' into the
builder buffer via 'putText' — no intermediate pinned
'BS.ByteString' copy.
-}
archString :: Word64 -> Text -> SizedBuilder
archString !tag !val =
  let !len = TF.lengthWord8 val
      !sz = varintSize tag + varintSize (fromIntegral len) + len
  in sized sz (putVarint tag <> putText val)
{-# INLINE archString #-}


-- | bytes field (length-delimited). Tag + length varint + bytes.
archBytes :: Word64 -> ByteString -> SizedBuilder
archBytes !tag !val =
  let !len = BS.length val
      !sz = varintSize tag + varintSize (fromIntegral len) + len
  in sized sz (putVarint tag <> putVarint (fromIntegral len) <> B.byteString val)
{-# INLINE archBytes #-}


{- | Submessage field. Tag + (varint length + payload) wrapped from
the already-sized inner builder.

The inner 'SizedBuilder' has carried its byte count up from each
of its fields' archetype calls, so this just wraps with one tag
byte and lets 'withSubMessage' prepend the canonical length
varint — no separate size traversal of the submessage tree.
-}
archSubmessage :: Word64 -> SizedBuilder -> SizedBuilder
archSubmessage !tag inner =
  sized (varintSize tag) (putVarint tag) <> withSubMessage inner
{-# INLINE archSubmessage #-}


-- | Repeated string archetype — alias for 'archString' (one element).
archRepeatedString :: Word64 -> Text -> SizedBuilder
archRepeatedString = archString
{-# INLINE archRepeatedString #-}


-- | Repeated submessage archetype — alias for 'archSubmessage'.
archRepeatedSubmessage :: Word64 -> SizedBuilder -> SizedBuilder
archRepeatedSubmessage = archSubmessage
{-# INLINE archRepeatedSubmessage #-}


{- | Packed repeated varint field. Tag + total payload length
varint + the concatenated value varints. Empty input emits
nothing (proto3 packed encoding omits empty fields).
-}
archPackedVarints :: Word64 -> VU.Vector Int32 -> SizedBuilder
archPackedVarints !tag vs
  | VU.null vs = mempty
  | otherwise =
      let (!packedSz, !packedBld) =
            VU.foldl'
              ( \(!sz, !bld) v ->
                  let !w = fromIntegral v :: Word64
                  in (sz + varintSize w, bld <> putVarint w)
              )
              (0, mempty)
              vs
          !totalSz = varintSize tag + varintSize (fromIntegral packedSz) + packedSz
      in sized totalSz (putVarint tag <> putVarint (fromIntegral packedSz) <> packedBld)
{-# INLINE archPackedVarints #-}


-- | ZigZag encoding for Int32 → Word32. Used only to compute the
-- post-ZigZag varint byte count for the size pass; 'putSVarint32'
-- emits the same encoding when the bytes go to the buffer.
zigZagW32 :: Int32 -> Word32
zigZagW32 n =
  fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 31))
{-# INLINE zigZagW32 #-}


-- | ZigZag encoding for Int64 → Word64. Used only to compute the
-- post-ZigZag varint byte count for the size pass; 'putSVarint64'
-- emits the same encoding when the bytes go to the buffer.
zigZagW64 :: Int64 -> Word64
zigZagW64 n =
  fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 63))
{-# INLINE zigZagW64 #-}


-- ============================================================
-- Legacy size-pass helpers
-- ============================================================
--
-- These were used by the pre-SizedBuilder codegen output. They're
-- kept here so already-generated modules whose import lists still
-- mention them continue to compile; freshly regenerated code
-- doesn't import them.

archVarintSize :: Word64 -> Int
archVarintSize !val = varintSize val
{-# INLINE archVarintSize #-}

archStringSize :: Text -> Int
archStringSize !val =
  let !len = TF.lengthWord8 val
  in varintSize (fromIntegral len) + len
{-# INLINE archStringSize #-}

archBytesSize :: ByteString -> Int
archBytesSize !val =
  let !len = BS.length val
  in varintSize (fromIntegral len) + len
{-# INLINE archBytesSize #-}

archBoolSize :: Int
archBoolSize = 1
{-# INLINE archBoolSize #-}

archFixed32Size :: Int
archFixed32Size = 4
{-# INLINE archFixed32Size #-}

archFixed64Size :: Int
archFixed64Size = 8
{-# INLINE archFixed64Size #-}

archSubmessageSize :: Int -> Int
archSubmessageSize !payloadSz = varintSize (fromIntegral payloadSz) + payloadSz
{-# INLINE archSubmessageSize #-}
