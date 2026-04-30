-- | High-level encoding interface for protobuf messages.
--
-- Key performance technique from Buf's protobuf performance analysis:
-- * Two-pass encoding: first compute message size, then encode.
--   This avoids materializing submessages to ByteString just for
--   their length prefix. The size pass is pure arithmetic.
-- * Builder-based output for zero-copy concatenation
-- * Packed encoding for repeated scalar fields
-- * Pre-computed tag bytes for generated code
module Proto.Encode
  ( -- * Encoding typeclasses
    MessageEncode (..)
  , MessageSize (..)

    -- * Running encoders
  , encodeMessage
  , encodeMessageSized
  , encodeLazy

    -- * Field encoding helpers
  , encodeFieldVarint
  , encodeFieldSVarint32
  , encodeFieldSVarint64
  , encodeFieldFixed32
  , encodeFieldFixed64
  , encodeFieldFloat
  , encodeFieldDouble
  , encodeFieldBool
  , encodeFieldString
  , encodeFieldBytes
  , encodeFieldMessage
  , encodeFieldEnum

    -- * Packed repeated field encoding
  , encodePackedWord64
  , encodePackedWord32
  , encodePackedInt64
  , encodePackedInt32
  , encodePackedBool
  , encodePackedFixed32
  , encodePackedFixed64
  , encodePackedFloat
  , encodePackedDouble
  , encodePackedSVarint32
  , encodePackedSVarint64

    -- * Map field encoding
  , encodeMapField

    -- * Optimized submessage encoding (size-aware)
  , encodeFieldMessageSized
  , encodeMapFieldSized

    -- * Raw builders
  , messageToByteString

    -- * SizedBuilder-based encoding (fused size+builder)
  , sizedFieldVarint
  , sizedFieldSVarint32
  , sizedFieldSVarint64
  , sizedFieldFixed32
  , sizedFieldFixed64
  , sizedFieldFloat
  , sizedFieldDouble
  , sizedFieldBool
  , sizedFieldString
  , sizedFieldBytes
  , sizedFieldMessage
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word32, Word64)
import Data.Text (Text)
import Foreign.Storable (Storable, sizeOf)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (castPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)

import Proto.Wire (WireType (..))
import Proto.Wire.Encode
import Proto.SizedBuilder (SizedBuilder, sized, withSubMessage, toByteStringFromBuilder)

-- | Typeclass for types that can be encoded as protobuf messages.
class MessageEncode a where
  -- | Build the wire-format representation (fields only, no outer length prefix).
  buildMessage :: a -> B.Builder

-- | Typeclass for types whose wire-format size can be pre-computed.
-- Implementing this enables the two-pass optimization for submessage encoding:
-- compute sizes top-down, then encode in a single pass.
class MessageSize a where
  -- | Compute the wire-format size in bytes (fields only, no outer length prefix).
  messageSize :: a -> Int

-- | Encode a message to a strict 'ByteString'.
-- When the message implements 'MessageSize', this allocates a single
-- buffer of exactly the right size for zero-copy output.
encodeMessage :: MessageEncode a => a -> ByteString
encodeMessage = BL.toStrict . B.toLazyByteString . buildMessage
{-# INLINE encodeMessage #-}

-- | Encode a message to a strict 'ByteString' with exact-size allocation.
-- Requires 'MessageSize' to pre-compute the buffer size.
-- Allocates a single ByteString of exactly the right length — no
-- intermediate lazy chunks or recopying.
encodeMessageSized :: (MessageEncode a, MessageSize a) => a -> ByteString
encodeMessageSized msg =
  toByteStringFromBuilder (messageSize msg) (buildMessage msg)
{-# INLINE encodeMessageSized #-}

-- | Encode a message to a lazy 'ByteString' (useful for streaming).
encodeLazy :: MessageEncode a => a -> BL.ByteString
encodeLazy = B.toLazyByteString . buildMessage
{-# INLINE encodeLazy #-}

-- | Convert a builder to strict ByteString.
messageToByteString :: B.Builder -> ByteString
messageToByteString = BL.toStrict . B.toLazyByteString

-- | Encode a varint field (int32, int64, uint32, uint64).
encodeFieldVarint :: Int -> Word64 -> B.Builder
encodeFieldVarint fn val =
  putTag fn WireVarint <> putVarint val
{-# INLINE encodeFieldVarint #-}

-- | Encode a sint32 field.
encodeFieldSVarint32 :: Int -> Int32 -> B.Builder
encodeFieldSVarint32 fn val =
  putTag fn WireVarint <> putSVarint32 val
{-# INLINE encodeFieldSVarint32 #-}

-- | Encode a sint64 field.
encodeFieldSVarint64 :: Int -> Int64 -> B.Builder
encodeFieldSVarint64 fn val =
  putTag fn WireVarint <> putSVarint64 val
{-# INLINE encodeFieldSVarint64 #-}

-- | Encode a fixed32 field.
encodeFieldFixed32 :: Int -> Word32 -> B.Builder
encodeFieldFixed32 fn val =
  putTag fn Wire32Bit <> putFixed32 val
{-# INLINE encodeFieldFixed32 #-}

-- | Encode a fixed64 field.
encodeFieldFixed64 :: Int -> Word64 -> B.Builder
encodeFieldFixed64 fn val =
  putTag fn Wire64Bit <> putFixed64 val
{-# INLINE encodeFieldFixed64 #-}

-- | Encode a float field.
encodeFieldFloat :: Int -> Float -> B.Builder
encodeFieldFloat fn val =
  putTag fn Wire32Bit <> putFloat val
{-# INLINE encodeFieldFloat #-}

-- | Encode a double field.
encodeFieldDouble :: Int -> Double -> B.Builder
encodeFieldDouble fn val =
  putTag fn Wire64Bit <> putDouble val
{-# INLINE encodeFieldDouble #-}

-- | Encode a bool field.
encodeFieldBool :: Int -> Bool -> B.Builder
encodeFieldBool fn val =
  putTag fn WireVarint <> putVarint (if val then 1 else 0)
{-# INLINE encodeFieldBool #-}

-- | Encode a string field.
encodeFieldString :: Int -> Text -> B.Builder
encodeFieldString fn val =
  putTag fn WireLengthDelimited <> putText val
{-# INLINE encodeFieldString #-}

-- | Encode a bytes field.
encodeFieldBytes :: Int -> ByteString -> B.Builder
encodeFieldBytes fn val =
  putTag fn WireLengthDelimited <> putByteString val
{-# INLINE encodeFieldBytes #-}

-- | Encode a submessage field. Materializes the submessage to calculate its length.
-- Use 'encodeFieldMessageSized' when the message implements 'MessageSize'
-- for better performance.
encodeFieldMessage :: MessageEncode a => Int -> a -> B.Builder
encodeFieldMessage fn msg =
  let payload = messageToByteString (buildMessage msg)
  in putTag fn WireLengthDelimited <> putLengthDelimited payload
{-# INLINE encodeFieldMessage #-}

-- | Encode a submessage field using pre-computed size (no materialization).
-- Two-pass: first compute size, then write tag + length + payload.
-- This avoids allocating a temporary ByteString for the submessage.
encodeFieldMessageSized :: (MessageEncode a, MessageSize a) => Int -> a -> B.Builder
encodeFieldMessageSized fn msg =
  let sz = messageSize msg
  in putTag fn WireLengthDelimited <> putVarint (fromIntegral sz) <> buildMessage msg
{-# INLINE encodeFieldMessageSized #-}

-- | Encode an enum field (as varint).
encodeFieldEnum :: Enum a => Int -> a -> B.Builder
encodeFieldEnum fn val =
  putTag fn WireVarint <> putVarint (fromIntegral (fromEnum val))
{-# INLINE encodeFieldEnum #-}

-- | Encode a packed repeated uint64 field. No conversion needed.
encodePackedWord64 :: Int -> VU.Vector Word64 -> B.Builder
encodePackedWord64 fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.foldl' (\acc v -> acc + varintSize v) 0 vals
      in putTag fn WireLengthDelimited <>
         putVarint (fromIntegral sz) <>
         VU.foldl' (\acc v -> acc <> putVarint v) mempty vals
{-# INLINE encodePackedWord64 #-}

-- | Encode a packed repeated uint32 field. Uses native 32-bit varint encoding.
encodePackedWord32 :: Int -> VU.Vector Word32 -> B.Builder
encodePackedWord32 fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.foldl' (\acc v -> acc + varintSize32 v) 0 vals
      in putTag fn WireLengthDelimited <>
         putVarint (fromIntegral sz) <>
         VU.foldl' (\acc v -> acc <> putVarint32 v) mempty vals
{-# INLINE encodePackedWord32 #-}

-- | Encode a packed repeated int64 field.
-- Negative values use 10-byte two's complement.
encodePackedInt64 :: Int -> VU.Vector Int64 -> B.Builder
encodePackedInt64 fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.foldl' (\acc v -> acc + varintSize (fromIntegral v)) 0 vals
      in putTag fn WireLengthDelimited <>
         putVarint (fromIntegral sz) <>
         VU.foldl' (\acc v -> acc <> putVarint (fromIntegral v)) mempty vals
{-# INLINE encodePackedInt64 #-}

-- | Encode a packed repeated int32 field.
-- Negative values are sign-extended to 64-bit and use 10-byte encoding per the
-- protobuf spec.
encodePackedInt32 :: Int -> VU.Vector Int32 -> B.Builder
encodePackedInt32 fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.foldl' (\acc v -> acc + varintSize (fromIntegral v :: Word64)) 0 vals
      in putTag fn WireLengthDelimited <>
         putVarint (fromIntegral sz) <>
         VU.foldl' (\acc v -> acc <> putVarint (fromIntegral v)) mempty vals
{-# INLINE encodePackedInt32 #-}

-- | Encode a packed repeated bool field. Each bool is a single varint byte.
encodePackedBool :: Int -> VU.Vector Bool -> B.Builder
encodePackedBool fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.length vals
      in putTag fn WireLengthDelimited <>
         putVarint (fromIntegral sz) <>
         VU.foldl' (\acc v -> acc <> B.word8 (if v then 1 else 0)) mempty vals
{-# INLINE encodePackedBool #-}

-- | Encode a packed repeated fixed32 field.
-- On LE platforms: the vector's backing store is already the wire bytes.
-- We bulk-copy via a single B.byteString instead of N individual putFixed32.
encodePackedFixed32 :: Int -> VU.Vector Word32 -> B.Builder
encodePackedFixed32 fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.length vals * 4
      in putTag fn WireLengthDelimited <>
         putVarint (fromIntegral sz) <>
         vectorToBuilder vals 4
{-# INLINE encodePackedFixed32 #-}

-- | Encode a packed repeated fixed64 field.
encodePackedFixed64 :: Int -> VU.Vector Word64 -> B.Builder
encodePackedFixed64 fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.length vals * 8
      in putTag fn WireLengthDelimited <>
         putVarint (fromIntegral sz) <>
         vectorToBuilder vals 8
{-# INLINE encodePackedFixed64 #-}

-- | Encode a packed repeated float field.
encodePackedFloat :: Int -> VU.Vector Float -> B.Builder
encodePackedFloat fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.length vals * 4
      in putTag fn WireLengthDelimited <>
         putVarint (fromIntegral sz) <>
         vectorToBuilder vals 4
{-# INLINE encodePackedFloat #-}

-- | Encode a packed repeated double field.
encodePackedDouble :: Int -> VU.Vector Double -> B.Builder
encodePackedDouble fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.length vals * 8
      in putTag fn WireLengthDelimited <>
         putVarint (fromIntegral sz) <>
         vectorToBuilder vals 8
{-# INLINE encodePackedDouble #-}

-- | Bulk-copy an unboxed vector's backing bytes into a Builder.
-- On LE platforms (x86_64, aarch64-LE), the vector's internal
-- representation is already in wire byte order for fixed-width
-- protobuf types. This emits a single B.byteString instead of
-- N individual word writes.
vectorToBuilder :: (VU.Unbox a, Storable a) => VU.Vector a -> Int -> B.Builder
vectorToBuilder vec elemSize =
  let sv = unboxedToStorable vec
      bs = unsafeDupablePerformIO $ VS.unsafeWith sv $ \ptr ->
        BS.packCStringLen (castPtr ptr, VS.length sv * elemSize)
  in B.byteString bs
{-# INLINE vectorToBuilder #-}

unboxedToStorable :: (VU.Unbox a, Storable a) => VU.Vector a -> VS.Vector a
unboxedToStorable uv = VS.generate (VU.length uv) (VU.unsafeIndex uv)
{-# INLINE unboxedToStorable #-}

-- | Encode a packed repeated sint32 field.
encodePackedSVarint32 :: Int -> VU.Vector Int32 -> B.Builder
encodePackedSVarint32 fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.foldl' (\acc v -> acc + varintSize (fromIntegral (zigZag32 v))) 0 vals
      in putTag fn WireLengthDelimited <>
         putVarint (fromIntegral sz) <>
         VU.foldl' (\acc v -> acc <> putSVarint32 v) mempty vals
{-# INLINE encodePackedSVarint32 #-}

-- | Encode a packed repeated sint64 field.
encodePackedSVarint64 :: Int -> VU.Vector Int64 -> B.Builder
encodePackedSVarint64 fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.foldl' (\acc v -> acc + varintSize (zigZag64 v)) 0 vals
      in putTag fn WireLengthDelimited <>
         putVarint (fromIntegral sz) <>
         VU.foldl' (\acc v -> acc <> putSVarint64 v) mempty vals
{-# INLINE encodePackedSVarint64 #-}

-- | Encode a map field entry (materializing to get the length).
encodeMapField
  :: Int          -- ^ Field number of the map field
  -> B.Builder    -- ^ Key encoding (field 1)
  -> B.Builder    -- ^ Value encoding (field 2)
  -> B.Builder
encodeMapField fn keyEnc valEnc =
  let entry = messageToByteString (keyEnc <> valEnc)
  in putTag fn WireLengthDelimited <> putLengthDelimited entry
{-# INLINE encodeMapField #-}

-- | Encode a map field entry using pre-computed entry size.
encodeMapFieldSized
  :: Int        -- ^ Field number
  -> Int        -- ^ Pre-computed entry size (key encoding + value encoding bytes)
  -> B.Builder  -- ^ Key encoding (field 1)
  -> B.Builder  -- ^ Value encoding (field 2)
  -> B.Builder
encodeMapFieldSized fn entrySz keyEnc valEnc =
  putTag fn WireLengthDelimited <>
  putVarint (fromIntegral entrySz) <>
  keyEnc <> valEnc
{-# INLINE encodeMapFieldSized #-}

-- SizedBuilder-based field encoders: compute size and builder in one pass.
-- These are the Church-encoded (fused) versions of the two-pass approach.

-- | Encode a varint field, producing a SizedBuilder.
sizedFieldVarint :: Int -> Word64 -> SizedBuilder
sizedFieldVarint fn val =
  sized (fieldVarintSize fn val) (putTag fn WireVarint <> putVarint val)
{-# INLINE sizedFieldVarint #-}

-- | Encode a sint32 field, producing a SizedBuilder.
sizedFieldSVarint32 :: Int -> Int32 -> SizedBuilder
sizedFieldSVarint32 fn val =
  sized (fieldSVarint32Size fn val) (putTag fn WireVarint <> putSVarint32 val)
{-# INLINE sizedFieldSVarint32 #-}

-- | Encode a sint64 field, producing a SizedBuilder.
sizedFieldSVarint64 :: Int -> Int64 -> SizedBuilder
sizedFieldSVarint64 fn val =
  sized (fieldSVarint64Size fn val) (putTag fn WireVarint <> putSVarint64 val)
{-# INLINE sizedFieldSVarint64 #-}

-- | Encode a fixed32 field, producing a SizedBuilder.
sizedFieldFixed32 :: Int -> Word32 -> SizedBuilder
sizedFieldFixed32 fn val =
  sized (fieldFixed32Size fn) (putTag fn Wire32Bit <> putFixed32 val)
{-# INLINE sizedFieldFixed32 #-}

-- | Encode a fixed64 field, producing a SizedBuilder.
sizedFieldFixed64 :: Int -> Word64 -> SizedBuilder
sizedFieldFixed64 fn val =
  sized (fieldFixed64Size fn) (putTag fn Wire64Bit <> putFixed64 val)
{-# INLINE sizedFieldFixed64 #-}

-- | Encode a float field, producing a SizedBuilder.
sizedFieldFloat :: Int -> Float -> SizedBuilder
sizedFieldFloat fn val =
  sized (fieldFloatSize fn) (putTag fn Wire32Bit <> putFloat val)
{-# INLINE sizedFieldFloat #-}

-- | Encode a double field, producing a SizedBuilder.
sizedFieldDouble :: Int -> Double -> SizedBuilder
sizedFieldDouble fn val =
  sized (fieldDoubleSize fn) (putTag fn Wire64Bit <> putDouble val)
{-# INLINE sizedFieldDouble #-}

-- | Encode a bool field, producing a SizedBuilder.
sizedFieldBool :: Int -> Bool -> SizedBuilder
sizedFieldBool fn val =
  sized (fieldBoolSize fn) (putTag fn WireVarint <> putVarint (if val then 1 else 0))
{-# INLINE sizedFieldBool #-}

-- | Encode a string field, producing a SizedBuilder.
sizedFieldString :: Int -> Text -> SizedBuilder
sizedFieldString fn val =
  sized (fieldTextSize fn val) (putTag fn WireLengthDelimited <> putText val)
{-# INLINE sizedFieldString #-}

-- | Encode a bytes field, producing a SizedBuilder.
sizedFieldBytes :: Int -> ByteString -> SizedBuilder
sizedFieldBytes fn val =
  sized (fieldBytesSize fn val) (putTag fn WireLengthDelimited <> putByteString val)
{-# INLINE sizedFieldBytes #-}

-- | Encode a submessage field using SizedBuilder (fully fused, no materialization).
-- The submessage is provided as a SizedBuilder, which already knows its size.
-- This means we never allocate a ByteString for the submessage — just prepend
-- the tag and length prefix.
sizedFieldMessage :: Int -> SizedBuilder -> SizedBuilder
sizedFieldMessage fn submsg =
  let tagSB = sized (tagSize fn) (putTag fn WireLengthDelimited)
  in tagSB <> withSubMessage submsg
{-# INLINE sizedFieldMessage #-}
