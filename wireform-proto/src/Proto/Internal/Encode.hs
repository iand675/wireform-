{- | High-level encoding interface for protobuf messages.

This is the primary module for serialising protobuf messages to bytes.
Most users only need 'encodeMessage' for a strict 'ByteString',
'encodeLazy' for a lazy one, or 'hPutMessage' to stream straight to a
file handle.

== Quick start

@
import Proto

-- Encode to a strict ByteString (exact-size, single allocation).
let bs = 'encodeMessage' myMsg

-- Encode to a lazy ByteString (chunks).
let bsL = 'encodeLazy' myMsg

-- Stream directly to a Handle, no intermediate ByteString.
'hPutMessage' handle myMsg
@

== One-pass size+build

Generated message types implement 'MessageEncode', whose
'buildMessage' method returns a
'Proto.Internal.SizedBuilder.SizedBuilder' — a pair of @(size,
Builder)@ that propagates the wire byte count up the message tree
alongside the byte fragments. The exact size is known at every
submessage boundary so the varint length prefix can be written
inline without a separate @messageSize@ traversal.

The old two-pass shape ("compute @messageSize@ first, then
@buildMessage@ a second time") is gone; it was 'O(N²)' on deeply
nested messages because each level re-traversed its subtree to
compute the inner size. The fused 'SizedBuilder' shape is 'O(N)'
in the total number of fields.

'messageSize' is still exported as a convenience function for
external callers — it's @'Proto.Internal.SizedBuilder.size' .
'buildMessage'@.

== Field-level helpers

The @encodeField*@ family in this module is used by generated code
and returns 'Builder' fragments. Generated message encoders go
through the archetypes in "Proto.Internal.Encode.Archetype"
instead, which return 'SizedBuilder' fragments (so each call site
carries its own byte count up the tree).
-}
module Proto.Internal.Encode (
  -- * Builder type (re-exported from wireform-core)
  WB.Builder,

  -- * Encoding typeclass
  MessageEncode (..),
  buildMessage,
  messageSize,

  -- * Running encoders (strict ByteString output)
  encodeMessage,
  encodeLazy,

  -- * Stream encoding (length-delimited framing)
  encodeMessageLazy,
  encodeMessageStream,
  encodeMessageStreamSized,
  hPutMessageStream,
  buildMessageFramed,

  -- * Running encoders (Builder output)
  hPutMessage,
  hPutMessageLen,

  -- * Field encoding helpers
  encodeFieldVarint,
  encodeFieldSVarint32,
  encodeFieldSVarint64,
  encodeFieldFixed32,
  encodeFieldFixed64,
  encodeFieldFloat,
  encodeFieldDouble,
  encodeFieldBool,
  encodeFieldString,
  encodeFieldBytes,
  encodeFieldMessage,
  encodeFieldEnum,

  -- * Packed repeated field encoding
  encodePackedWord64,
  encodePackedWord32,
  encodePackedInt64,
  encodePackedInt32,
  encodePackedBool,
  encodePackedFixed32,
  encodePackedFixed64,
  encodePackedFloat,
  encodePackedDouble,
  encodePackedSVarint32,
  encodePackedSVarint64,

  -- * Legacy alias
  encodePackedVarint,

  -- * Map field encoding
  encodeMapField,

  -- * Submessage encoding helpers (used by generated code)
  encodeFieldMessageSized,
  encodeMapFieldSized,
  mapField,

  -- * Raw builders
  messageToByteString,

  -- * SizedBuilder-based field helpers (used by generated code)
  fieldVarint,
  fieldSVarint32,
  fieldSVarint64,
  fieldFixed32,
  fieldFixed64,
  fieldFloat,
  fieldDouble,
  fieldBool,
  fieldString,
  fieldBytes,
  fieldMessage,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32, Word64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable, sizeOf)
import Proto.Internal.SizedBuilder (SizedBuilder, sized, toByteStringFromBuilder, withSubMessage)
import Proto.Internal.SizedBuilder qualified as SB
import Proto.Internal.Wire (WireType (..))
import Proto.Internal.Wire.Encode
import System.IO (Handle)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Wireform.Builder qualified as B
import Wireform.Builder qualified as WB


{- | Typeclass for types that can be encoded as protobuf messages.

The only method is 'buildSized', which returns a 'SizedBuilder' —
byte fragments and their cumulative wire byte count, fused in a
single pass. 'buildMessage' (a 'Builder' view) and 'messageSize'
(an 'Int' view) are free standalone projections over the same
fused traversal.

The @MessageSize@ class that used to live next to 'MessageEncode'
is /gone/. The wire byte count is read off 'buildSized' via
'messageSize'. There is no longer a 'Builder'-returning method on
the class: that path always involved materialising a 'Builder' to
a 'ByteString' just to read the length, which lost the @O(N)@
property on nested submessages. New encoders (generated and
hand-written) all produce 'SizedBuilder' fragments; the byte
count flows up the message tree for free alongside the bytes
themselves.
-}
class MessageEncode a where
  -- | Fused encoding: byte fragments + cumulative size in a single
  -- pass.
  buildSized :: a -> SizedBuilder


-- | Plain 'Builder' view of an encoded message. Free projection
-- of 'buildSized'.
buildMessage :: MessageEncode a => a -> B.Builder
buildMessage = SB.toBuilder . buildSized
{-# INLINE buildMessage #-}


-- | Wire-format size in bytes (fields only, no outer length prefix).
-- Reads the size off the message's 'SizedBuilder' without a
-- separate traversal.
messageSize :: MessageEncode a => a -> Int
messageSize = SB.size . buildSized
{-# INLINE messageSize #-}




{- | Encode a message to a strict 'ByteString'.

Uses 'buildSized' so the output buffer is allocated at exactly the
right size in one shot — no growing-buffer reallocations.
-}
encodeMessage :: MessageEncode a => a -> ByteString
encodeMessage = SB.toByteString . buildSized
{-# INLINE encodeMessage #-}


-- | Encode a message to a lazy 'ByteString' (useful for streaming).
encodeLazy :: MessageEncode a => a -> BL.ByteString
encodeLazy = SB.toLazyByteString . buildSized
{-# INLINE encodeLazy #-}


{- | Write a message directly to a 'Handle' without materialising an
intermediate 'ByteString'. Uses fast-builder's streaming output.
-}
hPutMessage :: MessageEncode a => Handle -> a -> IO ()
hPutMessage h = WB.hPutBuilder h . buildMessage
{-# INLINE hPutMessage #-}


-- | Like 'hPutMessage' but also returns the number of bytes written.
hPutMessageLen :: MessageEncode a => Handle -> a -> IO Int
hPutMessageLen h = WB.hPutBuilderLen h . buildMessage
{-# INLINE hPutMessageLen #-}


-- | Convert a builder to strict ByteString.
messageToByteString :: B.Builder -> ByteString
messageToByteString = B.toStrictByteString


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


{- | Encode a submessage field. Materializes the submessage to calculate its length.
-}
encodeFieldMessage :: MessageEncode a => Int -> a -> B.Builder
encodeFieldMessage fn msg =
  let !sb = buildSized msg
      !sz = SB.size sb
  in putTag fn WireLengthDelimited
      <> putVarint (fromIntegral sz)
      <> SB.toBuilder sb
{-# INLINE encodeFieldMessage #-}


-- | Pre-fused-builder synonym for 'encodeFieldMessage'. Kept under
-- the old name because some downstream callers still reach for it.
-- Both paths are now O(1) extra over @'buildMessage' msg@ (the
-- size is just read from the 'SizedBuilder' rather than recomputed).
encodeFieldMessageSized :: MessageEncode a => Int -> a -> B.Builder
encodeFieldMessageSized = encodeFieldMessage
{-# INLINE encodeFieldMessageSized #-}


-- | Encode an enum field (as varint).
encodeFieldEnum :: Enum a => Int -> a -> B.Builder
encodeFieldEnum fn val =
  putTag fn WireVarint <> putVarint (fromIntegral (fromEnum val))
{-# INLINE encodeFieldEnum #-}


{- | Shared @tag(LengthDelimited) || varint(payloadLen) || payload@
framing for packed repeated scalars. Threads the precomputed payload
byte count into the size component of the resulting 'SizedBuilder'
so the caller doesn't traverse the payload twice.
-}
packedFrame :: Int -> Int -> B.Builder -> SizedBuilder
packedFrame fn payloadSz inner =
  SB.sized
    (tagSize fn + varintSize (fromIntegral payloadSz) + payloadSz)
    ( putTag fn WireLengthDelimited
        <> putVarint (fromIntegral payloadSz)
        <> inner
    )
{-# INLINE packedFrame #-}


-- | Encode a packed repeated uint64 field. No conversion needed.
encodePackedWord64 :: Int -> VU.Vector Word64 -> SizedBuilder
encodePackedWord64 fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.foldl' (\acc v -> acc + varintSize v) 0 vals
      in packedFrame fn sz
          (VU.foldl' (\acc v -> acc <> putVarint v) mempty vals)
{-# INLINE encodePackedWord64 #-}


-- | Encode a packed repeated uint32 field. Uses native 32-bit varint encoding.
encodePackedWord32 :: Int -> VU.Vector Word32 -> SizedBuilder
encodePackedWord32 fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.foldl' (\acc v -> acc + varintSize32 v) 0 vals
      in packedFrame fn sz
          (VU.foldl' (\acc v -> acc <> putVarint32 v) mempty vals)
{-# INLINE encodePackedWord32 #-}


{- | Encode a packed repeated int64 field.
Negative values use 10-byte two's complement.
-}
encodePackedInt64 :: Int -> VU.Vector Int64 -> SizedBuilder
encodePackedInt64 fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.foldl' (\acc v -> acc + varintSize (fromIntegral v)) 0 vals
      in packedFrame fn sz
          (VU.foldl' (\acc v -> acc <> putVarint (fromIntegral v)) mempty vals)
{-# INLINE encodePackedInt64 #-}


{- | Encode a packed repeated int32 field.
Negative values are sign-extended to 64-bit and use 10-byte encoding per the
protobuf spec.
-}
encodePackedInt32 :: Int -> VU.Vector Int32 -> SizedBuilder
encodePackedInt32 fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.foldl' (\acc v -> acc + varintSize (fromIntegral v :: Word64)) 0 vals
      in packedFrame fn sz
          (VU.foldl' (\acc v -> acc <> putVarint (fromIntegral v)) mempty vals)
{-# INLINE encodePackedInt32 #-}


-- | Encode a packed repeated bool field. Each bool is a single varint byte.
encodePackedBool :: Int -> VU.Vector Bool -> SizedBuilder
encodePackedBool fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.length vals
      in packedFrame fn sz
          (VU.foldl' (\acc v -> acc <> B.word8 (if v then 1 else 0)) mempty vals)
{-# INLINE encodePackedBool #-}


-- | Legacy alias. Prefer the type-specific variants.
encodePackedVarint :: Int -> VU.Vector Word64 -> SizedBuilder
encodePackedVarint = encodePackedWord64
{-# INLINE encodePackedVarint #-}


{- | Encode a packed repeated fixed32 field.
On LE platforms: the vector's backing store is already the wire bytes.
We bulk-copy via a single B.byteString instead of N individual putFixed32.
-}
encodePackedFixed32 :: Int -> VU.Vector Word32 -> SizedBuilder
encodePackedFixed32 fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.length vals * 4
      in packedFrame fn sz (vectorToBuilder vals 4)
{-# INLINE encodePackedFixed32 #-}


-- | Encode a packed repeated fixed64 field.
encodePackedFixed64 :: Int -> VU.Vector Word64 -> SizedBuilder
encodePackedFixed64 fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.length vals * 8
      in packedFrame fn sz (vectorToBuilder vals 8)
{-# INLINE encodePackedFixed64 #-}


-- | Encode a packed repeated float field.
encodePackedFloat :: Int -> VU.Vector Float -> SizedBuilder
encodePackedFloat fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.length vals * 4
      in packedFrame fn sz (vectorToBuilder vals 4)
{-# INLINE encodePackedFloat #-}


-- | Encode a packed repeated double field.
encodePackedDouble :: Int -> VU.Vector Double -> SizedBuilder
encodePackedDouble fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.length vals * 8
      in packedFrame fn sz (vectorToBuilder vals 8)
{-# INLINE encodePackedDouble #-}


{- | Bulk-copy an unboxed vector's backing bytes into a Builder.
On LE platforms (x86_64, aarch64-LE), the vector's internal
representation is already in wire byte order for fixed-width
protobuf types. This emits a single B.byteString instead of
N individual word writes.
-}
vectorToBuilder :: (VU.Unbox a, Storable a) => VU.Vector a -> Int -> B.Builder
vectorToBuilder vec elemSize =
  let sv = unboxedToStorable vec
      bs = unsafeDupablePerformIO $ VS.unsafeWith sv $ \ptr ->
        BS.packCStringLen (castPtr ptr, VS.length sv * elemSize)
  in B.byteStringCopy bs
{-# INLINE vectorToBuilder #-}


unboxedToStorable :: (VU.Unbox a, Storable a) => VU.Vector a -> VS.Vector a
unboxedToStorable uv = VS.generate (VU.length uv) (VU.unsafeIndex uv)
{-# INLINE unboxedToStorable #-}


-- | Encode a packed repeated sint32 field.
encodePackedSVarint32 :: Int -> VU.Vector Int32 -> SizedBuilder
encodePackedSVarint32 fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.foldl' (\acc v -> acc + varintSize (fromIntegral (zigZag32 v))) 0 vals
      in packedFrame fn sz
          (VU.foldl' (\acc v -> acc <> putSVarint32 v) mempty vals)
{-# INLINE encodePackedSVarint32 #-}


-- | Encode a packed repeated sint64 field.
encodePackedSVarint64 :: Int -> VU.Vector Int64 -> SizedBuilder
encodePackedSVarint64 fn vals
  | VU.null vals = mempty
  | otherwise =
      let sz = VU.foldl' (\acc v -> acc + varintSize (zigZag64 v)) 0 vals
      in packedFrame fn sz
          (VU.foldl' (\acc v -> acc <> putSVarint64 v) mempty vals)
{-# INLINE encodePackedSVarint64 #-}


-- | Encode a map field entry (materializing to get the length).
encodeMapField
  :: Int
  -- ^ Field number of the map field
  -> B.Builder
  -- ^ Key encoding (field 1)
  -> B.Builder
  -- ^ Value encoding (field 2)
  -> B.Builder
encodeMapField fn keyEnc valEnc =
  let entry = messageToByteString (keyEnc <> valEnc)
  in putTag fn WireLengthDelimited <> putLengthDelimited entry
{-# INLINE encodeMapField #-}


-- | Encode a map field entry using pre-computed entry size.
encodeMapFieldSized
  :: Int
  -- ^ Field number
  -> Int
  -- ^ Pre-computed entry size (key encoding + value encoding bytes)
  -> B.Builder
  -- ^ Key encoding (field 1)
  -> B.Builder
  -- ^ Value encoding (field 2)
  -> B.Builder
encodeMapFieldSized fn entrySz keyEnc valEnc =
  putTag fn WireLengthDelimited
    <> putVarint (fromIntegral entrySz)
    <> keyEnc
    <> valEnc
{-# INLINE encodeMapFieldSized #-}


{- | 'encodeMapField' fused with size tracking: combine the key
'SizedBuilder' and value 'SizedBuilder' into one length-prefixed
entry. The entry's wire size comes directly from the
'SizedBuilder' inputs — no @messageToByteString@ materialisation.
Used by the generated 'buildSized' for @map\<K, V\>@ fields.
-}
mapField :: Int -> SizedBuilder -> SizedBuilder -> SizedBuilder
mapField fn keyEnc valEnc =
  let !innerSz = SB.size keyEnc + SB.size valEnc
      !sz = tagSize fn + varintSize (fromIntegral innerSz) + innerSz
      !bld =
        putTag fn WireLengthDelimited
          <> putVarint (fromIntegral innerSz)
          <> SB.toBuilder keyEnc
          <> SB.toBuilder valEnc
  in sized sz bld
{-# INLINE mapField #-}


-- SizedBuilder-based field encoders: compute size and builder in one pass.
-- These are the Church-encoded (fused) versions of the two-pass approach.

-- | Encode a varint field, producing a SizedBuilder.
fieldVarint :: Int -> Word64 -> SizedBuilder
fieldVarint fn val =
  sized (fieldVarintSize fn val) (putTag fn WireVarint <> putVarint val)
{-# INLINE fieldVarint #-}


-- | Encode a sint32 field, producing a SizedBuilder.
fieldSVarint32 :: Int -> Int32 -> SizedBuilder
fieldSVarint32 fn val =
  sized (fieldSVarint32Size fn val) (putTag fn WireVarint <> putSVarint32 val)
{-# INLINE fieldSVarint32 #-}


-- | Encode a sint64 field, producing a SizedBuilder.
fieldSVarint64 :: Int -> Int64 -> SizedBuilder
fieldSVarint64 fn val =
  sized (fieldSVarint64Size fn val) (putTag fn WireVarint <> putSVarint64 val)
{-# INLINE fieldSVarint64 #-}


-- | Encode a fixed32 field, producing a SizedBuilder.
fieldFixed32 :: Int -> Word32 -> SizedBuilder
fieldFixed32 fn val =
  sized (fieldFixed32Size fn) (putTag fn Wire32Bit <> putFixed32 val)
{-# INLINE fieldFixed32 #-}


-- | Encode a fixed64 field, producing a SizedBuilder.
fieldFixed64 :: Int -> Word64 -> SizedBuilder
fieldFixed64 fn val =
  sized (fieldFixed64Size fn) (putTag fn Wire64Bit <> putFixed64 val)
{-# INLINE fieldFixed64 #-}


-- | Encode a float field, producing a SizedBuilder.
fieldFloat :: Int -> Float -> SizedBuilder
fieldFloat fn val =
  sized (fieldFloatSize fn) (putTag fn Wire32Bit <> putFloat val)
{-# INLINE fieldFloat #-}


-- | Encode a double field, producing a SizedBuilder.
fieldDouble :: Int -> Double -> SizedBuilder
fieldDouble fn val =
  sized (fieldDoubleSize fn) (putTag fn Wire64Bit <> putDouble val)
{-# INLINE fieldDouble #-}


-- | Encode a bool field, producing a SizedBuilder.
fieldBool :: Int -> Bool -> SizedBuilder
fieldBool fn val =
  sized (fieldBoolSize fn) (putTag fn WireVarint <> putVarint (if val then 1 else 0))
{-# INLINE fieldBool #-}


-- | Encode a string field, producing a SizedBuilder.
fieldString :: Int -> Text -> SizedBuilder
fieldString fn val =
  sized (fieldTextSize fn val) (putTag fn WireLengthDelimited <> putText val)
{-# INLINE fieldString #-}


-- | Encode a bytes field, producing a SizedBuilder.
fieldBytes :: Int -> ByteString -> SizedBuilder
fieldBytes fn val =
  sized (fieldBytesSize fn val) (putTag fn WireLengthDelimited <> putByteString val)
{-# INLINE fieldBytes #-}


{- | Encode a submessage field using SizedBuilder (fully fused, no materialization).
The submessage is provided as a SizedBuilder, which already knows its size.
This means we never allocate a ByteString for the submessage — just prepend
the tag and length prefix.
-}
fieldMessage :: Int -> SizedBuilder -> SizedBuilder
fieldMessage fn submsg =
  let tagSB = sized (tagSize fn) (putTag fn WireLengthDelimited)
  in tagSB <> withSubMessage submsg
{-# INLINE fieldMessage #-}


-- | Encode a message to a lazy 'ByteString'.
encodeMessageLazy :: MessageEncode a => a -> BL.ByteString
encodeMessageLazy = SB.toLazyByteString . buildSized
{-# INLINE encodeMessageLazy #-}


{- | Encode a list of messages with length-delimited framing.
Each message is preceded by a varint length prefix. The size for
each frame comes from the message's 'SizedBuilder' directly, no
intermediate materialisation.
-}
encodeMessageStream :: MessageEncode a => [a] -> BL.ByteString
encodeMessageStream = B.toLazyByteString . foldMap buildMessageFramed


-- | Synonym for 'encodeMessageStream' — both paths use the fused
-- 'SizedBuilder' size now, so the old "Sized" variant is redundant.
encodeMessageStreamSized :: MessageEncode a => [a] -> BL.ByteString
encodeMessageStreamSized = encodeMessageStream
{-# INLINE encodeMessageStreamSized #-}


{- | Write a stream of messages directly to a 'Handle' with
length-delimited framing.
-}
hPutMessageStream :: MessageEncode a => Handle -> [a] -> IO ()
hPutMessageStream h = WB.hPutBuilder h . foldMap buildMessageFramed


{- | Build a single length-delimited frame: varint size prefix
followed by the message payload. Reads the prefix size from the
fused 'SizedBuilder' so there's no separate size traversal.
-}
buildMessageFramed :: MessageEncode a => a -> B.Builder
buildMessageFramed msg =
  let !sb = buildSized msg
      !sz = SB.size sb
  in putVarint (fromIntegral sz) <> SB.toBuilder sb
{-# INLINE buildMessageFramed #-}
