{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{- | High-level decoding interface for protobuf messages.

This is the primary module for deserialising protobuf messages from
bytes. Most users only need 'decodeMessage'.

== Quick start

@
import Proto.Internal.Decode

case 'decodeMessage' rawBytes of
  Left err -> handleError err   -- 'DecodeError' with details
  Right msg -> use msg
@

== Typeclass approach

Generated message types automatically get a 'MessageDecode' instance
via Template Haskell ('Proto.TH.loadProto'). The instance provides
'messageDecoder', a 'Decoder' action that is run by 'decodeMessage'.

== Error handling

'decodeMessage' returns @'Either' 'DecodeError' a@. 'DecodeError'
covers truncated input, invalid varints, UTF-8 validation failures,
negative length prefixes, and custom errors from generated code.

== Performance characteristics

* __Zero-copy__: @bytes@ and @string@ fields reference slices of the
  input 'Data.ByteString.ByteString' (no allocation for those fields).
* __Lazy submessage decoding__: with 'LazyMessage', submessage bytes
  are captured but not parsed until 'forceLazyMessage' is called.
* __Packed repeated fields__: adaptive multi-strategy decode with
  SWAR pre-counting for exact vector pre-allocation.
* __Unknown field preservation__: unrecognised fields are captured as
  'UnknownField' values for round-trip fidelity.
-}
module Proto.Internal.Decode (
  -- * Decoding typeclass
  MessageDecode (..),

  -- * Running decoders
  decodeMessage,

  -- * Field decoding helpers
  decodeFieldVarint,
  decodeFieldSVarint32,
  decodeFieldSVarint64,
  decodeFieldFixed32,
  decodeFieldFixed64,
  decodeFieldFloat,
  decodeFieldDouble,
  decodeFieldBool,
  decodeFieldString,
  decodeFieldBytes,
  decodeFieldMessage,
  decodeFieldEnum,

  -- * Packed repeated field decoding
  decodePackedVarint,
  decodePackedFixed32,
  decodePackedFixed64,
  decodePackedFloat,
  decodePackedDouble,
  decodePackedSVarint32,
  decodePackedSVarint64,

  -- * Submessage decoding
  decodeSubmessage,

  -- * Lazy submessage decoding
  LazyMessage (..),
  forceLazyMessage,
  decodeFieldLazyMessage,

  -- * Unknown field preservation
  UnknownField (..),
  captureUnknownField,
  encodeUnknownFields,
  encodeUnknownFieldsSized,

  -- * Re-exports for generated code
  Decoder,
  DecodeResult (..),
  DecodeError (..),
  runDecoder,
  getVarint,
  getVarintSigned,
  getTagOr,
  getTag,
  skipField,
  getLengthDelimited,
  getFixed32,
  getFixed64,
  getFloat,
  getDouble,
  getText,
  getSVarint32,
  getSVarint64,

  -- * Map entry decoding
  decodeMapEntry,

  -- * CPS failure
  decodeFail,

  -- * Unknown field sizes
  unknownFieldsSize,

  -- * Unboxed internal types (re-exported for generated code)
  UMaybe (UJust, UNothing),
  umaybe,
  getTagOrU,

  -- * Three-way tag CPS (flattened, zero-allocation decode loop)
  TagResult#,
  withTag,

  -- * Monadic CPS tag dispatch (zero Tag allocation, for generated code)
  withTagM,
  skipWireType,
  inOrderStage,
  inOrderStage1,
) where

import Control.DeepSeq (NFData (..))
import Control.Monad qualified
import Control.Monad.ST (runST)
import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int32, Int64)
import Data.List (foldl')
import Data.Text (Text)
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as MVU
import Data.Word (Word32, Word64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable)
import GHC.Exts (Int (I#))
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Proto.Internal.SizedBuilder qualified as SB
import Proto.Internal.Wire (Tag (..), WireType (..))
import Proto.Internal.Wire.Decode
import Proto.Internal.Wire.Encode (putFixed32, putFixed64, putLengthDelimited, putTag, putVarint, tagSize, varintSize)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Wireform.Builder qualified as B
import Wireform.FFI (countPackedVarints)


-- | Typeclass for types that can be decoded from protobuf wire format.
class MessageDecode a where
  -- | Decode a message from a ByteString, starting from default field values.
  -- The decoder should consume all bytes of the submessage.
  messageDecoder :: Decoder a


-- | Decode a message from a strict 'ByteString'.
decodeMessage :: MessageDecode a => ByteString -> Either DecodeError a
decodeMessage = runDecoder messageDecoder
{-# INLINE decodeMessage #-}


-- | Decode a varint value.
decodeFieldVarint :: Decoder Word64
decodeFieldVarint = getVarint
{-# INLINE decodeFieldVarint #-}


-- | Decode a sint32 value.
decodeFieldSVarint32 :: Decoder Int32
decodeFieldSVarint32 = getSVarint32
{-# INLINE decodeFieldSVarint32 #-}


-- | Decode a sint64 value.
decodeFieldSVarint64 :: Decoder Int64
decodeFieldSVarint64 = getSVarint64
{-# INLINE decodeFieldSVarint64 #-}


-- | Decode a fixed32 value.
decodeFieldFixed32 :: Decoder Word32
decodeFieldFixed32 = getFixed32
{-# INLINE decodeFieldFixed32 #-}


-- | Decode a fixed64 value.
decodeFieldFixed64 :: Decoder Word64
decodeFieldFixed64 = getFixed64
{-# INLINE decodeFieldFixed64 #-}


-- | Decode a float value.
decodeFieldFloat :: Decoder Float
decodeFieldFloat = getFloat
{-# INLINE decodeFieldFloat #-}


-- | Decode a double value.
decodeFieldDouble :: Decoder Double
decodeFieldDouble = getDouble
{-# INLINE decodeFieldDouble #-}


-- | Decode a bool value.
decodeFieldBool :: Decoder Bool
decodeFieldBool = do
  v <- getVarint
  pure (v /= 0)
{-# INLINE decodeFieldBool #-}


-- | Decode a string value (validated UTF-8, zero-copy).
decodeFieldString :: Decoder Text
decodeFieldString = getText
{-# INLINE decodeFieldString #-}


-- | Decode a bytes value (zero-copy).
decodeFieldBytes :: Decoder ByteString
decodeFieldBytes = getByteString
{-# INLINE decodeFieldBytes #-}


{- | Decode a submessage field using a bounded sub-buffer.

Reads the length prefix from the parent buffer, slices the exact
submessage bytes (zero-copy — just ForeignPtr offset adjustment),
then runs the sub-decoder on that slice. The sub-decoder's end-of-input
check naturally enforces the submessage boundary.
-}
decodeFieldMessage :: MessageDecode a => Decoder a
decodeFieldMessage = Decoder $ \bs off ->
  case runDecoder# getVarint bs off of
    (# (# lenW, off' #) | #) ->
      let !len = fromIntegral lenW :: Int
      in if len < 0
          then (# | NegativeLength #)
          else
            if I# off' + len > BS.length bs
              then (# | UnexpectedEnd #)
              else
                let !subBs = case bs of
                      BSI.BS fp _ -> BSI.BS (BSI.plusForeignPtr fp (I# off')) len
                in case runDecoder# messageDecoder subBs 0# of
                    (# (# a, subOff #) | #)
                      | I# subOff == len ->
                          (# (# a, case I# off' + len of I# r -> r #) | #)
                      | otherwise -> (# | SubMessageError ExtraBytes #)
                    (# | e #) -> (# | SubMessageError e #)
    (# | e #) -> (# | e #)
{-# INLINE decodeFieldMessage #-}


-- | CPS-compatible failure: calls the error continuation.
decodeFail :: DecodeError -> Decoder a
decodeFail e = Decoder $ \_ _ -> (# | e #)
{-# INLINE decodeFail #-}


{- | Decode an enum field (as varint, then fromEnum).

Per the proto3 wire spec, enum values are 32-bit on the wire
even though varints can carry larger values: a sender that
writes @kInt64Max@ for an enum field is expected to be parsed
as the int32 truncation of that value (low 32 bits, sign-
extended). We honour that by casting the decoded 'Word64'
through 'Int32' before handing it to 'toEnum', so a generated
enum with @NEG = -1@ correctly reconstitutes from a 10-byte
varint of @0xFF...FF7F@.
-}
decodeFieldEnum :: Enum a => Decoder a
decodeFieldEnum =
  toEnum . fromIntegral . (fromIntegral :: Word64 -> Int32)
    <$> getVarint
{-# INLINE decodeFieldEnum #-}


-- | Decode a submessage from raw bytes.
decodeSubmessage :: MessageDecode a => ByteString -> Either DecodeError a
decodeSubmessage = runDecoder messageDecoder
{-# INLINE decodeSubmessage #-}


-- Packed decoders: decode a length-delimited chunk containing multiple values.

-- | Decode packed varint values.
decodePackedVarint :: Decoder (VU.Vector Word64)
decodePackedVarint = do
  bs <- getLengthDelimited
  case decodeAllVarints bs of
    Left e -> decodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedVarint #-}


{- | Decode all varints from a packed buffer.

Optimizations borrowed from hyperpb (mcyoung.xyz/2025/07/16/hyperpb):

 1. SWAR pre-count: use the C SWAR routine to count terminator bytes
    in one pass, then allocate the output vector to exact size upfront.
    This avoids grow-and-copy or list-reversal overhead.

 2. Single-byte zero-copy: when every varint is one byte (values 0-127),
    each byte IS the value. We skip varint parsing entirely and just
    widen bytes to Word64. This is the common case for enum fields,
    small indices, and boolean-like repeated fields.
| Decode all varints from a packed buffer.

Adaptive 3-strategy decode from hyperpb's parsePackedVarint:

 1. count == byteLength: every varint is 1 byte (values 0-127).
    Zero-copy widening — each byte IS the value.

 2. count >= byteLength/2: mostly 1-2 byte varints.
    Inline 1-2 byte fast path with fallback for larger varints.
    The 1-2 byte branches are well-predicted because they're common.

 3. count < byteLength/2: many large varints.
    Call full varint decoder (1-2 byte branches would mispredict often).
-}
decodeAllVarints :: ByteString -> Either DecodeError (VU.Vector Word64)
decodeAllVarints bs
  | BS.null bs = Right VU.empty
  | otherwise =
      let !len = BS.length bs
          !n = countPackedVarints bs
      in if n == len
          -- Strategy 1: all single-byte
          then Right $! VU.generate n (fromIntegral . BSU.unsafeIndex bs)
          else Right $! runST $ do
            mv <- MVU.unsafeNew n
            if n >= len `quot` 2
              -- Strategy 2: mostly small varints, inline 1-2 byte fast path
              then do
                let go !idx !off
                      | off >= len = pure ()
                      | otherwise =
                          let !b0 = fromIntegral (BSU.unsafeIndex bs off) :: Word64
                          in if b0 < 0x80
                              then do
                                MVU.unsafeWrite mv idx b0
                                go (idx + 1) (off + 1)
                              else
                                Control.Monad.when (off + 1 < len) $
                                  let !b1 = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Word64
                                  in if b1 < 0x80
                                      then do
                                        MVU.unsafeWrite mv idx ((b0 .&. 0x7F) .|. (b1 `shiftL` 7))
                                        go (idx + 1) (off + 2)
                                      else case runDecoder' getVarint bs off of
                                        DecodeOK v off' -> do
                                          MVU.unsafeWrite mv idx v
                                          go (idx + 1) off'
                                        DecodeFail _ -> pure ()
                go 0 0
              -- Strategy 3: many large varints, full decoder
              else do
                let go !idx !off
                      | off >= len = pure ()
                      | otherwise = case runDecoder' getVarint bs off of
                          DecodeOK v off' -> do
                            MVU.unsafeWrite mv idx v
                            go (idx + 1) off'
                          DecodeFail _ -> pure ()
                go 0 0
            VU.unsafeFreeze mv


-- | Decode packed fixed32 values.
decodePackedFixed32 :: Decoder (VU.Vector Word32)
decodePackedFixed32 = do
  bs <- getLengthDelimited
  case decodeAllFixed32 bs of
    Left e -> decodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedFixed32 #-}


{- | Decode packed fixed32 values. Count is known: byteLength / 4.
On little-endian (x86_64, aarch64-LE): single memcpy of the entire packed
buffer into the vector's backing store. The wire bytes are already the
native representation, so no per-element work is needed.
-}
decodeAllFixed32 :: ByteString -> Either DecodeError (VU.Vector Word32)
decodeAllFixed32 bs
  | r /= 0 = Left (CustomError "packed fixed32: byte length not multiple of 4")
  | otherwise = Right $! unsafeBulkCopyToVectorU n 4 bs
  where
    (!n, !r) = BS.length bs `quotRem` 4


-- | Decode packed fixed64 values. Count is known: byteLength / 8.
decodePackedFixed64 :: Decoder (VU.Vector Word64)
decodePackedFixed64 = do
  bs <- getLengthDelimited
  case decodeAllFixed64 bs of
    Left e -> decodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedFixed64 #-}


decodeAllFixed64 :: ByteString -> Either DecodeError (VU.Vector Word64)
decodeAllFixed64 bs
  | r /= 0 = Left (CustomError "packed fixed64: byte length not multiple of 8")
  | otherwise = Right $! unsafeBulkCopyToVectorU n 8 bs
  where
    (!n, !r) = BS.length bs `quotRem` 8


-- | Decode packed float values. Count is known: byteLength / 4.
decodePackedFloat :: Decoder (VU.Vector Float)
decodePackedFloat = do
  bs <- getLengthDelimited
  case decodeAllFloat bs of
    Left e -> decodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedFloat #-}


decodeAllFloat :: ByteString -> Either DecodeError (VU.Vector Float)
decodeAllFloat bs
  | r /= 0 = Left (CustomError "packed float: byte length not multiple of 4")
  | otherwise = Right $! VU.map castWord32ToFloat (unsafeBulkCopyToVectorU n 4 bs :: VU.Vector Word32)
  where
    (!n, !r) = BS.length bs `quotRem` 4


-- | Decode packed double values. Count is known: byteLength / 8.
decodePackedDouble :: Decoder (VU.Vector Double)
decodePackedDouble = do
  bs <- getLengthDelimited
  case decodeAllDouble bs of
    Left e -> decodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedDouble #-}


decodeAllDouble :: ByteString -> Either DecodeError (VU.Vector Double)
decodeAllDouble bs
  | r /= 0 = Left (CustomError "packed double: byte length not multiple of 8")
  | otherwise = Right $! VU.map castWord64ToDouble (unsafeBulkCopyToVectorU n 8 bs :: VU.Vector Word64)
  where
    (!n, !r) = BS.length bs `quotRem` 8


-- | Bulk-copy a ByteString into a new unboxed vector
unsafeBulkCopyToVectorU
  :: (VU.Unbox a, Storable a)
  => Int
  -> Int
  -> ByteString
  -> VU.Vector a
unsafeBulkCopyToVectorU elemCount elemSize (BSI.BS fp _) =
  let sv = unsafeDupablePerformIO $ do
        mvs <- VSM.unsafeNew elemCount
        VSM.unsafeWith mvs $ \dst ->
          withForeignPtr fp $ \src ->
            copyBytes dst (castPtr src) (elemCount * elemSize)
        VS.unsafeFreeze mvs
  in VU.convert sv
{-# INLINE unsafeBulkCopyToVectorU #-}


-- | Decode packed sint32 values.
decodePackedSVarint32 :: Decoder (VU.Vector Int32)
decodePackedSVarint32 = do
  bs <- getLengthDelimited
  case decodeAllSVarint32 bs of
    Left e -> decodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedSVarint32 #-}


decodeAllSVarint32 :: ByteString -> Either DecodeError (VU.Vector Int32)
decodeAllSVarint32 bs
  | BS.null bs = Right VU.empty
  | otherwise =
      let !n = countPackedVarints bs
      in Right $! runST $ do
          mv <- MVU.unsafeNew n
          let len = BS.length bs
              go !idx !off
                | off >= len = pure ()
                | otherwise = case runDecoder' getSVarint32 bs off of
                    DecodeOK v off' -> do
                      MVU.unsafeWrite mv idx v
                      go (idx + 1) off'
                    DecodeFail _ -> pure ()
          go 0 0
          VU.unsafeFreeze mv


-- | Decode packed sint64 values.
decodePackedSVarint64 :: Decoder (VU.Vector Int64)
decodePackedSVarint64 = do
  bs <- getLengthDelimited
  case decodeAllSVarint64 bs of
    Left e -> decodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedSVarint64 #-}


decodeAllSVarint64 :: ByteString -> Either DecodeError (VU.Vector Int64)
decodeAllSVarint64 bs
  | BS.null bs = Right VU.empty
  | otherwise =
      let !n = countPackedVarints bs
      in Right $! runST $ do
          mv <- MVU.unsafeNew n
          let len = BS.length bs
              go !idx !off
                | off >= len = pure ()
                | otherwise = case runDecoder' getSVarint64 bs off of
                    DecodeOK v off' -> do
                      MVU.unsafeWrite mv idx v
                      go (idx + 1) off'
                    DecodeFail _ -> pure ()
          go 0 0
          VU.unsafeFreeze mv


{- | A lazily-decoded submessage.

The parent message decode captures the submessage's raw bytes but does
not parse them immediately. Parsing is deferred until 'forceLazyMessage'
is called — so if you never access a particular submessage field, its
decode cost is zero.

== When to opt in

Lazy decoding pays off when:

* Your code path only inspects a subset of submessages (e.g. routing
  middleware that reads a header field but forwards the body opaque).
* The submessage is large and infrequently accessed.
* You are decoding many messages in bulk and want to amortise heavy
  submessage parsing.

== How to opt in (text codegen)

Pass @genLazySubmessages = True@ in 'Proto.CodeGen.GenerateOpts' when
generating code.  Every submessage field in the generated types then
has type @'LazyMessage' Foo@ instead of @Foo@.

== Usage

@
-- Given a generated type with a lazy submessage field:
--   data Envelope = Envelope { envelopeBody :: 'LazyMessage' Body, … }

-- Decode the envelope — O(envelope fields), Body not parsed yet.
case 'decodeMessage' raw of
  Left err  -> handleError err
  Right env ->
    -- Force the body only if you actually need it:
    case 'forceLazyMessage' (envelopeBody env) of
      Left err  -> handleBodyError err
      Right body -> use body
@

== Caveats

* A 'LazyMessage' retains a reference to the slice of the original
  input 'ByteString'.  If the input is large and you store
  'LazyMessage' values long-term, consider forcing them promptly to
  avoid holding onto the parent buffer.

* 'Eq' compares the raw bytes, not the decoded value; two structurally
  equal messages encoded differently will compare unequal.
-}
data LazyMessage a = LazyMessage
  { lazyRawBytes :: !ByteString
  -- ^ The raw submessage bytes captured from the parent buffer.
  , lazyCached :: ~(Either DecodeError a)
  -- ^ The lazily-evaluated decode result; not forced until 'forceLazyMessage' is called.
  }


instance Show a => Show (LazyMessage a) where
  show (LazyMessage bs _) = "LazyMessage (" <> show (BS.length bs) <> " bytes)"


instance Eq a => Eq (LazyMessage a) where
  a == b = lazyRawBytes a == lazyRawBytes b


{- | Force a 'LazyMessage', decoding the raw bytes.

Returns @'Left' 'DecodeError'@ if the bytes are malformed.
The result is memoised: calling 'forceLazyMessage' more than once on
the same 'LazyMessage' does not repeat the decode work.
-}
forceLazyMessage :: LazyMessage a -> Either DecodeError a
forceLazyMessage = lazyCached
{-# INLINE forceLazyMessage #-}


-- | Decode a submessage field lazily: capture the bytes but defer parsing.
decodeFieldLazyMessage :: MessageDecode a => Decoder (LazyMessage a)
decodeFieldLazyMessage = do
  bs <- getLengthDelimited
  pure
    LazyMessage
      { lazyRawBytes = bs
      , lazyCached = runDecoder messageDecoder bs
      }
{-# INLINE decodeFieldLazyMessage #-}


{- | An unknown field captured during decoding for round-trip preservation.
Storing unknown fields allows messages to pass through intermediaries
without losing data added by newer protocol versions.
-}
data UnknownField
  = -- | A varint-encoded unknown field (wire type 0).
    UnknownVarint {-# UNPACK #-} !Int {-# UNPACK #-} !Word64
  | -- | A 64-bit fixed-width unknown field (wire type 1).
    UnknownFixed64 {-# UNPACK #-} !Int {-# UNPACK #-} !Word64
  | -- | A 32-bit fixed-width unknown field (wire type 5).
    UnknownFixed32 {-# UNPACK #-} !Int {-# UNPACK #-} !Word32
  | -- | A length-delimited unknown field (wire type 2).
    UnknownLenDelim {-# UNPACK #-} !Int !ByteString
  deriving stock (Show, Eq)


instance NFData UnknownField where
  rnf (UnknownVarint a b) = rnf a `seq` rnf b
  rnf (UnknownFixed64 a b) = rnf a `seq` rnf b
  rnf (UnknownFixed32 a b) = rnf a `seq` rnf b
  rnf (UnknownLenDelim a b) = rnf a `seq` rnf b


-- | Capture an unknown field value during decoding.
captureUnknownField :: Int -> WireType -> Decoder UnknownField
captureUnknownField fn = \case
  WireVarint -> UnknownVarint fn <$> getVarint
  Wire64Bit -> UnknownFixed64 fn <$> getFixed64
  Wire32Bit -> UnknownFixed32 fn <$> getFixed32
  WireLengthDelimited -> UnknownLenDelim fn <$> getLengthDelimited
  wt -> skipField wt >> decodeFail (CustomError ("Unsupported unknown wire type: " <> show wt))


-- | Compute the wire-format size of unknown fields.
unknownFieldsSize :: [UnknownField] -> Int
unknownFieldsSize = foldl' (\acc uf -> acc + unknownFieldSize uf) 0
  where
    unknownFieldSize (UnknownVarint fn val) =
      tagSize fn + varintSize val
    unknownFieldSize (UnknownFixed64 fn _) =
      tagSize fn + 8
    unknownFieldSize (UnknownFixed32 fn _) =
      tagSize fn + 4
    unknownFieldSize (UnknownLenDelim fn val) =
      tagSize fn + varintSize (fromIntegral (BS.length val)) + BS.length val


-- | Re-encode unknown fields for round-trip preservation.
encodeUnknownFields :: [UnknownField] -> B.Builder
encodeUnknownFields = foldMap encodeOne
  where
    encodeOne (UnknownVarint fn val) =
      putTag fn WireVarint <> putVarint val
    encodeOne (UnknownFixed64 fn val) =
      putTag fn Wire64Bit <> putFixed64 val
    encodeOne (UnknownFixed32 fn val) =
      putTag fn Wire32Bit <> putFixed32 val
    encodeOne (UnknownLenDelim fn val) =
      putTag fn WireLengthDelimited <> putLengthDelimited val


-- | 'encodeUnknownFields' fused with 'unknownFieldsSize'. The
-- generated 'buildSized' methods append this fragment at the end of
-- their @SizedBuilder@ chain so unknown fields contribute their
-- exact byte count to the parent's wire size without a second pass.
encodeUnknownFieldsSized :: [UnknownField] -> SB.SizedBuilder
encodeUnknownFieldsSized = foldMap encodeOne
  where
    encodeOne :: UnknownField -> SB.SizedBuilder
    encodeOne (UnknownVarint fn val) =
      SB.sized (tagSize fn + varintSize val) (putTag fn WireVarint <> putVarint val)
    encodeOne (UnknownFixed64 fn val) =
      SB.sized (tagSize fn + 8) (putTag fn Wire64Bit <> putFixed64 val)
    encodeOne (UnknownFixed32 fn val) =
      SB.sized (tagSize fn + 4) (putTag fn Wire32Bit <> putFixed32 val)
    encodeOne (UnknownLenDelim fn val) =
      let !len = BS.length val
          !sz = tagSize fn + varintSize (fromIntegral len) + len
      in SB.sized sz (putTag fn WireLengthDelimited <> putLengthDelimited val)
{-# INLINE encodeUnknownFieldsSized #-}


-- | Decode a map entry (key=field1, value=field2) from a length-delimited chunk.
decodeMapEntry :: Decoder k -> Decoder v -> k -> v -> Decoder (k, v)
decodeMapEntry decK decV = loop
  where
    loop !mk !mv =
      withTagM
        (pure (mk, mv))
        ( \fn wt -> case fn of
            1 -> do kv <- decK; loop kv mv
            2 -> do vv <- decV; loop mk vv
            _ -> skipWireType wt >> loop mk mv
        )
