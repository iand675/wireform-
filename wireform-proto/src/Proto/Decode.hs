{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{- | High-level decoding interface for protobuf messages.

This is the primary module for deserialising protobuf messages from
bytes. Most users only need 'decodeMessage'.

== Quick start

@
import Proto.Decode

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
module Proto.Decode (
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
  decodeFieldMessageMerge,
  decodeFieldGroup,
  decodeFieldGroupMerge,
  decodeLengthDelimitedWith,
  decodeGroupWith,
  decodeFieldEnum,
  decodeProtoEnum,
  decodeFieldNullValue,

  -- * Packed repeated field decoding
  decodePackedVarint,
  decodePackedEnum,
  PackedProtoEnum (..),
  decodePackedProtoEnum,
  decodePackedFixed32,
  decodePackedFixed64,
  decodePackedFloat,
  decodePackedDouble,
  decodePackedSVarint32,
  decodePackedSVarint64,
  decodeAllVarints,

  -- * Submessage decoding
  decodeSubmessage,

  -- * Lazy submessage decoding
  LazyMessage (..),
  forceLazyMessage,
  decodeFieldLazyMessage,

  -- * Unknown field preservation
  UnknownField (..),
  captureUnknownField,
  captureVerifiedStringExtension,
  decodeUnknownFields,
  encodeUnknownFields,

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
  decodeMapEntryTyped,
  decodeMessageMapEntry,
  decodeMessageMapEntryTyped,
  decodeProtoEnumMapField,

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
  skipTaggedWireType,
) where

import Control.DeepSeq (NFData (..))
import Control.Monad.ST (runST)
import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int32, Int64)
import Data.List (foldl')
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
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
import Proto.Encode qualified as PE
import Proto.Internal.Wire (WireType (..))
import Proto.Internal.Wire.Decode
import Proto.Internal.Wire.Encode (putFixed32, putFixed64, putLengthDelimited, putTag, putVarint, tagSize, varintSize)
import Proto.Schema (ProtoEnum (..))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Wireform.Builder qualified as B
import Wireform.FFI (countPackedVarints)


-- | Typeclass for types that can be decoded from protobuf wire format.
class MessageDecode a where
  {- | Decode a message from a ByteString, starting from default field values.
  The decoder should consume all bytes of the submessage.
  -}
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
decodeFieldMessage = decodeLengthDelimitedWith messageDecoder
{-# INLINE decodeFieldMessage #-}


-- | Run a decoder against exactly one length-delimited payload.
decodeLengthDelimitedWith :: Decoder a -> Decoder a
decodeLengthDelimitedWith = decodeMessagePayloadWith getLengthDelimited
{-# INLINE decodeLengthDelimitedWith #-}


decodeMessagePayloadWith :: Decoder ByteString -> Decoder a -> Decoder a
decodeMessagePayloadWith payload decoder = Decoder $ \bs off ->
  case runDecoder# payload bs off of
    (# (# subBs, off' #) | #) ->
      case runDecoder# decoder subBs 0# of
        (# (# a, subOff #) | #)
          | I# subOff == BS.length subBs -> (# (# a, off' #) | #)
          | otherwise -> (# | SubMessageError ExtraBytes #)
        (# | e #) -> (# | SubMessageError e #)
    (# | e #) -> (# | e #)
{-# INLINE decodeMessagePayloadWith #-}


-- | Decode a singular submessage occurrence, merging it into the previous
-- value when present. Keep the incoming bytes intact so explicit wire
-- occurrences of implicit defaults can still clear an earlier value.
decodeFieldMessageMerge :: (PE.MessageEncode a, MessageDecode a) => Maybe a -> Decoder a
decodeFieldMessageMerge = decodeMessageMergeWith getLengthDelimited
{-# INLINE decodeFieldMessageMerge #-}


-- | Decode the message between the matching group tags.
decodeFieldGroup :: MessageDecode a => Int -> Decoder a
decodeFieldGroup fn = decodeGroupWith fn messageDecoder
{-# INLINE decodeFieldGroup #-}

-- | Decode a group body and consume its matching end tag.
decodeGroupWith :: Int -> Decoder a -> Decoder a
decodeGroupWith fn = decodeMessagePayloadWith (getGroupBody fn)
{-# INLINE decodeGroupWith #-}


decodeFieldGroupMerge :: (PE.MessageEncode a, MessageDecode a) => Int -> Maybe a -> Decoder a
decodeFieldGroupMerge fn = decodeMessageMergeWith (getGroupBody fn)
{-# INLINE decodeFieldGroupMerge #-}


decodeMessageMergeWith :: (PE.MessageEncode a, MessageDecode a) => Decoder ByteString -> Maybe a -> Decoder a
decodeMessageMergeWith payload Nothing = decodeMessagePayloadWith payload messageDecoder
decodeMessageMergeWith payload (Just previous) = do
  incoming <- payload
  if BS.null incoming
    then pure previous
    else case decodeMessage (PE.encodeMessage previous <> incoming) of
      Left err -> decodeFail (SubMessageError err)
      Right merged -> pure merged
{-# INLINE decodeMessageMergeWith #-}


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
  enumFromVarint <$> getVarint
{-# INLINE decodeFieldEnum #-}


enumFromVarint :: Enum a => Word64 -> a
enumFromVarint = toEnum . fromIntegral . (fromIntegral :: Word64 -> Int32)
{-# INLINE enumFromVarint #-}


protoEnumFromVarint :: ProtoEnum a => Word64 -> Maybe a
protoEnumFromVarint = fromProtoEnumValue . fromIntegral . (fromIntegral :: Word64 -> Int32)
{-# INLINE protoEnumFromVarint #-}


-- | Decode a schema-backed enum field, preserving an unrecognized closed
-- enum value as an unknown varint on the same field number.
decodeProtoEnum
  :: forall a r. (Enum a, ProtoEnum a)
  => Int -> (a -> Decoder r) -> (UnknownField -> Decoder r) -> Decoder r
decodeProtoEnum fn known unknown
  | protoEnumIsOpen (Proxy :: Proxy a) = decodeFieldEnum >>= known
  | otherwise = do
      raw <- getVarint
      case protoEnumFromVarint raw of
        Just value -> known value
        Nothing -> unknown (UnknownVarint fn raw)
{-# INLINE decodeProtoEnum #-}


-- | Decode a packed enum field into the boxed vector used by textual codecs.
decodePackedEnum :: Enum a => Decoder (V.Vector a)
decodePackedEnum = do
  values <- decodePackedVarint
  pure $! V.generate (VU.length values) (enumFromVarint . VU.unsafeIndex values)
{-# INLINE decodePackedEnum #-}


-- | Decoded values and the updated reverse-order unknown field accumulator.
data PackedProtoEnum a = PackedProtoEnum !(V.Vector a) ![UnknownField]


-- | Split a packed enum field into recognized values and unknown varints.
decodePackedProtoEnum
  :: forall a. (Enum a, ProtoEnum a)
  => Int -> [UnknownField] -> Decoder (PackedProtoEnum a)
decodePackedProtoEnum fn unknowns
  | protoEnumIsOpen (Proxy :: Proxy a) =
      flip PackedProtoEnum unknowns <$> decodePackedEnum
  | otherwise = do
      values <- decodePackedVarint
      pure $! runST $ do
        let len = VU.length values
        output <- MV.unsafeNew len
        let go !src !dst !rest
              | src == len = do
                  kept <-
                    if dst == len
                      then V.unsafeFreeze output
                      else V.freeze (MV.unsafeSlice 0 dst output)
                  pure (PackedProtoEnum kept rest)
              | otherwise =
                  let raw = VU.unsafeIndex values src
                  in case protoEnumFromVarint raw of
                       Just value -> do
                         MV.unsafeWrite output dst value
                         go (src + 1) (dst + 1) rest
                       Nothing -> go (src + 1) dst (UnknownVarint fn raw : rest)
        go 0 0 unknowns
{-# INLINE decodePackedProtoEnum #-}


-- | Read NullValue's varint, including the empty submessage written by older
-- Wireform oneof codecs.
decodeFieldNullValue :: Enum a => Int -> Decoder a
decodeFieldNullValue 0 = decodeFieldEnum
decodeFieldNullValue 2 = do
  legacy <- getLengthDelimited
  if BS.null legacy then pure (toEnum 0) else decodeFail ExtraBytes
decodeFieldNullValue wt = decodeFail (InvalidWireType wt)
{-# INLINE decodeFieldNullValue #-}


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

 2. Single-byte values need only widening to Word64.

 3. For longer inputs, favor inline 1–2 byte decoding when terminators
    are dense. For sparse terminators, use the general varint decoder.
-}
decodeAllVarints :: ByteString -> Either DecodeError (VU.Vector Word64)
decodeAllVarints bs
  | len == 0 = Right VU.empty
  | n == len = Right $! VU.generate n (fromIntegral . BSU.unsafeIndex bs)
  | n < len `quot` 2 = decodePackedElements getVarint bs n
  | otherwise = runST $ do
      mv <- MVU.unsafeNew n
      let write !idx !off !v = do
            MVU.unsafeWrite mv idx v
            go (idx + 1) off
          go !idx !off@(I# off#)
            | off == len = Right <$> VU.unsafeFreeze mv
            | otherwise =
                let !b0 = fromIntegral (BSU.unsafeIndex bs off) :: Word64
                in if b0 < 0x80
                     then write idx (off + 1) b0
                     else
                       if off + 1 == len
                         then pure (Left UnexpectedEnd)
                         else
                           let !b1 = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Word64
                           in if b1 < 0x80
                                then write idx (off + 2) ((b0 .&. 0x7F) .|. (b1 `shiftL` 7))
                                else case runDecoder# getVarint bs off# of
                                  (# (# v, next #) | #) -> write idx (I# next) v
                                  (# | e #) -> pure (Left e)
      go 0 0
  where
    len = BS.length bs
    n = countPackedVarints bs


-- Each successful element consumes exactly one of the counted terminators.
-- A malformed element must leave the mutable vector private.
decodePackedElements
  :: VU.Unbox a
  => Decoder a
  -> ByteString
  -> Int
  -> Either DecodeError (VU.Vector a)
decodePackedElements dec bs n
  | BS.null bs = Right VU.empty
  | otherwise = runST $ do
      mv <- MVU.unsafeNew n
      let !len = BS.length bs
          go !idx !off@(I# off#)
            | off == len = Right <$> VU.unsafeFreeze mv
            | otherwise = case runDecoder# dec bs off# of
                (# (# v, next #) | #) -> do
                  MVU.unsafeWrite mv idx v
                  go (idx + 1) (I# next)
                (# | e #) -> pure (Left e)
      go 0 0
{-# INLINE decodePackedElements #-}


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
  case decodePackedElements getSVarint32 bs (countPackedVarints bs) of
    Left e -> decodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedSVarint32 #-}


-- | Decode packed sint64 values.
decodePackedSVarint64 :: Decoder (VU.Vector Int64)
decodePackedSVarint64 = do
  bs <- getLengthDelimited
  case decodePackedElements getSVarint64 bs (countPackedVarints bs) of
    Left e -> decodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedSVarint64 #-}


{- | A lazily-decoded submessage. The raw bytes are captured during the
parent message decode, but the actual submessage parsing is deferred
until 'forceLazyMessage' is called. This is a key performance
optimization from the Buf protobuf performance guide: if the consumer
never accesses a submessage field, the decode cost is zero.
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


-- | Force a lazy message, decoding the raw bytes.
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
  | -- | A legacy group, with its raw body between the start and end tags.
    UnknownGroup {-# UNPACK #-} !Int !ByteString
  deriving stock (Show, Eq)


instance NFData UnknownField where
  rnf (UnknownVarint a b) = rnf a `seq` rnf b
  rnf (UnknownFixed64 a b) = rnf a `seq` rnf b
  rnf (UnknownFixed32 a b) = rnf a `seq` rnf b
  rnf (UnknownLenDelim a b) = rnf a `seq` rnf b
  rnf (UnknownGroup a b) = rnf a `seq` rnf b


-- | Capture an unknown field value during decoding.
captureUnknownField :: Int -> WireType -> Decoder UnknownField
captureUnknownField fn = \case
  WireVarint -> UnknownVarint fn <$> getVarint
  Wire64Bit -> UnknownFixed64 fn <$> getFixed64
  Wire32Bit -> UnknownFixed32 fn <$> getFixed32
  WireLengthDelimited -> UnknownLenDelim fn <$> getLengthDelimited
  WireStartGroup -> UnknownGroup fn <$> getGroupBody fn
  WireEndGroup -> decodeFail (InvalidWireType 4)


-- | Capture a known length-delimited string extension after UTF-8 verification.
-- Keep the original slice for the extension accessor and wire round-trips.
captureVerifiedStringExtension :: Int -> Decoder UnknownField
captureVerifiedStringExtension fn = do
  bytes <- getLengthDelimited
  if validateUtf8 bytes
    then pure (UnknownLenDelim fn bytes)
    else decodeFail InvalidUtf8
{-# INLINE captureVerifiedStringExtension #-}


-- | Read all wire records in a payload without a message schema.
decodeUnknownFields :: ByteString -> Either DecodeError [UnknownField]
decodeUnknownFields = runDecoder (go [])
  where
    go fields = withTagM (pure (reverse fields)) $ \number wire -> do
      field <- captureUnknownField number (toEnum wire)
      go (field : fields)


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
    unknownFieldSize (UnknownGroup fn body) =
      2 * tagSize fn + BS.length body


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
    encodeOne (UnknownGroup fn body) =
      putTag fn WireStartGroup <> B.byteStringCopy body <> putTag fn WireEndGroup


-- | Decode a map entry (key=field1, value=field2) from a length-delimited chunk.
decodeMapEntry :: Decoder k -> Decoder v -> k -> v -> Decoder (k, v)
decodeMapEntry decK decV = decodeMapEntryWith (const True) decK (const True) (const decV) id
{-# INLINE decodeMapEntry #-}


-- | Decode a map entry using its declared key and value wire types.
-- Mismatched members are skipped as unknown members of the entry.
decodeMapEntryTyped :: Int -> Decoder k -> Int -> Decoder v -> k -> v -> Decoder (k, v)
decodeMapEntryTyped keyWire decK valueWire decV =
  decodeMapEntryWith (== keyWire) decK (== valueWire) (const decV) id
{-# INLINE decodeMapEntryTyped #-}


-- | The final value field determines whether a closed-enum map entry is known.
decodeProtoEnumMapField
  :: forall k a r. (Enum a, ProtoEnum a)
  => Int -> Int -> Decoder k -> k -> a
  -> (k -> a -> Decoder r) -> (UnknownField -> Decoder r) -> Decoder r
decodeProtoEnumMapField fn keyWire decK initialKey initialValue known unknown
  | protoEnumIsOpen (Proxy :: Proxy a) = do
      (key, value) <- decodeLengthDelimitedWith (decodeMapEntryTyped keyWire decK 0 decodeFieldEnum initialKey initialValue)
      known key value
  | otherwise = do
      body <- getLengthDelimited
      case runDecoder (decodeMapEntryWith (== keyWire) decK (== 0) readValue id initialKey (Just initialValue)) body of
        Left e -> decodeFail (SubMessageError e)
        Right (key, Just value) -> known key value
        Right (_, Nothing) -> unknown (UnknownLenDelim fn body)
  where
    readValue _ = do
      raw <- getVarint
      pure (protoEnumFromVarint raw)
{-# INLINE decodeProtoEnumMapField #-}


-- | Merge repeated value fields inside a single message-valued map entry.
-- When separate entries carry the same key, the caller still keeps the last.
decodeMessageMapEntry :: (PE.MessageEncode v, MessageDecode v) => Decoder k -> k -> v -> Decoder (k, v)
decodeMessageMapEntry decK initialKey initialValue =
  decodeMessageMapEntryWith (const True) (const True) decK initialKey initialValue
{-# INLINE decodeMessageMapEntry #-}


-- | Decode a message-valued map entry with a declared key wire type.
decodeMessageMapEntryTyped :: (PE.MessageEncode v, MessageDecode v) => Int -> Decoder k -> k -> v -> Decoder (k, v)
decodeMessageMapEntryTyped keyWire = decodeMessageMapEntryWith (== keyWire) (== 2)
{-# INLINE decodeMessageMapEntryTyped #-}


decodeMessageMapEntryWith
  :: (PE.MessageEncode v, MessageDecode v)
  => (Int -> Bool) -> (Int -> Bool) -> Decoder k -> k -> v -> Decoder (k, v)
decodeMessageMapEntryWith acceptsKey acceptsValue decK initialKey initialValue =
  decodeMapEntryWith acceptsKey decK acceptsValue
    (fmap Just . decodeFieldMessageMerge) (maybe initialValue id) initialKey Nothing
{-# INLINE decodeMessageMapEntryWith #-}


decodeMapEntryWith
  :: (Int -> Bool) -> Decoder k -> (Int -> Bool) -> (acc -> Decoder acc) -> (acc -> v) -> k -> acc -> Decoder (k, v)
decodeMapEntryWith acceptsKey decK acceptsValue decV finish = loop
  where
    loop !mk !mv =
      withTagM
        (pure (mk, finish mv))
        ( \fn wt -> case fn of
            1 | acceptsKey wt -> do kv <- decK; loop kv mv
            2 | acceptsValue wt -> do vv <- decV mv; loop mk vv
            _ -> skipTaggedWireType fn wt >> loop mk mv
        )
{-# INLINE decodeMapEntryWith #-}
