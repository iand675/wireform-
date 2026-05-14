{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

{- | Proto2 extensions runtime support.

Proto2 @extend Foo { optional int32 bar = 123; }@ declarations
expose typed accessors for fields whose numbers live in a message's
declared extension ranges. At the Haskell level we preserve those
fields through the existing unknown-field machinery in
"Proto.Decode" — each extension becomes a typed @'Extension' msg a@
descriptor that knows how to read the corresponding entry out of a
message's unknown-fields list and how to write it back in.

Typical generated shape:

@
-- In @Foo.hs@:
data Foo = Foo { fooUnknownFields :: ![UnknownField], ... }

-- In the module that contains the @extend Foo@ block:
barExt :: Extension Foo Int32
barExt = Extension
  { extNumber = 123
  , extType   = ExtInt32
  }
@

Callers then use 'getExtension' / 'setExtension' / 'clearExtension'
to read, write, and remove the value.
-}
module Proto.Extension (
  -- * Extension descriptors
  Extension (..),
  ExtensionType (..),

  -- * Accessing extension values on a message
  HasExtensions (..),
  hasExtension,
  getExtension,
  setExtension,
  clearExtension,

  -- * Repeated extensions
  RepeatedExtension (..),
  getRepeatedExtension,
  setRepeatedExtension,
  appendRepeatedExtension,
  clearRepeatedExtension,

  -- * Internal helpers

  -- | Reused by "Proto.Internal.JSON.Extension" to bridge the
  -- bracket-quoted JSON @[FQN]@ extension key syntax through
  -- the same encoder \/ decoder used for typed extension
  -- accessors.
  encodeExtensionValue,
  decodeExtensionValue,
  unknownFieldNumber,
) where

import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Word (Word32, Word64)
import GHC.Float (
  castDoubleToWord64,
  castFloatToWord32,
  castWord32ToFloat,
  castWord64ToDouble,
 )
import Proto.Internal.Decode (UnknownField (..))


-- ============================================================
-- Types
-- ============================================================

{- | A typed extension descriptor. Carries the wire-level field
number plus the type information needed to decode/encode the
value and tie it to the right 'UnknownField' constructor.
-}
data Extension msg a = Extension
  { extNumber :: !Int
  -- ^ The field number declared in the @extend@ block.
  , extType :: !(ExtensionType a)
  -- ^ Payload type information — drives the decoder + encoder.
  }


deriving stock instance Show (ExtensionType a) => Show (Extension msg a)


{- | Payload types supported by extensions today. Covers every
singular scalar proto2 supports plus an embedded message escape
hatch that round-trips raw bytes. Repeated / packed extensions
aren't covered here; the generated 'Extension' for those would
need a small additional wrapper.
-}
data ExtensionType a where
  ExtInt32 :: ExtensionType Int32
  ExtInt64 :: ExtensionType Int64
  ExtUInt32 :: ExtensionType Word32
  ExtUInt64 :: ExtensionType Word64
  ExtSInt32 :: ExtensionType Int32
  ExtSInt64 :: ExtensionType Int64
  ExtFixed32 :: ExtensionType Word32
  ExtFixed64 :: ExtensionType Word64
  ExtSFixed32 :: ExtensionType Int32
  ExtSFixed64 :: ExtensionType Int64
  ExtFloat :: ExtensionType Float
  ExtDouble :: ExtensionType Double
  ExtBool :: ExtensionType Bool
  ExtString :: ExtensionType Text
  ExtBytes :: ExtensionType ByteString
  ExtMessage :: ExtensionType ByteString
    -- ^ Sub-message stored as its raw length-delimited payload;
    -- callers use 'Proto.Decode.decodeMessage' to re-project.


deriving stock instance Show (ExtensionType a)


-- ============================================================
-- Accessors
-- ============================================================

{- | Lens-like access to the unknown-fields list on a message.
Generated message types that carry extensions provide an instance
of this class; the three combinators below are written once in
terms of the instance.
-}
class HasExtensions msg where
  messageUnknownFields :: msg -> [UnknownField]
  setMessageUnknownFields :: [UnknownField] -> msg -> msg


-- | 'True' when the message carries a value for this extension.
hasExtension :: HasExtensions msg => Extension msg a -> msg -> Bool
hasExtension ext msg =
  any
    (\uf -> unknownFieldNumber uf == extNumber ext)
    (messageUnknownFields msg)


{- | Retrieve an extension value. Returns 'Nothing' when the field
is absent or the stored bytes don't fit the declared payload
type (corruption or aliasing — real deployments treat the
missing case as "use the extension's proto default", which
callers can layer on top).
-}
getExtension :: HasExtensions msg => Extension msg a -> msg -> Maybe a
getExtension ext msg = do
  uf <- lookupField (extNumber ext) (messageUnknownFields msg)
  decodeExtensionValue (extType ext) uf


{- | Attach (or overwrite) an extension value. The underlying
unknown-fields list has any prior entries for the same field
number removed before the new one is appended, matching
protobuf's "last one wins" semantics for singular fields.
-}
setExtension
  :: HasExtensions msg => Extension msg a -> a -> msg -> msg
setExtension ext value msg =
  let !fresh = encodeExtensionValue (extNumber ext) (extType ext) value
      !rest =
        filter
          (\uf -> unknownFieldNumber uf /= extNumber ext)
          (messageUnknownFields msg)
  in setMessageUnknownFields (rest ++ [fresh]) msg


{- | Remove an extension value. Leaves the message unchanged when
the extension wasn't set.
-}
clearExtension
  :: HasExtensions msg => Extension msg a -> msg -> msg
clearExtension ext msg =
  setMessageUnknownFields
    ( filter
        (\uf -> unknownFieldNumber uf /= extNumber ext)
        (messageUnknownFields msg)
    )
    msg


-- ============================================================
-- Internal
-- ============================================================

-- | Extract the field number from an 'UnknownField'.
unknownFieldNumber :: UnknownField -> Int
unknownFieldNumber = \case
  UnknownVarint n _ -> n
  UnknownFixed64 n _ -> n
  UnknownLenDelim n _ -> n
  UnknownFixed32 n _ -> n


-- The proto spec says later occurrences of a singular field
-- override earlier ones, so we walk from the tail.
lookupField :: Int -> [UnknownField] -> Maybe UnknownField
lookupField fn = go . reverse
  where
    go [] = Nothing
    go (uf : rest)
      | unknownFieldNumber uf == fn = Just uf
      | otherwise = go rest


-- | Decode a typed value from an 'UnknownField' using the given 'ExtensionType'.
decodeExtensionValue :: ExtensionType a -> UnknownField -> Maybe a
decodeExtensionValue ty uf = case (ty, uf) of
  (ExtInt32, UnknownVarint _ v) -> Just (fromIntegral v)
  (ExtInt64, UnknownVarint _ v) -> Just (fromIntegral v)
  (ExtUInt32, UnknownVarint _ v) -> Just (fromIntegral v)
  (ExtUInt64, UnknownVarint _ v) -> Just v
  (ExtSInt32, UnknownVarint _ v) -> Just (zigzagDecode32 v)
  (ExtSInt64, UnknownVarint _ v) -> Just (zigzagDecode64 v)
  (ExtBool, UnknownVarint _ v) -> Just (v /= 0)
  (ExtFixed32, UnknownFixed32 _ v) -> Just v
  (ExtSFixed32, UnknownFixed32 _ v) -> Just (fromIntegral v)
  (ExtFloat, UnknownFixed32 _ v) -> Just (castWord32ToFloat v)
  (ExtFixed64, UnknownFixed64 _ v) -> Just v
  (ExtSFixed64, UnknownFixed64 _ v) -> Just (fromIntegral v)
  (ExtDouble, UnknownFixed64 _ v) -> Just (castWord64ToDouble v)
  (ExtString, UnknownLenDelim _ b) -> case TE.decodeUtf8' b of
    Right t -> Just t
    Left _ -> Nothing
  (ExtBytes, UnknownLenDelim _ b) -> Just b
  (ExtMessage, UnknownLenDelim _ b) -> Just b
  _ -> Nothing


-- | Encode a typed value into an 'UnknownField' for the given field number and type.
encodeExtensionValue :: Int -> ExtensionType a -> a -> UnknownField
encodeExtensionValue fn ty value = case ty of
  ExtInt32 -> UnknownVarint fn (fromIntegral value)
  ExtInt64 -> UnknownVarint fn (fromIntegral value)
  ExtUInt32 -> UnknownVarint fn (fromIntegral value)
  ExtUInt64 -> UnknownVarint fn value
  ExtSInt32 -> UnknownVarint fn (zigzagEncode32 value)
  ExtSInt64 -> UnknownVarint fn (zigzagEncode64 value)
  ExtBool -> UnknownVarint fn (if value then 1 else 0)
  ExtFixed32 -> UnknownFixed32 fn value
  ExtSFixed32 -> UnknownFixed32 fn (fromIntegral value)
  ExtFloat -> UnknownFixed32 fn (castFloatToWord32 value)
  ExtFixed64 -> UnknownFixed64 fn value
  ExtSFixed64 -> UnknownFixed64 fn (fromIntegral value)
  ExtDouble -> UnknownFixed64 fn (castDoubleToWord64 value)
  ExtString -> UnknownLenDelim fn (TE.encodeUtf8 value)
  ExtBytes -> UnknownLenDelim fn value
  ExtMessage -> UnknownLenDelim fn value


-- ============================================================
-- Repeated extensions
-- ============================================================

{- | A typed repeated-extension descriptor. The 'reIsPacked' flag
selects between protobuf's two repeated-on-the-wire encodings:

  * 'False' (the proto2 default): one wire entry per element,
    all sharing the same field number.
  * 'True' (the proto3 default for fixed-width scalars; opt-in
    in proto2 via @[packed = true]@): a single
    length-delimited entry whose payload is the concatenation
    of every element. Only valid for fixed-width scalar types
    (varint integers, fixed32/64, float/double, bool); strings,
    bytes, and submessages always use the unpacked encoding.
-}
data RepeatedExtension msg a = RepeatedExtension
  { reNumber :: !Int
  , reType :: !(ExtensionType a)
  , reIsPacked :: !Bool
  }


deriving stock instance Show (ExtensionType a) => Show (RepeatedExtension msg a)


{- | Read every value associated with a repeated extension, in
wire order (which matches the order the user wrote them). Both
packed and unpacked encodings are accepted regardless of
'reIsPacked'; protobuf parsers must honour either form on read.
-}
getRepeatedExtension
  :: HasExtensions msg => RepeatedExtension msg a -> msg -> [a]
getRepeatedExtension ext msg =
  concatMap
    decodeOne
    [ uf
    | uf <- messageUnknownFields msg
    , unknownFieldNumber uf == reNumber ext
    ]
  where
    decodeOne uf = case (reType ext, uf) of
      -- Packed scalars can show up as a single UnknownLenDelim.
      (ty, UnknownLenDelim _ payload)
        | not (isLenDelimNative ty) -> decodePacked ty payload
      -- Otherwise, decode as a single unpacked entry.
      (ty, _) ->
        case decodeExtensionValue ty uf of
          Just v -> [v]
          Nothing -> []


{- | Replace every occurrence of the extension with the given list,
in order. Uses packed or unpacked encoding per 'reIsPacked' (the
former requires fixed-width scalar types).
-}
setRepeatedExtension
  :: HasExtensions msg => RepeatedExtension msg a -> [a] -> msg -> msg
setRepeatedExtension ext values msg =
  let !rest =
        filter
          (\uf -> unknownFieldNumber uf /= reNumber ext)
          (messageUnknownFields msg)
      !fresh =
        if reIsPacked ext && isPackable (reType ext)
          then [packRepeated (reNumber ext) (reType ext) values]
          else map (encodeExtensionValue (reNumber ext) (reType ext)) values
  in setMessageUnknownFields (rest ++ fresh) msg


{- | Append one element to a repeated extension. Always produces an
unpacked entry; combine with 'setRepeatedExtension' to repack.
-}
appendRepeatedExtension
  :: HasExtensions msg => RepeatedExtension msg a -> a -> msg -> msg
appendRepeatedExtension ext value msg =
  let !uf = encodeExtensionValue (reNumber ext) (reType ext) value
  in setMessageUnknownFields
      (messageUnknownFields msg ++ [uf])
      msg


-- | Drop every entry for this repeated extension.
clearRepeatedExtension
  :: HasExtensions msg => RepeatedExtension msg a -> msg -> msg
clearRepeatedExtension ext msg =
  setMessageUnknownFields
    ( filter
        (\uf -> unknownFieldNumber uf /= reNumber ext)
        (messageUnknownFields msg)
    )
    msg


{- | Whether the given type's wire form is itself
length-delimited. Only @string@, @bytes@, and @message@ are.
-}
isLenDelimNative :: ExtensionType a -> Bool
isLenDelimNative = \case
  ExtString -> True
  ExtBytes -> True
  ExtMessage -> True
  _ -> False


{- | Whether a packed encoding is permitted for this type. Per the
protobuf spec only fixed-width scalars and varint integers are
packable.
-}
isPackable :: ExtensionType a -> Bool
isPackable ty = not (isLenDelimNative ty)


{- | Decode the body of a packed-format @UnknownLenDelim@ entry
into a list of values.
-}
decodePacked :: ExtensionType a -> ByteString -> [a]
decodePacked ty bs = case ty of
  ExtInt32 -> map fromIntegral (varintList bs)
  ExtInt64 -> map fromIntegral (varintList bs)
  ExtUInt32 -> map fromIntegral (varintList bs)
  ExtUInt64 -> varintList bs
  ExtSInt32 -> map zigzagDecode32 (varintList bs)
  ExtSInt64 -> map zigzagDecode64 (varintList bs)
  ExtBool -> map (/= 0) (varintList bs)
  ExtFixed32 -> chunked 4 readU32 bs
  ExtSFixed32 -> map fromIntegral (chunked 4 readU32 bs)
  ExtFloat -> map castWord32ToFloat (chunked 4 readU32 bs)
  ExtFixed64 -> chunked 8 readU64 bs
  ExtSFixed64 -> map fromIntegral (chunked 8 readU64 bs)
  ExtDouble -> map castWord64ToDouble (chunked 8 readU64 bs)
  -- LEN-delimited types can't be packed; treat the whole payload
  -- as one unpacked element.
  ExtString -> case TE.decodeUtf8' bs of
    Right t -> [t]
    Left _ -> []
  ExtBytes -> [bs]
  ExtMessage -> [bs]


-- | Pack a list of values into a single 'UnknownLenDelim' entry.
packRepeated :: Int -> ExtensionType a -> [a] -> UnknownField
packRepeated fn ty values =
  let !payload = packedPayload ty values
  in UnknownLenDelim fn payload


packedPayload :: ExtensionType a -> [a] -> ByteString
packedPayload ty values =
  BS8.concat $ map encodeOne values
  where
    encodeOne v = case ty of
      ExtInt32 -> encodeVarint (fromIntegral v)
      ExtInt64 -> encodeVarint (fromIntegral v)
      ExtUInt32 -> encodeVarint (fromIntegral v)
      ExtUInt64 -> encodeVarint v
      ExtSInt32 -> encodeVarint (zigzagEncode32 v)
      ExtSInt64 -> encodeVarint (zigzagEncode64 v)
      ExtBool -> encodeVarint (if v then 1 else 0)
      ExtFixed32 -> writeU32 v
      ExtSFixed32 -> writeU32 (fromIntegral v)
      ExtFloat -> writeU32 (castFloatToWord32 v)
      ExtFixed64 -> writeU64 v
      ExtSFixed64 -> writeU64 (fromIntegral v)
      ExtDouble -> writeU64 (castDoubleToWord64 v)
      _ -> BS.empty -- shouldn't be reachable: isPackable guards


varintList :: ByteString -> [Word64]
varintList = go
  where
    go bs
      | BS.null bs = []
      | otherwise = case readVarint bs of
          Nothing -> []
          Just (v, rest) -> v : go rest


readVarint :: ByteString -> Maybe (Word64, ByteString)
readVarint = step 0 0
  where
    step !acc !shift bs
      | BS.null bs = Nothing
      | otherwise =
          let b = BS.head bs
              acc' = acc + (fromIntegral (b .&. 0x7F) `shiftL` shift)
          in if b < 0x80
              then Just (acc', BS.tail bs)
              else step acc' (shift + 7) (BS.tail bs)


encodeVarint :: Word64 -> ByteString
encodeVarint n0 = BS.pack (go n0)
  where
    go n
      | n < 0x80 = [fromIntegral n]
      | otherwise = (fromIntegral (n .&. 0x7F) .|. 0x80) : go (n `shiftR` 7)


writeU32 :: Word32 -> ByteString
writeU32 w =
  BS.pack
    [ fromIntegral w
    , fromIntegral (w `shiftR` 8)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 24)
    ]


writeU64 :: Word64 -> ByteString
writeU64 w =
  BS.pack
    [ fromIntegral w
    , fromIntegral (w `shiftR` 8)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 32)
    , fromIntegral (w `shiftR` 40)
    , fromIntegral (w `shiftR` 48)
    , fromIntegral (w `shiftR` 56)
    ]


readU32 :: ByteString -> Word32
readU32 bs =
  let b0 = fromIntegral (BS.index bs 0) :: Word32
      b1 = fromIntegral (BS.index bs 1) :: Word32
      b2 = fromIntegral (BS.index bs 2) :: Word32
      b3 = fromIntegral (BS.index bs 3) :: Word32
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)


readU64 :: ByteString -> Word64
readU64 bs =
  let b0 = fromIntegral (BS.index bs 0) :: Word64
      b1 = fromIntegral (BS.index bs 1) :: Word64
      b2 = fromIntegral (BS.index bs 2) :: Word64
      b3 = fromIntegral (BS.index bs 3) :: Word64
      b4 = fromIntegral (BS.index bs 4) :: Word64
      b5 = fromIntegral (BS.index bs 5) :: Word64
      b6 = fromIntegral (BS.index bs 6) :: Word64
      b7 = fromIntegral (BS.index bs 7) :: Word64
  in b0
      .|. (b1 `shiftL` 8)
      .|. (b2 `shiftL` 16)
      .|. (b3 `shiftL` 24)
      .|. (b4 `shiftL` 32)
      .|. (b5 `shiftL` 40)
      .|. (b6 `shiftL` 48)
      .|. (b7 `shiftL` 56)


chunked :: Int -> (ByteString -> a) -> ByteString -> [a]
chunked n decode = go
  where
    go bs
      | BS.length bs < n = []
      | otherwise =
          decode (BS.take n bs) : go (BS.drop n bs)


-- Zig-zag encodings per the protobuf spec.
zigzagEncode32 :: Int32 -> Word64
zigzagEncode32 n =
  let !w = fromIntegral n :: Word32
  in fromIntegral ((w `shiftL` 1) `xor` fromIntegral (n `shiftR` 31))


zigzagEncode64 :: Int64 -> Word64
zigzagEncode64 n =
  let !w = fromIntegral n :: Word64
  in (w `shiftL` 1) `xor` fromIntegral (n `shiftR` 63)


zigzagDecode32 :: Word64 -> Int32
zigzagDecode32 v =
  let !w = fromIntegral v :: Word32
  in fromIntegral ((w `shiftR` 1) `xor` negate (w .&. 1))


zigzagDecode64 :: Word64 -> Int64
zigzagDecode64 v =
  fromIntegral ((v `shiftR` 1) `xor` negate (v .&. 1))
