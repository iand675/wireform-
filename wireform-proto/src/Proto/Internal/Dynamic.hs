{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Dynamic messages for runtime protobuf manipulation.

A 'DynamicMessage' can represent any protobuf message without
compile-time generated code, using field descriptors to interpret
the wire format at runtime.

Use cases:

* Proxies and middleware that forward messages without knowing types
* Schema registries and tooling
* Testing and debugging
* Dynamic configuration systems

This module also provides a schema-driven fast decode path via
'compileParseTable' and 'decodeDynamicWithSchema', inspired by
hyperpb's table-driven parser. The schema-driven path
compiles a 'ProtoMessage' schema into a flat array of 'FieldParser'
entries that a small interpreter loop evaluates against incoming
wire data, sharing the same 'DynamicValue'/'DynamicMessage' types
as the schemaless decoder.
-}
module Proto.Internal.Dynamic (
  -- * Dynamic value type
  DynamicValue (..),

  -- * Dynamic message
  DynamicMessage (..),
  emptyDynamic,
  dynamicField,
  setDynamicField,
  removeDynamicField,

  -- * Encoding / decoding
  encodeDynamic,
  decodeDynamic,

  -- * Streaming decode (length-delimited frames)
  decodeDynamicStream,
  decodeDynamicStreamLazy,

  -- * Schema-driven decoding (fast path)
  ParseTable (..),
  FieldParser (..),
  FieldThunk,
  compileParseTable,
  decodeDynamicWithSchema,
  decodeDynamicStreamWithSchema,

  -- * Conversion
  dynamicToJson,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (..))
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Builder.Int qualified as TLBI
import Data.Vector qualified as V
import Data.Word (Word32, Word64, Word8)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Proto.Internal.Wire (Tag (..), WireType (..), fieldTag)
import Proto.Internal.Wire.Decode (
  DecodeError (..),
  DecodeResult (..),
  Decoder,
  UMaybe (UJust, UNothing),
  getFixed32,
  getFixed64,
  getLengthDelimited,
  getTagOrU,
  getVarint,
  runDecoder,
  runDecoder',
  skipField,
  validateUtf8,
 )
import Proto.Internal.Wire.Encode (
  putByteString,
  putDouble,
  putFixed32,
  putFixed64,
  putFloat,
  putLengthDelimited,
  putTag,
  putText,
  putVarint,
 )
import Proto.Schema (
  FieldDescriptor (..),
  FieldLabel' (..),
  FieldTypeDescriptor (..),
  ProtoMessage (..),
  ScalarFieldType (..),
  SomeFieldDescriptor (..),
 )
import Wireform.Builder qualified as B
import Wireform.FFI (decodeVarintSWAR, relocatePageBoundary)


-- | A dynamically-typed protobuf value.
data DynamicValue
  = DynVarint !Word64
  | DynSVarint !Int64
  | DynFixed32 !Word32
  | DynFixed64 !Word64
  | DynFloat !Float
  | DynDouble !Double
  | DynBool !Bool
  | DynString !Text
  | DynBytes !ByteString
  | DynMessage !DynamicMessage
  | DynEnum !Int
  | DynRepeated ![DynamicValue]
  | DynMap !(Map DynamicValue DynamicValue)
  deriving stock (Show, Eq, Ord)


-- | A dynamically-typed protobuf message, storing fields by number.
data DynamicMessage = DynamicMessage
  { dynFields :: !(IntMap DynamicValue)
  , dynUnknownFields :: ![(Int, WireType, ByteString)]
  }
  deriving stock (Show, Eq, Ord)


-- | An empty dynamic message with no fields and no unknown fields.
emptyDynamic :: DynamicMessage
emptyDynamic = DynamicMessage IntMap.empty []


-- | Look up a field by its proto field number in a dynamic message.
dynamicField :: Int -> DynamicMessage -> Maybe DynamicValue
dynamicField n (DynamicMessage fs _) = IntMap.lookup n fs


-- | Insert or replace a field value at the given field number.
setDynamicField :: Int -> DynamicValue -> DynamicMessage -> DynamicMessage
setDynamicField n v (DynamicMessage fs unk) =
  DynamicMessage (IntMap.insert n v fs) unk


-- | Remove a field by its proto field number, if present.
removeDynamicField :: Int -> DynamicMessage -> DynamicMessage
removeDynamicField n (DynamicMessage fs unk) =
  DynamicMessage (IntMap.delete n fs) unk


{- | Encode a dynamic message to bytes.
Uses varint wire type for integer values, length-delimited for strings/bytes/messages.
-}
encodeDynamic :: DynamicMessage -> ByteString
encodeDynamic (DynamicMessage fs _) =
  BL.toStrict $ B.toLazyByteString $ IntMap.foldlWithKey' encodeField mempty fs
  where
    encodeField acc fn val = acc <> encodeDynValue fn val


encodeDynValue :: Int -> DynamicValue -> B.Builder
encodeDynValue fn = \case
  DynVarint v -> putTag fn WireVarint <> putVarint v
  DynSVarint v -> putTag fn WireVarint <> putVarint (fromIntegral v)
  DynFixed32 v -> putTag fn Wire32Bit <> putFixed32 v
  DynFixed64 v -> putTag fn Wire64Bit <> putFixed64 v
  DynFloat v -> putTag fn Wire32Bit <> putFloat v
  DynDouble v -> putTag fn Wire64Bit <> putDouble v
  DynBool v -> putTag fn WireVarint <> putVarint (if v then 1 else 0)
  DynString v -> putTag fn WireLengthDelimited <> putText v
  DynBytes v -> putTag fn WireLengthDelimited <> putByteString v
  DynEnum v -> putTag fn WireVarint <> putVarint (fromIntegral v)
  DynMessage m ->
    let payload = encodeDynamic m
    in putTag fn WireLengthDelimited <> putLengthDelimited payload
  DynRepeated vs -> foldMap (encodeDynValue fn) vs
  DynMap _kvs -> mempty


-- | Decode a dynamic message from bytes using wire type inference.
decodeDynamic :: ByteString -> Either DecodeError DynamicMessage
decodeDynamic = runDecoder decodeDynLoop


decodeDynLoop :: Decoder DynamicMessage
decodeDynLoop = go IntMap.empty
  where
    go !acc = do
      mt <- getTagOrU
      case mt of
        UNothing -> pure (DynamicMessage acc [])
        UJust (Tag fn wt) -> do
          val <- decodeWireValue wt
          let acc' = case IntMap.lookup fn acc of
                Nothing -> IntMap.insert fn val acc
                Just (DynRepeated vs) -> IntMap.insert fn (DynRepeated (vs <> [val])) acc
                Just existing -> IntMap.insert fn (DynRepeated [existing, val]) acc
          go acc'


decodeWireValue :: WireType -> Decoder DynamicValue
decodeWireValue = \case
  WireVarint -> DynVarint <$> getVarint
  Wire64Bit -> DynFixed64 <$> getFixed64
  Wire32Bit -> DynFixed32 <$> getFixed32
  WireLengthDelimited -> DynBytes <$> getLengthDelimited
  wt -> skipField wt >> pure (DynBytes BS.empty)


{- | Decode a sequence of length-delimited dynamic messages from a strict
'ByteString'. Each message must be preceded by a varint byte count.
-}
decodeDynamicStream :: ByteString -> [Either DecodeError DynamicMessage]
decodeDynamicStream bs = go 0
  where
    !len = BS.length bs
    go !off
      | off >= len = []
      | otherwise = case runDecoder' getVarint bs off of
          DecodeFail _ -> [Left UnexpectedEnd]
          DecodeOK lenW off1 ->
            let !msgLen = fromIntegral lenW
                !off2 = off1 + msgLen
            in if off2 > len
                then [Left UnexpectedEnd]
                else
                  let !msgBs = BSU.unsafeTake msgLen (BSU.unsafeDrop off1 bs)
                  in decodeDynamic msgBs : go off2


{- | Decode a sequence of length-delimited dynamic messages from a lazy
'BL.ByteString'. Useful when reading from a network or file stream.
-}
decodeDynamicStreamLazy :: BL.ByteString -> [Either DecodeError DynamicMessage]
decodeDynamicStreamLazy = decodeDynamicStream . BL.toStrict


-- | Convert a dynamic message to JSON (field numbers as keys).
dynamicToJson :: DynamicMessage -> Aeson.Value
dynamicToJson (DynamicMessage fs _) =
  Aeson.Object
    ( AesonKM.fromList
        (fmap (\(k, v) -> (AesonKey.fromText (intToText k), dynValueToJson v)) (IntMap.toList fs))
    )


dynValueToJson :: DynamicValue -> Aeson.Value
dynValueToJson = \case
  DynVarint v -> Aeson.Number (fromIntegral v)
  DynSVarint v -> Aeson.Number (fromIntegral v)
  DynFixed32 v -> Aeson.Number (fromIntegral v)
  DynFixed64 v -> Aeson.String (word64ToText v)
  DynFloat v -> Aeson.Number (fromFloatDigits v)
  DynDouble v -> Aeson.Number (fromFloatDigits v)
  DynBool v -> Aeson.Bool v
  DynString v -> Aeson.String v
  DynBytes bs -> Aeson.String (TE.decodeUtf8 (Base64.encode bs))
  DynEnum v -> Aeson.Number (fromIntegral v)
  DynMessage m -> dynamicToJson m
  DynRepeated vs -> Aeson.toJSON (fmap dynValueToJson vs)
  DynMap _ -> Aeson.object []


-- ============================================================
-- Schema-driven fast decode path (Table-Driven Parser)
-- ============================================================

{- | A thunk for parsing a single field. Takes a 'ByteString' and offset,
returns either a decode error or the parsed value and new offset.
-}
type FieldThunk = ByteString -> Int -> Either DecodeError (DynamicValue, Int)


-- | A single field's parse configuration in the table.
data FieldParser = FieldParser
  { fpTag :: {-# UNPACK #-} !Word64
  -- ^ The wire tag (field number << 3 | wire type) this entry matches.
  , fpFieldNum :: {-# UNPACK #-} !Int
  -- ^ Proto field number.
  , fpNextOk :: {-# UNPACK #-} !Int
  -- ^ Index of next FieldParser to try on successful match (field scheduling).
  , fpNextErr :: {-# UNPACK #-} !Int
  -- ^ Index of next FieldParser to try on mismatch.
  , fpParse :: !FieldThunk
  -- ^ The thunk that actually decodes this field's value.
  , fpLabel :: !FieldLabel'
  -- ^ Whether this field is repeated (affects accumulation).
  , fpSubmsg :: !(Maybe ParseTable)
  -- ^ Nested parse table for submessage fields.
  }


-- | Compiled parse table for a message type.
data ParseTable = ParseTable
  { ptFields :: !(V.Vector FieldParser)
  -- ^ Field parsers in scheduled order.
  , ptTagLUT :: !ByteString
  -- ^ 128-byte LUT: tag byte -> index in ptFields (0xFF = miss).
  -- For tags < 128 (field numbers 1-15), this is a direct O(1) lookup.
  , ptTagMap :: !(IntMap Int)
  -- ^ Fallback map: wire tag -> index in ptFields, for tags >= 128.
  , ptMaxMiss :: {-# UNPACK #-} !Int
  -- ^ Max consecutive misses before hitting the hash table.
  }


-- | Compile a 'ProtoMessage' schema into a 'ParseTable'.
compileParseTable :: forall a. ProtoMessage a => Proxy a -> ParseTable
compileParseTable proxy =
  let fieldList = IntMap.toAscList (protoFieldDescriptors proxy)
      nFields = length fieldList

      fpList :: [(Int, FieldParser)]
      fpList =
        [ (i, mkFieldParser i nFields fd)
        | (i, (_, SomeField fd)) <- zip [0 ..] fieldList
        ]

      parsers = V.fromList (fmap snd fpList)

      tagLUTBytes = BS.pack (fmap tagLUTEntry [0 .. 127])

      tagIdxMap :: IntMap Int
      tagIdxMap =
        IntMap.fromList
          [ (fromIntegral (fpTag (snd fp)), fst fp)
          | fp <- fpList
          ]

      tagLUTEntry :: Word8 -> Word8
      tagLUTEntry tag =
        case IntMap.lookup (fromIntegral tag) tagIdxMap of
          Just idx | idx < 256 -> fromIntegral idx
          _ -> 0xFF

      tagMap =
        IntMap.fromList
          [ (fromIntegral (fpTag fp), i)
          | (i, fp) <- zip [0 ..] (V.toList parsers)
          ]
  in ParseTable
      { ptFields = parsers
      , ptTagLUT = tagLUTBytes
      , ptTagMap = tagMap
      , ptMaxMiss = min 4 nFields
      }


-- | Build a FieldParser from a schema FieldDescriptor.
mkFieldParser :: Int -> Int -> FieldDescriptor msg a -> FieldParser
mkFieldParser idx nFields fd =
  let fn = fdNumber fd
      wt = fieldWireType (fdTypeDesc fd)
      tag = fieldTag fn wt
      nextOk = (idx + 1) `mod` nFields
      nextErr = (idx + 1) `mod` nFields
  in FieldParser
      { fpTag = tag
      , fpFieldNum = fn
      , fpNextOk = nextOk
      , fpNextErr = nextErr
      , fpParse = mkThunk (fdTypeDesc fd)
      , fpLabel = fdLabel fd
      , fpSubmsg = Nothing
      }


fieldWireType :: FieldTypeDescriptor -> WireType
fieldWireType = \case
  ScalarType DoubleField -> Wire64Bit
  ScalarType FloatField -> Wire32Bit
  ScalarType Int32Field -> WireVarint
  ScalarType Int64Field -> WireVarint
  ScalarType UInt32Field -> WireVarint
  ScalarType UInt64Field -> WireVarint
  ScalarType SInt32Field -> WireVarint
  ScalarType SInt64Field -> WireVarint
  ScalarType Fixed32Field -> Wire32Bit
  ScalarType Fixed64Field -> Wire64Bit
  ScalarType SFixed32Field -> Wire32Bit
  ScalarType SFixed64Field -> Wire64Bit
  ScalarType BoolField -> WireVarint
  ScalarType StringField -> WireLengthDelimited
  ScalarType BytesField -> WireLengthDelimited
  MessageType _ -> WireLengthDelimited
  EnumType _ -> WireVarint
  MapType _ _ -> WireLengthDelimited


-- | Make a decode thunk for a field type.
mkThunk :: FieldTypeDescriptor -> FieldThunk
mkThunk = \case
  ScalarType DoubleField -> thunkFixed64 (DynDouble . castWord64ToDouble)
  ScalarType FloatField -> thunkFixed32 (DynFloat . castWord32ToFloat)
  ScalarType Int32Field -> thunkVarint DynVarint
  ScalarType Int64Field -> thunkVarint DynVarint
  ScalarType UInt32Field -> thunkVarint DynVarint
  ScalarType UInt64Field -> thunkVarint DynVarint
  ScalarType SInt32Field -> thunkVarint DynVarint
  ScalarType SInt64Field -> thunkVarint DynVarint
  ScalarType Fixed32Field -> thunkFixed32 DynFixed32
  ScalarType Fixed64Field -> thunkFixed64 DynFixed64
  ScalarType SFixed32Field -> thunkFixed32 DynFixed32
  ScalarType SFixed64Field -> thunkFixed64 DynFixed64
  ScalarType BoolField -> thunkVarint (\v -> DynBool (v /= 0))
  ScalarType StringField -> thunkLenDelim (fmap DynString . decodeTextValue)
  ScalarType BytesField -> thunkLenDelim (Right . DynBytes)
  MessageType _ -> thunkLenDelim (fmap DynMessage . decodeSubmsg)
  EnumType _ -> thunkVarint (DynEnum . fromIntegral)
  MapType _ _ -> thunkLenDelim (Right . DynBytes)


decodeTextValue :: ByteString -> Either DecodeError Text
decodeTextValue bs
  | validateUtf8 bs = Right (TE.decodeUtf8Lenient bs)
  | otherwise = Left InvalidUtf8


-- Thunk builders: decode a wire value at a given offset in a ByteString.

thunkVarint :: (Word64 -> DynamicValue) -> FieldThunk
thunkVarint f bs off =
  case runDecoder' getVarint bs off of
    DecodeOK v off' -> Right (f v, off')
    DecodeFail e -> Left e


thunkFixed32 :: (Word32 -> DynamicValue) -> FieldThunk
thunkFixed32 f bs off =
  case runDecoder' getFixed32 bs off of
    DecodeOK v off' -> Right (f v, off')
    DecodeFail e -> Left e


thunkFixed64 :: (Word64 -> DynamicValue) -> FieldThunk
thunkFixed64 f bs off =
  case runDecoder' getFixed64 bs off of
    DecodeOK v off' -> Right (f v, off')
    DecodeFail e -> Left e


thunkLenDelim :: (ByteString -> Either DecodeError DynamicValue) -> FieldThunk
thunkLenDelim f bs off =
  case runDecoder' getLengthDelimited bs off of
    DecodeOK bytes off' -> case f bytes of
      Right v -> Right (v, off')
      Left e -> Left e
    DecodeFail e -> Left e


-- | Decode a submessage using schemaless wire-type inference.
decodeSubmsg :: ByteString -> Either DecodeError DynamicMessage
decodeSubmsg = decodeDynamic


{- | Schemaless best-effort raw decode. Silently skips malformed fields
since there is no schema to validate against.
-}
decodeRaw :: ByteString -> DynamicMessage
decodeRaw bs = DynamicMessage (go IntMap.empty 0) []
  where
    !len = BS.length bs
    go !acc !off
      | off >= len = acc
      | otherwise = case runDecoder' getVarint bs off of
          DecodeFail _ -> acc
          DecodeOK tagW off1 ->
            let !fn = fromIntegral (tagW `div` 8) :: Int
                !wt = fromIntegral (tagW `mod` 8) :: Int
            in case wt of
                 0 -> case runDecoder' getVarint bs off1 of
                   DecodeOK v off2 -> go (IntMap.insert fn (DynVarint v) acc) off2
                   DecodeFail _ -> acc
                 1 -> case runDecoder' getFixed64 bs off1 of
                   DecodeOK v off2 -> go (IntMap.insert fn (DynFixed64 v) acc) off2
                   DecodeFail _ -> acc
                 2 -> case runDecoder' getLengthDelimited bs off1 of
                   DecodeOK v off2 -> go (IntMap.insert fn (DynBytes v) acc) off2
                   DecodeFail _ -> acc
                 5 -> case runDecoder' getFixed32 bs off1 of
                   DecodeOK v off2 -> go (IntMap.insert fn (DynFixed32 v) acc) off2
                   DecodeFail _ -> acc
                 _ -> acc


{- | Decode a message using a compiled 'ParseTable'.

This is the schema-driven fast decode path. The core table-driven interpreter loop:

1. Decode a tag varint (SWAR-accelerated for common cases).
2. If tag < 128, use the TagLUT for O(1) field lookup.
3. Otherwise, try the predicted next field ('fpNextOk').
4. On mismatch, walk 'fpNextErr' up to 'ptMaxMiss' times.
5. Fall back to the tag hash map.
6. Call the matched field's thunk ('fpParse') to decode the value.
7. Accumulate into a strict 'IntMap' (the native 'DynamicMessage' storage).
8. Repeat until end of input.
-}
decodeDynamicWithSchema :: ParseTable -> ByteString -> Either DecodeError DynamicMessage
decodeDynamicWithSchema pt bs0
  | BS.null bs0 = Right (DynamicMessage IntMap.empty [])
  | V.null (ptFields pt) = Right (decodeRaw bs0)
  | otherwise =
      let !bs = relocatePageBoundary bs0
          !len = BS.length bs0

          decodeTag !off
            | off + 8 <= BS.length bs =
                case decodeVarintSWAR bs off of
                  Just (v, consumed) -> Just (v, off + consumed)
                  Nothing -> decodeTagSlow off
            | otherwise = decodeTagSlow off

          decodeTagSlow !off =
            case runDecoder' getVarint bs0 off of
              DecodeOK v o -> Just (v, o)
              DecodeFail _ -> Nothing

          go !fields !off !curIdx
            | off >= len = Right fields
            | otherwise =
                case decodeTag off of
                  Nothing -> Right fields
                  Just (tagW, off1)
                    | off1 > len -> Right fields
                    | otherwise ->
                        let !tagInt = fromIntegral tagW :: Int
                        in case findField pt tagW tagInt curIdx of
                             Just idx ->
                               let !fp = ptFields pt V.! idx
                               in case fpParse fp bs0 off1 of
                                    Left e -> Left e
                                    Right (val, off2) ->
                                      let fn = fpFieldNum fp
                                          val' = case fpLabel fp of
                                            LabelRepeated -> case IntMap.lookup fn fields of
                                              Just (DynRepeated vs) -> DynRepeated (vs ++ [val])
                                              Just existing -> DynRepeated [existing, val]
                                              Nothing -> val
                                            _ -> val
                                      in go (IntMap.insert fn val' fields) off2 (fpNextOk fp)
                             Nothing ->
                               case skipWireValue (fromIntegral (tagW `mod` 8)) bs0 off1 of
                                 Just off2 -> go fields off2 curIdx
                                 Nothing -> Right fields
      in fmap (flip DynamicMessage []) (go IntMap.empty 0 0)


{- | Decode a sequence of length-delimited dynamic messages from a strict
'ByteString' using a compiled 'ParseTable'. Each message must be preceded
by a varint byte count.
-}
decodeDynamicStreamWithSchema :: ParseTable -> ByteString -> [Either DecodeError DynamicMessage]
decodeDynamicStreamWithSchema pt bs = go 0
  where
    !len = BS.length bs
    go !off
      | off >= len = []
      | otherwise = case runDecoder' getVarint bs off of
          DecodeFail _ -> [Left UnexpectedEnd]
          DecodeOK lenW off1 ->
            let !msgLen = fromIntegral lenW
                !off2 = off1 + msgLen
            in if off2 > len
                then [Left UnexpectedEnd]
                else
                  let !msgBs = BSU.unsafeTake msgLen (BSU.unsafeDrop off1 bs)
                  in decodeDynamicWithSchema pt msgBs : go off2


-- | Find the matching field parser index for a given tag.
findField :: ParseTable -> Word64 -> Int -> Int -> Maybe Int
findField pt tagW tagInt curIdx
  -- TagLUT fast path: single-byte tags (field numbers 1-15)
  | tagInt >= 0
  , tagInt < 128 =
      let !lutVal = BSU.unsafeIndex (ptTagLUT pt) tagInt
      in if lutVal /= 0xFF then Just (fromIntegral lutVal) else Nothing
  -- Predicted next field
  | curIdx < V.length (ptFields pt)
  , let fp = ptFields pt V.! curIdx
  , fpTag fp == tagW =
      Just curIdx
  -- Walk NextErr chain
  | otherwise = walkErr pt tagW curIdx (ptMaxMiss pt)


walkErr :: ParseTable -> Word64 -> Int -> Int -> Maybe Int
walkErr pt tagW curIdx !tries
  | tries <= 0 = IntMap.lookup (fromIntegral tagW) (ptTagMap pt)
  | curIdx >= V.length (ptFields pt) = IntMap.lookup (fromIntegral tagW) (ptTagMap pt)
  | otherwise =
      let !fp = ptFields pt V.! curIdx
      in if fpTag fp == tagW
          then Just curIdx
          else walkErr pt tagW (fpNextErr fp) (tries - 1)


-- | Skip a wire value based on wire type. Returns new offset or Nothing.
skipWireValue :: Int -> ByteString -> Int -> Maybe Int
skipWireValue wt bs off = case wt of
  0 -> case runDecoder' getVarint bs off of
    DecodeOK _ off' -> Just off'
    DecodeFail _ -> Nothing
  1 -> let off' = off + 8 in if off' <= BS.length bs then Just off' else Nothing
  2 -> case runDecoder' getVarint bs off of
    DecodeOK lenW off' ->
      let off'' = off' + fromIntegral lenW
      in if off'' <= BS.length bs then Just off'' else Nothing
    DecodeFail _ -> Nothing
  5 -> let off' = off + 4 in if off' <= BS.length bs then Just off' else Nothing
  _ -> Nothing


intToText :: Int -> Text
intToText = TL.toStrict . TLB.toLazyText . TLBI.decimal


word64ToText :: Word64 -> Text
word64ToText = TL.toStrict . TLB.toLazyText . TLBI.decimal
