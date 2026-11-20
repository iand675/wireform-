{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedSums #-}
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
module Proto.Dynamic (
  -- * Dynamic value type
  DynamicValue (..),

  -- * Dynamic message
  DynamicMessage (DynamicMessage, dynFields, dynUnknownFields),
  emptyDynamic,
  dynamicField,
  setDynamicField,
  removeDynamicField,

  -- * Encoding / decoding
  encodeDynamic,
  decodeDynamic,

  -- * Schema-driven decoding (fast path)
  ParseTable (ParseTable, ptFields, ptTagLUT, ptTagMap, ptMaxMiss),
  FieldParser (..),
  FieldThunk,
  compileParseTable,
  decodeDynamicWithSchema,

  -- * Conversion
  dynamicToJson,
) where

import Control.Exception (Exception, catch, throwIO)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.IORef
import Data.Int (Int32, Int64)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32, Word64)
import GHC.Exts (Int (I#))
import Proto.Decode (
  decodeFail,
  decodeFieldNullValue,
  decodeGroupWith,
  decodeLengthDelimitedWith,
  decodeMapEntryTyped,
  decodePackedDouble,
  decodePackedFixed32,
  decodePackedFixed64,
  decodePackedFloat,
  decodePackedSVarint32,
  decodePackedSVarint64,
  decodePackedVarint,
 )
import Proto.Encode (encodeMapField)
import Proto.Internal.JSON (protoDoubleToJSON, protoFloatToJSON)
import Proto.Internal.Wire (WireType (..), fieldTag, validTagWord)
import Proto.Internal.Wire.Decode (
  DecodeError (..),
  DecodeResult (..),
  Decoder (..),
  getDouble,
  getFixed32,
  getFixed64,
  getFloat,
  getGroupBody,
  getLengthDelimited,
  getSVarint32,
  getSVarint64,
  getTagVarint,
  getText,
  getVarint,
  runDecoder,
  runDecoder',
  skipTaggedWireType,
  validateUtf8,
  withTagM,
 )
import Proto.Internal.Wire.Encode (
  putByteString,
  putDouble,
  putFixed32,
  putFixed64,
  putFloat,
  putLengthDelimited,
  putSVarint64,
  putTag,
  putText,
  putVarint,
 )
import Proto.Schema (
  FieldDescriptor (..),
  FieldLabel' (..),
  FieldTypeDescriptor (..),
  OneofDescriptor (..),
  OneofFieldDescriptor (..),
  ProtoMessage (..),
  ScalarFieldType (..),
  SomeFieldDescriptor (..),
  SomeProtoMessage (..),
 )
import System.IO.Unsafe (unsafePerformIO)
import Wireform.Builder qualified as B
import Wireform.FFI (decodeVarintSWAR)


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
  -- | A submessage encoded between matching group tags.
  | DynGroup !DynamicMessage
  | DynEnum !Int
  | DynRepeated ![DynamicValue]
  | DynMap !(Map DynamicValue DynamicValue)
  deriving stock (Show, Eq, Ord)


data DynamicMessage = DynamicMessageState
  ~(Map Int DynamicValue)
  ~[(Int, WireType, ByteString)]
  !(Maybe [WireRecord])


{- | A dynamically-typed protobuf message, storing fields by number.
Unknown records are in wire order. Each payload starts after its tag
and includes any length prefix or matching end-group tag.

Use 'setDynamicField' and 'removeDynamicField' to retain the encounter order
of unedited records in messages decoded without a schema.
-}
pattern DynamicMessage :: Map Int DynamicValue -> [(Int, WireType, ByteString)] -> DynamicMessage
pattern DynamicMessage {dynFields, dynUnknownFields} <- DynamicMessageState dynFields dynUnknownFields _
  where
    DynamicMessage !fields !unknown = DynamicMessageState fields unknown Nothing
{-# COMPLETE DynamicMessage #-}


-- Retain the encounter order of schemaless fields, since different field
-- numbers can belong to the same oneof. Store records in reverse order so
-- that merging successive embedded payloads adds only the incoming records.
data WireRecord
  = WireField {-# UNPACK #-} !Int !DynamicValue
  | WireGroup {-# UNPACK #-} !Int !ByteString


instance Eq DynamicMessage where
  DynamicMessage a au == DynamicMessage b bu = a == b && au == bu


instance Ord DynamicMessage where
  compare (DynamicMessage a au) (DynamicMessage b bu) = compare a b <> compare au bu


instance Show DynamicMessage where
  showsPrec d (DynamicMessage fields unknown) =
    showParen (d > 10) $
      showString "DynamicMessage {dynFields = "
        . shows fields
        . showString ", dynUnknownFields = "
        . shows unknown
        . showChar '}'


-- | An empty dynamic message with no fields and no unknown fields.
emptyDynamic :: DynamicMessage
emptyDynamic = DynamicMessage Map.empty []


-- | Look up a field by its proto field number in a dynamic message.
dynamicField :: Int -> DynamicMessage -> Maybe DynamicValue
dynamicField n (DynamicMessage fs _) = Map.lookup n fs


-- | Insert or replace a field value at the given field number.
setDynamicField :: Int -> DynamicValue -> DynamicMessage -> DynamicMessage
setDynamicField n v (DynamicMessageState fields unknown records) =
  DynamicMessageState
    (Map.insert n v fields)
    unknown
    (fmap (\wire -> WireField n v : filter (keepWireRecord n) wire) records)


-- | Remove a field by its proto field number, if present.
removeDynamicField :: Int -> DynamicMessage -> DynamicMessage
removeDynamicField n (DynamicMessageState fields unknown records) =
  DynamicMessageState (Map.delete n fields) unknown (fmap (filter (keepWireRecord n)) records)


keepWireRecord :: Int -> WireRecord -> Bool
keepWireRecord n (WireField number _) = n /= number
keepWireRecord _ (WireGroup _ _) = True


{- | Encode a dynamic message to bytes.
Uses varint wire type for integer values, length-delimited for strings/bytes/messages.
-}
encodeDynamic :: DynamicMessage -> ByteString
encodeDynamic = B.toStrictByteString . buildDynamic


buildDynamic :: DynamicMessage -> B.Builder
buildDynamic (DynamicMessageState _ _ (Just records)) = foldl' prependRecord mempty records
  where
    prependRecord acc (WireField fn value) = encodeDynValue fn value <> acc
    prependRecord acc (WireGroup fn payload) = encodeUnknown fn WireStartGroup payload <> acc
buildDynamic (DynamicMessage fs unknown) =
  Map.foldlWithKey' encodeField mempty fs <> foldMap (\(fn, wt, payload) -> encodeUnknown fn wt payload) unknown
  where
    encodeField acc fn val = acc <> encodeDynValue fn val


encodeUnknown :: Int -> WireType -> ByteString -> B.Builder
encodeUnknown fn wt payload = putTag fn wt <> B.byteStringCopy payload


encodeDynValue :: Int -> DynamicValue -> B.Builder
encodeDynValue fn = \case
  DynVarint v -> putTag fn WireVarint <> putVarint v
  DynSVarint v -> putTag fn WireVarint <> putSVarint64 v
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
  DynGroup m -> putTag fn WireStartGroup <> buildDynamic m <> putTag fn WireEndGroup
  DynRepeated vs -> foldMap (encodeDynValue fn) vs
  DynMap entries ->
    Map.foldMapWithKey (\key value -> encodeMapField fn (encodeDynValue 1 key) (encodeDynValue 2 value)) entries


-- | Decode a dynamic message from bytes using wire type inference.
decodeDynamic :: ByteString -> Either DecodeError DynamicMessage
decodeDynamic = runDecoder decodeDynLoop


decodeDynLoop :: Decoder DynamicMessage
decodeDynLoop = fromWireRecords <$> decodeWireRecords


decodeWireRecords :: Decoder [WireRecord]
decodeWireRecords = go []
  where
    go !records =
      withTagM (pure records) $ \fn wt ->
        if wt == 3
          then do
            payload <- captureRawValue fn wt
            go (WireGroup fn payload : records)
          else do
            value <- decodeWireValue wt
            go (WireField fn value : records)


fromWireRecords :: [WireRecord] -> DynamicMessage
fromWireRecords records =
  DynamicMessageState (foldl' collectField Map.empty records) (foldl' collectGroup [] records) (Just records)
  where
    collectField fields (WireField fn value) = Map.insertWith prependWireValue fn value fields
    collectField fields (WireGroup _ _) = fields
    collectGroup unknown (WireField _ _) = unknown
    collectGroup unknown (WireGroup fn payload) = (fn, WireStartGroup, payload) : unknown


decodeWireValue :: Int -> Decoder DynamicValue
decodeWireValue = \case
  0 -> DynVarint <$> getVarint
  1 -> DynFixed64 <$> getFixed64
  2 -> DynBytes <$> getLengthDelimited
  5 -> DynFixed32 <$> getFixed32
  wt -> decodeFail (InvalidWireType wt)


-- The wire skipper checks the value's bounds and validates group nesting
-- before the original bytes are sliced.
captureRawValue :: Int -> Int -> Decoder ByteString
captureRawValue fn wt = Decoder $ \bs start ->
  case runDecoder# (skipTaggedWireType fn wt) bs start of
    (# (# (), end #) | #) ->
      (# (# BSU.unsafeTake (I# end - I# start) (BSU.unsafeDrop (I# start) bs), end #) | #)
    (# | err #) -> (# | err #)
{-# INLINE captureRawValue #-}


-- | Convert a dynamic message to JSON (field numbers as keys).
dynamicToJson :: DynamicMessage -> Aeson.Value
dynamicToJson (DynamicMessage fs _) =
  Aeson.Object
    ( AesonKM.fromList
        (fmap (\(k, v) -> (AesonKey.fromText (intToText k), dynValueToJson v)) (Map.toList fs))
    )


dynValueToJson :: DynamicValue -> Aeson.Value
dynValueToJson = \case
  DynVarint v -> Aeson.Number (fromIntegral v)
  DynSVarint v -> Aeson.Number (fromIntegral v)
  DynFixed32 v -> Aeson.Number (fromIntegral v)
  DynFixed64 v -> Aeson.String (word64ToText v)
  DynFloat v -> protoFloatToJSON v
  DynDouble v -> protoDoubleToJSON v
  DynBool v -> Aeson.Bool v
  DynString v -> Aeson.String v
  DynBytes bs -> Aeson.String (TE.decodeUtf8 (Base64.encode bs))
  DynEnum v -> Aeson.Number (fromIntegral v)
  DynMessage m -> dynamicToJson m
  DynGroup m -> dynamicToJson m
  DynRepeated vs -> Aeson.toJSON (fmap dynValueToJson vs)
  DynMap entries ->
    Aeson.Object (Map.foldlWithKey' insertEntry AesonKM.empty entries)
    where
      insertEntry object key value =
        AesonKM.insert (AesonKey.fromText (dynMapKeyToText key)) (dynValueToJson value) object


dynMapKeyToText :: DynamicValue -> Text
dynMapKeyToText = \case
  DynString v -> v
  DynBool v -> if v then "true" else "false"
  DynVarint v -> word64ToText v
  DynSVarint v -> int64ToText v
  DynFixed32 v -> word64ToText (fromIntegral v)
  DynFixed64 v -> word64ToText v
  other -> TE.decodeUtf8 (BL.toStrict (Aeson.encode (dynValueToJson other)))


-- ============================================================
-- Schema-driven fast decode path (Table-Driven Parser)
-- ============================================================

{- | A thunk for parsing a single field. Takes a ByteString and offset,
returns the parsed value and new offset.
-}
type FieldThunk = ByteString -> Int -> IO (DynamicValue, Int)


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
  -- ^ Nested parse table for submessage fields. The thunk may return the
  -- raw body as 'DynBytes' or a schemaless 'DynMessage' or 'DynGroup'.
  }


data ParseTable = ParseTableState
  !(V.Vector FieldParser)
  !ByteString
  !(IntMap Int)
  {-# UNPACK #-} !Int
  !(IntMap IntSet.IntSet)
  !(IntMap ClosedEnumField)
  !IntSet.IntSet
  !Bool


{- | Compiled parse table. Manually constructed tables have independent fields.
The 128-byte tag lookup table stores parser indices 0–254 for single-byte
tags. A value of 255 falls back to the map, which contains every wire tag.
-}
pattern ParseTable :: V.Vector FieldParser -> ByteString -> IntMap Int -> Int -> ParseTable
pattern ParseTable {ptFields, ptTagLUT, ptTagMap, ptMaxMiss} <- ParseTableState ptFields ptTagLUT ptTagMap ptMaxMiss _ _ _ _
  where
    ParseTable fields lut tags maxMiss = makeParseTable fields lut tags maxMiss IntMap.empty IntMap.empty
{-# COMPLETE ParseTable #-}


makeParseTable
  :: V.Vector FieldParser
  -> ByteString
  -> IntMap Int
  -> Int
  -> IntMap IntSet.IntSet
  -> IntMap ClosedEnumField
  -> ParseTable
makeParseTable fields lut tags maxMiss otherMembers closedEnums =
  ParseTableState fields lut tags maxMiss otherMembers closedEnums repeatedFields hasChildren
  where
    repeatedFields = V.foldl' collectRepeated IntSet.empty fields
    collectRepeated repeated fp
      | fpLabel fp == LabelRepeated = IntSet.insert (fpFieldNum fp) repeated
      | otherwise = repeated
    hasChildren = V.any (isJust . fpSubmsg) fields


data ClosedEnumField
  = ClosedEnumValue !IntSet.IntSet
  | ClosedEnumMap !ScalarFieldType !IntSet.IntSet


-- | Compile a 'ProtoMessage' schema into a 'ParseTable'.
compileParseTable :: forall a. ProtoMessage a => Proxy a -> ParseTable
compileParseTable proxy =
  let oneofs = protoOneofDescriptors proxy
      descriptors = protoFieldDescriptors proxy
      groupNames = Map.fromList (map (\group -> (odName group, ())) oneofs)
      ordinary (SomeField fd) = not (Map.member (fdName fd) groupNames)
      mapDecoders = protoMapValueDecoders proxy
      entries =
        concatMap (fieldParsers mapDecoders closedEnums) (filter ordinary (Map.elems descriptors))
          <> concatMap (concatMap oneofParsers . odFields) oneofs
      nFields = length entries
      schedule i entry =
        let next = (i + 1) `mod` nFields
        in attachChild entry {fpNextOk = next, fpNextErr = next}
      children = protoSubmessageSchemas proxy
      attachChild entry =
        case Map.lookup (fpFieldNum entry) children of
          Nothing -> entry
          Just (SomeProtoMessage child) ->
            let body
                  | fpTag entry `mod` 8 == 3 = getGroupBody (fpFieldNum entry)
                  | otherwise = getLengthDelimited
            in entry
                 { fpParse = fieldThunk (DynBytes <$> body)
                 , fpSubmsg = Just (compileParseTable child)
                 }
      parsers = V.fromList (zipWith schedule [0 ..] entries)
      tagMap = V.ifoldl' (\m i fp -> IntMap.insert (fromIntegral (fpTag fp)) i m) IntMap.empty parsers
      tagLUTEntry tag = case IntMap.lookup tag tagMap of
        Just idx | idx < 255 -> fromIntegral idx
        _ -> 0xFF
      tagLUTBytes = BS.pack (fmap tagLUTEntry [0 .. 127])
      otherMembers = IntMap.fromList (concatMap groupMembers oneofs)
      closedEnums = IntMap.fromList
        (map (ordinaryEnum descriptors) (Map.toList (protoClosedEnumFields proxy))
          <> concatMap (mapMaybe oneofEnum . odFields) oneofs)
  in makeParseTable parsers tagLUTBytes tagMap (min 4 nFields) otherMembers closedEnums
  where
    oneofParsers field =
      let number = ofdNumber field
          fieldType = ofdType field
          legacy = case fieldType of
            EnumType "google.protobuf.NullValue" ->
              [fieldParser number LabelOptional WireLengthDelimited (fieldThunk (DynEnum <$> decodeFieldNullValue 2))]
            _ -> []
      in fieldParsersFor number fieldType LabelOptional (mkThunk number fieldType) <> legacy
    groupMembers group =
      let members = IntSet.fromList (map ofdNumber (odFields group))
      in map (\number -> (number, IntSet.delete number members)) (IntSet.toList members)
    oneofEnum field =
      fmap (\values -> (ofdNumber field, ClosedEnumValue (IntSet.fromList values))) (ofdEnumValues field)
    ordinaryEnum descriptors (number, values) =
      let allowed = IntSet.fromList values
          info = case Map.lookup number descriptors of
            Just (SomeField fd) | MapType key _ <- fdTypeDesc fd ->
              ClosedEnumMap key allowed
            _ -> ClosedEnumValue allowed
      in (number, info)


fieldParsers :: Map Int (Decoder ()) -> IntMap ClosedEnumField -> SomeFieldDescriptor msg -> [FieldParser]
fieldParsers mapDecoders closedEnums (SomeField fd) =
  fieldParsersFor fn fieldType (fdLabel fd) parse
  where
    fn = fdNumber fd
    fieldType = fdTypeDesc fd
    parse = case fieldType of
      MapType key value
        | Just (ClosedEnumMap _ _) <- IntMap.lookup fn closedEnums ->
            fieldThunk (DynBytes <$> getLengthDelimited)
        | otherwise -> mapFieldThunk key value (Map.lookup fn mapDecoders)
      _ -> mkThunk fn fieldType


-- A packable repeated field accepts either wire spelling.
fieldParsersFor :: Int -> FieldTypeDescriptor -> FieldLabel' -> FieldThunk -> [FieldParser]
fieldParsersFor fn fieldType label parseExpanded
  | label == LabelRepeated
  , Just parse <- mkPackedThunk fieldType =
      [expanded, fieldParser fn label WireLengthDelimited parse]
  | otherwise = [expanded]
  where
    expanded = fieldParser fn label (fieldWireType fieldType) parseExpanded


fieldParser :: Int -> FieldLabel' -> WireType -> FieldThunk -> FieldParser
fieldParser fn label wireType parse =
  FieldParser
    { fpTag = fieldTag fn wireType
    , fpFieldNum = fn
    , fpNextOk = 0
    , fpNextErr = 0
    , fpParse = parse
    , fpLabel = label
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
  GroupType _ -> WireStartGroup
  EnumType _ -> WireVarint
  MapType _ _ -> WireLengthDelimited


-- | Make a decode thunk for a field type.
mkThunk :: Int -> FieldTypeDescriptor -> FieldThunk
mkThunk fn fieldType = case fieldType of
  ScalarType DoubleField -> fieldThunk (DynDouble <$> getDouble)
  ScalarType FloatField -> fieldThunk (DynFloat <$> getFloat)
  ScalarType Int32Field -> fieldThunk (DynVarint <$> getVarint)
  ScalarType Int64Field -> fieldThunk (DynVarint <$> getVarint)
  ScalarType UInt32Field -> fieldThunk (DynVarint <$> getVarint)
  ScalarType UInt64Field -> fieldThunk (DynVarint <$> getVarint)
  ScalarType SInt32Field -> fieldThunk (DynSVarint . fromIntegral <$> getSVarint32)
  ScalarType SInt64Field -> fieldThunk (DynSVarint <$> getSVarint64)
  ScalarType Fixed32Field -> fieldThunk (DynFixed32 <$> getFixed32)
  ScalarType Fixed64Field -> fieldThunk (DynFixed64 <$> getFixed64)
  ScalarType SFixed32Field -> fieldThunk (DynFixed32 <$> getFixed32)
  ScalarType SFixed64Field -> fieldThunk (DynFixed64 <$> getFixed64)
  ScalarType BoolField -> fieldThunk (DynBool . (/= 0) <$> getVarint)
  ScalarType StringField -> fieldThunk (DynString <$> getText)
  ScalarType BytesField -> fieldThunk (DynBytes <$> getLengthDelimited)
  MessageType _ -> fieldThunk (DynMessage <$> decodeLengthDelimitedWith decodeDynLoop)
  GroupType _ -> fieldThunk (DynGroup <$> decodeGroupWith fn decodeDynLoop)
  EnumType _ -> fieldThunk (dynEnumFromVarint <$> getVarint)
  MapType key value -> mapFieldThunk key value Nothing


-- Validate the entry while keeping its original body and occurrence order.
mapFieldThunk :: ScalarFieldType -> FieldTypeDescriptor -> Maybe (Decoder ()) -> FieldThunk
mapFieldThunk key value child = fieldThunk $ do
  body <- getLengthDelimited
  case runDecoder validateEntry body of
    Left e -> decodeFail (SubMessageError e)
    Right _ -> pure (DynBytes body)
  where
    validateEntry =
      decodeMapEntryTyped
        (fromEnum (fieldWireType (ScalarType key))) (validateMapMember 1 (ScalarType key) Nothing)
        (fromEnum (fieldWireType value)) (validateMapMember 2 value child)
        () ()


validateMapMember :: Int -> FieldTypeDescriptor -> Maybe (Decoder ()) -> Decoder ()
validateMapMember number fieldType child = case fieldType of
  ScalarType StringField -> do
    bytes <- getLengthDelimited
    if validateUtf8 bytes then pure () else decodeFail InvalidUtf8
  MessageType _ -> decodeLengthDelimitedWith (fromMaybe skipFields child)
  GroupType _ -> decodeGroupWith number (fromMaybe skipFields child)
  _ -> skipTaggedWireType number (fromEnum (fieldWireType fieldType))
  where
    skipFields = withTagM (pure ()) (\fn wt -> skipTaggedWireType fn wt >> skipFields)


mkPackedThunk :: FieldTypeDescriptor -> Maybe FieldThunk
mkPackedThunk = \case
  ScalarType StringField -> Nothing
  ScalarType BytesField -> Nothing
  MessageType _ -> Nothing
  GroupType _ -> Nothing
  MapType _ _ -> Nothing
  ScalarType DoubleField -> packedField DynDouble decodePackedDouble
  ScalarType FloatField -> packedField DynFloat decodePackedFloat
  ScalarType SInt32Field -> packedField (DynSVarint . fromIntegral) decodePackedSVarint32
  ScalarType SInt64Field -> packedField DynSVarint decodePackedSVarint64
  ScalarType Fixed32Field -> packedField DynFixed32 decodePackedFixed32
  ScalarType SFixed32Field -> packedField DynFixed32 decodePackedFixed32
  ScalarType Fixed64Field -> packedField DynFixed64 decodePackedFixed64
  ScalarType SFixed64Field -> packedField DynFixed64 decodePackedFixed64
  ScalarType BoolField -> packedField (DynBool . (/= 0)) decodePackedVarint
  EnumType _ -> packedField dynEnumFromVarint decodePackedVarint
  _ -> packedField DynVarint decodePackedVarint


dynEnumFromVarint :: Word64 -> DynamicValue
dynEnumFromVarint = DynEnum . enumNumber
{-# INLINE dynEnumFromVarint #-}

enumNumber :: Word64 -> Int
enumNumber = fromIntegral . (fromIntegral :: Word64 -> Int32)
{-# INLINE enumNumber #-}


packedField :: VU.Unbox a => (a -> DynamicValue) -> Decoder (VU.Vector a) -> Maybe FieldThunk
packedField wrap decoder =
  Just (fieldThunk (DynRepeated . VU.foldr (\value rest -> wrap value : rest) [] <$> decoder))
{-# INLINE packedField #-}


-- Built-in thunks carry typed failures through the public IO callback.
-- Only this exception type is caught at the table decoder boundary.
newtype DynamicDecodeFailure = DynamicDecodeFailure DecodeError
  deriving stock (Show)


instance Exception DynamicDecodeFailure


fieldThunk :: Decoder DynamicValue -> FieldThunk
fieldThunk decoder bs off =
  case runDecoder' decoder bs off of
    DecodeOK value next -> pure (value, next)
    DecodeFail e -> throwIO (DynamicDecodeFailure e)
{-# INLINE fieldThunk #-}


{- | Decode a message using a compiled 'ParseTable'.

This is the schema-driven fast decode path. The core table-driven interpreter loop:

1. Decode a tag varint.
2. If tag < 128, use the TagLUT for O(1) field lookup.
3. Otherwise, try the predicted next field ('fpNextOk').
4. On mismatch, walk 'fpNextErr' up to 'ptMaxMiss' times.
5. Fall back to the tag hash map.
6. Call the matched field's thunk ('fpParse') to decode the value.
7. Store the value in the accumulator.
8. Repeat until end of input.
-}
decodeDynamicWithSchema :: ParseTable -> ByteString -> Either DecodeError DynamicMessage
decodeDynamicWithSchema pt bs
  | BS.null bs = Right (DynamicMessage Map.empty [])
  | V.null (ptFields pt) = decodeDynamic bs
  | otherwise = unsafePerformIO $
      (Right <$> decodeWithTable) `catch` \(DynamicDecodeFailure e) -> pure (Left e)
  where
    decodeWithTable = do
      state <- newTableAccumulator pt
      unknown <- scanTable state bs []
      finishTable state unknown


data TableAccumulator = TableAccumulator
  !ParseTable
  !(IORef (IntMap DynamicValue))
  !(Maybe (IORef (IntMap ChildField)))


data ChildField = ChildField !Bool ![ChildAccumulator]


data ChildAccumulator
  = RawChild !(IORef [WireRecord])
  | SchemaChild !TableAccumulator !(IORef [(Int, WireType, ByteString)])


newTableAccumulator :: ParseTable -> IO TableAccumulator
newTableAccumulator pt@(ParseTableState _ _ _ _ _ _ _ hasChildren) = do
  fields <- newIORef IntMap.empty
  children <- if hasChildren then Just <$> newIORef IntMap.empty else pure Nothing
  pure (TableAccumulator pt fields children)


-- Reuse a singular child's state across occurrences so parsing a fragment
-- never has to revisit earlier fragments. Validate each body immediately,
-- including oneof arms that are later replaced.
scanTable
  :: TableAccumulator
  -> ByteString
  -> [(Int, WireType, ByteString)]
  -> IO [(Int, WireType, ByteString)]
scanTable state@(TableAccumulator pt@(ParseTableState _ _ _ _ _ closedEnums _ _) fieldsRef _) bs0 =
  go 0 0
  where
    len = BS.length bs0

    decodeTag !off
      | len - off >= 8 =
          case decodeVarintSWAR bs0 off of
            Just (v, consumed)
              | consumed <= 5 && validTagWord v ->
                  DecodeOK v (off + consumed)
            _ -> decodeTagSlow off
      | otherwise = decodeTagSlow off

    decodeTagSlow !off =
      case runDecoder' getTagVarint bs0 off of
        DecodeOK v _
          | not (validTagWord v) -> DecodeFail (InvalidTag v)
        result -> result

    go !off !curIdx !unknown
      | off >= len = pure unknown
      | otherwise =
          case decodeTag off of
            DecodeFail e -> throwIO (DynamicDecodeFailure e)
            DecodeOK tagW off1 -> do
              let !tagInt = fromIntegral tagW :: Int
              mIdx <- findField pt tagW tagInt curIdx
              case mIdx of
                Just idx ->
                  let !fp = ptFields pt V.! idx
                      !fn = fpFieldNum fp
                      !wt = fromIntegral (tagW `mod` 8)
                  in case IntMap.lookup fn closedEnums of
                       Just (ClosedEnumValue allowed) | wt == 2, fpLabel fp == LabelRepeated ->
                         case runDecoder' decodePackedVarint bs0 off1 of
                           DecodeFail e -> throwIO (DynamicDecodeFailure e)
                           DecodeOK raws off2 -> do
                             fields <- readIORef fieldsRef
                             let previous = reversedRepeatedValues (IntMap.lookup fn fields)
                                 PackedClosedEnums values retained =
                                   VU.foldl' (collectPackedEnum fn allowed) (PackedClosedEnums previous unknown) raws
                             case values of
                               [] -> pure ()
                               [value] -> writeIORef fieldsRef $! IntMap.insert fn value fields
                               _ -> writeIORef fieldsRef $! IntMap.insert fn (DynRepeated values) fields
                             go off2 (fpNextOk fp) retained
                       closed -> do
                         (value, off2) <- fpParse fp bs0 off1
                         accepted <- case (closed, value) of
                           (Just (ClosedEnumValue allowed), DynEnum number) ->
                             pure (IntSet.member number allowed)
                           (Just (ClosedEnumMap key allowed), DynBytes body) ->
                             case runDecoder (lastMapEnum key) body of
                               Left e -> throwIO (DynamicDecodeFailure (SubMessageError e))
                               Right number -> pure (maybe True (\raw -> IntSet.member (enumNumber raw) allowed) number)
                           _ -> pure True
                         if accepted
                           then do
                               case fpSubmsg fp of
                                 Just child -> storeChild state fp child value
                                 Nothing -> storeField state fp value
                               go off2 (fpNextOk fp) unknown
                           else do
                             let payload = BSU.unsafeTake (off2 - off1) (BSU.unsafeDrop off1 bs0)
                             go off2 (fpNextOk fp) ((fn, toEnum wt, payload) : unknown)
                Nothing -> do
                  let !fn = fromIntegral (tagW `div` 8)
                      !wt = fromIntegral (tagW `mod` 8)
                  case runDecoder' (captureRawValue fn wt) bs0 off1 of
                    DecodeOK payload off2 -> go off2 curIdx ((fn, toEnum wt, payload) : unknown)
                    DecodeFail e -> throwIO (DynamicDecodeFailure e)


clearOneofSiblings :: TableAccumulator -> Int -> IO ()
clearOneofSiblings (TableAccumulator (ParseTableState _ _ _ _ otherMembers _ _ _) fieldsRef childrenRef) fn =
  case IntMap.lookup fn otherMembers of
    Just others | not (IntSet.null others) -> do
      modifyIORef' fieldsRef (`IntMap.withoutKeys` others)
      mapM_ (\ref -> modifyIORef' ref (`IntMap.withoutKeys` others)) childrenRef
    _ -> pure ()


storeField :: TableAccumulator -> FieldParser -> DynamicValue -> IO ()
storeField state@(TableAccumulator _ fieldsRef _) fp value =
  case fpLabel fp of
    LabelRepeated -> do
      fields <- readIORef fieldsRef
      case prependRepeated value (IntMap.lookup fn fields) of
        Just repeated -> writeIORef fieldsRef $! IntMap.insert fn repeated fields
        Nothing -> pure ()
    _ -> do
      clearOneofSiblings state fn
      modifyIORef' fieldsRef $ case value of
        DynMessage _ -> IntMap.insertWith mergeSubmessages fn value
        DynGroup _ -> IntMap.insertWith mergeSubmessages fn value
        _ -> IntMap.insert fn value
  where
    fn = fpFieldNum fp


storeChild :: TableAccumulator -> FieldParser -> ParseTable -> DynamicValue -> IO ()
storeChild state@(TableAccumulator _ _ childrenRef) fp pt value =
  case childrenRef of
    Nothing -> storeField state fp value
    Just ref -> do
      body <- case value of
        DynBytes bytes -> pure bytes
        DynMessage message -> pure (encodeDynamic message)
        DynGroup message -> pure (encodeDynamic message)
        _ -> throwIO (DynamicDecodeFailure (CustomError "submessage parser returned a non-message value"))
      clearOneofSiblings state fn
      fields <- readIORef ref
      let previous = case IntMap.lookup fn fields of
            Just (ChildField _ children) -> children
            Nothing -> []
      case previous of
        child : _ | fpLabel fp /= LabelRepeated -> appendChild child body
        _ -> do
          child <- if V.null (ptFields pt)
            then RawChild <$> newIORef []
            else SchemaChild <$> newTableAccumulator pt <*> newIORef []
          appendChild child body
          writeIORef ref $! IntMap.insert fn (ChildField isGroup (child : previous)) fields
  where
    fn = fpFieldNum fp
    isGroup = fpTag fp `mod` 8 == 3


appendChild :: ChildAccumulator -> ByteString -> IO ()
appendChild child body =
  append `catch` \(DynamicDecodeFailure err) -> throwIO (DynamicDecodeFailure (SubMessageError err))
  where
    append = case child of
      RawChild recordsRef -> case runDecoder decodeWireRecords body of
        Left err -> throwIO (DynamicDecodeFailure err)
        Right records -> modifyIORef' recordsRef (records <>)
      SchemaChild state unknownRef -> do
        unknown <- readIORef unknownRef >>= scanTable state body
        writeIORef unknownRef unknown


finishTable :: TableAccumulator -> [(Int, WireType, ByteString)] -> IO DynamicMessage
finishTable (TableAccumulator (ParseTableState _ _ _ _ _ _ repeatedFields _) fieldsRef childrenRef) unknown = do
  fields <- readIORef fieldsRef
  let ordered = IntMap.foldlWithKey' restoreField Map.empty fields
  allFields <- case childrenRef of
    Nothing -> pure ordered
    Just ref -> do
      children <- readIORef ref >>= traverse finishChildren
      pure (IntMap.foldlWithKey' (\acc fn value -> Map.insert fn value acc) ordered children)
  pure (DynamicMessage allFields (reverse unknown))
  where
    restoreField fields fn value = Map.insert fn ordered fields
      where
        ordered
          | IntSet.member fn repeatedFields = restoreRepeatedOrder value
          | otherwise = value
    finishChildren (ChildField isGroup children) =
      case children of
        [child] -> wrap <$> finishChild child
        _ -> DynRepeated <$> traverse (fmap wrap . finishChild) (reverse children)
      where
        wrap = if isGroup then DynGroup else DynMessage


finishChild :: ChildAccumulator -> IO DynamicMessage
finishChild (RawChild ref) = fromWireRecords <$> readIORef ref
finishChild (SchemaChild state unknownRef) = readIORef unknownRef >>= finishTable state


data PackedClosedEnums = PackedClosedEnums ![DynamicValue] ![(Int, WireType, ByteString)]


collectPackedEnum :: Int -> IntSet.IntSet -> PackedClosedEnums -> Word64 -> PackedClosedEnums
collectPackedEnum fn allowed (PackedClosedEnums values unknown) raw
  | IntSet.member number allowed = PackedClosedEnums (DynEnum number : values) unknown
  | otherwise = PackedClosedEnums values ((fn, WireVarint, B.toStrictByteString (putVarint raw)) : unknown)
  where
    number = enumNumber raw


lastMapEnum :: ScalarFieldType -> Decoder (Maybe Word64)
lastMapEnum key =
  let keyType = ScalarType key
  in snd <$> decodeMapEntryTyped (fromEnum (fieldWireType keyType)) (validateMapMember 1 keyType Nothing)
       0 (Just <$> getVarint) () Nothing


-- Both payloads have already passed wire validation. Concatenating their
-- records applies protobuf's merge rules without guessing the child's schema.
mergeSubmessages :: DynamicValue -> DynamicValue -> DynamicValue
mergeSubmessages (DynMessage incoming) (DynMessage previous) = DynMessage (mergeBodies incoming previous)
mergeSubmessages (DynGroup incoming) (DynGroup previous) = DynGroup (mergeBodies incoming previous)
mergeSubmessages incoming _ = incoming

mergeBodies :: DynamicMessage -> DynamicMessage -> DynamicMessage
mergeBodies (DynamicMessageState _ _ (Just incoming)) (DynamicMessageState _ _ (Just previous)) =
  fromWireRecords (incoming <> previous)
mergeBodies incoming _ = incoming


prependWireValue :: DynamicValue -> DynamicValue -> DynamicValue
prependWireValue value (DynRepeated values) = DynRepeated (value : values)
prependWireValue value previous = DynRepeated [value, previous]
{-# INLINE prependWireValue #-}


restoreRepeatedOrder :: DynamicValue -> DynamicValue
restoreRepeatedOrder (DynRepeated values) = DynRepeated (reverse values)
restoreRepeatedOrder value = value


-- Keep values in reverse order while parsing. An empty packed run has no
-- effect, and a single occurrence keeps the same representation in both
-- wire spellings.
prependRepeated :: DynamicValue -> Maybe DynamicValue -> Maybe DynamicValue
prependRepeated incoming previous = case incoming of
  DynRepeated [] -> previous
  DynRepeated [value] -> prependOne value
  DynRepeated values -> Just (DynRepeated (foldl' (flip (:)) reversed values))
  value -> prependOne value
  where
    prependOne value = Just (maybe value (prependWireValue value) previous)
    reversed = reversedRepeatedValues previous
{-# INLINE prependRepeated #-}

reversedRepeatedValues :: Maybe DynamicValue -> [DynamicValue]
reversedRepeatedValues Nothing = []
reversedRepeatedValues (Just (DynRepeated values)) = values
reversedRepeatedValues (Just value) = [value]
{-# INLINE reversedRepeatedValues #-}


-- | Find the matching field parser for a given tag.
findField :: ParseTable -> Word64 -> Int -> Int -> IO (Maybe Int)
findField pt tagW tagInt curIdx
  -- TagLUT fast path: single-byte tags (field numbers 1-15)
  | tagInt >= 0
  , tagInt < 128 =
      let !lutVal = BSU.unsafeIndex (ptTagLUT pt) tagInt
      in if lutVal /= 0xFF
           then pure (Just (fromIntegral lutVal))
           else pure (IntMap.lookup tagInt (ptTagMap pt))
  -- Predicted next field
  | curIdx < V.length (ptFields pt)
  , let fp = ptFields pt V.! curIdx
  , fpTag fp == tagW =
      pure (Just curIdx)
  -- Walk NextErr chain
  | otherwise = walkErr pt tagW curIdx (ptMaxMiss pt)


walkErr :: ParseTable -> Word64 -> Int -> Int -> IO (Maybe Int)
walkErr pt tagW curIdx !tries
  | tries <= 0 = pure (IntMap.lookup (fromIntegral tagW) (ptTagMap pt))
  | curIdx >= V.length (ptFields pt) = pure (IntMap.lookup (fromIntegral tagW) (ptTagMap pt))
  | otherwise =
      let !fp = ptFields pt V.! curIdx
      in if fpTag fp == tagW
           then pure (Just curIdx)
           else walkErr pt tagW (fpNextErr fp) (tries - 1)


intToText :: Int -> Text
intToText = int64ToText . fromIntegral


int64ToText :: Int64 -> Text
int64ToText n
  | n < 0 = "-" <> word64ToText (fromIntegral (negate n))
  | otherwise = word64ToText (fromIntegral n)


word64ToText :: Word64 -> Text
word64ToText 0 = "0"
word64ToText n = go T.empty n
  where
    go !acc 0 = acc
    go !acc v =
      let (!q, !r) = v `quotRem` 10
      in go (T.cons (toEnum (fromIntegral r + 48)) acc) q
