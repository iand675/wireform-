{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

{- | Proto3 canonical JSON encoding and decoding helpers.

Provides helpers for proto-canonical JSON representations:

* 64-bit integers encoded as JSON strings (JavaScript precision limits)
* bytes as base64 strings
* float\/double with Infinity\/NaN as string sentinels
* enum values as their proto name strings
-}
module Proto.Internal.JSON (
  -- * Object construction helpers for generated code
  jsonObject,
  jsonField,
  protoObject,
  (.=:),
  bytesFieldToJSON,
  parseField,
  parseFieldMaybe,
  parseFieldMaybeWith,
  parsePresentFieldMaybe,
  parseFieldWithAlias,
  repeatedVectorToJSON,
  parseRepeatedFieldMaybeWith,
  parseBytesFieldMaybe,
  OneofVariant (..),
  parseOneofVariants,

  -- * Proto-specific scalar JSON encoding
  protoInt64ToJSON,
  protoWord64ToJSON,
  protoFloatToJSON,
  protoDoubleToJSON,
  protoBytesToJSON,
  protoBytesFromJSON,
  protoInt32FromJSON,
  protoEnumNumberFromJSON,
  unknownEnumFailPrefix,
  protoWord32FromJSON,
  protoInt64FromJSON,
  protoWord64FromJSON,
  protoDoubleFromJSON,
  protoFloatFromJSON,
  protoNullValueFromJSON,

  -- * Well-known temporal JSON helpers
  formatRfc3339,
  parseTimestampRfc3339,
  formatDurationSeconds,
  parseDurationSeconds,

  -- * FieldMask JSON helpers
  fieldMaskPathsToJSON,
  fieldMaskPathsFromJSON,

  -- * Representation-aware bytes field helpers

  -- | These handle all 'BytesRep' variants (strict, lazy, short).
  lazyBytesFieldToJSON,
  parseLazyBytesFieldMaybe,
  shortBytesFieldToJSON,
  parseShortBytesFieldMaybe,
  protoLazyBytesToJSON,
  protoLazyBytesFromJSON,
  protoShortBytesToJSON,
  protoShortBytesFromJSON,

  -- * Representation-aware string field helpers

  {- | These handle all 'StringRep' variants (strict text, lazy text,
  short bytestring, String).
  -}
  lazyTextFieldToJSON,
  parseLazyTextFieldMaybe,
  shortTextFieldToJSON,
  parseShortTextFieldMaybe,
  hsStringFieldToJSON,
  parseHsStringFieldMaybe,

  -- * Map representation helpers
  ordMapToJSON,
  hashMapToJSON,
  parseOrdMapFromJSON,
  parseHashMapFromJSON,

  -- * Bytes map helpers (maps with ByteString values)
  bytesMapFieldToJSON,
  parseBytesMapFieldMaybe,
  lazyBytesMapFieldToJSON,
  parseLazyBytesMapFieldMaybe,
  shortBytesMapFieldToJSON,
  parseShortBytesMapFieldMaybe,
  mapValuesToJSON,
  parseMapFieldMaybeWith,

  -- * Decoding raw ProtoJSON
  decodeProtoJSON,
  checkJSONNumberExponents,

  -- * Opt-in handling of unknown enum names
  parseJSONIgnoringUnknownEnumNames,
  EnumJSONField (..),
  normalizeEnumJSONFields,
  normalizeEnumJSONArray,
  normalizeEnumJSONMap,
  normalizeEnumJSONValue,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKM
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Base64.URL qualified as Base64URL
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as SBS
import Data.Coerce (coerce)
import Data.Foldable (foldl')
import Data.HashMap.Strict qualified as HM
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, toLower, toUpper)
import Data.Hashable (Hashable)
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific, fromFloatDigits, toBoundedInteger)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Read qualified as TR
import Data.Time.Calendar (gregorianMonthLength)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Word (Word32, Word64)
import Proto.Internal.JSON.Number qualified as Number
import Proto.Internal.JSON.Number (checkJSONNumberExponents)
import Proto.Schema (ProtoEnum (fromProtoEnumValue, protoEnumValues), ProtoMessage (protoJSONIgnoreUnknownEnumNames))


-- | Decode raw JSON after checking that decimal exponents fit the JSON lexer.
decodeProtoJSON :: Aeson.FromJSON a => ByteString -> Either String a
decodeProtoJSON bytes = checkJSONNumberExponents bytes >> Aeson.eitherDecodeStrict' bytes


-- | Parse with unrecognized enum names treated as absent. Other invalid enum
-- values, including invalid numbers and null collection elements, still fail.
parseJSONIgnoringUnknownEnumNames :: forall a. (ProtoMessage a, Aeson.FromJSON a) => Aeson.Value -> Aeson.Parser a
parseJSONIgnoringUnknownEnumNames =
  Aeson.parseJSON . protoJSONIgnoreUnknownEnumNames (Proxy @a)


data EnumJSONField = EnumJSONField !Text !Text !(Aeson.Value -> Maybe Aeson.Value)


normalizeEnumJSONFields :: [EnumJSONField] -> Aeson.Value -> Aeson.Value
normalizeEnumJSONFields fields (Aeson.Object obj) = Aeson.Object (foldl' normalize obj fields)
  where
    normalize current (EnumJSONField jsonName protoName clean)
      -- Keep both spellings for the ordinary parser to report the duplicate.
      | jsonKey /= protoKey && AesonKM.member jsonKey current && AesonKM.member protoKey current = current
      | Just value <- AesonKM.lookup jsonKey current = update jsonKey value
      | Just value <- AesonKM.lookup protoKey current = update protoKey value
      | otherwise = current
      where
        jsonKey = AesonKey.fromText jsonName
        protoKey = AesonKey.fromText protoName
        update key value = case clean value of
          Nothing -> AesonKM.delete key current
          Just result -> AesonKM.insert key result current
normalizeEnumJSONFields _ value = value


normalizeEnumJSONArray :: (Aeson.Value -> Maybe Aeson.Value) -> Aeson.Value -> Maybe Aeson.Value
normalizeEnumJSONArray clean (Aeson.Array items) = Just (Aeson.Array (V.mapMaybe clean items))
normalizeEnumJSONArray _ value = Just value


normalizeEnumJSONMap :: (Aeson.Value -> Maybe Aeson.Value) -> Aeson.Value -> Maybe Aeson.Value
normalizeEnumJSONMap clean (Aeson.Object items) = Just (Aeson.Object (AesonKM.mapMaybe clean items))
normalizeEnumJSONMap _ value = Just value


normalizeEnumJSONValue :: ProtoEnum a => Proxy a -> Aeson.Value -> Maybe Aeson.Value
normalizeEnumJSONValue p =
  let known = Set.fromList (map fst (protoEnumValues p))
  in \value -> case value of
       Aeson.String name
         | Set.notMember name known, Nothing <- quotedEnumNumber name -> Nothing
       _ -> Just value


-- | Build a JSON object from key-value pairs (for generated code).
jsonObject :: [(Text, Aeson.Value)] -> Aeson.Value
jsonObject = Aeson.Object . AesonKM.fromList . fmap (first AesonKey.fromText)


-- | Create a JSON field pair.
jsonField :: Text -> Aeson.Value -> (Text, Aeson.Value)
jsonField = (,)


-- | Build a proto JSON object with field-skipping for default values.
protoObject :: [(Text, Aeson.Value)] -> Aeson.Value
protoObject = jsonObject


-- | Build a JSON field pair using 'ToJSON'.
(.=:) :: Aeson.ToJSON a => Text -> a -> (Text, Aeson.Value)
key .=: val = (key, Aeson.toJSON val)


-- | Parse a required field from a JSON object (for generated FromJSON).
parseField :: Aeson.FromJSON a => Aeson.Object -> Text -> Aeson.Parser a
parseField obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing -> fail ("Missing field: " <> T.unpack key)
  Just v -> Aeson.parseJSON v


-- | Parse an optional field from a JSON object (for generated FromJSON).
parseFieldMaybe :: Aeson.FromJSON a => Aeson.Object -> Text -> Aeson.Parser (Maybe a)
parseFieldMaybe = parseFieldMaybeWith Aeson.parseJSON


-- | Parse a non-null optional field with a ProtoJSON scalar parser.
parseFieldMaybeWith :: (Aeson.Value -> Aeson.Parser a) -> Aeson.Object -> Text -> Aeson.Parser (Maybe a)
parseFieldMaybeWith parse obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just v -> Just <$> parse v


-- | Encode a boxed or unboxed vector with the supplied ProtoJSON codec.
repeatedVectorToJSON :: VG.Vector v a => (a -> Aeson.Value) -> v a -> Aeson.Value
repeatedVectorToJSON encode values =
  Aeson.Array (V.generate (VG.length values) (encode . VG.unsafeIndex values))
{-# INLINE repeatedVectorToJSON #-}


-- | Parse each member with its scalar codec. The enclosing field may be null.
parseRepeatedFieldMaybeWith
  :: VG.Vector v a
  => (Aeson.Value -> Aeson.Parser a)
  -> (v a -> b)
  -> Aeson.Object
  -> Text
  -> Aeson.Parser (Maybe b)
parseRepeatedFieldMaybeWith parse collect =
  parseFieldMaybeWith $ Aeson.withArray "repeated protobuf field" $ \values ->
    collect <$> VG.generateM (V.length values) (\i -> parse (V.unsafeIndex values i) Aeson.<?> Aeson.Index i)
{-# INLINE parseRepeatedFieldMaybeWith #-}


-- | Parse a field whose JSON representation includes an explicit null.
parsePresentFieldMaybe :: Aeson.FromJSON a => Aeson.Object -> Text -> Aeson.Parser (Maybe a)
parsePresentFieldMaybe obj key = traverse Aeson.parseJSON (AesonKM.lookup (AesonKey.fromText key) obj)


-- | Run a field parser with whichever accepted spelling is present.
parseFieldWithAlias
  :: (Aeson.Object -> Text -> Aeson.Parser (Maybe a))
  -> Aeson.Object
  -> Text
  -> Text
  -> Aeson.Parser (Maybe a)
parseFieldWithAlias parse obj jsonKey protoKey =
  withFieldAlias obj jsonKey protoKey (pure Nothing) (\key _ -> parse obj key)


-- KeyMap has no source order, so simultaneous spellings are ambiguous
-- even when one spelling carries a null that would normally be ignored.
withFieldAlias
  :: Aeson.Object
  -> Text
  -> Text
  -> Aeson.Parser a
  -> (Text -> Aeson.Value -> Aeson.Parser a)
  -> Aeson.Parser a
withFieldAlias obj jsonKey protoKey missing present =
  case lookupKey jsonKey of
    Nothing
      | jsonKey == protoKey -> missing
      | otherwise -> maybe missing (present protoKey) (lookupKey protoKey)
    Just value
      | jsonKey /= protoKey && AesonKM.member (AesonKey.fromText protoKey) obj ->
          fail ("Multiple spellings of field " <> show protoKey <> " (" <> show jsonKey <> ")")
      | otherwise -> present jsonKey value
  where
    lookupKey key = AesonKM.lookup (AesonKey.fromText key) obj
{-# INLINE withFieldAlias #-}


-- | A variant's accepted keys and the parser for its selected payload.
data OneofVariant a = OneofVariant
  { oneofJsonKey :: !Text
  , oneofProtoKey :: !Text
  , oneofNullSelects :: !Bool
  , oneofParseValue :: Aeson.Value -> Aeson.Parser a
  }


-- | Parse a flattened oneof. An Aeson object does not retain the original
-- order, so reject conflicting selections instead of guessing which was last.
parseOneofVariants :: Aeson.Object -> [OneofVariant a] -> Aeson.Parser (Maybe a)
parseOneofVariants obj variants = do
  fields <- traverse present variants
  case catMaybes fields of
    [] -> pure Nothing
    [(_, parse)] -> Just <$> parse
    conflicts -> fail ("Multiple oneof variants set: " <> show (map fst conflicts))
  where
    present variant =
      withFieldAlias obj (oneofJsonKey variant) (oneofProtoKey variant) (pure Nothing) $
        \key value -> case value of
          Aeson.Null | not (oneofNullSelects variant) -> pure Nothing
          _ -> pure (Just (key, oneofParseValue variant value))


-- | Parse the null sentinel or a normal enum representation of NullValue.
protoNullValueFromJSON :: Enum a => Aeson.Value -> Aeson.Parser a
protoNullValueFromJSON = \case
  Aeson.Null -> pure (toEnum 0)
  Aeson.String "NULL_VALUE" -> pure (toEnum 0)
  v -> toEnum . fromIntegral <$> protoInt32FromJSON v


-- | Encode a bytes field as a base64 JSON string field pair.
bytesFieldToJSON :: Text -> ByteString -> (Text, Aeson.Value)
bytesFieldToJSON key bs = (key, protoBytesToJSON bs)


-- | Parse an optional bytes field from base64.
parseBytesFieldMaybe :: Aeson.Object -> Text -> Aeson.Parser (Maybe ByteString)
parseBytesFieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just v -> Just <$> protoBytesFromJSON v


-- Proto3 canonical JSON: 64-bit integers are encoded as strings.

-- | Parse a signed 32-bit integer from a JSON string or number.
protoInt32FromJSON :: Aeson.Value -> Aeson.Parser Int32
protoInt32FromJSON = integralFromJSON "int32"


-- | Parse the numeric form of a schema-backed enum.
protoEnumNumberFromJSON :: ProtoEnum a => Aeson.Value -> Aeson.Parser a
protoEnumNumberFromJSON value = do
  number <- case value of
    Aeson.String name -> maybe (fail "Unknown enum name") pure (quotedEnumNumber name)
    _ -> protoInt32FromJSON value
  maybe (fail (unknownEnumFailPrefix <> show number)) pure (fromProtoEnumValue (fromIntegral number))


quotedEnumNumber :: Text -> Maybe Int32
quotedEnumNumber input =
  let stripped = T.strip input
      (negative, digits) = case T.uncons stripped of
        Just ('-', rest) -> (True, rest)
        Just ('+', rest) -> (False, rest)
        _ -> (False, stripped)
      significant = T.dropWhile (== '0') digits
  in if T.null digits || T.length significant > 10
       then Nothing
       else if T.null significant
         then Just 0
         else case TR.decimal significant of
           Right (magnitude, rest) | T.null rest ->
             let signed = if negative then negate magnitude else magnitude
             in toBoundedInteger (fromInteger signed)
           _ -> Nothing


unknownEnumFailPrefix :: String
unknownEnumFailPrefix = "wireform-unknown-enum-value:"


-- | Parse an unsigned 32-bit integer from a JSON string or number.
protoWord32FromJSON :: Aeson.Value -> Aeson.Parser Word32
protoWord32FromJSON = integralFromJSON "uint32"

-- | Encode an 'Int64' as a JSON string (proto3 canonical 64-bit encoding).
protoInt64ToJSON :: Int64 -> Aeson.Value
protoInt64ToJSON n = Aeson.String (int64ToText n)


-- | Encode a 'Word64' as a JSON string (proto3 canonical 64-bit encoding).
protoWord64ToJSON :: Word64 -> Aeson.Value
protoWord64ToJSON n = Aeson.String (word64ToText n)


{- | Parse an 'Int64' from a JSON string or number (proto3 canonical).

Proto3 spec, "JSON Mapping": int64 / uint64 are encoded as
decimal strings on output, but accepted as either string or
number on input. The conformance suite verifies range +
integrality, so we route both shapes through 'boundedFromSci'
which rejects fractional and out-of-range values.
-}
protoInt64FromJSON :: Aeson.Value -> Aeson.Parser Int64
protoInt64FromJSON = integralFromJSON "int64"


-- | Parse a 'Word64' from a JSON string or number (proto3 canonical).
protoWord64FromJSON :: Aeson.Value -> Aeson.Parser Word64
protoWord64FromJSON = integralFromJSON "uint64"


integralFromJSON :: (Integral i, Bounded i) => String -> Aeson.Value -> Aeson.Parser i
integralFromJSON ty = \case
  Aeson.String s -> Number.numericString s >>= boundedFromSci ty
  Aeson.Number n -> boundedFromSci ty n
  _ -> fail ("Expected " <> ty <> " string or number")


{- | Coerce a 'Scientific' to a bounded integral type, failing
both when the value falls outside the type's range and when
it has a fractional part. This is what the conformance
@Int*Field{TooLarge,TooSmall,NotInteger}@ tests assert on.
-}
boundedFromSci
  :: forall i
   . (Integral i, Bounded i)
  => String
  -> Scientific
  -> Aeson.Parser i
boundedFromSci ty s = case toBoundedInteger s of
  Just n -> pure n
  Nothing -> fail (ty <> " value out of range or non-integer: " <> show s)


-- Proto3 canonical JSON: floats with NaN/Infinity as strings.

-- | Encode a 'Double' as JSON, using string sentinels for NaN and Infinity.
protoDoubleToJSON :: Double -> Aeson.Value
protoDoubleToJSON = floatingToJSON


-- | Encode a 'Float' as JSON, using string sentinels for NaN and Infinity.
protoFloatToJSON :: Float -> Aeson.Value
protoFloatToJSON = floatingToJSON


floatingToJSON :: RealFloat a => a -> Aeson.Value
floatingToJSON value
  | isNaN value = Aeson.String "NaN"
  | isInfinite value = Aeson.String (if value > 0 then "Infinity" else "-Infinity")
  | otherwise = Aeson.Number (fromFloatDigits value)


-- | Parse a 'Double' from JSON, accepting NaN\/Infinity string sentinels.
protoDoubleFromJSON :: Aeson.Value -> Aeson.Parser Double
protoDoubleFromJSON = Number.floating


-- | Parse a 'Float' with ProtoJSON sentinel, exponent and overflow handling.
protoFloatFromJSON :: Aeson.Value -> Aeson.Parser Float
protoFloatFromJSON = Number.floating


-- | Render FieldMask paths as a comma-separated lowerCamelCase string.
fieldMaskPathsToJSON :: V.Vector Text -> Aeson.Value
fieldMaskPathsToJSON paths
  | paths == V.singleton "" = error "A single empty FieldMask path cannot round-trip"
  | otherwise =
      case traverse toCamel (V.toList paths) of
        Left e -> error e
        Right rendered -> Aeson.String (T.intercalate "," rendered)
  where
    toCamel path =
      let (firstSegment, rest) = T.breakOn "_" path
          capitalize segment = case T.uncons segment of
            Just (c, tailText) | isAsciiLower c -> Right (T.cons (toUpper c) tailText)
            _ -> Left ("'_' must precede lowercase ASCII in FieldMask path: " <> show path)
      in if not (T.all (\c -> fieldMaskPlainChar c || c == '_') path)
           then Left ("Invalid character in FieldMask path: " <> show path)
           else if T.null rest
             then Right firstSegment
             else (firstSegment <>) . T.concat <$> traverse capitalize (T.splitOn "_" (T.drop 1 rest))


-- | Parse a comma-separated FieldMask string into lower_snake_case paths.
fieldMaskPathsFromJSON :: Aeson.Value -> Aeson.Parser (V.Vector Text)
fieldMaskPathsFromJSON = Aeson.withText "FieldMask" $ \text ->
  if T.null text
    then pure V.empty
    else V.fromList <$> traverse toSnake (T.splitOn "," text)
  where
    toSnake path
      | T.all (\c -> fieldMaskPlainChar c || isAsciiUpper c) path =
          pure (T.concatMap lower path)
      | otherwise = fail ("Invalid character in JSON FieldMask path: " <> show path)
    lower c
      | isAsciiUpper c = T.cons '_' (T.singleton (toLower c))
      | otherwise = T.singleton c


fieldMaskPlainChar :: Char -> Bool
fieldMaskPlainChar c = isAsciiLower c || ('0' <= c && c <= '9') || c == '.'


-- Proto3 canonical JSON: bytes as base64.

-- | Encode a strict 'ByteString' as a base64 JSON string.
protoBytesToJSON :: ByteString -> Aeson.Value
protoBytesToJSON bs = Aeson.String (TE.decodeUtf8 (Base64.encode bs))


-- | Parse a strict 'ByteString' from a base64 or base64url JSON string.
protoBytesFromJSON :: Aeson.Value -> Aeson.Parser ByteString
protoBytesFromJSON (Aeson.String s) =
  case Base64.decode bytes of
    Right decoded -> pure decoded
    Left err
      | validPadding && BS.all isStandard body -> pure (Base64.decodeLenient body)
      | validPadding && BS.all isUrlSafe body -> pure (Base64URL.decodeLenient body)
      | otherwise -> fail ("Invalid base64 bytes: " <> err)
  where
    bytes = TE.encodeUtf8 s
    (body, padding) = BS.break (== 0x3D) bytes
    remainder = BS.length body `rem` 4
    expectedPadding = (4 - remainder) `rem` 4
    -- Both alphabets allow omitted padding. Validate its shape and the
    -- alphabet before using decoders that ignore nonzero unused tail bits,
    -- which are exercised by the upstream BytesFieldBase64Url case.
    validPadding =
      remainder /= 1
        && (BS.null padding || (BS.length padding == expectedPadding && BS.all (== 0x3D) padding))
    isStandard c = isAlphaNumeric c || c == 0x2B || c == 0x2F -- '+' '/'
    isUrlSafe c = isAlphaNumeric c || c == 0x2D || c == 0x5F -- '-' '_'
    isAlphaNumeric c =
      (c >= 0x41 && c <= 0x5A) -- A-Z
        || (c >= 0x61 && c <= 0x7A) -- a-z
        || (c >= 0x30 && c <= 0x39) -- 0-9
protoBytesFromJSON _ = fail "Expected base64 string for bytes"


-- ---------------------------------------------------------------------------
-- Lazy ByteString (base64)
-- ---------------------------------------------------------------------------

-- | Encode a lazy 'BL.ByteString' as a base64 JSON string.
protoLazyBytesToJSON :: BL.ByteString -> Aeson.Value
protoLazyBytesToJSON = protoBytesToJSON . BL.toStrict


-- | Parse a lazy 'BL.ByteString' from a base64 JSON string.
protoLazyBytesFromJSON :: Aeson.Value -> Aeson.Parser BL.ByteString
protoLazyBytesFromJSON v = BL.fromStrict <$> protoBytesFromJSON v


-- | Encode a lazy bytes field as a base64 JSON string field pair.
lazyBytesFieldToJSON :: Text -> BL.ByteString -> (Text, Aeson.Value)
lazyBytesFieldToJSON key lbs = (key, protoLazyBytesToJSON lbs)


-- | Parse an optional lazy bytes field from base64.
parseLazyBytesFieldMaybe :: Aeson.Object -> Text -> Aeson.Parser (Maybe BL.ByteString)
parseLazyBytesFieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just v -> Just <$> protoLazyBytesFromJSON v


-- ---------------------------------------------------------------------------
-- ShortByteString (base64)
-- ---------------------------------------------------------------------------

-- | Encode a 'SBS.ShortByteString' as a base64 JSON string.
protoShortBytesToJSON :: SBS.ShortByteString -> Aeson.Value
protoShortBytesToJSON = protoBytesToJSON . SBS.fromShort


-- | Parse a 'SBS.ShortByteString' from a base64 JSON string.
protoShortBytesFromJSON :: Aeson.Value -> Aeson.Parser SBS.ShortByteString
protoShortBytesFromJSON v = SBS.toShort <$> protoBytesFromJSON v


-- | Encode a short bytes field as a base64 JSON string field pair.
shortBytesFieldToJSON :: Text -> SBS.ShortByteString -> (Text, Aeson.Value)
shortBytesFieldToJSON key sbs = (key, protoShortBytesToJSON sbs)


-- | Parse an optional short bytes field from base64.
parseShortBytesFieldMaybe :: Aeson.Object -> Text -> Aeson.Parser (Maybe SBS.ShortByteString)
parseShortBytesFieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just v -> Just <$> protoShortBytesFromJSON v


-- ---------------------------------------------------------------------------
-- Lazy Text (JSON string)
-- ---------------------------------------------------------------------------

-- | Encode a lazy 'TL.Text' field as a JSON string field pair.
lazyTextFieldToJSON :: Text -> TL.Text -> (Text, Aeson.Value)
lazyTextFieldToJSON key lt = (key, Aeson.String (TL.toStrict lt))


-- | Parse an optional lazy 'TL.Text' field from a JSON string.
parseLazyTextFieldMaybe :: Aeson.Object -> Text -> Aeson.Parser (Maybe TL.Text)
parseLazyTextFieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just (Aeson.String s) -> pure (Just (TL.fromStrict s))
  Just _ -> fail ("Expected string for field: " <> T.unpack key)


-- ---------------------------------------------------------------------------
-- ShortByteString as text (UTF-8 stored in SBS)
-- ---------------------------------------------------------------------------

-- | Encode a UTF-8 'SBS.ShortByteString' text field as a JSON string field pair.
shortTextFieldToJSON :: Text -> SBS.ShortByteString -> (Text, Aeson.Value)
shortTextFieldToJSON key sbs = case TE.decodeUtf8' (SBS.fromShort sbs) of
  Right t -> (key, Aeson.String t)
  Left _ -> (key, Aeson.String "")


-- | Parse an optional UTF-8 'SBS.ShortByteString' text field from a JSON string.
parseShortTextFieldMaybe :: Aeson.Object -> Text -> Aeson.Parser (Maybe SBS.ShortByteString)
parseShortTextFieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just (Aeson.String s) -> pure (Just (SBS.toShort (TE.encodeUtf8 s)))
  Just _ -> fail ("Expected string for field: " <> T.unpack key)


-- ---------------------------------------------------------------------------
-- Haskell String (JSON string)
-- ---------------------------------------------------------------------------

-- | Encode a Haskell 'String' field as a JSON string field pair.
hsStringFieldToJSON :: Text -> String -> (Text, Aeson.Value)
hsStringFieldToJSON key s = (key, Aeson.String (T.pack s))


-- | Parse an optional Haskell 'String' field from a JSON string.
parseHsStringFieldMaybe :: Aeson.Object -> Text -> Aeson.Parser (Maybe String)
parseHsStringFieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just (Aeson.String s) -> pure (Just (T.unpack s))
  Just _ -> fail ("Expected string for field: " <> T.unpack key)


-- ---------------------------------------------------------------------------
-- Map representation helpers
-- ---------------------------------------------------------------------------

{- | Convert an ordered Map to a JSON object. Keys are converted to text
via their 'ToJSON' instance (proto map keys are always scalar types).
-}
ordMapToJSON :: (Aeson.ToJSON k, Aeson.ToJSON v) => Map k v -> Aeson.Value
ordMapToJSON m =
  Aeson.Object
    ( AesonKM.fromList
        (fmap (\(k, v) -> (AesonKey.fromText (keyToText k), Aeson.toJSON v)) (Map.toList m))
    )


-- | Convert a HashMap to a JSON object.
hashMapToJSON :: (Aeson.ToJSON k, Aeson.ToJSON v) => HM.HashMap k v -> Aeson.Value
hashMapToJSON m =
  Aeson.Object
    ( AesonKM.fromList
        (fmap (\(k, v) -> (AesonKey.fromText (keyToText k), Aeson.toJSON v)) (HM.toList m))
    )


keyToText :: Aeson.ToJSON k => k -> Text
keyToText k = case Aeson.toJSON k of
  Aeson.String s -> s
  Aeson.Number n -> T.pack (show n)
  Aeson.Bool b -> if b then "true" else "false"
  other -> T.pack (show other)


-- | Parse a JSON object into an ordered Map.
parseOrdMapFromJSON :: (Ord k, Aeson.FromJSON k, Aeson.FromJSON v) => Aeson.Value -> Aeson.Parser (Map k v)
parseOrdMapFromJSON (Aeson.Object o) =
  Map.fromList <$> traverse parseEntry (AesonKM.toList o)
  where
    parseEntry (k, v) = do
      key <- Aeson.parseJSON (Aeson.String (AesonKey.toText k))
      val <- Aeson.parseJSON v
      pure (key, val)
parseOrdMapFromJSON _ = fail "Expected JSON object for map field"


-- | Parse a JSON object into a HashMap.
parseHashMapFromJSON :: (Eq k, Hashable k, Aeson.FromJSON k, Aeson.FromJSON v) => Aeson.Value -> Aeson.Parser (HM.HashMap k v)
parseHashMapFromJSON (Aeson.Object o) =
  HM.fromList <$> traverse parseEntry (AesonKM.toList o)
  where
    parseEntry (k, v) = do
      key <- Aeson.parseJSON (Aeson.String (AesonKey.toText k))
      val <- Aeson.parseJSON v
      pure (key, val)
parseHashMapFromJSON _ = fail "Expected JSON object for map field"


-- ---------------------------------------------------------------------------
-- Bytes map helpers (Map k ByteString, common in proto APIs)
-- ---------------------------------------------------------------------------

-- | Encode the values of a map while using JSON object keys for its key type.
mapValuesToJSON :: Aeson.ToJSONKey k => (v -> Aeson.Value) -> Map k v -> Aeson.Value
mapValuesToJSON encode values = case Aeson.toJSONKey of
  Aeson.ToJSONKeyText key _ ->
    Aeson.Object (AesonKM.fromList (Map.foldrWithKey (\k v rest -> (key k, encode v) : rest) [] values))
  Aeson.ToJSONKeyValue _ _ -> Aeson.toJSON (Map.map encode values)


-- | Parse typed object keys and apply the supplied decoder to each value.
parseMapFieldMaybeWith
  :: forall k v. (Ord k, Aeson.FromJSONKey k)
  => (Aeson.Value -> Aeson.Parser v)
  -> Aeson.Object
  -> Text
  -> Aeson.Parser (Maybe (Map k v))
parseMapFieldMaybeWith parse =
  parseFieldMaybeWith (Aeson.withObject "protobuf map" parseObject)
  where
    parseObject = case Aeson.fromJSONKey of
      Aeson.FromJSONKeyCoerce -> parseEntries (pure . coerce)
      Aeson.FromJSONKeyText key -> parseEntries (pure . key)
      Aeson.FromJSONKeyTextParser key -> parseEntries key
      Aeson.FromJSONKeyValue _ -> \obj -> Aeson.parseJSON (Aeson.Object obj) >>= traverse parse
    parseEntries :: (Text -> Aeson.Parser k) -> Aeson.Object -> Aeson.Parser (Map k v)
    parseEntries key obj = Map.fromList <$> traverse parseEntry (AesonKM.toList obj)
      where
        parseEntry (k, v) = (,) <$> key (AesonKey.toText k) <*> parse v


-- | Encode a map field with base64 values.
bytesMapFieldToJSON :: Aeson.ToJSONKey k => Text -> Map k ByteString -> (Text, Aeson.Value)
bytesMapFieldToJSON key = jsonField key . mapValuesToJSON protoBytesToJSON


-- | Parse an optional map field from a JSON object with base64 values.
parseBytesMapFieldMaybe :: (Ord k, Aeson.FromJSONKey k) => Aeson.Object -> Text -> Aeson.Parser (Maybe (Map k ByteString))
parseBytesMapFieldMaybe = parseMapFieldMaybeWith protoBytesFromJSON


-- | Encode a lazy bytes map with base64 values.
lazyBytesMapFieldToJSON :: Aeson.ToJSONKey k => Text -> Map k BL.ByteString -> (Text, Aeson.Value)
lazyBytesMapFieldToJSON key = jsonField key . mapValuesToJSON protoLazyBytesToJSON


-- | Parse an optional lazy bytes map from a JSON object with base64 values.
parseLazyBytesMapFieldMaybe
  :: (Ord k, Aeson.FromJSONKey k) => Aeson.Object -> Text -> Aeson.Parser (Maybe (Map k BL.ByteString))
parseLazyBytesMapFieldMaybe = parseMapFieldMaybeWith protoLazyBytesFromJSON


-- | Encode a short bytes map with base64 values.
shortBytesMapFieldToJSON :: Aeson.ToJSONKey k => Text -> Map k SBS.ShortByteString -> (Text, Aeson.Value)
shortBytesMapFieldToJSON key = jsonField key . mapValuesToJSON protoShortBytesToJSON


-- | Parse an optional short bytes map from a JSON object with base64 values.
parseShortBytesMapFieldMaybe
  :: (Ord k, Aeson.FromJSONKey k) => Aeson.Object -> Text -> Aeson.Parser (Maybe (Map k SBS.ShortByteString))
parseShortBytesMapFieldMaybe = parseMapFieldMaybeWith protoShortBytesFromJSON


-- ---------------------------------------------------------------------------
-- Internal numeric helpers
-- ---------------------------------------------------------------------------

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


-- | Format seconds and nanos as an RFC 3339 timestamp string.
formatRfc3339 :: Int64 -> Int32 -> Text
formatRfc3339 secs nanos
  | secs < timestampMinSecs || secs > timestampMaxSecs = error ("Timestamp out of range: " <> show secs)
  | nanos < 0 || nanos > 999999999 = error ("Timestamp nanos out of range: " <> show nanos)
  | otherwise =
      let !civil = unixToCivil secs
          dateStr = padInt 4 (cvYear civil) <> "-" <> padInt 2 (cvMonth civil) <> "-" <> padInt 2 (cvDay civil)
          timeStr = padInt 2 (cvHour civil) <> ":" <> padInt 2 (cvMinute civil) <> ":" <> padInt 2 (cvSecond civil)
      in dateStr <> "T" <> timeStr <> canonicalNanoSuffix nanos <> "Z"


{- | Per the proto3 JSON spec, Timestamp/Duration nanos are
formatted with EXACTLY 0, 3, 6, or 9 fractional digits. A
value with fewer significant digits is padded out to the
next bucket; zero suppresses the entire @\".nnn\"@ suffix.

This is the rule the conformance suite's Validator tests
(Timestamp/Duration {Has3, Has6, Has9}FractionalDigits and
TimestampZeroNormalized) check.
-}
canonicalNanoSuffix :: Int32 -> Text
canonicalNanoSuffix n
  | n == 0 = T.empty
  | otherwise =
      let !digits = padInt 9 (fromIntegral (abs n))
          -- Trim trailing zeros, then round /up/ the kept count
          -- to the next multiple of 3 (3, 6, or 9).
          !trimmed = T.dropWhileEnd (== '0') digits
          !keep = case T.length trimmed of
            k
              | k <= 3 -> 3
              | k <= 6 -> 6
              | otherwise -> 9
      in T.cons '.' (T.take keep digits)


data CivilTime = CivilTime
  { cvYear :: {-# UNPACK #-} !Int
  , cvMonth :: {-# UNPACK #-} !Int
  , cvDay :: {-# UNPACK #-} !Int
  , cvHour :: {-# UNPACK #-} !Int
  , cvMinute :: {-# UNPACK #-} !Int
  , cvSecond :: {-# UNPACK #-} !Int
  }


unixToCivil :: Int64 -> CivilTime
unixToCivil totalSecs =
  let !s' = fromIntegral totalSecs :: Int
      (!days, !dayRem) = s' `divMod` 86400
      (!h, !hmRem) = dayRem `quotRem` 3600
      (!mi, !sec) = hmRem `quotRem` 60
      !date = civilFromDays (days + 719468)
  in CivilTime (cdYear date) (cdMonth date) (cdDay date) h mi sec


data CivilDate = CivilDate
  { cdYear :: {-# UNPACK #-} !Int
  , cdMonth :: {-# UNPACK #-} !Int
  , cdDay :: {-# UNPACK #-} !Int
  }


civilFromDays :: Int -> CivilDate
civilFromDays z =
  let !era = (if z >= 0 then z else z - 146096) `quot` 146097
      !doe = z - era * 146097
      !yoe = (doe - doe `quot` 1460 + doe `quot` 36524 - doe `quot` 146096) `quot` 365
      !y = yoe + era * 400
      !doy = doe - (365 * yoe + yoe `quot` 4 - yoe `quot` 100)
      !mp = (5 * doy + 2) `quot` 153
      !d = doy - (153 * mp + 2) `quot` 5 + 1
      !m = mp + (if mp < 10 then 3 else -9)
      !y' = y + (if m <= 2 then 1 else 0)
  in CivilDate y' m d


daysFromCivil :: Int -> Int -> Int -> Int
daysFromCivil y m d =
  let !y' = y - (if m <= 2 then 1 else 0)
      !era = (if y' >= 0 then y' else y' - 399) `quot` 400
      !yoe = y' - era * 400
      !doy = (153 * (m + (if m > 2 then -3 else 9)) + 2) `quot` 5 + d - 1
      !doe = yoe * 365 + yoe `quot` 4 - yoe `quot` 100 + doy
  in era * 146097 + doe


padInt :: Int -> Int -> Text
padInt width n =
  let !raw = intToText n
      !pad = width - T.length raw
  in if pad <= 0 then raw else T.replicate pad "0" <> raw


intToText :: Int -> Text
intToText n
  | n < 0 = "-" <> intToText (negate n)
  | n < 10 = T.singleton (digit n)
  | otherwise = go T.empty n
  where
    go !acc 0 = acc
    go !acc v =
      let (!q, !r) = v `quotRem` 10
      in go (T.cons (digit r) acc) q
    digit i = toEnum (i + 48)


{- | Proto3 spec range for Timestamp: @[0001-01-01T00:00:00Z,
9999-12-31T23:59:59.999999999Z]@. Outside that range
timestamps are not representable in canonical JSON.
-}
timestampMinSecs, timestampMaxSecs :: Int64
timestampMinSecs = -62135596800 -- 0001-01-01T00:00:00Z
timestampMaxSecs = 253402300799 -- 9999-12-31T23:59:59Z


-- | Parse RFC 3339 into the seconds and nanos of a Protobuf timestamp.
parseTimestampRfc3339 :: (Int64 -> Int32 -> a) -> Text -> Either String a
parseTimestampRfc3339 build t = do
  let stripped = T.strip t
  case T.breakOn "T" stripped of
    (datePart, rest)
      | T.null rest -> Left "Invalid RFC 3339 timestamp: missing T separator"
      | otherwise -> do
          let timePart' = T.drop 1 rest
          (timePart, !offsetSecs) <- splitOffset timePart'
          date <- parseDate datePart
          time <- parseTime timePart
          let !days = daysFromCivil (pdYear date) (pdMonth date) (pdDay date) - 719468
              !rawSecs =
                fromIntegral days * 86400
                  + fromIntegral (ptHour time) * 3600
                  + fromIntegral (ptMinute time) * 60
                  + fromIntegral (ptSecond time)
              -- If the input came with a +HH:MM offset, normalise
              -- to UTC by subtracting the offset (so an East-of-
              -- UTC wall clock yields a smaller unix epoch).
              !totalSecs = rawSecs - offsetSecs
          if totalSecs < timestampMinSecs || totalSecs > timestampMaxSecs
            then Left "Timestamp out of range [0001-01-01, 9999-12-31]"
            else Right (build totalSecs (ptNanos time))


{- | Strip the trailing zone designator from an RFC 3339 time
and return the remaining time component plus the offset in
seconds (positive for east-of-UTC, negative for west).

The proto3 spec requires the timestamp to end with @Z@ (the
conformance suite explicitly rejects lowercase @z@ and
missing zone). Numeric offsets @+HH:MM@ and @-HH:MM@ are
accepted and normalised to UTC.
-}
splitOffset :: Text -> Either String (Text, Int64)
splitOffset t
  | T.null t = Left "Empty time component"
  | last' == 'Z' = Right (T.init t, 0)
  | last' == 'z' = Left "Lowercase 'z' zone designator not allowed"
  | otherwise =
      -- Try to peel a +HH:MM / -HH:MM suffix. We scan back from
      -- the end for the first '+' or '-' that's part of the zone
      -- (the seconds field can also have a leading '-' but we
      -- look for an offset signature: ±DD:DD).
      case parseOffsetSuffix t of
        Just (timePart, offsetSecs) -> Right (timePart, offsetSecs)
        Nothing -> Left "Timestamp must end with 'Z' or numeric offset"
  where
    last' = T.last t


{- | Recognise a trailing @+HH:MM@ / @-HH:MM@ suffix on an RFC 3339 time string.
-}
parseOffsetSuffix :: Text -> Maybe (Text, Int64)
parseOffsetSuffix t
  -- ±HH:MM (6 chars, e.g. "+08:00")
  | T.length t >= 6
  , Just (rest, suf) <- splitAtRev 6 t
  , Just (sign, hh, mm) <- parseHhMmSuffix suf =
      Just (rest, sign * (hh * 3600 + mm * 60))
  | otherwise = Nothing
  where
    splitAtRev n s
      | T.length s < n = Nothing
      | otherwise = Just (T.dropEnd n s, T.takeEnd n s)

    parseHhMmSuffix s = case T.unpack s of
      [c1, h1, h2, ':', m1, m2]
        | c1 == '+' || c1 == '-'
        , isDigit h1
        , isDigit h2
        , isDigit m1
        , isDigit m2
        , let hours = d h1 * 10 + d h2
        , let minutes = d m1 * 10 + d m2
        , hours < 24
        , minutes < 60 ->
            Just
              ( if c1 == '+' then 1 else -1
              , fromIntegral hours
              , fromIntegral minutes
              )
      _ -> Nothing

    d c = fromEnum c - fromEnum '0'


data ParsedDate = ParsedDate
  { pdYear :: {-# UNPACK #-} !Int
  , pdMonth :: {-# UNPACK #-} !Int
  , pdDay :: {-# UNPACK #-} !Int
  }


data ParsedTime = ParsedTime
  { ptHour :: {-# UNPACK #-} !Int
  , ptMinute :: {-# UNPACK #-} !Int
  , ptSecond :: {-# UNPACK #-} !Int
  , ptNanos :: {-# UNPACK #-} !Int32
  }


parseDate :: Text -> Either String ParsedDate
parseDate t = case T.splitOn "-" t of
  [ys, ms, ds] -> do
    y <- timestampDigits 4 ys
    m <- timestampDigits 2 ms
    d <- timestampDigits 2 ds
    if m >= 1 && m <= 12 && d >= 1 && d <= gregorianMonthLength (fromIntegral y) m
      then Right (ParsedDate y m d)
      else Left "Invalid calendar date"
  _ -> Left "Invalid date format"


-- | Format normalized seconds and nanos as a ProtoJSON Duration string.
formatDurationSeconds :: Int64 -> Int32 -> Text
formatDurationSeconds seconds nanos
  | seconds < negate durationSecondsLimit || seconds > durationSecondsLimit =
      error "Duration seconds out of range"
  | nanos < -999999999 || nanos > 999999999 =
      error "Duration nanos out of range"
  | (seconds < 0 && nanos > 0) || (seconds > 0 && nanos < 0) =
      error "Duration seconds and nanos have different signs"
  | otherwise =
      let sign = if seconds < 0 || nanos < 0 then "-" else ""
      in sign <> intToText (fromIntegral (abs seconds)) <> canonicalNanoSuffix nanos <> "s"


-- | Decode a ProtoJSON Duration without narrowing an unvalidated magnitude.
parseDurationSeconds :: (Int64 -> Int32 -> a) -> Text -> Either String a
parseDurationSeconds build input = do
  body <- maybe (Left "Duration must end with 's'") Right (T.stripSuffix "s" input)
  let (negative, unsigned) = case T.stripPrefix "-" body of
        Just rest -> (True, rest)
        Nothing -> (False, body)
      (secondsText, fraction) = T.breakOn "." unsigned
      significant = T.dropWhile (== '0') secondsText
  if T.null secondsText || not (T.all isDigit secondsText)
    then Left "Invalid Duration seconds"
    else if T.length significant > 12
      then Left "Duration seconds out of range"
      else do
        seconds <- if T.null significant then Right 0 else readInt significant
        nanos <- parseFracNanos fraction
        if fromIntegral seconds > durationSecondsLimit
          then Left "Duration seconds out of range"
          else Right (if negative then build (negate (fromIntegral seconds)) (negate nanos) else build (fromIntegral seconds) nanos)


durationSecondsLimit :: Int64
durationSecondsLimit = 315576000000


parseTime :: Text -> Either String ParsedTime
parseTime t =
  let (wholePart, fracPart) = T.breakOn "." t
  in case T.splitOn ":" wholePart of
       [hs, ms, ss] -> do
         h <- timestampDigits 2 hs
         m <- timestampDigits 2 ms
         s <- timestampDigits 2 ss
         nanos <- parseFracNanos fracPart
         if h < 24 && m < 60 && s < 60
           then Right (ParsedTime h m s nanos)
           else Left "Invalid clock time"
       _ -> Left "Invalid time format"


timestampDigits :: Int -> Text -> Either String Int
timestampDigits width digits
  | T.length digits == width && T.all isDigit digits = readInt digits
  | otherwise = Left "Invalid timestamp digits"


parseFracNanos :: Text -> Either String Int32
parseFracNanos fraction
  | T.null fraction = Right 0
  | Just digits <- T.stripPrefix "." fraction
  , let width = T.length digits
  , width >= 1 && width <= 9
  , T.all isDigit digits =
      fromIntegral <$> readInt (digits <> T.replicate (9 - width) "0")
  | otherwise = Left "Invalid fractional seconds"


readInt :: Text -> Either String Int
readInt t = case TR.signed TR.decimal t of
  Right (n, rest) | T.null rest -> Right n
  Right (_, rest) -> Left ("Trailing chars: " <> T.unpack rest)
  Left e -> Left e
