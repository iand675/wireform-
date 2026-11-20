{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Proto3 canonical JSON mapping for well-known types.

These functions provide the canonical conversions specified by the
proto3 JSON specification.
-}
module Proto.Internal.JSON.WellKnown (
  timestampToJSON,
  timestampFromJSON,
  durationToJSON,
  durationFromJSON,
  fieldMaskToJSON,
  fieldMaskFromJSON,
  structToJSON,
  structFromJSON,
  valueToJSON,
  valueFromJSON,
  formatRfc3339,
  parseRfc3339,

  -- * Wrapper types

  {- | Per the proto3 JSON spec, every @google.protobuf.XValue@
  wrapper serialises as just its inner value (rather than the
  generic @{"value": ...}@ shape the pre-generated @ToJSON@
  instances produce). The 'wrap*' helpers go in the encode
  direction; the 'unwrap*' helpers parse a bare JSON value
  and construct the wrapper. 64-bit integer wrappers
  additionally string-encode their inner value.
  -}
  wrapBoolValue,
  wrapInt32Value,
  wrapInt64Value,
  wrapUInt32Value,
  wrapUInt64Value,
  wrapFloatValue,
  wrapDoubleValue,
  wrapStringValue,
  wrapBytesValue,
  unwrapBoolValue,
  unwrapInt32Value,
  unwrapInt64Value,
  unwrapUInt32Value,
  unwrapUInt64Value,
  unwrapFloatValue,
  unwrapDoubleValue,
  unwrapStringValue,
  unwrapBytesValue,

  -- * Empty / NullValue
  emptyToJSON,
  emptyFromJSON,
  nullValueToJSON,
  nullValueFromJSON,

  -- * Any
  anyToJSON,
  anyFromJSON,
  anyPartsToJSON,
  anyPartsFromJSON,
  normalizeAnyEnumJSON,
  standardWktRegistry,
  anyJsonRegistry,

  -- * Re-exports from "Proto.Registry"
  AnyCodec (..),
  TypeRegistry,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as AesonKM
import Data.Aeson.Types qualified as AesonT
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Proto.Decode qualified as PD
import Proto.Encode qualified as PE
import {-# SOURCE #-} Proto.Google.Protobuf.Any qualified as Any
import Proto.Google.Protobuf.Duration
import Proto.Google.Protobuf.Empty qualified as Empty
import Proto.Google.Protobuf.FieldMask
import Proto.Google.Protobuf.Struct
import Proto.Google.Protobuf.Timestamp
import Proto.Google.Protobuf.Wrappers qualified as W
import Proto.Internal.JSON (formatDurationSeconds, formatRfc3339, parseDurationSeconds, parseTimestampRfc3339, protoBytesFromJSON, protoBytesToJSON)
import Data.IORef (readIORef)
import System.IO.Unsafe (unsafePerformIO)
import Proto.Registry (AnyCodec (..), SomeDecoder (..), TypeRegistry (trDecoders), emptyRegistry, globalAnyRegistry, lookupCodec, registerCodec)
import Proto.Schema (protoJSONIgnoreUnknownEnumNames)


{- | Encode a 'Timestamp' as canonical RFC 3339. Throws on out-
of-range input — the conformance suite requires that
serialisation fail when the wire-format value is outside
@[0001-01-01T00:00:00Z, 9999-12-31T23:59:59Z]@. The runner
catches the exception and turns it into a serialize-error
response.
-}
timestampToJSON :: Timestamp -> Aeson.Value
timestampToJSON ts = Aeson.String (formatRfc3339 (timestampSeconds ts) (timestampNanos ts))


-- | Parse a 'Timestamp' from an RFC 3339 JSON string.
timestampFromJSON :: Aeson.Value -> Either String Timestamp
timestampFromJSON (Aeson.String t) = parseRfc3339 t
timestampFromJSON _ = Left "Expected RFC 3339 string for Timestamp"


-- | Parse an RFC 3339 string into a 'Timestamp'.
parseRfc3339 :: Text -> Either String Timestamp
parseRfc3339 = parseTimestampRfc3339 $ \seconds nanos ->
  defaultTimestamp {timestampSeconds = seconds, timestampNanos = nanos}


-- | Encode a 'Duration' as a canonical JSON string.
durationToJSON :: Duration -> Aeson.Value
durationToJSON dur = Aeson.String (formatDurationSeconds (durationSeconds dur) (durationNanos dur))


-- | Parse a 'Duration' from a canonical JSON string.
durationFromJSON :: Aeson.Value -> Either String Duration
durationFromJSON (Aeson.String t) = parseDurationSeconds build t
  where
    build seconds nanos = defaultDuration {durationSeconds = seconds, durationNanos = nanos}
durationFromJSON _ = Left "Expected duration string"


-- | Encode a 'FieldMask' as a comma-separated lowerCamelCase JSON string.
fieldMaskToJSON :: FieldMask -> Aeson.Value
fieldMaskToJSON = Aeson.toJSON


-- | Parse a 'FieldMask' from a comma-separated lowerCamelCase JSON string.
fieldMaskFromJSON :: Aeson.Value -> Either String FieldMask
fieldMaskFromJSON = AesonT.parseEither Aeson.parseJSON


-- Struct/Value: native JSON

-- | Encode a 'Struct' as a native JSON object.
structToJSON :: Struct -> Aeson.Value
structToJSON = Aeson.toJSON


-- | Parse a 'Struct' from a native JSON object.
structFromJSON :: Aeson.Value -> Either String Struct
structFromJSON = AesonT.parseEither Aeson.parseJSON


-- | Encode a protobuf 'Value' as a native JSON value.
valueToJSON :: Value -> Aeson.Value
valueToJSON = Aeson.toJSON


-- | Parse a protobuf 'Value' from a native JSON value.
valueFromJSON :: Aeson.Value -> Either String Value
valueFromJSON = AesonT.parseEither Aeson.parseJSON


-- ---------------------------------------------------------------------------
-- Wrappers (proto3 spec: emit just the inner value, not @{"value": ...}@)
-- ---------------------------------------------------------------------------

-- The generated instances share scalar codecs with ordinary message fields.

-- | Encode a 'W.BoolValue' as its bare inner JSON boolean.
wrapBoolValue :: W.BoolValue -> Aeson.Value
wrapBoolValue = Aeson.toJSON


-- | Encode an 'W.Int32Value' as its bare inner JSON number.
wrapInt32Value :: W.Int32Value -> Aeson.Value
wrapInt32Value = Aeson.toJSON


-- | Encode an 'W.Int64Value' as its bare inner JSON string (64-bit canonical).
wrapInt64Value :: W.Int64Value -> Aeson.Value
wrapInt64Value = Aeson.toJSON


-- | Encode a 'W.UInt32Value' as its bare inner JSON number.
wrapUInt32Value :: W.UInt32Value -> Aeson.Value
wrapUInt32Value = Aeson.toJSON


-- | Encode a 'W.UInt64Value' as its bare inner JSON string (64-bit canonical).
wrapUInt64Value :: W.UInt64Value -> Aeson.Value
wrapUInt64Value = Aeson.toJSON


-- | Encode a 'W.FloatValue' as its bare inner JSON number (with NaN\/Infinity sentinels).
wrapFloatValue :: W.FloatValue -> Aeson.Value
wrapFloatValue = Aeson.toJSON


-- | Encode a 'W.DoubleValue' as its bare inner JSON number (with NaN\/Infinity sentinels).
wrapDoubleValue :: W.DoubleValue -> Aeson.Value
wrapDoubleValue = Aeson.toJSON


-- | Encode a 'W.StringValue' as its bare inner JSON string.
wrapStringValue :: W.StringValue -> Aeson.Value
wrapStringValue = Aeson.toJSON


-- | Encode a 'W.BytesValue' as its bare inner base64 JSON string.
wrapBytesValue :: W.BytesValue -> Aeson.Value
wrapBytesValue = Aeson.toJSON


-- Decoders parse a bare JSON value and construct the wrapper.

-- | Parse a bare JSON boolean into a 'W.BoolValue'.
unwrapBoolValue :: Aeson.Value -> Either String W.BoolValue
unwrapBoolValue = AesonT.parseEither Aeson.parseJSON


-- | Parse a bare JSON number or string into an 'W.Int32Value'.
unwrapInt32Value :: Aeson.Value -> Either String W.Int32Value
unwrapInt32Value = AesonT.parseEither Aeson.parseJSON


-- | Parse a bare JSON number or string into an 'W.Int64Value'.
unwrapInt64Value :: Aeson.Value -> Either String W.Int64Value
unwrapInt64Value = AesonT.parseEither Aeson.parseJSON


-- | Parse a bare JSON number or string into a 'W.UInt32Value'.
unwrapUInt32Value :: Aeson.Value -> Either String W.UInt32Value
unwrapUInt32Value = AesonT.parseEither Aeson.parseJSON


-- | Parse a bare JSON number or string into a 'W.UInt64Value'.
unwrapUInt64Value :: Aeson.Value -> Either String W.UInt64Value
unwrapUInt64Value = AesonT.parseEither Aeson.parseJSON


-- | Parse a bare JSON number (or NaN\/Infinity string) into a 'W.FloatValue'.
unwrapFloatValue :: Aeson.Value -> Either String W.FloatValue
unwrapFloatValue = AesonT.parseEither Aeson.parseJSON


-- | Parse a bare JSON number (or NaN\/Infinity string) into a 'W.DoubleValue'.
unwrapDoubleValue :: Aeson.Value -> Either String W.DoubleValue
unwrapDoubleValue = AesonT.parseEither Aeson.parseJSON


-- | Parse a bare JSON string into a 'W.StringValue'.
unwrapStringValue :: Aeson.Value -> Either String W.StringValue
unwrapStringValue = AesonT.parseEither Aeson.parseJSON


-- | Parse a bare base64 JSON string into a 'W.BytesValue'.
unwrapBytesValue :: Aeson.Value -> Either String W.BytesValue
unwrapBytesValue = AesonT.parseEither Aeson.parseJSON


-- ---------------------------------------------------------------------------
-- Empty / NullValue / Any
-- ---------------------------------------------------------------------------

-- | Encode 'Empty.Empty' as an empty JSON object.
emptyToJSON :: Empty.Empty -> Aeson.Value
emptyToJSON _ = Aeson.Object AesonKM.empty


-- | Parse 'Empty.Empty' from a JSON object.
emptyFromJSON :: Aeson.Value -> Either String Empty.Empty
emptyFromJSON (Aeson.Object _) = Right Empty.defaultEmpty
emptyFromJSON _ = Left "Expected JSON Object for Empty"


{- | 'NullValue' is the proto3 enum @NULL_VALUE = 0@; it serialises
as JSON @null@. We import it from "Proto.Google.Protobuf.Struct"
(since that's where the codegen put it).
-}
nullValueToJSON :: NullValue -> Aeson.Value
nullValueToJSON = Aeson.toJSON


nullValueFromJSON :: Aeson.Value -> Either String NullValue
nullValueFromJSON = AesonT.parseEither Aeson.parseJSON


-- | The segment after the last slash is the canonical protobuf type name.
typeFromUrl :: Text -> Maybe Text
typeFromUrl url =
  case T.breakOnEnd "/" url of
    (prefix, name)
      | not (T.null prefix) && all validIdentifier (T.splitOn "." name) -> Just name
    _ -> Nothing
  where
    validIdentifier segment = case T.uncons segment of
      Just (first, rest) -> isIdentifierStart first && T.all isIdentifierChar rest
      Nothing -> False
    isIdentifierStart c = isAsciiLower c || isAsciiUpper c || c == '_'
    isIdentifierChar c = isIdentifierStart c || ('0' <= c && c <= '9')


{- | @google.protobuf.Any@ JSON shape:
@{"@type": "type.googleapis.com/...", ...other fields embedded...}@
for a regular message, or
@{"@type": "...", "value": <canonical>}@ for a WKT.

Falls back to the degenerate shape (@@type + base64 value@)
when the type isn't in the codec registry.
-}
anyToJSON :: TypeRegistry -> Any.Any -> Aeson.Value
anyToJSON registry a =
  anyPartsToJSON registry (Any.anyTypeUrl a) (Any.anyValue a)


-- | Encode the fields of an Any from either the shipped or a generated type.
anyPartsToJSON :: TypeRegistry -> Text -> ByteString -> Aeson.Value
anyPartsToJSON _ url bytes | T.null url && BS.null bytes = Aeson.Object AesonKM.empty
anyPartsToJSON registry url bytes =
  let typeKey = "@type"
      typeVal = (typeKey, Aeson.String url)
      envelope value = Aeson.object [typeVal, ("value", value)]
      fallback = envelope (protoBytesToJSON bytes)
  in case typeFromUrl url >>= lookupAnyCodec registry of
       Just codec ->
         case acToJSON codec bytes of
           Left _ -> fallback
           Right v
             | acIsWkt codec -> envelope v
             | Aeson.Object obj <- v ->
                 Aeson.Object (AesonKM.insert typeKey (Aeson.String url) obj)
             | otherwise -> envelope v
       Nothing -> fallback


anyFromJSON :: TypeRegistry -> Aeson.Value -> Either String Any.Any
anyFromJSON registry json = do
  (url, bytes) <- anyPartsFromJSON registry json
  pure (Any.Any url bytes [])


-- | Parse the fields of an Any without assuming a particular generated type.
anyPartsFromJSON :: TypeRegistry -> Aeson.Value -> Either String (Text, ByteString)
anyPartsFromJSON _ (Aeson.Object o) | AesonKM.null o = Right (T.empty, BS.empty)
anyPartsFromJSON registry (Aeson.Object o) = do
  let typeKey = "@type"
  url <- case AesonKM.lookup typeKey o of
    Just (Aeson.String s) -> Right s
    _ -> Left "Any: missing or non-string @type"
  ty <- maybe (Left "Any: invalid type URL") Right (typeFromUrl url)
  let fields = AesonKM.delete typeKey o
  bytes <- case lookupAnyCodec registry ty of
    Just codec
      | acIsWkt codec ->
          anyEnvelopeValue fields >>= maybe (Right BS.empty) (acFromJSON codec)
      | otherwise -> acFromJSON codec (Aeson.Object fields)
    Nothing -> do
      value <- anyEnvelopeValue fields
      maybe (Right BS.empty) (AesonT.parseEither protoBytesFromJSON) value
  pure (url, bytes)
anyPartsFromJSON _ _ = Left "Expected JSON Object for Any"


-- | Apply the embedded message's enum-name rules to a typed Any payload.
-- Leave envelope validation and all other value parsing to the usual decoder.
normalizeAnyEnumJSON :: TypeRegistry -> Aeson.Value -> Aeson.Value
normalizeAnyEnumJSON registry input@(Aeson.Object fields)
  | Just tag@(Aeson.String url) <- AesonKM.lookup "@type" fields
  , Just name <- typeFromUrl url
  , Just codec <- lookupAnyCodec registry name
  = normalizePayload name codec tag
  where
    normalizePayload name codec tag
      | acIsWkt codec, name == "google.protobuf.Any", Just value <- AesonKM.lookup "value" fields =
          Aeson.Object (AesonKM.insert "value" (normalizeAnyEnumJSON registry value) fields)
      | not (acIsWkt codec)
      , Just (SomeDecoder proxy) <- Map.lookup name (trDecoders registry)
      , Aeson.Object payload <-
          protoJSONIgnoreUnknownEnumNames proxy (Aeson.Object (AesonKM.delete "@type" fields))
      = Aeson.Object (AesonKM.insert "@type" tag payload)
      | otherwise = input
normalizeAnyEnumJSON _ input = input


-- Custom WKT JSON and opaque payloads both use the single-field envelope.
anyEnvelopeValue :: Aeson.Object -> Either String (Maybe Aeson.Value)
anyEnvelopeValue fields
  | AesonKM.null (AesonKM.delete "value" fields) = Right (AesonKM.lookup "value" fields)
  | otherwise = Left "Any: expected only @type and value in the envelope"


lookupAnyCodec :: TypeRegistry -> Text -> Maybe AnyCodec
lookupAnyCodec registry name
  -- A nested Any needs the application codecs from the outer envelope.
  | name == "google.protobuf.Any" = nestedAnyCodec registry <$ lookupCodec name registry
  | otherwise = lookupCodec name registry


nestedAnyCodec :: TypeRegistry -> AnyCodec
nestedAnyCodec registry =
  wktCodecVia PD.decodeMessage PE.encodeMessage (anyToJSON registry) (anyFromJSON registry)


{- | The 'TypeRegistry' generated code uses to (de)serialise @Any@-typed
fields: 'standardWktRegistry' unioned with the process-global registry
('Proto.Registry.globalAnyRegistry'), so applications can register their
own message types at runtime (via
'Proto.Registry.registerGlobalAnyCodecs') and have @Any@ values wrapping
them produce the canonical inlined @{"\@type": …, …}@ JSON instead of the
degenerate @{"value": <base64>}@ fallback.

This is a CAF: it is evaluated once, on first @Any@ (de)serialisation,
capturing the global registry as it stands at that moment. Register all
application types /before/ any @Any@ value crosses the wire (i.e. during
process startup). When nothing is registered it is exactly
'standardWktRegistry', so WKT-only users see no change.
-}
{-# NOINLINE anyJsonRegistry #-}
anyJsonRegistry :: TypeRegistry
anyJsonRegistry =
  standardWktRegistry <> unsafePerformIO (readIORef globalAnyRegistry)


{- | A pure 'TypeRegistry' containing all 17 standard well-known
type codecs (Timestamp, Duration, FieldMask, Struct, Value,
ListValue, NullValue, Empty, Any, BoolValue, Int32Value,
Int64Value, UInt32Value, UInt64Value, FloatValue, DoubleValue,
StringValue, BytesValue).

Use this as the base registry and extend it with
'registerCodec' for application-specific message types.
-}
standardWktRegistry :: TypeRegistry
standardWktRegistry =
  foldr
    (uncurry registerCodec)
    emptyRegistry
    [
      ( "google.protobuf.Timestamp"
      , wktCodecVia
          PD.decodeMessage
          PE.encodeMessage
          timestampToJSON
          timestampFromJSON
      )
    ,
      ( "google.protobuf.Duration"
      , wktCodecVia
          PD.decodeMessage
          PE.encodeMessage
          durationToJSON
          durationFromJSON
      )
    ,
      ( "google.protobuf.FieldMask"
      , wktCodecVia
          PD.decodeMessage
          PE.encodeMessage
          fieldMaskToJSON
          fieldMaskFromJSON
      )
    ,
      ( "google.protobuf.Struct"
      , wktCodecVia
          PD.decodeMessage
          PE.encodeMessage
          structToJSON
          structFromJSON
      )
    ,
      ( "google.protobuf.Value"
      , wktCodecVia
          PD.decodeMessage
          PE.encodeMessage
          valueToJSON
          valueFromJSON
      )
    ,
      ( "google.protobuf.ListValue"
      , wktCodecVia
          (PD.decodeMessage @ListValue)
          PE.encodeMessage
          Aeson.toJSON
          (AesonT.parseEither Aeson.parseJSON)
      )
    ,
      ( "google.protobuf.NullValue"
      , wktCodecVia
          PD.decodeMessage
          PE.encodeMessage
          nullValueToJSON
          nullValueFromJSON
      )
    ,
      ( "google.protobuf.Empty"
      , (wktCodecVia PD.decodeMessage PE.encodeMessage emptyToJSON emptyFromJSON) {acIsWkt = False}
      )
    , -- google.protobuf.Any nested inside another Any: per
      -- proto3 spec, Any IS a WKT, so the envelope is "value"
      -- and the value of "value" is itself the recursive
      -- @{"@type":...,...}@ object.

      ( "google.protobuf.Any"
      , nestedAnyCodec standardWktRegistry
      )
    , -- All 9 wrapper types: each round-trips through its bare
      -- inner value rather than the generic @{"value": ...}@ shape.
      -- The Any envelope still uses @"value"@, but that's the
      -- envelope wrapping the wrapper's own bare value.

      ( "google.protobuf.BoolValue"
      , wktCodecVia PD.decodeMessage PE.encodeMessage wrapBoolValue unwrapBoolValue
      )
    ,
      ( "google.protobuf.Int32Value"
      , wktCodecVia PD.decodeMessage PE.encodeMessage wrapInt32Value unwrapInt32Value
      )
    ,
      ( "google.protobuf.Int64Value"
      , wktCodecVia PD.decodeMessage PE.encodeMessage wrapInt64Value unwrapInt64Value
      )
    ,
      ( "google.protobuf.UInt32Value"
      , wktCodecVia PD.decodeMessage PE.encodeMessage wrapUInt32Value unwrapUInt32Value
      )
    ,
      ( "google.protobuf.UInt64Value"
      , wktCodecVia PD.decodeMessage PE.encodeMessage wrapUInt64Value unwrapUInt64Value
      )
    ,
      ( "google.protobuf.FloatValue"
      , wktCodecVia PD.decodeMessage PE.encodeMessage wrapFloatValue unwrapFloatValue
      )
    ,
      ( "google.protobuf.DoubleValue"
      , wktCodecVia PD.decodeMessage PE.encodeMessage wrapDoubleValue unwrapDoubleValue
      )
    ,
      ( "google.protobuf.StringValue"
      , wktCodecVia PD.decodeMessage PE.encodeMessage wrapStringValue unwrapStringValue
      )
    ,
      ( "google.protobuf.BytesValue"
      , wktCodecVia PD.decodeMessage PE.encodeMessage wrapBytesValue unwrapBytesValue
      )
    ]


-- | Build a WKT-style 'AnyCodec' (envelope = @"value"@).
wktCodecVia
  :: (ByteString -> Either e a)
  -- ^ wire decoder
  -> (a -> ByteString)
  -- ^ wire encoder
  -> (a -> Aeson.Value)
  -- ^ JSON encoder
  -> (Aeson.Value -> Either String a)
  -- ^ JSON decoder
  -> AnyCodec
wktCodecVia decBytes encBytes encJson decJson =
  AnyCodec
    { acToJSON = \bs -> case decBytes bs of
        Left _ -> Left "Any: failed to decode embedded value"
        Right a -> Right (encJson a)
    , acFromJSON = fmap encBytes . decJson
    , acIsWkt = True
    }
