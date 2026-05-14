{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{- | Proto3 canonical JSON encoding and decoding helpers.

__Stability:__ exposed for use by wireform-proto-generated code; not
part of the stable public API.

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
  parseBytesFieldMaybe,

  -- * Proto-specific scalar JSON encoding
  protoInt64ToJSON,
  protoWord64ToJSON,
  protoFloatToJSON,
  protoDoubleToJSON,
  protoBytesToJSON,
  protoBytesFromJSON,
  protoInt64FromJSON,
  protoWord64FromJSON,
  protoDoubleFromJSON,
  protoFloatFromJSON,

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

  -- | These handle all 'StringRep' variants (strict text, lazy text,
  -- short bytestring, String).
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

  -- * Oneof variant JSON
  OneofVariantNullSemantics (..),
  parseOneofVariants,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKM
import Data.Aeson.Types qualified as Aeson
import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Base64.URL qualified as Base64URL
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as SBS
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Scientific (Scientific, fromFloatDigits, toBoundedInteger, toRealFloat)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Word (Word64)


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
parseFieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just v -> Just <$> Aeson.parseJSON v


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

-- | Encode an 'Int64' as a JSON string (proto3 canonical 64-bit encoding).
protoInt64ToJSON :: Int64 -> Aeson.Value
protoInt64ToJSON n = Aeson.String (int64ToText n)


-- | Encode a 'Word64' as a JSON string (proto3 canonical 64-bit encoding).
protoWord64ToJSON :: Word64 -> Aeson.Value
protoWord64ToJSON n = Aeson.String (word64ToText n)


-- | Parse an 'Int64' from a JSON string or number (proto3 canonical).
--
-- Proto3 spec, "JSON Mapping": int64 / uint64 are encoded as
-- decimal strings on output, but accepted as either string or
-- number on input. The conformance suite verifies range +
-- integrality, so we route both shapes through 'boundedFromSci'
-- which rejects fractional and out-of-range values.
protoInt64FromJSON :: Aeson.Value -> Aeson.Parser Int64
protoInt64FromJSON (Aeson.String s) = sciFromText s >>= boundedFromSci "int64"
protoInt64FromJSON (Aeson.Number n) = boundedFromSci "int64" n
protoInt64FromJSON _ = fail "Expected int64 string or number"


-- | Parse a 'Word64' from a JSON string or number (proto3 canonical).
protoWord64FromJSON :: Aeson.Value -> Aeson.Parser Word64
protoWord64FromJSON (Aeson.String s) = sciFromText s >>= boundedFromSci "uint64"
protoWord64FromJSON (Aeson.Number n) = boundedFromSci "uint64" n
protoWord64FromJSON _ = fail "Expected uint64 string or number"


{- | Parse a 'Scientific' from a textual JSON value. Used for
the 64-bit-int code path (proto3 spec encodes them as strings
on output, accepts both shapes on input).

@reads@ is unfortunate but @Data.Text.Read@ doesn't export a
'Scientific' parser, and pulling in @attoparsec@ here just
for one helper isn't worth it.
-}
sciFromText :: Text -> Aeson.Parser Scientific
sciFromText t
  | hasLeadingSpace t = fail ("Invalid numeric string (leading whitespace): " <> show t)
  | otherwise = case reads (T.unpack t) :: [(Scientific, String)] of
      [(s, "")] -> pure s
      _ -> fail ("Invalid numeric string: " <> show t)
  where
    hasLeadingSpace s = case T.uncons s of
      Just (c, _) -> c == ' ' || c == '\t' || c == '\n' || c == '\r'
      Nothing -> True


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
protoDoubleToJSON d
  | isNaN d = Aeson.String "NaN"
  | isInfinite d = Aeson.String (if d > 0 then "Infinity" else "-Infinity")
  | otherwise = Aeson.Number (fromFloatDigits d)


-- | Encode a 'Float' as JSON, using string sentinels for NaN and Infinity.
protoFloatToJSON :: Float -> Aeson.Value
protoFloatToJSON = protoDoubleToJSON . realToFrac


-- | Parse a 'Double' from JSON, accepting NaN\/Infinity string sentinels.
protoDoubleFromJSON :: Aeson.Value -> Aeson.Parser Double
protoDoubleFromJSON (Aeson.Number n) = pure (toRealFloat n)
protoDoubleFromJSON (Aeson.String "NaN") = pure (0 / 0)
protoDoubleFromJSON (Aeson.String "Infinity") = pure (1 / 0)
protoDoubleFromJSON (Aeson.String "-Infinity") = pure (negate (1 / 0))
protoDoubleFromJSON _ = fail "Expected number or special float string"


-- | Parse a 'Float' from JSON, accepting NaN\/Infinity string sentinels.
protoFloatFromJSON :: Aeson.Value -> Aeson.Parser Float
protoFloatFromJSON v = realToFrac <$> protoDoubleFromJSON v


-- Proto3 canonical JSON: bytes as base64.

-- | Encode a strict 'ByteString' as a base64 JSON string.
protoBytesToJSON :: ByteString -> Aeson.Value
protoBytesToJSON bs = Aeson.String (TE.decodeUtf8 (Base64.encode bs))


-- Proto3 canonical-JSON spec: bytes use standard base64, but
-- the receiver MUST also accept the URL-safe variant
-- (BytesFieldBase64Url conformance test). We try standard
-- first, then URL-safe. URL.decode pads if needed but only
-- when input length is already a multiple of 4 internally;
-- for \"-_\"-style 2-char inputs we manually pad first so the
-- @decode@ entrypoint accepts them.
-- | Parse a strict 'ByteString' from a base64 or base64url JSON string.
protoBytesFromJSON :: Aeson.Value -> Aeson.Parser ByteString
protoBytesFromJSON (Aeson.String s) =
  let bs = TE.encodeUtf8 s
  in case Base64.decode bs of
      Right out -> pure out
      -- Standard base64 failed; if the input is plausibly
      -- base64url (no '+' or '/'), retry via the lenient
      -- URL decoder which tolerates unpadded inputs and
      -- the non-canonical trailing pad bits the conformance
      -- BytesFieldBase64Url test sends ("-_").
      Left err
        | looksLikeBase64Url bs ->
            pure (Base64URL.decodeLenient bs)
        | otherwise -> fail ("Invalid base64 bytes: " <> err)
protoBytesFromJSON _ = fail "Expected base64 string for bytes"


{- | Quick sniff: a string that contains no @+@\/@/@ chars
and includes either @-@ or @_@ (or is short enough that
standard base64 already failed) is plausibly base64url.
-}
looksLikeBase64Url :: ByteString -> Bool
looksLikeBase64Url bs =
  not (BS.any (\c -> c == 0x2B || c == 0x2F) bs) -- no '+' '/'
    && BS.all isUrlChar bs
  where
    isUrlChar c =
      (c >= 0x41 && c <= 0x5A) -- A-Z
        || (c >= 0x61 && c <= 0x7A) -- a-z
        || (c >= 0x30 && c <= 0x39) -- 0-9
        || c == 0x2D
        || c == 0x5F -- '-' '_'
        || c == 0x3D -- '='


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

-- | Encode a @Map Text ByteString@ field as a JSON object with base64 values.
bytesMapFieldToJSON :: Text -> Map Text ByteString -> (Text, Aeson.Value)
bytesMapFieldToJSON key m =
  ( key
  , Aeson.Object
      ( AesonKM.fromList
          (fmap (bimap AesonKey.fromText protoBytesToJSON) (Map.toList m))
      )
  )


-- | Parse an optional @Map Text ByteString@ field from a JSON object with base64 values.
parseBytesMapFieldMaybe :: Aeson.Object -> Text -> Aeson.Parser (Maybe (Map Text ByteString))
parseBytesMapFieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just (Aeson.Object o) -> do
    pairs <- traverse (\(k, v) -> (,) (AesonKey.toText k) <$> protoBytesFromJSON v) (AesonKM.toList o)
    pure (Just (Map.fromList pairs))
  Just _ -> fail ("Expected object for bytes map field: " <> T.unpack key)


{- | 'bytesMapFieldToJSON' specialised to 'BL.ByteString' values --
used by the codegen when @fieldBytes = LazyBytesRep@ on a
@map<K, bytes>@ field.
-}
lazyBytesMapFieldToJSON :: Text -> Map Text BL.ByteString -> (Text, Aeson.Value)
lazyBytesMapFieldToJSON key m =
  ( key
  , Aeson.Object
      ( AesonKM.fromList
          (fmap (bimap AesonKey.fromText protoLazyBytesToJSON) (Map.toList m))
      )
  )


-- | Parse an optional @Map Text BL.ByteString@ field from a JSON object with base64 values.
parseLazyBytesMapFieldMaybe
  :: Aeson.Object -> Text -> Aeson.Parser (Maybe (Map Text BL.ByteString))
parseLazyBytesMapFieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just (Aeson.Object o) -> do
    pairs <- traverse (\(k, v) -> (,) (AesonKey.toText k) <$> protoLazyBytesFromJSON v) (AesonKM.toList o)
    pure (Just (Map.fromList pairs))
  Just _ -> fail ("Expected object for lazy-bytes map field: " <> T.unpack key)


{- | 'bytesMapFieldToJSON' specialised to 'SBS.ShortByteString' values --
used by the codegen when @fieldBytes = ShortBytesRep@ on a
@map<K, bytes>@ field.
-}
shortBytesMapFieldToJSON :: Text -> Map Text SBS.ShortByteString -> (Text, Aeson.Value)
shortBytesMapFieldToJSON key m =
  ( key
  , Aeson.Object
      ( AesonKM.fromList
          (fmap (bimap AesonKey.fromText protoShortBytesToJSON) (Map.toList m))
      )
  )


-- | Parse an optional @Map Text SBS.ShortByteString@ field from a JSON object with base64 values.
parseShortBytesMapFieldMaybe
  :: Aeson.Object -> Text -> Aeson.Parser (Maybe (Map Text SBS.ShortByteString))
parseShortBytesMapFieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just (Aeson.Object o) -> do
    pairs <- traverse (\(k, v) -> (,) (AesonKey.toText k) <$> protoShortBytesFromJSON v) (AesonKM.toList o)
    pure (Just (Map.fromList pairs))
  Just _ -> fail ("Expected object for short-bytes map field: " <> T.unpack key)


-- ---------------------------------------------------------------------------
-- Oneof variant JSON helpers
-- ---------------------------------------------------------------------------

{- | Per-variant interpretation of JSON @null@ in a oneof. For most
variants @null@ means "this variant is unset" (matching the proto3
spec's "absent submessage" convention), so 'parseOneofVariants'
skips that variant and continues looking. For the
@google.protobuf.NullValue@ WKT, @null@ /is/ the variant's only
inhabitant — the variant must be selected with the singleton enum
value as its payload.
-}
data OneofVariantNullSemantics
  = OneofVariantNullIsUnset
  | OneofVariantNullIsValue


{- | Parse the JSON encoding of a proto3 oneof.

Proto3 JSON encodes a oneof inline at the /parent/ message level: only
the selected variant's JSON name appears, mapped to that variant's
payload. There is no enclosing object for the oneof itself.

This helper drives the parser side. The caller supplies the parent
@Aeson.Object@ and a list of @(jsonKey, nullSemantics, parser)@
triples — one per variant. We scan the object for keys that match,
apply the per-variant null filter, and produce:

* @Nothing@ — no matching variant key present.
* @Just v@ — exactly one matching variant key; @v@ is the parsed
  Haskell-side oneof sum value.
* a parser failure — more than one variant key present
  (proto3-conformant: @OneofFieldDuplicate@ is an error).

Used by both the pure-text codegen and the Template-Haskell
codegen so the two paths share one source of truth.
-}
parseOneofVariants
  :: Aeson.Object
  -> [(Text, OneofVariantNullSemantics, Aeson.Value -> Aeson.Parser a)]
  -> Aeson.Parser (Maybe a)
parseOneofVariants obj variants =
  let present = mapMaybe collect variants
      collect (k, sem, p) = do
        v <- AesonKM.lookup (AesonKey.fromText k) obj
        if keep sem v then Just (k, v, p) else Nothing
      keep OneofVariantNullIsUnset Aeson.Null = False
      keep _ _ = True
  in case present of
      [] -> pure Nothing
      [(_, v, p)] -> Just <$> p v
      _ ->
        fail
          ( "Multiple oneof variants set: "
              <> show (fmap (\(k, _, _) -> k) present)
          )
{-# INLINE parseOneofVariants #-}


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
