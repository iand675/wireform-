{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Proto3 canonical JSON encoding and decoding via aeson.
--
-- This module re-exports the aeson types and provides helpers for
-- proto-canonical JSON representations:
--
-- * 64-bit integers encoded as JSON strings (JavaScript precision limits)
-- * bytes as base64 strings
-- * float\/double with Infinity\/NaN as string sentinels
-- * enum values as their proto name strings
module Proto.JSON
  ( -- * Re-exports from aeson
    Aeson.Value (..)
  , Aeson.ToJSON (..)
  , Aeson.FromJSON (..)
  , Aeson.object
  , (Aeson..=)

    -- * Object construction helpers for generated code
  , jsonObject
  , jsonField
  , protoObject
  , (.=:)
  , bytesFieldToJSON
  , parseField
  , parseFieldMaybe
  , parseBytesFieldMaybe
  , int64FieldToJSON
  , uint64FieldToJSON
  , parseInt64FieldMaybe
  , parseUInt64FieldMaybe
  , int64MapFieldToJSON
  , uint64MapFieldToJSON
  , parseInt64MapFieldMaybe
  , parseUInt64MapFieldMaybe

    -- * Proto-specific scalar JSON encoding
  , protoInt64ToJSON
  , protoWord64ToJSON
  , protoFloatToJSON
  , protoDoubleToJSON
  , protoBytesToJSON
  , protoBytesFromJSON
  , protoInt64FromJSON
  , protoWord64FromJSON
  , protoDoubleFromJSON
  , protoFloatFromJSON

    -- * Representation-aware bytes field helpers
    -- | These handle all 'BytesRep' variants (strict, lazy, short).
  , lazyBytesFieldToJSON
  , parseLazyBytesFieldMaybe
  , shortBytesFieldToJSON
  , parseShortBytesFieldMaybe
  , protoLazyBytesToJSON
  , protoLazyBytesFromJSON
  , protoShortBytesToJSON
  , protoShortBytesFromJSON

    -- * Representation-aware string field helpers
    -- | These handle all 'StringRep' variants (strict text, lazy text,
    -- short bytestring, String).
  , lazyTextFieldToJSON
  , parseLazyTextFieldMaybe
  , shortTextFieldToJSON
  , parseShortTextFieldMaybe
  , hsStringFieldToJSON
  , parseHsStringFieldMaybe

    -- * Map representation helpers
  , ordMapToJSON
  , hashMapToJSON
  , parseOrdMapFromJSON
  , parseHashMapFromJSON

    -- * Bytes map helpers (maps with ByteString values)
  , bytesMapFieldToJSON
  , parseBytesMapFieldMaybe
  ) where

import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Read as TR
import Data.Word (Word64)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKM

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
  Just v  -> Aeson.parseJSON v

-- | Parse an optional field from a JSON object (for generated FromJSON).
parseFieldMaybe :: Aeson.FromJSON a => Aeson.Object -> Text -> Aeson.Parser (Maybe a)
parseFieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing        -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just v         -> Just <$> Aeson.parseJSON v

-- | Encode a bytes field as a base64 JSON string field pair.
bytesFieldToJSON :: Text -> ByteString -> (Text, Aeson.Value)
bytesFieldToJSON key bs = (key, protoBytesToJSON bs)

-- | Parse an optional bytes field from base64.
parseBytesFieldMaybe :: Aeson.Object -> Text -> Aeson.Parser (Maybe ByteString)
parseBytesFieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing        -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just v         -> Just <$> protoBytesFromJSON v

-- proto3 JSON: 64-bit integer fields are emitted as JSON strings to
-- preserve precision in JS consumers (which only have 53-bit
-- integers). The parser also accepts numbers for compatibility.

int64FieldToJSON :: Text -> Int64 -> (Text, Aeson.Value)
int64FieldToJSON key n = (key, protoInt64ToJSON n)

uint64FieldToJSON :: Text -> Word64 -> (Text, Aeson.Value)
uint64FieldToJSON key n = (key, protoWord64ToJSON n)

parseInt64FieldMaybe :: Aeson.Object -> Text -> Aeson.Parser (Maybe Int64)
parseInt64FieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing         -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just v          -> Just <$> protoInt64FromJSON v

parseUInt64FieldMaybe :: Aeson.Object -> Text -> Aeson.Parser (Maybe Word64)
parseUInt64FieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing         -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just v          -> Just <$> protoWord64FromJSON v

-- proto3 JSON: a map<K, int64> field is encoded as a JSON object whose
-- VALUES are strings (the keys are always strings regardless of the
-- proto key type).

int64MapFieldToJSON :: Text -> Map.Map Text Int64 -> (Text, Aeson.Value)
int64MapFieldToJSON key m =
  (key, Aeson.Object $ AesonKM.fromList
    (fmap (bimap AesonKey.fromText protoInt64ToJSON) (Map.toList m)))

uint64MapFieldToJSON :: Text -> Map.Map Text Word64 -> (Text, Aeson.Value)
uint64MapFieldToJSON key m =
  (key, Aeson.Object $ AesonKM.fromList
    (fmap (bimap AesonKey.fromText protoWord64ToJSON) (Map.toList m)))

parseInt64MapFieldMaybe
  :: Aeson.Object -> Text -> Aeson.Parser (Maybe (Map.Map Text Int64))
parseInt64MapFieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing         -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just (Aeson.Object o) -> do
    pairs <- traverse
      (\(k, v) -> (,) (AesonKey.toText k) <$> protoInt64FromJSON v)
      (AesonKM.toList o)
    pure (Just (Map.fromList pairs))
  Just _ -> fail ("Expected JSON object for int64 map: " <> T.unpack key)

parseUInt64MapFieldMaybe
  :: Aeson.Object -> Text -> Aeson.Parser (Maybe (Map.Map Text Word64))
parseUInt64MapFieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing         -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just (Aeson.Object o) -> do
    pairs <- traverse
      (\(k, v) -> (,) (AesonKey.toText k) <$> protoWord64FromJSON v)
      (AesonKM.toList o)
    pure (Just (Map.fromList pairs))
  Just _ -> fail ("Expected JSON object for uint64 map: " <> T.unpack key)

-- Proto3 canonical JSON: 64-bit integers are encoded as strings.

protoInt64ToJSON :: Int64 -> Aeson.Value
protoInt64ToJSON n = Aeson.String (int64ToText n)

protoWord64ToJSON :: Word64 -> Aeson.Value
protoWord64ToJSON n = Aeson.String (word64ToText n)

protoInt64FromJSON :: Aeson.Value -> Aeson.Parser Int64
protoInt64FromJSON (Aeson.String s) = case TR.signed TR.decimal s of
  Right (n, rest) | T.null rest -> pure n
  _ -> fail "Invalid int64 string"
protoInt64FromJSON (Aeson.Number n) = pure (round n)
protoInt64FromJSON _ = fail "Expected int64 string or number"

protoWord64FromJSON :: Aeson.Value -> Aeson.Parser Word64
protoWord64FromJSON (Aeson.String s) = case TR.decimal s of
  Right (n, rest) | T.null rest -> pure n
  _ -> fail "Invalid uint64 string"
protoWord64FromJSON (Aeson.Number n) = pure (round n)
protoWord64FromJSON _ = fail "Expected uint64 string or number"

-- Proto3 canonical JSON: floats with NaN/Infinity as strings.

protoDoubleToJSON :: Double -> Aeson.Value
protoDoubleToJSON d
  | isNaN d      = Aeson.String "NaN"
  | isInfinite d = Aeson.String (if d > 0 then "Infinity" else "-Infinity")
  | otherwise    = Aeson.Number (fromFloatDigits d)

protoFloatToJSON :: Float -> Aeson.Value
protoFloatToJSON = protoDoubleToJSON . realToFrac

protoDoubleFromJSON :: Aeson.Value -> Aeson.Parser Double
protoDoubleFromJSON (Aeson.Number n) = pure (toRealFloat n)
protoDoubleFromJSON (Aeson.String "NaN") = pure (0/0)
protoDoubleFromJSON (Aeson.String "Infinity") = pure (1/0)
protoDoubleFromJSON (Aeson.String "-Infinity") = pure (negate (1/0))
protoDoubleFromJSON _ = fail "Expected number or special float string"

protoFloatFromJSON :: Aeson.Value -> Aeson.Parser Float
protoFloatFromJSON v = realToFrac <$> protoDoubleFromJSON v

-- Proto3 canonical JSON: bytes as base64.

protoBytesToJSON :: ByteString -> Aeson.Value
protoBytesToJSON bs = Aeson.String (TE.decodeUtf8 (Base64.encode bs))

protoBytesFromJSON :: Aeson.Value -> Aeson.Parser ByteString
protoBytesFromJSON (Aeson.String s) = case Base64.decode (TE.encodeUtf8 s) of
  Right bs -> pure bs
  Left err -> fail ("Invalid base64 bytes: " <> err)
protoBytesFromJSON _ = fail "Expected base64 string for bytes"

-- ---------------------------------------------------------------------------
-- Lazy ByteString (base64)
-- ---------------------------------------------------------------------------

protoLazyBytesToJSON :: BL.ByteString -> Aeson.Value
protoLazyBytesToJSON = protoBytesToJSON . BL.toStrict

protoLazyBytesFromJSON :: Aeson.Value -> Aeson.Parser BL.ByteString
protoLazyBytesFromJSON v = BL.fromStrict <$> protoBytesFromJSON v

lazyBytesFieldToJSON :: Text -> BL.ByteString -> (Text, Aeson.Value)
lazyBytesFieldToJSON key lbs = (key, protoLazyBytesToJSON lbs)

parseLazyBytesFieldMaybe :: Aeson.Object -> Text -> Aeson.Parser (Maybe BL.ByteString)
parseLazyBytesFieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing        -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just v         -> Just <$> protoLazyBytesFromJSON v

-- ---------------------------------------------------------------------------
-- ShortByteString (base64)
-- ---------------------------------------------------------------------------

protoShortBytesToJSON :: SBS.ShortByteString -> Aeson.Value
protoShortBytesToJSON = protoBytesToJSON . SBS.fromShort

protoShortBytesFromJSON :: Aeson.Value -> Aeson.Parser SBS.ShortByteString
protoShortBytesFromJSON v = SBS.toShort <$> protoBytesFromJSON v

shortBytesFieldToJSON :: Text -> SBS.ShortByteString -> (Text, Aeson.Value)
shortBytesFieldToJSON key sbs = (key, protoShortBytesToJSON sbs)

parseShortBytesFieldMaybe :: Aeson.Object -> Text -> Aeson.Parser (Maybe SBS.ShortByteString)
parseShortBytesFieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing        -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just v         -> Just <$> protoShortBytesFromJSON v

-- ---------------------------------------------------------------------------
-- Lazy Text (JSON string)
-- ---------------------------------------------------------------------------

lazyTextFieldToJSON :: Text -> TL.Text -> (Text, Aeson.Value)
lazyTextFieldToJSON key lt = (key, Aeson.String (TL.toStrict lt))

parseLazyTextFieldMaybe :: Aeson.Object -> Text -> Aeson.Parser (Maybe TL.Text)
parseLazyTextFieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing        -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just (Aeson.String s) -> pure (Just (TL.fromStrict s))
  Just _ -> fail ("Expected string for field: " <> T.unpack key)

-- ---------------------------------------------------------------------------
-- ShortByteString as text (UTF-8 stored in SBS)
-- ---------------------------------------------------------------------------

shortTextFieldToJSON :: Text -> SBS.ShortByteString -> (Text, Aeson.Value)
shortTextFieldToJSON key sbs = case TE.decodeUtf8' (SBS.fromShort sbs) of
  Right t -> (key, Aeson.String t)
  Left _  -> (key, Aeson.String "")

parseShortTextFieldMaybe :: Aeson.Object -> Text -> Aeson.Parser (Maybe SBS.ShortByteString)
parseShortTextFieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing        -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just (Aeson.String s) -> pure (Just (SBS.toShort (TE.encodeUtf8 s)))
  Just _ -> fail ("Expected string for field: " <> T.unpack key)

-- ---------------------------------------------------------------------------
-- Haskell String (JSON string)
-- ---------------------------------------------------------------------------

hsStringFieldToJSON :: Text -> String -> (Text, Aeson.Value)
hsStringFieldToJSON key s = (key, Aeson.String (T.pack s))

parseHsStringFieldMaybe :: Aeson.Object -> Text -> Aeson.Parser (Maybe String)
parseHsStringFieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing        -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just (Aeson.String s) -> pure (Just (T.unpack s))
  Just _ -> fail ("Expected string for field: " <> T.unpack key)

-- ---------------------------------------------------------------------------
-- Map representation helpers
-- ---------------------------------------------------------------------------

-- | Convert an ordered Map to a JSON object. Keys are converted to text
-- via their 'ToJSON' instance (proto map keys are always scalar types).
ordMapToJSON :: (Aeson.ToJSON k, Aeson.ToJSON v) => Map k v -> Aeson.Value
ordMapToJSON m = Aeson.Object (AesonKM.fromList
  (fmap (\(k, v) -> (AesonKey.fromText (keyToText k), Aeson.toJSON v)) (Map.toList m)))

-- | Convert a HashMap to a JSON object.
hashMapToJSON :: (Aeson.ToJSON k, Aeson.ToJSON v) => HM.HashMap k v -> Aeson.Value
hashMapToJSON m = Aeson.Object (AesonKM.fromList
  (fmap (\(k, v) -> (AesonKey.fromText (keyToText k), Aeson.toJSON v)) (HM.toList m)))

keyToText :: Aeson.ToJSON k => k -> Text
keyToText k = case Aeson.toJSON k of
  Aeson.String s -> s
  Aeson.Number n -> T.pack (show n)
  Aeson.Bool b   -> if b then "true" else "false"
  other          -> T.pack (show other)

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

bytesMapFieldToJSON :: Text -> Map Text ByteString -> (Text, Aeson.Value)
bytesMapFieldToJSON key m =
  (key, Aeson.Object (AesonKM.fromList
    (fmap (bimap AesonKey.fromText protoBytesToJSON) (Map.toList m))))

parseBytesMapFieldMaybe :: Aeson.Object -> Text -> Aeson.Parser (Maybe (Map Text ByteString))
parseBytesMapFieldMaybe obj key = case AesonKM.lookup (AesonKey.fromText key) obj of
  Nothing        -> pure Nothing
  Just Aeson.Null -> pure Nothing
  Just (Aeson.Object o) -> do
    pairs <- traverse (\(k, v) -> (,) (AesonKey.toText k) <$> protoBytesFromJSON v) (AesonKM.toList o)
    pure (Just (Map.fromList pairs))
  Just _ -> fail ("Expected object for bytes map field: " <> T.unpack key)

-- ---------------------------------------------------------------------------
-- Internal numeric helpers
-- ---------------------------------------------------------------------------

int64ToText :: Int64 -> Text
int64ToText n
  | n < 0     = "-" <> word64ToText (fromIntegral (negate n))
  | otherwise = word64ToText (fromIntegral n)

word64ToText :: Word64 -> Text
word64ToText 0 = "0"
word64ToText n = go T.empty n
  where
    go !acc 0 = acc
    go !acc v = let (!q, !r) = v `quotRem` 10
                in go (T.cons (toEnum (fromIntegral r + 48)) acc) q
