{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Avro JSON encoding, decoding, and schema serialization.
--
-- Implements the Avro specification's JSON encoding for values and schemas.
module Avro.JSON
  ( avroToJSON
  , avroFromJSON
  , avroSchemaToJSON
  , avroSchemaFromJSON
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Char (chr, ord)
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import Data.Scientific (fromFloatDigits, toBoundedInteger, toRealFloat)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Avro.Schema (AvroType(..), AvroSchema(..), AvroField(..), SortOrder(..), LogicalType(..))
import qualified Avro.Value as AV

-- | Convert an 'AV.Value' to a JSON 'Aeson.Value' according to its schema.
avroToJSON :: AvroType -> AV.Value -> Aeson.Value
avroToJSON (AvroPrimitive s) v = primToJSON s v
avroToJSON AvroRecord{avroRecordFields = fields} (AV.Record vals) =
  Aeson.Object $ KM.fromList
    [ (Key.fromText (avroFieldName f), avroToJSON (avroFieldType f) v)
    | (f, v) <- zip (V.toList fields) (V.toList vals)
    ]
avroToJSON AvroEnum{avroEnumSymbols = syms} (AV.Enum idx) =
  Aeson.String (syms V.! idx)
avroToJSON AvroArray{avroArrayItems = itemTy} (AV.Array items) =
  Aeson.Array $ V.map (avroToJSON itemTy) items
avroToJSON AvroMap{avroMapValues = valTy} (AV.Map entries) =
  Aeson.Object $ KM.fromList
    [(Key.fromText k, avroToJSON valTy v) | (k, v) <- V.toList entries]
avroToJSON AvroUnion{avroUnionBranches = branches} (AV.Union idx val) =
  let branchTy = branches V.! idx
  in case val of
    AV.Null -> Aeson.Null
    _       -> Aeson.Object $ KM.singleton
                (Key.fromText (typeName branchTy))
                (avroToJSON branchTy val)
avroToJSON AvroFixed{} (AV.Fixed bs) = bytesToJSON bs
avroToJSON AvroLogical{avroLogicalBase = base} v = avroToJSON base v
avroToJSON _ _ = error "Avro.JSON: schema/value mismatch"

-- | Parse a JSON 'Aeson.Value' into an 'AV.Value' according to its schema.
avroFromJSON :: AvroType -> Aeson.Value -> Either String AV.Value
avroFromJSON (AvroPrimitive s) v = primFromJSON s v
avroFromJSON AvroRecord{avroRecordFields = fields} (Aeson.Object obj) =
  AV.Record . V.fromList <$> mapM (\f ->
    case KM.lookup (Key.fromText (avroFieldName f)) obj of
      Just v  -> avroFromJSON (avroFieldType f) v
      Nothing -> Left $ "missing field: " ++ T.unpack (avroFieldName f)
    ) (V.toList fields)
avroFromJSON AvroEnum{avroEnumSymbols = syms} (Aeson.String s) =
  case V.findIndex (== s) syms of
    Just idx -> Right (AV.Enum idx)
    Nothing  -> Left $ "unknown enum symbol: " ++ T.unpack s
avroFromJSON AvroArray{avroArrayItems = itemTy} (Aeson.Array arr) =
  AV.Array <$> V.mapM (avroFromJSON itemTy) arr
avroFromJSON AvroMap{avroMapValues = valTy} (Aeson.Object obj) =
  AV.Map . V.fromList <$> mapM (\(k, v) -> (Key.toText k,) <$> avroFromJSON valTy v) (KM.toList obj)
avroFromJSON AvroUnion{avroUnionBranches = branches} v = unionFromJSON branches v
avroFromJSON AvroFixed{avroFixedSize = sz} (Aeson.String s) = do
  bs <- textToBytesStrict s
  if BS.length bs == sz
     then Right (AV.Fixed bs)
     else Left $ "fixed size mismatch: expected " ++ show sz ++ " got " ++ show (BS.length bs)
avroFromJSON AvroLogical{avroLogicalBase = base} v = avroFromJSON base v
avroFromJSON _ _ = Left "Avro.JSON: type/value mismatch"

-- | Serialize an Avro schema to its canonical JSON representation.
avroSchemaToJSON :: AvroType -> Aeson.Value
avroSchemaToJSON (AvroPrimitive s) = Aeson.String (schemaName s)
avroSchemaToJSON ty@AvroRecord{} =
  Aeson.Object $ KM.fromList $
    [ ("type", Aeson.String "record")
    , ("name", Aeson.String (avroRecordName ty))
    , ("fields", Aeson.Array $ V.map fieldToJSON (avroRecordFields ty))
    ] ++ aliasesPair (avroRecordAliases ty)
avroSchemaToJSON ty@AvroEnum{} =
  Aeson.Object $ KM.fromList $
    [ ("type", Aeson.String "enum")
    , ("name", Aeson.String (avroEnumName ty))
    , ("symbols", Aeson.Array $ V.map Aeson.String (avroEnumSymbols ty))
    ] ++ maybe [] (\d -> [("default", Aeson.String d)]) (avroEnumDefault ty)
      ++ aliasesPair (avroEnumAliases ty)
avroSchemaToJSON AvroArray{avroArrayItems = items} =
  Aeson.Object $ KM.fromList
    [ ("type", Aeson.String "array")
    , ("items", avroSchemaToJSON items)
    ]
avroSchemaToJSON AvroMap{avroMapValues = vals} =
  Aeson.Object $ KM.fromList
    [ ("type", Aeson.String "map")
    , ("values", avroSchemaToJSON vals)
    ]
avroSchemaToJSON AvroUnion{avroUnionBranches = branches} =
  Aeson.Array $ V.map avroSchemaToJSON branches
avroSchemaToJSON ty@AvroFixed{} =
  Aeson.Object $ KM.fromList $
    [ ("type", Aeson.String "fixed")
    , ("name", Aeson.String (avroFixedName ty))
    , ("size", Aeson.Number (fromIntegral (avroFixedSize ty)))
    ] ++ aliasesPair (avroFixedAliases ty)
avroSchemaToJSON AvroLogical{avroLogicalBase = base, avroLogicalType = lt} =
  case avroSchemaToJSON base of
    Aeson.Object obj -> Aeson.Object (KM.insert "logicalType" (Aeson.String (logicalTypeName lt)) obj)
    Aeson.String s -> Aeson.Object $ KM.fromList
      [ ("type", Aeson.String s)
      , ("logicalType", Aeson.String (logicalTypeName lt))
      ]
    other -> other

aliasesPair :: V.Vector Text -> [(Key.Key, Aeson.Value)]
aliasesPair aliases
  | V.null aliases = []
  | otherwise = [("aliases", Aeson.Array (V.map Aeson.String aliases))]

-- | Parse a JSON value as an Avro schema.
avroSchemaFromJSON :: Aeson.Value -> Either String AvroType
avroSchemaFromJSON (Aeson.String s) = case s of
  "null"    -> Right (AvroPrimitive AvroNull)
  "boolean" -> Right (AvroPrimitive AvroBool)
  "int"     -> Right (AvroPrimitive AvroInt)
  "long"    -> Right (AvroPrimitive AvroLong)
  "float"   -> Right (AvroPrimitive AvroFloat)
  "double"  -> Right (AvroPrimitive AvroDouble)
  "bytes"   -> Right (AvroPrimitive AvroBytes)
  "string"  -> Right (AvroPrimitive AvroString)
  other     -> Right (AvroPrimitive (AvroSchemaRef other))
avroSchemaFromJSON (Aeson.Array arr) =
  AvroUnion <$> V.mapM avroSchemaFromJSON arr
avroSchemaFromJSON (Aeson.Object obj) = do
  typStr <- case KM.lookup "type" obj of
    Just (Aeson.String t) -> Right t
    _ -> Left "schema object missing 'type' string field"
  baseTy <- parseBaseType typStr obj
  case KM.lookup "logicalType" obj of
    Just (Aeson.String lt) -> case parseLogicalType lt of
      Just logTy -> Right AvroLogical { avroLogicalBase = baseTy, avroLogicalType = logTy }
      Nothing    -> Right baseTy
    _ -> Right baseTy
avroSchemaFromJSON _ = Left "invalid schema JSON: expected string, array, or object"

parseBaseType :: Text -> KM.KeyMap Aeson.Value -> Either String AvroType
parseBaseType typStr obj =
  case typStr of
    "record" -> do
      name <- requireString "name" obj
      fieldsArr <- case KM.lookup "fields" obj of
        Just (Aeson.Array arr) -> Right arr
        _ -> Left "record missing 'fields' array"
      fields <- V.mapM fieldFromJSON fieldsArr
      let aliases = parseAliases obj
          knownKeys = Set.fromList ["type", "name", "namespace", "doc", "fields", "aliases"]
          props = extractProps knownKeys obj
      Right AvroRecord
        { avroRecordName      = name
        , avroRecordNamespace  = optString "namespace" obj
        , avroRecordDoc        = optString "doc" obj
        , avroRecordAliases    = aliases
        , avroRecordFields     = fields
        , avroRecordProps      = props
        }
    "enum" -> do
      name <- requireString "name" obj
      symsArr <- case KM.lookup "symbols" obj of
        Just (Aeson.Array arr) -> Right arr
        _ -> Left "enum missing 'symbols' array"
      syms <- V.mapM (\case
        Aeson.String s -> Right s
        _ -> Left "enum symbol must be a string") symsArr
      let aliases = parseAliases obj
      Right AvroEnum
        { avroEnumName      = name
        , avroEnumNamespace  = optString "namespace" obj
        , avroEnumDoc        = optString "doc" obj
        , avroEnumAliases    = aliases
        , avroEnumSymbols    = syms
        , avroEnumDefault    = optString "default" obj
        }
    "array" -> do
      itemsV <- case KM.lookup "items" obj of
        Just v  -> avroSchemaFromJSON v
        Nothing -> Left "array missing 'items'"
      Right AvroArray { avroArrayItems = itemsV }
    "map" -> do
      valsV <- case KM.lookup "values" obj of
        Just v  -> avroSchemaFromJSON v
        Nothing -> Left "map missing 'values'"
      Right AvroMap { avroMapValues = valsV }
    "fixed" -> do
      name <- requireString "name" obj
      sz <- case KM.lookup "size" obj of
        Just (Aeson.Number n) -> case toBoundedInteger n of
          Just i  -> Right (i :: Int)
          Nothing -> Left "fixed: size out of range"
        _ -> Left "fixed missing 'size'"
      let aliases = parseAliases obj
      Right AvroFixed
        { avroFixedName      = name
        , avroFixedNamespace  = optString "namespace" obj
        , avroFixedSize       = sz
        , avroFixedAliases    = aliases
        }
    other -> case primitiveFromString other of
      Just ty -> Right ty
      Nothing -> Left $ "unknown schema type: " ++ T.unpack other

primitiveFromString :: Text -> Maybe AvroType
primitiveFromString s = case s of
  "null"    -> Just (AvroPrimitive AvroNull)
  "boolean" -> Just (AvroPrimitive AvroBool)
  "int"     -> Just (AvroPrimitive AvroInt)
  "long"    -> Just (AvroPrimitive AvroLong)
  "float"   -> Just (AvroPrimitive AvroFloat)
  "double"  -> Just (AvroPrimitive AvroDouble)
  "bytes"   -> Just (AvroPrimitive AvroBytes)
  "string"  -> Just (AvroPrimitive AvroString)
  _         -> Nothing

-- ============================================================
-- Internal helpers — value encoding
-- ============================================================

primToJSON :: AvroSchema -> AV.Value -> Aeson.Value
primToJSON AvroNull   AV.Null       = Aeson.Null
primToJSON AvroBool   (AV.Bool b)   = Aeson.Bool b
primToJSON AvroInt    (AV.Int n)    = Aeson.Number (fromIntegral n)
primToJSON AvroLong   (AV.Long n)
  | n > 9007199254740992 || n < -9007199254740992 =
      Aeson.String (T.pack (show n))
  | otherwise = Aeson.Number (fromIntegral n)
primToJSON AvroFloat  (AV.Float f)  = floatToJSON f
primToJSON AvroDouble (AV.Double d) = doubleToJSON d
primToJSON AvroBytes  (AV.Bytes bs) = bytesToJSON bs
primToJSON AvroString (AV.String t) = Aeson.String t
primToJSON _ _ = error "Avro.JSON: primitive schema/value mismatch"

primFromJSON :: AvroSchema -> Aeson.Value -> Either String AV.Value
primFromJSON AvroNull Aeson.Null = Right AV.Null
primFromJSON AvroBool (Aeson.Bool b) = Right (AV.Bool b)
primFromJSON AvroInt (Aeson.Number n) =
  case toBoundedInteger n :: Maybe Int32 of
    Just i  -> Right (AV.Int i)
    Nothing -> Left "int: value out of Int32 range"
primFromJSON AvroLong (Aeson.Number n) =
  case toBoundedInteger n :: Maybe Int64 of
    Just i  -> Right (AV.Long i)
    Nothing -> Left "long: value out of Int64 range"
primFromJSON AvroLong (Aeson.String s) =
  case reads (T.unpack s) of
    [(i, "")] -> Right (AV.Long i)
    _         -> Left "long: invalid string encoding"
primFromJSON AvroFloat (Aeson.Number n) = Right (AV.Float (toRealFloat n))
primFromJSON AvroFloat (Aeson.String s) = case s of
  "NaN"       -> Right (AV.Float (0 / 0))
  "Infinity"  -> Right (AV.Float (1 / 0))
  "-Infinity" -> Right (AV.Float (negate (1 / 0)))
  _           -> Left "float: unrecognized string value"
primFromJSON AvroDouble (Aeson.Number n) = Right (AV.Double (toRealFloat n))
primFromJSON AvroDouble (Aeson.String s) = case s of
  "NaN"       -> Right (AV.Double (0 / 0))
  "Infinity"  -> Right (AV.Double (1 / 0))
  "-Infinity" -> Right (AV.Double (negate (1 / 0)))
  _           -> Left "double: unrecognized string value"
primFromJSON AvroBytes (Aeson.String s) =
  AV.Bytes <$> textToBytesStrict s
primFromJSON AvroString (Aeson.String s) = Right (AV.String s)
primFromJSON _ _ = Left "primitive type/JSON mismatch"

floatToJSON :: Float -> Aeson.Value
floatToJSON !f
  | isNaN f               = Aeson.String "NaN"
  | isInfinite f && f > 0 = Aeson.String "Infinity"
  | isInfinite f          = Aeson.String "-Infinity"
  | otherwise             = Aeson.Number (fromFloatDigits f)

doubleToJSON :: Double -> Aeson.Value
doubleToJSON !d
  | isNaN d               = Aeson.String "NaN"
  | isInfinite d && d > 0 = Aeson.String "Infinity"
  | isInfinite d          = Aeson.String "-Infinity"
  | otherwise             = Aeson.Number (fromFloatDigits d)

bytesToJSON :: BS.ByteString -> Aeson.Value
bytesToJSON bs = Aeson.String $ T.pack [chr (fromIntegral b) | b <- BS.unpack bs]

textToBytes :: Text -> BS.ByteString
textToBytes = BS.pack . map (fromIntegral . ord) . T.unpack

-- | Avro JSON spec maps bytes/fixed values to a JSON string where each
-- character is the Unicode code point of one byte (i.e. code points
-- 0..255). Reject anything outside that range so we don't silently
-- truncate.
textToBytesStrict :: Text -> Either String BS.ByteString
textToBytesStrict t = go (T.unpack t) []
  where
    go [] acc = Right (BS.pack (reverse acc))
    go (c : rest) acc
      | ord c <= 0xFF =
          go rest (fromIntegral (ord c) : acc)
      | otherwise =
          Left $
            "Avro.JSON: bytes/fixed JSON character U+"
              ++ pad4Hex (ord c)
              ++ " is outside [U+0000, U+00FF]"

    pad4Hex :: Int -> String
    pad4Hex n =
      let hex = "0123456789ABCDEF"
          d :: Int -> Char
          d i = hex !! ((n `div` (16 ^ i)) `mod` 16)
      in [d 3, d 2, d 1, d 0]

-- ============================================================
-- Internal helpers — union encoding
-- ============================================================

unionFromJSON :: V.Vector AvroType -> Aeson.Value -> Either String AV.Value
unionFromJSON branches Aeson.Null =
  case V.findIndex isNullType branches of
    Just idx -> Right (AV.Union idx AV.Null)
    Nothing  -> Left "union: null is not a branch of this union"
unionFromJSON branches (Aeson.Object obj) =
  case KM.toList obj of
    [(k, v)] ->
      let name = Key.toText k
      in case V.findIndex (\t -> typeName t == name) branches of
           Just idx -> do
             val <- avroFromJSON (branches V.! idx) v
             Right (AV.Union idx val)
           Nothing -> Left $ "union: no branch named " ++ T.unpack name
    _ -> Left "union: expected single-key object or null"
unionFromJSON _ _ = Left "union: expected null or single-key object"

isNullType :: AvroType -> Bool
isNullType (AvroPrimitive AvroNull) = True
isNullType _                        = False

typeName :: AvroType -> Text
typeName (AvroPrimitive s)                  = schemaName s
typeName AvroRecord{avroRecordName = n}     = n
typeName AvroEnum{avroEnumName = n}         = n
typeName AvroArray{}                        = "array"
typeName AvroMap{}                          = "map"
typeName AvroFixed{avroFixedName = n}       = n
typeName AvroUnion{}                        = "union"
typeName AvroLogical{avroLogicalBase = base} = typeName base

-- ============================================================
-- Internal helpers — schema encoding
-- ============================================================

schemaName :: AvroSchema -> Text
schemaName AvroNull          = "null"
schemaName AvroBool          = "boolean"
schemaName AvroInt           = "int"
schemaName AvroLong          = "long"
schemaName AvroFloat         = "float"
schemaName AvroDouble        = "double"
schemaName AvroBytes         = "bytes"
schemaName AvroString        = "string"
schemaName (AvroSchemaRef t) = t

fieldToJSON :: AvroField -> Aeson.Value
fieldToJSON f = Aeson.Object $ KM.fromList $
  [ ("name", Aeson.String (avroFieldName f))
  , ("type", avroSchemaToJSON (avroFieldType f))
  ] ++ dfltPair ++ orderPair ++ aliasesPair (avroFieldAliases f) ++ docPair
  where
    dfltPair = case avroFieldDefault f of
      Just s  -> [("default", defaultSchemaToJSON s)]
      Nothing -> []
    orderPair = case avroFieldOrder f of
      Just Ascending  -> [("order", Aeson.String "ascending")]
      Just Descending -> [("order", Aeson.String "descending")]
      Just Ignore     -> [("order", Aeson.String "ignore")]
      Nothing         -> []
    docPair = case avroFieldDoc f of
      Just d  -> [("doc", Aeson.String d)]
      Nothing -> []

defaultSchemaToJSON :: AvroSchema -> Aeson.Value
defaultSchemaToJSON AvroNull   = Aeson.Null
defaultSchemaToJSON AvroBool   = Aeson.Bool False
defaultSchemaToJSON AvroInt    = Aeson.Number 0
defaultSchemaToJSON AvroLong   = Aeson.Number 0
defaultSchemaToJSON AvroFloat  = Aeson.Number 0
defaultSchemaToJSON AvroDouble = Aeson.Number 0
defaultSchemaToJSON AvroBytes  = Aeson.String ""
defaultSchemaToJSON AvroString = Aeson.String ""
defaultSchemaToJSON _          = Aeson.Null

fieldFromJSON :: Aeson.Value -> Either String AvroField
fieldFromJSON (Aeson.Object obj) = do
  name <- requireString "name" obj
  ty <- case KM.lookup "type" obj of
    Just v  -> avroSchemaFromJSON v
    Nothing -> Left "field missing 'type'"
  let dflt = case KM.lookup "default" obj of
        Just _  -> defaultForType ty
        Nothing -> Nothing
      order = case KM.lookup "order" obj of
        Just (Aeson.String "ascending")  -> Just Ascending
        Just (Aeson.String "descending") -> Just Descending
        Just (Aeson.String "ignore")     -> Just Ignore
        _                                -> Nothing
      aliases = parseAliases obj
      doc = optString "doc" obj
      knownFieldKeys = Set.fromList ["name", "type", "default", "order", "aliases", "doc"]
      props = extractProps knownFieldKeys obj
  Right AvroField
    { avroFieldName    = name
    , avroFieldType    = ty
    , avroFieldDefault = dflt
    , avroFieldOrder   = order
    , avroFieldAliases = aliases
    , avroFieldDoc     = doc
    , avroFieldProps   = props
    }
fieldFromJSON _ = Left "field must be a JSON object"

defaultForType :: AvroType -> Maybe AvroSchema
defaultForType (AvroPrimitive s) = Just s
defaultForType _                 = Just AvroNull

requireString :: Text -> KM.KeyMap Aeson.Value -> Either String Text
requireString k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.String s) -> Right s
  _                     -> Left $ "missing or non-string field: " ++ T.unpack k

optString :: Text -> KM.KeyMap Aeson.Value -> Maybe Text
optString k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.String s) -> Just s
  _                     -> Nothing

parseAliases :: KM.KeyMap Aeson.Value -> V.Vector Text
parseAliases obj = case KM.lookup "aliases" obj of
  Just (Aeson.Array arr) -> V.mapMaybe (\case
    Aeson.String s -> Just s
    _ -> Nothing) arr
  _ -> V.empty

extractProps :: Set.Set Text -> KM.KeyMap Aeson.Value -> Map.Map Text Text
extractProps known obj =
  Map.fromList
    [ (Key.toText k, t)
    | (k, v) <- KM.toList obj
    , not (Set.member (Key.toText k) known)
    , t <- case v of
        Aeson.String s -> [s]
        _ -> []
    ]

parseLogicalType :: Text -> Maybe LogicalType
parseLogicalType "date"             = Just DateLogical
parseLogicalType "time-millis"      = Just TimeMillisLogical
parseLogicalType "time-micros"      = Just TimeMicrosLogical
parseLogicalType "timestamp-millis" = Just TimestampMillisLogical
parseLogicalType "timestamp-micros" = Just TimestampMicrosLogical
parseLogicalType "duration"         = Just DurationLogical
parseLogicalType "uuid"             = Just UuidLogical
parseLogicalType other              = Just (CustomLogical other)

logicalTypeName :: LogicalType -> Text
logicalTypeName DateLogical             = "date"
logicalTypeName TimeMillisLogical       = "time-millis"
logicalTypeName TimeMicrosLogical       = "time-micros"
logicalTypeName TimestampMillisLogical  = "timestamp-millis"
logicalTypeName TimestampMicrosLogical  = "timestamp-micros"
logicalTypeName DurationLogical         = "duration"
logicalTypeName UuidLogical             = "uuid"
logicalTypeName (DecimalLogical _ _)    = "decimal"
logicalTypeName (CustomLogical name)    = name

instance Aeson.ToJSON AV.Value where
  toJSON = valueToJSON

valueToJSON :: AV.Value -> Aeson.Value
valueToJSON AV.Null         = Aeson.Null
valueToJSON (AV.Bool b)     = Aeson.Bool b
valueToJSON (AV.Int n)      = Aeson.Number (fromIntegral n)
valueToJSON (AV.Long n)     = Aeson.Number (fromIntegral n)
valueToJSON (AV.Float f)    = floatToJSON f
valueToJSON (AV.Double d)   = doubleToJSON d
valueToJSON (AV.Bytes bs)   = bytesToJSON bs
valueToJSON (AV.String t)   = Aeson.String t
valueToJSON (AV.Record vs)  = Aeson.Array (V.map valueToJSON vs)
valueToJSON (AV.Enum idx)   = Aeson.Number (fromIntegral idx)
valueToJSON (AV.Array vs)   = Aeson.Array (V.map valueToJSON vs)
valueToJSON (AV.Map entries) =
  Aeson.Object $ KM.fromList
    [(Key.fromText k, valueToJSON v) | (k, v) <- V.toList entries]
valueToJSON (AV.Union _ val) = valueToJSON val
valueToJSON (AV.Fixed bs)   = bytesToJSON bs
