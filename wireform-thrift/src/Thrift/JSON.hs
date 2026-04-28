-- | Thrift JSON protocol encoding/decoding.
--
-- Provides two JSON representations:
--
-- * /Simple JSON/ ('thriftToJSON' \/ 'thriftFromJSON'): primitives as direct
--   JSON values, structs as objects keyed by field ID, containers as arrays.
--   Decoding is template-guided (a sample 'TV.Value' provides the type
--   structure).
--
-- * /Typed JSON/ ('thriftToTypedJSON' \/ 'thriftFromTypedJSON'): each value is
--   wrapped in a type-tagged object (TJSONProtocol-like), enabling decoding
--   without a separate schema.
{-# OPTIONS_GHC -Wno-orphans #-}
module Thrift.JSON
  ( -- * Simple JSON
    thriftToJSON
  , thriftFromJSON
    -- * Typed JSON (TJSONProtocol-like)
  , thriftToTypedJSON
  , thriftFromTypedJSON
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64
import Data.Int (Int8, Int16, Int64)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Scientific as Sci
import qualified Data.Vector as V

import qualified Thrift.Value as TV
import Thrift.Wire (ThriftType(..))

--------------------------------------------------------------------------------
-- Simple JSON encoding
--------------------------------------------------------------------------------

-- | Encode a 'TV.Value' as simple JSON.
thriftToJSON :: TV.Value -> Aeson.Value
thriftToJSON = \case
  TV.Bool b    -> Aeson.Bool b
  TV.Byte v    -> Aeson.Number (fromIntegral v)
  TV.I16 v     -> Aeson.Number (fromIntegral v)
  TV.I32 v     -> Aeson.Number (fromIntegral v)
  TV.I64 v     -> Aeson.String (Text.pack (show v))
  TV.Double d  -> Aeson.Number (Sci.fromFloatDigits d)
  TV.String t  -> Aeson.String t
  TV.Binary bs -> Aeson.String (TE.decodeUtf8 (Base64.encode bs))
  TV.UUID bs   -> Aeson.String (TE.decodeUtf8 (Base16.encode bs))
  TV.Struct fields ->
    Aeson.object [ (Key.fromText (Text.pack (show fid)), thriftToJSON v)
                 | (fid, v) <- V.toList fields ]
  TV.List _ elems ->
    Aeson.Array (V.map thriftToJSON elems)
  TV.Set _ elems ->
    Aeson.Array (V.map thriftToJSON elems)
  TV.Map _ _ entries ->
    Aeson.Array (V.map
      (\(k, v) -> Aeson.Array (V.fromList [thriftToJSON k, thriftToJSON v]))
      entries)

--------------------------------------------------------------------------------
-- Simple JSON decoding (template-guided)
--------------------------------------------------------------------------------

-- | Decode a JSON value into a 'TV.Value', using a template value that
-- supplies the expected type structure.
thriftFromJSON :: TV.Value -> Aeson.Value -> Either String TV.Value
thriftFromJSON template json = case template of
  TV.Bool _ -> case json of
    Aeson.Bool b -> Right (TV.Bool b)
    _            -> Left "Expected JSON bool"

  TV.Byte _ -> case json of
    Aeson.Number n -> case Sci.toBoundedInteger n of
      Just v  -> Right (TV.Byte (v :: Int8))
      Nothing -> Left "Number out of range for byte"
    _ -> Left "Expected JSON number for byte"

  TV.I16 _ -> case json of
    Aeson.Number n -> case Sci.toBoundedInteger n of
      Just v  -> Right (TV.I16 v)
      Nothing -> Left "Number out of range for i16"
    _ -> Left "Expected JSON number for i16"

  TV.I32 _ -> case json of
    Aeson.Number n -> case Sci.toBoundedInteger n of
      Just v  -> Right (TV.I32 v)
      Nothing -> Left "Number out of range for i32"
    _ -> Left "Expected JSON number for i32"

  TV.I64 _ -> case json of
    Aeson.String s -> case (TR.signed TR.decimal s :: Either String (Int64, Text)) of
      Right (v, "") -> Right (TV.I64 v)
      _             -> Left ("Invalid i64 string: " <> Text.unpack s)
    Aeson.Number n -> case Sci.toBoundedInteger n of
      Just v  -> Right (TV.I64 (v :: Int64))
      Nothing -> Left "Number out of range for i64"
    _ -> Left "Expected JSON string or number for i64"

  TV.Double _ -> case json of
    Aeson.Number n -> Right (TV.Double (Sci.toRealFloat n))
    _              -> Left "Expected JSON number for double"

  TV.String _ -> case json of
    Aeson.String t -> Right (TV.String t)
    _              -> Left "Expected JSON string"

  TV.Binary _ -> case json of
    Aeson.String s -> case Base64.decode (TE.encodeUtf8 s) of
      Right bs -> Right (TV.Binary bs)
      Left err -> Left ("Invalid base64: " <> err)
    _ -> Left "Expected JSON string (base64) for binary"

  TV.UUID _ -> case json of
    Aeson.String s -> case Base16.decode (TE.encodeUtf8 s) of
      Right bs | BS.length bs == 16 -> Right (TV.UUID bs)
               | otherwise -> Left "UUID hex must decode to 16 bytes"
      Left err -> Left ("Invalid hex for UUID: " <> err)
    _ -> Left "Expected JSON string (hex) for UUID"

  TV.Struct templateFields -> case json of
    Aeson.Object obj -> do
      let tmplMap = [ (Text.pack (show fid), tmpl)
                    | (fid, tmpl) <- V.toList templateFields ]
      fields <- mapM (\(key, val) -> do
          let kt = Key.toText key
          fid <- parseFieldId kt
          case lookup kt tmplMap of
            Just tmpl -> do
              v <- thriftFromJSON tmpl val
              Right (fid, v)
            Nothing -> Left ("Unknown field: " <> Text.unpack kt)
        ) (KM.toList obj)
      Right (TV.Struct (V.fromList (sortBy (comparing fst) fields)))
    _ -> Left "Expected JSON object for struct"

  TV.List et tmplElems -> case json of
    Aeson.Array arr -> do
      let etmpl = if V.null tmplElems
                  then defaultForWireType et
                  else V.head tmplElems
      elems <- V.mapM (thriftFromJSON etmpl) arr
      Right (TV.List et elems)
    _ -> Left "Expected JSON array for list"

  TV.Set et tmplElems -> case json of
    Aeson.Array arr -> do
      let etmpl = if V.null tmplElems
                  then defaultForWireType et
                  else V.head tmplElems
      elems <- V.mapM (thriftFromJSON etmpl) arr
      Right (TV.Set et elems)
    _ -> Left "Expected JSON array for set"

  TV.Map kt vt tmplEntries -> case json of
    Aeson.Array arr -> do
      let (kTmpl, vTmpl) = if V.null tmplEntries
                            then (defaultForWireType kt, defaultForWireType vt)
                            else let (k, v) = V.head tmplEntries in (k, v)
      entries <- V.mapM (\pairJson -> case pairJson of
          Aeson.Array pairArr
            | V.length pairArr == 2 -> do
                k <- thriftFromJSON kTmpl (pairArr V.! 0)
                v <- thriftFromJSON vTmpl (pairArr V.! 1)
                Right (k, v)
          _ -> Left "Expected [key, value] pair in map"
        ) arr
      Right (TV.Map kt vt entries)
    _ -> Left "Expected JSON array of pairs for map"

--------------------------------------------------------------------------------
-- Typed JSON encoding (TJSONProtocol-like)
--------------------------------------------------------------------------------

-- | Encode a 'TV.Value' as typed JSON (TJSONProtocol format).
thriftToTypedJSON :: TV.Value -> Aeson.Value
thriftToTypedJSON = \case
  TV.Struct fields -> encodeTypedStruct fields
  v                -> wrapTyped v

encodeTypedStruct :: V.Vector (Int16, TV.Value) -> Aeson.Value
encodeTypedStruct fields =
  Aeson.object [ (Key.fromText (Text.pack (show fid)), wrapTyped v)
               | (fid, v) <- V.toList fields ]

wrapTyped :: TV.Value -> Aeson.Value
wrapTyped v = Aeson.object [(Key.fromText (typeAbbrev v), rawTyped v)]

rawTyped :: TV.Value -> Aeson.Value
rawTyped = \case
  TV.Bool b    -> Aeson.Bool b
  TV.Byte v    -> Aeson.Number (fromIntegral v)
  TV.I16 v     -> Aeson.Number (fromIntegral v)
  TV.I32 v     -> Aeson.Number (fromIntegral v)
  TV.I64 v     -> Aeson.String (Text.pack (show v))
  TV.Double d  -> Aeson.Number (Sci.fromFloatDigits d)
  TV.String t  -> Aeson.String t
  TV.Binary bs -> Aeson.String (TE.decodeUtf8 (Base64.encode bs))
  TV.UUID bs   -> Aeson.String (TE.decodeUtf8 (Base16.encode bs))
  TV.Struct fields -> encodeTypedStruct fields
  TV.List et elems ->
    let etTag = containerElemTag et elems
    in Aeson.Array (V.fromList
        ( Aeson.String etTag
        : Aeson.Number (fromIntegral (V.length elems))
        : V.toList (V.map rawTyped elems) ))
  TV.Set et elems ->
    let etTag = containerElemTag et elems
    in Aeson.Array (V.fromList
        ( Aeson.String etTag
        : Aeson.Number (fromIntegral (V.length elems))
        : V.toList (V.map rawTyped elems) ))
  TV.Map kt vt entries ->
    let ktTag = if V.null entries then wireTypeTag kt else typeAbbrev (fst (V.head entries))
        vtTag = if V.null entries then wireTypeTag vt else typeAbbrev (snd (V.head entries))
    in Aeson.Array (V.fromList
        [ Aeson.String ktTag
        , Aeson.String vtTag
        , Aeson.Number (fromIntegral (V.length entries))
        , Aeson.object [ (Key.fromText (stringifyKey k), rawTyped v)
                        | (k, v) <- V.toList entries ]
        ])

--------------------------------------------------------------------------------
-- Typed JSON decoding
--------------------------------------------------------------------------------

-- | Decode a typed JSON value (TJSONProtocol format) into a 'TV.Value'.
thriftFromTypedJSON :: Aeson.Value -> Either String TV.Value
thriftFromTypedJSON json = case json of
  Aeson.Object obj
    | KM.null obj -> Right (TV.Struct V.empty)
    | [(key, val)] <- KM.toList obj
    , isTypeTag (Key.toText key) ->
        decodeTypedWrapped (Key.toText key) val
    | otherwise -> decodeTypedStruct obj
  _ -> Left "Expected JSON object for typed Thrift value"

decodeTypedStruct :: KM.KeyMap Aeson.Value -> Either String TV.Value
decodeTypedStruct obj = do
  fields <- mapM (\(key, val) -> do
      fid <- parseFieldId (Key.toText key)
      v <- decodeWrappedField val
      Right (fid, v)
    ) (KM.toList obj)
  Right (TV.Struct (V.fromList (sortBy (comparing fst) fields)))

decodeWrappedField :: Aeson.Value -> Either String TV.Value
decodeWrappedField json = case json of
  Aeson.Object obj
    | [(key, val)] <- KM.toList obj ->
        decodeTypedWrapped (Key.toText key) val
    | otherwise -> Left "Expected single type-tag in wrapped field"
  _ -> Left "Expected type-wrapped object for field"

decodeTypedWrapped :: Text -> Aeson.Value -> Either String TV.Value
decodeTypedWrapped tag val = case tag of
  "tf" -> case val of
    Aeson.Bool b -> Right (TV.Bool b)
    _            -> Left "Expected bool for 'tf'"
  "i8" -> case val of
    Aeson.Number n -> case Sci.toBoundedInteger n of
      Just v  -> Right (TV.Byte v)
      Nothing -> Left "Out of range for i8"
    _ -> Left "Expected number for 'i8'"
  "i16" -> case val of
    Aeson.Number n -> case Sci.toBoundedInteger n of
      Just v  -> Right (TV.I16 v)
      Nothing -> Left "Out of range for i16"
    _ -> Left "Expected number for 'i16'"
  "i32" -> case val of
    Aeson.Number n -> case Sci.toBoundedInteger n of
      Just v  -> Right (TV.I32 v)
      Nothing -> Left "Out of range for i32"
    _ -> Left "Expected number for 'i32'"
  "i64" -> case val of
    Aeson.String s -> case (TR.signed TR.decimal s :: Either String (Int64, Text)) of
      Right (v, "") -> Right (TV.I64 v)
      _             -> Left ("Invalid i64: " <> Text.unpack s)
    Aeson.Number n -> case Sci.toBoundedInteger n of
      Just v  -> Right (TV.I64 (v :: Int64))
      Nothing -> Left "Out of range for i64"
    _ -> Left "Expected string or number for 'i64'"
  "dbl" -> case val of
    Aeson.Number n -> Right (TV.Double (Sci.toRealFloat n))
    _              -> Left "Expected number for 'dbl'"
  "str" -> case val of
    Aeson.String t -> Right (TV.String t)
    _              -> Left "Expected string for 'str'"
  "bin" -> case val of
    Aeson.String s -> case Base64.decode (TE.encodeUtf8 s) of
      Right bs -> Right (TV.Binary bs)
      Left err -> Left ("Invalid base64: " <> err)
    _ -> Left "Expected string for 'bin'"
  "uuid" -> case val of
    Aeson.String s -> case Base16.decode (TE.encodeUtf8 s) of
      Right bs | BS.length bs == 16 -> Right (TV.UUID bs)
               | otherwise -> Left "UUID hex must be 16 bytes"
      Left err -> Left ("Invalid hex: " <> err)
    _ -> Left "Expected string for 'uuid'"
  "rec" -> thriftFromTypedJSON val
  "lst" -> decodeTypedList val
  "set" -> decodeTypedSet val
  "map" -> decodeTypedMap val
  _     -> Left ("Unknown type tag: " <> Text.unpack tag)

decodeTypedList :: Aeson.Value -> Either String TV.Value
decodeTypedList json = case json of
  Aeson.Array arr
    | V.length arr >= 2 -> do
        etTag <- parseTypeTagStr (arr V.! 0)
        et <- wireTypeFromTag etTag
        let elems = V.toList (V.drop 2 arr)
        decoded <- mapM (decodeTypedWrapped etTag) elems
        Right (TV.List et (V.fromList decoded))
  _ -> Left "Expected array (length >= 2) for typed list"

decodeTypedSet :: Aeson.Value -> Either String TV.Value
decodeTypedSet json = case json of
  Aeson.Array arr
    | V.length arr >= 2 -> do
        etTag <- parseTypeTagStr (arr V.! 0)
        et <- wireTypeFromTag etTag
        let elems = V.toList (V.drop 2 arr)
        decoded <- mapM (decodeTypedWrapped etTag) elems
        Right (TV.Set et (V.fromList decoded))
  _ -> Left "Expected array (length >= 2) for typed set"

decodeTypedMap :: Aeson.Value -> Either String TV.Value
decodeTypedMap json = case json of
  Aeson.Array arr
    | V.length arr >= 3 -> do
        ktTag <- parseTypeTagStr (arr V.! 0)
        vtTag <- parseTypeTagStr (arr V.! 1)
        kt <- wireTypeFromTag ktTag
        vt <- wireTypeFromTag vtTag
        if V.length arr < 4
          then Right (TV.Map kt vt V.empty)
          else case arr V.! 3 of
            Aeson.Object obj -> do
              entries <- mapM (\(k, v) -> do
                  key <- parseTypedKeyFromTag ktTag (Key.toText k)
                  val' <- decodeTypedWrapped vtTag v
                  Right (key, val')
                ) (KM.toList obj)
              Right (TV.Map kt vt (V.fromList entries))
            _ -> Left "Expected object for map entries"
  _ -> Left "Expected array (length >= 3) for typed map"

--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------

parseFieldId :: Text -> Either String Int16
parseFieldId t = case (TR.signed TR.decimal t :: Either String (Integer, Text)) of
  Right (n, "") -> Right (fromIntegral n)
  _             -> Left ("Invalid field ID: " <> Text.unpack t)

parseTypeTagStr :: Aeson.Value -> Either String Text
parseTypeTagStr (Aeson.String t) = Right t
parseTypeTagStr _                = Left "Expected string type tag"

wireTypeFromTag :: Text -> Either String ThriftType
wireTypeFromTag = \case
  "tf"   -> Right TT_BOOL
  "i8"   -> Right TT_BYTE
  "i16"  -> Right TT_I16
  "i32"  -> Right TT_I32
  "i64"  -> Right TT_I64
  "dbl"  -> Right TT_DOUBLE
  "str"  -> Right TT_STRING
  "bin"  -> Right TT_STRING
  "rec"  -> Right TT_STRUCT
  "map"  -> Right TT_MAP
  "lst"  -> Right TT_LIST
  "set"  -> Right TT_SET
  "uuid" -> Right TT_UUID
  t      -> Left ("Unknown type tag: " <> Text.unpack t)

wireTypeTag :: ThriftType -> Text
wireTypeTag = \case
  TT_BOOL   -> "tf"
  TT_BYTE   -> "i8"
  TT_I16    -> "i16"
  TT_I32    -> "i32"
  TT_I64    -> "i64"
  TT_DOUBLE -> "dbl"
  TT_STRING -> "str"
  TT_STRUCT -> "rec"
  TT_MAP    -> "map"
  TT_LIST   -> "lst"
  TT_SET    -> "set"
  TT_UUID   -> "uuid"
  TT_STOP   -> "stop"

typeAbbrev :: TV.Value -> Text
typeAbbrev = \case
  TV.Bool{}   -> "tf"
  TV.Byte{}   -> "i8"
  TV.I16{}    -> "i16"
  TV.I32{}    -> "i32"
  TV.I64{}    -> "i64"
  TV.Double{} -> "dbl"
  TV.String{} -> "str"
  TV.Binary{} -> "bin"
  TV.Struct{} -> "rec"
  TV.Map{}    -> "map"
  TV.List{}   -> "lst"
  TV.Set{}    -> "set"
  TV.UUID{}   -> "uuid"

isTypeTag :: Text -> Bool
isTypeTag t = t `elem`
  ["tf","i8","i16","i32","i64","dbl","str","bin","rec","map","lst","set","uuid"]

containerElemTag :: ThriftType -> V.Vector TV.Value -> Text
containerElemTag et elems
  | V.null elems = wireTypeTag et
  | otherwise    = typeAbbrev (V.head elems)

stringifyKey :: TV.Value -> Text
stringifyKey = \case
  TV.String t  -> t
  TV.Bool b    -> if b then "true" else "false"
  TV.Byte v    -> Text.pack (show v)
  TV.I16 v     -> Text.pack (show v)
  TV.I32 v     -> Text.pack (show v)
  TV.I64 v     -> Text.pack (show v)
  TV.Double d
    | isNaN d                  -> "NaN"
    | isInfinite d, d > 0      -> "Infinity"
    | isInfinite d             -> "-Infinity"
    | otherwise                -> Text.pack (show d)
  TV.Binary bs -> TE.decodeUtf8 (Base64.encode bs)
  TV.UUID bs   -> TE.decodeUtf8 (Base16.encode bs)
  _            -> "<complex>"

parseTypedKeyFromTag :: Text -> Text -> Either String TV.Value
parseTypedKeyFromTag tag keyStr = case tag of
  "str" -> Right (TV.String keyStr)
  "tf"  -> case keyStr of
    "true"  -> Right (TV.Bool True)
    "false" -> Right (TV.Bool False)
    _       -> Left "Invalid bool key"
  "i8"  -> readIntKey (\n -> TV.Byte (fromIntegral n)) keyStr
  "i16" -> readIntKey (\n -> TV.I16 (fromIntegral n)) keyStr
  "i32" -> readIntKey (\n -> TV.I32 (fromIntegral n)) keyStr
  "i64" -> readIntKey TV.I64 keyStr
  -- 'dbl' is the TJSONProtocol type identifier for double values.
  -- Apache Thrift renders NaN / Infinity as the JSON strings "NaN" /
  -- "Infinity" / "-Infinity"; we accept the same shapes here.
  "dbl" -> readDoubleKey keyStr
  _     -> Left ("Unsupported map key type: " <> Text.unpack tag)

readDoubleKey :: Text -> Either String TV.Value
readDoubleKey t
  | t == "NaN"       = Right (TV.Double (0/0))
  | t == "Infinity"  = Right (TV.Double (1/0))
  | t == "-Infinity" = Right (TV.Double (negate (1/0)))
  | otherwise = case TR.double t of
      Right (d, "") -> Right (TV.Double d)
      _ -> Left ("Invalid numeric key: " <> Text.unpack t)

readIntKey :: (Int64 -> TV.Value) -> Text -> Either String TV.Value
readIntKey f t = case (TR.signed TR.decimal t :: Either String (Int64, Text)) of
  Right (n, "") -> Right (f n)
  _             -> Left ("Invalid numeric key: " <> Text.unpack t)

defaultForWireType :: ThriftType -> TV.Value
defaultForWireType = \case
  TT_BOOL   -> TV.Bool False
  TT_BYTE   -> TV.Byte 0
  TT_I16    -> TV.I16 0
  TT_I32    -> TV.I32 0
  TT_I64    -> TV.I64 0
  TT_DOUBLE -> TV.Double 0
  TT_STRING -> TV.String ""
  TT_STRUCT -> TV.Struct V.empty
  TT_MAP    -> TV.Map TT_STOP TT_STOP V.empty
  TT_LIST   -> TV.List TT_STOP V.empty
  TT_SET    -> TV.Set TT_STOP V.empty
  TT_UUID   -> TV.UUID BS.empty
  TT_STOP   -> TV.Bool False

instance Aeson.ToJSON TV.Value where
  toJSON = thriftToJSON

instance Aeson.FromJSON TV.Value where
  parseJSON v = case thriftFromTypedJSON v of
    Right val -> pure val
    Left err  -> fail err
