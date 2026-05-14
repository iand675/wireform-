{- | Protobuf text format (pbtxt) serialization and deserialization.

The text format is a human-readable representation of protobuf messages,
used for configuration files, test fixtures, and debugging.

Example text format:

@
name: "John Doe"
id: 1234
email: "jdoe\@example.com"
phones {
  number: "555-4321"
  type: HOME
}
@
-}
module Proto.TextFormat (
  -- * Rendering
  dynamicToText,
  dynamicToTextPretty,
  typedToTextPretty,

  -- * Parsing
  textToDynamic,
  textToTyped,

  -- * Text format value type
  TextValue (..),
  TextField (..),
) where

import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as AesonKey
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as BL
import Data.Char (isDigit)
import Data.Int (Int64)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Data.Vector qualified as V
import Data.Word (Word64)
import Proto.Dynamic
import Proto qualified as PE
import Proto.Schema qualified as PS
import Wireform.Builder qualified as BB


-- | A text format field.
data TextField = TextField
  { tfName :: !Text
  , tfValue :: !TextValue
  }
  deriving stock (Show, Eq)


-- | A text format value.
data TextValue
  = TVString !Text
  | TVNumber !Double
  | TVInteger !Integer
  | TVBool !Bool
  | TVIdent !Text
  | TVMessage ![TextField]
  deriving stock (Show, Eq)


-- | Render a dynamic message in text format (compact).
dynamicToText :: DynamicMessage -> Text
dynamicToText = renderDyn 0 False


-- | Render a dynamic message in text format (pretty-printed).
dynamicToTextPretty :: DynamicMessage -> Text
dynamicToTextPretty = renderDyn 0 True


{- | Render a typed message in proto text format (pbtxt) with
field-name keys (rather than the field-number keys
'dynamicToTextPretty' produces). Resolves the names from the
message's 'PS.ProtoMessage' descriptor.

Returns 'Nothing' only if re-encoding the message and decoding it
back as a dynamic message fails, which should never happen in
practice for a well-formed value.

Implementation: re-encodes the typed value to bytes (so we
have a single source of truth for the wire shape), decodes
those bytes as a 'DynamicMessage', and walks the result with
the descriptor-supplied field names.
-}
typedToTextPretty
  :: forall a
   . (PE.MessageEncode a, PS.ProtoMessage a)
  => Proxy a
  -> a
  -> Maybe Text
typedToTextPretty p msg =
  let !bytes = BL.toStrict (BB.toLazyByteString (PE.buildMessage msg))
  in case decodeDynamic bytes of
      Left _ -> Nothing
      Right dyn ->
        let descriptors = PS.protoFieldDescriptors p
            nameOf fn = case IntMap.lookup fn descriptors of
              Just (PS.SomeField fd) -> PS.fdName fd
              Nothing -> intToText fn
        in Just (renderDynNamed nameOf 0 dyn)


renderDynNamed :: (Int -> Text) -> Int -> DynamicMessage -> Text
renderDynNamed nameOf depth (DynamicMessage fs _) =
  IntMap.foldlWithKey'
    ( \acc fn val ->
        acc <> renderDynField depth True (nameOf fn) val <> "\n"
    )
    ""
    fs


renderDyn :: Int -> Bool -> DynamicMessage -> Text
renderDyn depth pretty (DynamicMessage fs _) =
  let sep = if pretty then "\n" else " "
      fieldTexts =
        IntMap.foldlWithKey'
          ( \acc fn val ->
              acc <> renderDynField depth pretty (intToText fn) val <> sep
          )
          ""
          fs
  in fieldTexts


renderDynField :: Int -> Bool -> Text -> DynamicValue -> Text
renderDynField depth pretty name val =
  let ind = if pretty then T.replicate (depth * 2) " " else ""
  in case val of
      DynMessage m ->
        ind
          <> name
          <> " {"
          <> (if pretty then "\n" else " ")
          <> renderDyn (depth + 1) pretty m
          <> (if pretty then T.replicate (depth * 2) " " else "")
          <> "}"
      DynRepeated vs ->
        T.concat
          ( fmap
              ( \v ->
                  renderDynField depth pretty name v
                    <> (if pretty then "\n" else " ")
              )
              vs
          )
      DynString s -> ind <> name <> ": \"" <> escapeText s <> "\""
      DynBytes bs -> ind <> name <> ": \"" <> TE.decodeUtf8 (Base16.encode bs) <> "\""
      DynBool b -> ind <> name <> ": " <> (if b then "true" else "false")
      DynVarint v -> ind <> name <> ": " <> word64ToText v
      DynSVarint v -> ind <> name <> ": " <> int64ToText v
      DynFixed32 v -> ind <> name <> ": " <> word64ToText (fromIntegral v)
      DynFixed64 v -> ind <> name <> ": " <> word64ToText v
      DynFloat v -> ind <> name <> ": " <> T.pack (show v)
      DynDouble v -> ind <> name <> ": " <> T.pack (show v)
      DynEnum v -> ind <> name <> ": " <> intToText v
      DynMap _ -> ind <> name <> " {}"


escapeText :: Text -> Text
escapeText = T.concatMap $ \case
  '"' -> "\\\""
  '\\' -> "\\\\"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  c -> T.singleton c


{- | Parse text format into a dynamic message.
This is a simplified parser that handles the common text format subset.
-}
textToDynamic :: Text -> Either String DynamicMessage
textToDynamic t = case parseFields (T.strip t) of
  Right (fs, _) -> Right (fieldsToDynamic fs)
  Left e -> Left e


{- | Parse a proto text format (@pbtxt@) string into a typed message.

Uses the 'PS.ProtoMessage' schema to map field names to field numbers
and wire types, then converts through the proto3 JSON mapping
(via 'fromJSON') to produce the typed value.

Limitations:

  * Bytes fields at the top level are handled correctly: the raw
    string from the text format is re-encoded as base64 for the
    JSON path. Bytes inside nested submessages are passed through
    as-is; callers should pre-encode nested bytes values as base64.

  * Proto text format extensions (@[foo.bar]: value@) and unknown
    fields are silently ignored.

  * Repeated fields with scalar payloads listed on a single line
    (packed text format) are not supported; each element must appear
    as a separate @field: value@ line.

Returns 'Left' on parse failure or if 'fromJSON' rejects the value.
-}
textToTyped
  :: forall a
   . (A.FromJSON a, PS.ProtoMessage a)
  => Proxy a
  -> Text
  -> Either String a
textToTyped p src = do
  (fields, _) <- parseFields (T.strip src)
  let schema  = PS.protoFieldDescriptors p
      nameMap = buildNameMap schema
      jsonVal = textFieldsToJSON nameMap fields
  case A.fromJSON jsonVal of
    A.Error e   -> Left ("JSON decode failed: " <> e)
    A.Success a -> Right a


-- | Build a field-name → 'PS.FieldTypeDescriptor' map from the schema.
buildNameMap :: IntMap.IntMap (PS.SomeFieldDescriptor a) -> Map.Map Text PS.FieldTypeDescriptor
buildNameMap =
  IntMap.foldl'
    (\acc (PS.SomeField fd) -> Map.insert (PS.fdName fd) (PS.fdTypeDesc fd) acc)
    Map.empty


-- | Convert a list of text format fields to an Aeson 'A.Value' (always an Object).
-- Repeated fields (multiple entries with the same name) are collected into an Array.
textFieldsToJSON :: Map.Map Text PS.FieldTypeDescriptor -> [TextField] -> A.Value
textFieldsToJSON nameMap fields =
  let grouped =
        foldl
          (\acc tf -> Map.insertWith (<>) (tfName tf) [tfValue tf] acc)
          Map.empty
          fields
  in A.object
      [ AesonKey.fromText name .= valToJSON (Map.lookup name nameMap) vals
      | (name, vals) <- Map.toList grouped
      ]


valToJSON :: Maybe PS.FieldTypeDescriptor -> [TextValue] -> A.Value
valToJSON mftd vals =
  let conv = textValueToJSON mftd
  in case vals of
      [v] -> conv v
      vs  -> A.Array (V.fromList (fmap conv vs))


textValueToJSON :: Maybe PS.FieldTypeDescriptor -> TextValue -> A.Value
textValueToJSON mftd = \case
  TVString s ->
    case mftd of
      Just (PS.ScalarType PS.BytesField) ->
        -- Bytes in text format are raw strings; JSON needs base64.
        A.String (TE.decodeUtf8 (B64.encode (TE.encodeUtf8 s)))
      _ -> A.String s
  TVNumber n  -> A.Number (realToFrac n)
  TVInteger n -> A.Number (fromInteger n)
  TVBool b    -> A.Bool b
  TVIdent t   -> A.String t  -- enum name or bare identifier
  TVMessage subfields ->
    -- Nested message: recurse without sub-schema (bytes inside nested
    -- messages are passed through as-is; callers should pre-encode them).
    textFieldsToJSON Map.empty subfields


fieldsToDynamic :: [TextField] -> DynamicMessage
fieldsToDynamic tfs =
  let numbered =
        fmap
          ( \tf -> case TR.decimal (tfName tf) of
              Right (n, rest) | T.null rest -> (n, textValueToDyn (tfValue tf))
              _ -> (0, textValueToDyn (tfValue tf))
          )
          tfs
  in DynamicMessage (IntMap.fromList numbered) []


textValueToDyn :: TextValue -> DynamicValue
textValueToDyn = \case
  TVString s -> DynString s
  TVNumber n -> DynDouble n
  TVInteger n -> DynVarint (fromIntegral n)
  TVBool b -> DynBool b
  TVIdent t -> DynString t
  TVMessage fs -> DynMessage (fieldsToDynamic fs)


parseFields :: Text -> Either String ([TextField], Text)
parseFields = go []
  where
    go acc t =
      let s = T.stripStart t
      in if T.null s || T.head s == '}'
          then Right (reverse acc, s)
          else case parseField s of
            Right (f, rest) -> go (f : acc) rest
            Left e -> Left e


parseField :: Text -> Either String (TextField, Text)
parseField t = do
  let s = T.stripStart t
  let (name, rest) = T.span (\c -> c /= ':' && c /= '{' && c /= ' ' && c /= '\n') s
  if T.null name
    then Left "Expected field name"
    else do
      let rest' = T.stripStart rest
      case T.uncons rest' of
        Just (':', afterColon) -> do
          (val, remaining) <- parseTextValue (T.stripStart afterColon)
          let remaining' = T.stripStart remaining
          let remaining'' = case T.uncons remaining' of
                Just (';', r) -> r
                Just (',', r) -> r
                _ -> remaining'
          Right (TextField name val, remaining'')
        Just ('{', afterBrace) -> do
          (fields, afterFields) <- parseFields afterBrace
          case T.uncons (T.stripStart afterFields) of
            Just ('}', r) -> Right (TextField name (TVMessage fields), r)
            _ -> Left "Expected '}'"
        _ -> Left ("Expected ':' or '{' after field name '" <> T.unpack name <> "'")


parseTextValue :: Text -> Either String (TextValue, Text)
parseTextValue t
  | T.null t = Left "Empty value"
  | T.head t == '"' = parseTextString t
  | T.isPrefixOf "true" t = Right (TVBool True, T.drop 4 t)
  | T.isPrefixOf "false" t = Right (TVBool False, T.drop 5 t)
  | T.head t == '-' || isDigit (T.head t) = parseTextNumber t
  | otherwise =
      let (ident, rest) = T.span (\c -> c /= '\n' && c /= ' ' && c /= ';' && c /= ',' && c /= '}') t
      in Right (TVIdent ident, rest)


parseTextString :: Text -> Either String (TextValue, Text)
parseTextString t = go (T.drop 1 t) []
  where
    go s acc
      | T.null s = Left "Unterminated string"
      | T.head s == '"' = Right (TVString (T.pack (reverse acc)), T.drop 1 s)
      | T.head s == '\\' && T.length s >= 2 =
          case T.index s 1 of
            'n' -> go (T.drop 2 s) ('\n' : acc)
            'r' -> go (T.drop 2 s) ('\r' : acc)
            't' -> go (T.drop 2 s) ('\t' : acc)
            '"' -> go (T.drop 2 s) ('"' : acc)
            '\\' -> go (T.drop 2 s) ('\\' : acc)
            _ -> go (T.drop 2 s) (T.index s 1 : acc)
      | otherwise = go (T.drop 1 s) (T.head s : acc)


parseTextNumber :: Text -> Either String (TextValue, Text)
parseTextNumber t =
  let (numStr, rest) = T.span (\c -> c == '-' || c == '+' || c == '.' || c == 'e' || c == 'E' || isDigit c) t
  in if T.any (== '.') numStr || T.any (\c -> c == 'e' || c == 'E') numStr
      then case TR.signed TR.double numStr of
        Right (n, leftover) | T.null leftover -> Right (TVNumber n, rest)
        _ -> Left ("Invalid number: " <> T.unpack numStr)
      else case TR.signed TR.decimal numStr of
        Right (n, leftover) | T.null leftover -> Right (TVInteger n, rest)
        _ -> Left ("Invalid integer: " <> T.unpack numStr)


intToText :: Int -> Text
intToText n
  | n < 0 = "-" <> word64ToText (fromIntegral (negate n))
  | otherwise = word64ToText (fromIntegral n)


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
