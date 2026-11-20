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
  typedToTextPrettyWithUnknownFields,
  typedToTextWithSchema,

  -- * Parsing
  parseTextFields,
  textToDynamic,
  TextFormatSchema,
  buildTextFormatSchema,
  buildOptionTextFormatSchema,
  encodeRuntimeOptions,
  textToTyped,

  -- * Text format value type
  TextValue (TVString, TVBytes, TVNumber, TVInteger, TVBool, TVIdent, TVMessage, TVList),
  TextField (..),
) where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (digitToInt, isAsciiLower, isAsciiUpper, isDigit, isHexDigit, isOctDigit)
import Data.Int (Int32, Int64)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (groupBy, mapAccumL, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific, toRealFloat)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Text.Lazy.Builder.Int qualified as TBI
import Data.Text.Lazy.Builder.RealFloat qualified as TBF
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32, Word64, Word8)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Proto.Decode qualified as PD
import Proto.Dynamic
import Proto.Encode qualified as PE
import Proto.Extension qualified as Ext
import Proto.IDL.AST qualified as AST
import Proto.IDL.Annotations (lookupSimpleOption)
import Proto.Internal.FieldPresence (enumIsClosed, messageEncodingDelimited, repeatedFieldPacked, resolveFieldPresence)
import Proto.Internal.JSON.Number qualified as Number
import Proto.Internal.Wire (WireType (WireEndGroup, WireStartGroup))
import Proto.Internal.Wire.Decode (unZigZag32, unZigZag64, validateUtf8)
import Proto.Internal.Wire.Encode (putFixed32, putFixed64, putLengthDelimited, putTag, putVarint)
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
  | TVBytes !ByteString
  | TVNumber !Double
  | TVInteger !Integer
  | TVBool !Bool
  | TVIdent !Text
  | TVMessage ![TextField]
  | TVList ![TextValue]
  | TVExactInteger !Integer !Bool !Text
  | TVExactFloat !Bool !Scientific
  | TVEmptyMessageList
  deriving stock (Show, Eq)


{-# COMPLETE TVString, TVBytes, TVNumber, TVInteger, TVBool, TVIdent, TVMessage, TVList #-}


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
  -> Text
typedToTextPretty p msg =
  let !bytes = BL.toStrict (BB.toLazyByteString (PE.buildMessage msg))
  in case decodeDynamic bytes of
       Left _ -> "" -- shouldn't happen: we just re-encoded a valid value
       Right dyn ->
         let descriptors = PS.protoFieldDescriptors p
             nameOf fn = case Map.lookup fn descriptors of
               Just (PS.SomeField fd) -> PS.fdName fd
               Nothing -> intToText fn
         in renderDynNamed nameOf 0 dyn


-- | Render known fields and optionally include the message's top-level
-- unknown fields in the numeric debug spelling used by Protobuf printers.
typedToTextPrettyWithUnknownFields
  :: (PE.MessageEncode a, PS.ProtoMessage a, Ext.HasExtensions a)
  => Bool
  -> Proxy a
  -> a
  -> Text
typedToTextPrettyWithUnknownFields printUnknown p msg =
  let known = typedToTextPretty p (Ext.setMessageUnknownFields [] msg)
  in if printUnknown
       then known <> TL.toStrict (TB.toLazyText (renderUnknownFields 0 100 (Ext.messageUnknownFields msg)))
       else known


renderUnknownFields :: Int -> Int -> [PD.UnknownField] -> TB.Builder
renderUnknownFields depth budget = foldMap render
  where
    indentation = TB.fromText (T.replicate (depth * 2) " ")
    render field =
      indentation <> TB.fromText (intToText (Ext.unknownFieldNumber field)) <> body field <> "\n"
    body = \case
      PD.UnknownVarint _ n -> ": " <> TB.fromText (word64ToText n)
      PD.UnknownFixed32 _ n -> ": 0x" <> TB.fromText (fixedHex 8 (fromIntegral n))
      PD.UnknownFixed64 _ n -> ": 0x" <> TB.fromText (fixedHex 16 n)
      PD.UnknownLenDelim _ bytes
        | budget > 0, not (BS.null bytes), Right fields <- PD.decodeUnknownFields bytes -> nested fields
        | otherwise -> ": \"" <> escapedBytes bytes <> "\""
      PD.UnknownGroup _ bytes
        | budget > 0, Right fields <- PD.decodeUnknownFields bytes -> nested fields
        | otherwise -> " {}"
    nested fields =
      " {\n" <> renderUnknownFields (depth + 1) (budget - 1) fields <> indentation <> "}"


fixedHex :: Int -> Word64 -> Text
fixedHex width number =
  T.unfoldrN width next (4 * (width - 1))
  where
    next shift = Just (hexDigit (fromIntegral ((number `shiftR` shift) .&. 15)), shift - 4)
    hexDigit digit = toEnum (digit + if digit < 10 then 48 else 87)


escapedBytes :: ByteString -> TB.Builder
escapedBytes = BS.foldr (\byte rest -> escape byte <> rest) mempty
  where
    escape :: Word8 -> TB.Builder
    escape 9 = "\\t"
    escape 10 = "\\n"
    escape 13 = "\\r"
    escape 34 = "\\\""
    escape 92 = "\\\\"
    escape byte
      | byte >= 32 && byte <= 126 = TB.singleton (toEnum (fromIntegral byte))
      | otherwise =
          "\\" <> digit (byte `shiftR` 6) <> digit (byte `shiftR` 3) <> digit byte
    digit byte = TB.singleton (toEnum (48 + fromIntegral (byte .&. 7)))


renderDynNamed :: (Int -> Text) -> Int -> DynamicMessage -> Text
renderDynNamed nameOf depth (DynamicMessage fs _) =
  Map.foldlWithKey'
    ( \acc fn val ->
        acc <> renderDynField depth True (nameOf fn) val <> "\n"
    )
    ""
    fs


renderDyn :: Int -> Bool -> DynamicMessage -> Text
renderDyn depth pretty (DynamicMessage fs _) =
  let sep = if pretty then "\n" else " "
      fieldTexts =
        Map.foldlWithKey'
          ( \acc fn val ->
              acc <> renderDynField depth pretty (intToText fn) val <> sep
          )
          ""
          fs
  in fieldTexts


renderDynField :: Int -> Bool -> Text -> DynamicValue -> Text
renderDynField depth pretty name val =
  let ind = if pretty then T.replicate (depth * 2) " " else ""
      sep = if pretty then "\n" else " "
      renderBlock body = ind <> name <> " {" <> sep <> body <> ind <> "}"
      renderEntry (key, value) =
        renderBlock
          (renderDynField (depth + 1) pretty "1" key <> sep <> renderDynField (depth + 1) pretty "2" value <> sep)
  in case val of
       DynMessage m -> renderBlock (renderDyn (depth + 1) pretty m)
       DynGroup m -> renderBlock (renderDyn (depth + 1) pretty m)
       DynRepeated vs ->
         T.concat
           ( fmap
               ( \v ->
                   renderDynField depth pretty name v
                     <> sep
               )
               vs
           )
       DynString s -> ind <> name <> ": \"" <> escapeText s <> "\""
       DynBytes bs -> ind <> name <> ": \"" <> TL.toStrict (TB.toLazyText (escapedBytes bs)) <> "\""
       DynBool b -> ind <> name <> ": " <> (if b then "true" else "false")
       DynVarint v -> ind <> name <> ": " <> word64ToText v
       DynSVarint v -> ind <> name <> ": " <> int64ToText v
       DynFixed32 v -> ind <> name <> ": " <> word64ToText (fromIntegral v)
       DynFixed64 v -> ind <> name <> ": " <> word64ToText v
       DynFloat v -> ind <> name <> ": " <> T.pack (show v)
       DynDouble v -> ind <> name <> ": " <> T.pack (show v)
       DynEnum v -> ind <> name <> ": " <> intToText v
       DynMap entries -> T.intercalate sep (fmap renderEntry (Map.toAscList entries))


escapeText :: Text -> Text
escapeText = T.concatMap $ \case
  '"' -> "\\\""
  '\\' -> "\\\\"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  c -> T.singleton c


-- | Parse TextFormat syntax, preserving field names and encounter order.
-- Nested messages are limited to 100 levels.
parseTextFields :: Text -> Either String [TextField]
parseTextFields input = fst <$> parseFields False 100 Nothing input


-- | Parse a schema-free message whose fields use numeric names.
-- Use 'parseTextFields' to inspect text that uses declared field names.
textToDynamic :: Text -> Either String DynamicMessage
textToDynamic input = parseTextFields input >>= fieldsToDynamic


-- | A TextFormat schema compiled from source .proto files, including imports.
-- Both code generators use the same schema and their ordinary wire decoders.
data TextFormatSchema = TextFormatSchema
  { textMessages :: !(Map.Map Text TextMessageSchema)
  , textEnums :: !(Map.Map Text TextEnumSchema)
  , textHiddenOptions :: !(Map.Map Text (Set.Set Text))
  }


data TextMessageSchema = TextMessageSchema
  { textMessageFields :: !(Map.Map Text TextFieldSchema)
  , textFieldsByNumber :: !(IntMap.IntMap TextNamedField)
  , textReservedNames :: !(Set.Set Text)
  }


data TextNamedField = TextNamedField !Text !TextFieldSchema


data TextEnumSchema = TextEnumSchema
  { textEnumNames :: !(Map.Map Text Int32)
  , textEnumNumbers :: !(Map.Map Int32 Text)
  , textEnumClosed :: !Bool
  }


data TextFieldSchema = TextFieldSchema
  { textFieldNumber :: !Int
  , textFieldType :: !TextFieldType
  , textFieldLabel :: !(Maybe AST.FieldLabel)
  , textFieldOneof :: !(Maybe Text)
  , textFieldScope :: !Text
  , textFieldSourceOnly :: !Bool
  , textFieldPacked :: !Bool
  }


data TextFieldType
  = TextOrdinaryField !AST.FieldType
  | TextGroupField !Text
  | TextMapField !AST.ScalarType !AST.FieldType


data TextExtension = TextExtension !Text !Text !Text !TextFieldSchema


-- | Build a name and type index. Pass the defining file and the .proto files
-- it imports so message and enum references resolve in their lexical scope.
buildTextFormatSchema :: [AST.ProtoFile] -> Either String TextFormatSchema
buildTextFormatSchema sources = buildTextFormatSchemaFrom Map.empty sources []


-- | Build the schema used for custom descriptor options. The standard
-- option messages are available as extension targets even when the caller
-- has only the option definition files and no source copy of descriptor.proto.
buildOptionTextFormatSchema :: [AST.ProtoFile] -> [AST.ProtoFile] -> Either String TextFormatSchema
buildOptionTextFormatSchema = buildTextFormatSchemaFrom targets
  where
    targets = Map.fromList (map target names)
    target name = ("google.protobuf." <> name, messageSchema [] Set.empty)
    names =
      [ "FileOptions", "MessageOptions", "FieldOptions", "OneofOptions"
      , "EnumOptions", "EnumValueOptions", "ServiceOptions", "MethodOptions", "ExtensionRangeOptions"
      ]


buildTextFormatSchemaFrom
  :: Map.Map Text TextMessageSchema -> [AST.ProtoFile] -> [AST.ProtoFile] -> Either String TextFormatSchema
buildTextFormatSchemaFrom base sources hiddenSources = do
  files <- traverse (first T.unpack . resolveFieldPresence) sources
  hiddenFiles <- traverse (first T.unpack . resolveFieldPresence) hiddenSources
  schema <- foldM addFile (TextFormatSchema base Map.empty Map.empty) (hiddenFiles <> files)
  extensions <- concat <$> traverse extensionsInFile files
  hiddenExtensions <- concat <$> traverse extensionsInFile hiddenFiles
  let withExtensions = foldl' addExtension schema extensions
      hiddenNames = Map.fromListWith Set.union (mapMaybe (hiddenOption schema) hiddenExtensions)
  pure withExtensions
    { textMessages = Map.map (resolveDelimitedFields schema) (textMessages withExtensions)
    , textHiddenOptions = hiddenNames
    }
  where
    addFile schema file = foldM top schema (AST.protoTopLevels file)
      where
        pkg = maybe "" id (AST.protoPackage file)
        top acc (AST.TLMessage message) = addMessage file pkg acc message
        top acc (AST.TLEnum enumDef) = addEnum file pkg acc enumDef
        top acc _ = Right acc

    addMessage file scope schema message = do
      let name = qualify scope (AST.msgName message)
          elements = AST.msgElements message
      fields <- concat <$> traverse (messageField file name) elements
      let entry = messageSchema fields (Set.fromList (concatMap reservedNames elements))
          updated = schema {textMessages = Map.insert name entry (textMessages schema)}
          nested acc (AST.MEMessage child) = addMessage file name acc child
          nested acc (AST.MEEnum enumDef) = addEnum file name acc enumDef
          nested acc _ = Right acc
      foldM nested updated elements

    addEnum file scope schema enumDef = do
      closed <- first T.unpack (enumIsClosed (AST.protoSyntax file) (AST.protoOptions file) (AST.enumOptions enumDef))
      let values = fmap (\value -> (AST.evName value, fromIntegral (AST.evNumber value))) (AST.enumValues enumDef)
          numbers = Map.fromListWith (flip const) (fmap (\(name, number) -> (number, name)) values)
          entry = TextEnumSchema (Map.fromList values) numbers closed
      pure schema {textEnums = Map.insert (qualify scope (AST.enumName enumDef)) entry (textEnums schema)}

    reservedNames (AST.MEReserved (AST.ReservedNames names)) = fmap reservedName names
    reservedNames _ = []
    reservedName (AST.QuotedReservedName name) = name
    reservedName (AST.IdentReservedName name) = name

    addExtension schema (TextExtension scope owner key fieldSchema) =
      case resolveTextName schema scope owner of
        Nothing -> schema
        Just name ->
          let add message = message
                { textMessageFields = Map.insert key fieldSchema (textMessageFields message)
                , textFieldsByNumber = IntMap.insert (textFieldNumber fieldSchema) (TextNamedField key fieldSchema) (textFieldsByNumber message)
                }
          in schema {textMessages = Map.adjust add name (textMessages schema)}

    hiddenOption schema (TextExtension scope owner key _) =
      fmap (\name -> (name, Set.singleton key)) (resolveTextName schema scope owner)


messageSchema :: [(Text, TextFieldSchema)] -> Set.Set Text -> TextMessageSchema
messageSchema fields reserved =
  TextMessageSchema (Map.fromList fields) (IntMap.fromList (fmap byNumber fields)) reserved
  where
    byNumber (name, field) = (textFieldNumber field, TextNamedField name field)


mapEntrySchema :: Text -> AST.ScalarType -> AST.FieldType -> TextMessageSchema
mapEntrySchema scope keyType valueType =
  messageSchema
    [ ("key", TextFieldSchema 1 (TextOrdinaryField (AST.FTScalar keyType)) (Just AST.Optional) Nothing scope False False)
    , ("value", TextFieldSchema 2 (TextOrdinaryField valueType) (Just AST.Optional) Nothing scope False False)
    ]
    Set.empty


messageField :: AST.ProtoFile -> Text -> AST.MessageElement -> Either String [(Text, TextFieldSchema)]
messageField file scope = \case
  AST.MEField field -> do
    schema <- ordinaryField file scope field
    pure [(AST.fieldName field, schema)]
  AST.MEMapField field ->
    Right [(AST.mapFieldName field, TextFieldSchema (AST.unFieldNumber (AST.mapFieldNum field))
        (TextMapField (AST.mapKeyType field) (AST.mapValueType field)) (Just AST.Repeated) Nothing scope
        (sourceOnly (AST.mapOptions field)) False)]
  AST.MEOneof oneof -> traverse oneofField (AST.oneofFields oneof)
    where
      oneofField field = do
        fieldType <- textFieldEncoding file (AST.oneofFieldOptions field) (AST.oneofFieldType field)
        pure
          ( AST.oneofFieldName field
          , TextFieldSchema (AST.unFieldNumber (AST.oneofFieldNumber field))
              fieldType Nothing (Just (AST.oneofName oneof)) scope (sourceOnly (AST.oneofFieldOptions field)) False
          )
  _ -> Right []


ordinaryField :: AST.ProtoFile -> Text -> AST.FieldDef -> Either String TextFieldSchema
ordinaryField file scope field = do
  fieldType <- case AST.fieldType field of
    AST.FTNamed name | AST.isLegacyGroupField field -> Right (TextGroupField name)
    ordinary -> textFieldEncoding file (AST.fieldOptions field) ordinary
  packed <- if AST.fieldLabel field == Just AST.Repeated
    then first T.unpack (repeatedFieldPacked (AST.protoSyntax file) (AST.protoOptions file) (AST.fieldOptions field))
    else Right False
  pure (TextFieldSchema (AST.unFieldNumber (AST.fieldNumber field)) fieldType
    (AST.fieldLabel field) Nothing scope (sourceOnly (AST.fieldOptions field)) packed)


sourceOnly :: [AST.OptionDef] -> Bool
sourceOnly opts = lookupSimpleOption "retention" opts == Just (AST.CIdent "RETENTION_SOURCE")


textFieldEncoding :: AST.ProtoFile -> [AST.OptionDef] -> AST.FieldType -> Either String TextFieldType
textFieldEncoding file opts ty@(AST.FTNamed name) = do
  delimited <- first T.unpack (messageEncodingDelimited (AST.protoSyntax file) (AST.protoOptions file) opts)
  pure (if delimited then TextGroupField name else TextOrdinaryField ty)
textFieldEncoding _ _ ty = Right (TextOrdinaryField ty)


-- A file's encoding default applies to messages but not to enums.
-- Add the capitalized spelling only for group-like fields after their
-- referenced type has been resolved in the complete schema.
resolveDelimitedFields :: TextFormatSchema -> TextMessageSchema -> TextMessageSchema
resolveDelimitedFields schema message = IntMap.foldl' resolve message (textFieldsByNumber message)
  where
    resolve result (TextNamedField fieldName field)
      | TextGroupField typeName <- textFieldType field
      , let scope = textFieldScope field
      , Just qualified <- resolveTextName schema scope typeName =
          if Map.member qualified (textEnums schema)
            then update fieldName field {textFieldType = TextOrdinaryField (AST.FTNamed typeName)}
            else let typeShort = T.takeWhileEnd (/= '.') qualified
                 in if qualified == qualify scope typeShort && T.toLower typeShort == fieldName
                      then update typeShort field
                      else result
      | otherwise = result
      where
        update canonical entry = result
          { textMessageFields = Map.insert canonical entry (Map.insert fieldName entry (textMessageFields result))
          , textFieldsByNumber = IntMap.insert (textFieldNumber field) (TextNamedField canonical entry) (textFieldsByNumber result)
          }


extensionsInFile :: AST.ProtoFile -> Either String [TextExtension]
extensionsInFile file = concat <$> traverse top (AST.protoTopLevels file)
  where
    pkg = maybe "" id (AST.protoPackage file)
    top (AST.TLExtend owner fields) = traverse (extension pkg owner) fields
    top (AST.TLMessage message) = nested pkg message
    top _ = Right []
    nested scope message = concat <$> traverse (element (qualify scope (AST.msgName message))) (AST.msgElements message)
    element scope (AST.MEExtend owner fields) = traverse (extension scope owner) fields
    element scope (AST.MEMessage message) = nested scope message
    element _ _ = Right []
    extension scope owner field =
      TextExtension scope owner ("[" <> qualify scope (AST.fieldName field) <> "]")
        <$> ordinaryField file scope field


qualify :: Text -> Text -> Text
qualify scope name
  | T.null scope = name
  | otherwise = scope <> "." <> name


resolveTextName :: TextFormatSchema -> Text -> Text -> Maybe Text
resolveTextName schema = resolveScopedName known
  where
    known name = Map.member name (textMessages schema) || Map.member name (textEnums schema)


resolveScopedName :: (Text -> Bool) -> Text -> Text -> Maybe Text
resolveScopedName known scope name = case T.stripPrefix "." name of
  Just absolute -> if known absolute then Just absolute else Nothing
  Nothing -> go scope
  where
    go prefix
      | known candidate = Just candidate
      | T.null prefix = Nothing
      | otherwise = go (T.dropEnd 1 (fst (T.breakOnEnd "." prefix)))
      where
        candidate = qualify prefix name


-- | Resolve source custom-option assignments and encode their runtime values.
-- An unresolved extension is left to the caller's codegen hooks, which can
-- also consume annotations whose schema was never provided.
encodeRuntimeOptions
  :: TextFormatSchema -> Text -> Text -> [AST.OptionDef] -> Either String [PD.UnknownField]
encodeRuntimeOptions schema scope target options = case Map.lookup target (textMessages schema) of
  Nothing -> Right []
  Just message -> do
    fields <- foldM addOption [] options
    if null fields
      then Right []
      else do
        encoded <- encodeTextFields True schema message fields
        first show (PD.decodeUnknownFields (BB.toStrictByteString encoded))
    where
      known name = Map.member (bracket name) (textMessageFields message)
      hidden name = Set.member (bracket name) (Map.findWithDefault Set.empty target (textHiddenOptions schema))
      addOption acc opt = case AST.optNameParts (AST.optName opt) of
        AST.ExtensionOption name : rest
          | Just qualified <- resolveScopedName known scope name -> do
              let key = bracket qualified
                  (leaf, path) = mapAccumL renderPart (Map.lookup key (textMessageFields message)) rest
                  constant = AST.optValue opt
              validateScalarOption leaf constant
              value <- constantValue False leaf constant
              Right (assign acc key path value)
          | Just qualified <- resolveScopedName hidden scope name ->
              Left ("custom option " <> T.unpack qualified <> " is not visible through this file's imports")
        _ -> Right acc
      renderPart field part =
        let container = field >>= childSchema
            name = case part of
              AST.SimpleOption plain -> plain
              AST.ExtensionOption extension -> extensionKey container extension
        in (container >>= Map.lookup name . textMessageFields, name)
      extensionKey container name =
        let inScope candidate = maybe False (Map.member (bracket candidate) . textMessageFields) container
        in bracket (fromMaybe (T.dropWhile (== '.') name) (resolveScopedName inScope scope name))
      bracket name = "[" <> name <> "]"
      childSchema field = case textFieldType field of
        TextOrdinaryField (AST.FTNamed name) -> namedChild field name
        TextGroupField name -> namedChild field name
        TextMapField key val -> Just (mapEntrySchema (textFieldScope field) key val)
        TextOrdinaryField _ -> Nothing
      namedChild field name =
        resolveTextName schema (textFieldScope field) name >>= (\qualified -> Map.lookup qualified (textMessages schema))
      -- Outside an aggregate, protoc requires source identifiers for
      -- booleans and enums instead of TextFormat's numeric spellings.
      validateScalarOption (Just field) value = case textFieldType field of
        TextOrdinaryField (AST.FTScalar AST.SBool) -> case value of
          AST.CBool _ -> Right ()
          _ -> Left "Boolean option requires true or false"
        TextOrdinaryField (AST.FTNamed name)
          | Just qualified <- resolveTextName schema (textFieldScope field) name
          , Map.member qualified (textEnums schema) -> case value of
              AST.CIdent _ -> Right ()
              AST.CBool _ -> Right () -- An enum can name its values true and false.
              _ -> Left "Enum option requires an enum value name"
        _ -> Right ()
      validateScalarOption Nothing _ = Right ()
      assign acc name [] value = acc <> [TextField name value]
      assign acc name (child : rest) value =
        case break (\field -> tfName field == name) acc of
          (before, TextField _ (TVMessage fields') : after) ->
            before <> (TextField name (TVMessage (assign fields' child rest value)) : after)
          _ -> acc <> [TextField name (TVMessage (assign [] child rest value))]
      constantValue aggregate field = \case
        AST.CIdent name -> Right (TVIdent name)
        -- Aggregate literals use TextFormat's number grammar. Scalar
        -- assignments are converted from the parsed option integer instead.
        AST.CSpelledInt spelling n
          | aggregate ->
              let digits = T.stripPrefix "-" spelling
              in parseTextNumber True (isJust digits) (fromMaybe spelling digits)
          | n == 0 && T.isPrefixOf "-" spelling && not (maybe False floatingField field) ->
              Right (TVExactInteger 10 True "0")
          | otherwise -> Right (TVInteger n)
        AST.CInt n -> Right (TVInteger n)
        AST.CFloat n -> Right (TVNumber n)
        AST.CString text -> Right (TVString text)
        AST.CBytes bytes -> Right (TVBytes bytes)
        AST.CBool flag -> Right (TVBool flag)
        AST.CAggregate entries ->
          TVMessage <$> traverse (aggregateEntry (field >>= childSchema)) entries
      floatingField field = case textFieldType field of
        TextOrdinaryField (AST.FTScalar AST.SFloat) -> True
        TextOrdinaryField (AST.FTScalar AST.SDouble) -> True
        _ -> False
      aggregateEntry container (name, value) =
        let key = case T.stripPrefix "[" name >>= T.stripSuffix "]" of
              Just extension -> extensionKey container extension
              Nothing -> name
            field = container >>= Map.lookup key . textMessageFields
        in TextField key <$> constantValue True field value


-- | Decode TextFormat using the source schema of a generated message.
textToTyped :: forall a. (PS.ProtoMessage a, PD.MessageDecode a) => TextFormatSchema -> Text -> Either String a
textToTyped schema input = do
  (fields, _) <- parseFields True 100 Nothing input
  encoded <- encodeTextMessage False schema (PS.protoMessageName (Proxy @a)) fields
  first show (PD.decodeMessage (BB.toStrictByteString encoded))


-- | Print a generated message using its source schema. Unknown fields are
-- omitted unless requested, in which case they use numeric debug syntax.
typedToTextWithSchema
  :: forall a. (PS.ProtoMessage a, PE.MessageEncode a)
  => TextFormatSchema
  -> Bool
  -> a
  -> Either String Text
typedToTextWithSchema schema printUnknown message =
  TL.toStrict . TB.toLazyText
    <$> renderTextMessage schema printUnknown 0 (PS.protoMessageName (Proxy @a)) (PE.encodeMessage message)


renderTextMessage :: TextFormatSchema -> Bool -> Int -> Text -> ByteString -> Either String TB.Builder
renderTextMessage schema printUnknown depth name bytes
  | depth > 100 = Left "TextFormat message nesting exceeds 100 levels"
  | otherwise = do
      message <- maybe (Left ("Unknown TextFormat message type " <> T.unpack name)) Right
        (Map.lookup name (textMessages schema))
      fields <- first show (PD.decodeUnknownFields bytes)
      case expandAny fields of
        Just expanded -> Right expanded
        Nothing -> renderTextWireFields schema printUnknown depth message fields
  where
    expandAny fields
      | name /= "google.protobuf.Any" || printUnknown = Nothing
      | otherwise = case fields of
          [PD.UnknownLenDelim 1 url] -> expanded url BS.empty
          [PD.UnknownLenDelim 1 url, PD.UnknownLenDelim 2 payload] -> expanded url payload
          _ -> Nothing
    expanded urlBytes payload
      | Right url <- TE.decodeUtf8' urlBytes
      , T.all isBracketNameChar url
      , (prefix, innerName) <- T.breakOnEnd "/" url
      , not (T.null prefix)
      , Right body <- renderTextMessage schema False (depth + 1) innerName payload
      = Just (textMessageLine depth ("[" <> url <> "]") body)
      | otherwise = Nothing


data TextPrintState = TextPrintState !(IntMap.IntMap TB.Builder) ![PD.UnknownField]


renderTextWireFields
  :: TextFormatSchema
  -> Bool
  -> Int
  -> TextMessageSchema
  -> [PD.UnknownField]
  -> Either String TB.Builder
renderTextWireFields schema printUnknown depth message fields = do
  TextPrintState known unknown <- foldM render (TextPrintState IntMap.empty []) fields
  pure (foldMap id known <> renderUnknownFields depth (100 - depth) (reverse unknown))
  where
    render (TextPrintState known unknown) raw =
      let number = Ext.unknownFieldNumber raw
          addUnknown = Right (TextPrintState known (if printUnknown then raw : unknown else unknown))
      in case IntMap.lookup number (textFieldsByNumber message) of
           Nothing -> addUnknown
           Just (TextNamedField name field) -> do
             output <- renderTextWireField schema printUnknown depth name field raw
             case output of
               Nothing -> addUnknown
               Just text -> Right (TextPrintState (IntMap.insertWith (flip (<>)) number text known) unknown)


renderTextWireField
  :: TextFormatSchema
  -> Bool
  -> Int
  -> Text
  -> TextFieldSchema
  -> PD.UnknownField
  -> Either String (Maybe TB.Builder)
renderTextWireField schema printUnknown depth name field raw = case textFieldType field of
  TextOrdinaryField (AST.FTScalar scalar) -> printScalar (textScalarPrinter scalar)
  TextOrdinaryField (AST.FTNamed typeName) ->
    case resolveTextName schema scope typeName of
      Nothing -> Left ("Unknown TextFormat type " <> T.unpack typeName)
      Just qualified -> case Map.lookup qualified (textEnums schema) of
        Just enumDef -> printScalar (PrintVarint (enumBody enumDef . fromIntegral))
        Nothing -> nested (renderTextMessage schema printUnknown (depth + 1) qualified)
  TextGroupField typeName -> case raw of
    PD.UnknownGroup _ payload -> do
      qualified <- maybe (Left ("Unknown TextFormat group type " <> T.unpack typeName)) Right
        (resolveTextName schema scope typeName)
      nestedBody (renderTextMessage schema printUnknown (depth + 1) qualified) payload
    _ -> Right Nothing
  TextMapField keyType valueType -> nested $ \payload -> do
    let entry = mapEntrySchema scope keyType valueType
    fields <- first show (PD.decodeUnknownFields payload)
    renderTextWireFields schema printUnknown (depth + 1) entry fields
  where
    scope = textFieldScope field
    repeated = textFieldLabel field == Just AST.Repeated
    printScalar = renderTextScalar printUnknown depth name repeated raw
    nested render = case raw of
      PD.UnknownLenDelim _ payload -> nestedBody render payload
      _ -> Right Nothing
    nestedBody render payload
      | depth >= 100 = Left "TextFormat message nesting exceeds 100 levels"
      | otherwise = Just . textMessageLine depth name <$> render payload

    enumBody enumDef number = case Map.lookup number (textEnumNumbers enumDef) of
      Just alias -> Just (TB.fromText alias)
      Nothing | textEnumClosed enumDef -> Nothing
      Nothing -> Just (TBI.decimal number)


data TextScalarPrinter
  = PrintVarint !(Word64 -> Maybe TB.Builder)
  | PrintFixed32 !(Word32 -> TB.Builder)
  | PrintFixed64 !(Word64 -> TB.Builder)
  | PrintBytes !Bool


textScalarPrinter :: AST.ScalarType -> TextScalarPrinter
textScalarPrinter = \case
  AST.SInt32 -> varint (TBI.decimal . (fromIntegral :: Word64 -> Int32))
  AST.SInt64 -> varint (TBI.decimal . (fromIntegral :: Word64 -> Int64))
  AST.SUInt32 -> varint (TBI.decimal . (fromIntegral :: Word64 -> Word32))
  AST.SUInt64 -> varint TBI.decimal
  AST.SSInt32 -> varint (TBI.decimal . unZigZag32 . fromIntegral)
  AST.SSInt64 -> varint (TBI.decimal . unZigZag64)
  AST.SBool -> varint (\n -> if n == 0 then "false" else "true")
  AST.SFixed32 -> PrintFixed32 TBI.decimal
  AST.SFixed64 -> PrintFixed64 TBI.decimal
  AST.SSFixed32 -> PrintFixed32 (TBI.decimal . (fromIntegral :: Word32 -> Int32))
  AST.SSFixed64 -> PrintFixed64 (TBI.decimal . (fromIntegral :: Word64 -> Int64))
  AST.SFloat -> PrintFixed32 (textFloat . castWord32ToFloat)
  AST.SDouble -> PrintFixed64 (textFloat . castWord64ToDouble)
  AST.SString -> PrintBytes True
  AST.SBytes -> PrintBytes False
  where
    varint printValue = PrintVarint (Just . printValue)


renderTextScalar
  :: Bool -> Int -> Text -> Bool -> PD.UnknownField -> TextScalarPrinter
  -> Either String (Maybe TB.Builder)
renderTextScalar printUnknown depth name repeated raw printer = case (printer, raw) of
  (PrintVarint render, PD.UnknownVarint _ value) -> Right (line <$> render value)
  (PrintFixed32 render, PD.UnknownFixed32 _ value) -> Right (Just (line (render value)))
  (PrintFixed64 render, PD.UnknownFixed64 _ value) -> Right (Just (line (render value)))
  (PrintBytes utf8, PD.UnknownLenDelim _ bytes)
    | utf8 && not (validateUtf8 bytes) -> Left ("Invalid UTF-8 in TextFormat field " <> T.unpack name)
    | otherwise -> Right (Just (line ("\"" <> escapedBytes bytes <> "\"")))
  (PrintVarint render, PD.UnknownLenDelim number bytes) | repeated ->
    packed PD.decodePackedVarint (\value -> case render value of
      Just body -> line body
      Nothing | printUnknown -> renderUnknownFields depth (100 - depth) [PD.UnknownVarint number value]
      Nothing -> mempty) bytes
  (PrintFixed32 render, PD.UnknownLenDelim _ bytes) | repeated ->
    packed PD.decodePackedFixed32 (line . render) bytes
  (PrintFixed64 render, PD.UnknownLenDelim _ bytes) | repeated ->
    packed PD.decodePackedFixed64 (line . render) bytes
  _ -> Right Nothing
  where
    line body = textIndent depth <> TB.fromText name <> ": " <> body <> "\n"
    packed :: VU.Unbox a => PD.Decoder (VU.Vector a) -> (a -> TB.Builder) -> ByteString -> Either String (Maybe TB.Builder)
    packed decoder render bytes = do
      values <- first show (PD.runDecoder decoder (BB.toStrictByteString (putLengthDelimited bytes)))
      pure (Just (VU.foldl' (\out value -> out <> render value) mempty values))


textFloat :: RealFloat a => a -> TB.Builder
textFloat value
  | isNaN value = "nan"
  | isInfinite value = if value < 0 then "-inf" else "inf"
  | isNegativeZero value = "-0"
  | otherwise = TBF.realFloat value


textIndent :: Int -> TB.Builder
textIndent depth = TB.fromText (T.replicate (depth * 2) " ")


textMessageLine :: Int -> Text -> TB.Builder -> TB.Builder
textMessageLine depth name body =
  let indent = textIndent depth
  in indent <> TB.fromText name <> " {\n" <> body <> indent <> "}\n"


data TextEncodeState = TextEncodeState !IntSet.IntSet !(Set.Set Text) !BB.Builder


encodeTextMessage :: Bool -> TextFormatSchema -> Text -> [TextField] -> Either String BB.Builder
encodeTextMessage runtimeOnly schema "google.protobuf.Any" [TextField name (TVMessage fields)]
  | Just url <- T.stripPrefix "[" name >>= T.stripSuffix "]"
  , (prefix, messageName) <- T.breakOnEnd "/" url
  , not (T.null prefix)
  = do
      payload <- encodeTextMessage runtimeOnly schema messageName fields
      pure (PE.encodeFieldString 1 url <> PE.encodeFieldBytes 2 (BB.toStrictByteString payload))
encodeTextMessage runtimeOnly schema name fields =
  case Map.lookup name (textMessages schema) of
    Nothing -> Left ("Unknown TextFormat message type " <> T.unpack name)
    Just message -> encodeTextFields runtimeOnly schema message fields


encodeTextFields :: Bool -> TextFormatSchema -> TextMessageSchema -> [TextField] -> Either String BB.Builder
encodeTextFields runtimeOnly schema message fields = do
  TextEncodeState seen _ output <- foldM add (TextEncodeState IntSet.empty Set.empty mempty) fields
  let require name field next
        | textFieldLabel field == Just AST.Required && not (omit field)
        , IntSet.notMember (textFieldNumber field) seen =
            Left ("Missing required TextFormat field " <> T.unpack name)
        | otherwise = next
  Map.foldrWithKey require (Right ()) (textMessageFields message)
  if runtimeOnly then canonicalOptionWire message output else Right output
  where
    omit field = runtimeOnly && textFieldSourceOnly field
    add state@(TextEncodeState seen oneofs output) (TextField name value) =
      case Map.lookup name (textMessageFields message) of
        Nothing
          | Set.member name (textReservedNames message) -> Right state
          | otherwise -> Left ("Unknown TextFormat field " <> T.unpack name)
        Just field
          | omit field -> Right state
          | not (repeated field) && IntSet.member (textFieldNumber field) seen ->
              Left ("Duplicate TextFormat field " <> T.unpack name)
          | Just group <- textFieldOneof field, Set.member group oneofs ->
              Left ("Multiple values for TextFormat oneof " <> T.unpack group)
          | otherwise -> do
              encoded <- first (\err -> T.unpack name <> ": " <> err) (encodeField field value)
              let groups = maybe oneofs (\group -> Set.insert group oneofs) (textFieldOneof field)
              pure (TextEncodeState (IntSet.insert (textFieldNumber field) seen) groups (output <> encoded))

    repeated field = textFieldLabel field == Just AST.Repeated
    encodeField field TVEmptyMessageList
      | repeated field && messageType field = Right mempty
      | otherwise = Left "A scalar field requires ':' before its list"
    encodeField field (TVList values)
      | repeated field = foldM (\out value -> (out <>) <$> encodeValue field value) mempty values
      | otherwise = Left "A singular field cannot contain a list"
    encodeField field value = encodeValue field value

    messageType field = case textFieldType field of
      TextMapField _ _ -> True
      TextGroupField _ -> True
      TextOrdinaryField (AST.FTNamed name) ->
        case resolveTextName schema (textFieldScope field) name of
          Just qualified -> Map.member qualified (textMessages schema)
          Nothing -> False
      TextOrdinaryField _ -> False

    encodeValue field value =
      let number = textFieldNumber field
          scope = textFieldScope field
      in case textFieldType field of
           TextOrdinaryField fieldType -> encodeTextValue runtimeOnly schema scope number fieldType value
           TextGroupField typeName -> do
             qualified <- maybe (Left ("Unknown TextFormat group type " <> T.unpack typeName)) Right
               (resolveTextName schema scope typeName)
             children <- textMessageValue value
             body <- encodeTextMessage runtimeOnly schema qualified children
             pure (putTag number WireStartGroup <> body <> putTag number WireEndGroup)
           TextMapField keyType valueType -> do
             children <- textMessageValue value
             let entry = mapEntrySchema scope keyType valueType
             values <- if runtimeOnly then completeMapEntry schema scope keyType valueType children else Right children
             PE.encodeFieldBytes number . BB.toStrictByteString <$> encodeTextFields runtimeOnly schema entry values


-- Descriptor options are serialized as messages by protoc. Its map entries
-- contain both members, even when one was absent from the option literal.
completeMapEntry
  :: TextFormatSchema -> Text -> AST.ScalarType -> AST.FieldType -> [TextField] -> Either String [TextField]
completeMapEntry schema scope keyType valueType fields = do
  withKey <- addDefault "key" (Right (scalarDefault keyType)) fields
  addDefault "value" valueDefault withKey
  where
    addDefault name value entries
      | any ((== name) . tfName) entries = Right entries
      | otherwise = (\v -> entries <> [TextField name v]) <$> value
    valueDefault = case valueType of
      AST.FTScalar scalar -> Right (scalarDefault scalar)
      AST.FTNamed name -> case resolveTextName schema scope name of
        Just qualified
          | Map.member qualified (textEnums schema) -> Right (TVInteger 0)
          | otherwise -> Right (TVMessage [])
        Nothing -> Left ("Unknown TextFormat map value type " <> T.unpack name)
    scalarDefault = \case
      AST.SString -> TVString ""
      AST.SBytes -> TVBytes BS.empty
      _ -> TVInteger 0


canonicalOptionWire :: TextMessageSchema -> BB.Builder -> Either String BB.Builder
canonicalOptionWire message wire = do
  fields <- first show (PD.decodeUnknownFields (BB.toStrictByteString wire))
  pure $ foldMap renderGroup (groupBy sameNumber (sortOn Ext.unknownFieldNumber fields))
  where
    sameNumber a b = Ext.unknownFieldNumber a == Ext.unknownFieldNumber b
    renderGroup [] = mempty
    renderGroup fields@(field : _)
      | let number = Ext.unknownFieldNumber field
      , Just (TextNamedField _ fieldSchema) <- IntMap.lookup number (textFieldsByNumber message)
      = if implicitDefault fieldSchema field
          then mempty
          else case traverse packedValue fields of
            Just payload | textFieldPacked fieldSchema ->
              PE.encodeFieldBytes number (BB.toStrictByteString (mconcat payload))
            _ -> PD.encodeUnknownFields fields
      | otherwise = PD.encodeUnknownFields fields
    implicitDefault fieldSchema field =
      textFieldLabel fieldSchema == Nothing && textFieldOneof fieldSchema == Nothing
        && case (textFieldType fieldSchema, field) of
          (TextOrdinaryField _, PD.UnknownVarint _ 0) -> True
          (TextOrdinaryField _, PD.UnknownFixed32 _ 0) -> True
          (TextOrdinaryField _, PD.UnknownFixed64 _ 0) -> True
          (TextOrdinaryField (AST.FTScalar _), PD.UnknownLenDelim _ bytes) -> BS.null bytes
          _ -> False
    packedValue = \case
      PD.UnknownVarint _ n -> Just (putVarint n)
      PD.UnknownFixed32 _ n -> Just (putFixed32 n)
      PD.UnknownFixed64 _ n -> Just (putFixed64 n)
      _ -> Nothing


encodeTextValue :: Bool -> TextFormatSchema -> Text -> Int -> AST.FieldType -> TextValue -> Either String BB.Builder
encodeTextValue runtimeOnly schema scope number fieldType value = case fieldType of
  AST.FTScalar scalar -> encodeTextScalar number scalar value
  AST.FTNamed name -> case resolveTextName schema scope name of
    Nothing -> Left ("Unknown TextFormat type " <> T.unpack name)
    Just qualified -> case Map.lookup qualified (textEnums schema) of
      Nothing -> do
        fields <- textMessageValue value
        PE.encodeFieldBytes number . BB.toStrictByteString <$> encodeTextMessage runtimeOnly schema qualified fields
      Just enumDef ->
        PE.encodeFieldVarint number . fromIntegral <$> textEnumValue enumDef value


textMessageValue :: TextValue -> Either String [TextField]
textMessageValue (TVMessage fields) = Right fields
textMessageValue _ = Left "Expected a message value"


textEnumValue :: TextEnumSchema -> TextValue -> Either String Int32
textEnumValue schema value = case value of
  TVIdent name -> byName name
  TVBool flag -> byName (if flag then "true" else "false")
  _ -> do
    number <- textIntegral value
    if textEnumClosed schema && Map.notMember number (textEnumNumbers schema)
      then Left "Unknown number for a closed enum"
      else Right number
  where
    byName name = maybe (Left "Unknown TextFormat enum name") Right (Map.lookup name (textEnumNames schema))


encodeTextScalar :: Int -> AST.ScalarType -> TextValue -> Either String BB.Builder
encodeTextScalar number scalar value = case scalar of
  AST.SDouble -> PE.encodeFieldDouble number <$> textFloating value
  AST.SFloat -> PE.encodeFieldFloat number <$> textFloating value
  AST.SInt32 -> varint <$> (textIntegral value :: Either String Int32)
  AST.SInt64 -> varint <$> (textIntegral value :: Either String Int64)
  AST.SUInt32 -> varint <$> (textIntegral value :: Either String Word32)
  AST.SUInt64 -> PE.encodeFieldVarint number <$> textIntegral value
  AST.SSInt32 -> PE.encodeFieldSVarint32 number <$> textIntegral value
  AST.SSInt64 -> PE.encodeFieldSVarint64 number <$> textIntegral value
  AST.SFixed32 -> PE.encodeFieldFixed32 number <$> textIntegral value
  AST.SFixed64 -> PE.encodeFieldFixed64 number <$> textIntegral value
  AST.SSFixed32 -> PE.encodeFieldFixed32 number . fromIntegral <$> (textIntegral value :: Either String Int32)
  AST.SSFixed64 -> PE.encodeFieldFixed64 number . fromIntegral <$> (textIntegral value :: Either String Int64)
  AST.SBool -> PE.encodeFieldBool number <$> textBool value
  AST.SString -> case value of
    TVString text -> Right (PE.encodeFieldString number text)
    _ -> Left "Expected a valid UTF-8 string"
  AST.SBytes -> case value of
    TVString text -> Right (PE.encodeFieldBytes number (TE.encodeUtf8 text))
    TVBytes bytes -> Right (PE.encodeFieldBytes number bytes)
    _ -> Left "Expected a quoted byte string"
  where
    varint :: Integral i => i -> BB.Builder
    varint = PE.encodeFieldVarint number . fromIntegral


textIntegral :: forall a. (Bounded a, Integral a) => TextValue -> Either String a
textIntegral (TVExactInteger base negative digits)
  | negative && lo == 0 = Left "Unsigned field requires an unsigned integer literal"
  | otherwise = parseInteger base (const True) digits >>= checkedIntegral . applySign negative
  where
    lo = toInteger (minBound :: a)
textIntegral (TVInteger number) = checkedIntegral number
textIntegral _ = Left "Expected an integer value"


checkedIntegral :: forall a. (Bounded a, Integral a) => Integer -> Either String a
checkedIntegral number
  | number < toInteger (minBound :: a) || number > toInteger (maxBound :: a) =
      Left "TextFormat integer is out of range"
  | otherwise = Right (fromInteger number)


textBool :: TextValue -> Either String Bool
textBool (TVBool flag) = Right flag
textBool (TVIdent name)
  | name == "True" || name == "t" = Right True
  | name == "False" || name == "f" = Right False
textBool value = case textIntegral value :: Either String Word64 of
  Right 0 -> Right False
  Right 1 -> Right True
  _ -> Left "Expected a boolean value"


textFloating :: RealFloat a => TextValue -> Either String a
textFloating (TVExactFloat negative value) = Right (applySign negative (toRealFloat value))
textFloating (TVInteger value) = Right (fromInteger value)
textFloating (TVNumber value)
  | isNaN value = Right (0 / 0)
  | isInfinite value = Right (if value < 0 then -1 / 0 else 1 / 0)
  | isNegativeZero value = Right (negate 0)
  | otherwise = Right (realToFrac value)
textFloating (TVExactInteger 10 negative digits) =
  case Number.decimalMagnitude digits T.empty T.empty of
    Just value -> Right (applySign negative (toRealFloat value))
    Nothing -> Left "Invalid decimal literal"
textFloating (TVIdent name) = case T.toLower name of
  "inf" -> Right (1 / 0)
  "infinity" -> Right (1 / 0)
  "-inf" -> Right (-1 / 0)
  "-infinity" -> Right (-1 / 0)
  "nan" -> Right (0 / 0)
  "-nan" -> Right (negate (0 / 0))
  _ -> Left "Expected a floating-point value"
textFloating _ = Left "Expected a decimal or floating-point value"


fieldsToDynamic :: [TextField] -> Either String DynamicMessage
fieldsToDynamic fields = do
  entries <- traverse entry fields
  let collected = foldl' add Map.empty entries
  pure (DynamicMessage (Map.map inOrder collected) [])
  where
    entry field = do
      number <- case parseInteger 10 isDigit (tfName field) of
        Right n | n > 0 && n <= 536870911 -> Right (fromInteger n)
        _ -> Left "Expected a numeric protobuf field name in 1..536870911"
      values <- case tfValue field of
        TVList items -> traverse textValueToDyn items
        value -> (: []) <$> textValueToDyn value
      pure (number, reverse values)
    add fieldsByNumber (_, []) = fieldsByNumber
    add fieldsByNumber (number, values) = Map.insertWith (++) number values fieldsByNumber
    inOrder [value] = value
    inOrder values = DynRepeated (reverse values)


textValueToDyn :: TextValue -> Either String DynamicValue
textValueToDyn = \case
  TVString s -> Right (DynString s)
  TVBytes bs -> Right (DynBytes bs)
  TVNumber n -> Right (DynDouble n)
  TVInteger n
    | n >= toInteger (minBound :: Int64), n <= toInteger (maxBound :: Word64) ->
        Right (DynVarint (fromIntegral n))
    | otherwise -> Left "TextFormat integer outside the supported 64-bit range"
  TVBool b -> Right (DynBool b)
  TVIdent t -> Right (DynString t)
  TVMessage fields -> DynMessage <$> fieldsToDynamic fields
  TVList _ -> Left "Nested TextFormat lists are invalid"


parseFields :: Bool -> Int -> Maybe Char -> Text -> Either String ([TextField], Text)
parseFields exactNumbers budget closing = go []
  where
    go fields input =
      case T.uncons (skipSpace input) of
        Nothing | Nothing <- closing -> Right (reverse fields, T.empty)
        Nothing -> Left "Unterminated TextFormat message"
        Just (c, rest) | Just c == closing -> Right (reverse fields, rest)
        Just _ -> do
          (field, rest) <- parseField exactNumbers budget (skipSpace input)
          let remaining = case T.uncons (skipSpace rest) of
                Just (c, after) | c == ';' || c == ',' -> after
                _ -> rest
          go (field : fields) remaining


parseField :: Bool -> Int -> Text -> Either String (TextField, Text)
parseField exactNumbers budget input = do
  (name, rest) <- parseFieldName input
  let (hasColon, afterName) = case T.stripPrefix ":" (skipSpace rest) of
        Just suffix -> (True, suffix)
        Nothing -> (False, rest)
  (value, remaining) <- parseTextValue exactNumbers budget (skipSpace afterName)
  case value of
    TVList [] | exactNumbers && not hasColon -> Right (TextField name TVEmptyMessageList, remaining)
    _ | hasColon || messageValue value -> Right (TextField name value, remaining)
      | otherwise -> Left "A TextFormat scalar field requires ':'"
  where
    messageValue (TVMessage _) = True
    messageValue (TVList items) = all isMessage items
    messageValue _ = False
    isMessage (TVMessage _) = True
    isMessage _ = False


parseFieldName :: Text -> Either String (Text, Text)
parseFieldName input = case T.stripPrefix "[" input of
  Just rest -> bracketName mempty rest
  Nothing ->
    let (name, rest) = T.span isNameChar input
    in case T.uncons name of
         Just (c, _) | not (isDigit c) || T.all isDigit name -> Right (name, rest)
         _ -> Left "Expected TextFormat field name"
  where
    bracketName name input' =
      case T.uncons (skipSpace input') of
        Just (']', rest) ->
          let qualifiedName = TL.toStrict (TB.toLazyText name)
          in if T.null qualifiedName
               then Left "Empty TextFormat extension or Any name"
               else Right ("[" <> qualifiedName <> "]", rest)
        Just _ ->
          let (part, rest) = T.span isBracketNameChar (skipSpace input')
          in if T.null part
               then Left "Invalid TextFormat extension or Any name"
               else bracketName (name <> TB.fromText part) rest
        Nothing -> Left "Unterminated TextFormat extension or Any name"


isBracketNameChar :: Char -> Bool
isBracketNameChar c = isNameChar c || elem c (".:/%-~!$&()*+,;=" :: String)


isNameChar :: Char -> Bool
isNameChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_'


skipSpace :: Text -> Text
skipSpace input =
  let rest = T.dropWhile isTextSpace input
  in case T.stripPrefix "#" rest of
       Just comment -> skipSpace (T.dropWhile (/= '\n') comment)
       Nothing -> rest


isTextSpace :: Char -> Bool
isTextSpace c = c == ' ' || (c >= '\t' && c <= '\r')


parseTextValue :: Bool -> Int -> Text -> Either String (TextValue, Text)
parseTextValue exactNumbers budget input = case T.uncons input of
  Just ('{', rest) -> message '}' rest
  Just ('<', rest) -> message '>' rest
  Just ('[', rest) -> parseTextList exactNumbers budget (skipSpace rest)
  Just (quote, _) | quote == '"' || quote == '\'' -> parseTextStrings input
  _ -> parseTextAtom exactNumbers input
  where
    message close rest
      | budget <= 0 = Left "TextFormat message nesting exceeds 100 levels"
      | otherwise = do
          (fields, remaining) <- parseFields exactNumbers (budget - 1) (Just close) rest
          Right (TVMessage fields, remaining)


parseTextList :: Bool -> Int -> Text -> Either String (TextValue, Text)
parseTextList exactNumbers budget input = case T.stripPrefix "]" input of
  Just rest -> Right (TVList [], rest)
  Nothing -> items [] input
  where
    items values rest
      | T.isPrefixOf "[" rest = Left "Nested TextFormat lists are invalid"
      | otherwise = do
          (value, suffix) <- parseTextValue exactNumbers budget rest
          case T.uncons (skipSpace suffix) of
            Just (']', remaining)
              | all isMessage (value : values) || not (any isMessage (value : values)) ->
                  Right (TVList (reverse (value : values)), remaining)
              | otherwise -> Left "TextFormat lists cannot mix scalar and message values"
            Just (',', remaining) -> items (value : values) (skipSpace remaining)
            _ -> Left "Expected ',' or ']' in TextFormat list"
    isMessage (TVMessage _) = True
    isMessage _ = False


parseTextAtom :: Bool -> Text -> Either String (TextValue, Text)
parseTextAtom exactNumbers input =
  let (negative, body) = case T.stripPrefix "-" input of
        Just suffix -> (True, skipSpace suffix)
        Nothing -> (False, input)
      (token, rest) = T.break delimiter body
      delimiter c = isTextSpace c || elem c ("#,:;[]{}<>\"'" :: String)
  in case T.uncons token of
       Nothing -> Left "Expected TextFormat value"
       Just (c, _)
         | isDigit c || c == '.' -> do
             number <- parseTextNumber exactNumbers negative token
             Right (number, rest)
         | isIdentifier token ->
             let value = case (negative, token) of
                   (True, _) -> TVIdent ("-" <> token)
                   (False, "true") -> TVBool True
                   (False, "false") -> TVBool False
                   _ -> TVIdent token
             in Right (value, rest)
         | otherwise -> Left "Invalid TextFormat value"
  where
    isIdentifier t = case T.uncons t of
      Just (c, tailText) -> isNameChar c && not (isDigit c) && T.all isNameChar tailText
      Nothing -> False


parseTextStrings :: Text -> Either String (TextValue, Text)
parseTextStrings = go mempty
  where
    go bytes input = case T.uncons input of
      Just (quote, rest) | quote == '"' || quote == '\'' -> do
        (chunk, remaining) <- parseQuoted quote rest
        go (bytes <> chunk) (skipSpace remaining)
      _ ->
        let raw = BB.toStrictByteString bytes
            value = either (const (TVBytes raw)) TVString (TE.decodeUtf8' raw)
        in Right (value, input)


parseQuoted :: Char -> Text -> Either String (BB.Builder, Text)
parseQuoted quote = go mempty
  where
    go bytes input =
      let (plain, rest) = T.break special input
          more = bytes <> BB.byteString (TE.encodeUtf8 plain)
      in case T.uncons rest of
           Just (c, remaining)
             | c == quote -> Right (more, remaining)
             | c == '\\' -> do
                 (chunk, suffix) <- parseEscape remaining
                 go (more <> chunk) suffix
             | otherwise -> Left "Unescaped line break or NUL in TextFormat string"
           Nothing -> Left "Unterminated TextFormat string"
    special c = c == quote || elem c ("\\\n\r\0" :: String)


parseEscape :: Text -> Either String (BB.Builder, Text)
parseEscape input = case T.uncons input of
  Just (c, rest) -> case c of
    'a' -> byte 7 rest
    'b' -> byte 8 rest
    'f' -> byte 12 rest
    'n' -> byte 10 rest
    'r' -> byte 13 rest
    't' -> byte 9 rest
    'v' -> byte 11 rest
    '\\' -> byte 92 rest
    '?' -> byte 63 rest
    '\'' -> byte 39 rest
    '"' -> byte 34 rest
    'x' -> hexByte rest
    'X' -> hexByte rest
    'u' -> unicode 4 rest
    'U' -> unicode 8 rest
    _ | isOctDigit c ->
          let (digits, remaining) = T.splitAt (T.length (T.takeWhile isOctDigit (T.take 3 input))) input
              n = T.foldl' (\acc digit -> acc * 8 + digitToInt digit) 0 digits
          in if n <= 255 then byte (fromIntegral n) remaining else Left "Octal TextFormat escape exceeds one byte"
      | otherwise -> Left "Unknown TextFormat escape"
  Nothing -> Left "Unterminated TextFormat escape"
  where
    byte n rest = Right (BB.word8 n, rest)
    hexByte text =
      let (digits, remaining) = T.splitAt (T.length (T.takeWhile isHexDigit (T.take 2 text))) text
      in if T.null digits
           then Left "Expected hexadecimal digits after TextFormat escape"
           else byte (fromIntegral (hexDigits digits)) remaining
    unicode width text =
      let (digits, rest) = T.splitAt width text
      in if T.length digits /= width || not (T.all isHexDigit digits)
           then Left "Invalid Unicode TextFormat escape"
           else
             let n = hexDigits digits
             in if n > 0x10ffff || (n >= 0xd800 && n <= 0xdfff)
                  then Left "Invalid Unicode scalar in TextFormat escape"
                  else Right (BB.byteString (TE.encodeUtf8 (T.singleton (toEnum n))), rest)
    hexDigits = T.foldl' (\acc digit -> acc * 16 + digitToInt digit) (0 :: Int)


parseTextNumber :: Bool -> Bool -> Text -> Either String TextValue
parseTextNumber exactNumbers negative body =
  let lower = T.toLower body
      integer base valid digits
        | exactNumbers =
            if T.null digits || not (T.all valid digits)
              then Left "Invalid TextFormat integer"
              else Right (TVExactInteger base negative digits)
        | otherwise = TVInteger . applySign negative <$> parseInteger base valid digits
      floating magnitude
        | exactNumbers = TVExactFloat negative magnitude
        | otherwise = TVNumber (applySign negative (toRealFloat magnitude))
  in case T.stripPrefix "0x" lower of
       Just digits -> integer 16 isHexDigit digits
       Nothing
         | T.any (\c -> c == '.' || c == 'e' || c == 'f') lower ->
             floating <$> parseFloat lower
         | T.isPrefixOf "0" body && T.compareLength body 1 == GT -> integer 8 isOctDigit body
         | otherwise -> integer 10 isDigit body


parseInteger :: Integer -> (Char -> Bool) -> Text -> Either String Integer
parseInteger base valid digits
  | T.null digits || not (T.all valid digits) = Left "Invalid TextFormat integer"
  -- No TextFormat integer type needs more than 22 significant digits
  -- in any supported base. Bound the conversion before allocating a bignum.
  | T.compareLength significant 22 == GT = Left "TextFormat integer exceeds 64-bit range"
  | otherwise = Right (T.foldl' (\n c -> n * base + toInteger (digitToInt c)) 0 significant)
  where
    significant = T.dropWhile (== '0') digits


applySign :: Num a => Bool -> a -> a
applySign negative = if negative then negate else id


parseFloat :: Text -> Either String Scientific
parseFloat lower = do
  let withoutSuffix = maybe lower id (T.stripSuffix "f" lower)
      (whole, afterWhole) = T.span isDigit withoutSuffix
      (fraction, afterFraction) = case T.stripPrefix "." afterWhole of
        Just rest -> T.span isDigit rest
        Nothing -> (T.empty, afterWhole)
  case T.uncons whole of
    Nothing | T.null fraction -> Left invalid
    Just ('0', rest) | not (T.null rest) -> Left invalid
    _ -> maybe (Left invalid) Right (Number.decimalMagnitude whole fraction afterFraction)
  where
    invalid = "Invalid TextFormat float"


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
