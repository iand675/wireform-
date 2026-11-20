{- | Convert wireform's AST types to descriptor.proto types and back.

This enables bundling schema metadata with generated types:
the parsed 'ProtoFile' is converted to a 'FileDescriptorProto',
serialized, and embedded in the generated code.

The reverse direction ('fileDescriptorToAST') is used by the protoc
plugin, which receives 'FileDescriptorProto' from protoc and needs
wireform's 'ProtoFile' to drive code generation.
-}
module Proto.IDL.Descriptor (
  astToFileDescriptor,
  astToDescriptor,
  astToFieldDescriptor,
  astToEnumDescriptor,
  astToServiceDescriptor,
  descriptorTypeDefinitions,
  serializeFileDescriptor,
  serializeFileDescriptorWithTypes,
  serializeFileDescriptorWithImports,
  resolveDescriptorName,
  resolveProtoName,
  resolveFieldPresence,
  enumIsClosed,
  descriptorFieldHasPresence,
  descriptorDefaultConstant,
  fileDescriptorToAST,
  descriptorToMessage,
  fieldDescriptorToField,
  enumDescriptorToEnum,
  serviceDescriptorToService,
) where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Char (toUpper)
import Data.Int (Int32)
import Data.List (mapAccumL, partition)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)
import GHC.Float (double2Float)
import Proto.Decode (UnknownField (..), decodeAllVarints, decodeUnknownFields, encodeUnknownFields)
import Proto.Encode (encodeMessage)
import Proto.Google.Protobuf.Descriptor
import Proto.IDL.AST
import Proto.IDL.Annotations (lookupSimpleOption, optionAsBool, optionAsString)
import Proto.IDL.Inspect (messageOptions)
import Proto.IDL.Parser.Lexer (boolLiteral, bytesLiteral, floatLiteral, intLiteral)
import Proto.IDL.Parser.Resolver (importClosure)
import Proto.IDL.Print (escapeProtoBytes, printConstant, printProtoFloat)
import Proto.Internal.FieldPresence (enumIsClosed, featureOption, resolveFieldPresence)
import Proto.TextFormat (TextFormatSchema, buildOptionTextFormatSchema, encodeRuntimeOptions)
import Text.Megaparsec qualified as MP
import Wireform.Builder qualified as Builder


-- | Convert a parsed ProtoFile to a FileDescriptorProto.
astToFileDescriptor :: FilePath -> ProtoFile -> FileDescriptorProto
astToFileDescriptor = astToFileDescriptorWithTypes Map.empty


astToFileDescriptorWithTypes :: Map.Map Text Int32 -> FilePath -> ProtoFile -> FileDescriptorProto
astToFileDescriptorWithTypes importedTypes path pf =
  defaultFileDescriptorProto
    { fdpName = T.pack path
    , fdpPackage = fromMaybe "" (protoPackage pf)
    , fdpDependency = V.fromList (map importPath typeImports)
    , fdpMessageType = V.fromList (concatMap topMessages (protoTopLevels pf))
    , fdpEnumType = V.fromList (concatMap topEnums (protoTopLevels pf))
    , fdpService = V.fromList (concatMap topServices (protoTopLevels pf))
    , fdpExtension = V.fromList (concatMap topExtensions (protoTopLevels pf))
    , fdpOptions = fileOptionsDescriptor (protoOptions pf)
    , fdpSyntax = syntaxStr (protoSyntax pf)
    , fdpEdition = editionStr (protoSyntax pf)
    , fdpUnknownFields =
        mapMaybe importIndexField (zip [0 ..] typeImports)
          <> map (UnknownLenDelim 15 . TE.encodeUtf8 . importPath) optionImports
    }
  where
    (optionImports, typeImports) = partition ((== Just ImportOption) . importModifier) (protoImports pf)
    pkg = fromMaybe "" (protoPackage pf)
    definitions = Map.union (descriptorTypeDefinitions pf) importedTypes
    topMessages (TLMessage msg) = [astToDescriptorIn (protoSyntax pf) definitions pkg msg]
    topMessages _ = []
    topEnums (TLEnum ed) = [astToEnumDescriptor ed]
    topEnums _ = []
    topServices (TLService svc) = [astToServiceDescriptorWith resolveRpcType svc]
    topServices _ = []
    resolveRpcType name = case resolveDescriptorName definitions pkg name of
      Just (qualified, 11) -> "." <> qualified
      _ -> name
    topExtensions (TLExtend owner fields) = fmap (extensionDescriptor definitions pkg owner) fields
    topExtensions _ = []
    syntaxStr Proto2 = "proto2"
    syntaxStr Proto3 = "proto3"
    syntaxStr (Editions _) = "editions"
    editionStr (Editions ed) = editionName ed
    editionStr _ = ""


-- The bootstrap descriptor preserves dependency index lists as unknown fields.
importIndexField :: (Word64, ImportDef) -> Maybe UnknownField
importIndexField (index, dependency) =
  case importModifier dependency of
    Just ImportPublic -> Just (UnknownVarint 10 index)
    Just ImportWeak -> Just (UnknownVarint 11 index)
    Just ImportOption -> Nothing
    Nothing -> Nothing


descriptorImports :: FileDescriptorProto -> [ImportDef]
descriptorImports file =
  V.toList (V.imap importAt (fdpDependency file))
    <> mapMaybe optionImport (fdpUnknownFields file)
  where
    importAt index path = ImportDef () (Map.lookup (fromIntegral index) modifiers) path
    optionImport (UnknownLenDelim 15 bytes) =
      ImportDef () (Just ImportOption) <$> either (const Nothing) Just (TE.decodeUtf8' bytes)
    optionImport _ = Nothing
    modifiers = Map.fromList (concatMap importIndexes (fdpUnknownFields file))
    importIndexes (UnknownVarint 10 index) = [(index, ImportPublic)]
    importIndexes (UnknownVarint 11 index) = [(index, ImportWeak)]
    importIndexes (UnknownLenDelim 10 bytes) = packedIndexes ImportPublic bytes
    importIndexes (UnknownLenDelim 11 bytes) = packedIndexes ImportWeak bytes
    importIndexes _ = []
    packedIndexes modifier = either (const []) (map (\index -> (index, modifier)) . VU.toList) . decodeAllVarints


-- | Fully qualified message and enum types supplied by one proto file.
descriptorTypeDefinitions :: ProtoFile -> Map.Map Text Int32
descriptorTypeDefinitions pf = Map.fromList (concatMap topDefinitions (protoTopLevels pf))
  where
    pkg = fromMaybe "" (protoPackage pf)
    topDefinitions (TLMessage msg) = messageDefinitions pkg msg
    topDefinitions (TLEnum ed) = [(scopedName pkg (enumName ed), 14)]
    topDefinitions _ = []


-- | Convert a MessageDef to a DescriptorProto.
astToDescriptor :: MessageDef -> DescriptorProto
astToDescriptor msg = astToDescriptorIn Proto2 (Map.fromList (messageDefinitions "" msg)) "" msg


messageDefinitions :: Text -> MessageDef -> [(Text, Int32)]
messageDefinitions parent msg = (scope, 11) : concatMap nested (msgElements msg)
  where
    scope = scopedName parent (msgName msg)
    nested (MEMessage child) = messageDefinitions scope child
    nested (MEEnum ed) = [(scopedName scope (enumName ed), 14)]
    nested _ = []


astToDescriptorIn :: Syntax -> Map.Map Text Int32 -> Text -> MessageDef -> DescriptorProto
astToDescriptorIn syntax definitions parent msg =
  defaultDescriptorProto
    { dpName = msgName msg
    , dpField = V.fromList fieldDescriptors
    , dpNestedType = V.fromList (concatMap extractNested (msgElements msg))
    , dpEnumType = V.fromList (concatMap extractEnum (msgElements msg))
    , dpOneofDecl = V.fromList (realOneofs <> syntheticOneofs)
    , dpExtensionRange = V.fromList (concatMap extractRanges (msgElements msg))
    , dpExtension = V.fromList (concatMap extractExtensions (msgElements msg))
    }
  where
    scope = scopedName parent (msgName msg)
    realOneofs = concatMap extractOneof (msgElements msg)
    (fieldDescriptors, syntheticOneofs) = addSyntheticOneofs realOneofs (fields 0 (msgElements msg))
    fields _ [] = []
    fields index (element : rest) = case element of
      MEField fd ->
        (scopedFieldDescriptor definitions scope fd)
          {fdpFieldProto3Optional = syntax == Proto3 && fieldLabel fd == Just Optional}
          : fields index rest
      MEMapField mf -> mapToFieldDescriptor scope mf : fields index rest
      MEOneof od ->
        fmap (scopeDescriptorType definitions scope . oneofFieldToFDP index) (oneofFields od) <> fields (index + 1) rest
      _ -> fields index rest

    extractNested (MEMessage inner) = [astToDescriptorIn syntax definitions scope inner]
    extractNested (MEMapField mf) = [mapEntryDescriptor definitions scope mf]
    extractNested _ = []

    extractEnum (MEEnum ed) = [astToEnumDescriptor ed]
    extractEnum _ = []

    extractOneof (MEOneof od) = [defaultOneofDescriptorProto {odpName = oneofName od}]
    extractOneof _ = []
    extractRanges (MEExtensions ranges _) = fmap extensionRangeDescriptor ranges
    extractRanges _ = []
    extractExtensions (MEExtend owner extensionFields) = fmap (extensionDescriptor definitions scope owner) extensionFields
    extractExtensions _ = []


addSyntheticOneofs :: [OneofDescriptorProto] -> [FieldDescriptorProto] -> ([FieldDescriptorProto], [OneofDescriptorProto])
addSyntheticOneofs oneofs fields =
  let names = Set.fromList (map fdpFieldName fields <> map odpName oneofs)
      ((_, _, synthetic), updated) = mapAccumL addOneof (names, fromIntegral (length oneofs), []) fields
  in (updated, reverse synthetic)
  where
    addOneof state@(names, index, declarations) field
      | not (fdpFieldProto3Optional field) = (state, field)
      | otherwise =
          let name = fdpFieldName field
              start = if "_" `T.isPrefixOf` name then name else "_" <> name
              oneof = freshName names start
              declaration = defaultOneofDescriptorProto {odpName = oneof}
          in ((Set.insert oneof names, index + 1, declaration : declarations), field {fdpFieldOneofIdx = index})
    freshName names name
      | Set.member name names = freshName names ("X" <> name)
      | otherwise = name


-- | Convert a FieldDef to a FieldDescriptorProto.
astToFieldDescriptor :: FieldDef -> FieldDescriptorProto
astToFieldDescriptor fd =
  defaultFieldDescriptorProto
    { fdpFieldName = fieldName fd
    , fdpFieldNumber = fromIntegral (unFieldNumber (fieldNumber fd))
    , fdpFieldLabel = labelToInt (fieldLabel fd)
    , fdpFieldType = if isLegacyGroupField fd then 10 else fieldTypeToInt (fieldType fd)
    , fdpFieldTypeName = case fieldType fd of
        FTNamed n -> n
        _ -> ""
    , fdpFieldDefault = fromMaybe "" defaultValue
    , fdpFieldHasEmptyDefault = defaultValue == Just ""
    , fdpFieldJsonName = fromMaybe "" jsonName
    , fdpFieldHasEmptyJsonName = jsonName == Just ""
    , fdpFieldOptions = fieldOptionsDescriptor options
    }
  where
    options = fieldOptions fd
    defaultValue = defaultValueText (fieldType fd) <$> lookupSimpleOption "default" options
    jsonName = explicitJsonName options


explicitJsonName :: [OptionDef] -> Maybe Text
explicitJsonName options = lookupSimpleOption "json_name" options >>= optionAsString


defaultValueText :: FieldType -> Constant -> Text
defaultValueText fieldType value = case (fieldType, value) of
  (FTScalar SBytes, CBytes bytes) -> escapeProtoBytes bytes
  (FTScalar SBytes, CString text) -> escapeProtoBytes (TE.encodeUtf8 text)
  (FTScalar SString, CString text) -> text
  (FTScalar SFloat, CFloat n) -> floatingDefaultText (double2Float n)
  (FTScalar SFloat, CInt n) -> floatingDefaultText (fromInteger n :: Float)
  (FTScalar SDouble, CFloat n) -> floatingDefaultText n
  (FTScalar SDouble, CInt n) -> floatingDefaultText (fromInteger n :: Double)
  (_, CInt n) -> printConstant (CInt n)
  _ -> printConstant value


floatingDefaultText :: RealFloat a => a -> Text
floatingDefaultText number =
  let text = printProtoFloat number
  in fromMaybe text (T.stripSuffix ".0" text)


scopedName :: Text -> Text -> Text
scopedName scope name = if T.null scope then name else scope <> "." <> name


scopedFieldDescriptor :: Map.Map Text Int32 -> Text -> FieldDef -> FieldDescriptorProto
scopedFieldDescriptor definitions scope = scopeDescriptorType definitions scope . astToFieldDescriptor


scopeDescriptorType :: Map.Map Text Int32 -> Text -> FieldDescriptorProto -> FieldDescriptorProto
scopeDescriptorType definitions scope field
  | not (T.null name)
  , Just (qualified, typeNumber) <- resolveDescriptorName definitions scope name =
      field
        { fdpFieldTypeName = "." <> qualified
        , fdpFieldType = if fdpFieldType field == 10 then 10 else typeNumber
        }
  | otherwise = field
  where
    name = fdpFieldTypeName field


extensionDescriptor :: Map.Map Text Int32 -> Text -> Text -> FieldDef -> FieldDescriptorProto
extensionDescriptor definitions scope owner field =
  (scopedFieldDescriptor definitions scope field) {fdpFieldExtendee = absoluteOwner}
  where
    absoluteOwner = "." <> maybe (T.dropWhile (== '.') owner) fst (resolveDescriptorName definitions scope owner)


-- | Resolve a protobuf name against qualified definitions and an enclosing scope.
resolveDescriptorName :: Map.Map Text a -> Text -> Text -> Maybe (Text, a)
resolveDescriptorName definitions scope name = do
  qualified <- resolveProtoName (`Map.member` definitions) scope name
  definition <- Map.lookup qualified definitions
  pure (qualified, definition)


-- | Resolve a type name from its enclosing protobuf scope.
resolveProtoName :: (Text -> Bool) -> Text -> Text -> Maybe Text
resolveProtoName known scope name = case T.stripPrefix "." name of
  Just absolute -> match absolute
  Nothing -> resolve scope
  where
    match candidate
      | known candidate = Just candidate
      | otherwise = Nothing
    resolve prefix
      | Just found <- match (scopedName prefix name) = Just found
      | T.null prefix = Nothing
      | otherwise = resolve (T.dropEnd 1 (fst (T.breakOnEnd "." prefix)))


extensionRangeDescriptor :: ExtensionRange -> DescriptorProtoExtensionRange
extensionRangeDescriptor range =
  defaultDescriptorProtoExtensionRange
    { dperStart = fromIntegral (extStart range)
    , dperEnd = case extEnd range of
        ExtBoundNum end -> fromIntegral end + 1
        ExtBoundMax -> 536870912
    }


mapToFieldDescriptor :: Text -> MapField -> FieldDescriptorProto
mapToFieldDescriptor scope mf =
  defaultFieldDescriptorProto
    { fdpFieldName = mapFieldName mf
    , fdpFieldNumber = fromIntegral (unFieldNumber (mapFieldNum mf))
    , fdpFieldLabel = 3
    , fdpFieldType = 11
    , fdpFieldTypeName = "." <> scopedName scope (mapEntryName mf)
    , fdpFieldJsonName = fromMaybe "" jsonName
    , fdpFieldHasEmptyJsonName = jsonName == Just ""
    , fdpFieldOptions = fieldOptionsDescriptor (mapOptions mf)
    }
  where
    jsonName = explicitJsonName (mapOptions mf)


mapEntryName :: MapField -> Text
mapEntryName mf = foldMap capitalize (T.splitOn "_" (mapFieldName mf)) <> "Entry"
  where
    capitalize part = case T.uncons part of
      Just (first, rest) -> T.cons (toUpper first) rest
      Nothing -> ""


mapEntryDescriptor :: Map.Map Text Int32 -> Text -> MapField -> DescriptorProto
mapEntryDescriptor definitions scope mf =
  defaultDescriptorProto
    { dpName = mapEntryName mf
    , dpField = V.fromList [entryField "key" 1 (FTScalar (mapKeyType mf)), entryField "value" 2 (mapValueType mf)]
    , dpOptions = Just defaultMessageOptions {moMapEntry = True}
    }
  where
    entryField name number fieldType' =
      scopedFieldDescriptor definitions scope (FieldDef () Nothing Nothing fieldType' name (FieldNumber number) [])


oneofFieldToFDP :: Int32 -> OneofField -> FieldDescriptorProto
oneofFieldToFDP index field =
  (astToFieldDescriptor member) {fdpFieldOneofIdx = index}
  where
    member =
      FieldDef () (oneofFieldDoc field) Nothing (oneofFieldType field)
        (oneofFieldName field) (oneofFieldNumber field) (oneofFieldOptions field)


-- | Convert an EnumDef to an EnumDescriptorProto.
astToEnumDescriptor :: EnumDef -> EnumDescriptorProto
astToEnumDescriptor ed =
  defaultEnumDescriptorProto
    { edpName = enumName ed
    , edpValue = V.fromList (fmap toEVDP (enumValues ed))
    , edpOptions =
        if allowAlias
          then Just ((fromMaybe defaultEnumOptions features) {enoAllowAlias = True})
          else features
    }
  where
    allowAlias = (lookupSimpleOption "allow_alias" (enumOptions ed) >>= optionAsBool) == Just True
    features = withFeatureFields 7 (enumOptions ed) $ \fields ->
      defaultEnumOptions {enoUnknownFields = fields}
    toEVDP ev = defaultEnumValueDescriptorProto {evdpName = evName ev, evdpNumber = fromIntegral (evNumber ev)}


-- | Convert a ServiceDef to a ServiceDescriptorProto.
astToServiceDescriptor :: ServiceDef -> ServiceDescriptorProto
astToServiceDescriptor = astToServiceDescriptorWith id


astToServiceDescriptorWith :: (Text -> Text) -> ServiceDef -> ServiceDescriptorProto
astToServiceDescriptorWith resolveType svc =
  defaultServiceDescriptorProto
    { sdpName = svcName svc
    , sdpMethod = V.fromList (fmap toMDP (svcRpcs svc))
    }
  where
    toMDP rpc =
      defaultMethodDescriptorProto
        { mdpName = rpcName rpc
        , mdpInputType = resolveType (rpcInput rpc)
        , mdpOutputType = resolveType (rpcOutput rpc)
        , mdpClientStreaming = rpcInputStr rpc == Streaming
        , mdpServerStreaming = rpcOutputStr rpc == Streaming
        }


-- | Serialize a ProtoFile's schema to bytes (as a FileDescriptorProto).
serializeFileDescriptor :: FilePath -> ProtoFile -> ByteString
serializeFileDescriptor path pf = encodeMessage (astToFileDescriptor path pf)


-- | Serialize a schema with the fully qualified types from its imports.
serializeFileDescriptorWithTypes :: Map.Map Text Int32 -> FilePath -> ProtoFile -> ByteString
serializeFileDescriptorWithTypes definitions path pf =
  encodeMessage (astToFileDescriptorWithTypes definitions path pf)


-- | Serialize a source descriptor with custom options resolved against its
-- imports. The bootstrap descriptor keeps extension values as unknown fields.
serializeFileDescriptorWithImports
  :: Map.Map Text Int32 -> FilePath -> Map.Map Text ProtoFile -> ProtoFile -> Either Text ByteString
serializeFileDescriptorWithImports definitions path imports file =
  first T.pack $ encodeMessage <$> addCustomOptions file schema descriptor
  where
    visiblePaths = importClosure imports ((== Just ImportPublic) . importModifier) (map importPath (protoImports file))
    schema = buildOptionTextFormatSchema
      (Map.elems (Map.restrictKeys imports visiblePaths) <> [file])
      (Map.elems (Map.withoutKeys imports visiblePaths))
    descriptor = astToFileDescriptorWithTypes definitions path file


addCustomOptions
  :: ProtoFile -> Either String TextFormatSchema -> FileDescriptorProto
  -> Either String FileDescriptorProto
addCustomOptions file schema descriptor = do
  options <- append pkg "FileOptions" (protoOptions file) defaultFileOptions
    (\fields o -> o {foUnknownFields = foUnknownFields o <> fields}) (fdpOptions descriptor)
  messages <- V.zipWithM (addMessage pkg) (topLevel (\case TLMessage msg -> [msg]; _ -> [])) (fdpMessageType descriptor)
  enums <- V.zipWithM (addEnum pkg) (topLevel (\case TLEnum ed -> [ed]; _ -> [])) (fdpEnumType descriptor)
  services <- V.zipWithM addService (topLevel (\case TLService svc -> [svc]; _ -> [])) (fdpService descriptor)
  extensions <- V.zipWithM (addField pkg) (topLevel (\case TLExtend _ fields -> map fieldOptions fields; _ -> [])) (fdpExtension descriptor)
  pure descriptor {fdpOptions = options, fdpMessageType = messages, fdpEnumType = enums, fdpService = services, fdpExtension = extensions}
  where
    pkg = fromMaybe "" (protoPackage file)
    topLevel :: (TopLevel -> [a]) -> V.Vector a
    topLevel extract = V.fromList (concatMap extract (protoTopLevels file))
    append :: Text -> Text -> [OptionDef] -> a -> ([UnknownField] -> a -> a) -> Maybe a -> Either String (Maybe a)
    append scope target source defaults set old
      | not (any isCustom source) = Right old
      | otherwise = do
          resolved <- schema
          fields <- first (\err -> T.unpack scope <> ": " <> err) $
            encodeRuntimeOptions resolved scope ("google.protobuf." <> target) source
          pure $ if null fields then old else Just (set fields (fromMaybe defaults old))
    isCustom opt = case optNameParts (optName opt) of
      ExtensionOption _ : _ -> True
      _ -> False

    addMessage parent source msg = do
      let scope = scopedName parent (msgName source)
          elements :: (MessageElement -> [a]) -> V.Vector a
          elements extract = V.fromList (concatMap extract (msgElements source))
      options <- append scope "MessageOptions" (messageOptions source) defaultMessageOptions
        (\fields o -> o {moUnknownFields = moUnknownFields o <> fields}) (dpOptions msg)
      fields <- V.zipWithM (addField scope) (elements messageFieldOptions) (dpField msg)
      nested <- V.zipWithM (maybe pure (addMessage scope)) (elements childMessage) (dpNestedType msg)
      enums <- V.zipWithM (addEnum scope) (elements (\case MEEnum ed -> [ed]; _ -> [])) (dpEnumType msg)
      oneofs <- V.zipWithM (addOneof scope) (elements (\case MEOneof od -> [od]; _ -> [])) (dpOneofDecl msg)
      ranges <- V.zipWithM (addRange scope) (elements rangeOptions) (dpExtensionRange msg)
      extensions <- V.zipWithM (addField scope) (elements (\case MEExtend _ fs -> map fieldOptions fs; _ -> [])) (dpExtension msg)
      pure msg
        { dpOptions = options, dpField = fields, dpNestedType = nested, dpEnumType = enums
        , dpOneofDecl = oneofs <> V.drop (V.length oneofs) (dpOneofDecl msg)
        , dpExtensionRange = ranges, dpExtension = extensions
        }

    messageFieldOptions = \case
      MEField fd -> [fieldOptions fd]
      MEMapField mf -> [mapOptions mf]
      MEOneof od -> map oneofFieldOptions (oneofFields od)
      _ -> []
    childMessage = \case
      MEMessage child -> [Just child]
      MEMapField _ -> [Nothing]
      _ -> []
    rangeOptions = \case
      MEExtensions ranges opts -> map (const opts) ranges
      _ -> []

    addField scope source field = do
      opts <- append scope "FieldOptions" source defaultFieldOptions
        (\fields o -> o {fldoUnknownFields = fldoUnknownFields o <> fields}) (fdpFieldOptions field)
      pure field {fdpFieldOptions = opts}
    addOneof scope source oneof = do
      opts <- append scope "OneofOptions" (oneofOptions source) defaultOneofOptions
        (\fields o -> o {oneofoUnknownFields = oneofoUnknownFields o <> fields}) (odpOptions oneof)
      pure oneof {odpOptions = opts}
    addRange scope source range = do
      opts <- append scope "ExtensionRangeOptions" source defaultExtensionRangeOptions
        (\fields o -> o {eroUnknownFields = eroUnknownFields o <> fields}) (dperOptions range)
      pure range {dperOptions = opts}

    addEnum parent source enum = do
      let scope = scopedName parent (enumName source)
      opts <- append scope "EnumOptions" (enumOptions source) defaultEnumOptions
        (\fields o -> o {enoUnknownFields = enoUnknownFields o <> fields}) (edpOptions enum)
      values <- V.zipWithM (addEnumValue scope) (V.fromList (enumValues source)) (edpValue enum)
      pure enum {edpOptions = opts, edpValue = values}
    addEnumValue scope source val = do
      opts <- append scope "EnumValueOptions" (evOptions source) defaultEnumValueOptions
        (\fields o -> o {evoUnknownFields = evoUnknownFields o <> fields}) (evdpOptions val)
      pure val {evdpOptions = opts}

    addService source service = do
      let scope = scopedName pkg (svcName source)
      opts <- append scope "ServiceOptions" (svcOptions source) defaultServiceOptions
        (\fields o -> o {svoUnknownFields = svoUnknownFields o <> fields}) (sdpOptions service)
      methods <- V.zipWithM (addMethod scope) (V.fromList (svcRpcs source)) (sdpMethod service)
      pure service {sdpOptions = opts, sdpMethod = methods}
    addMethod scope source method = do
      opts <- append scope "MethodOptions" (rpcOptions source) defaultMethodOptions
        (\fields o -> o {mtoUnknownFields = mtoUnknownFields o <> fields}) (mdpOptions method)
      pure method {mdpOptions = opts}


labelToInt :: Maybe FieldLabel -> Int32
labelToInt Nothing = 1
labelToInt (Just Optional) = 1
labelToInt (Just Required) = 2
labelToInt (Just Repeated) = 3


fieldTypeToInt :: FieldType -> Int32
fieldTypeToInt (FTScalar s) = scalarTypeToInt s
fieldTypeToInt (FTNamed _) = 11


scalarTypeToInt :: ScalarType -> Int32
scalarTypeToInt = \case
  SDouble -> 1
  SFloat -> 2
  SInt64 -> 3
  SUInt64 -> 4
  SInt32 -> 5
  SFixed64 -> 6
  SFixed32 -> 7
  SBool -> 8
  SString -> 9
  SBytes -> 12
  SUInt32 -> 13
  SSFixed32 -> 15
  SSFixed64 -> 16
  SSInt32 -> 17
  SSInt64 -> 18


-- The bootstrap descriptor types retain FeatureSet as unknown wire fields.
-- The same table translates the feature options in both directions.
featureValues :: [(Int, Text, [(Word64, Text)])]
featureValues =
  [ (1, "field_presence", [(1, "EXPLICIT"), (2, "IMPLICIT"), (3, "LEGACY_REQUIRED")])
  , (2, "enum_type", [(1, "OPEN"), (2, "CLOSED")])
  , (3, "repeated_field_encoding", [(1, "PACKED"), (2, "EXPANDED")])
  , (4, "utf8_validation", [(2, "VERIFY"), (3, "NONE")])
  , (5, "message_encoding", [(1, "LENGTH_PREFIXED"), (2, "DELIMITED")])
  , (6, "json_format", [(1, "ALLOW"), (2, "LEGACY_BEST_EFFORT")])
  ]


fieldOptionsDescriptor :: [OptionDef] -> Maybe FieldOptions
fieldOptionsDescriptor opts = withFeatureFields 21 opts $ \fields ->
  defaultFieldOptions {fldoUnknownFields = fields}


-- FileOptions.csharp_namespace is retained in the bootstrap unknown fields.
-- Text generation uses it for the Haskell module name.
fileOptionsDescriptor :: [OptionDef] -> Maybe FileOptions
fileOptionsDescriptor opts = case namespaceFields <> featureFields 50 opts of
  [] -> Nothing
  fields -> Just defaultFileOptions {foUnknownFields = fields}
  where
    namespaceFields = maybeToList $ do
      name <- lookupSimpleOption "csharp_namespace" opts >>= optionAsString
      pure (UnknownLenDelim 37 (TE.encodeUtf8 name))


descriptorFileOptions :: FileOptions -> [OptionDef]
descriptorFileOptions options = namespaceOption <> descriptorFeatureOptions 50 fields
  where
    fields = foUnknownFields options
    namespaceOption = maybeToList $ do
      name <- listToMaybe (mapMaybe decodeNamespace (reverse fields))
      pure (OptionDef () (OptionName [SimpleOption "csharp_namespace"]) (CString name))
    decodeNamespace (UnknownLenDelim 37 bytes) = either (const Nothing) Just (TE.decodeUtf8' bytes)
    decodeNamespace _ = Nothing


withFeatureFields :: Int -> [OptionDef] -> ([UnknownField] -> a) -> Maybe a
withFeatureFields number options makeOptions = case featureFields number options of
  [] -> Nothing
  fields -> Just (makeOptions fields)


featureFields :: Int -> [OptionDef] -> [UnknownField]
featureFields number options = case mapMaybe encodeFeature featureValues of
  [] -> []
  features -> [UnknownLenDelim number (Builder.toStrictByteString (encodeUnknownFields features))]
  where
    encodeFeature (tag, name, values) = do
      CIdent ident <- featureOption name options
      value <- lookup ident (fmap (\(code, spelling) -> (spelling, code)) values)
      pure (UnknownVarint tag value)


descriptorFeatureOptions :: Int -> [UnknownField] -> [OptionDef]
descriptorFeatureOptions number fields = mapMaybe option featureValues
  where
    features = foldl' addPayload Map.empty fields
    addPayload acc (UnknownLenDelim tag payload)
      | tag == number, Right values <- decodeUnknownFields payload = foldl' addValue acc values
    addPayload acc _ = acc
    addValue acc (UnknownVarint tag value) = Map.insert tag value acc
    addValue acc _ = acc
    option (tag, name, values) = do
      code <- Map.lookup tag features
      value <- lookup code values
      pure (OptionDef () (OptionName [SimpleOption "features", SimpleOption name]) (CIdent value))


-- | Whether a descriptor field distinguishes an unset value from its default.
descriptorFieldHasPresence :: FileDescriptorProto -> FieldDescriptorProto -> Bool
descriptorFieldHasPresence file field
  | fdpFieldLabel field == 3 = False
  | fdpFieldOneofIdx field >= 0 || fdpFieldProto3Optional field = True
  | fdpFieldType field == 11 || fdpFieldType field == 10 = True
  | fdpSyntax file == "proto3" = False
  | fdpSyntax file == "editions" = presence /= Just (CIdent "IMPLICIT")
  | otherwise = True
  where
    presence =
      (fdpFieldOptions field >>= featureOption "field_presence" . descriptorFeatureOptions 21 . fldoUnknownFields)
        <|> (fdpOptions file >>= featureOption "field_presence" . descriptorFeatureOptions 50 . foUnknownFields)


-- ---------------------------------------------------------------------------
-- Reverse conversion: FileDescriptorProto -> ProtoFile
-- ---------------------------------------------------------------------------

-- | Convert a 'FileDescriptorProto' (as received from protoc) to a 'ProtoFile'.
fileDescriptorToAST :: FileDescriptorProto -> ProtoFile
fileDescriptorToAST fdp =
  ProtoFile
    { protoSyntax = parseSyntax (fdpSyntax fdp) (fdpEdition fdp)
    , protoPackage = if T.null (fdpPackage fdp) then Nothing else Just (fdpPackage fdp)
    , protoImports = descriptorImports fdp
    , protoOptions = maybe [] descriptorFileOptions (fdpOptions fdp)
    , protoTopLevels =
        fmap (TLMessage . descriptorToMessageIn syntax (fdpPackage fdp)) (V.toList (fdpMessageType fdp))
          <> fmap (TLEnum . enumDescriptorToEnum) (V.toList (fdpEnumType fdp))
          <> fmap (TLService . serviceDescriptorToService) (V.toList (fdpService fdp))
          <> fmap (uncurry TLExtend) (descriptorExtensions (fdpExtension fdp))
    , protoSource = Nothing
    }
  where
    syntax = parseSyntax (fdpSyntax fdp) (fdpEdition fdp)


parseSyntax :: Text -> Text -> Syntax
parseSyntax syn ed
  | syn == "proto2" || T.null syn = Proto2
  | syn == "editions" = Editions (Edition ed)
  | otherwise = Proto3


-- | Convert a 'DescriptorProto' to a 'MessageDef'.
descriptorToMessage :: DescriptorProto -> MessageDef
descriptorToMessage = descriptorToMessageIn Proto2 ""


descriptorToMessageIn :: Syntax -> Text -> DescriptorProto -> MessageDef
descriptorToMessageIn syntax parent dp =
  MessageDef
    { msgExt = ()
    , msgDoc = Nothing
    , msgName = dpName dp
    , msgElements = fieldElems <> nestedElems <> enumElems <> extensions
    }
  where
    scope = scopedName parent (dpName dp)
    allFields = V.toList (dpField dp)
    nested = V.toList (dpNestedType dp)
    isMapEntry = maybe False moMapEntry . dpOptions
    mapEntries = Map.fromList (fmap (\entry -> (dpName entry, entry)) (filter isMapEntry nested))
    fieldElems = mapMaybe fieldElement allFields
    nestedElems = fmap (MEMessage . descriptorToMessageIn syntax scope) (filter (not . isMapEntry) nested)
    enumElems = fmap (MEEnum . enumDescriptorToEnum) (V.toList (dpEnumType dp))
    oneofArms = foldl' addArm Map.empty allFields
    addArm grouped field
      | fdpFieldOneofIdx field >= 0 && not (fdpFieldProto3Optional field) =
          Map.insertWith (flip (<>)) (fdpFieldOneofIdx field) [field] grouped
      | otherwise = grouped
    fieldElement field
      | fdpFieldLabel field == 3
      , fdpFieldType field == 11
      , Just entry <- Map.lookup (T.takeWhileEnd (/= '.') (fdpFieldTypeName field)) mapEntries
      , fdpFieldTypeName field `elem` [dpName entry, "." <> scopedName scope (dpName entry)]
      , Just mapField <- descriptorMapField field entry = Just (MEMapField mapField)
      | fdpFieldOneofIdx field >= 0
      , not (fdpFieldProto3Optional field)
      , Just decl <- dpOneofDecl dp V.!? fromIntegral (fdpFieldOneofIdx field)
      , Just arms@(first : _) <- Map.lookup (fdpFieldOneofIdx field) oneofArms =
          if fdpFieldNumber first == fdpFieldNumber field
            then Just (MEOneof (descriptorOneof decl arms))
            else Nothing
      | otherwise = Just (MEField regular)
      where
        fieldDef = fieldDescriptorToField field
        regular
          | syntax == Proto3
          , fieldLabel fieldDef == Just Optional
          , not (fdpFieldProto3Optional field) = fieldDef {fieldLabel = Nothing}
          | otherwise = fieldDef
    extensions =
      let ranges = fmap toRange (V.toList (dpExtensionRange dp))
      in (if null ranges then [] else [MEExtensions ranges []])
           <> fmap (uncurry MEExtend) (descriptorExtensions (dpExtension dp))
    toRange range = ExtensionRange (fromIntegral (dperStart range))
      (if dperEnd range == 536870912 then ExtBoundMax else ExtBoundNum (fromIntegral (dperEnd range) - 1))


descriptorExtensions :: V.Vector FieldDescriptorProto -> [(Text, [FieldDef])]
descriptorExtensions =
  Map.toList . V.foldl' add Map.empty
  where
    add grouped field =
      Map.insertWith (flip (<>)) (fdpFieldExtendee field) [fieldDescriptorToField field] grouped


descriptorMapField :: FieldDescriptorProto -> DescriptorProto -> Maybe MapField
descriptorMapField field entry = do
  key <- V.find ((== 1) . fdpFieldNumber) (dpField entry)
  value <- V.find ((== 2) . fdpFieldNumber) (dpField entry)
  keyType <- intToScalar (fdpFieldType key)
  pure
    MapField
      { mapExt = ()
      , mapDoc = Nothing
      , mapKeyType = keyType
      , mapValueType = intToFieldType (fdpFieldType value) (fdpFieldTypeName value)
      , mapFieldName = fdpFieldName field
      , mapFieldNum = FieldNumber (fromIntegral (fdpFieldNumber field))
      , mapOptions = descriptorFieldOptions field
      }


descriptorOneof :: OneofDescriptorProto -> [FieldDescriptorProto] -> OneofDef
descriptorOneof decl fields =
  OneofDef
    { oneofExt = ()
    , oneofDoc = Nothing
    , oneofName = odpName decl
    , oneofFields = fmap mkOneofField fields
    , oneofOptions = []
    }
  where
    mkOneofField f =
      OneofField
        { oneofFieldExt = ()
        , oneofFieldDoc = Nothing
        , oneofFieldType = intToFieldType (fdpFieldType f) (fdpFieldTypeName f)
        , oneofFieldName = fdpFieldName f
        , oneofFieldNumber = FieldNumber (fromIntegral (fdpFieldNumber f))
        , oneofFieldOptions = descriptorFieldOptions f
        }


-- | Convert a 'FieldDescriptorProto' to a 'FieldDef'.
fieldDescriptorToField :: FieldDescriptorProto -> FieldDef
fieldDescriptorToField f =
  constructor () Nothing (intToLabel (fdpFieldLabel f)) fieldType (fdpFieldName f)
    (FieldNumber (fromIntegral (fdpFieldNumber f))) (descriptorFieldOptions f)
  where
    (constructor, fieldType)
      | fdpFieldType f == 10 = (GroupFieldDef, FTNamed (fdpFieldTypeName f))
      | otherwise = (FieldDef, intToFieldType (fdpFieldType f) (fdpFieldTypeName f))


descriptorFieldOptions :: FieldDescriptorProto -> [OptionDef]
descriptorFieldOptions field =
  jsonNameOption
    <> maybe [] (\value -> [simpleOption "default" value]) (descriptorDefaultConstant field)
    <> maybe [] (descriptorFeatureOptions 21 . fldoUnknownFields) (fdpFieldOptions field)
  where
    jsonName = fdpFieldJsonName field
    jsonNameOption
      | T.null jsonName && not (fdpFieldHasEmptyJsonName field) = []
      | otherwise = [simpleOption "json_name" (CString jsonName)]
    simpleOption name value = OptionDef () (OptionName [SimpleOption name]) value


-- | Decode the declared default using the field's protobuf type. Descriptor
-- strings are already unescaped, whereas bytes carry C-escaped contents.
descriptorDefaultConstant :: FieldDescriptorProto -> Maybe Constant
descriptorDefaultConstant field
  | T.null raw && not (fdpFieldHasEmptyDefault field) = Nothing
  | otherwise = case fdpFieldType field of
      1 -> CFloat <$> floating
      2 -> CFloat <$> floating
      8 -> CBool <$> parseDefault boolLiteral raw
      9 -> Just (CString raw)
      12 -> CBytes <$> parseDefault bytesLiteral ("\"" <> raw <> "\"")
      14 -> Just (CIdent raw)
      code | Just _ <- intToScalar code -> CInt <$> parseDefault intLiteral raw
      _ -> Nothing
  where
    raw = fdpFieldDefault field
    floating = parseDefault floatLiteral raw <|> (asDouble <$> parseDefault intLiteral raw)
    asDouble n
      | n == 0 && "-" `T.isPrefixOf` raw = -0.0
      | otherwise = fromInteger n
    parseDefault parser = either (const Nothing) Just . MP.parse (parser <* MP.eof) "<descriptor default>"


-- | Convert an 'EnumDescriptorProto' to an 'EnumDef'.
enumDescriptorToEnum :: EnumDescriptorProto -> EnumDef
enumDescriptorToEnum e =
  EnumDef
    { enumExt = ()
    , enumDoc = Nothing
    , enumName = edpName e
    , enumValues =
        fmap
          (\v -> EnumValue () Nothing (evdpName v) (fromIntegral (evdpNumber v)) [])
          (V.toList (edpValue e))
    , enumOptions = maybe [] options (edpOptions e)
    }
  where
    options opts = descriptorFeatureOptions 7 (enoUnknownFields opts)
      <> if enoAllowAlias opts
           then [OptionDef () (OptionName [SimpleOption "allow_alias"]) (CBool True)]
           else []


-- | Convert a 'ServiceDescriptorProto' to a 'ServiceDef'.
serviceDescriptorToService :: ServiceDescriptorProto -> ServiceDef
serviceDescriptorToService s =
  ServiceDef
    { svcExt = ()
    , svcDoc = Nothing
    , svcName = sdpName s
    , svcRpcs = fmap methodToRpc (V.toList (sdpMethod s))
    , svcOptions = []
    }
  where
    methodToRpc m =
      RpcDef
        { rpcExt = ()
        , rpcDoc = Nothing
        , rpcName = mdpName m
        , rpcInput = mdpInputType m
        , rpcInputStr = if mdpClientStreaming m then Streaming else NoStream
        , rpcOutput = mdpOutputType m
        , rpcOutputStr = if mdpServerStreaming m then Streaming else NoStream
        , rpcOptions = []
        }


intToLabel :: Int32 -> Maybe FieldLabel
intToLabel 1 = Just Optional
intToLabel 2 = Just Required
intToLabel 3 = Just Repeated
intToLabel _ = Nothing


intToFieldType :: Int32 -> Text -> FieldType
intToFieldType typeInt typeName = case intToScalar typeInt of
  Just s -> FTScalar s
  Nothing -> FTNamed typeName


intToScalar :: Int32 -> Maybe ScalarType
intToScalar = \case
  1 -> Just SDouble
  2 -> Just SFloat
  3 -> Just SInt64
  4 -> Just SUInt64
  5 -> Just SInt32
  6 -> Just SFixed64
  7 -> Just SFixed32
  8 -> Just SBool
  9 -> Just SString
  12 -> Just SBytes
  13 -> Just SUInt32
  15 -> Just SSFixed32
  16 -> Just SSFixed64
  17 -> Just SSInt32
  18 -> Just SSInt64
  _ -> Nothing
