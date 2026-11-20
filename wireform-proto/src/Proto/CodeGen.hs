{-# LANGUAGE PatternSynonyms #-}

{- | Pure-text code generation for Haskell modules from parsed proto files.

This module powers the standalone @wireform-protoc@ code generator
(as opposed to the Template Haskell path in "Proto.TH"). It produces
complete, compilable Haskell source text with:

* Proper cross-module imports via a 'TypeRegistry'
* Record types for messages, sum types for enums and oneofs
* @MessageEncode@ \/ @MessageDecode@ \/ @MessageSize@ instances
* @Aeson.ToJSON@ \/ @Aeson.FromJSON@ instances (respecting @json_name@)
* Map field, oneof, and nested message support

== 'GenerateOpts' configuration

Use 'GenerateOpts' to control the output:

  ['genModulePrefix'] The Haskell module prefix prepended to generated
    module names. Default: @\"Proto.Gen\"@.

  ['genFieldNaming'] How to name record fields -- 'PrefixedFields'
    (default, e.g. @personName@) or 'UnprefixedFields' (e.g. @name@,
    requires @DuplicateRecordFields@).

  ['genStrictFields'] Whether to add strict-field annotations (@!@).
    Default: 'True'.

  ['genUnpackPrims'] Whether to add @UNPACK@ pragmas on primitive
    fields. Default: 'True'.

  ['genDeriveGeneric'] Derive @GHC.Generics.Generic@. Default: 'True'.

  ['genDeriveNFData'] Derive @Control.DeepSeq.NFData@. Default: 'True'.

  ['genPackedRepeated'] Use packed encoding for repeated scalar fields.
    Default: 'True'.

  ['genLazySubmessages'] Decode submessage fields lazily
    (via 'Proto.Decode.LazyMessage'). Default: 'False'.

  ['genJsonOverrides'] Per-message overrides for JSON instances. See
    'JsonOverride'.

  ['genHooks'] 'CodeGenHooks' callbacks for emitting extra declarations.
-}
module Proto.CodeGen (
  generateModule,
  generateModuleText,
  generateModuleTextWithImports,
  generateModuleTextFromDescriptor,
  GenerateOpts (..),
  defaultGenerateOpts,
  FieldNaming (..),
  JsonOverride (..),
  defaultJsonOverrides,
  TypeRegistry,
  TypeInfo (TypeInfo, tiModule, tiHsName, tiKind),
  TypeKind (..),
  buildTypeRegistry,
  restrictOptionImportTypes,
  builtinDescriptorTypes,
  registryMessageNames,
  resolveTypeKey,
  moduleNameForProto,
  moduleNameConflict,
  hsTypeName,
  hsModuleName,
  scopedTypeName,
  scopedFieldName,
  scopedFieldNameWith,
  RecordBindingNames (..),
  recordBindingNames,
  fieldLabelNames,
  OneofBindingNames,
  oneofBindingNames,
  oneofTypeName,
  oneofConstructorName,
  scopedEnumCon,
  enumConstructorNames,
  snakeToCamel,
  snakeToPascal,
  protoJsonName,
  NamedJSONField (..),
  NamedJSONShape (..),
  namedJSONFields,
  ExtensionNames (..),
  extensionBindingNames,
  lookupExtensionNames,
  verifiedStringExtensions,
  mapScopedMessages,
  extensionRegistryRequirements,
  lowerFirst,
  escapeReserved,

  -- * Codegen combinators (re-exported from Combinators)
  txt,
  tshow,
  braceBlock,
  instanceHead,

  -- * Codegen hooks (re-exported from Hooks)
  module Proto.CodeGen.Hooks,
) where

import Control.Applicative ((<|>))
import Data.ByteString qualified as BS
import Data.Char (isAsciiUpper, isLower, toLower, toUpper)
import Data.Int (Int32)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Data.Word (Word8)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Proto.CodeGen.Hooks
import Proto.CodeGen.Service qualified as Service
import Proto.Encode (encodeMessage)
import Proto.Google.Protobuf.Descriptor (FileDescriptorProto (..))
import Proto.IDL.AST
import Proto.IDL.Annotations (lookupSimpleOption, optionAsString)
import Proto.IDL.Descriptor (descriptorTypeDefinitions, fileDescriptorToAST, resolveProtoName, serializeFileDescriptorWithImports)
import Proto.IDL.Options
import Proto.IDL.Options.Custom (emptyCustomOptionRegistry, extractExtensionOptions, registerCustomOption)
import Proto.IDL.Parser.Resolver (ResolvedProto (..), importClosure)
import Proto.Internal.CodeGen.Combinators (braceBlock, chooseGeneratedName, instanceHead, tshow, txt)
import Proto.Internal.FieldPresence (enumIsClosed, explicitFieldPresence, messageEncodingDelimited, repeatedFieldPacked, resolveFieldPresence, verifyStringUtf8)
import Proto.Internal.Wire.Encode qualified as Wire


-- | Emit a Haddock doc comment block from a proto doc comment.
haddockDoc :: Maybe Text -> [Doc ann]
haddockDoc Nothing = []
haddockDoc (Just doc) =
  let ls = T.lines doc
  in case ls of
       [] -> []
       (first_ : rest) ->
         [txt "-- | " <> pretty first_]
           <> fmap (\l -> txt "-- " <> pretty l) rest


-- | Emit a Haddock post-item doc comment (@-- ^@) from a proto doc comment.
haddockFieldDoc :: Maybe Text -> [Doc ann]
haddockFieldDoc Nothing = []
haddockFieldDoc (Just doc) =
  let ls = T.lines doc
  in case ls of
       [] -> []
       (first_ : rest) ->
         [indent 2 (txt "-- ^ " <> pretty first_)]
           <> fmap (\l -> indent 2 (txt "-- " <> pretty l)) rest


wireVarint, wire64Bit, wireLengthDelimited, wireStartGroup, wire32Bit :: Int
wireVarint = 0
wire64Bit = 1
wireLengthDelimited = 2
wireStartGroup = 3
wire32Bit = 5


scalarWireType :: ScalarType -> Int
scalarWireType = \case
  SDouble -> wire64Bit
  SFixed64 -> wire64Bit
  SSFixed64 -> wire64Bit
  SFloat -> wire32Bit
  SFixed32 -> wire32Bit
  SSFixed32 -> wire32Bit
  SString -> wireLengthDelimited
  SBytes -> wireLengthDelimited
  _ -> wireVarint


namedWireType :: TypeKind -> Int
namedWireType = \case
  TKEnum -> wireVarint
  TKMessage -> wireLengthDelimited


computeTagByte :: Int -> Int -> Int
computeTagByte fieldNum wireType = fieldNum * 8 + wireType


tagLit :: Text -> Int -> Doc ann
tagLit fn wt = pretty (computeTagByte (fieldNumberLiteral fn) wt)


fieldNumberLiteral :: Text -> Int
fieldNumberLiteral fn = case TR.decimal fn of
  Right (n, "") -> n
  _ -> error "Invalid generated field number"


singleByteTag :: Text -> Bool
singleByteTag fn = fieldNumberLiteral fn <= 15


adjustTagSize :: Text -> Doc ann -> Doc ann
adjustTagSize fn size =
  let extraBytes = Wire.tagSize (fieldNumberLiteral fn) - 1
  in if extraBytes == 0 then size else parens (pretty extraBytes <+> txt "+" <+> size)


-- ---------------------------------------------------------------------------
-- Options
-- ---------------------------------------------------------------------------

{- | Controls how generated record field names relate to the proto
field name and the enclosing message name.
-}
data FieldNaming
  = {- | Prefix each field with the lowercased message name.
    @message Person { string name = 1; }@ becomes
    @personName :: !Text@. This is the default; it avoids
    name clashes without @DuplicateRecordFields@.
    -}
    PrefixedFields
  | {- | Use the proto field name directly (snake_case to camelCase).
    @message Person { string name = 1; }@ becomes
    @name :: !Text@.  Requires @DuplicateRecordFields@ if two
    messages share a field name. Works well with
    @OverloadedRecordDot@ and GHC's @HasField@ class.
    -}
    UnprefixedFields
  deriving stock (Show, Eq)


{- | Options controlling the pure-text code generator.
See the module-level documentation for a description of each field.
-}
data GenerateOpts = GenerateOpts
  { genModulePrefix :: Text
  -- ^ Haskell module prefix (e.g. @\"Proto.Gen\"@).
  , genFieldNaming :: FieldNaming
  -- ^ Record field naming strategy.
  , genStrictFields :: Bool
  -- ^ Add strict annotations (@!@) to record fields.
  , genUnpackPrims :: Bool
  -- ^ Add @UNPACK@ pragmas on primitive fields.
  , genDeriveGeneric :: Bool
  -- ^ Derive @GHC.Generics.Generic@.
  , genDeriveNFData :: Bool
  -- ^ Derive @Control.DeepSeq.NFData@.
  , genPackedRepeated :: Bool
  -- ^ Use packed wire encoding for repeated scalar fields.
  , genLazySubmessages :: Bool
  -- ^ Decode submessage fields lazily via 'Proto.Decode.LazyMessage'.
  , genJsonOverrides :: Map Text JsonOverride
  -- ^ Per-message 'JsonOverride' keyed by fully-qualified proto name.
  , genHooks :: CodeGenHooks
  -- ^ Callbacks for emitting extra declarations during codegen.
  }


{- | Custom JSON instance override for a specific FQ proto message name.
When present, the code generator emits the provided Haskell source text
instead of the generic JSON instances.
-}
data JsonOverride = JsonOverride
  { joToJSON :: Text
  , joFromJSON :: Text
  }
  deriving stock (Show, Eq)


{- | Sensible defaults: @\"Proto.Gen\"@ module prefix, prefixed field
names, strict fields, unpack primitives, derive Generic and NFData,
packed repeated scalars, eager submessages, built-in JSON overrides
for well-known types, no hooks.
-}
defaultGenerateOpts :: GenerateOpts
defaultGenerateOpts =
  GenerateOpts
    { genModulePrefix = "Proto.Gen"
    , genFieldNaming = PrefixedFields
    , genStrictFields = True
    , genUnpackPrims = True
    , genDeriveGeneric = True
    , genDeriveNFData = True
    , genPackedRepeated = True
    , genLazySubmessages = False
    , genJsonOverrides = defaultJsonOverrides
    , genHooks = defaultCodeGenHooks
    }


{- | Built-in JSON overrides for well-known types that require canonical
proto3 JSON representations.
-}
defaultJsonOverrides :: Map Text JsonOverride
defaultJsonOverrides =
  Map.fromList
    [
      ( "google.protobuf.Timestamp"
      , JsonOverride
          { joToJSON =
              "  toJSON (Timestamp seconds nanos _) = Aeson.String (formatRfc3339 seconds nanos)"
          , joFromJSON =
              T.unlines
                [ "  parseJSON (Aeson.String t) = either fail pure (parseTimestampRfc3339 (\\s n -> Timestamp s n []) t)"
                , "  parseJSON _ = fail \"Expected RFC 3339 timestamp string\""
                ]
          }
      )
    ,
      ( "google.protobuf.Duration"
      , JsonOverride
          { joToJSON =
              "  toJSON (Duration seconds nanos _) = Aeson.String (formatDurationSeconds seconds nanos)"
          , joFromJSON =
              T.unlines
                [ "  parseJSON (Aeson.String t) = either fail pure (parseDurationSeconds (\\s n -> Duration s n []) t)"
                , "  parseJSON _ = fail \"Expected duration string\""
                ]
          }
      )
    , ( "google.protobuf.Any"
      , JsonOverride
          { joToJSON = "  toJSON (Any url bytes _) = WK.anyPartsToJSON WK.anyJsonRegistry url bytes"
          , joFromJSON =
              T.unlines
                [ "  parseJSON j = case WK.anyPartsFromJSON WK.anyJsonRegistry j of"
                , "    Left err -> fail err"
                , "    Right (url, bytes) -> pure (Any url bytes [])"
                ]
          }
      )
    , scalarWrapper "FieldMask" "PJ.fieldMaskPathsToJSON" "PJ.fieldMaskPathsFromJSON"
    , scalarWrapper "BoolValue" "Aeson.toJSON" "Aeson.parseJSON"
    , scalarWrapper "Int32Value" "Aeson.toJSON" "PJ.protoInt32FromJSON"
    , scalarWrapper "Int64Value" "PJ.protoInt64ToJSON" "PJ.protoInt64FromJSON"
    , scalarWrapper "UInt32Value" "Aeson.toJSON" "PJ.protoWord32FromJSON"
    , scalarWrapper "UInt64Value" "PJ.protoWord64ToJSON" "PJ.protoWord64FromJSON"
    , scalarWrapper "FloatValue" "PJ.protoFloatToJSON" "PJ.protoFloatFromJSON"
    , scalarWrapper "DoubleValue" "PJ.protoDoubleToJSON" "PJ.protoDoubleFromJSON"
    , scalarWrapper "StringValue" "Aeson.toJSON" "Aeson.parseJSON"
    , scalarWrapper "BytesValue" "PJ.protoBytesToJSON" "PJ.protoBytesFromJSON"
    , ( "google.protobuf.Struct"
      , JsonOverride
          { joToJSON = "  toJSON (Struct fields _) = Aeson.toJSON fields"
          , joFromJSON = "  parseJSON j = flip Struct [] <$> Aeson.parseJSON j"
          }
      )
    , ( "google.protobuf.ListValue"
      , JsonOverride
          { joToJSON = "  toJSON (ListValue values _) = Aeson.toJSON values"
          , joFromJSON = "  parseJSON j = flip ListValue [] <$> Aeson.parseJSON j"
          }
      )
    , ( "google.protobuf.Value"
      , JsonOverride
          { joToJSON = T.unlines
              [ "  toJSON (Value kind _) = case kind of"
              , "    Nothing -> Aeson.Null"
              , "    Just (Value'Kind'NullValue _) -> Aeson.Null"
              , "    Just (Value'Kind'NumberValue x)"
              , "      | isNaN x || isInfinite x -> error \"Value: non-finite number not allowed\""
              , "      | otherwise -> Aeson.toJSON x"
              , "    Just (Value'Kind'StringValue x) -> Aeson.String x"
              , "    Just (Value'Kind'BoolValue x) -> Aeson.Bool x"
              , "    Just (Value'Kind'StructValue x) -> Aeson.toJSON x"
              , "    Just (Value'Kind'ListValue x) -> Aeson.toJSON x"
              ]
          , joFromJSON = T.unlines
              [ "  parseJSON j = (\\kind -> Value (Just kind) []) <$> case j of"
              , "    Aeson.Null -> pure (Value'Kind'NullValue (toEnum 0))"
              , "    Aeson.Number _ -> Value'Kind'NumberValue <$> Aeson.parseJSON j"
              , "    Aeson.String x -> pure (Value'Kind'StringValue x)"
              , "    Aeson.Bool x -> pure (Value'Kind'BoolValue x)"
              , "    Aeson.Object _ -> Value'Kind'StructValue <$> Aeson.parseJSON j"
              , "    Aeson.Array _ -> Value'Kind'ListValue <$> Aeson.parseJSON j"
              ]
          }
      )
    ]
  where
    scalarWrapper name encode decode =
      ( "google.protobuf." <> name
      , JsonOverride
          { joToJSON = "  toJSON (" <> name <> " value _) = " <> encode <> " value"
          , joFromJSON = "  parseJSON json = flip " <> name <> " [] <$> " <> decode <> " json"
          }
      )


-- ---------------------------------------------------------------------------
-- Type registry: maps fully-qualified proto names to Haskell type info
-- ---------------------------------------------------------------------------

-- | Whether a registered type is a message or an enum.
data TypeKind
  = -- | A message type.
    TKMessage
  | -- | An enum type.
    TKEnum
  deriving stock (Show, Eq, Ord)


-- | Information about a registered proto type's Haskell mapping.
data TypeInfo = TypeInfoWithRegistry !Text !Text !TypeKind !Bool
  deriving stock (Show, Eq)


pattern TypeInfo :: Text -> Text -> TypeKind -> TypeInfo
pattern TypeInfo
  { tiModule
  -- ^ The Haskell module containing this type.
  , tiHsName
  -- ^ The Haskell type name.
  , tiKind
  -- ^ Whether this is a message or enum.
  } <- TypeInfoWithRegistry tiModule tiHsName tiKind _
  where
    TypeInfo modName hsName kind = TypeInfoWithRegistry modName hsName kind False
{-# COMPLETE TypeInfo #-}


-- | Maps fully-qualified proto type names to their Haskell type info.
type TypeRegistry = Map Text TypeInfo


{- | Build a TypeRegistry from a list of (proto file path, resolved proto).
Walks all top-levels and nested definitions, recording their FQ names.
| Build a TypeRegistry from resolved proto files. Also includes entries
for the standard well-known Google protobuf types, pointing to their
generated modules under @Proto.Google.Protobuf.*@.
-}
buildTypeRegistry :: GenerateOpts -> [(FilePath, ResolvedProto)] -> TypeRegistry
buildTypeRegistry opts rpList = Map.mapWithKey mark registry
  where
    registry = Map.union builtinWellKnownTypes
      (Map.unions (fmap (\(fp, rp) -> registryForFile opts (normalizeProtoPath fp) (rpFile rp)) rpList))
    required = registryRequirementsForFiles registry (fmap (rpFile . snd) rpList)
    mark fqn (TypeInfo modName hsName kind) =
      TypeInfoWithRegistry modName hsName kind (Set.member fqn required)


-- | Remove types reachable only through an option import, and diagnose
-- references to those types before the legacy Haskell name fallback can
-- turn them into accidental dependencies.
restrictOptionImportTypes :: Map Text ProtoFile -> ProtoFile -> TypeRegistry -> Either Text TypeRegistry
restrictOptionImportTypes files file registry
  | null optionRoots = Right registry
  | Just ref <- List.find unavailable (Set.toList (collectReferencedTypes (protoTopLevels file))) =
      Left ("type " <> refName ref <> " is only available through an option import")
  | otherwise = Right visibleRegistry
  where
    optionRoots = concatMap (map importPath . filter optionOnly . protoImports) (file : Map.elems files)
    optionOnly imp = importModifier imp == Just ImportOption
    visiblePaths = importClosure files ((== Just ImportPublic) . importModifier)
      (map importPath (filter (not . optionOnly) (protoImports file)))
    optionPaths = importClosure files (const True) optionRoots
    visibleDefinitions = Map.keysSet (descriptorTypeDefinitions file) <> definitions visiblePaths
    hidden = definitions optionPaths `Set.difference` visibleDefinitions
    definitions paths =
      foldMap (Map.keysSet . descriptorTypeDefinitions) (Map.restrictKeys files paths)
    visibleRegistry = Map.withoutKeys registry hidden
    unavailable ref =
      let name = refName ref
          pkg = protoPackage file
          scope = refScope ref
          visible = resolveLexicalName (`Map.member` visibleRegistry) pkg scope name
          hiddenMatch =
            isJust (resolveLexicalName (`Set.member` hidden) pkg scope name)
              || (not (T.isPrefixOf "." name) && any (T.isSuffixOf ("." <> name)) hidden)
      in hiddenMatch && not (isJust visible)


{- | Registry entries for standard Google well-known types.
These reference the generated modules at @Proto.Google.Protobuf.*@,
which are produced by running the code generator on the bundled
@proto/google/protobuf/*.proto@ files with prefix @Proto@.
-}
builtinWellKnownTypes :: TypeRegistry
builtinWellKnownTypes =
  Map.fromList
    [ wkt "google.protobuf.Duration" "Proto.Google.Protobuf.Duration" "Duration"
    , wkt "google.protobuf.Timestamp" "Proto.Google.Protobuf.Timestamp" "Timestamp"
    , wkt "google.protobuf.Empty" "Proto.Google.Protobuf.Empty" "Empty"
    , wkt "google.protobuf.Any" "Proto.Google.Protobuf.Any" "Any"
    , wkt "google.protobuf.Struct" "Proto.Google.Protobuf.Struct" "Struct"
    , wkt "google.protobuf.Value" "Proto.Google.Protobuf.Struct" "Value"
    , wkt "google.protobuf.ListValue" "Proto.Google.Protobuf.Struct" "ListValue"
    , ("google.protobuf.NullValue", TypeInfo "Proto.Google.Protobuf.Struct" "NullValue" TKEnum)
    , wkt "google.protobuf.FieldMask" "Proto.Google.Protobuf.FieldMask" "FieldMask"
    , wkt "google.protobuf.SourceContext" "Proto.Google.Protobuf.SourceContext" "SourceContext"
    , wkt "google.protobuf.DoubleValue" "Proto.Google.Protobuf.Wrappers" "DoubleValue"
    , wkt "google.protobuf.FloatValue" "Proto.Google.Protobuf.Wrappers" "FloatValue"
    , wkt "google.protobuf.Int64Value" "Proto.Google.Protobuf.Wrappers" "Int64Value"
    , wkt "google.protobuf.UInt64Value" "Proto.Google.Protobuf.Wrappers" "UInt64Value"
    , wkt "google.protobuf.Int32Value" "Proto.Google.Protobuf.Wrappers" "Int32Value"
    , wkt "google.protobuf.UInt32Value" "Proto.Google.Protobuf.Wrappers" "UInt32Value"
    , wkt "google.protobuf.BoolValue" "Proto.Google.Protobuf.Wrappers" "BoolValue"
    , wkt "google.protobuf.StringValue" "Proto.Google.Protobuf.Wrappers" "StringValue"
    , wkt "google.protobuf.BytesValue" "Proto.Google.Protobuf.Wrappers" "BytesValue"
    ]
  where
    wkt fqn modName hsName = (fqn, TypeInfo modName hsName TKMessage)


-- | Descriptor type numbers for well-known types available to both generators.
builtinDescriptorTypes :: Map Text Int32
builtinDescriptorTypes = descriptorRegistryTypes builtinWellKnownTypes


descriptorRegistryTypes :: TypeRegistry -> Map Text Int32
descriptorRegistryTypes = Map.map (typeNumber . tiKind)
  where
    typeNumber TKMessage = 11
    typeNumber TKEnum = 14


{- | Normalize a proto file path to a relative path suitable for module naming.
Handles absolute paths by extracting the proto-relative portion.
-}
normalizeProtoPath :: FilePath -> FilePath
normalizeProtoPath = id


registryForFile :: GenerateOpts -> FilePath -> ProtoFile -> TypeRegistry
registryForFile opts fp pf =
  let modName = moduleNameForProto opts fp pf
      pkg = fromMaybe "" (protoPackage pf)
  in Map.unions (fmap (registryForTopLevel modName pkg []) (protoTopLevels pf))


registryForTopLevel :: Text -> Text -> [Text] -> TopLevel -> TypeRegistry
registryForTopLevel modName pkg scope = \case
  TLMessage msg -> registryForMessage modName pkg scope msg
  TLEnum ed -> registryForEnum modName pkg scope ed
  _ -> Map.empty


registryForMessage :: Text -> Text -> [Text] -> MessageDef -> TypeRegistry
registryForMessage modName pkg scope msg =
  let scope' = scope <> [msgName msg]
      fqName = if T.null pkg then T.intercalate "." scope' else pkg <> "." <> T.intercalate "." scope'
      hsName = T.intercalate "'" (fmap hsTypeName scope')
      self =
        Map.singleton
          fqName
          TypeInfo
            { tiModule = modName
            , tiHsName = hsName
            , tiKind = TKMessage
            }
      nested = Map.unions (fmap (registryForElement modName pkg scope') (msgElements msg))
  in Map.union self nested


registryForElement :: Text -> Text -> [Text] -> MessageElement -> TypeRegistry
registryForElement modName pkg scope = \case
  MEMessage inner -> registryForMessage modName pkg scope inner
  MEEnum ed -> registryForEnum modName pkg scope ed
  _ -> Map.empty


registryForEnum :: Text -> Text -> [Text] -> EnumDef -> TypeRegistry
registryForEnum modName pkg scope ed =
  let scope' = scope <> [enumName ed]
      fqName = if T.null pkg then T.intercalate "." scope' else pkg <> "." <> T.intercalate "." scope'
      hsName = T.intercalate "'" (fmap hsTypeName scope')
  in Map.singleton
       fqName
       TypeInfo
         { tiModule = modName
         , tiHsName = hsName
         , tiKind = TKEnum
         }


{- | Compute the Haskell module name for a proto file.

Prefers @csharp_namespace@ if set, appending the proto file's base name
when multiple files share a namespace. Namespace components that cannot
appear verbatim in a Haskell module name are escaped.

@csharp_namespace = "Temporalio.Api.Common.V1"@ with file @message.proto@
produces @Proto.Temporalio.Api.Common.V1.Message@.

Conventional lowercase paths retain their PascalCase spelling. Other path
components retain their identity in an escaped suffix, so generating a file
independently gives the same name as generating it alongside its imports.
-}
moduleNameForProto :: GenerateOpts -> FilePath -> ProtoFile -> Text
moduleNameForProto opts filePath pf =
  let fo = extractFileOptions (protoOptions pf)
      path = T.pack filePath
      parts = filter (\part -> not (T.null part) && part /= ".") $
        T.splitOn "/" (fromMaybe path (T.stripSuffix ".proto" path))
      modules = case foCsharpNamespace fo of
        Just ns | not (T.null ns) ->
          concatMap namespacePartToModule (T.splitOn "." ns)
            <> pathPartToModule (fromMaybe "" (listLast parts))
        _ -> concatMap pathPartToModule (if null parts then [""] else parts)
  in T.intercalate "." (genModulePrefix opts : modules)
  where
    listLast = foldl (\_ part -> Just part) Nothing


pathPartToModule :: Text -> [Text]
pathPartToModule source
  | Just (first, rest) <- T.uncons conventional
  , isAsciiUpper first
  , T.all asciiAlphaNumeric rest
  , T.cons (toLower first) (T.concatMap snakePart rest) == source =
      [conventional]
  | otherwise = escapeModulePart source conventional
  where
    conventional = snakeToPascal source
    snakePart c
      | isAsciiUpper c = T.pack ['_', toLower c]
      | otherwise = T.singleton c


namespacePartToModule :: Text -> [Text]
namespacePartToModule source
  | Just (first, rest) <- T.uncons source
  , isAsciiUpper first
  , T.all (\c -> asciiAlphaNumeric c || c == '_' || c == '\'') rest =
      [source]
  | otherwise = escapeModulePart source (snakeToPascal source)


escapeModulePart :: Text -> Text -> [Text]
escapeModulePart source conventional = case T.chunksOf 160 (hexPath source) of
  [] -> ["Path'Path'"]
  [chunk] -> [stem <> "'Path'" <> chunk]
  chunks -> ["Path'Begin"] <> map ("Path'Chunk" <>) chunks <> ["Path'End"]
  where
    stem = case T.uncons conventional of
      Just (first, _) | isAsciiUpper first -> T.take 40 (T.takeWhile asciiAlphaNumeric conventional)
      _ -> "Path"


asciiAlphaNumeric :: Char -> Bool
asciiAlphaNumeric c = isAsciiUpper c || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')


hexPath :: Text -> Text
hexPath = TE.decodeLatin1 . BS.concatMap hexByte . TE.encodeUtf8
  where
    hexByte byte =
      let (hi, lo) = byte `quotRem` 16
      in BS.pack [digit hi, digit lo]
    digit nibble
      | nibble < 10 = 48 + nibble
      | otherwise = 87 + nibble


-- | Report an explicit module-name clash before generation can mistake
-- an imported type for one declared in the output module.
moduleNameConflict :: GenerateOpts -> [(FilePath, ProtoFile)] -> Maybe Text
moduleNameConflict opts files =
  fmap describe (Map.lookupMin (Map.filter ((> 1) . Set.size) names))
  where
    names = Map.fromListWith Set.union
      (map (\(path, file) -> entry path (moduleNameForProto opts path file)) files)
    entry path modName = (T.toCaseFold modName, Set.singleton (path, modName))
    describe (_, entries) =
      let paths = T.intercalate ", " (map (T.pack . fst) (Set.toAscList entries))
          modules = Set.map snd entries
      in case Set.toList modules of
           [modName] -> paths <> " both generate " <> modName
           _ -> paths <> " generate module paths differing only by case: "
                  <> T.intercalate ", " (Set.toAscList modules)


-- ---------------------------------------------------------------------------
-- Code generation
-- ---------------------------------------------------------------------------

data GenCtx = GenCtx
  { gcOpts :: GenerateOpts
  , gcRegistry :: TypeRegistry
  , gcThisMod :: Text
  , gcImportAliases :: Map Text Text
  , gcPkg :: Maybe Text
  , gcSyntax :: Syntax
  , gcFileOptions :: [OptionDef]
  , gcLocalTypes :: Set Text
  , gcRecordNames :: RecordBindingNames
  , gcOneofNames :: OneofBindingNames
  , gcServiceNames :: Map Text Service.ServiceBindings
  , gcExtensionNames :: Map Text ExtensionNames
  , gcVerifiedStringExtensions :: Map Text [Int]
  , gcRegistryMessages :: Set Text
  }


-- | Produce a record field name using the context's naming mode.
ctxFieldName :: GenCtx -> [Text] -> Text -> Text
ctxFieldName ctx scope name =
  Map.findWithDefault
    (scopedFieldNameWith (genFieldNaming (gcOpts ctx)) scope name)
    (scope, name)
    (recordSelectorNames (gcRecordNames ctx))


ctxUnknownFieldName :: GenCtx -> [Text] -> Text
ctxUnknownFieldName ctx scope =
  Map.findWithDefault (unknownFieldAccessor scope) scope (recordUnknownNames (gcRecordNames ctx))


-- | Generate a complete Haskell module as a 'Doc' from a proto file.
generateModule :: GenerateOpts -> TypeRegistry -> FilePath -> ProtoFile -> Doc ann
generateModule opts reg filePath sourceFile =
  generateModuleWithDescriptor opts reg filePath sourceFile
    (\registry -> either (error . T.unpack) id $
      serializeCodeGenDescriptor registry filePath Map.empty sourceFile)


generateModuleWithDescriptor
  :: GenerateOpts -> TypeRegistry -> FilePath -> ProtoFile -> (TypeRegistry -> BS.ByteString) -> Doc ann
generateModuleWithDescriptor opts reg filePath sourceFile descriptorBytes =
  let pf = either (error . T.unpack) id (resolveFieldPresence sourceFile)
      thisMod = moduleNameForProto opts filePath pf
      localTypes = collectLocalTypes (fromMaybe "" (protoPackage pf)) [] (protoTopLevels pf)
      definitions = protoTopLevels pf
      naming = genFieldNaming opts
      records = recordBindingNames naming id definitions
      oneofs = oneofBindingNames definitions
      extensions = extensionBindingNames naming (protoPackage pf) definitions
      registry = Map.unions [reg, builtinWellKnownTypes, registryForFile opts filePath pf]
      ctx =
        GenCtx
          { gcOpts = opts
          , gcRegistry = registry
          , gcThisMod = thisMod
          , gcImportAliases = aliases
          , gcPkg = protoPackage pf
          , gcSyntax = protoSyntax pf
          , gcFileOptions = protoOptions pf
          , gcLocalTypes = localTypes
          , gcRecordNames = records
          , gcOneofNames = oneofs
          , gcServiceNames = moduleServiceBindings naming records oneofs extensions definitions
          , gcExtensionNames = extensions
          , gcVerifiedStringExtensions = either (error . T.unpack) id (verifiedStringExtensions pf)
          , gcRegistryMessages = registryRequirementsForFiles registry [pf]
          }
      body = concatMap (genTopLevel ctx []) (protoTopLevels pf)
      referencedTypes = collectReferencedTypes (protoTopLevels pf)
      importedModules = computeImports registry (protoPackage pf) thisMod referencedTypes
      aliases = allocateImportAliases importedModules
      customOpts =
        foldl
          (flip registerCustomOption)
          emptyCustomOptionRegistry
          (extractExtensionOptions pf)
      fileHookCtx =
        FileHookCtx
          { fhcProtoFile = pf
          , fhcModuleName = thisMod
          , fhcFileOptions = protoOptions pf
          , fhcCustomOptions = customOpts
          }
      fileHookOutput = onFileCodeGen (genHooks opts) fileHookCtx
      fileHookDocs = fmap pretty fileHookOutput
  in vsep $
       [ genModuleHeader opts filePath pf
       , mempty
       , vsep $
           [genImports (or (mapScopedMessages (usesSchemaImport ctx) (protoTopLevels pf))) aliases]
             <> (if Set.member "google.protobuf.Timestamp" localTypes then [txt "import Proto.Internal.JSON (formatRfc3339, parseTimestampRfc3339)"] else [])
             <> (if Set.member "google.protobuf.Duration" localTypes then [txt "import Proto.Internal.JSON (formatDurationSeconds, parseDurationSeconds)"] else [])
             <> (if Set.member "google.protobuf.Any" localTypes then [txt "import Proto.Internal.JSON.WellKnown qualified as WK"] else [])
       , mempty
       , genFileDescriptorBinding (descriptorBytes registry)
       , mempty
       , vsep body
       ]
         <> case fileHookDocs of
           [] -> []
           ds -> [mempty, vsep ds]


-- | Generate a complete Haskell module as rendered 'Text' from a proto file.
generateModuleText :: GenerateOpts -> TypeRegistry -> FilePath -> ProtoFile -> Text
generateModuleText opts reg filePath pf =
  renderStrict (layoutPretty defaultLayoutOptions (generateModule opts reg filePath pf))


-- | Generate from source and the schemas needed to resolve its custom options.
generateModuleTextWithImports
  :: GenerateOpts -> TypeRegistry -> FilePath -> Map Text ProtoFile -> ProtoFile -> Either Text Text
generateModuleTextWithImports opts reg path imports file = do
  descriptor <- serializeCodeGenDescriptor reg path imports file
  pure $ renderStrict $ layoutPretty defaultLayoutOptions $
    generateModuleWithDescriptor opts reg path file (const descriptor)


serializeCodeGenDescriptor
  :: TypeRegistry -> FilePath -> Map Text ProtoFile -> ProtoFile -> Either Text BS.ByteString
serializeCodeGenDescriptor registry =
  serializeFileDescriptorWithImports (Map.union (descriptorRegistryTypes registry) builtinDescriptorTypes)


-- | Generate from a validated protoc descriptor, retaining its runtime
-- options and dependency metadata. Source locations are only needed by
-- protoc and are omitted from the embedded runtime descriptor.
generateModuleTextFromDescriptor :: GenerateOpts -> TypeRegistry -> FileDescriptorProto -> Text
generateModuleTextFromDescriptor opts reg descriptor =
  renderStrict (layoutPretty defaultLayoutOptions moduleDoc)
  where
    moduleDoc = generateModuleWithDescriptor opts reg (T.unpack (fdpName descriptor))
      (fileDescriptorToAST descriptor) (const (encodeMessage descriptor {fdpSourceCodeInfo = Nothing}))


-- Collect all FQ type names defined locally in this file
collectLocalTypes :: Text -> [Text] -> [TopLevel] -> Set Text
collectLocalTypes pkg scope = foldMap go
  where
    go = \case
      TLMessage msg ->
        let scope' = scope <> [msgName msg]
            fqName = if T.null pkg then T.intercalate "." scope' else pkg <> "." <> T.intercalate "." scope'
        in Set.singleton fqName <> foldMap (goElem scope') (msgElements msg)
      TLEnum ed ->
        let scope' = scope <> [enumName ed]
            fqName = if T.null pkg then T.intercalate "." scope' else pkg <> "." <> T.intercalate "." scope'
        in Set.singleton fqName
      _ -> Set.empty
    goElem s = \case
      MEMessage inner ->
        let scope' = s <> [msgName inner]
            fqName = if T.null pkg then T.intercalate "." scope' else pkg <> "." <> T.intercalate "." scope'
        in Set.singleton fqName <> foldMap (goElem scope') (msgElements inner)
      MEEnum ed ->
        let scope' = s <> [enumName ed]
            fqName = if T.null pkg then T.intercalate "." scope' else pkg <> "." <> T.intercalate "." scope'
        in Set.singleton fqName
      _ -> Set.empty


-- Collect all FTNamed type references from top-levels. Covers message
-- fields (including map values, oneof branches, nested messages) and
-- service RPC request/response types — without the service coverage,
-- a @.proto@ that defines a service whose inputs and outputs live in
-- a separate module would emit the module-header imports block
-- without those dependencies, which broke compilation for the
-- generated Service modules (they would @import@ their
-- RequestResponse module after top-level declarations, which is a
-- Haskell parse error).
data ScopedTypeRef = ScopedTypeRef
  { refScope :: ![Text]
  , refName :: !Text
  }
  deriving stock (Eq, Ord)


collectReferencedTypes :: [TopLevel] -> Set ScopedTypeRef
collectReferencedTypes = foldMap goTL
  where
    goTL = \case
      TLMessage msg -> goMsg [] msg
      TLService svc -> foldMap goRpc (svcRpcs svc)
      TLExtend owner fields -> goExt [] owner fields
      _ -> Set.empty
    goMsg scope msg = foldMap (goElem (scope <> [msgName msg])) (msgElements msg)
    goElem scope = \case
      MEField fd -> goFT scope (fieldType fd)
      MEMapField mf -> goFT scope (mapValueType mf)
      MEOneof od -> foldMap (goFT scope . oneofFieldType) (oneofFields od)
      MEMessage inner -> goMsg scope inner
      MEExtend owner fields -> goExt scope owner fields
      _ -> Set.empty
    goRpc r = Set.fromList [ScopedTypeRef [] (rpcInput r), ScopedTypeRef [] (rpcOutput r)]
    goExt scope owner fields = Set.insert (ScopedTypeRef scope owner) (foldMap (goFT scope . fieldType) fields)
    goFT scope = \case
      FTNamed n -> Set.singleton (ScopedTypeRef scope n)
      _ -> Set.empty


-- Compute the set of external modules referenced by this file.
computeImports :: TypeRegistry -> Maybe Text -> Text -> Set ScopedTypeRef -> Set Text
computeImports registry pkg thisMod = Set.fromList . mapMaybe external . Set.toList
  where
    external ref = do
      key <- resolveTypeKey registry pkg (refScope ref) (refName ref)
      ti <- Map.lookup key registry
      if tiModule ti == thisMod then Nothing else Just (tiModule ti)


-- Resolve a proto type name to TypeInfo. Tries FQ lookup first, then
-- with current package prefix, then with parent message scopes, then simple.
resolveType :: GenCtx -> Text -> Maybe TypeInfo
resolveType ctx = resolveTypeWithScope ctx []


resolveTypeWithScope :: GenCtx -> [Text] -> Text -> Maybe TypeInfo
resolveTypeWithScope ctx scope name =
  resolveTypeKeyWithScope ctx scope name >>= (`Map.lookup` gcRegistry ctx)


resolveTypeKeyWithScope :: GenCtx -> [Text] -> Text -> Maybe Text
resolveTypeKeyWithScope ctx = resolveTypeKey (gcRegistry ctx) (gcPkg ctx)


resolveTypeKey :: TypeRegistry -> Maybe Text -> [Text] -> Text -> Maybe Text
resolveTypeKey reg pkg scope name =
  resolveLexicalName (`Map.member` reg) pkg scope name
    <|> if T.isPrefixOf "." name
          then Nothing
          else List.find (T.isSuffixOf ("." <> name)) (Map.keys reg)


resolveLexicalName :: (Text -> Bool) -> Maybe Text -> [Text] -> Text -> Maybe Text
resolveLexicalName known pkg scope name =
  resolveProtoName known (T.intercalate "." (maybe scope (: scope) pkg)) name


-- ---------------------------------------------------------------------------
-- Module header & imports
-- ---------------------------------------------------------------------------

genModuleHeader :: GenerateOpts -> FilePath -> ProtoFile -> Doc ann
genModuleHeader opts filePath pf =
  let modName = moduleNameForProto opts filePath pf
      pkgDoc = maybe "" (\p -> " from package @" <> p <> "@") (protoPackage pf)
      fo = extractFileOptions (protoOptions pf)
      deprLine =
        if foDeprecated fo
          then [txt "--", txt "-- __This file is deprecated.__"]
          else []
      dupRecordExts = case genFieldNaming opts of
        UnprefixedFields ->
          [ txt "{-# LANGUAGE DuplicateRecordFields #-}"
          , txt "{-# LANGUAGE NoFieldSelectors #-}"
          , -- The type-annotated getters/setters/updates that make bare
            -- field names unambiguous still trip -Wambiguous-fields (a
            -- deprecation notice for the type-directed resolution they rely
            -- on); silence it so the generated module builds under -Werror.
            txt "{-# OPTIONS_GHC -Wno-ambiguous-fields #-}"
          ]
        PrefixedFields -> []
  in vsep $
       [ txt "{-# LANGUAGE StrictData #-}"
       , txt "{-# LANGUAGE DeriveGeneric #-}"
       , txt "{-# LANGUAGE DeriveAnyClass #-}"
       , txt "{-# LANGUAGE DerivingStrategies #-}"
       , txt "{-# LANGUAGE OverloadedStrings #-}"
       , txt "{-# LANGUAGE OverloadedRecordDot #-}"
       , txt "{-# LANGUAGE PatternSynonyms #-}"
       , txt "{-# LANGUAGE FlexibleContexts #-}"
       , txt "{-# LANGUAGE UndecidableInstances #-}"
       , txt "{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches -Wno-unused-top-binds #-}"
       ]
         <> dupRecordExts
         <> Service.emptyServicePragmas (mapMaybe topLevelService (protoTopLevels pf))
         <> [ txt "-- | Auto-generated protobuf types" <> pretty pkgDoc <> txt "."
            , txt "--"
            , txt "-- __THIS FILE IS AUTO-GENERATED BY wireform. DO NOT EDIT.__"
            , txt "--"
            , txt "-- Any manual changes will be overwritten the next time code"
            , txt "-- generation is run.  To modify the types or instances, edit the"
            , txt "-- @.proto@ source file and re-run the code generator."
            ]
         <> deprLine
         <> [txt "module " <> pretty modName <> txt " where"]


isOneofElement :: MessageElement -> Bool
isOneofElement MEOneof {} = True
isOneofElement _ = False


usesSchemaImport :: GenCtx -> [Text] -> MessageDef -> Bool
usesSchemaImport ctx scope msg =
  any isOneofElement (msgElements msg) || not (null (submessageEntries ctx scope (msgElements msg)))


genImports :: Bool -> Map Text Text -> Doc ann
genImports needsSchema externalModules =
  vsep $
    [ txt "import Data.ByteString (ByteString)"
    , txt "import qualified Data.ByteString as BS"
    , txt "import qualified Wireform.Builder as B"
    , txt "import Data.Int (Int32, Int64)"
    , txt "import Data.Text (Text)"
    , txt "import qualified Data.Text as T"
    , txt "import Data.Word (Word32, Word64)"
    , txt "import qualified Data.Map.Strict as Map"
    , txt "import qualified Data.Vector as V"
    , txt "import qualified Data.Vector.Unboxed as VU"
    , txt "import GHC.Generics (Generic)"
    , txt "import Control.DeepSeq (NFData(..))"
    , txt "import Data.Hashable (Hashable(..))"
    , txt "import Proto.Encode"
    , txt "import Proto.Decode"
    , txt "import qualified Data.Aeson as Aeson"
    , txt "import qualified Data.Aeson.Types as Aeson"
    , txt "import qualified Data.Aeson.Key as AesonKey"
    , txt "import qualified Data.Aeson.KeyMap as AesonKM"
    , txt "import Proto.Internal.JSON (jsonObject, (.=:), parseFieldMaybe, bytesFieldToJSON, parseBytesFieldMaybe, bytesMapFieldToJSON, parseBytesMapFieldMaybe, protoBytesToJSON, protoBytesFromJSON)"
    , txt "import qualified Proto.Internal.JSON as PJ"
    , txt "import Data.Proxy (Proxy(..))"
    , txt "import Data.Reflection (Given(..), given)"
    , txt "import Proto.Internal.JSON.Extension qualified as PJExt"
    , txt "import Proto.Schema (ProtoMessage(..), ProtoEnum(..), SomeFieldDescriptor(..), FieldDescriptor(..), FieldTypeDescriptor(..), ScalarFieldType(..), FieldLabel'(..))"
    , txt "import Proto.Registry (IsMessage)"
    , txt "import Proto.Registry qualified"
    , txt "import qualified Proto.Extension"
    , txt "import Proto.Internal.Wire (Tag(..), WireType(..))"
    , txt "import Proto.Internal.Wire.Encode (putTag, putVarint, putFixed32, putFixed64,"
    , txt "  putFloat, putDouble, putText, putByteString, putLengthDelimited,"
    , txt "  putSVarint32, putSVarint64, putVarintSigned,"
    , txt "  varintSize, tagSize, fieldMessageSize,"
    , txt "  fieldVarintSize, fieldFixed32Size, fieldFixed64Size,"
    , txt "  fieldBoolSize, fieldFloatSize, fieldDoubleSize,"
    , txt "  fieldTextSize, fieldBytesSize,"
    , txt "  fieldSVarint32Size, fieldSVarint64Size,"
    , txt "  varintSize32, zigZag32, zigZag64)"
    , txt "import Proto.Internal.Encode.Archetype (archVarint, archSVarint32, archSVarint64,"
    , txt "  archFixed32, archFixed64, archFloat, archDouble, archBool,"
    , txt "  archString, archBytes, archSubmessage,"
    , txt "  archVarintSize, archStringSize, archBytesSize, archBoolSize,"
    , txt "  archFixed32Size, archFixed64Size, archSubmessageSize)"
    ]
      <> (if needsSchema then [txt "import qualified Proto.Schema as PSchema"] else [])
      <> map (uncurry genQualifiedImport) (Map.toAscList externalModules)


genQualifiedImport :: Text -> Text -> Doc ann
genQualifiedImport modName alias =
  txt "import qualified " <> pretty modName <> txt " as " <> pretty alias


allocateImportAliases :: Set Text -> Map Text Text
allocateImportAliases modules =
  snd (Set.foldl' allocate (reserved, Map.empty) modules)
  where
    reserved = Set.fromList ["Aeson", "AesonKey", "AesonKM", "B", "BS", "Map", "PJ", "PJExt", "PSchema", "T", "V", "VU", "WK"]
    allocate (used, aliases) modName =
      let alias = chooseGeneratedName used (moduleAlias modName) (T.replace "." "'" modName)
      in (Set.insert alias used, Map.insert modName alias aliases)


{- | Derive a short, deterministic alias from a module name.
Strips common prefixes and uses initials for boilerplate segments.

@Proto.Google.Protobuf.Timestamp@             -> @PB_Timestamp@
@Proto.Google.Protobuf.WellKnownTypes.Empty@  -> @PB_WellKnownTypes_Empty@
@Proto.Temporalio.Api.Common.V1.Message@       -> @TA_Common_V1_Message@
@Proto.Temporalio.Api.Enums.V1.Common@         -> @TA_Enums_V1_Common@
-}
moduleAlias :: Text -> Text
moduleAlias modName =
  let parts = T.splitOn "." modName
      meaningful = dropBoilerplate parts
  in T.intercalate "_" meaningful
  where
    dropBoilerplate = \case
      ("Proto" : "Google" : "Protobuf" : rest) -> "PB" : rest
      ("Proto" : ns : "Api" : rest) -> initials ns : rest
      ("Proto" : ns : rest) -> initials ns : rest
      ps -> ps
    initials t =
      let uppers = T.filter isAsciiUpper t
      in if T.length uppers >= 2 then uppers else T.toUpper (T.take 2 t)


-- | Generate a top-level binding containing the serialized FileDescriptorProto.
genFileDescriptorBinding :: BS.ByteString -> Doc ann
genFileDescriptorBinding bytes =
  let escapedLit = byteStringToHsLiteral bytes
  in vsep
       [ txt "-- | Serialized FileDescriptorProto for this .proto file."
       , txt "-- Decode with @Proto.Google.Protobuf.Descriptor.decodeMessage@."
       , txt "fileDescriptorProtoBytes :: ByteString"
       , txt "fileDescriptorProtoBytes = " <> pretty ("\"" :: Text) <> pretty escapedLit <> pretty ("\"" :: Text)
       ]


-- | Render a 'ByteString' as a Haskell string literal body using @\\xHH@ escapes.
byteStringToHsLiteral :: BS.ByteString -> Text
byteStringToHsLiteral = T.concat . fmap escapeWord8 . BS.unpack
  where
    escapeWord8 :: Word8 -> Text
    escapeWord8 w =
      let hi = hexNibble (w `div` 16)
          lo = hexNibble (w `mod` 16)
      in T.pack ['\\', 'x', hi, lo]
    hexNibble :: Word8 -> Char
    hexNibble n
      | n < 10 = toEnum (fromEnum '0' + fromIntegral n)
      | otherwise = toEnum (fromEnum 'a' + fromIntegral n - 10)


-- ---------------------------------------------------------------------------
-- Top-level generation
-- ---------------------------------------------------------------------------

genTopLevel :: GenCtx -> [Text] -> TopLevel -> [Doc ann]
genTopLevel ctx scope = \case
  TLMessage msg -> genMessage ctx scope msg
  TLEnum ed -> genEnum ctx scope ed
  TLService svc -> genServiceTopLevel ctx scope svc
  TLExtend extName fields -> genExtensionBlock ctx scope extName fields
  TLOption _ -> []
  TLComment _ -> []


genMessage :: GenCtx -> [Text] -> MessageDef -> [Doc ann]
genMessage ctx scope msg =
  let scope' = scope <> [msgName msg]
      tyN = scopedTypeName scope'
      nestedDefs = concatMap (genNestedElement ctx scope') (msgElements msg)
      hookCtx =
        MessageHookCtx
          { mhcMessageDef = msg
          , mhcScope = scope'
          , mhcHsTypeName = tyN
          , mhcFqProtoName = fqProtoName (gcPkg ctx) scope'
          , mhcOptions = messageOptions msg
          }
      hookOutput = onMessageCodeGen (genHooks (gcOpts ctx)) hookCtx
      hookDocs = fmap pretty hookOutput
  in [ mempty
     , genMessageDataDecl ctx scope' msg
     ]
       <> nestedDefs
       <> [ mempty
          , genDefaultInstance ctx scope' msg
          , mempty
          , genEncodeInstance ctx scope' msg
          , mempty
          , genSizeInstance ctx scope' msg
          , mempty
          , genDecodeInstance ctx scope' msg
          , mempty
          , genProtoMessageInstance ctx scope' msg
          , mempty
          , genIsMessageInstance ctx scope'
          , mempty
          , genToJSONInstance ctx scope' msg
          , mempty
          , genFromJSONInstance ctx scope' msg
          , mempty
          , genHashableInstance ctx scope' msg
          , mempty
          , genHasExtensionsInstance ctx scope' msg
          , mempty
          , genSemigroupInstance ctx scope' msg
          , mempty
          , genMonoidInstance scope'
          ]
       <> case hookDocs of
         [] -> []
         ds -> [mempty, vsep ds]


genNestedElement :: GenCtx -> [Text] -> MessageElement -> [Doc ann]
genNestedElement ctx scope = \case
  MEMessage inner -> genMessage ctx scope inner
  MEEnum ed -> genEnum ctx scope ed
  MEOneof od -> [genOneofDecl ctx scope od, genOneofToJSONInstance ctx scope od, genOneofFromJSONInstance ctx scope od, genOneofHashableInstance ctx scope od]
  MEExtend extName fields -> genExtensionBlock ctx scope extName fields
  _ -> []


-- ---------------------------------------------------------------------------
-- Data declarations
-- ---------------------------------------------------------------------------

genMessageDataDecl :: GenCtx -> [Text] -> MessageDef -> Doc ann
genMessageDataDecl ctx scope msg =
  let tyN = scopedTypeName scope
      userFields = concatMap (extractFieldDecl ctx scope) (msgElements msg)
      unknownFieldDecl = pretty (ctxUnknownFieldName ctx scope) <+> txt "::" <+> txt "![UnknownField]"
      allFields = userFields <> [unknownFieldDecl]
  in vsep $
       haddockDoc (msgDoc msg)
         <> [ txt "data " <> pretty tyN <> txt " = " <> pretty tyN
            , indent 2 (braceBlock allFields)
            , indent 2 (txt "deriving stock (Show, Eq, Generic)")
            , indent 2 (txt "deriving anyclass NFData")
            ]


unknownFieldAccessor :: [Text] -> Text
unknownFieldAccessor scope =
  let prefix = case scope of
        [] -> ""
        [s] -> lowerFirst (hsTypeName s)
        _ -> lowerFirst (T.intercalate "" (fmap hsTypeName scope))
  in prefix <> "UnknownFields"


extractFieldDecl :: GenCtx -> [Text] -> MessageElement -> [Doc ann]
extractFieldDecl ctx scope = \case
  MEField fd -> [genFieldDecl ctx scope fd]
  MEMapField mf -> [genMapFieldDecl ctx scope mf]
  MEOneof od -> [genOneofFieldRef ctx scope od]
  _ -> []


genFieldDecl :: GenCtx -> [Text] -> FieldDef -> Doc ann
genFieldDecl ctx scope fd =
  let fieldLine =
        pretty (ctxFieldName ctx scope (fieldName fd))
          <+> txt "::"
          <+> hsFieldType ctx scope (fieldType fd) (fieldLabel fd)
  in case fieldDoc fd of
       Nothing -> fieldLine
       Just doc ->
         let ls = T.lines doc
         in case ls of
              [] -> fieldLine
              (first_ : rest) ->
                vsep $
                  [fieldLine]
                    <> [indent 2 (txt "-- ^ " <> pretty first_)]
                    <> fmap (\l -> indent 2 (txt "-- " <> pretty l)) rest


genMapFieldDecl :: GenCtx -> [Text] -> MapField -> Doc ann
genMapFieldDecl ctx scope mf =
  let fieldLine =
        pretty (ctxFieldName ctx scope (mapFieldName mf))
          <+> txt "::"
          <+> txt "!(Map.Map "
          <> hsScalarType (mapKeyType mf)
          <+> hsFieldTypeInner ctx scope (mapValueType mf)
          <> txt ")"
  in case mapDoc mf of
       Nothing -> fieldLine
       Just doc ->
         let ls = T.lines doc
         in case ls of
              [] -> fieldLine
              (first_ : rest) ->
                vsep $
                  [fieldLine]
                    <> [indent 2 (txt "-- ^ " <> pretty first_)]
                    <> fmap (\l -> indent 2 (txt "-- " <> pretty l)) rest


genOneofFieldRef :: GenCtx -> [Text] -> OneofDef -> Doc ann
genOneofFieldRef ctx scope od =
  pretty (ctxFieldName ctx scope (oneofName od))
    <+> txt "::"
    <+> txt "!(Maybe "
    <> pretty (oneofTypeName (gcOneofNames ctx) scope (oneofName od))
    <> txt ")"


genOneofDecl :: GenCtx -> [Text] -> OneofDef -> Doc ann
genOneofDecl ctx scope od =
  let tyN = oneofTypeName (gcOneofNames ctx) scope (oneofName od)
  in vsep $
       haddockDoc (oneofDoc od)
         <> [ txt "data " <> pretty tyN
            , indent 2 (vsep (concatMap (\(pfx, f) -> (pfx <+> genOneofCon ctx scope f) : haddockFieldDoc (oneofFieldDoc f)) (zip seps (oneofFields od))))
            , indent 2 (txt "deriving stock (Show, Eq, Generic)")
            , indent 2 (txt "deriving anyclass NFData")
            ]
  where
    seps = txt "=" : repeat (txt "|")
    genOneofCon cx s f =
      pretty (oneofConstructorName (gcOneofNames cx) s (oneofName od) (oneofFieldName f))
        <+> hsOneofFieldType cx s (oneofFieldType f)


genOneofToJSONInstance :: GenCtx -> [Text] -> OneofDef -> Doc ann
genOneofToJSONInstance ctx scope od =
  let tyN = oneofTypeName (gcOneofNames ctx) scope (oneofName od)
  in vsep
       [ instanceHead "Aeson.ToJSON" tyN
       , indent 2 (txt "toJSON _ = Aeson.Null")
       ]


genOneofFromJSONInstance :: GenCtx -> [Text] -> OneofDef -> Doc ann
genOneofFromJSONInstance ctx scope od =
  let tyN = oneofTypeName (gcOneofNames ctx) scope (oneofName od)
  in vsep
       [ instanceHead "Aeson.FromJSON" tyN
       , indent 2 (pretty ("parseJSON _ = fail \"Cannot parse oneof from JSON\"" :: Text))
       ]


-- ---------------------------------------------------------------------------
-- Default instances
-- ---------------------------------------------------------------------------

genDefaultInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genDefaultInstance ctx scope msg =
  let tyN = scopedTypeName scope
  in vsep
       [ txt "default" <> pretty tyN <+> txt "::" <+> pretty tyN
       , txt "default" <> pretty tyN <+> txt "=" <+> pretty tyN
       , indent 2 (genDefaultFields ctx scope (msgElements msg))
       ]


genDefaultFields :: GenCtx -> [Text] -> [MessageElement] -> Doc ann
genDefaultFields ctx scope elems =
  let userFields = concatMap extractDefault elems
      unknownDefault = pretty (ctxUnknownFieldName ctx scope) <+> txt "=" <+> txt "[]"
      fields = userFields <> [unknownDefault]
  in braceBlock fields
  where
    extractDefault = \case
      MEField fd ->
        [pretty (ctxFieldName ctx scope (fieldName fd)) <+> txt "=" <+> defaultValue ctx (fieldLabel fd) (fieldType fd)]
      MEMapField mf ->
        [pretty (ctxFieldName ctx scope (mapFieldName mf)) <+> txt "=" <+> txt "Map.empty"]
      MEOneof od ->
        [pretty (ctxFieldName ctx scope (oneofName od)) <+> txt "=" <+> txt "Nothing"]
      _ -> []


defaultValue :: GenCtx -> Maybe FieldLabel -> FieldType -> Doc ann
defaultValue ctx lbl ft = case lbl of
  Just Repeated -> case ft of
    FTScalar s | isUnboxableScalar s -> txt "VU.empty"
    _ -> txt "V.empty"
  _ | explicitFieldPresence lbl -> txt "Nothing"
  _ -> case ft of
    FTScalar SBool -> txt "False"
    FTScalar SString -> pretty ("\"\"" :: Text)
    FTScalar SBytes -> pretty ("\"\"" :: Text)
    FTScalar _ -> txt "0"
    FTNamed n ->
      case resolveType ctx n of
        Just ti | tiKind ti == TKEnum -> txt "(toEnum 0)"
        _ -> txt "Nothing"


-- ---------------------------------------------------------------------------
-- Encode instances
-- ---------------------------------------------------------------------------

genEncodeInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genEncodeInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fields = extractAllFields ctx scope (msgElements msg)
      unknownAcc = "msg." <> ctxUnknownFieldName ctx scope
  in vsep
       [ instanceHead "MessageEncode" tyN
       , indent 2 $
           vsep
             [ txt "buildMessage msg ="
             , indent 2 $ case fields of
                 [] -> txt "encodeUnknownFields " <> pretty unknownAcc
                 _ ->
                   vsep
                     ( zipWith (genFieldBuild ctx) [0 ..] fields
                         <> [txt "<> encodeUnknownFields " <> pretty unknownAcc]
                     )
             ]
       ]


genFieldBuild :: GenCtx -> Int -> FieldInfoFull -> Doc ann
genFieldBuild ctx idx fi =
  let op = if idx == 0 then mempty else txt "<> "
      accessor = "msg." <> fifAccessor fi
      fn = T.pack (show (fifFieldNum fi))
  in op <> case fifKind fi of
       FKScalar lbl ft -> genBuildExprScalar fn accessor lbl (fifPacked fi) ft
       FKNamed lbl _ tk -> genBuildExprNamed fn accessor lbl (fifPacked fi) (fifDelimited fi) tk
       FKMap scope keyT valT -> genBuildExprMap ctx scope fn accessor keyT valT
       FKOneof scope ood -> genBuildExprOneof ctx scope fn accessor ood


genBuildExprScalar :: Text -> Text -> Maybe FieldLabel -> Bool -> ScalarType -> Doc ann
genBuildExprScalar fn accessor lbl packed st = case lbl of
  Just Repeated -> genRepeatedScalarBuild fn accessor packed st
  _ | explicitFieldPresence lbl ->
        txt "(maybe mempty (\\v -> " <> genSingleScalarBuild fn "v" st <> txt ") " <> pretty accessor <> txt ")"
  _ -> txt "(if " <> scalarDefaultCheck accessor st <> txt " then mempty else " <> genSingleScalarBuild fn accessor st <> txt ")"


genBuildExprNamed :: Text -> Text -> Maybe FieldLabel -> Bool -> Bool -> TypeKind -> Doc ann
genBuildExprNamed fn accessor lbl packed delimited tk = case lbl of
  Just Repeated | packed -> txt "encodePackedEnum" <+> pretty fn <+> pretty accessor
  Just Repeated -> txt "V.foldl' (\\acc v -> acc <> " <> build "v" <> txt ") mempty " <> pretty accessor
  _ | tk == TKMessage || explicitFieldPresence lbl ->
        txt "(maybe mempty (\\v -> " <> build "v" <> txt ") " <> pretty accessor <> txt ")"
    | otherwise ->
        txt "(if fromEnum " <> pretty accessor <> txt " == 0 then mempty else " <> build accessor <> txt ")"
  where
    build = genSingleNamedBuild fn tk delimited


genSingleNamedBuild :: Text -> TypeKind -> Bool -> Text -> Doc ann
genSingleNamedBuild fn _ True accessor = txt "encodeFieldGroup" <+> pretty fn <+> pretty accessor
genSingleNamedBuild fn tk False accessor = case tk of
  TKEnum
    | singleByteTag fn ->
        txt "archVarint" <+> tagLit fn wireVarint <+> parens (txt "fromIntegral (fromEnum" <+> value <> txt ")")
    | otherwise -> txt "encodeFieldEnum" <+> pretty fn <+> value
  TKMessage
    | singleByteTag fn ->
        parens (txt "let sz = messageSize" <+> value <+> txt "in archSubmessage"
          <+> tagLit fn wireLengthDelimited <+> txt "sz" <+> parens (txt "buildMessage" <+> value))
    | otherwise -> txt "encodeFieldMessageSized" <+> pretty fn <+> value
  where
    value = pretty accessor


genBuildExprMap :: GenCtx -> [Text] -> Text -> Text -> ScalarType -> FieldType -> Doc ann
genBuildExprMap ctx scope fn accessor keyT valT =
  txt "Map.foldlWithKey' (\\acc k v -> acc <> encodeMapField "
    <> pretty fn
    <> txt " ("
    <> genMapKeyEncode keyT
    <> txt " k) ("
    <> genMapValEncode ctx scope valT
    <> txt " v)) mempty "
    <> pretty accessor


genMapKeyEncode :: ScalarType -> Doc ann
genMapKeyEncode st = case st of
  SString -> txt "encodeFieldString 1"
  SBool -> txt "encodeFieldBool 1"
  SInt32 -> txt "(\\x -> encodeFieldVarint 1 (fromIntegral x))"
  SInt64 -> txt "(\\x -> encodeFieldVarint 1 (fromIntegral x))"
  SUInt32 -> txt "(\\x -> encodeFieldVarint 1 (fromIntegral x))"
  SUInt64 -> txt "encodeFieldVarint 1"
  SSInt32 -> txt "encodeFieldSVarint32 1"
  SSInt64 -> txt "encodeFieldSVarint64 1"
  SFixed32 -> txt "encodeFieldFixed32 1"
  SFixed64 -> txt "encodeFieldFixed64 1"
  SSFixed32 -> txt "(\\x -> encodeFieldFixed32 1 (fromIntegral x))"
  SSFixed64 -> txt "(\\x -> encodeFieldFixed64 1 (fromIntegral x))"
  _ -> txt "encodeFieldBytes 1"


genMapValEncode :: GenCtx -> [Text] -> FieldType -> Doc ann
genMapValEncode ctx scope = \case
  FTScalar st -> genScalarFieldEncoder "2" st
  FTNamed n -> case resolveTypeWithScope ctx scope n of
    Just ti | tiKind ti == TKEnum -> txt "(\\x -> encodeFieldVarint 2 (fromIntegral (fromEnum x)))"
    _ -> txt "encodeFieldMessage 2"


genScalarFieldEncoder :: Text -> ScalarType -> Doc ann
genScalarFieldEncoder fn = \case
  SString -> call "encodeFieldString"
  SBytes -> call "encodeFieldBytes"
  SBool -> call "encodeFieldBool"
  SSInt32 -> call "encodeFieldSVarint32"
  SSInt64 -> call "encodeFieldSVarint64"
  SDouble -> call "encodeFieldDouble"
  SFloat -> call "encodeFieldFloat"
  SFixed32 -> call "encodeFieldFixed32"
  SFixed64 -> call "encodeFieldFixed64"
  SSFixed32 -> converted "encodeFieldFixed32"
  SSFixed64 -> converted "encodeFieldFixed64"
  _ -> converted "encodeFieldVarint"
  where
    call name = txt name <+> pretty fn
    converted name = parens (call name <+> txt ". fromIntegral")


genBuildExprOneof :: GenCtx -> [Text] -> Text -> Text -> OneofDef -> Doc ann
genBuildExprOneof ctx scope _fn accessor ood =
  txt "(case "
    <> pretty accessor
    <> txt " of"
    <> line
    <> indent
      2
      ( vsep
          ( txt "Nothing -> mempty"
              : fmap (genOneofArmEncode ctx scope (oneofName ood)) (oneofFields ood)
          )
      )
    <> txt ")"


genOneofArmEncode :: GenCtx -> [Text] -> Text -> OneofField -> Doc ann
genOneofArmEncode ctx scope ooName f =
  let conName = oneofConstructorName (gcOneofNames ctx) scope ooName (oneofFieldName f)
      fn = T.pack (show (unFieldNumber (oneofFieldNumber f)))
  in txt "Just ("
       <> pretty conName
       <+> txt "v) -> "
       <> case oneofFieldType f of
         FTScalar st -> genSingleScalarBuild fn "v" st
         FTNamed n -> genSingleNamedBuild fn (resolveTypeKindScoped ctx scope n)
           (messageDelimitedIn ctx scope (oneofFieldType f) (oneofFieldOptions f)) "v"


genSingleScalarBuild :: Text -> Text -> ScalarType -> Doc ann
genSingleScalarBuild fn accessor scalar
  | singleByteTag fn = genSingleScalarArch fn accessor scalar
  | otherwise = genScalarFieldEncoder fn scalar <+> pretty accessor


genSingleScalarArch :: Text -> Text -> ScalarType -> Doc ann
genSingleScalarArch fn accessor = \case
  SDouble -> txt "archDouble " <> tagLit fn wire64Bit <+> pretty accessor
  SFloat -> txt "archFloat " <> tagLit fn wire32Bit <+> pretty accessor
  SInt32 -> txt "archVarint " <> tagLit fn wireVarint <+> txt "(fromIntegral " <> pretty accessor <> txt ")"
  SInt64 -> txt "archVarint " <> tagLit fn wireVarint <+> txt "(fromIntegral " <> pretty accessor <> txt ")"
  SUInt32 -> txt "archVarint " <> tagLit fn wireVarint <+> txt "(fromIntegral " <> pretty accessor <> txt ")"
  SUInt64 -> txt "archVarint " <> tagLit fn wireVarint <+> pretty accessor
  SSInt32 -> txt "archSVarint32 " <> tagLit fn wireVarint <+> pretty accessor
  SSInt64 -> txt "archSVarint64 " <> tagLit fn wireVarint <+> pretty accessor
  SFixed32 -> txt "archFixed32 " <> tagLit fn wire32Bit <+> pretty accessor
  SFixed64 -> txt "archFixed64 " <> tagLit fn wire64Bit <+> pretty accessor
  SSFixed32 -> txt "archFixed32 " <> tagLit fn wire32Bit <+> txt "(fromIntegral " <> pretty accessor <> txt ")"
  SSFixed64 -> txt "archFixed64 " <> tagLit fn wire64Bit <+> txt "(fromIntegral " <> pretty accessor <> txt ")"
  SBool -> txt "archBool " <> tagLit fn wireVarint <+> pretty accessor
  SString -> txt "archString " <> tagLit fn wireLengthDelimited <+> pretty accessor
  SBytes -> txt "archBytes " <> tagLit fn wireLengthDelimited <+> pretty accessor


genRepeatedScalarBuild :: Text -> Text -> Bool -> ScalarType -> Doc ann
genRepeatedScalarBuild fn accessor packed s
  | packed = txt "encode" <> pretty (packedFnName s) <+> pretty fn <+> pretty accessor
  | otherwise =
      pretty (scalarVector s) <> txt ".foldl' (\\acc v -> acc <> " <> genSingleScalarBuild fn "v" s <> txt ") mempty " <> pretty accessor


scalarVector :: ScalarType -> Text
scalarVector s = if isUnboxableScalar s then "VU" else "V"


scalarDefaultCheck :: Text -> ScalarType -> Doc ann
scalarDefaultCheck accessor = \case
  SBool -> pretty accessor <+> txt "== False"
  SString -> pretty accessor <+> txt "== T.empty"
  SBytes -> txt "BS.null " <> pretty accessor
  SFloat -> txt "isDefaultFloat " <> pretty accessor
  SDouble -> txt "isDefaultDouble " <> pretty accessor
  _ -> pretty accessor <+> txt "== 0"


packedFnName :: ScalarType -> Text
packedFnName = \case
  SDouble -> "PackedDouble"
  SFloat -> "PackedFloat"
  SInt32 -> "PackedInt32"
  SInt64 -> "PackedInt64"
  SUInt32 -> "PackedWord32"
  SUInt64 -> "PackedWord64"
  SSInt32 -> "PackedSVarint32"
  SSInt64 -> "PackedSVarint64"
  SFixed32 -> "PackedFixed32"
  SFixed64 -> "PackedFixed64"
  SSFixed32 -> "PackedSFixed32"
  SSFixed64 -> "PackedSFixed64"
  SBool -> "PackedBool"
  s -> error ("Cannot pack: " <> show s)


-- ---------------------------------------------------------------------------
-- Size instances
-- ---------------------------------------------------------------------------

genSizeInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genSizeInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fields = extractAllFields ctx scope (msgElements msg)
      unknownAcc = "msg." <> ctxUnknownFieldName ctx scope
  in vsep
       [ instanceHead "MessageSize" tyN
       , indent 2 $
           vsep
             [ txt "messageSize msg ="
             , indent 2 $ case fields of
                 [] -> txt "unknownFieldsSize " <> pretty unknownAcc
                 _ ->
                   vsep
                     ( zipWith (genFieldSizeExpr ctx) [0 ..] fields
                         <> [txt "+ unknownFieldsSize " <> pretty unknownAcc]
                     )
             ]
       ]


genFieldSizeExpr :: GenCtx -> Int -> FieldInfoFull -> Doc ann
genFieldSizeExpr ctx idx fi =
  let op = if idx == 0 then mempty else txt "+ "
      accessor = "msg." <> fifAccessor fi
      fn = T.pack (show (fifFieldNum fi))
  in op <> case fifKind fi of
       FKScalar lbl ft -> genSizeScalar fn accessor lbl (fifPacked fi) ft
       FKNamed lbl _ tk -> genSizeNamed fn accessor lbl (fifPacked fi) (fifDelimited fi) tk
       FKMap scope keyT valT -> genSizeMap ctx scope fn accessor keyT valT
       FKOneof scope ood -> genSizeOneof ctx scope fn accessor ood


genSizeScalar :: Text -> Text -> Maybe FieldLabel -> Bool -> ScalarType -> Doc ann
genSizeScalar fn accessor lbl packed st = case lbl of
  Just Repeated
    | packed -> genRepeatedSizeScalar fn accessor st
    | otherwise ->
        parens (pretty (scalarVector st) <> txt ".foldl' (\\acc v -> acc + " <> genSingleSizeScalar fn "v" st <> txt ") 0 " <> pretty accessor)
  _ | explicitFieldPresence lbl ->
        txt "(maybe 0 (\\v -> " <> genSingleSizeScalar fn "v" st <> txt ") " <> pretty accessor <> txt ")"
  _ -> txt "(if " <> scalarDefaultCheck accessor st <> txt " then 0 else " <> genSingleSizeScalar fn accessor st <> txt ")"


genRepeatedSizeScalar :: Text -> Text -> ScalarType -> Doc ann
genRepeatedSizeScalar fn accessor = \case
  SString ->
    txt "(V.foldl' (\\acc v -> acc + fieldTextSize " <> pretty fn <+> txt "v) 0 " <> pretty accessor <> txt ")"
  SBytes ->
    txt "(V.foldl' (\\acc v -> acc + fieldBytesSize " <> pretty fn <+> txt "v) 0 " <> pretty accessor <> txt ")"
  SDouble ->
    txt "(let n = VU.length " <> pretty accessor <> txt " in if n == 0 then 0 else tagSize " <> pretty fn <> txt " + varintSize (fromIntegral (n * 8)) + n * 8)"
  SFloat ->
    txt "(let n = VU.length " <> pretty accessor <> txt " in if n == 0 then 0 else tagSize " <> pretty fn <> txt " + varintSize (fromIntegral (n * 4)) + n * 4)"
  SFixed32 ->
    txt "(let n = VU.length " <> pretty accessor <> txt " in if n == 0 then 0 else tagSize " <> pretty fn <> txt " + varintSize (fromIntegral (n * 4)) + n * 4)"
  SFixed64 ->
    txt "(let n = VU.length " <> pretty accessor <> txt " in if n == 0 then 0 else tagSize " <> pretty fn <> txt " + varintSize (fromIntegral (n * 8)) + n * 8)"
  SSFixed32 ->
    txt "(let n = VU.length " <> pretty accessor <> txt " in if n == 0 then 0 else tagSize " <> pretty fn <> txt " + varintSize (fromIntegral (n * 4)) + n * 4)"
  SSFixed64 ->
    txt "(let n = VU.length " <> pretty accessor <> txt " in if n == 0 then 0 else tagSize " <> pretty fn <> txt " + varintSize (fromIntegral (n * 8)) + n * 8)"
  SBool ->
    txt "(let n = VU.length " <> pretty accessor <> txt " in if n == 0 then 0 else tagSize " <> pretty fn <> txt " + varintSize (fromIntegral n) + n)"
  SUInt64 ->
    txt "(let pl = VU.foldl' (\\a v -> a + varintSize v) 0 " <> pretty accessor <> txt " in if pl == 0 then 0 else tagSize " <> pretty fn <> txt " + varintSize (fromIntegral pl) + pl)"
  SUInt32 ->
    txt "(let pl = VU.foldl' (\\a v -> a + varintSize32 v) 0 " <> pretty accessor <> txt " in if pl == 0 then 0 else tagSize " <> pretty fn <> txt " + varintSize (fromIntegral pl) + pl)"
  SInt32 ->
    txt "(let pl = VU.foldl' (\\a v -> a + varintSize (fromIntegral v)) 0 " <> pretty accessor <> txt " in if pl == 0 then 0 else tagSize " <> pretty fn <> txt " + varintSize (fromIntegral pl) + pl)"
  SInt64 ->
    txt "(let pl = VU.foldl' (\\a v -> a + varintSize (fromIntegral v)) 0 " <> pretty accessor <> txt " in if pl == 0 then 0 else tagSize " <> pretty fn <> txt " + varintSize (fromIntegral pl) + pl)"
  SSInt32 ->
    txt "(let pl = VU.foldl' (\\a v -> a + varintSize (fromIntegral (zigZag32 v))) 0 " <> pretty accessor <> txt " in if pl == 0 then 0 else tagSize " <> pretty fn <> txt " + varintSize (fromIntegral pl) + pl)"
  SSInt64 ->
    txt "(let pl = VU.foldl' (\\a v -> a + varintSize (zigZag64 v)) 0 " <> pretty accessor <> txt " in if pl == 0 then 0 else tagSize " <> pretty fn <> txt " + varintSize (fromIntegral pl) + pl)"


genSingleSizeScalar :: Text -> Text -> ScalarType -> Doc ann
genSingleSizeScalar fn accessor scalar = adjustTagSize fn (genSingleSizeScalarArch accessor scalar)


genSingleSizeScalarArch :: Text -> ScalarType -> Doc ann
genSingleSizeScalarArch accessor = \case
  SDouble -> txt "archFixed64Size"
  SFloat -> txt "archFixed32Size"
  SFixed32 -> txt "archFixed32Size"
  SFixed64 -> txt "archFixed64Size"
  SSFixed32 -> txt "archFixed32Size"
  SSFixed64 -> txt "archFixed64Size"
  SBool -> txt "archBoolSize"
  SInt32 -> txt "archVarintSize (fromIntegral " <> pretty accessor <> txt ")"
  SInt64 -> txt "archVarintSize (fromIntegral " <> pretty accessor <> txt ")"
  SUInt32 -> txt "archVarintSize (fromIntegral " <> pretty accessor <> txt ")"
  SUInt64 -> txt "archVarintSize " <> pretty accessor
  SSInt32 -> txt "(1 + varintSize (fromIntegral (zigZag32 " <> pretty accessor <> txt ")))"
  SSInt64 -> txt "(1 + varintSize (zigZag64 " <> pretty accessor <> txt "))"
  SString -> txt "archStringSize " <> pretty accessor
  SBytes -> txt "archBytesSize " <> pretty accessor


genSizeNamed :: Text -> Text -> Maybe FieldLabel -> Bool -> Bool -> TypeKind -> Doc ann
genSizeNamed fn accessor lbl packed delimited tk = case lbl of
  Just Repeated | packed -> txt "packedEnumSize" <+> pretty fn <+> pretty accessor
  Just Repeated -> txt "(V.foldl' (\\acc v -> acc + " <> size "v" <> txt ") 0 " <> pretty accessor <> txt ")"
  _ | tk == TKMessage || explicitFieldPresence lbl ->
        txt "(maybe 0 (\\v -> " <> size "v" <> txt ") " <> pretty accessor <> txt ")"
    | otherwise ->
        txt "(if fromEnum " <> pretty accessor <> txt " == 0 then 0 else " <> size accessor <> txt ")"
  where
    size = genSingleSizeNamed fn tk delimited


genSingleSizeNamed :: Text -> TypeKind -> Bool -> Text -> Doc ann
genSingleSizeNamed fn _ True accessor = txt "fieldGroupSize" <+> pretty fn <+> pretty accessor
genSingleSizeNamed fn tk False accessor = adjustTagSize fn $ case tk of
  TKEnum -> txt "archVarintSize (fromIntegral (fromEnum" <+> pretty accessor <> txt "))"
  TKMessage -> txt "archSubmessageSize (messageSize" <+> pretty accessor <> txt ")"


genSizeMap :: GenCtx -> [Text] -> Text -> Text -> ScalarType -> FieldType -> Doc ann
genSizeMap ctx scope fn accessor keyT valT =
  let keySizeExpr = mapKeySizeExpr keyT
      valSizeExpr = mapValSizeExpr ctx scope valT
  in txt "(Map.foldlWithKey' (\\acc k v -> let entrySz = "
       <> keySizeExpr
       <> txt " + "
       <> valSizeExpr
       <> txt " in acc + tagSize "
       <> pretty fn
       <> txt " + varintSize (fromIntegral entrySz) + entrySz) 0 "
       <> pretty accessor
       <> txt ")"


mapKeySizeExpr :: ScalarType -> Doc ann
mapKeySizeExpr = \case
  SString -> txt "fieldTextSize 1 k"
  SBool -> txt "fieldBoolSize 1"
  SInt32 -> txt "fieldVarintSize 1 (fromIntegral k)"
  SInt64 -> txt "fieldVarintSize 1 (fromIntegral k)"
  SUInt32 -> txt "fieldVarintSize 1 (fromIntegral k)"
  SUInt64 -> txt "fieldVarintSize 1 k"
  SSInt32 -> txt "fieldSVarint32Size 1 k"
  SSInt64 -> txt "fieldSVarint64Size 1 k"
  SFixed32 -> txt "fieldFixed32Size 1"
  SFixed64 -> txt "fieldFixed64Size 1"
  SSFixed32 -> txt "fieldFixed32Size 1"
  SSFixed64 -> txt "fieldFixed64Size 1"
  _ -> txt "fieldBytesSize 1 k"


mapValSizeExpr :: GenCtx -> [Text] -> FieldType -> Doc ann
mapValSizeExpr ctx scope = \case
  FTScalar SSInt32 -> txt "fieldSVarint32Size 2 v"
  FTScalar SSInt64 -> txt "fieldSVarint64Size 2 v"
  FTScalar SString -> txt "fieldTextSize 2 v"
  FTScalar SBytes -> txt "fieldBytesSize 2 v"
  FTScalar SBool -> txt "fieldBoolSize 2"
  FTScalar SDouble -> txt "fieldDoubleSize 2"
  FTScalar SFloat -> txt "fieldFloatSize 2"
  FTScalar SFixed32 -> txt "fieldFixed32Size 2"
  FTScalar SFixed64 -> txt "fieldFixed64Size 2"
  FTScalar SSFixed32 -> txt "fieldFixed32Size 2"
  FTScalar SSFixed64 -> txt "fieldFixed64Size 2"
  FTScalar _s -> txt "fieldVarintSize 2 (fromIntegral v)"
  FTNamed n -> case resolveTypeWithScope ctx scope n of
    Just ti | tiKind ti == TKEnum -> txt "fieldVarintSize 2 (fromIntegral (fromEnum v))"
    _ -> txt "fieldMessageSize 2 (messageSize v)"


genSizeOneof :: GenCtx -> [Text] -> Text -> Text -> OneofDef -> Doc ann
genSizeOneof ctx scope _fn accessor ood =
  txt "(case "
    <> pretty accessor
    <> txt " of { Nothing -> 0"
    <> vsep
      ( fmap
          ( \f ->
              let conName = oneofConstructorName (gcOneofNames ctx) scope (oneofName ood) (oneofFieldName f)
                  ffn = T.pack (show (unFieldNumber (oneofFieldNumber f)))
              in txt "; Just ("
                   <> pretty conName
                   <+> txt "v) -> "
                   <> case oneofFieldType f of
                     FTScalar st -> genSingleSizeScalar ffn "v" st
                     FTNamed n -> genSingleSizeNamed ffn (resolveTypeKindScoped ctx scope n)
                       (messageDelimitedIn ctx scope (oneofFieldType f) (oneofFieldOptions f)) "v"
          )
          (oneofFields ood)
      )
    <> txt " })"


-- ---------------------------------------------------------------------------
-- Decode instances
-- ---------------------------------------------------------------------------

genDecodeInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genDecodeInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fields = extractAllFields ctx scope (msgElements msg)
      allAccs = fmap (\(i, _) -> "acc_" <> T.pack (show i)) (zip ([0 ..] :: [Int]) fields)
      unknownAcc = "acc_unknown_"
      allAccsWithUnknown = allAccs <> [unknownAcc]
      unknownFieldName = ctxUnknownFieldName ctx scope
      verifiedStrings = Map.findWithDefault [] (fqProtoName (gcPkg ctx) scope) (gcVerifiedStringExtensions ctx)
      unknownCases = fmap (genVerifiedStringCase allAccsWithUnknown) verifiedStrings
        <> [genDefaultDecodeCase allAccsWithUnknown]
  in vsep
       [ instanceHead "MessageDecode" tyN
       , indent 2 $
           vsep
             [ txt "{-# INLINE messageDecoder #-}"
             , txt "messageDecoder = "
                 <> txt "loop"
                 <+> hsep (fmap (pretty . fieldDefaultText ctx) fields)
                 <+> txt "[]"
             , indent 2 $ txt "where"
             , indent 4 $
                 vsep
                   [ txt "loop " <> hsep (fmap pretty allAccsWithUnknown) <+> txt "= withTagM"
                   , -- withTagM (Proto.Internal.Wire.Decode) is the CPS form of
                     -- 'getTagOrU': the EOI continuation produces the
                     -- final message, the tag continuation takes the
                     -- field number and wire type as raw 'Int' values
                     -- (no 'Tag' record allocation, no 'UMaybe' wrapper).
                     -- This avoids one boxing step per field on every
                     -- generated decoder. The previous shape
                     -- (@mTag <- getTagOrU; case mTag of ...@) is still
                     -- correct but pays for an unboxed-sum case-split
                     -- per field that this form fuses through the
                     -- continuation.
                     indent 2 $
                       vsep
                         [ txt "(pure ("
                             <> pretty tyN
                             <+> braces
                               ( hsep
                                   ( punctuate
                                       comma
                                       ( fmap
                                           ( \fi ->
                                               pretty (fifAccessor fi) <+> txt "=" <+> txt "acc_" <> pretty (T.pack (show (fifIndex fi)))
                                           )
                                           fields
                                           <> [pretty unknownFieldName <+> txt "= reverse " <> pretty unknownAcc]
                                       )
                                   )
                               )
                             <> txt "))"
                         , txt "(\\fn wt -> case fn of"
                         , indent 2 $ vsep (concatMap (genFieldDecodeCase ctx allAccsWithUnknown) fields <> unknownCases) <> txt ")"
                         ]
                   ]
             ]
       ]


genFieldDecodeCase :: GenCtx -> [Text] -> FieldInfoFull -> [Doc ann]
genFieldDecodeCase ctx allAccs fi =
  case fifKind fi of
    FKScalar lbl ft -> [genScalarDecodeCase allAccs fi lbl ft]
    FKNamed lbl _ tk -> [genNamedDecodeCase allAccs fi lbl tk]
    FKMap scope keyT valT -> [genMapDecodeCase ctx scope allAccs fi keyT valT]
    FKOneof scope ood -> concatMap (genOneofDecodeCase ctx scope (oneofName ood) allAccs fi) (oneofFields ood)


genScalarDecodeCase :: [Text] -> FieldInfoFull -> Maybe FieldLabel -> ScalarType -> Doc ann
genScalarDecodeCase allAccs fi lbl st =
  let expected = scalarWireType st
      decoder = scalarDecoderExpr st
  in case lbl of
       Just Repeated ->
         let packedDecoder = if packableScalar st then Just (scalarPackedDecoderExpr st) else Nothing
         in genRepeatedDecodeCase allAccs fi (scalarVector st) expected decoder packedDecoder
       _ ->
         let value = if explicitFieldPresence lbl then "(Just v)" else "v"
         in genWireMatch (fifFieldNum fi) expected False (txt "do") <> line
              <> indent 2
                (vsep [txt "v <-" <+> pretty decoder, txt "loop" <+> hsep (map pretty (replaceAt (fifIndex fi) value allAccs))])


-- A failed guard falls through to the shared unknown-field arm.
genWireMatch :: Int -> Int -> Bool -> Doc ann -> Doc ann
genWireMatch field expected allowDelimited body =
  let extra = if allowDelimited then txt " || wt == 2" else mempty
  in pretty field <+> txt "| wt ==" <+> pretty expected <> extra <+> txt "->" <+> body


{- | Decoder expression for a 'repeated <scalar>' encoded as a
single packed length-delimited blob.
-}
scalarPackedDecoderExpr :: ScalarType -> Text
scalarPackedDecoderExpr = \case
  SDouble -> "decodePackedDouble"
  SFloat -> "decodePackedFloat"
  SInt32 -> "(VU.map fromIntegral <$> decodePackedVarint)"
  SInt64 -> "(VU.map fromIntegral <$> decodePackedVarint)"
  SUInt32 -> "(VU.map fromIntegral <$> decodePackedVarint)"
  SUInt64 -> "decodePackedVarint"
  SSInt32 -> "decodePackedSVarint32"
  SSInt64 -> "decodePackedSVarint64"
  SFixed32 -> "decodePackedFixed32"
  SFixed64 -> "decodePackedFixed64"
  SSFixed32 -> "(VU.map fromIntegral <$> decodePackedFixed32)"
  SSFixed64 -> "(VU.map fromIntegral <$> decodePackedFixed64)"
  SBool -> "(VU.map (/= 0) <$> decodePackedVarint)"
  -- 'repeated string' / 'repeated bytes' aren't packable per the
  -- proto spec; they always come as one-tag-per-entry. Calling this
  -- helper for them is a codegen bug; emit a runtime fail to surface
  -- it loudly rather than silently mis-decoding.
  SString -> "(decodeFail (CustomError \"repeated string is never packed\"))"
  SBytes -> "(decodeFail (CustomError \"repeated bytes is never packed\"))"


genNamedDecodeCase :: [Text] -> FieldInfoFull -> Maybe FieldLabel -> TypeKind -> Doc ann
genNamedDecodeCase allAccs fi lbl TKEnum =
  let idx = fifIndex fi
      fn = fifFieldNum fi
      acc = allAccs !! idx
      value = if explicitFieldPresence lbl then "(Just v)" else "v"
      decode valueExpr = genProtoEnumDispatch fn allAccs (replaceAt idx valueExpr allAccs)
      packedAccs = init (replaceAt idx ("(" <> acc <> " <> vs)") allAccs) <> ["ufs"]
  in case lbl of
       Just Repeated ->
         genWireMatch fn wireVarint True (txt "case wt of") <> line
           <> indent 2
             (vsep
               [ txt "2 -> do"
               , indent 2 (txt "PackedProtoEnum vs ufs <- decodePackedProtoEnum" <+> pretty fn <+> pretty (last allAccs))
               , indent 2 (txt "loop" <+> hsep (map pretty packedAccs))
               , txt "_ ->" <+> decode ("(" <> acc <> " <> V.singleton v)")
               ])
       _ -> genWireMatch fn wireVarint False (decode value)
genNamedDecodeCase allAccs fi lbl TKMessage =
  let idx = fifIndex fi
      fn = fifFieldNum fi
      (expected, repeatedDecoder, mergeDecoder)
        | fifDelimited fi = (wireStartGroup, "decodeFieldGroup " <> tshow fn, "decodeFieldGroupMerge " <> tshow fn)
        | otherwise = (wireLengthDelimited, "decodeFieldMessage", "decodeFieldMessageMerge")
      decoder = mergeDecoder <> " acc_" <> tshow idx
  in case lbl of
       Just Repeated -> genRepeatedDecodeCase allAccs fi "V" expected repeatedDecoder Nothing
       _ -> genWireMatch fn expected False (txt "do") <> line
         <> indent 2
           (vsep [txt "v <-" <+> pretty decoder, txt "loop" <+> hsep (map pretty (replaceAt idx "(Just v)" allAccs))])


genProtoEnumDispatch :: Int -> [Text] -> [Text] -> Doc ann
genProtoEnumDispatch fn allAccs known =
  txt "decodeProtoEnum" <+> pretty fn <> line
    <> indent 2 (vsep [genDecodeContinuation "v" known, genUnknownContinuation allAccs])


genDecodeContinuation :: Text -> [Text] -> Doc ann
genDecodeContinuation binders accs =
  parens (txt "\\" <> pretty binders <+> txt "-> loop" <+> hsep (map pretty accs))


genUnknownContinuation :: [Text] -> Doc ann
genUnknownContinuation allAccs =
  genDecodeContinuation "uf" (init allAccs <> ["(uf : " <> last allAccs <> ")"])


-- | Repeated packable fields accept both wire representations, regardless
-- of the encoding selected by the schema.
genRepeatedDecodeCase :: [Text] -> FieldInfoFull -> Text -> Int -> Text -> Maybe Text -> Doc ann
genRepeatedDecodeCase allAccs fi vec expected decoder packedDecoder =
  let idx = fifIndex fi
      acc = "acc_" <> tshow idx
      newAccs value = replaceAt idx ("(" <> acc <> " <> " <> value <> ")") allAccs
      step name dec next =
        indent 2 (vsep [name <+> txt "<-" <+> pretty dec, txt "loop" <+> hsep next])
      unpacked = step (txt "v") decoder
        (map pretty (newAccs (vec <> ".singleton v")))
      packed dec = step (txt "vs") dec (map pretty (newAccs "vs"))
      header = genWireMatch (fifFieldNum fi) expected (isJust packedDecoder)
  in case packedDecoder of
       Nothing -> header (txt "do") <> line <> unpacked
       Just dec -> header (txt "case wt of") <> line <> indent 2
         (vsep [txt "2 -> do", packed dec, txt "_ -> do", unpacked])


genMapDecodeCase :: GenCtx -> [Text] -> [Text] -> FieldInfoFull -> ScalarType -> FieldType -> Doc ann
genMapDecodeCase ctx scope allAccs fi keyT valT =
  let idx = fifIndex fi
      accName = "acc_" <> tshow idx
      newAccs = replaceAt idx ("(Map.insert mk' mv' " <> accName <> ")") allAccs
      keyDecoder = pretty (scalarWireType keyT) <+> pretty (scalarDecoderExpr keyT)
      decodeMap entryDecoder valDefault =
        genWireMatch (fifFieldNum fi) wireLengthDelimited False (txt "do")
          <> line
          <> indent 2
            (vsep
              [ txt "(mk', mv') <- decodeLengthDelimitedWith"
                  <+> parens (entryDecoder <+> pretty (scalarDefaultLit keyT) <+> txt valDefault)
              , txt "loop" <+> hsep (map pretty newAccs)
              ])
  in case valT of
       FTScalar scalar ->
         let entryDecoder = txt "decodeMapEntryTyped" <+> keyDecoder
               <+> pretty (scalarWireType scalar) <+> pretty (scalarDecoderExpr scalar)
         in decodeMap entryDecoder (scalarDefaultLit scalar)
       FTNamed n -> case resolveTypeKindScoped ctx scope n of
         TKMessage -> decodeMap (txt "decodeMessageMapEntryTyped" <+> keyDecoder) "protoDefaultValue"
         TKEnum ->
           genWireMatch (fifFieldNum fi) wireLengthDelimited False
             (txt "decodeProtoEnumMapField" <+> pretty (fifFieldNum fi) <+> keyDecoder
               <+> pretty (scalarDefaultLit keyT) <+> txt "(toEnum 0)")
             <> line
             <> indent 2 (vsep [genDecodeContinuation "mk' mv'" newAccs, genUnknownContinuation allAccs])


genOneofDecodeCase :: GenCtx -> [Text] -> Text -> [Text] -> FieldInfoFull -> OneofField -> [Doc ann]
genOneofDecodeCase ctx scope ooName allAccs fi oof =
  let fn = unFieldNumber (oneofFieldNumber oof)
      idx = fifIndex fi
      conName = oneofConstructorName (gcOneofNames ctx) scope ooName (oneofFieldName oof)
      newAccs = replaceAt idx ("(Just (" <> conName <> " v))") allAccs
      fieldType' = oneofFieldType oof
      delimited = messageDelimitedIn ctx scope fieldType' (oneofFieldOptions oof)
      expected = case fieldType' of
        FTScalar st -> scalarWireType st
        FTNamed _ | delimited -> wireStartGroup
        FTNamed n -> namedWireType (resolveTypeKindScoped ctx scope n)
      legacyNullValue = case fieldType' of
        FTNamed n -> isNullValueType ctx scope n
        _ -> False
      decoderExpr = case fieldType' of
        FTScalar st -> scalarDecoderExpr st
        FTNamed n
          | legacyNullValue -> "decodeFieldNullValue wt"
          | otherwise -> case resolveTypeKindScoped ctx scope n of
              TKEnum -> "decodeFieldEnum"
              TKMessage ->
                (if delimited then "decodeFieldGroupMerge " <> tshow fn else "decodeFieldMessageMerge")
                  <> " (case acc_" <> tshow idx
                  <> " of { Just (" <> conName <> " old) -> Just old; _ -> Nothing })"
  in [ case fieldType' of
         FTNamed n | not legacyNullValue && resolveTypeKindScoped ctx scope n == TKEnum ->
           genWireMatch fn expected False (genProtoEnumDispatch fn allAccs newAccs)
         _ ->
           genWireMatch fn expected legacyNullValue (txt "do")
             <> line
             <> indent 2
               (vsep
                 [ txt "v <- " <> pretty decoderExpr
                 , txt "loop " <> hsep (fmap pretty newAccs)
                 ])
     ]


genDefaultDecodeCase :: [Text] -> Doc ann
genDefaultDecodeCase accs =
  genCaptureUnknownCase accs (txt "_ -> do") (txt "captureUnknownField fn (toEnum wt)")


genVerifiedStringCase :: [Text] -> Int -> Doc ann
genVerifiedStringCase accs field =
  genCaptureUnknownCase accs (genWireMatch field wireLengthDelimited False (txt "do"))
    (txt "captureVerifiedStringExtension" <+> pretty field)


genCaptureUnknownCase :: [Text] -> Doc ann -> Doc ann -> Doc ann
genCaptureUnknownCase allAccsWithUnknown header capture =
  let unknownAcc = last allAccsWithUnknown
      fieldAccs = init allAccsWithUnknown
      newAccs = fieldAccs <> ["(uf : " <> unknownAcc <> ")"]
  in header
       <> line
       <> indent
         2
         ( vsep
             [ txt "uf <-" <+> capture
             , txt "loop " <> hsep (fmap pretty newAccs)
             ]
         )


fieldDefaultText :: GenCtx -> FieldInfoFull -> Text
fieldDefaultText _ctx fi = case fifKind fi of
  FKScalar lbl ft -> case lbl of
    Just Repeated | isUnboxableScalar ft -> "VU.empty"
    Just Repeated -> "V.empty"
    _ | explicitFieldPresence lbl -> "Nothing"
    _ -> scalarDefaultText ft
  FKNamed lbl _ tk -> case lbl of
    Just Repeated -> "V.empty"
    _ | explicitFieldPresence lbl -> "Nothing"
    _ -> case tk of
      TKEnum -> "(toEnum 0)"
      TKMessage -> "Nothing"
  FKMap _ _ _ -> "Map.empty"
  FKOneof _ _ -> "Nothing"


scalarDefaultText :: ScalarType -> Text
scalarDefaultText = \case
  SBool -> "False"
  SString -> "\"\""
  SBytes -> "\"\""
  _ -> "0"


scalarDecoderExpr :: ScalarType -> Text
scalarDecoderExpr = \case
  SDouble -> "decodeFieldDouble"
  SFloat -> "decodeFieldFloat"
  SInt32 -> "(fromIntegral <$> decodeFieldVarint)"
  SInt64 -> "(fromIntegral <$> decodeFieldVarint)"
  SUInt32 -> "(fromIntegral <$> decodeFieldVarint)"
  SUInt64 -> "decodeFieldVarint"
  SSInt32 -> "decodeFieldSVarint32"
  SSInt64 -> "decodeFieldSVarint64"
  SFixed32 -> "decodeFieldFixed32"
  SFixed64 -> "decodeFieldFixed64"
  SSFixed32 -> "(fromIntegral <$> decodeFieldFixed32)"
  SSFixed64 -> "(fromIntegral <$> decodeFieldFixed64)"
  SBool -> "decodeFieldBool"
  SString -> "decodeFieldString"
  SBytes -> "decodeFieldBytes"


scalarDefaultLit :: ScalarType -> Text
scalarDefaultLit = \case
  SBool -> "False"
  SString -> "\"\""
  SBytes -> "\"\""
  _ -> "0"


-- ---------------------------------------------------------------------------
-- JSON instances
-- ---------------------------------------------------------------------------

{- | Generate an empty @instance IsMessage Foo@. 'IsMessage' is a marker
class with no methods of its own; its superclasses ('MessageEncode',
'MessageDecode', 'ProtoMessage', 'Aeson.ToJSON', 'Aeson.FromJSON',
'Typeable') do the work. Emitting an instance signals to the registry
TH discovery splice that this type is eligible for inclusion.
-}
genIsMessageInstance :: GenCtx -> [Text] -> Doc ann
genIsMessageInstance ctx scope =
  let tyN = scopedTypeName scope
      context = if needsRegistry ctx scope then txt "Given PJExt.ExtensionRegistry => " else mempty
  in txt "instance " <> context <> txt "IsMessage " <> pretty tyN


genProtoMessageInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genProtoMessageInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fqn = fqProtoName (gcPkg ctx) scope
      pkg = fromMaybe "" (gcPkg ctx)
      fields = extractMessageFieldsForSchema ctx scope (msgElements msg)
      defN = "default" <> tyN
  in vsep
       [ instanceHead "ProtoMessage" tyN
       , indent 2 $ pretty ("protoMessageName _ = \"" :: Text) <> pretty fqn <> pretty ("\"" :: Text)
       , indent 2 $ pretty ("protoPackageName _ = \"" :: Text) <> pretty pkg <> pretty ("\"" :: Text)
       , indent 2 $ txt "protoDefaultValue = " <> pretty defN
       , indent 2 $ txt "protoFileDescriptorBytes _ = fileDescriptorProtoBytes"
       , indent 2 $
           vsep
             [ txt "protoFieldDescriptors _ = Map.fromList"
             , indent 2 $ genFieldDescriptorList ctx scope fields
             ]
       ]
       <> (case concatMap oneofMetadata (msgElements msg) of
             [] -> mempty
             groups -> line <> indent 2 (txt "protoOneofDescriptors _ =" <> line <> indent 2 (list groups)))
       <> (case mapMaybe closedEnumMetadata (msgElements msg) of
             [] -> mempty
             enums -> line <> indent 2
               (txt "protoClosedEnumFields _ = Map.mapMaybe id (Map.fromList" <> line <> indent 2 (list enums <> txt ")")))
       <> (case submessageEntries ctx scope (msgElements msg) of
             [] -> mempty
             children -> line <> indent 2
               (txt "protoSubmessageSchemas _ = Map.fromList" <> line <> indent 2 (list children)))
       <> (case mapMaybe mapValueDecoder (msgElements msg) of
             [] -> mempty
             validators -> line <> indent 2
               (txt "protoMapValueDecoders _ = Map.fromList" <> line <> indent 2 (list validators)))
       <> if fqn == "google.protobuf.Any" && builtInAnyOverride
            then line <> indent 2 (txt "protoJSONIgnoreUnknownEnumNames _ = WK.normalizeAnyEnumJSON WK.anyJsonRegistry")
            else case normalizeFields of
              [] -> mempty
              rules ->
                line <> indent 2
                  (txt "protoJSONIgnoreUnknownEnumNames _ = PJ.normalizeEnumJSONFields" <> line <> indent 2 (list rules))
  where
    mapValueDecoder (MEMapField field)
      | FTNamed name <- mapValueType field
      , Just info <- resolveTypeWithScope ctx scope name
      , tiKind info == TKMessage =
          Just (tupled
            [ pretty (unFieldNumber (mapFieldNum field))
            , txt "() <$ (Proto.Decode.messageDecoder :: Proto.Decode.Decoder" <+> pretty (resolveHsTypeNameScoped ctx scope name) <> txt ")"
            ])
    mapValueDecoder _ = Nothing

    oneofMetadata (MEOneof oneof) =
      [txt "PSchema.OneofDescriptor" <+> viaShow (oneofName oneof) <+> list (map member (oneofFields oneof))]
    oneofMetadata _ = []
    member field =
      let ft = oneofFieldType field
          descriptor = case ft of
            FTNamed name | isNullValueType ctx scope name -> txt "EnumType \"google.protobuf.NullValue\""
            _ -> genWireFieldTypeDesc ctx scope ft (oneofFieldOptions field) False
      in txt "PSchema.OneofFieldDescriptor" <+> viaShow (oneofFieldName field)
           <+> pretty (unFieldNumber (oneofFieldNumber field))
           <+> parens descriptor
           <+> maybe (txt "Nothing") parens (closedEnumValues ft)
    closedEnumMetadata (MEField field) = enumEntry (fieldNumber field) (fieldType field)
    closedEnumMetadata (MEMapField field) = enumEntry (mapFieldNum field) (mapValueType field)
    closedEnumMetadata _ = Nothing
    enumEntry number ft =
      fmap (\values -> tupled [pretty (unFieldNumber number), values]) (closedEnumValues ft)
    closedEnumValues (FTNamed name) | resolveTypeKindScoped ctx scope name == TKEnum =
      let proxy = txt "(Proxy :: Proxy " <> pretty (resolveHsTypeNameScoped ctx scope name) <> txt ")"
      in Just (txt "if protoEnumIsOpen" <+> proxy
           <+> txt "then Nothing else Just (map snd (protoEnumValues" <+> proxy <> txt "))")
    closedEnumValues _ = Nothing
    builtInAnyOverride =
      Map.lookup "google.protobuf.Any" (genJsonOverrides (gcOpts ctx))
        == Map.lookup "google.protobuf.Any" defaultJsonOverrides
    normalizeFields
      | Map.member (fqProtoName (gcPkg ctx) scope) (genJsonOverrides (gcOpts ctx)) = []
      | otherwise = mapMaybe rule (namedJSONFields (msgElements msg))
    rule field = do
      let name = njfType field
      info <- resolveTypeWithScope ctx scope name
      let proxy = txt "(Proxy :: Proxy " <> pretty (resolveHsTypeNameScoped ctx scope name) <> txt ")"
          normalizeValue = case tiKind info of
            TKEnum -> parens (txt "PJ.normalizeEnumJSONValue" <+> proxy)
            TKMessage -> parens (txt "Just . protoJSONIgnoreUnknownEnumNames" <+> proxy)
          normalize = case njfShape field of
            NamedJSONSingle -> normalizeValue
            NamedJSONRepeated -> parens (txt "PJ.normalizeEnumJSONArray" <+> normalizeValue)
            NamedJSONMap -> parens (txt "PJ.normalizeEnumJSONMap" <+> normalizeValue)
      pure (txt "PJ.EnumJSONField" <+> viaShow (njfJsonName field) <+> viaShow (njfProtoName field) <+> normalize)


submessageEntries :: GenCtx -> [Text] -> [MessageElement] -> [Doc ann]
submessageEntries ctx scope = concatMap $ \case
  MEField field -> mapMaybe id [entry (fieldNumber field) (fieldType field)]
  MEOneof oneof -> mapMaybe (\field -> entry (oneofFieldNumber field) (oneofFieldType field)) (oneofFields oneof)
  _ -> []
  where
    entry number (FTNamed name)
      | Just info <- resolveTypeWithScope ctx scope name, tiKind info == TKMessage =
          Just (tupled [pretty (unFieldNumber number),
            txt "PSchema.SomeProtoMessage (Proxy :: Proxy" <+> pretty (resolveHsTypeNameScoped ctx scope name) <> txt ")"])
    entry _ _ = Nothing


data NamedJSONShape = NamedJSONSingle | NamedJSONRepeated | NamedJSONMap


data NamedJSONField = NamedJSONField
  { njfProtoName :: !Text
  , njfJsonName :: !Text
  , njfType :: !Text
  , njfShape :: !NamedJSONShape
  }


-- | Named fields that can contain an enum name, directly or in a submessage.
namedJSONFields :: [MessageElement] -> [NamedJSONField]
namedJSONFields = concatMap $ \case
  MEField fd ->
    let shape = if fieldLabel fd == Just Repeated then NamedJSONRepeated else NamedJSONSingle
    in named shape (fieldName fd) (fieldOptions fd) (fieldType fd)
  MEMapField mf ->
    named NamedJSONMap (mapFieldName mf) (mapOptions mf) (mapValueType mf)
  MEOneof od ->
    concatMap (\f -> named NamedJSONSingle (oneofFieldName f) (oneofFieldOptions f) (oneofFieldType f)) (oneofFields od)
  _ -> []
  where
    named shape protoName opts (FTNamed ty) =
      [NamedJSONField protoName (fromMaybe (protoJsonName protoName) (getJsonName opts)) ty shape]
    named _ _ _ _ = []


data SchemaField ann = SchemaField
  { sfName :: Text
  , sfNum :: Int
  , sfType :: Doc ann
  , sfLabel :: Maybe FieldLabel
  }


extractMessageFieldsForSchema :: GenCtx -> [Text] -> [MessageElement] -> [SchemaField ann]
extractMessageFieldsForSchema ctx scope = concatMap go
  where
    go (MEField fd) =
      [ SchemaField
          (fieldName fd)
          (unFieldNumber (fieldNumber fd))
          (genWireFieldTypeDesc ctx scope (fieldType fd) (fieldOptions fd) (isLegacyGroupField fd))
          (fieldLabel fd)
      ]
    go (MEMapField mf) =
      [ SchemaField
          (mapFieldName mf)
          (unFieldNumber (mapFieldNum mf))
          (txt "MapType" <+> genScalarFieldTypeDesc (mapKeyType mf) <+> parens (genFieldTypeDesc ctx scope (mapValueType mf)))
          (Just Repeated)
      ]
    go (MEOneof od) = case oneofFields od of
      (f : _) -> [SchemaField (oneofName od) (unFieldNumber (oneofFieldNumber f)) (txt "MessageType" <+> viaShow (oneofName od)) (Just Optional)]
      [] -> []
    go _ = []


genFieldDescriptorList :: GenCtx -> [Text] -> [SchemaField ann] -> Doc ann
genFieldDescriptorList ctx scope fields =
  let entries = fmap (genFieldDescriptorEntry ctx scope) fields
  in case entries of
       [] -> txt "[]"
       (e : es) ->
         vsep [txt "[ " <> e]
           <> vsep (fmap (\x -> txt ", " <> x) es)
           <> line
           <> txt "]"


genFieldDescriptorEntry :: GenCtx -> [Text] -> SchemaField ann -> Doc ann
genFieldDescriptorEntry ctx scope sf =
  let accN = ctxFieldName ctx scope (sfName sf)
      -- Under 'UnprefixedFields' the record fields are bare (and
      -- 'NoFieldSelectors' is on), so a bare selector for 'fdGet' does not
      -- exist and an un-annotated update for 'fdSet' is ambiguous when the
      -- same field name recurs across messages in the file. Both are made
      -- concrete by fixing the message type: a record-dot read and an
      -- update with an explicitly typed result. Under 'PrefixedFields' the
      -- selectors are unique, so the plain forms are kept byte-identical.
      tyN = scopedTypeName scope
      (getExpr, setExpr) = case genFieldNaming (gcOpts ctx) of
        UnprefixedFields ->
          ( "\\(m :: " <> tyN <> ") -> m." <> accN
          , "\\v m -> (m { " <> accN <> " = v } :: " <> tyN <> ")"
          )
        PrefixedFields ->
          ( accN
          , "\\v m -> m { " <> accN <> " = v }"
          )
  in txt "("
       <> pretty (T.pack (show (sfNum sf)))
       <> txt ", SomeField FieldDescriptor"
       <> line
       <> indent
         4
         ( vsep
             [ pretty ("{ fdName = \"" :: Text) <> pretty (sfName sf) <> pretty ("\"" :: Text)
             , txt ", fdNumber = " <> pretty (T.pack (show (sfNum sf)))
             , txt ", fdTypeDesc = " <> sfType sf
             , txt ", fdLabel = " <> genLabelLit (sfLabel sf)
             , txt ", fdGet = " <> pretty getExpr
             , txt ", fdSet = " <> pretty setExpr
             , txt "})"
             ]
         )


genWireFieldTypeDesc :: GenCtx -> [Text] -> FieldType -> [OptionDef] -> Bool -> Doc ann
genWireFieldTypeDesc ctx scope ft@(FTNamed name) opts legacy
  | legacy || messageDelimitedIn ctx scope ft opts = txt "GroupType" <+> viaShow name
genWireFieldTypeDesc ctx scope ft _ _ = genFieldTypeDesc ctx scope ft


genFieldTypeDesc :: GenCtx -> [Text] -> FieldType -> Doc ann
genFieldTypeDesc ctx scope = \case
  FTScalar scalar -> txt "ScalarType" <+> genScalarFieldTypeDesc scalar
  FTNamed n
    | resolveTypeKindScoped ctx scope n == TKEnum -> txt "EnumType" <+> viaShow n
    | otherwise -> txt "MessageType" <+> viaShow n


genScalarFieldTypeDesc :: ScalarType -> Doc ann
genScalarFieldTypeDesc = \case
  SDouble -> txt "DoubleField"
  SFloat -> txt "FloatField"
  SInt32 -> txt "Int32Field"
  SInt64 -> txt "Int64Field"
  SUInt32 -> txt "UInt32Field"
  SUInt64 -> txt "UInt64Field"
  SSInt32 -> txt "SInt32Field"
  SSInt64 -> txt "SInt64Field"
  SFixed32 -> txt "Fixed32Field"
  SFixed64 -> txt "Fixed64Field"
  SSFixed32 -> txt "SFixed32Field"
  SSFixed64 -> txt "SFixed64Field"
  SBool -> txt "BoolField"
  SString -> txt "StringField"
  SBytes -> txt "BytesField"


genLabelLit :: Maybe FieldLabel -> Doc ann
genLabelLit Nothing = txt "LabelOptional"
genLabelLit (Just Optional) = txt "LabelOptional"
genLabelLit (Just Required) = txt "LabelRequired"
genLabelLit (Just Repeated) = txt "LabelRepeated"


-- ---------------------------------------------------------------------------
-- JSON instances
-- ---------------------------------------------------------------------------

hasExtensions :: MessageDef -> Bool
hasExtensions msg = any isExt (msgElements msg)
  where
    isExt (MEExtensions _ _) = True
    isExt _ = False


needsRegistry :: GenCtx -> [Text] -> Bool
needsRegistry ctx scope = Set.member (fqProtoName (gcPkg ctx) scope) (gcRegistryMessages ctx)


mapScopedMessages :: ([Text] -> MessageDef -> a) -> [TopLevel] -> [a]
mapScopedMessages select = concatMap top
  where
    top (TLMessage msg) = nested [] msg
    top _ = []
    nested parent msg = select scope msg : concatMap child (msgElements msg)
      where
        scope = parent <> [msgName msg]
        child (MEMessage inner) = nested scope inner
        child _ = []


-- Walk from each extendable message to the messages that embed it.
-- Reaching a message once is enough, even when schemas are recursive.
extensionRegistryRequirements
  :: Ord key => [(key, MessageDef)] -> (key -> Text -> Maybe key) -> Set key -> Set key
extensionRegistryRequirements messages resolve inherited =
  visit Set.empty (Set.toList seeds)
  where
    seeds = Set.union inherited (Set.fromList (mapMaybe ownExtensions messages))
    ownExtensions (key, msg) = if hasExtensions msg then Just key else Nothing
    parents = Map.fromListWith (<>)
      (concatMap (\(key, msg) -> mapMaybe (parent key) (namedJSONFields (msgElements msg))) messages)
    parent key field = fmap (\child -> (child, [key])) (resolve key (njfType field))
    visit seen [] = seen
    visit seen (key : rest)
      | Set.member key seen = visit seen rest
      | otherwise = visit (Set.insert key seen) (Map.findWithDefault [] key parents <> rest)


registryRequirementsForFiles :: TypeRegistry -> [ProtoFile] -> Set Text
registryRequirementsForFiles registry files =
  extensionRegistryRequirements (fmap fstAndMessage entries) resolve inherited
  where
    entries = concatMap fileEntries files
    fileEntries file =
      let pkg = protoPackage file
      in mapScopedMessages (\scope msg -> (fqProtoName pkg scope, pkg, scope, msg)) (protoTopLevels file)
    fstAndMessage (key, _, _, msg) = (key, msg)
    contexts = Map.fromList (fmap (\(key, pkg, scope, _) -> (key, (pkg, scope))) entries)
    resolve key name = do
      (pkg, scope) <- Map.lookup key contexts
      resolveTypeKey registry pkg scope name
    inherited = registryMessageNames registry


registryMessageNames :: TypeRegistry -> Set Text
registryMessageNames = Map.keysSet . Map.filter (\(TypeInfoWithRegistry _ _ _ required) -> required)


genToJSONInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genToJSONInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fqn = fqProtoName (gcPkg ctx) scope
      overrides = genJsonOverrides (gcOpts ctx)
      hasExts = hasExtensions msg
      headDoc =
        if needsRegistry ctx scope
          then txt "instance Given PJExt.ExtensionRegistry => Aeson.ToJSON " <> pretty tyN <> txt " where"
          else instanceHead "Aeson.ToJSON" tyN
  in case Map.lookup fqn overrides of
       Just jo ->
         vsep
           [ headDoc
           , pretty (joToJSON jo)
           ]
       Nothing ->
         let fields = extractAllFieldsJSON ctx scope (msgElements msg)
         in vsep
              [ headDoc
              , indent 2 $
                  vsep
                    [ txt "toJSON msg = jsonObject"
                    , indent 4 $ case fields of
                        [] ->
                          if hasExts
                            then txt "(PJExt.extensionEntriesForJson (given :: PJExt.ExtensionRegistry) " <> pretty ("\"" :: Text) <> pretty fqn <> pretty ("\" msg." :: Text) <> txt (ctxUnknownFieldName ctx scope) <> txt ")"
                            else txt "[]"
                        (f0 : fs) ->
                          let prefix =
                                if hasExts
                                  then txt "(PJExt.extensionEntriesForJson (given :: PJExt.ExtensionRegistry) " <> pretty ("\"" :: Text) <> pretty fqn <> pretty ("\" msg." :: Text) <> txt (ctxUnknownFieldName ctx scope) <> txt " ++ concat"
                                  else txt "(concat"
                          in vsep
                               [ prefix
                               , indent 2 $ vsep
                                   [ txt "[ " <> genToJSONEntry f0
                                   , vsep (fmap (\f -> txt ", " <> genToJSONEntry f) fs)
                                   , txt "])"
                                   ]
                               ]
                    ]
              ]


genToJSONEntry :: JSONFieldInfo -> Doc ann
genToJSONEntry jfi = case jfiKind jfi of
  JFKBytes ->
    skipEmpty "BS.null" (txt "bytesFieldToJSON" <+> key <+> field)
  JFKOptionalBytes ->
    txt "maybe [] (\\value -> " <> brackets (txt "bytesFieldToJSON" <+> key <+> txt "value") <> txt ") " <> field
  JFKRepeatedBytes ->
    skipEmpty "V.null" (key <> txt " .=: Aeson.Array (V.map protoBytesToJSON " <> field <> txt ")")
  JFKNumericRepeated vector encode _ ->
    skipEmpty (vector <> ".null") (tupled [key, txt "PJ.repeatedVectorToJSON" <+> pretty encode <+> field])
  JFKBytesMap ->
    skipEmpty "Map.null" (txt "bytesMapFieldToJSON" <+> key <+> field)
  JFKNumericMap encode _ ->
    skipEmpty "Map.null" (tupled [key, txt "PJ.mapValuesToJSON" <+> pretty encode <+> field])
  JFKContainer isEmpty ->
    skipEmpty isEmpty (key <> txt " .=: " <> field)
  JFKOptional _ ->
    txt "maybe [] (\\value -> " <> brackets (key <> txt " .=: value") <> txt ") " <> field
  JFKImplicitEnum ->
    txt "if fromEnum " <> field <> txt " == 0 then [] else " <> brackets (key <> txt " .=: " <> field)
  JFKNormal ->
    brackets (key <> txt " .=: " <> field)
  JFKNumeric explicit encode _ ->
    let entry value = brackets (tupled [key, pretty encode <+> value])
    in if explicit
         then txt "maybe [] (\\value -> " <> entry (txt "value") <> txt ") " <> field
         else txt "if " <> field <> txt " == 0 then [] else " <> entry field
  JFKOneof variants ->
    txt "case " <> field <> txt " of"
      <> line
      <> indent 4
        (vsep (txt "{ Nothing -> []" : map genVariant variants <> [txt "}"]))
  where
    key = viaShow (jfiJsonName jfi)
    field = txt "msg." <> pretty (jfiAccessor jfi)
    skipEmpty predicate entry = txt "if" <+> txt predicate <+> field <+> txt "then [] else" <+> brackets entry
    genVariant variant =
      txt "; Just" <+> parens (pretty (jvConstructor variant) <+> txt "v") <+> txt "->"
        <+> brackets
          (tupled [viaShow (jvJsonName variant), pretty (jvEncoder variant) <+> txt "v"])


genFromJSONInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genFromJSONInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fqn = fqProtoName (gcPkg ctx) scope
      overrides = genJsonOverrides (gcOpts ctx)
      hasExts = hasExtensions msg
      headDoc =
        if needsRegistry ctx scope
          then txt "instance Given PJExt.ExtensionRegistry => Aeson.FromJSON " <> pretty tyN <> txt " where"
          else instanceHead "Aeson.FromJSON" tyN
  in case Map.lookup fqn overrides of
       Just jo ->
         vsep
           [ headDoc
           , pretty (joFromJSON jo)
           ]
       Nothing ->
         let fields = extractAllFieldsJSON ctx scope (msgElements msg)
             extsAssign = if hasExts then txt "exts" else txt "[]"
             extsBind =
               if hasExts
                 then [txt "exts <- either fail pure (PJExt.parseExtensionsFromJson (given :: PJExt.ExtensionRegistry) " <> pretty ("\"" :: Text) <> pretty fqn <> pretty ("\" obj)" :: Text)]
                 else []
         in case fields of
              [] ->
                vsep
                  [ headDoc
                  , indent 2 $
                      if hasExts
                        then
                          txt "parseJSON = Aeson.withObject "
                            <> pretty ("\"" :: Text)
                            <> pretty tyN
                            <> pretty ("\" $ \\obj -> do" :: Text)
                            <> line
                            <> indent 4 (vsep (extsBind <> [txt "pure default" <> pretty tyN, indent 2 (braceBlock [txt (ctxUnknownFieldName ctx scope) <+> txt "=" <+> extsAssign])]))
                        else txt "parseJSON _ = pure default" <> pretty tyN
                  ]
              (f0 : frest) ->
                -- Under 'UnprefixedFields' a record update whose fields recur
                -- across messages is ambiguous; pin it with a trailing type
                -- annotation. 'PrefixedFields' updates are unique, so they are
                -- left byte-identical.
                let naming = genFieldNaming (gcOpts ctx)
                    assignField = genFromJSONFieldAssign naming tyN
                    (pureOpen, closeBrace) = case naming of
                      UnprefixedFields -> (txt "pure (default" <> pretty tyN, txt "} :: " <> pretty tyN <> txt ")")
                      PrefixedFields -> (txt "pure default" <> pretty tyN, txt "}")
                in vsep
                  [ headDoc
                  , indent 2 $
                      vsep
                        [ txt "parseJSON = Aeson.withObject " <> pretty ("\"" :: Text) <> pretty tyN <> pretty ("\" $ \\obj -> do" :: Text)
                        , indent 2 $
                            vsep
                              ( fmap genFromJSONFieldBind (f0 : frest)
                                  <> extsBind
                                  <> [ pureOpen
                                     , indent 2 $
                                         vsep
                                           ( txt "{ " <> assignField f0
                                               : fmap (\jfi -> txt ", " <> assignField jfi) frest
                                                 <> [txt ", " <> txt (ctxUnknownFieldName ctx scope) <+> txt "=" <+> extsAssign]
                                                 <> [closeBrace]
                                           )
                                     ]
                              )
                        ]
                  ]


genFromJSONFieldBind :: JSONFieldInfo -> Doc ann
genFromJSONFieldBind jfi = case jfiKind jfi of
  JFKBytes -> field "parseBytesFieldMaybe" []
  JFKOptionalBytes -> field "parseBytesFieldMaybe" []
  JFKRepeatedBytes ->
    field "parseFieldMaybe" [] <+> txt ">>= traverse (traverse protoBytesFromJSON)"
  JFKNumericRepeated _ _ decode -> field "PJ.parseRepeatedFieldMaybeWith" [pretty decode, txt "id"]
  JFKBytesMap -> field "parseBytesMapFieldMaybe" []
  JFKNumericMap _ decode -> field "PJ.parseMapFieldMaybeWith" [pretty decode]
  JFKOptional True -> field "PJ.parsePresentFieldMaybe" []
  JFKNumeric _ _ decode -> field "PJ.parseFieldMaybeWith" [pretty decode]
  JFKOneof variants ->
    bind (txt "PJ.parseOneofVariants obj")
      <> line
      <> indent 2 (list (map genVariant variants))
    where
      genVariant variant =
        txt "PJ.OneofVariant" <+> viaShow (jvJsonName variant) <+> viaShow (jvProtoName variant)
          <+> viaShow (jvSelectsNull variant)
          <+> parens (txt "\\v ->" <+> pretty (jvConstructor variant) <+> txt "<$>" <+> pretty (jvDecoder variant) <+> txt "v")
  _ -> field "parseFieldMaybe" []
  where
    bind parser = txt "fld_" <> pretty (jfiAccessor jfi) <+> txt "<-" <+> parser
    jsonName = jfiJsonName jfi
    protoName = jfiProtoName jfi
    field parserName args =
      let parser = hsep (txt parserName : args)
          objAndKey = txt "obj" <+> viaShow jsonName
          callback = if null args then parser else parens parser
      in bind $
           if jsonName == protoName
             then parser <+> objAndKey
             else txt "PJ.parseFieldWithAlias" <+> callback <+> objAndKey <+> viaShow protoName


genFromJSONFieldAssign :: FieldNaming -> Text -> JSONFieldInfo -> Doc ann
genFromJSONFieldAssign naming tyN jfi = case jfiKind jfi of
  JFKOptionalBytes ->
    -- The slot is already 'Maybe ByteString'; the parse helper
    -- returned 'Maybe ByteString' too, so just plug it in
    -- directly instead of stripping the wrapper.
    pretty (jfiAccessor jfi) <+> txt "= fld_" <> pretty (jfiAccessor jfi)
  JFKOptional _ ->
    pretty (jfiAccessor jfi) <+> txt "= fld_" <> pretty (jfiAccessor jfi)
  JFKNumeric True _ _ ->
    pretty (jfiAccessor jfi) <+> txt "= fld_" <> pretty (jfiAccessor jfi)
  JFKOneof _ ->
    pretty (jfiAccessor jfi) <+> txt "= fld_" <> pretty (jfiAccessor jfi)
  _ ->
    pretty (jfiAccessor jfi) <+> txt "= maybe (" <> defaultField <> txt ") id fld_" <> pretty (jfiAccessor jfi)
  where
    selector = pretty (jfiAccessor jfi)
    record = txt "default" <> pretty tyN
    defaultField = case naming of
      PrefixedFields -> selector <+> record
      UnprefixedFields -> record <> txt "." <> selector


-- ---------------------------------------------------------------------------
-- Hashable instances
-- ---------------------------------------------------------------------------

genHashableInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genHashableInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fields = extractAllFields ctx scope (msgElements msg)
  in vsep
       [ instanceHead "Hashable" tyN
       , indent 2 $ case fields of
           [] -> txt "hashWithSalt salt _ = salt"
           _ -> txt "hashWithSalt salt msg = " <> genHashExpr fields
       ]


{- | Emit the per-message 'Proto.Extension.HasExtensions' instance so
that generated messages with @extensions@ declarations (or any
@extend@ block targeting them) support the typed extension
accessors from "Proto.Extension". The instance is always safe to
emit because every generated message type already carries an
unknown-fields list; messages without extension ranges will just
never have callers that use the instance.
-}
genHasExtensionsInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genHasExtensionsInstance ctx scope _msg =
  let tyN = scopedTypeName scope
      acc = ctxUnknownFieldName ctx scope
      update = txt "msg { " <> pretty acc <> txt " = ufs }"
      (getter, setter) = case genFieldNaming (gcOpts ctx) of
        PrefixedFields -> (txt "messageUnknownFields = " <> pretty acc, update)
        UnprefixedFields -> (txt "messageUnknownFields msg = msg." <> pretty acc, parens (update <> txt " :: " <> pretty tyN))
  in vsep
       [ txt "instance Proto.Extension.HasExtensions " <> pretty tyN <> txt " where"
       , indent 2 getter
       , indent 2 $
           txt "setMessageUnknownFields !ufs msg = " <> setter
       ]


{- | Generate a 'Semigroup' instance encoding protobuf merge semantics:
singular scalar fields take the right value only if non-default
(matching MergeFrom in protoc-gen-go/cpp), repeated and map fields
append/union (incoming map entries win), submessages and oneofs pick
the right value when set.
-}
genSemigroupInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genSemigroupInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fields = extractAllFields ctx scope (msgElements msg)
      conN = scopedTypeName scope
      unknownAcc = ctxUnknownFieldName ctx scope
      mergeField fi =
        let acc = fifAccessor fi
            ba = txt "b." <> pretty acc
            aa = txt "a." <> pretty acc
            lastPresent = txt "case " <> ba <> txt " of { Nothing -> " <> aa <> txt "; x -> x }"
            lastUnless condition = txt "if " <> condition <> txt " then " <> aa <> txt " else " <> ba
            lastNonDefault def = lastUnless (ba <> txt " == " <> txt def)
        in pretty acc <> txt " = " <> case fifKind fi of
             FKMap _ _ _ -> txt "Map.union" <+> ba <+> aa
             FKOneof _ _ -> lastPresent
             FKScalar (Just Repeated) _ -> aa <> txt " <> " <> ba
             FKNamed (Just Repeated) _ _ -> aa <> txt " <> " <> ba
             FKNamed lbl _ tk
               | explicitFieldPresence lbl || tk == TKMessage -> lastPresent
               | otherwise -> lastNonDefault "(toEnum 0)"
             FKScalar lbl ft
               | explicitFieldPresence lbl -> lastPresent
               | ft == SFloat || ft == SDouble -> lastUnless (scalarDefaultCheck ("b." <> acc) ft)
               | otherwise -> lastNonDefault (scalarDefaultText ft)
  in vsep
       [ txt "instance Semigroup " <> pretty tyN <> txt " where"
       , indent 2 $
           vsep
             [ txt "a <> b = " <> pretty conN
             , indent 2 $
                 braceBlock
                   ( fmap mergeField fields
                       <> [pretty unknownAcc <> txt " = a." <> pretty unknownAcc <> txt " <> b." <> pretty unknownAcc]
                   )
             ]
       ]


{- | Generate a 'Monoid' instance whose identity is the proto3 default
message (all scalars zero, all repeated / map / optional fields empty
/ 'Nothing'). The 'Semigroup' instance skips default-valued scalars
from the right operand, so @a <> mempty = a@ holds.
-}
genMonoidInstance :: [Text] -> Doc ann
genMonoidInstance scope =
  let tyN = scopedTypeName scope
      defN = "default" <> tyN
  in vsep
       [ txt "instance Monoid " <> pretty tyN <> txt " where"
       , indent 2 $ txt "mempty = " <> pretty defN
       ]


genHashExpr :: [FieldInfoFull] -> Doc ann
genHashExpr = go (txt "salt")
  where
    go acc [] = acc
    go acc (fi : rest) = go (genFieldHashApp acc fi) rest


genFieldHashApp :: Doc ann -> FieldInfoFull -> Doc ann
genFieldHashApp acc fi =
  let fld = txt "msg." <> pretty (fifAccessor fi)
  in case fifKind fi of
       FKScalar (Just Repeated) st
         | isUnboxableScalar st ->
             txt "VU.foldl' hashWithSalt (" <> acc <> txt ") " <> fld
       FKScalar (Just Repeated) _ ->
         txt "V.foldl' hashWithSalt (" <> acc <> txt ") " <> fld
       FKNamed (Just Repeated) _ _ ->
         txt "V.foldl' hashWithSalt (" <> acc <> txt ") " <> fld
       FKMap _ _ _ ->
         txt "Map.foldlWithKey' (\\s k v -> s `hashWithSalt` k `hashWithSalt` v) (" <> acc <> txt ") " <> fld
       _ ->
         txt "hashWithSalt (" <> acc <> txt ") " <> fld


genOneofHashableInstance :: GenCtx -> [Text] -> OneofDef -> Doc ann
genOneofHashableInstance ctx scope od =
  let tyN = oneofTypeName (gcOneofNames ctx) scope (oneofName od)
      arms = zipWith (genOneofHashArm ctx scope (oneofName od)) [0 :: Int ..] (oneofFields od)
  in vsep
       [ instanceHead "Hashable" tyN
       , indent 2 $ vsep arms
       ]


genOneofHashArm :: GenCtx -> [Text] -> Text -> Int -> OneofField -> Doc ann
genOneofHashArm ctx scope ooName tag f =
  let conName = oneofConstructorName (gcOneofNames ctx) scope ooName (oneofFieldName f)
      tagStr = T.pack (show tag)
  in txt "hashWithSalt salt (" <> pretty conName <+> txt "v) = salt `hashWithSalt` (" <> pretty tagStr <> txt " :: Int) `hashWithSalt` v"


genEnumHashableInstance :: [Text] -> EnumDef -> Doc ann
genEnumHashableInstance scope _ed =
  let tyN = scopedTypeName scope
  in vsep
       [ instanceHead "Hashable" tyN
       , indent 2 (txt "hashWithSalt salt x = hashWithSalt salt (toProtoEnum" <> pretty tyN <+> txt "x)")
       ]


fqProtoName :: Maybe Text -> [Text] -> Text
fqProtoName pkg scope =
  let msgName = T.intercalate "." scope
  in case pkg of
       Just p -> p <> "." <> msgName
       Nothing -> msgName


-- ---------------------------------------------------------------------------
-- Field extraction (unified)
-- ---------------------------------------------------------------------------

data FieldKindFull
  = FKScalar (Maybe FieldLabel) ScalarType
  | FKNamed (Maybe FieldLabel) Text TypeKind
  | FKMap [Text] ScalarType FieldType
  | FKOneof [Text] OneofDef
  deriving stock (Show, Eq)


data FieldInfoFull = FieldInfoFull
  { fifAccessor :: Text
  , fifFieldNum :: Int
  , fifIndex :: Int
  , fifPacked :: Bool
  , fifDelimited :: Bool
  , fifKind :: FieldKindFull
  }
  deriving stock (Show, Eq)


extractAllFields :: GenCtx -> [Text] -> [MessageElement] -> [FieldInfoFull]
extractAllFields ctx scope elems =
  zipWith (\i fi -> fi {fifIndex = i}) [0 ..] (concatMap go elems)
  where
    go = \case
      MEField fd ->
        let accessor = ctxFieldName ctx scope (fieldName fd)
            fn = unFieldNumber (fieldNumber fd)
            kind = case fieldType fd of
              FTScalar st -> FKScalar (fieldLabel fd) st
              FTNamed n -> FKNamed (fieldLabel fd) n (resolveTypeKindScoped ctx scope n)
            packable = case kind of
              FKScalar (Just Repeated) st -> packableScalar st
              FKNamed (Just Repeated) _ TKEnum -> True
              _ -> False
            packed = packable && fieldPackedIn ctx fd
            delimited = isLegacyGroupField fd || messageDelimitedIn ctx scope (fieldType fd) (fieldOptions fd)
        in [FieldInfoFull accessor fn 0 packed delimited kind]
      MEMapField mf ->
        let accessor = ctxFieldName ctx scope (mapFieldName mf)
            fn = unFieldNumber (mapFieldNum mf)
        in [FieldInfoFull accessor fn 0 False False (FKMap scope (mapKeyType mf) (mapValueType mf))]
      MEOneof od ->
        let accessor = ctxFieldName ctx scope (oneofName od)
            fn = 0
        in [FieldInfoFull accessor fn 0 False False (FKOneof scope od)]
      _ -> []


resolveTypeKindScoped :: GenCtx -> [Text] -> Text -> TypeKind
resolveTypeKindScoped ctx scope name = maybe TKMessage tiKind (resolveTypeWithScope ctx scope name)


messageDelimitedIn :: GenCtx -> [Text] -> FieldType -> [OptionDef] -> Bool
messageDelimitedIn ctx scope (FTNamed name) opts =
  resolveTypeKindScoped ctx scope name == TKMessage
    && either (error . T.unpack) id (messageEncodingDelimited (gcSyntax ctx) (gcFileOptions ctx) opts)
messageDelimitedIn _ _ _ _ = False


fieldPackedIn :: GenCtx -> FieldDef -> Bool
fieldPackedIn ctx fd =
  genPackedRepeated (gcOpts ctx)
    && either (error . T.unpack) id (repeatedFieldPacked (gcSyntax ctx) (gcFileOptions ctx) (fieldOptions fd))


-- JSON field info
--
-- 'JFKBytes' is split into bare-bytes and optional-bytes shapes
-- so the bytes ToJSON / FromJSON helpers (which require a
-- 'ByteString' / 'Maybe ByteString' argument respectively) line
-- up with the actual record-slot type. Without the split,
-- proto3 'optional bytes' fields generate code that takes a
-- 'Maybe ByteString' where 'bytesFieldToJSON' wants 'ByteString',
-- which doesn't type-check.
data JSONFieldKind
  = JFKNormal
  | JFKOptional Bool
  | JFKImplicitEnum
  | JFKBytes
  | JFKOptionalBytes
  | JFKRepeatedBytes
  | JFKNumericRepeated Text Text Text
  | JFKBytesMap
  | JFKNumericMap Text Text
  | JFKContainer Text
  | JFKNumeric Bool Text Text
  | JFKOneof [JSONOneofVariant]
  deriving stock (Show, Eq)


data JSONFieldInfo = JSONFieldInfo
  { jfiAccessor :: Text
  , jfiProtoName :: Text
  , jfiJsonName :: Text
  , jfiKind :: JSONFieldKind
  }
  deriving stock (Show, Eq)


data JSONOneofVariant = JSONOneofVariant
  { jvConstructor :: Text
  , jvProtoName :: Text
  , jvJsonName :: Text
  , jvSelectsNull :: Bool
  , jvEncoder :: Text
  , jvDecoder :: Text
  }
  deriving stock (Show, Eq)


extractAllFieldsJSON :: GenCtx -> [Text] -> [MessageElement] -> [JSONFieldInfo]
extractAllFieldsJSON ctx scope = concatMap go
  where
    go = \case
      MEField fd ->
        let accessor = ctxFieldName ctx scope (fieldName fd)
            jsonName = fromMaybe (protoJsonName (fieldName fd)) (getJsonName (fieldOptions fd))
            explicit = explicitFieldPresence (fieldLabel fd)
            kind = case fieldType fd of
              FTScalar SBytes -> case fieldLabel fd of
                Just Repeated -> JFKRepeatedBytes
                _ | explicit -> JFKOptionalBytes
                _ -> JFKBytes
              FTScalar scalar
                | fieldLabel fd == Just Repeated
                , Just (encode, decode) <- numericJSONHelpers scalar ->
                    JFKNumericRepeated (scalarVector scalar) encode decode
                | fieldLabel fd == Just Repeated -> JFKContainer (scalarVector scalar <> ".null")
                | Just (encode, decode) <- numericJSONHelpers scalar ->
                    JFKNumeric explicit encode decode
                | explicit -> JFKOptional False
                | scalar == SString -> JFKContainer "T.null"
                | scalar == SBool -> JFKContainer "not"
              FTNamed n
                | fieldLabel fd == Just Repeated -> JFKContainer "V.null"
                | explicit || resolveTypeKindScoped ctx scope n == TKMessage ->
                    JFKOptional (acceptsJsonNull ctx scope n)
                | otherwise -> JFKImplicitEnum
              _ -> JFKNormal
        in [JSONFieldInfo accessor (fieldName fd) jsonName kind]
      MEMapField mf ->
        let accessor = ctxFieldName ctx scope (mapFieldName mf)
            jsonName = fromMaybe (protoJsonName (mapFieldName mf)) (getJsonName (mapOptions mf))
            kind = case mapValueType mf of
              FTScalar SBytes -> JFKBytesMap
              FTScalar scalar | Just (encode, decode) <- numericJSONHelpers scalar -> JFKNumericMap encode decode
              _ -> JFKContainer "Map.null"
        in [JSONFieldInfo accessor (mapFieldName mf) jsonName kind]
      MEOneof od ->
        let accessor = ctxFieldName ctx scope (oneofName od)
            jsonName = protoJsonName (oneofName od)
            variants = map (oneofVariantJSON ctx scope (oneofName od)) (oneofFields od)
        in [JSONFieldInfo accessor (oneofName od) jsonName (JFKOneof variants)]
      _ -> []


oneofVariantJSON :: GenCtx -> [Text] -> Text -> OneofField -> JSONOneofVariant
oneofVariantJSON ctx scope oneof field =
  let protoName = oneofFieldName field
      jsonName = fromMaybe (protoJsonName protoName) (getJsonName (oneofFieldOptions field))
      selectsNull = case oneofFieldType field of
        FTNamed n -> acceptsJsonNull ctx scope n
        _ -> False
      (encode, decode) = case oneofFieldType field of
        FTScalar SBytes -> ("PJ.protoBytesToJSON", "PJ.protoBytesFromJSON")
        FTScalar scalar -> fromMaybe aeson (numericJSONHelpers scalar)
        FTNamed _ -> aeson
      aeson = ("Aeson.toJSON", "Aeson.parseJSON")
  in JSONOneofVariant (oneofConstructorName (gcOneofNames ctx) scope oneof protoName) protoName jsonName selectsNull encode decode


isNullValueType :: GenCtx -> [Text] -> Text -> Bool
isNullValueType ctx scope name =
  resolveTypeKeyWithScope ctx scope name == Just "google.protobuf.NullValue"


acceptsJsonNull :: GenCtx -> [Text] -> Text -> Bool
acceptsJsonNull ctx scope name = case resolveTypeKeyWithScope ctx scope name of
  Just "google.protobuf.NullValue" -> True
  Just "google.protobuf.Value" -> True
  _ -> False


numericJSONHelpers :: ScalarType -> Maybe (Text, Text)
numericJSONHelpers = \case
  SDouble -> Just ("PJ.protoDoubleToJSON", "PJ.protoDoubleFromJSON")
  SFloat -> Just ("PJ.protoFloatToJSON", "PJ.protoFloatFromJSON")
  SInt64 -> int64
  SSInt64 -> int64
  SSFixed64 -> int64
  SUInt64 -> word64
  SFixed64 -> word64
  SInt32 -> int32
  SSInt32 -> int32
  SSFixed32 -> int32
  SUInt32 -> word32
  SFixed32 -> word32
  _ -> Nothing
  where
    int64 = Just ("PJ.protoInt64ToJSON", "PJ.protoInt64FromJSON")
    word64 = Just ("PJ.protoWord64ToJSON", "PJ.protoWord64FromJSON")
    int32 = Just ("Aeson.toJSON", "PJ.protoInt32FromJSON")
    word32 = Just ("Aeson.toJSON", "PJ.protoWord32FromJSON")


getJsonName :: [OptionDef] -> Maybe Text
getJsonName = \case
  [] -> Nothing
  opts -> lookupSimpleOption "json_name" opts >>= optionAsString


-- ---------------------------------------------------------------------------
-- Service generation
-- ---------------------------------------------------------------------------

genServiceTopLevel :: GenCtx -> [Text] -> ServiceDef -> [Doc ann]
genServiceTopLevel ctx scope svc =
  let svcScope = scope <> [svcName svc]
      hookCtx =
        ServiceHookCtx
          { shcServiceDef = svc
          , shcScope = svcScope
          , shcHsTypeName = T.intercalate "'" (fmap hsTypeName svcScope)
          , shcOptions = svcOptions svc
          }
      hookOutput = onServiceCodeGen (genHooks (gcOpts ctx)) hookCtx
      hookDocs = fmap pretty hookOutput
      names = gcServiceNames ctx Map.! svcName svc
  in Service.genServiceDeclsNamed (gcPkg ctx) scope (resolveHsTypeNameScoped ctx scope) names svc
       <> case hookDocs of
         [] -> []
         ds -> [mempty, vsep ds]


topLevelService :: TopLevel -> Maybe ServiceDef
topLevelService (TLService svc) = Just svc
topLevelService _ = Nothing


moduleServiceBindings
  :: FieldNaming
  -> RecordBindingNames
  -> OneofBindingNames
  -> Map Text ExtensionNames
  -> [TopLevel]
  -> Map Text Service.ServiceBindings
moduleServiceBindings naming records (OneofBindingNames oneofTypes oneofCons) extensions definitions =
  Service.serviceBindings [] types constructors values (mapMaybe topLevelService definitions)
  where
    (declaredTypes, declaredConstructors) = protoDeclarationNames definitions
    types = Set.union declaredTypes (Set.fromList (Map.elems oneofTypes))
    constructors = Set.union declaredConstructors (Set.fromList (Map.elems oneofCons))
    values = Set.fromList (generatedValueNames (filter nonService definitions) <> selectors <> extensionValues)
    nonService (TLService _) = False
    nonService _ = True
    selectors = case naming of
      PrefixedFields -> Map.elems (recordSelectorNames records) <> Map.elems (recordUnknownNames records)
      UnprefixedFields -> []
    extensionValues = concatMap (\names -> [extensionValueName names, extensionRegistryName names]) (Map.elems extensions)


-- ---------------------------------------------------------------------------
-- Proto2 extension blocks (@extend Foo { optional int32 bar = 123; }@)
-- ---------------------------------------------------------------------------

data ExtensionNames = ExtensionNames
  { extensionValueName :: !Text
  , extensionRegistryName :: !Text
  }


legacyExtensionNames :: Text -> Text -> FieldDef -> ExtensionNames
legacyExtensionNames fqn owner field =
  ExtensionNames
    (scopedFieldName [lastDot owner] (fieldName field))
    ("registerExt_" <> T.replace "." "_" fqn)


lookupExtensionNames :: Map Text ExtensionNames -> Text -> Text -> FieldDef -> ExtensionNames
lookupExtensionNames names fqn owner field =
  Map.findWithDefault (legacyExtensionNames fqn owner field) fqn names


mapExtensionFields :: ([Text] -> Text -> FieldDef -> a) -> [TopLevel] -> [a]
mapExtensionFields select = concatMap top
  where
    top (TLMessage message) = nested [] message
    top (TLExtend owner fields) = fmap (select [] owner) fields
    top _ = []
    nested parent message = concatMap element (msgElements message)
      where
        scope = parent <> [msgName message]
        element (MEMessage child) = nested scope child
        element (MEExtend owner fields) = fmap (select scope owner) fields
        element _ = []


-- | Verified string extension tags whose extendee is generated in this file.
-- Other extensions remain opaque to a decoder built from this schema.
verifiedStringExtensions :: ProtoFile -> Either Text (Map Text [Int])
verifiedStringExtensions file =
  fmap (Map.map Set.toAscList . Map.fromListWith Set.union . mapMaybe id)
    (sequence (mapExtensionFields check (protoTopLevels file)))
  where
    pkg = protoPackage file
    messages = Set.fromList (mapScopedMessages (\scope _ -> fqProtoName pkg scope) (protoTopLevels file))
    check scope owner field = case fieldType field of
      FTScalar SString
        | Just target <- resolveLexicalName (`Set.member` messages) pkg scope owner -> do
            verify <- verifyStringUtf8 (protoSyntax file) (protoOptions file) (fieldOptions field)
            pure (if verify then Just (target, Set.singleton (unFieldNumber (fieldNumber field))) else Nothing)
      _ -> Right Nothing


-- | Reserve the module's generated value names before choosing names
-- for extensions. Keep the old spelling when it is available. Use
-- the proto declaration's scope to distinguish colliding extensions.
extensionBindingNames :: FieldNaming -> Maybe Text -> [TopLevel] -> Map Text ExtensionNames
extensionBindingNames naming pkg definitions =
  snd (List.foldl' allocate (reserved, Map.empty) ordered)
  where
    ordered = List.sortOn (\(fqn, _, _) -> (T.count "." fqn, fqn)) (mapExtensionFields request definitions)
    request scope owner field = (fqProtoName pkg (scope <> [fieldName field]), owner, field)

    reserved = Set.fromList
      (generatedValueNames definitions <> concatMap recordNamesFor [id, pure . scopedTypeName])
    recordNamesFor selectorScope =
      let names = recordBindingNames naming selectorScope definitions
      in Map.elems (recordSelectorNames names) <> Map.elems (recordUnknownNames names)

    allocate (used, names) (fqn, owner, field) =
      let legacy = legacyExtensionNames fqn owner field
          value = chooseGeneratedName used (extensionValueName legacy) (T.replace "." "'" fqn)
          withValue = Set.insert value used
          (registration, occupied) = case fieldLabel field of
            Just Repeated -> (extensionRegistryName legacy, withValue)
            _ ->
              let name = chooseGeneratedName withValue (extensionRegistryName legacy) (T.replace "." "'" fqn)
              in (name, Set.insert name withValue)
      in (occupied, Map.insert fqn (ExtensionNames value registration) names)


generatedValueNames :: [TopLevel] -> [Text]
generatedValueNames definitions = "fileDescriptorProtoBytes" : concatMap topValueNames definitions
  where
    topValueNames (TLMessage message) = messageValueNames [] message
    topValueNames (TLEnum enum) = enumValueNames [enumName enum]
    topValueNames (TLService svc) = Service.serviceValueNames [] svc
    topValueNames _ = []
    messageValueNames parent message =
      ("default" <> ty) : concatMap element (msgElements message)
      where
        scope = parent <> [msgName message]
        ty = scopedTypeName scope
        element (MEMessage nested) = messageValueNames scope nested
        element (MEEnum enum) = enumValueNames (scope <> [enumName enum])
        element _ = []
    enumValueNames scope = ["toProtoEnum" <> ty, "fromProtoEnum" <> ty]
      where
        ty = scopedTypeName scope


data RecordBindingNames = RecordBindingNames
  { recordSelectorNames :: !(Map ([Text], Text) Text)
  , recordUnknownNames :: !(Map [Text] Text)
  }


-- | Allocate names once for all fields, including map and oneof carriers.
-- Prefixed selectors are module bindings. Bare labels only need to be
-- unique within a record. TH supplies its existing nested selector prefix.
recordBindingNames :: FieldNaming -> ([Text] -> [Text]) -> [TopLevel] -> RecordBindingNames
recordBindingNames naming selectorScope definitions =
  case naming of
    PrefixedFields ->
      let (reserved, unknownNames) = List.foldl' allocateUnknown (Set.fromList (generatedValueNames definitions), Map.empty) messages
          (_, selectors) = List.foldl' allocateFields (reserved, Map.empty) messages
      in RecordBindingNames selectors unknownNames
    UnprefixedFields ->
      let unknownNames = Map.fromList (map (\(scope, _) -> (scope, unknownBase scope)) messages)
          selectors = Map.unions (map (\entry@(scope, _) -> snd (allocateFields (Set.singleton (unknownBase scope), Map.empty) entry)) messages)
      in RecordBindingNames selectors unknownNames
  where
    messages = mapScopedMessages (,) definitions
    unknownBase = unknownFieldAccessor . selectorScope
    allocateUnknown (used, names) (scope, _) =
      let name = chooseGeneratedName used (unknownBase scope) (T.intercalate "'" scope)
      in (Set.insert name used, Map.insert scope name names)
    allocateFields state (scope, msg) =
      List.foldl' (allocateField scope) state (mapMaybe protoSelector (msgElements msg))
    allocateField scope (used, names) protoName =
      let base = scopedFieldNameWith naming (selectorScope scope) protoName
          name = chooseGeneratedName used base protoName
      in (Set.insert name used, Map.insert (scope, protoName) name names)
    protoSelector (MEField field) = Just (fieldName field)
    protoSelector (MEMapField field) = Just (mapFieldName field)
    protoSelector (MEOneof oneof) = Just (oneofName oneof)
    protoSelector _ = Nothing


-- | Template Haskell's type-level HasField labels have their own namespace.
fieldLabelNames :: [Text] -> Map Text Text
fieldLabelNames = snd . List.foldl' allocate (Set.empty, Map.empty)
  where
    allocate (used, names) protoName =
      let name = chooseGeneratedName used (snakeToCamel protoName) protoName
      in (Set.insert name used, Map.insert protoName name names)


data OneofBindingNames = OneofBindingNames
  !(Map ([Text], Text) Text)
  !(Map ([Text], Text, Text) Text)


oneofTypeName :: OneofBindingNames -> [Text] -> Text -> Text
oneofTypeName (OneofBindingNames types _) scope oneof = types Map.! (scope, oneof)


oneofConstructorName :: OneofBindingNames -> [Text] -> Text -> Text -> Text
oneofConstructorName (OneofBindingNames _ constructors) scope oneof field =
  constructors Map.! (scope, oneof, field)


-- | Allocate carrier types and variant constructors in their respective
-- Haskell namespaces. The lookup keys retain the original proto spelling.
oneofBindingNames :: [TopLevel] -> OneofBindingNames
oneofBindingNames definitions = OneofBindingNames types constructors
  where
    messages = mapScopedMessages (,) definitions
    (declaredTypes, declaredConstructors) = protoDeclarationNames definitions
    oneofs = concatMap (\(scope, msg) -> mapMaybe (oneofAt scope) (msgElements msg)) messages
    (_, types) = List.foldl' allocateType (declaredTypes, Map.empty) oneofs
    (_, constructors) = List.foldl' allocateVariants (declaredConstructors, Map.empty) oneofs
    oneofAt scope (MEOneof oneof) = Just (scope, oneof)
    oneofAt _ _ = Nothing
    allocateType (used, names) (scope, oneof) =
      let protoName = oneofName oneof
          base = scopedTypeName scope <> "'" <> snakeToPascal protoName
          name = chooseGeneratedName used base protoName
      in (Set.insert name used, Map.insert (scope, protoName) name names)
    allocateVariants state (scope, oneof) =
      let protoName = oneofName oneof
          carrier = types Map.! (scope, protoName)
      in List.foldl' (allocateVariant scope protoName carrier) state (oneofFields oneof)
    allocateVariant scope oneof carrier (used, names) field =
      let protoName = oneofFieldName field
          name = chooseGeneratedName used (carrier <> "'" <> snakeToPascal protoName) protoName
      in (Set.insert name used, Map.insert (scope, oneof, protoName) name names)


protoDeclarationNames :: [TopLevel] -> (Set Text, Set Text)
protoDeclarationNames definitions =
  ( Set.fromList (messageNames <> map (scopedTypeName . fst) enums)
  , Set.fromList (messageNames <> concatMap enumConstructors enums)
  )
  where
    messages = mapScopedMessages (,) definitions
    messageNames = map (scopedTypeName . fst) messages
    enums = mapMaybe topEnum definitions <> concatMap (uncurry nestedEnums) messages
    topEnum (TLEnum enum) = Just ([enumName enum], enum)
    topEnum _ = Nothing
    nestedEnums scope = mapMaybe (nestedEnum scope) . msgElements
    nestedEnum scope (MEEnum enum) = Just (scope <> [enumName enum], enum)
    nestedEnum _ _ = Nothing
    enumConstructors (scope, enum) =
      enumUnknownCon scope : Map.elems (enumConstructorNames scope enum)


{- | Emit one top-level 'Proto.Extension.Extension' binding per field
in an @extend@ block. Callers of the generated module use
'Proto.Extension.getExtension' / 'setExtension' / 'clearExtension'
to interact with the extension through the message's
unknown-fields list; the owning message type automatically
satisfies 'Proto.Extension.HasExtensions' via the
per-message instance emitted by 'genHasExtensionsInstance'.

Singular and repeated extensions are emitted here, including
packed scalars and tag-delimited message payloads.
-}
genExtensionBlock
  :: GenCtx -> [Text] -> Text -> [FieldDef] -> [Doc ann]
genExtensionBlock ctx scope extOwnerName fields =
  let ownerFqn = fromMaybe (T.dropWhile (== '.') extOwnerName) (resolveTypeKeyWithScope ctx scope extOwnerName)
      ownerHsType = qualifyTypeRef ctx (Map.lookup ownerFqn (gcRegistry ctx)) extOwnerName
  in concatMap (genOneExtension ctx scope ownerHsType extOwnerName ownerFqn) fields


-- Generate one @Extension <owner> <payload>@ (or
-- @RepeatedExtension <owner> <payload>@) binding.
genOneExtension
  :: GenCtx -> [Text] -> Text -> Text -> Text -> FieldDef -> [Doc ann]
genOneExtension ctx scope ownerHsType ownerProtoName ownerFqn fd =
  let num = unFieldNumber (fieldNumber fd)
      repeated = fieldLabel fd == Just Repeated
      packed =
        repeated && case fieldType fd of
          FTScalar s -> packableScalar s && fieldPackedIn ctx fd
          FTNamed n -> resolveTypeKindScoped ctx scope n == TKEnum && fieldPackedIn ctx fd
      payload
        | isLegacyGroupField fd || messageDelimitedIn ctx scope (fieldType fd) (fieldOptions fd) =
            Just (txt "ByteString", "ExtGroup")
        | otherwise = extensionPayloadCore ctx scope (fieldType fd)
      extFqn = fqProtoName (gcPkg ctx) (scope <> [fieldName fd])
      ExtensionNames fieldNameHs registration = lookupExtensionNames (gcExtensionNames ctx) extFqn ownerProtoName fd
  in case payload of
       Nothing ->
         [ mempty
         , txt "-- WARNING: extension '"
             <> pretty (fieldName fd)
             <> txt "' uses an unsupported shape and was skipped."
         ]
       Just (haskellType, extTag) ->
         if repeated
           then
             [ mempty
             , txt fieldNameHs
                 <> txt " :: Proto.Extension.RepeatedExtension "
                 <> pretty ownerHsType
                 <> txt " "
                 <> haskellType
             , txt fieldNameHs <> txt " = Proto.Extension.RepeatedExtension"
             , indent 2 $ txt "{ Proto.Extension.reNumber   = " <> pretty (tshow num)
             , indent 2 $
                 txt ", Proto.Extension.reType     = Proto.Extension."
                   <> pretty extTag
             , indent 2 $
                 txt ", Proto.Extension.reIsPacked = "
                   <> (if packed then txt "True" else txt "False")
             , indent 2 $ txt "}"
             ]
           else
             [ mempty
             , txt fieldNameHs
                 <> txt " :: Proto.Extension.Extension "
                 <> pretty ownerHsType
                 <> txt " "
                 <> haskellType
             , txt fieldNameHs <> txt " = Proto.Extension.Extension"
             , indent 2 $ txt "{ Proto.Extension.extNumber = " <> pretty (tshow num)
             , indent 2 $
                 txt ", Proto.Extension.extType   = Proto.Extension."
                   <> pretty extTag
             , indent 2 $ txt "}"
             , mempty
             , pretty registration <+> txt ":: PJExt.ExtensionRegistry"
             , pretty registration <+> txt "= PJExt.registerTypedExtensionJson"
                 <+> viaShow ownerFqn <+> viaShow extFqn <+> pretty fieldNameHs
             ]


{- | Project a proto 'FieldType' onto @(Haskell type,
'ExtensionType' constructor name)@. The label-aware split is now
the caller's job: 'genOneExtension' picks Extension vs.
RepeatedExtension based on @fieldLabel@.
-}
extensionPayloadCore :: GenCtx -> [Text] -> FieldType -> Maybe (Doc ann, Text)
extensionPayloadCore _ _ (FTScalar s) = case s of
  SDouble -> Just (txt "Double", "ExtDouble")
  SFloat -> Just (txt "Float", "ExtFloat")
  SInt32 -> Just (txt "Int32", "ExtInt32")
  SInt64 -> Just (txt "Int64", "ExtInt64")
  SUInt32 -> Just (txt "Word32", "ExtUInt32")
  SUInt64 -> Just (txt "Word64", "ExtUInt64")
  SSInt32 -> Just (txt "Int32", "ExtSInt32")
  SSInt64 -> Just (txt "Int64", "ExtSInt64")
  SFixed32 -> Just (txt "Word32", "ExtFixed32")
  SFixed64 -> Just (txt "Word64", "ExtFixed64")
  SSFixed32 -> Just (txt "Int32", "ExtSFixed32")
  SSFixed64 -> Just (txt "Int64", "ExtSFixed64")
  SBool -> Just (txt "Bool", "ExtBool")
  SString -> Just (txt "Text", "ExtString")
  SBytes -> Just (txt "ByteString", "ExtBytes")
extensionPayloadCore ctx scope (FTNamed name)
  | resolveTypeKindScoped ctx scope name == TKEnum =
      Just (pretty (resolveHsTypeNameScoped ctx scope name), "ExtEnum")
  | otherwise = Just (txt "ByteString", "ExtMessage")


{- | Whether a scalar can use the packed encoding (proto2 default
false; proto3 default true). Strings and bytes can't.
-}
packableScalar :: ScalarType -> Bool
packableScalar = \case
  SString -> False
  SBytes -> False
  _ -> True


-- The unqualified name of an extension type.
lastDot :: Text -> Text
lastDot t = case T.splitOn "." t of
  [] -> t
  parts -> last parts


-- ---------------------------------------------------------------------------
-- Enum generation
-- ---------------------------------------------------------------------------

genEnum :: GenCtx -> [Text] -> EnumDef -> [Doc ann]
genEnum ctx scope ed =
  let scope' = scope <> [enumName ed]
      tyN = scopedTypeName scope'
      names = enumConstructorNames scope' ed
      con name = Map.findWithDefault (scopedEnumCon scope' name) name names
      hookCtx =
        EnumHookCtx
          { ehcEnumDef = ed
          , ehcScope = scope'
          , ehcHsTypeName = tyN
          , ehcOptions = enumOptions ed
          }
      hookOutput = onEnumCodeGen (genHooks (gcOpts ctx)) hookCtx
      hookDocs = fmap pretty hookOutput
  in [ mempty
     , genEnumDataDecl scope' ed con
     , mempty
     , genEnumToProto scope' ed con
     , mempty
     , genEnumFromProto scope' ed con
     , mempty
     , genEnumInstances scope' ed con
     , mempty
     , genEnumSchemaInstance ctx scope' ed
     , mempty
     , genEnumEncodeInstance scope' ed
     , mempty
     , genEnumToJSONInstance ctx scope' ed con
     , mempty
     , genEnumFromJSONInstance ctx scope' ed con
     , mempty
     , genEnumHashableInstance scope' ed
     ]
       <> case hookDocs of
         [] -> []
         ds -> [mempty, vsep ds]


genEnumSchemaInstance :: GenCtx -> [Text] -> EnumDef -> Doc ann
genEnumSchemaInstance ctx scope ed =
  let tyN = scopedTypeName scope
      fqn = fqProtoName (gcPkg ctx) scope
      closed = either (error . T.unpack) id (enumIsClosed (gcSyntax ctx) (gcFileOptions ctx) (enumOptions ed))
      decode = if closed then txt "fromProtoEnum" <> pretty tyN else txt "Just . toEnum"
      entry ev = tupled [viaShow (evName ev), pretty (evNumber ev)]
  in vsep $
       [ instanceHead "ProtoEnum" tyN
       , indent 2 (txt "protoEnumName _ =" <+> viaShow fqn)
       , indent 2 (hang 2 (txt "protoEnumValues _ =" <+> list (map entry (enumValues ed))))
       , indent 2 (txt "fromProtoEnumValue =" <+> decode)
       , indent 2 (txt "toProtoEnumValue = toProtoEnum" <> pretty tyN)
       ]
         <> if closed then [indent 2 (txt "protoEnumIsOpen _ = False")] else []


genEnumDataDecl :: [Text] -> EnumDef -> (Text -> Text) -> Doc ann
genEnumDataDecl scope ed con =
  let tyN = scopedTypeName scope
      primaryVals = enumPrimaryValues ed
      aliasVals = enumAliasValues ed
      knownCases = fmap (\(pfx, v) -> (pfx <+> pretty (con (evName v))) : haddockFieldDoc (evDoc v)) (zip seps primaryVals)
      unknownCase = txt "|" <+> pretty (enumUnknownCon scope) <+> txt "!Int32"
      aliasSyns =
        fmap
          ( \ev ->
              txt "pattern "
                <> pretty (con (evName ev))
                <+> txt "::"
                <+> pretty tyN
                <> line
                <> txt "pattern "
                <> pretty (con (evName ev))
                <+> txt "= "
                <> pretty (con (canonicalNameForNumber ed (evNumber ev)))
          )
          aliasVals
  in vsep $
       haddockDoc (enumDoc ed)
         <> [ txt "data " <> pretty tyN
            , indent 2 (vsep (concat knownCases <> [unknownCase]))
            , indent 2 (txt "deriving stock (Show, Eq, Ord, Generic)")
            , indent 2 (txt "deriving anyclass NFData")
            ]
         <> aliasSyns
  where
    seps = txt "=" : repeat (txt "|")


genEnumToProto :: [Text] -> EnumDef -> (Text -> Text) -> Doc ann
genEnumToProto scope ed con =
  let tyN = scopedTypeName scope
      primaryVals = enumPrimaryValues ed
      genCase ev =
        txt "toProtoEnum"
          <> pretty tyN
          <+> pretty (con (evName ev))
          <+> txt "= "
          <> pretty (T.pack (show (evNumber ev)))
  in vsep
       [ txt "toProtoEnum" <> pretty tyN <+> txt "::" <+> pretty tyN <+> txt "-> Int"
       , vsep
           (fmap genCase primaryVals
             <> [txt "toProtoEnum" <> pretty tyN <+> parens (pretty (enumUnknownCon scope) <+> txt "n") <+> txt "= fromIntegral n"])
       ]


genEnumFromProto :: [Text] -> EnumDef -> (Text -> Text) -> Doc ann
genEnumFromProto scope ed con =
  let tyN = scopedTypeName scope
      primaryVals = enumPrimaryValues ed
      genCase ev =
        txt "fromProtoEnum"
          <> pretty tyN
          <+> parens (pretty (evNumber ev))
          <+> txt "= Just "
          <> pretty (con (evName ev))
  in vsep
       [ txt "fromProtoEnum" <> pretty tyN <+> txt "::" <+> txt "Int -> Maybe " <> pretty tyN
       , vsep (fmap genCase primaryVals)
       , txt "fromProtoEnum" <> pretty tyN <+> txt "_ = Nothing"
       ]


enumUnknownCon :: [Text] -> Text
enumUnknownCon scope = scopedTypeName scope <> "''Unrecognized"


genEnumInstances :: [Text] -> EnumDef -> (Text -> Text) -> Doc ann
genEnumInstances scope ed con =
  let tyN = scopedTypeName scope
      enumDoc =
        [ instanceHead "Enum" tyN
        , indent 2 (txt "fromEnum = toProtoEnum" <> pretty tyN)
        , indent 2 (txt "toEnum n = case fromProtoEnum" <> pretty tyN <+> txt "n of")
        , indent 4 (txt "Just v -> v")
        , indent 4 (txt "Nothing -> " <> pretty (enumUnknownCon scope) <+> txt "(fromIntegral n)")
        ]
      boundedDoc = case enumValues ed of
        firstValue : rest | not (enumHasAliases ed) ->
          let lastValue = List.foldl' (\_ v -> v) firstValue rest
          in [ mempty
             , instanceHead "Bounded" tyN
             , indent 2 (txt "minBound =" <+> pretty (con (evName firstValue)))
             , indent 2 (txt "maxBound =" <+> pretty (con (evName lastValue)))
             ]
        _ -> []
  in vsep (enumDoc <> boundedDoc)


enumHasAliases :: EnumDef -> Bool
enumHasAliases ed =
  let nums = fmap evNumber (enumValues ed)
  in length nums /= length (Set.fromList nums)


enumPrimaryValues :: EnumDef -> [EnumValue]
enumPrimaryValues ed = go Set.empty (enumValues ed)
  where
    go _ [] = []
    go seen (ev : evs)
      | Set.member (evNumber ev) seen = go seen evs
      | otherwise = ev : go (Set.insert (evNumber ev) seen) evs


enumAliasValues :: EnumDef -> [EnumValue]
enumAliasValues ed = go Set.empty (enumValues ed)
  where
    go _ [] = []
    go seen (ev : evs)
      | Set.member (evNumber ev) seen = ev : go seen evs
      | otherwise = go (Set.insert (evNumber ev) seen) evs


-- | Primary constructors keep their usual spelling whenever possible.
-- Colliding values use the original, case-sensitive proto spelling as a suffix.
enumConstructorNames :: [Text] -> EnumDef -> Map Text Text
enumConstructorNames scope ed =
  snd (List.foldl' allocate (Set.singleton (enumUnknownCon scope), Map.empty) values)
  where
    values = enumPrimaryValues ed <> enumAliasValues ed
    allocate (used, names) value =
      let protoName = evName value
          hsName = chooseGeneratedName used (scopedEnumCon scope protoName) protoName
      in (Set.insert hsName used, Map.insert protoName hsName names)


canonicalNameForNumber :: EnumDef -> Int -> Text
canonicalNameForNumber ed num =
  case filter (\ev -> evNumber ev == num) (enumValues ed) of
    (ev : _) -> evName ev
    [] -> "UNKNOWN"


genEnumEncodeInstance :: [Text] -> EnumDef -> Doc ann
genEnumEncodeInstance scope _ed =
  let tyN = scopedTypeName scope
  in vsep
       [ instanceHead "MessageEncode" tyN
       , indent 2 (txt "buildMessage _ = mempty")
       , instanceHead "MessageSize" tyN
       , indent 2 (txt "messageSize _ = 0")
       , instanceHead "MessageDecode" tyN
       , indent 2 (txt "messageDecoder = pure (toEnum 0)")
       ]


genEnumToJSONInstance :: GenCtx -> [Text] -> EnumDef -> (Text -> Text) -> Doc ann
genEnumToJSONInstance ctx scope _ _ | fqProtoName (gcPkg ctx) scope == "google.protobuf.NullValue" =
  vsep [instanceHead "Aeson.ToJSON" (scopedTypeName scope), indent 2 (txt "toJSON _ = Aeson.Null")]
genEnumToJSONInstance _ scope ed con =
  let tyN = scopedTypeName scope
      primaryVals = enumPrimaryValues ed
      genCase ev =
        txt "toJSON "
          <> pretty (con (evName ev))
          <+> pretty ("= Aeson.String \"" :: Text)
          <> pretty (evName ev)
          <> pretty ("\"" :: Text)
  in vsep
       [ instanceHead "Aeson.ToJSON" tyN
       , indent 2
           (vsep (fmap genCase primaryVals <> [txt "toJSON" <+> parens (pretty (enumUnknownCon scope) <+> txt "n") <+> txt "= Aeson.toJSON n"]))
       ]


genEnumFromJSONInstance :: GenCtx -> [Text] -> EnumDef -> (Text -> Text) -> Doc ann
genEnumFromJSONInstance ctx scope _ _ | fqProtoName (gcPkg ctx) scope == "google.protobuf.NullValue" =
  vsep [instanceHead "Aeson.FromJSON" (scopedTypeName scope), indent 2 (txt "parseJSON = PJ.protoNullValueFromJSON")]
genEnumFromJSONInstance _ scope ed con =
  let tyN = scopedTypeName scope
      genCase ev =
        pretty ("  Aeson.String \"" :: Text) <> pretty (evName ev) <> pretty ("\" -> pure " :: Text) <> pretty (con (evName ev))
  in vsep
       [ instanceHead "Aeson.FromJSON" tyN
       , indent 2 $
           vsep
             [ txt "parseJSON value = case value of"
             , vsep (fmap genCase (enumValues ed))
             , txt "  v -> PJ.protoEnumNumberFromJSON v"
             ]
       ]


-- ---------------------------------------------------------------------------
-- Haskell type helpers
-- ---------------------------------------------------------------------------

hsFieldType :: GenCtx -> [Text] -> FieldType -> Maybe FieldLabel -> Doc ann
hsFieldType ctx scope ft lbl = case lbl of
  Just Repeated -> hsRepeatedType ctx scope ft
  _ | explicitFieldPresence lbl -> txt "!(Maybe " <> hsFieldTypeInner ctx scope ft <> txt ")"
  _ -> case ft of
    FTScalar s | isUnboxableScalar s -> txt "{-# UNPACK #-} !" <> hsScalarType s
    FTScalar _ -> txt "!" <> hsFieldTypeInner ctx scope ft
    FTNamed n -> case resolveTypeWithScope ctx scope n of
      Just ti | tiKind ti == TKEnum -> txt "!" <> pretty (qualifyTypeRef ctx (Just ti) n)
      _ -> txt "!(Maybe " <> hsFieldTypeInner ctx scope ft <> txt ")"


hsFieldTypeInner :: GenCtx -> [Text] -> FieldType -> Doc ann
hsFieldTypeInner ctx scope = \case
  FTScalar s -> hsScalarType s
  FTNamed n -> pretty (resolveHsTypeNameScoped ctx scope n)


hsOneofFieldType :: GenCtx -> [Text] -> FieldType -> Doc ann
hsOneofFieldType ctx scope = \case
  FTScalar s -> unpackPragma s <> hsScalarType s
  FTNamed n -> txt "!" <> pretty (resolveHsTypeNameScoped ctx scope n)


hsRepeatedType :: GenCtx -> [Text] -> FieldType -> Doc ann
hsRepeatedType ctx scope = \case
  FTScalar s | isUnboxableScalar s -> txt "!(VU.Vector " <> hsScalarType s <> txt ")"
  ft -> txt "!(V.Vector " <> hsFieldTypeInner ctx scope ft <> txt ")"


{- | Resolve a proto type name to its Haskell reference.
Returns a qualified name like @PB_Timestamp.Timestamp@ for external types,
or an unqualified name like @Payload@ for local types.
-}
resolveHsTypeNameScoped :: GenCtx -> [Text] -> Text -> Text
resolveHsTypeNameScoped ctx scope name = qualifyTypeRef ctx (resolveTypeWithScope ctx scope name) name


qualifyTypeRef :: GenCtx -> Maybe TypeInfo -> Text -> Text
qualifyTypeRef ctx mti name = case mti of
  Just ti
    | tiModule ti == gcThisMod ctx -> tiHsName ti
    | otherwise ->
        let modName = tiModule ti
            alias = Map.findWithDefault (moduleAlias modName) modName (gcImportAliases ctx)
        in alias <> "." <> tiHsName ti
  Nothing -> hsTypeName (lastPart name)
  where
    lastPart t = case T.splitOn "." t of
      [] -> t
      parts -> last parts


hsScalarType :: ScalarType -> Doc ann
hsScalarType = \case
  SDouble -> txt "Double"
  SFloat -> txt "Float"
  SInt32 -> txt "Int32"
  SInt64 -> txt "Int64"
  SUInt32 -> txt "Word32"
  SUInt64 -> txt "Word64"
  SSInt32 -> txt "Int32"
  SSInt64 -> txt "Int64"
  SFixed32 -> txt "Word32"
  SFixed64 -> txt "Word64"
  SSFixed32 -> txt "Int32"
  SSFixed64 -> txt "Int64"
  SBool -> txt "Bool"
  SString -> txt "Text"
  SBytes -> txt "ByteString"


isUnboxableScalar :: ScalarType -> Bool
isUnboxableScalar = \case
  SString -> False
  SBytes -> False
  _ -> True


unpackPragma :: ScalarType -> Doc ann
unpackPragma s
  | isUnboxableScalar s = txt "{-# UNPACK #-} !"
  | otherwise = txt "!"


-- ---------------------------------------------------------------------------
-- Name conversion helpers
-- ---------------------------------------------------------------------------

-- | Build a Haskell type name from a scope path, joining with @'@.
scopedTypeName :: [Text] -> Text
scopedTypeName = T.intercalate "'" . fmap hsTypeName


-- | Build a Haskell record field name using 'PrefixedFields' naming.
scopedFieldName :: [Text] -> Text -> Text
scopedFieldName = scopedFieldNameWith PrefixedFields


-- | Build a Haskell record field name using the given naming strategy.
scopedFieldNameWith :: FieldNaming -> [Text] -> Text -> Text
scopedFieldNameWith PrefixedFields scope fName =
  let prefix = case scope of
        [] -> ""
        [s] -> lowerFirst (hsTypeName s)
        _ -> lowerFirst (T.intercalate "" (fmap hsTypeName scope))
  in escapeReserved (prefix <> upperFirst (snakeToCamel fName))
scopedFieldNameWith UnprefixedFields _scope fName =
  let name = lowerFirst (snakeToCamel fName)
  in case T.uncons name of
       Just (first, _) | isLower first -> escapeReserved name
       _ | fName == "_" -> "_field"
       _ -> fName


upperFirst :: Text -> Text
upperFirst s = case T.uncons s of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing -> s


-- | Build a Haskell enum constructor name from scope and value name.
scopedEnumCon :: [Text] -> Text -> Text
scopedEnumCon scope valName =
  case scope of
    [] -> snakeToPascal valName
    _ -> scopedTypeName scope <> "'" <> snakeToPascal valName


-- | Capitalize the first character of a name for use as a Haskell type name.
hsTypeName :: Text -> Text
hsTypeName t = case T.uncons t of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing -> t


hsModuleName :: Text -> Text
hsModuleName = T.intercalate "." . fmap capitalize . T.splitOn "."
  where
    capitalize t = case T.uncons t of
      Just (c, rest) -> T.cons (toUpper c) rest
      Nothing -> t


lowerFirst :: Text -> Text
lowerFirst s = case T.uncons s of
  Just (c, rest) -> T.cons (toLower c) rest
  Nothing -> s


titleCase :: Text -> Text
titleCase s = case T.uncons s of
  Just (c, rest) -> T.cons (toUpper c) (T.toLower rest)
  Nothing -> s


snakeToCamel :: Text -> Text
snakeToCamel t =
  let parts = T.splitOn "_" t
  in case parts of
       [] -> t
       (p : ps) -> T.concat (lowerFirst p : fmap titleCase ps)


{- | Proto3 JSON name conversion per the canonical spec
(mirror of @ToJsonName@ in the upstream protoc C++ source).

Differences vs. 'snakeToCamel' that the conformance suite
(@FieldNameWithMixedCases@, @FieldNameWithDoubleUnderscores@,
etc.) depends on:

* The case of every non-underscore character is preserved
  /as-is/, only the character /after/ an @_@ is upcased.
  So @FieldName8@ stays @FieldName8@ (we don't lowercase
  the leading @F@).
* Repeated underscores collapse: @field__name15@ becomes
  @fieldName15@ (only the next non-underscore is upcased).
* Trailing underscores are dropped: @Field_name18__@
  becomes @FieldName18@.
-}
protoJsonName :: Text -> Text
protoJsonName = T.pack . go False . T.unpack
  where
    go _ [] = []
    go capNext (c : cs)
      | c == '_' = go True cs
      | capNext = toUpper c : go False cs
      | otherwise = c : go False cs


snakeToPascal :: Text -> Text
snakeToPascal t =
  let parts = T.splitOn "_" t
  in T.concat (fmap titleCase parts)


escapeReserved :: Text -> Text
escapeReserved t
  | t `elem` haskellReserved = t <> "'"
  | otherwise = t


haskellReserved :: [Text]
haskellReserved =
  [ "type"
  , "class"
  , "data"
  , "default"
  , "deriving"
  , "do"
  , "else"
  , "if"
  , "import"
  , "in"
  , "infix"
  , "infixl"
  , "infixr"
  , "instance"
  , "let"
  , "module"
  , "newtype"
  , "of"
  , "then"
  , "where"
  , "case"
  , "foreign"
  , "forall"
  , "mdo"
  , "qualified"
  , "hiding"
  ]


replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 x (_ : ys) = x : ys
replaceAt n x (y : ys) = y : replaceAt (n - 1) x ys
