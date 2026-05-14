{- | Pure-text code generation for Haskell modules from parsed proto files.

This module powers the @protoc-gen-wireform@ plugin, the 'Proto.Setup'
pre-build hook, and the Template Haskell path in "Proto.TH". It produces
complete, compilable Haskell source text with:

* Proper cross-module imports via a 'TypeRegistry'
* Record types for messages, sum types for enums and oneofs
* @MessageEncode@ \/ @MessageDecode@ instances (size is computed inline)
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
    (via 'LazyMessage'). Default: 'False'.

  ['genJsonOverrides'] Per-message overrides for JSON instances. See
    'JsonOverride'.

  ['genHooks'] 'CodeGenHooks' callbacks for emitting extra declarations.
-}
module Proto.CodeGen (
  generateModule,
  generateModuleText,
  GenerateOpts (..),
  defaultGenerateOpts,
  FieldNaming (..),
  JsonOverride (..),
  defaultJsonOverrides,
  TypeRegistry,
  TypeInfo (..),
  TypeKind (..),
  buildTypeRegistry,
  builtinWellKnownTypes,
  moduleNameForProto,
  hsTypeName,
  hsModuleName,
  scopedTypeName,
  scopedFieldName,
  scopedFieldNameWith,
  scopedEnumCon,
  snakeToCamel,
  snakeToPascal,
  protoJsonName,
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

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.Char (isAsciiUpper, toLower, toUpper)
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8, Word64)
import Numeric (showHex)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Proto.CodeGen.Hooks
import Proto.CodeGen.Service qualified as Service
import Proto.IDL.AST
import Proto.IDL.Annotations (lookupSimpleOption, optionAsString)
import Proto.IDL.Descriptor (serializeFileDescriptor)
import Proto.IDL.Options
import Proto.IDL.Options.Custom (emptyCustomOptionRegistry, extractExtensionOptions, registerCustomOption)
import Proto.IDL.Parser.Resolver (ResolvedProto (..))
import Proto.Internal.CodeGen.Combinators (braceBlock, instanceHead, tshow, txt)
import Proto.Internal.Wire (WireType (..))


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


wireVarint, wire64Bit, wireLengthDelimited, wire32Bit :: Int
wireVarint = 0
wire64Bit = 1
wireLengthDelimited = 2
wire32Bit = 5


computeTagByte :: Int -> Int -> Int
computeTagByte fieldNum wireType = fieldNum * 8 + wireType


tagLit :: Text -> Int -> Doc ann
tagLit fnText wt =
  let fieldNum = read (T.unpack fnText) :: Int
  in pretty (computeTagByte fieldNum wt)


-- ---------------------------------------------------------------------------
-- Options
-- ---------------------------------------------------------------------------

{- | Controls how generated record field names relate to the proto
field name and the enclosing message name.
-}
data FieldNaming
  = -- | Prefix each field with the lowercased message name.
    -- @message Person { string name = 1; }@ becomes
    -- @personName :: !Text@. This is the default; it avoids
    -- name clashes without @DuplicateRecordFields@.
    PrefixedFields
  | -- | Use the proto field name directly (snake_case to camelCase).
    -- @message Person { string name = 1; }@ becomes
    -- @name :: !Text@.  Requires @DuplicateRecordFields@ if two
    -- messages share a field name. Works well with
    -- @OverloadedRecordDot@ and GHC's @HasField@ class.
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
  , genClosedEnums :: Bool
  -- ^ Use closed-enum decoding: reject unknown wire values with 'decodeFail'
  -- instead of mapping them to the @'Unknown N@ constructor. Corresponds to
  -- @features.enum_type = CLOSED@ in proto editions.
  , genLazySubmessages :: Bool
  -- ^ Decode submessage fields lazily via 'LazyMessage'.
  , genJsonOverrides :: Map Text JsonOverride
  -- ^ Per-message 'JsonOverride' keyed by fully-qualified proto name.
  , genHooks :: CodeGenHooks
  -- ^ Callbacks for emitting extra declarations during codegen.
  , genExternalTypes :: TypeRegistry
  {- ^ Externally-managed types: proto FQNs whose Haskell definitions
       live in some other package the consumer already depends on.

       Two effects, both driven off the same registry:

       1. __Skip emission.__ When the code generator walks the
          top-levels of the @.proto@ file being processed and finds a
          FQN that appears in @genExternalTypes@, it /omits/ the type
          declaration, default value, encode/decode/schema/JSON
          instances, and lenses for that entry. The user is expected
          to import the externally-managed module that already
          provides them.

       2. __Resolve references.__ When some other field's type refers
          to that FQN, the generator looks the entry up here and
          emits a qualified reference to its 'tiModule' /
          'tiHsName'. This is the same mechanism 'builtinWellKnownTypes'
          uses for the bundled @google.protobuf.*@ types.

       Typical population: at the Setup.hs / build-tool level, the
       consumer iterates over packages they depend on, asks each for
       its @TypeRegistry@ (typically by calling 'buildTypeRegistry'
       on the @.proto@ files that package owns), and unions the
       results into @genExternalTypes@ before invoking the generator.

       The two cases @genExternalTypes@ supports overlap with
       @reg@'s second argument to 'generateModule' / 'generateModuleText'
       — that registry covers /every/ proto in the current regen
       batch, including the file currently being generated.
       @genExternalTypes@ is specifically for types defined /outside/
       this regen pass, in another package's source tree.

       Defaults to 'mempty'. -}
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
    , genClosedEnums = False
    , genLazySubmessages = False
    , genJsonOverrides = defaultJsonOverrides
    , genHooks = defaultCodeGenHooks
    , genExternalTypes = mempty
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
              T.unlines
                [ "  toJSON msg ="
                , "    let s = msg.timestampSeconds"
                , "        n = msg.timestampNanos"
                , "        (rawDays, remSec) = s `divMod` 86400"
                , "        (hours, remH) = remSec `divMod` 3600"
                , "        (mins, secs) = remH `divMod` 60"
                , "        z = rawDays + 719468"
                , "        era = (if z >= 0 then z else z - 146096) `div` 146097"
                , "        doe = z - era * 146097"
                , "        yoe = (doe - doe `div` 1460 + doe `div` 36524 - doe `div` 146096) `div` 365"
                , "        y = yoe + era * 400"
                , "        doy = doe - (365 * yoe + yoe `div` 4 - yoe `div` 100)"
                , "        mp = (5 * doy + 2) `div` 153"
                , "        d = doy - (153 * mp + 2) `div` 5 + 1"
                , "        m = mp + (if mp < 10 then 3 else -9)"
                , "        y' = y + (if m <= 2 then 1 else 0)"
                , "        pad2 x = let sx = T.pack (show (abs x)) in if T.length sx < 2 then T.pack \"0\" <> sx else sx"
                , "        pad4 x = let sx = T.pack (show (abs x)) in T.replicate (4 - T.length sx) (T.pack \"0\") <> sx"
                , "        pad9 x = let sx = T.pack (show (abs x)) in T.replicate (9 - T.length sx) (T.pack \"0\") <> sx"
                , "        nanoStr = if n == 0 then T.pack \"\" else T.pack \".\" <> dropTrailingZeros (pad9 (fromIntegral n))"
                , "        dropTrailingZeros t = case T.stripSuffix (T.pack \"0\") t of { Just t' -> dropTrailingZeros t'; Nothing -> t }"
                , "    in Aeson.String (pad4 y' <> T.pack \"-\" <> pad2 (fromIntegral m) <> T.pack \"-\" <> pad2 (fromIntegral d)"
                , "         <> T.pack \"T\" <> pad2 hours <> T.pack \":\" <> pad2 mins <> T.pack \":\" <> pad2 secs"
                , "         <> nanoStr <> T.pack \"Z\")"
                ]
          , joFromJSON =
              T.unlines
                [ "  parseJSON (Aeson.String _) = pure defaultTimestamp"
                , "  parseJSON _ = fail \"Expected RFC 3339 timestamp string\""
                ]
          }
      )
    ,
      ( "google.protobuf.Duration"
      , JsonOverride
          { joToJSON =
              T.unlines
                [ "  toJSON msg ="
                , "    let s = msg.durationSeconds"
                , "        n = msg.durationNanos"
                , "        nanoStr = if n == 0 then T.pack \"\" else T.pack \".\" <> dropTrailingZeros (pad9 (abs (fromIntegral n)))"
                , "        dropTrailingZeros t = case T.stripSuffix (T.pack \"0\") t of { Just t' -> dropTrailingZeros t'; Nothing -> t }"
                , "        pad9 x = let sx = T.pack (show x) in T.replicate (9 - T.length sx) (T.pack \"0\") <> sx"
                , "        sign = if s < 0 || n < 0 then T.pack \"-\" else T.pack \"\""
                , "    in Aeson.String (sign <> T.pack (show (abs s)) <> nanoStr <> T.pack \"s\")"
                ]
          , joFromJSON =
              T.unlines
                [ "  parseJSON (Aeson.String _) = pure defaultDuration"
                , "  parseJSON _ = fail \"Expected duration string like \\\"3.5s\\\"\""
                ]
          }
      )
    ]


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
data TypeInfo = TypeInfo
  { tiModule :: Text
  -- ^ The Haskell module containing this type.
  , tiHsName :: Text
  -- ^ The Haskell type name.
  , tiKind :: TypeKind
  -- ^ Whether this is a message or enum.
  }
  deriving stock (Show, Eq)


-- | Maps fully-qualified proto type names to their Haskell type info.
type TypeRegistry = Map Text TypeInfo


{- | Build a TypeRegistry from a list of (proto file path, resolved proto).
Walks all top-levels and nested definitions, recording their FQ names.
| Build a TypeRegistry from resolved proto files. Also includes entries
for the standard well-known Google protobuf types, pointing to their
generated modules under @Proto.Google.Protobuf.*@.
-}
buildTypeRegistry :: GenerateOpts -> [(FilePath, ResolvedProto)] -> TypeRegistry
buildTypeRegistry opts rpList =
  Map.union
    builtinWellKnownTypes
    (Map.unions (fmap (\(fp, rp) -> registryForFile opts (normalizeProtoPath fp) (rpFile rp)) rpList))


{- | Registry entries for standard Google well-known types.
These reference the generated modules at @Proto.Google.Protobuf.*@,
which are produced by running the code generator on the bundled
@proto/google/protobuf/*.proto@ files with prefix @Proto@.
-}
builtinWellKnownTypes :: TypeRegistry
builtinWellKnownTypes =
  Map.fromList
    [ wkt "google.protobuf.Duration" "Proto.Google.Protobuf.WellKnownTypes.Duration" "Duration"
    , wkt "google.protobuf.Timestamp" "Proto.Google.Protobuf.WellKnownTypes.Timestamp" "Timestamp"
    , wkt "google.protobuf.Empty" "Proto.Google.Protobuf.WellKnownTypes.Empty" "Empty"
    , wkt "google.protobuf.Any" "Proto.Google.Protobuf.WellKnownTypes.Any" "Any"
    , wkt "google.protobuf.Struct" "Proto.Google.Protobuf.WellKnownTypes.Struct" "Struct"
    , wkt "google.protobuf.Value" "Proto.Google.Protobuf.WellKnownTypes.Struct" "Value"
    , wkt "google.protobuf.ListValue" "Proto.Google.Protobuf.WellKnownTypes.Struct" "ListValue"
    , wkt "google.protobuf.NullValue" "Proto.Google.Protobuf.WellKnownTypes.Struct" "NullValue"
    , wkt "google.protobuf.FieldMask" "Proto.Google.Protobuf.WellKnownTypes.FieldMask" "FieldMask"
    , wkt "google.protobuf.SourceContext" "Proto.Google.Protobuf.WellKnownTypes.SourceContext" "SourceContext"
    , wkt "google.protobuf.DoubleValue" "Proto.Google.Protobuf.WellKnownTypes.Wrappers" "DoubleValue"
    , wkt "google.protobuf.FloatValue" "Proto.Google.Protobuf.WellKnownTypes.Wrappers" "FloatValue"
    , wkt "google.protobuf.Int64Value" "Proto.Google.Protobuf.WellKnownTypes.Wrappers" "Int64Value"
    , wkt "google.protobuf.UInt64Value" "Proto.Google.Protobuf.WellKnownTypes.Wrappers" "UInt64Value"
    , wkt "google.protobuf.Int32Value" "Proto.Google.Protobuf.WellKnownTypes.Wrappers" "Int32Value"
    , wkt "google.protobuf.UInt32Value" "Proto.Google.Protobuf.WellKnownTypes.Wrappers" "UInt32Value"
    , wkt "google.protobuf.BoolValue" "Proto.Google.Protobuf.WellKnownTypes.Wrappers" "BoolValue"
    , wkt "google.protobuf.StringValue" "Proto.Google.Protobuf.WellKnownTypes.Wrappers" "StringValue"
    , wkt "google.protobuf.BytesValue" "Proto.Google.Protobuf.WellKnownTypes.Wrappers" "BytesValue"
    ]
  where
    wkt fqn modName hsName = (fqn, TypeInfo modName hsName TKMessage)


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

Prefers @csharp_namespace@ if set (already PascalCase with dots), appending
the proto file's base name for disambiguation when multiple files share a
namespace (e.g. enum files). Falls back to the file path.

@csharp_namespace = "Temporalio.Api.Common.V1"@ with file @message.proto@
produces @Proto.Temporalio.Api.Common.V1.Message@.
-}
moduleNameForProto :: GenerateOpts -> FilePath -> ProtoFile -> Text
moduleNameForProto opts filePath pf =
  let fo = extractFileOptions (protoOptions pf)
      baseName = fileBaseName filePath
  in case foCsharpNamespace fo of
      Just ns -> genModulePrefix opts <> "." <> ns <> "." <> baseName
      Nothing -> genModulePrefix opts <> "." <> moduleFromPath filePath
  where
    fileBaseName fp =
      let t = T.pack fp
          stripped = fromMaybe t (T.stripSuffix ".proto" t)
          parts = T.splitOn "/" stripped
      in pathPartToModule (last parts)

    moduleFromPath fp =
      let t = T.pack fp
          s = fromMaybe t (T.stripSuffix ".proto" t)
          parts = T.splitOn "/" s
      in T.intercalate "." (fmap pathPartToModule parts)

    pathPartToModule t = snakeToPascal (capitalize t)

    capitalize t = case T.uncons t of
      Just (c, rest) -> T.cons (toUpper c) rest
      Nothing -> t


-- ---------------------------------------------------------------------------
-- Code generation
-- ---------------------------------------------------------------------------

data GenCtx = GenCtx
  { gcOpts :: GenerateOpts
  , gcRegistry :: TypeRegistry
  , gcThisMod :: Text
  , gcPkg :: Maybe Text
  , gcLocalTypes :: Set Text
  }


-- | Produce a record field name using the context's naming mode.
ctxFieldName :: GenCtx -> [Text] -> Text -> Text
ctxFieldName ctx = scopedFieldNameWith (genFieldNaming (gcOpts ctx))


-- | Generate a complete Haskell module as a 'Doc' from a proto file.
generateModule :: GenerateOpts -> TypeRegistry -> FilePath -> ProtoFile -> Doc ann
generateModule opts reg filePath pf =
  let thisMod = moduleNameForProto opts filePath pf
      -- Fold the user-supplied @genExternalTypes@ map into the
      -- resolution registry so cross-references in this file pick up
      -- the externally-managed module qualifications.
      reg' = reg <> genExternalTypes opts
      localTypes = collectLocalTypes (fromMaybe "" (protoPackage pf)) [] (protoTopLevels pf)
      -- For editions schemas, derive effective packed-encoding and closed-enum
      -- flags from the file-level features.* option overrides.
      editionFs = case protoSyntax pf of
        Editions ed -> Just (resolveFileFeatures (Editions ed) (protoOptions pf))
        _           -> Nothing
      effectivePacked = case editionFs of
        Just fs -> featureRepeatedFieldEncoding fs == PackedEncoding
        Nothing -> genPackedRepeated opts
      effectiveClosedEnums = case editionFs of
        Just fs -> featureEnumType fs == ClosedEnum
        Nothing -> genClosedEnums opts
      ctx =
        GenCtx
          { gcOpts = opts
              { genPackedRepeated = effectivePacked
              , genClosedEnums    = effectiveClosedEnums
              }
          , gcRegistry = reg'
          , gcThisMod = thisMod
          , gcPkg = protoPackage pf
          , gcLocalTypes = localTypes
          }
      -- Drop top-levels whose FQN is already owned by another
      -- package (per @genExternalTypes@). The user is expected to
      -- import that other package's module for the missing types;
      -- the generator just won't redefine them here.
      visibleTopLevels =
        filter (not . topLevelManagedExternally opts (protoPackage pf)) (protoTopLevels pf)
      body = concatMap (genTopLevel ctx []) visibleTopLevels
      referencedTypes = collectReferencedTypes (protoTopLevels pf)
      importedModules = computeImports ctx referencedTypes
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
      , genImports importedModules
      , mempty
      , genFileDescriptorBinding filePath pf
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
collectReferencedTypes :: [TopLevel] -> Set Text
collectReferencedTypes = foldMap goTL
  where
    goTL = \case
      TLMessage msg -> goMsg msg
      TLService svc -> foldMap goRpc (svcRpcs svc)
      _ -> Set.empty
    goMsg msg = foldMap goElem (msgElements msg)
    goElem = \case
      MEField fd -> goFT (fieldType fd)
      MEMapField mf -> goFT (mapValueType mf)
      MEOneof od -> foldMap (goFT . oneofFieldType) (oneofFields od)
      MEMessage inner -> goMsg inner
      _ -> Set.empty
    goRpc r = Set.fromList [rpcInput r, rpcOutput r]
    goFT = \case
      FTNamed n -> Set.singleton n
      _ -> Set.empty


-- Compute the set of external modules referenced by this file.
computeImports :: GenCtx -> Set Text -> Set Text
computeImports ctx refs =
  let thisMod = gcThisMod ctx
  in Set.fromList
      [ tiModule ti
      | ref <- Set.toList refs
      , Just ti <- [resolveType ctx ref]
      , tiModule ti /= thisMod
      ]


-- Resolve a proto type name to TypeInfo. Tries FQ lookup first, then
-- with current package prefix, then with parent message scopes, then simple.
resolveType :: GenCtx -> Text -> Maybe TypeInfo
resolveType ctx = resolveTypeWithScope ctx []


resolveTypeWithScope :: GenCtx -> [Text] -> Text -> Maybe TypeInfo
resolveTypeWithScope ctx scope name =
  let reg = gcRegistry ctx
      pkg = fromMaybe "" (gcPkg ctx)
      -- Check inner scopes first (proto type-resolution rule: innermost wins),
      -- then package-qualified, then bare. This ensures a locally-defined nested
      -- type (e.g. FieldDescriptorProto.Type) is preferred over a same-named
      -- top-level type in the same package (e.g. google.protobuf.Type).
      candidates =
        fmap (\s -> pkg <> "." <> T.intercalate "." s <> "." <> name) (filter (not . null) (tails' scope))
          <> [ pkg <> "." <> name
             , name
             ]
  in case firstJust (`Map.lookup` reg) candidates of
      Just ti -> Just ti
      Nothing ->
        let suffix = "." <> name
            matches = fmap snd (filter (\(k, _) -> T.isSuffixOf suffix k || k == name) (Map.toList reg))
        in case matches of
            (ti : _) -> Just ti
            [] -> Nothing
  where
    tails' [] = []
    tails' xs = xs : tails' (init xs)
    firstJust _ [] = Nothing
    firstJust f (x : xs) = case f x of
      Just v -> Just v
      Nothing -> firstJust f xs


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
          ]
        PrefixedFields -> []
  in vsep $
      [ txt "{-# LANGUAGE StrictData #-}"
      , txt "{-# LANGUAGE DeriveGeneric #-}"
      , txt "{-# LANGUAGE DeriveAnyClass #-}"
      , txt "{-# LANGUAGE DerivingStrategies #-}"
      , txt "{-# LANGUAGE OverloadedStrings #-}"
      , txt "{-# LANGUAGE OverloadedRecordDot #-}"
      ]
        <> dupRecordExts
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


genImports :: Set Text -> Doc ann
genImports externalModules =
  vsep $
    [ txt "import Data.ByteString (ByteString)"
    , txt "import qualified Data.ByteString as BS"
    , txt "import qualified Wireform.Builder as B"
    , txt "import Data.Int (Int32, Int64)"
    , txt "import Data.Text (Text)"
    , txt "import qualified Data.Text as T"
    , txt "import Data.Word (Word8, Word32, Word64)"
    , txt "import qualified Data.IntMap.Strict as IntMap"
    , txt "import qualified Data.Map.Strict as Map"
    , txt "import qualified Data.Vector as V"
    , txt "import qualified Data.Vector.Unboxed as VU"
    , txt "import GHC.Generics (Generic)"
    , txt "import Control.DeepSeq (NFData(..))"
    , txt "import Data.Hashable (Hashable(..))"
    , txt "import Proto.Internal.Encode"
    , txt "import Proto.Internal.Decode"
    , txt "import Proto.Internal.GrowList (GrowList, emptyGrowList, snocGrowList, growListToVector)"
    , txt "import qualified Data.Aeson as Aeson"
    , txt "import qualified Data.Aeson.Types as Aeson"
    , txt "import qualified Data.Aeson.Key as AesonKey"
    , txt "import qualified Data.Aeson.KeyMap as AesonKM"
    , txt "import Proto.Internal.JSON (jsonObject, (.=:), parseFieldMaybe, bytesFieldToJSON, parseBytesFieldMaybe, bytesMapFieldToJSON, parseBytesMapFieldMaybe, protoBytesToJSON, OneofVariantNullSemantics (..), parseOneofVariants)"
    , txt "import Data.Proxy (Proxy(..))"
    , txt "import Proto.Schema (ProtoMessage(..), SomeFieldDescriptor(..), FieldDescriptor(..), FieldTypeDescriptor(..), ScalarFieldType(..), FieldLabel'(..))"
    , txt "import Proto.Registry (IsMessage)"
    , txt "import Proto.Registry qualified"
    , txt "import qualified Proto.Extension"
    , txt "import Proto.Internal.Wire (Tag(..), WireType(..))"
    , txt "import Proto.Internal.Wire.Encode (putTag, putVarint, putFixed32, putFixed64,"
    , txt "  putFloat, putDouble, putText, putByteString, putLengthDelimited,"
    , txt "  putSVarint32, putSVarint64, putVarintSigned,"
    , txt "  varintSize, tagSize,"
    , txt "  varintSize32, zigZag32, zigZag64)"
    , txt "import Proto.Internal.Encode.Archetype (archVarint, archSVarint32, archSVarint64,"
    , txt "  archFixed32, archFixed64, archFloat, archDouble, archBool,"
    , txt "  archString, archBytes, archSubmessage)"
    ]
      <> fmap genQualifiedImport (Set.toAscList externalModules)


genQualifiedImport :: Text -> Doc ann
genQualifiedImport modName =
  txt "import qualified " <> pretty modName <> txt " as " <> pretty (moduleAlias modName)


{- | Derive a short, deterministic alias from a module name.
Strips common prefixes and uses initials for boilerplate segments.

@Proto.Google.Protobuf.WellKnownTypes.Timestamp@             -> @PB_Timestamp@
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
genFileDescriptorBinding :: FilePath -> ProtoFile -> Doc ann
genFileDescriptorBinding filePath pf =
  let fdpBytes = serializeFileDescriptor filePath pf
      escapedLit = byteStringToHsLiteral fdpBytes
  in vsep
      [ txt "-- | Serialized FileDescriptorProto for this .proto file."
      , txt "-- Decode with @Proto.Decode.decodeMessage@."
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
         -- 'genEncodeInstance' emits a single fused 'buildSized'
         -- method (returning 'SizedBuilder'); 'messageSize' falls
         -- out as a free function over 'buildSized', so the
         -- separate 'MessageSize' instance the codegen used to
         -- emit has been retired. See 'genEncodeInstance' for the
         -- O(N²) → O(N) rationale.
         , genEncodeInstance ctx scope' msg
         , mempty
         , genDecodeInstance ctx scope' msg
         , mempty
         , genProtoMessageInstance ctx scope' msg
         , mempty
         , genIsMessageInstance scope'
         , mempty
         , genToJSONInstance ctx scope' msg
         , mempty
         , genFromJSONInstance ctx scope' msg
         , mempty
         , genHashableInstance ctx scope' msg
         , mempty
         , genHasExtensionsInstance scope' msg
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
  _ -> []


-- ---------------------------------------------------------------------------
-- Data declarations
-- ---------------------------------------------------------------------------

genMessageDataDecl :: GenCtx -> [Text] -> MessageDef -> Doc ann
genMessageDataDecl ctx scope msg =
  let tyN = scopedTypeName scope
      userFields = concatMap (extractFieldDecl ctx scope) (msgElements msg)
      unknownFieldDecl = pretty (unknownFieldAccessor scope) <+> txt "::" <+> txt "![UnknownField]"
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
    <> pretty (scopedTypeName scope <> "'" <> snakeToPascal (oneofName od))
    <> txt ")"


genOneofDecl :: GenCtx -> [Text] -> OneofDef -> Doc ann
genOneofDecl ctx scope od =
  let tyN = scopedTypeName scope <> "'" <> snakeToPascal (oneofName od)
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
      pretty (oneofConName s (oneofName od) (oneofFieldName f))
        <+> hsOneofFieldType cx s (oneofFieldType f)


{- | Emit a /standalone/ 'Aeson.ToJSON' instance for a oneof carrier.

Proto3 JSON encodes a oneof by inlining the selected branch's key
and value at the /parent/ message level (see
'genToJSONFieldFragment'). When something serialises a oneof sum
value on its own (e.g. for debug printing, or because a user
treats the carrier as a top-level type), we render the same
single-key object: @{ "\<branchJsonKey\>": \<value\> }@. That keeps
the standalone instance compatible with what would have been
emitted inline, and it round-trips through 'parseJSON' below.

If the oneof has no fields (a malformed @oneof X {}@), we emit a
fallback that produces @Aeson.Null@.
-}
genOneofToJSONInstance :: GenCtx -> [Text] -> OneofDef -> Doc ann
genOneofToJSONInstance ctx scope od =
  let tyN = scopedTypeName scope <> "'" <> snakeToPascal (oneofName od)
      branches = fmap (oneofBranchToJSON ctx scope (oneofName od)) (oneofFields od)
  in case branches of
      [] ->
        vsep
          [ instanceHead "Aeson.ToJSON" tyN
          , indent 2 (txt "toJSON _ = Aeson.Null")
          ]
      _ ->
        vsep $
          [ instanceHead "Aeson.ToJSON" tyN
          , indent 2 (txt "toJSON v0 = jsonObject (case v0 of")
          ]
            <> fmap (\b -> indent 4 (genOneofStandaloneToJSONArm b)) branches
            <> [indent 4 (txt ")")]


-- | One arm of the standalone oneof @toJSON@'s @case v0 of@.
genOneofStandaloneToJSONArm :: JSONOneofBranch -> Doc ann
genOneofStandaloneToJSONArm branch =
  let keyLit = pretty ("\"" :: Text) <> pretty (jobJsonKey branch) <> pretty ("\"" :: Text)
  in case jobShape branch of
      OBSNullValue ->
        pretty (jobConstructor branch) <> txt " _ -> [(" <> keyLit <> txt ", Aeson.Null)]"
      _ ->
        pretty (jobConstructor branch) <> txt " v -> [" <> keyLit <> txt " .=: v]"


{- | Emit a /standalone/ 'Aeson.FromJSON' instance for a oneof
carrier. Symmetric to 'genOneofToJSONInstance': scan the object
for exactly one of the branch keys (via 'parseOneofVariants'), fail
otherwise. Useful for tests and for tooling that needs to parse
a oneof value out of context.
-}
genOneofFromJSONInstance :: GenCtx -> [Text] -> OneofDef -> Doc ann
genOneofFromJSONInstance ctx scope od =
  let tyN = scopedTypeName scope <> "'" <> snakeToPascal (oneofName od)
      branches = fmap (oneofBranchToJSON ctx scope (oneofName od)) (oneofFields od)
  in case branches of
      [] ->
        vsep
          [ instanceHead "Aeson.FromJSON" tyN
          , indent 2 (pretty ("parseJSON _ = fail \"Empty oneof has no JSON form\"" :: Text))
          ]
      _ ->
        vsep
          [ instanceHead "Aeson.FromJSON" tyN
          , indent 2 $
              vsep
                [ txt "parseJSON = Aeson.withObject "
                    <> pretty ("\"" :: Text)
                    <> pretty tyN
                    <> pretty ("\"" :: Text)
                    <> txt " $ \\obj -> do"
                , indent 2 $
                    vsep
                      [ txt "mr <- parseOneofVariants obj"
                      , indent 2 $
                          vsep
                            [ txt "[ " <> genOneofBranchParser (head branches)
                            , vsep (fmap (\b -> txt ", " <> genOneofBranchParser b) (tail branches))
                            , txt "]"
                            ]
                      , txt "case mr of"
                      , indent 2 $
                          vsep
                            [ txt "Just v -> pure v"
                            , txt "Nothing -> fail \"Expected one of: "
                                <> pretty (T.intercalate ", " (fmap jobJsonKey branches))
                                <> txt "\""
                            ]
                      ]
                ]
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
      unknownDefault = pretty (unknownFieldAccessor scope) <+> txt "=" <+> txt "[]"
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
  Just Optional -> txt "Nothing"
  _ -> case ft of
    FTScalar SBool -> txt "False"
    FTScalar SString -> pretty ("\"\"" :: Text)
    FTScalar SBytes -> pretty ("\"\"" :: Text)
    FTScalar _ -> txt "0"
    FTNamed n ->
      case resolveType ctx n of
        Just ti | tiKind ti == TKEnum -> txt "(Prelude.toEnum 0)"
        _ -> txt "Nothing"


-- ---------------------------------------------------------------------------
-- Encode instances
-- ---------------------------------------------------------------------------

{- | Emit the 'MessageEncode' instance for a message.

The instance defines 'buildSized' — the fused
@(SizedBuilder)@-returning method that carries the message's wire
byte count alongside the byte fragments in a single pass. The
default 'buildMessage' / 'messageSize' implementations off
'MessageEncode' compose from 'buildSized' for free, so the
codegen never needs to emit a separate @MessageSize@ instance or
duplicate the field-walk into two parallel functions. The old
'O(N²)' shape (parent calls @messageSize child@ which traverses
the entire subtree, then 'buildMessage' traverses the subtree
again) is replaced by a single 'O(N)' fused traversal.
-}
genEncodeInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genEncodeInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fields = extractAllFields ctx scope (msgElements msg)
      unknownAcc = "msg." <> unknownFieldAccessor scope
  in vsep
      [ instanceHead "MessageEncode" tyN
      , indent 2 $
          vsep
            [ txt "buildSized msg ="
            , indent 2 $ case fields of
                [] -> txt "encodeUnknownFieldsSized " <> pretty unknownAcc
                _ ->
                  vsep
                    ( zipWith (genFieldBuild ctx) [0 ..] fields
                        <> [txt "<> encodeUnknownFieldsSized " <> pretty unknownAcc]
                    )
            ]
      ]


genFieldBuild :: GenCtx -> Int -> FieldInfoFull -> Doc ann
genFieldBuild ctx idx fi =
  let op = if idx == 0 then mempty else txt "<> "
      accessor = "msg." <> fifAccessor fi
      fn = T.pack (show (fifFieldNum fi))
  in op <> case fifKind fi of
      FKScalar lbl ft -> genBuildExprScalar ctx fn accessor lbl ft
      FKNamed lbl name tk -> genBuildExprNamed ctx fn accessor lbl name tk
      FKMap keyT valT -> genBuildExprMap ctx fn accessor keyT valT
      FKOneof scope ood -> genBuildExprOneof ctx scope fn accessor ood


genBuildExprScalar :: GenCtx -> Text -> Text -> Maybe FieldLabel -> ScalarType -> Doc ann
genBuildExprScalar ctx fn accessor lbl st = case lbl of
  Just Repeated -> genRepeatedScalarBuild (genPackedRepeated (gcOpts ctx)) fn accessor st
  Just Optional -> txt "(maybe mempty (\\v -> " <> genSingleScalarBuild fn "v" st <> txt ") " <> pretty accessor <> txt ")"
  _ -> txt "(if " <> scalarDefaultCheck accessor st <> txt " then mempty else " <> genSingleScalarBuild fn accessor st <> txt ")"


genBuildExprNamed :: GenCtx -> Text -> Text -> Maybe FieldLabel -> Text -> TypeKind -> Doc ann
genBuildExprNamed ctx fn accessor lbl name tk = case tk of
  TKEnum ->
    -- Use toProtoEnum<TypeName> to get the wire value. The derived Haskell
    -- 'fromEnum' gives a 0-based constructor index, which is wrong when the
    -- proto enum's first value isn't 0 (e.g. FieldDescriptorProto.Type
    -- starts at 1). toProtoEnum<T> always returns the correct wire value.
    let toProto = case resolveType ctx name of
          Just ti -> txt "toProtoEnum" <> pretty (tiHsName ti)
          Nothing -> txt "Prelude.fromEnum" -- fallback (shouldn't happen)
    in case lbl of
    Just Repeated ->
      txt "V.foldl' (\\acc v -> acc <> archVarint "
        <> tagLit fn wireVarint
        <+> txt "(fromIntegral (" <> toProto <+> txt "v))) mempty "
        <> pretty accessor
    Just Optional ->
      txt "(maybe mempty (\\v -> archVarint "
        <> tagLit fn wireVarint
        <+> txt "(fromIntegral (" <> toProto <+> txt "v))) "
        <> pretty accessor
        <> txt ")"
    _ ->
      txt "(if "
        <> toProto
        <+> pretty accessor
        <> txt " == 0 then mempty else archVarint "
        <> tagLit fn wireVarint
        <+> txt "(fromIntegral ("
        <> toProto
        <+> pretty accessor
        <> txt ")))"
  TKMessage -> case lbl of
    -- New shape: 'archSubmessage' takes the child's SizedBuilder
    -- directly. The size flows up from the child's own 'buildSized'
    -- traversal — no separate @messageSize@ pre-pass.
    Just Repeated -> txt "V.foldl' (\\acc v -> acc <> archSubmessage " <> tagLit fn wireLengthDelimited <+> txt "(buildSized v)) mempty " <> pretty accessor
    Just Optional -> txt "(maybe mempty (\\v -> archSubmessage " <> tagLit fn wireLengthDelimited <+> txt "(buildSized v)) " <> pretty accessor <> txt ")"
    _ -> txt "(maybe mempty (\\v -> archSubmessage " <> tagLit fn wireLengthDelimited <+> txt "(buildSized v)) " <> pretty accessor <> txt ")"


genBuildExprMap :: GenCtx -> Text -> Text -> ScalarType -> FieldType -> Doc ann
genBuildExprMap ctx fn accessor keyT valT =
  -- Map field: fold over the entries, fusing each (key, value)
  -- pair into a 'SizedBuilder' submessage via 'mapField'.
  -- Keys go in slot 1 and values in slot 2 of the entry; both are
  -- encoded via @sizedFieldX@ helpers so each entry's wire size is
  -- known without a separate pass.
  txt "Map.foldlWithKey' (\\acc k v -> acc <> mapField "
    <> pretty fn
    <> txt " ("
    <> genMapKeyEncode keyT
    <> txt " k) ("
    <> genMapValEncode ctx valT
    <> txt " v)) mempty "
    <> pretty accessor


-- | Sized-encoder for a map key (always at field number 1 inside the
-- entry submessage). Returns a partially-applied @sizedFieldX 1@ so
-- the caller can supply the key value.
genMapKeyEncode :: ScalarType -> Doc ann
genMapKeyEncode st = case st of
  SString -> txt "fieldString 1"
  SBool -> txt "fieldBool 1"
  SInt32 -> txt "(\\x -> fieldVarint 1 (fromIntegral x))"
  SInt64 -> txt "(\\x -> fieldVarint 1 (fromIntegral x))"
  SUInt32 -> txt "(\\x -> fieldVarint 1 (fromIntegral x))"
  SUInt64 -> txt "fieldVarint 1"
  SSInt32 -> txt "fieldSVarint32 1"
  SSInt64 -> txt "fieldSVarint64 1"
  SFixed32 -> txt "fieldFixed32 1"
  SFixed64 -> txt "fieldFixed64 1"
  SSFixed32 -> txt "(\\x -> fieldFixed32 1 (fromIntegral x))"
  SSFixed64 -> txt "(\\x -> fieldFixed64 1 (fromIntegral x))"
  _ -> txt "fieldBytes 1"


-- | Sized-encoder for a map value (always at field number 2 inside
-- the entry submessage).
genMapValEncode :: GenCtx -> FieldType -> Doc ann
genMapValEncode ctx = \case
  FTScalar st -> genMapValEncodeScalar 2 st
  FTNamed n -> case resolveType ctx n of
    Just ti | tiKind ti == TKEnum ->
      txt "(\\x -> fieldVarint 2 (fromIntegral (toProtoEnum" <> pretty (tiHsName ti) <+> txt "x)))"
    -- Submessage value: build the child as a SizedBuilder and wrap
    -- with the slot-2 length prefix via 'fieldMessage'.
    _ -> txt "(fieldMessage 2 . buildSized)"
  where
    genMapValEncodeScalar :: Int -> ScalarType -> Doc ann
    genMapValEncodeScalar fn st = case st of
      SString -> txt "fieldString " <> pretty (T.pack (show fn))
      SBytes -> txt "fieldBytes " <> pretty (T.pack (show fn))
      SBool -> txt "fieldBool " <> pretty (T.pack (show fn))
      SDouble -> txt "fieldDouble " <> pretty (T.pack (show fn))
      SFloat -> txt "fieldFloat " <> pretty (T.pack (show fn))
      SFixed32 -> txt "fieldFixed32 " <> pretty (T.pack (show fn))
      SFixed64 -> txt "fieldFixed64 " <> pretty (T.pack (show fn))
      SSFixed32 -> txt "fieldFixed32 " <> pretty (T.pack (show fn)) <> txt " . fromIntegral"
      SSFixed64 -> txt "fieldFixed64 " <> pretty (T.pack (show fn)) <> txt " . fromIntegral"
      _ -> txt "fieldVarint " <> pretty (T.pack (show fn)) <> txt " . fromIntegral"


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
  let conName = oneofConName scope ooName (oneofFieldName f)
      fn = T.pack (show (unFieldNumber (oneofFieldNumber f)))
  in txt "Just ("
      <> pretty conName
      <+> txt "v) -> "
      <> case oneofFieldType f of
        FTScalar st -> genSingleScalarBuild fn "v" st
        FTNamed n -> case resolveType ctx n of
          Just ti
            | tiKind ti == TKEnum ->
                txt "archVarint " <> tagLit fn wireVarint <+> txt "(fromIntegral (toProtoEnum" <> pretty (tiHsName ti) <+> txt "v))"
          -- Submessage variant: hand the child's SizedBuilder
          -- straight to 'archSubmessage'. No separate size pass.
          _ -> txt "archSubmessage " <> tagLit fn wireLengthDelimited <+> txt "(buildSized v)"


genSingleScalarBuild :: Text -> Text -> ScalarType -> Doc ann
genSingleScalarBuild fn accessor = \case
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


genRepeatedScalarBuild :: Bool -> Text -> Text -> ScalarType -> Doc ann
genRepeatedScalarBuild packed fn accessor = \case
  SString ->
    txt "V.foldl' (\\acc v -> acc <> archString " <> tagLit fn wireLengthDelimited <+> txt "v) mempty " <> pretty accessor
  SBytes ->
    txt "V.foldl' (\\acc v -> acc <> archBytes " <> tagLit fn wireLengthDelimited <+> txt "v) mempty " <> pretty accessor
  s ->
    if packed
      then txt "encode" <> pretty (packedFnName s) <+> pretty fn <+> pretty accessor
      else
        -- Expanded encoding: one tag-value pair per element (proto2 style).
        txt "(VU.foldl' (\\acc v -> acc <> " <> genSingleScalarBuild fn "v" s <> txt ") mempty " <> pretty accessor <> txt ")"


scalarDefaultCheck :: Text -> ScalarType -> Doc ann
scalarDefaultCheck accessor = \case
  SBool -> pretty accessor <+> txt "== False"
  SString -> pretty accessor <+> txt "== T.empty"
  SBytes -> txt "BS.null " <> pretty accessor
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
  SSFixed32 -> "PackedFixed32"
  SSFixed64 -> "PackedFixed64"
  SBool -> "PackedBool"
  s -> error ("Cannot pack: " <> show s)


-- ---------------------------------------------------------------------------
-- Decode instances
-- ---------------------------------------------------------------------------

-- | Binding threaded through textual @messageDecoder@ (fast path and
-- generic @loop@): starts at @default\<Ty\>@ and is refined with record
-- updates after each decoded field. Using the message type itself keeps
-- arity fixed at one and avoids each fast-path stage closing over every
-- accumulator parameter (which used to make GHC's simplifier blow up on
-- large descriptors).
decodeThreadMsg :: Text
decodeThreadMsg = "msg"

-- | Marker in fast-path @fpsWrap@ strings from 'eligibleStage' —
-- substituted with 'decodeThreadMsg' when emitting @inOrderStage@
-- continuations (repeated slots need the previous field value).
decodeAccPlaceholder :: Text
decodeAccPlaceholder = "__DEC_MSG__"


-- ---------------------------------------------------------------------------
-- GrowList accumulator parameters
-- ---------------------------------------------------------------------------

{- | One @GrowList@-backed accumulator parameter threaded through the
decoder for a repeated unpacked boxed field (repeated message, repeated
string, repeated bytes).

For these field kinds, the previous @V.snoc@ approach was O(n²) — every
append copied the full vector. @GrowAccum@ replaces the vector with a
difference-list (@GrowList@) that is O(1) per snoc and converted to the
final @V.Vector@ only at end-of-input.

Repeated packed scalars and repeated enums do /not/ get a @GrowAccum@
— their fast path already returns the full vector in one shot via the
packed decoder, so the per-element append path is rare (non-conforming
writers only) and not worth complicating.
-}
data GrowAccum = GrowAccum
  { gaName :: !Text
  -- ^ Parameter name in generated code, e.g. @\"gl_0\"@.
  , gaFieldNum :: !Int
  -- ^ Proto field number, used to match the field in the generic loop.
  , gaAccessor :: !Text
  -- ^ Record field accessor, e.g. @\"withRepeatedTags\"@.
  }


-- | Collect 'GrowAccum' descriptors for the fields of a message that
-- benefit from @GrowList@ accumulation.  Each repeated unpacked boxed
-- field gets one entry.
collectGrowAccums :: [FieldInfoFull] -> [GrowAccum]
collectGrowAccums flds =
  let eligible fi = case fifKind fi of
        FKNamed (Just Repeated) _ TKMessage -> True
        FKScalar (Just Repeated) SString -> True
        FKScalar (Just Repeated) SBytes -> True
        _ -> False
      makeAcc i fi = GrowAccum
        { gaName = "gl_" <> T.pack (show (i :: Int))
        , gaFieldNum = fifFieldNum fi
        , gaAccessor = fifAccessor fi
        }
  in zipWith makeAcc [0 ..] (filter eligible flds)


-- | Render the GrowList argument list as a space-separated suffix for
-- a stage or loop call, e.g. @\" gl_0 gl_1\"@.  Empty string when there
-- are no accumulators.
growAccumArgs :: [GrowAccum] -> Text
growAccumArgs [] = ""
growAccumArgs gas = " " <> T.unwords (fmap gaName gas)


-- | Build the @pure (msg { … })@ EOI expression that:
--   (1) restores every GrowList field to a proper @V.Vector@, and
--   (2) reverses the unknown-fields accumulator.
--
-- When there are no GrowList fields, we optimise the common case where
-- @unknownFields@ is empty: emit @case msg.unknownFld of { [] -> msg; _ -> … }@
-- so GHC can statically eliminate the record reconstruction when no unknown
-- fields were seen (the fast path for well-formed messages).
buildResultExpr :: Text -> Text -> [GrowAccum] -> Text
buildResultExpr msgVar unknownFld gas =
  let glParts = fmap (\ga ->
        gaAccessor ga <> " = growListToVector " <> gaName ga) gas
      fullUpdate = msgVar <> " { " <> T.intercalate ", " (glParts <> [ufPart]) <> " }"
        where ufPart = unknownFld <> " = reverse (" <> msgVar <> "." <> unknownFld <> ")"
  in if null gas
      -- No GrowList fields: can skip record reconstruction when unknowns empty.
      then "(pure (case " <> msgVar <> "." <> unknownFld <> " of { [] -> " <> msgVar
           <> "; _ -> " <> msgVar <> " { " <> unknownFld <> " = reverse (" <> msgVar
           <> "." <> unknownFld <> ") } }))"
      -- GrowList fields must be converted regardless; include the null-check too.
      else "(pure (case " <> msgVar <> "." <> unknownFld <> " of { [] -> "
           <> msgVar <> " { " <> T.intercalate ", " (fmap (\ga -> gaAccessor ga <> " = growListToVector " <> gaName ga) gas) <> " }"
           <> "; _ -> " <> fullUpdate <> " }))"

msgFieldProj :: Text -> Text -> Text
msgFieldProj msgVar fieldAcc = msgVar <> "." <> fieldAcc

msgFieldUpdate :: Text -> Text -> Text -> Text
msgFieldUpdate msgVar fieldAcc rhs =
  "(" <> msgVar <> " { " <> fieldAcc <> " = " <> rhs <> "})"

substDecodeMsg :: Text -> Text -> Text
substDecodeMsg msgVar = T.replace decodeAccPlaceholder msgVar

{- | Emit the 'MessageDecode' instance for a message.

The decoder has a two-tier shape:

1. A chain of /per-field stage functions/ (@stage_0@, @stage_1@, …)
   that implement the hyperpb-style in-order tag predictor — each
   stage tries the precomputed varint-encoded tag for the next
   field /before/ falling back to the generic loop.

2. The generic @loop@, exactly as before — reads a tag, dispatches
   to the right field handler, handles repeated\/map\/oneof fields,
   collects unknown fields. This is the correctness backbone; the
   stages only ever short-circuit /well-formed/ in-order input.

A stage function is only emitted for a contiguous prefix of fields
that are eligible for the fast path (singular scalars, singular
messages, singular enums, repeated scalars/messages/enums with the
appropriate @inOrderStage@ shape, and oneofs via a chained predictor
per variant). Ineligible fields (currently: @map@ entries) end the
prefix; decoding continues in the generic @loop@, which handles every
wire shape.
-}
genDecodeInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genDecodeInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fields = extractAllFields ctx scope (msgElements msg)
      unknownFieldName = unknownFieldAccessor scope
      msgVar = decodeThreadMsg
      gas = collectGrowAccums fields
      fpStages = fastPathStages ctx fields
      -- Entry call: "stage_0 defaultTy emptyGrowList emptyGrowList …"
      -- or "loop defaultTy emptyGrowList …" when there are no fast-path stages.
      entryFn = case fpStages of [] -> "loop"; _ -> "stage_0"
      initAccumArgs = if null gas then "" else " " <> T.unwords (replicate (length gas) "emptyGrowList")
      entryCall = txt entryFn <+> txt "default" <> pretty tyN <> txt initAccumArgs
      -- end-of-input expression shared by all stages and the loop
      buildResultDoc = txt (buildResultExpr msgVar unknownFieldName gas)
      -- loop signature: "loop msg gl_0 gl_1 …"
      loopSig = txt "loop " <> pretty msgVar <> txt (growAccumArgs gas)
      genericLoop =
        vsep
          [ loopSig <+> txt "= withTagM"
          , indent 2 $
              vsep
                [ buildResultDoc
                , txt "(\\fn wt -> case fn of"
                , indent 2 $
                    vsep (concatMap (genFieldDecodeCase ctx gas msgVar) fields <> [genDefaultDecodeCase scope gas msgVar])
                    <> txt ")"
                ]
          ]
      stageDocs = fmap (genFastPathStage ctx tyN fields msgVar gas buildResultDoc fpStages) fpStages
      -- INLINE on small messages: GHC inlines the full decoder at the
      -- call site so it can specialize, eliminate intermediate branches,
      -- and keep values in registers. Benchmarks confirm this is
      -- measurably faster than INLINABLE or no pragma for the common
      -- sub-8-field message. Large messages cap here to avoid compile
      -- time explosion (Descriptor.hs has 30+ fields).
      decoderPragma =
        if length fields <= fastPathMaxStages
          then [txt "{-# INLINE messageDecoder #-}"]
          else []
  in vsep
      [ instanceHead "MessageDecode" tyN
      , indent 2 $
          vsep $
            decoderPragma
              <> [ txt "messageDecoder = " <> entryCall
                 , indent 2 $ txt "where"
                 ]
              <> fmap (indent 4) stageDocs
              <> [indent 4 genericLoop]
      ]


-- ---------------------------------------------------------------------------
-- In-order tag predictor (hyperpb-style fast path)
-- ---------------------------------------------------------------------------

data FastPathStageKind
  = SingularField
  | RepeatedUnpacked
  | RepeatedPacked
  | OneofStage
  deriving stock (Show, Eq)


data OneofBranch = OneofBranch
  { obExpected :: !Word64
  , obMask :: !Word64
  , obTagLen :: !Int
  , obDecoder :: !Text
  , obWrap :: !Text
  }


data FastPathStage = FastPathStage
  { fpsField :: !FieldInfoFull
  , fpsKind :: !FastPathStageKind
  , fpsBranches :: ![OneofBranch]
  , fpsExpected :: !Word64
  , fpsMask :: !Word64
  , fpsTagLen :: !Int
  , fpsValueDecoder :: !Text
  , fpsWrap :: !Text
  }


fastPathMaxStages :: Int
fastPathMaxStages = 8


fastPathStages :: GenCtx -> [FieldInfoFull] -> [FastPathStage]
fastPathStages ctx = take fastPathMaxStages . go
  where
    go [] = []
    go (fi : rest) = case eligibleStage ctx fi of
      Nothing -> []
      Just s -> s : go rest


eligibleStage :: GenCtx -> FieldInfoFull -> Maybe FastPathStage
eligibleStage ctx fi = case fifKind fi of
  FKScalar (Just Repeated) ft ->
    let snocFp = if isUnboxableScalar ft then "VU.snoc" else "V.snoc"
        isPackable = case ft of
          SString -> False
          SBytes -> False
          _ -> True
    in if isPackable
        then
          Just $
            mkStage
              RepeatedPacked
              (fifFieldNum fi)
              WireLengthDelimited
              (scalarPackedDecoderExpr ft)
              "v"
        else
          Just $
            mkStage
              RepeatedUnpacked
              (fifFieldNum fi)
              (scalarWireTypeFor ft)
              (scalarDecoderExpr ft)
              ("(" <> snocFp <> " " <> decodeAccPlaceholder <> "." <> fifAccessor fi <> " v)")
  FKScalar lbl ft ->
    let wt = scalarWireTypeFor ft
        wrap = case lbl of
          Just Optional -> "(Just v)"
          _ -> "v"
    in Just (mkStage SingularField (fifFieldNum fi) wt (scalarDecoderExpr ft) wrap)
  FKNamed (Just Repeated) _ TKMessage ->
    Just $
      mkStage
        RepeatedUnpacked
        (fifFieldNum fi)
        WireLengthDelimited
        "decodeFieldMessage"
        ("(V.snoc " <> decodeAccPlaceholder <> "." <> fifAccessor fi <> " v)")
  FKNamed (Just Repeated) _ TKEnum ->
    Just $
      mkStage
        RepeatedUnpacked
        (fifFieldNum fi)
        WireVarint
        "decodeFieldEnum"
        ("(V.snoc " <> decodeAccPlaceholder <> "." <> fifAccessor fi <> " v)")
  FKNamed lbl _ tk ->
    let wt = case tk of TKMessage -> WireLengthDelimited; TKEnum -> WireVarint
        decoder = case tk of TKMessage -> "decodeFieldMessage"; TKEnum -> "decodeFieldEnum"
        wrap = case (lbl, tk) of
          (Just Optional, _) -> "(Just v)"
          (_, TKMessage) -> "(Just v)"
          _ -> "v"
    in Just (mkStage SingularField (fifFieldNum fi) wt decoder wrap)
  FKMap _ _ -> Nothing
  FKOneof scope ood ->
    let branches = fmap (oneofBranch scope (oneofName ood)) (oneofFields ood)
    in if null branches
        then Nothing
        else
          Just $
            FastPathStage
              { fpsField = fi
              , fpsKind = OneofStage
              , fpsBranches = branches
              , fpsExpected = 0
              , fpsMask = 0
              , fpsTagLen = 0
              , fpsValueDecoder = ""
              , fpsWrap = ""
              }
  where
    oneofBranch oscope ooName oof =
      let conName = oneofConName oscope ooName (oneofFieldName oof)
          (wt, decoder) = case oneofFieldType oof of
            FTScalar st -> (scalarWireTypeFor st, scalarDecoderExpr st)
            FTNamed n -> case resolveType ctx n of
              Just ti | tiKind ti == TKEnum -> (WireVarint, "decodeFieldEnum")
              _ -> (WireLengthDelimited, "decodeFieldMessage")
          (expected, mask, tagLen) = tagVarintToWord64 (unFieldNumber (oneofFieldNumber oof)) wt
      in OneofBranch
          { obExpected = expected
          , obMask = mask
          , obTagLen = tagLen
          , obDecoder = decoder
          , obWrap = "(Just (" <> conName <> " v))"
          }

    mkStage kind fieldNum wt decoder wrap =
      let (expected, mask, tagLen) = tagVarintToWord64 fieldNum wt
      in FastPathStage
          { fpsField = fi
          , fpsKind = kind
          , fpsBranches = []
          , fpsExpected = expected
          , fpsMask = mask
          , fpsTagLen = tagLen
          , fpsValueDecoder = decoder
          , fpsWrap = wrap
          }


scalarWireTypeFor :: ScalarType -> WireType
scalarWireTypeFor = \case
  SDouble -> Wire64Bit
  SFixed64 -> Wire64Bit
  SSFixed64 -> Wire64Bit
  SFloat -> Wire32Bit
  SFixed32 -> Wire32Bit
  SSFixed32 -> Wire32Bit
  SString -> WireLengthDelimited
  SBytes -> WireLengthDelimited
  _ -> WireVarint


wireTypeNum :: WireType -> Int
wireTypeNum = \case
  WireVarint -> 0
  Wire64Bit -> 1
  WireLengthDelimited -> 2
  WireStartGroup -> 3
  WireEndGroup -> 4
  Wire32Bit -> 5


tagVarintToWord64 :: Int -> WireType -> (Word64, Word64, Int)
tagVarintToWord64 fieldNum wt =
  let tagVal = fromIntegral fieldNum * 8 + fromIntegral (wireTypeNum wt) :: Word64
      bytes = varintEncode tagVal
      tagLen = length bytes
      expected = foldl' (\acc (i, b) -> acc .|. (fromIntegral b `shiftL` (8 * i))) 0 (zip [0 ..] bytes)
      mask = if tagLen >= 8 then maxBound else (1 `shiftL` (8 * tagLen)) - 1
  in (expected, mask, tagLen)


varintEncode :: Word64 -> [Word8]
varintEncode n
  | n < 0x80 = [fromIntegral n]
  | otherwise = fromIntegral (n .&. 0x7F .|. 0x80) : varintEncode (n `shiftR` 7)


genFastPathStage
  :: GenCtx
  -> Text
  -> [FieldInfoFull]
  -> Text
  -> [GrowAccum]
  -> Doc ann
  -> [FastPathStage]
  -> FastPathStage
  -> Doc ann
genFastPathStage _ctx _tyN _fields msgVar gas buildResultDoc allStages stage =
  let fi = fpsField stage
      idx = fifIndex fi
      fldAcc = fifAccessor fi
      stageName = "stage_" <> T.pack (show idx)
      lastIdx = fifIndex (fpsField (last allStages))
      isLastStage = idx == lastIdx
      -- Loop / stage calls carry all GrowAccum args after the message var.
      accArgs = growAccumArgs gas
      loopFallback = txt "(loop " <> pretty msgVar <> txt (accArgs <> ")")
      -- Stage header: "stage_N msg gl_0 gl_1 … ="
      stageHeader = pretty stageName <+> pretty msgVar <> txt (accArgs <> " =")
      -- Look up the GrowAccum for a given field number (if any).
      gaFor :: Int -> Maybe GrowAccum
      gaFor fn = foldr (\ga _ -> Just ga) Nothing (filter (\ga -> gaFieldNum ga == fn) gas)
      bumpMsg :: Text -> Text
      bumpMsg wrapText =
        msgFieldUpdate msgVar fldAcc (substDecodeMsg msgVar wrapText)
      stageBody = case fpsKind stage of
        OneofStage -> emitOneofChain idx (fpsBranches stage) isLastStage
        _ -> emitSingleStage stage isLastStage idx
      emitSingleStage st isLast i =
        -- For a RepeatedUnpacked field that has a GrowAccum, the kHit
        -- updates the accumulator parameter (snocGrowList) instead of
        -- doing a V.snoc record update.
        let mGa = gaFor (fifFieldNum (fpsField st))
            -- Build the advance/same-stage call with all GrowAccum args.
            callWithAccs pfx = txt pfx <> pretty msgVar <> txt accArgs
            advanceCallFor msgArg =
              if isLast
                then txt "loop " <> txt msgArg <> txt accArgs
                else txt "stage_" <> pretty (T.pack (show (i + 1))) <+> txt msgArg <> txt accArgs
            -- For singular/packed: bump msg record, advance to next stage.
            msgBumped = msgFieldUpdate msgVar fldAcc (substDecodeMsg msgVar (fpsWrap st))
            -- For GrowList repeated: snoc into the accumulator, recurse same stage.
            kHit = case (fpsKind st, mGa) of
              (RepeatedUnpacked, Just ga) ->
                -- Replace the accumulator param for this field; leave msg unchanged.
                let newAcc = "(snocGrowList " <> gaName ga <> " v)"
                    accArgs' = " " <> T.unwords (fmap (\g -> if gaName g == gaName ga then newAcc else gaName g) gas)
                in txt "(\\v -> " <> pretty stageName <+> pretty msgVar <> txt accArgs' <> txt ")"
              (RepeatedUnpacked, Nothing) ->
                -- No GrowAccum (packed scalar fallback) — V.snoc as before.
                let msgBumped' = bumpMsg (fpsWrap st)
                in txt "(\\v -> " <> pretty stageName <+> txt msgBumped' <> txt accArgs <> txt ")"
              (SingularField, _) ->
                txt "(\\v -> " <> advanceCallFor msgBumped <> txt ")"
              (RepeatedPacked, _) ->
                txt "(\\v -> " <> advanceCallFor msgBumped <> txt ")"
              (OneofStage, _) -> txt "(error \"OneofStage uses emitOneofChain\")"
        in if fpsTagLen st == 1
            -- 1-byte tag (field numbers 1–15): skip the Word64 load+mask,
            -- use a direct byte comparison via the specialised variant.
            then vsep
              [ txt "inOrderStage1"
              , indent 2 $
                  vsep
                    [ pretty ("(0x" <> T.pack (showHex (fpsExpected st) "") <> " :: Data.Word.Word8)")
                    , pretty (fpsValueDecoder st)
                    , kHit
                    , buildResultDoc
                    , loopFallback
                    ]
              ]
            else vsep
              [ txt "inOrderStage"
              , indent 2 $
                  vsep
                    [ pretty (word64Lit (fpsExpected st))
                    , pretty (word64Lit (fpsMask st))
                    , pretty (T.pack (show (fpsTagLen st)))
                    , pretty (fpsValueDecoder st)
                    , kHit
                    , buildResultDoc
                    , loopFallback
                    ]
              ]
      emitOneofChain i branches isLast =
        let advanceWith wrap =
              let msgBumped = msgFieldUpdate msgVar fldAcc (substDecodeMsg msgVar wrap)
              in if isLast
                  then txt "loop " <> pretty msgBumped <> txt accArgs
                  else txt "stage_" <> pretty (T.pack (show (i + 1))) <+> pretty msgBumped <> txt accArgs
            go [] = loopFallback
            go (b : bs) =
              if obTagLen b == 1
                then vsep
                  [ txt "inOrderStage1"
                  , indent 2 $
                      vsep
                        [ pretty ("(0x" <> T.pack (showHex (obExpected b) "") <> " :: Data.Word.Word8)")
                        , pretty (obDecoder b)
                        , txt "(\\v -> " <> advanceWith (obWrap b) <> txt ")"
                        , buildResultDoc
                        , parens (go bs)
                        ]
                  ]
                else vsep
                  [ txt "inOrderStage"
                  , indent 2 $
                      vsep
                        [ pretty (word64Lit (obExpected b))
                        , pretty (word64Lit (obMask b))
                        , pretty (T.pack (show (obTagLen b)))
                        , pretty (obDecoder b)
                        , txt "(\\v -> " <> advanceWith (obWrap b) <> txt ")"
                        , buildResultDoc
                        , parens (go bs)
                        ]
                  ]
        in go branches
  in vsep [stageHeader, indent 2 stageBody]


word64Lit :: Word64 -> Text
word64Lit n = "0x" <> T.pack (showHex n "")




genFieldDecodeCase :: GenCtx -> [GrowAccum] -> Text -> FieldInfoFull -> [Doc ann]
genFieldDecodeCase ctx gas msgVar fi =
  let gaFor fn = foldr (\ga _ -> Just ga) Nothing (filter (\ga -> gaFieldNum ga == fn) gas)
      loopCall msgExpr = "loop " <> msgExpr <> growAccumArgs gas
  in case fifKind fi of
    FKScalar lbl ft -> [genScalarDecodeCase gas msgVar fi lbl ft loopCall]
    FKNamed lbl name tk -> [genNamedDecodeCase gas ctx msgVar fi lbl name tk loopCall gaFor]
    FKMap keyT valT -> [genMapDecodeCase gas ctx msgVar fi keyT valT loopCall]
    FKOneof scope ood -> concatMap (genOneofDecodeCase gas ctx scope (oneofName ood) msgVar fi loopCall) (oneofFields ood)


genScalarDecodeCase :: [GrowAccum] -> Text -> FieldInfoFull -> Maybe FieldLabel -> ScalarType -> (Text -> Text) -> Doc ann
genScalarDecodeCase gas msgVar fi lbl st loopCall =
  let fn = T.pack (show (fifFieldNum fi))
      fld = fifAccessor fi
      mGa = foldr (\ga _ -> Just ga) Nothing (filter (\ga -> gaFieldNum ga == fifFieldNum fi) gas)
  in case lbl of
      Just Repeated ->
        -- proto3 says a 'repeated <packable scalar>' field can appear
        -- on the wire either as a sequence of unpacked entries (one
        -- tag per value, wire-type 0/1/5 depending on the scalar)
        -- or as a single packed length-delimited blob (wire-type
        -- 2). Readers must accept both. Branching on 'wt' covers
        -- both shapes -- but only for packable scalars. Strings
        -- and bytes are NEVER packable (each element is
        -- self-delimiting on the wire), so their tag already
        -- carries wire-type 2 for every element; treating wire-type
        -- 2 as "packed mode" there would mis-decode every repeated
        -- string field (and previously did, until 'isPackable'
        -- below started gating the dispatch).
        let snocFn = if isUnboxableScalar st then "VU.snoc" else "V.snoc"
            appendMany = loopCall (msgFieldUpdate msgVar fld ("(" <> msgFieldProj msgVar fld <> " <> vs)"))
            packedDecoderE = scalarPackedDecoderExpr st
            unpackedDecoderE = scalarDecoderExpr st
            isPackable = case st of
              SString -> False
              SBytes -> False
              _ -> True
            -- For a GrowAccum field: snoc into the accumulator parameter.
            -- For plain repeated scalars: V.snoc/VU.snoc into the record.
            loopWithSnoc = case mGa of
              Just ga ->
                let newAcc = "(snocGrowList " <> gaName ga <> " v)"
                    accArgs' = T.unwords (fmap (\g -> if gaName g == gaName ga then newAcc else gaName g) gas)
                in "loop " <> msgVar <> " " <> accArgs'
              Nothing -> loopCall (msgFieldUpdate msgVar fld ("(" <> snocFn <> " " <> msgFieldProj msgVar fld <> " v)"))
        in if isPackable
            then
              pretty fn
                <+> txt "-> case wt of"
                <> line
                <> indent
                  2
                  ( vsep
                      -- 'wt' is an 'Int' under 'withTagM'; literal 2 is
                      -- 'WireLengthDelimited' per the proto wire-format
                      -- spec (the wire-type encoding @len = 2@).
                      [ txt "2 -> do"
                      , indent
                          2
                          ( vsep
                              [ txt "vs <- " <> pretty packedDecoderE
                              , txt (appendMany)
                              ]
                          )
                      , txt "_ -> do"
                      , indent
                          2
                          ( vsep
                              [ txt "v <- " <> pretty unpackedDecoderE
                              , txt loopWithSnoc
                              ]
                          )
                      ]
                  )
            else
              pretty fn
                <+> txt "-> do"
                <> line
                <> indent
                  2
                  ( vsep
                      [ txt "v <- " <> pretty unpackedDecoderE
                      , txt loopWithSnoc
                      ]
                  )
      Just Optional ->
        -- proto3 'optional <scalar>' fields synthesise a oneof-style
        -- presence bit; the field's record slot is 'Maybe T', so the
        -- decoded value has to be wrapped with 'Just' here.
        let bump = msgFieldUpdate msgVar fld "(Just v)"
        in pretty fn
            <+> txt "-> do"
            <> line
            <> indent
              2
              ( vsep
                  [ txt "v <- " <> pretty (scalarDecoderExpr st)
                  , txt (loopCall bump)
                  ]
              )
      _ ->
        let bump = msgFieldUpdate msgVar fld "v"
        in pretty fn
            <+> txt "-> do"
            <> line
            <> indent
              2
              ( vsep
                  [ txt "v <- " <> pretty (scalarDecoderExpr st)
                  , txt (loopCall bump)
                  ]
              )


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


genNamedDecodeCase :: [GrowAccum] -> GenCtx -> Text -> FieldInfoFull -> Maybe FieldLabel -> Text -> TypeKind -> (Text -> Text) -> (Int -> Maybe GrowAccum) -> Doc ann
genNamedDecodeCase gas ctx msgVar fi lbl name tk loopCall gaFor =
  let fn = T.pack (show (fifFieldNum fi))
      fld = fifAccessor fi
      mGa = gaFor (fifFieldNum fi)
      -- For closed enums, resolve the type name to its Haskell name and
      -- generate a fromProtoEnum-based decode that fails on unknown values.
      closedEnum = tk == TKEnum && genClosedEnums (gcOpts ctx)
      hsEnumName = case resolveType ctx name of
        Just ti -> tiHsName ti
        Nothing -> ""
      loopExpr = case (lbl, mGa) of
        (Just Repeated, Just ga) ->
          let newAcc = "(snocGrowList " <> gaName ga <> " v)"
              accArgs' = T.unwords (fmap (\g -> if gaName g == gaName ga then newAcc else gaName g) gas)
          in "loop " <> msgVar <> " " <> accArgs'
        (Just Repeated, Nothing) ->
          loopCall (msgFieldUpdate msgVar fld ("(V.snoc " <> msgFieldProj msgVar fld <> " v)"))
        (Just Optional, _) ->
          loopCall (msgFieldUpdate msgVar fld "(Just v)")
        -- proto2 required fields: non-Maybe record slot — assign v directly.
        -- Enum fields are also non-Maybe (same treatment as the Nothing case).
        (Just Required, _) ->
          loopCall (msgFieldUpdate msgVar fld "v")
        -- proto3 singular (lbl == Nothing): message slots are Maybe, scalars are plain.
        _ -> case tk of
          TKEnum    -> loopCall (msgFieldUpdate msgVar fld "v")
          TKMessage -> loopCall (msgFieldUpdate msgVar fld "(Just v)")
      decoderExpr :: Text
      decoderExpr = case tk of
        TKEnum -> "decodeFieldEnum"
        TKMessage -> "decodeFieldMessage"
  in if closedEnum && not (T.null hsEnumName)
      -- Closed enum: read varint → fromProtoEnum → fail on unknown.
      then
        pretty fn
          <+> txt "-> do"
          <> line
          <> indent
            2
            ( vsep
                [ txt "n <- fromIntegral <$> getVarint"
                , txt ("case fromProtoEnum" <> hsEnumName <> " n of")
                , indent 2 $
                    vsep
                      [ txt ("Nothing -> decodeFail (CustomError (\"Unknown closed enum value: \" <> show n))")
                      , txt "Just v ->"
                      , indent 2 (txt loopExpr)
                      ]
                ]
            )
      else
        pretty fn
          <+> txt "-> do"
          <> line
          <> indent
            2
            ( vsep
                [ txt "v <- " <> pretty decoderExpr
                , txt loopExpr
                ]
            )


genMapDecodeCase :: [GrowAccum] -> GenCtx -> Text -> FieldInfoFull -> ScalarType -> FieldType -> (Text -> Text) -> Doc ann
genMapDecodeCase _gas ctx msgVar fi keyT valT loopCall =
  let fn = T.pack (show (fifFieldNum fi))
      fld = fifAccessor fi
      bump =
        msgFieldUpdate
          msgVar
          fld
          ("(Map.union " <> msgFieldProj msgVar fld <> " (Map.singleton mk' mv'))")
      keyDecoder = scalarDecoderExpr keyT
      valDecoder = mapValDecoderExpr ctx valT
      keyDefault = scalarDefaultLit keyT
      valDefault = mapValDefaultLit ctx valT
  in pretty fn
      <+> txt "-> do"
      <> line
      <> indent
        2
        ( vsep
            [ txt "bs' <- getLengthDelimited"
            , txt "let decodeEntry = runDecoder (decodeMapEntry"
                <+> pretty keyDecoder
                <+> pretty valDecoder
                <+> pretty keyDefault
                <+> pretty valDefault
                <> txt ") bs'"
            , txt "case decodeEntry of"
            , indent 2 $
                vsep
                  [ txt ("Left _ -> " <> loopCall msgVar)
                  , txt ("Right (mk', mv') -> " <> loopCall bump)
                  ]
            ]
        )


genOneofDecodeCase :: [GrowAccum] -> GenCtx -> [Text] -> Text -> Text -> FieldInfoFull -> (Text -> Text) -> OneofField -> [Doc ann]
genOneofDecodeCase _gas ctx scope ooName msgVar fi loopCall oof =
  let fn = T.pack (show (unFieldNumber (oneofFieldNumber oof)))
      fld = fifAccessor fi
      conName = oneofConName scope ooName (oneofFieldName oof)
      bump = msgFieldUpdate msgVar fld ("(Just (" <> conName <> " v))")
      decoderExpr = case oneofFieldType oof of
        FTScalar st -> scalarDecoderExpr st
        FTNamed n -> case resolveType ctx n of
          Just ti | tiKind ti == TKEnum -> "decodeFieldEnum"
          _ -> "decodeFieldMessage"
  in [ pretty fn
        <+> txt "-> do"
        <> line
        <> indent
          2
          ( vsep
              [ txt "v <- " <> pretty decoderExpr
              , txt (loopCall bump)
              ]
          )
     ]


genDefaultDecodeCase :: [Text] -> [GrowAccum] -> Text -> Doc ann
genDefaultDecodeCase scope gas msgVar =
  let ufa = unknownFieldAccessor scope
      bump = msgFieldUpdate msgVar ufa ("(uf : " <> msgFieldProj msgVar ufa <> ")")
      loopExpr = "loop " <> bump <> growAccumArgs gas
  in txt "_ -> do"
      <> line
      <> indent
        2
        ( vsep
            -- 'wt' comes through as an 'Int' (withTagM's continuation
            -- takes raw Ints), so 'toEnum' it back into a 'WireType'
            -- before handing it to 'captureUnknownField'.
            [ txt "uf <- captureUnknownField fn (Prelude.toEnum wt)"
            , txt loopExpr
            ]
        )


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


mapValDecoderExpr :: GenCtx -> FieldType -> Text
mapValDecoderExpr ctx = \case
  FTScalar st -> scalarDecoderExpr st
  FTNamed n -> case resolveType ctx n of
    Just ti | tiKind ti == TKEnum -> "decodeFieldEnum"
    _ -> "decodeFieldMessage"


scalarDefaultLit :: ScalarType -> Text
scalarDefaultLit = \case
  SBool -> "False"
  SString -> "\"\""
  SBytes -> "\"\""
  _ -> "0"


mapValDefaultLit :: GenCtx -> FieldType -> Text
mapValDefaultLit ctx = \case
  FTScalar st -> scalarDefaultLit st
  FTNamed n -> case resolveType ctx n of
    Just ti | tiKind ti == TKEnum -> "(Prelude.toEnum 0)"
    Just ti -> "default" <> tiHsName ti
    Nothing -> "error \"mapValDefaultLit: unresolved type " <> n <> "\""


-- ---------------------------------------------------------------------------
-- JSON instances
-- ---------------------------------------------------------------------------

{- | Generate an empty @instance IsMessage Foo@. 'IsMessage' is a marker
class with no methods of its own; its superclasses ('MessageEncode',
'MessageDecode', 'ProtoMessage', 'Aeson.ToJSON', 'Aeson.FromJSON',
'Typeable') do the work. Emitting an instance signals to the registry
TH discovery splice that this type is eligible for inclusion.
-}
genIsMessageInstance :: [Text] -> Doc ann
genIsMessageInstance scope =
  let tyN = scopedTypeName scope
  in txt "instance IsMessage " <> pretty tyN


{- | Emit the @ProtoMessage@ instance for a message.

The field-descriptor map is bound as a top-level CAF
(@\<tyN\>FieldDescriptors@) and the instance method just returns it.

Why not inline @Map.fromList [...]@ in the instance body? GHC's
full-laziness pass at @-O1@ does already float the @Map.fromList@
out of the @\\_ -> ...@ lambda (verified in Core: the lambda becomes
@\\_ -> $fProtoMessage<X>1@ where @$fProtoMessage<X>1@ is a
top-level CAF), so the map is built once and shared. But that's a
contract of the optimiser, not the source. Making the CAF explicit:

* guarantees the optimisation independently of GHC version,
  optimisation level, or future heuristic changes;
* makes the intent obvious at the source level — no reader has to
  reason about full-laziness to know the map isn't rebuilt per
  call;
* gives downstream code a name to reference if it wants the map
  without going through the typeclass dictionary.
-}
genProtoMessageInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genProtoMessageInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fqn = fqProtoName (gcPkg ctx) scope
      pkg = fromMaybe "" (gcPkg ctx)
      fields = extractMessageFieldsForSchema scope (msgElements msg)
      defN = "default" <> tyN
      descN = fieldDescriptorsBindingName scope
      descTy = txt "IntMap.IntMap (SomeFieldDescriptor " <> pretty tyN <> txt ")"
      -- 'IntMap.fromList []' would also be fine, but emitting 'IntMap.empty'
      -- for the empty case keeps the generated source tidier and avoids
      -- the (admittedly tiny) cost of the runtime list traversal.
      descBody = case fields of
        [] -> txt "IntMap.empty"
        _ ->
          vsep
            [ txt "IntMap.fromList"
            , indent 2 $ genFieldDescriptorList ctx scope fields
            ]
  in vsep
      [ txt "-- | Field descriptors for '"
          <> pretty tyN
          <> txt "', keyed by proto field number."
      , txt "--"
      , txt "-- Top-level CAF: allocated lazily on first force, shared on every"
      , txt "-- subsequent 'protoFieldDescriptors' call (no per-call rebuild)."
      , pretty descN <+> txt "::" <+> descTy
      , pretty descN <+> txt "="
      , indent 2 descBody
      , mempty
      , instanceHead "ProtoMessage" tyN
      , indent 2 $ pretty ("protoMessageName _ = \"" :: Text) <> pretty fqn <> pretty ("\"" :: Text)
      , indent 2 $ pretty ("protoPackageName _ = \"" :: Text) <> pretty pkg <> pretty ("\"" :: Text)
      , indent 2 $ txt "protoDefaultValue = " <> pretty defN
      , indent 2 $ txt "protoFileDescriptorBytes _ = fileDescriptorProtoBytes"
      , indent 2 $ txt "protoFieldDescriptors _ = " <> pretty descN
      ]


{- | Name of the top-level CAF holding a message's field descriptors.

Format: @\<lowerFirst tyN\>FieldDescriptors@. The 'FieldDescriptors'
suffix is verbose on purpose — it makes accidental collision with a
user-declared field accessor near-impossible (record field 'fooBar'
would never share a name with binding 'fooBarFieldDescriptors').
-}
fieldDescriptorsBindingName :: [Text] -> Text
fieldDescriptorsBindingName scope =
  lowerFirst (scopedTypeName scope) <> "FieldDescriptors"


data SchemaField = SchemaField
  { sfName :: Text
  , sfNum :: Int
  , sfType :: FieldType
  , sfLabel :: Maybe FieldLabel
  }


extractMessageFieldsForSchema :: [Text] -> [MessageElement] -> [SchemaField]
extractMessageFieldsForSchema _scope = concatMap go
  where
    go (MEField fd) = [SchemaField (fieldName fd) (unFieldNumber (fieldNumber fd)) (fieldType fd) (fieldLabel fd)]
    go (MEMapField mf) = [SchemaField (mapFieldName mf) (unFieldNumber (mapFieldNum mf)) (FTScalar SBytes) (Just Repeated)]
    go (MEOneof od) = case oneofFields od of
      (f : _) -> [SchemaField (oneofName od) (unFieldNumber (oneofFieldNumber f)) (FTNamed (oneofName od)) (Just Optional)]
      [] -> []
    go _ = []


genFieldDescriptorList :: GenCtx -> [Text] -> [SchemaField] -> Doc ann
genFieldDescriptorList ctx scope fields =
  let entries = fmap (genFieldDescriptorEntry ctx scope) fields
  in case entries of
      [] -> txt "[]"
      _ ->
        vsep [txt "[ " <> head entries]
          <> vsep (fmap (\e -> txt ", " <> e) (tail entries))
          <> line
          <> txt "]"


genFieldDescriptorEntry :: GenCtx -> [Text] -> SchemaField -> Doc ann
genFieldDescriptorEntry ctx scope sf =
  let accN = ctxFieldName ctx scope (sfName sf)
  in txt "("
      <> pretty (T.pack (show (sfNum sf)))
      <> txt ", SomeField FieldDescriptor"
      <> line
      <> indent
        4
        ( vsep
            [ pretty ("{ fdName = \"" :: Text) <> pretty (sfName sf) <> pretty ("\"" :: Text)
            , txt ", fdNumber = " <> pretty (T.pack (show (sfNum sf)))
            , txt ", fdTypeDesc = " <> genFieldTypeDesc (sfType sf)
            , txt ", fdLabel = " <> genLabelLit (sfLabel sf)
            , txt ", fdGet = " <> pretty accN
            , txt ", fdSet = \\v m -> m { " <> pretty accN <> txt " = v }"
            , txt "})"
            ]
        )


genFieldTypeDesc :: FieldType -> Doc ann
genFieldTypeDesc = \case
  FTScalar SDouble -> txt "ScalarType DoubleField"
  FTScalar SFloat -> txt "ScalarType FloatField"
  FTScalar SInt32 -> txt "ScalarType Int32Field"
  FTScalar SInt64 -> txt "ScalarType Int64Field"
  FTScalar SUInt32 -> txt "ScalarType UInt32Field"
  FTScalar SUInt64 -> txt "ScalarType UInt64Field"
  FTScalar SSInt32 -> txt "ScalarType SInt32Field"
  FTScalar SSInt64 -> txt "ScalarType SInt64Field"
  FTScalar SFixed32 -> txt "ScalarType Fixed32Field"
  FTScalar SFixed64 -> txt "ScalarType Fixed64Field"
  FTScalar SSFixed32 -> txt "ScalarType SFixed32Field"
  FTScalar SSFixed64 -> txt "ScalarType SFixed64Field"
  FTScalar SBool -> txt "ScalarType BoolField"
  FTScalar SString -> txt "ScalarType StringField"
  FTScalar SBytes -> txt "ScalarType BytesField"
  FTNamed n -> pretty ("MessageType \"" :: Text) <> pretty n <> pretty ("\"" :: Text)


genLabelLit :: Maybe FieldLabel -> Doc ann
genLabelLit Nothing = txt "LabelOptional"
genLabelLit (Just Optional) = txt "LabelOptional"
genLabelLit (Just Required) = txt "LabelRequired"
genLabelLit (Just Repeated) = txt "LabelRepeated"


-- ---------------------------------------------------------------------------
-- JSON instances
-- ---------------------------------------------------------------------------

genToJSONInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genToJSONInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fqn = fqProtoName (gcPkg ctx) scope
      overrides = genJsonOverrides (gcOpts ctx)
  in case Map.lookup fqn overrides of
      Just jo ->
        vsep
          [ instanceHead "Aeson.ToJSON" tyN
          , pretty (joToJSON jo)
          ]
      Nothing ->
        let fields = extractAllFieldsJSON ctx scope (msgElements msg)
            anyOneof = any (\jfi -> case jfiKind jfi of JFKOneof _ -> True; _ -> False) fields
        in vsep
            [ instanceHead "Aeson.ToJSON" tyN
            , indent 2 $
                if anyOneof
                  then
                    -- A oneof contributes /0 or 1/ pairs to the
                    -- output depending on which variant is selected,
                    -- which means we can't fit every field into a
                    -- flat pair-list any more. Use @mconcat@ over
                    -- per-field fragment lists instead.
                    vsep
                      [ txt "toJSON msg = jsonObject $ mconcat"
                      , indent 4 $ case fields of
                          [] -> txt "[]"
                          _ ->
                            vsep
                              [ txt "[ " <> genToJSONFieldFragment ctx (head fields)
                              , vsep (fmap (\f -> txt ", " <> genToJSONFieldFragment ctx f) (tail fields))
                              , txt "]"
                              ]
                      ]
                  else
                    vsep
                      [ txt "toJSON msg = jsonObject"
                      , indent 4 $ case fields of
                          [] -> txt "[]"
                          _ ->
                            vsep
                              [ txt "[ " <> head (fmap (genToJSONField ctx) fields)
                              , vsep (fmap (\f -> txt ", " <> genToJSONField ctx f) (tail fields))
                              , txt "]"
                              ]
                      ]
            ]


-- | Emit the @(Text, Aeson.Value)@ pair expression for one
-- non-oneof JSON field.
genToJSONField :: GenCtx -> JSONFieldInfo -> Doc ann
genToJSONField _ctx jfi = case jfiKind jfi of
  JFKBytes ->
    txt "bytesFieldToJSON " <> pretty ("\"" :: Text) <> pretty (jfiJsonName jfi) <> pretty ("\"" :: Text) <+> txt "msg." <> pretty (jfiAccessor jfi)
  JFKOptionalBytes ->
    -- proto3 'optional bytes' fields are 'Maybe ByteString' on
    -- the record. Lift through 'Maybe' before calling the
    -- base64-encoding helper so each slot stays well-typed.
    pretty ("\"" :: Text) <> pretty (jfiJsonName jfi) <> pretty ("\" .=: (fmap protoBytesToJSON msg." :: Text) <> pretty (jfiAccessor jfi) <> txt ")"
  JFKBytesMap ->
    txt "bytesMapFieldToJSON " <> pretty ("\"" :: Text) <> pretty (jfiJsonName jfi) <> pretty ("\"" :: Text) <+> txt "msg." <> pretty (jfiAccessor jfi)
  JFKNormal ->
    pretty ("\"" :: Text) <> pretty (jfiJsonName jfi) <> pretty ("\" .=: msg." :: Text) <> pretty (jfiAccessor jfi)
  JFKOneof _ ->
    -- 'genToJSONField' is only reached on the "no oneofs in this
    -- message" path; the @mconcat@ form uses 'genToJSONFieldFragment'
    -- instead. If we ever land here for a oneof field, it's a bug
    -- in the caller. Emit a runtime fail so the mistake surfaces.
    txt "(error \"codegen bug: oneof reached genToJSONField\")"


-- | Emit a @[(Text, Aeson.Value)]@ fragment expression for one JSON
-- field. Singular fields produce a singleton list; oneofs produce a
-- @case@ that yields 0 or 1 entries.
genToJSONFieldFragment :: GenCtx -> JSONFieldInfo -> Doc ann
genToJSONFieldFragment ctx jfi = case jfiKind jfi of
  JFKOneof branches ->
    -- Inline each branch under its own JSON key. The carrier is
    -- @Maybe \<OneofSum\>@:
    --   Nothing                  -> []  (no variant selected)
    --   Just (\<ctor\> v)        -> [(\<jsonKey\>, encode v)]
    let nothingArm = txt "Nothing -> []"
        branchArms = fmap (genOneofBranchToJSONArm jfi) branches
    in vsep
        [ txt "(case msg." <> pretty (jfiAccessor jfi) <> txt " of"
        , indent 4 $
            vsep $
              [nothingArm] <> branchArms
        , txt ")"
        ]
  _ ->
    -- Non-oneof: wrap the existing single-pair expression in a
    -- singleton list so it composes under 'mconcat'.
    txt "[" <> genToJSONField ctx jfi <> txt "]"


-- | One arm of the parent's @case msg.foo of { Just (Con v) -> ... }@.
genOneofBranchToJSONArm :: JSONFieldInfo -> JSONOneofBranch -> Doc ann
genOneofBranchToJSONArm _jfi branch =
  let keyLit = pretty ("\"" :: Text) <> pretty (jobJsonKey branch) <> pretty ("\"" :: Text)
  in case jobShape branch of
      OBSNullValue ->
        -- The variant's value /is/ JSON null — emit the bare
        -- @Aeson.Null@ rather than routing through @.=:@ (whose
        -- @Aeson.toJSON@ on the singleton 'NullValue' enum would
        -- render the proto-name string @"NULL_VALUE"@, which the
        -- proto3 JSON spec explicitly rejects for this WKT).
        txt "Just ("
          <> pretty (jobConstructor branch)
          <> txt " _) -> [("
          <> keyLit
          <> txt ", Aeson.Null)]"
      _ ->
        -- Scalars, submessages, and ordinary enums all route
        -- through '.=:' (which is @Aeson.toJSON@ under the hood),
        -- matching 'genToJSONField' for a non-oneof field of the
        -- same Haskell type.
        txt "Just ("
          <> pretty (jobConstructor branch)
          <> txt " v) -> ["
          <> keyLit
          <> txt " .=: v]"


genFromJSONInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genFromJSONInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fqn = fqProtoName (gcPkg ctx) scope
      overrides = genJsonOverrides (gcOpts ctx)
  in case Map.lookup fqn overrides of
      Just jo ->
        vsep
          [ instanceHead "Aeson.FromJSON" tyN
          , pretty (joFromJSON jo)
          ]
      Nothing ->
        let fields = extractAllFieldsJSON ctx scope (msgElements msg)
        in case fields of
            [] ->
              vsep
                [ instanceHead "Aeson.FromJSON" tyN
                , indent 2 $ txt "parseJSON _ = pure default" <> pretty tyN
                ]
            _ ->
              vsep
                [ instanceHead "Aeson.FromJSON" tyN
                , indent 2 $
                    vsep
                      [ txt "parseJSON = Aeson.withObject " <> pretty ("\"" :: Text) <> pretty tyN <> pretty ("\"" :: Text) <> txt " $ \\obj -> do"
                      , indent 2 $
                          vsep
                            ( fmap genFromJSONFieldBind fields
                                <> [ txt "pure default" <> pretty tyN
                                   , indent 2 $
                                      vsep
                                        ( txt "{ " <> genFromJSONFieldAssign tyN (head fields)
                                            : fmap (\jfi -> txt ", " <> genFromJSONFieldAssign tyN jfi) (tail fields)
                                              <> [txt "}"]
                                        )
                                   ]
                            )
                      ]
                ]


genFromJSONFieldBind :: JSONFieldInfo -> Doc ann
genFromJSONFieldBind jfi = case jfiKind jfi of
  JFKBytes ->
    txt "fld_" <> pretty (jfiAccessor jfi) <+> txt "<- parseBytesFieldMaybe obj " <> pretty ("\"" :: Text) <> pretty (jfiJsonName jfi) <> pretty ("\"" :: Text)
  JFKOptionalBytes ->
    -- 'parseBytesFieldMaybe' already returns @Maybe ByteString@,
    -- which is the slot type; no second 'Just' wrap needed.
    txt "fld_" <> pretty (jfiAccessor jfi) <+> txt "<- parseBytesFieldMaybe obj " <> pretty ("\"" :: Text) <> pretty (jfiJsonName jfi) <> pretty ("\"" :: Text)
  JFKBytesMap ->
    txt "fld_" <> pretty (jfiAccessor jfi) <+> txt "<- parseBytesMapFieldMaybe obj " <> pretty ("\"" :: Text) <> pretty (jfiJsonName jfi) <> pretty ("\"" :: Text)
  JFKNormal ->
    txt "fld_" <> pretty (jfiAccessor jfi) <+> txt "<- parseFieldMaybe obj " <> pretty ("\"" :: Text) <> pretty (jfiJsonName jfi) <> pretty ("\"" :: Text)
  JFKOneof branches ->
    -- Hand off to the shared 'parseOneofVariants' helper. The
    -- variants list carries (jsonKey, null-semantics, parser)
    -- triples — one per branch — and the helper enforces
    -- "exactly one matching key" with a parser failure on
    -- duplicates (proto3 'OneofFieldDuplicate' conformance test).
    vsep
      [ txt "fld_" <> pretty (jfiAccessor jfi) <+> txt "<- parseOneofVariants obj"
      , indent 2 $ case branches of
          [] -> txt "[]"
          _ ->
            vsep
              [ txt "[ " <> genOneofBranchParser (head branches)
              , vsep (fmap (\b -> txt ", " <> genOneofBranchParser b) (tail branches))
              , txt "]"
              ]
      ]


-- | One @(jsonKey, OneofVariantNullSemantics, \\v -> Con \<$\> parser v)@
-- tuple for 'parseOneofVariants'.
genOneofBranchParser :: JSONOneofBranch -> Doc ann
genOneofBranchParser branch =
  let keyLit = pretty ("\"" :: Text) <> pretty (jobJsonKey branch) <> pretty ("\"" :: Text)
      nullSem = case jobShape branch of
        OBSNullValue -> txt "OneofVariantNullIsValue"
        _ -> txt "OneofVariantNullIsUnset"
      parserE = case jobShape branch of
        OBSNullValue ->
          -- @null@ is the value: accept it directly (yielding the
          -- singleton enum); fall through to the standard parser
          -- for the @"NULL_VALUE"@ string sentinel (which proto3
          -- also accepts).
          txt "(\\v -> "
            <> pretty (jobConstructor branch)
            <> txt " <$> (case v of Aeson.Null -> pure (Prelude.toEnum 0); _ -> Aeson.parseJSON v))"
        _ ->
          txt "(\\v -> "
            <> pretty (jobConstructor branch)
            <> txt " <$> Aeson.parseJSON v)"
  in txt "(" <> keyLit <> txt ", " <> nullSem <> txt ", " <> parserE <> txt ")"


genFromJSONFieldAssign :: Text -> JSONFieldInfo -> Doc ann
genFromJSONFieldAssign tyN jfi = case jfiKind jfi of
  JFKOptionalBytes ->
    -- The slot is already 'Maybe ByteString'; the parse helper
    -- returned 'Maybe ByteString' too, so just plug it in
    -- directly instead of stripping the wrapper.
    pretty (jfiAccessor jfi) <+> txt "= fld_" <> pretty (jfiAccessor jfi)
  JFKOneof _ ->
    -- The slot is @Maybe \<Oneof\>@ and 'parseOneofVariants' already
    -- returns @Maybe \<Oneof\>@, so plug it in directly — wrapping
    -- with another 'maybe' would type-error on the double-Maybe.
    pretty (jfiAccessor jfi) <+> txt "= fld_" <> pretty (jfiAccessor jfi)
  _ ->
    pretty (jfiAccessor jfi) <+> txt "= maybe (" <> pretty (jfiAccessor jfi) <+> txt "default" <> pretty tyN <> txt ") id fld_" <> pretty (jfiAccessor jfi)


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
genHasExtensionsInstance :: [Text] -> MessageDef -> Doc ann
genHasExtensionsInstance scope _msg =
  let tyN = scopedTypeName scope
      acc = unknownFieldAccessor scope
  in vsep
      [ txt "instance Proto.Extension.HasExtensions " <> pretty tyN <> txt " where"
      , indent 2 $ txt "messageUnknownFields = " <> pretty acc
      , indent 2 $
          txt "setMessageUnknownFields !ufs msg = msg { "
            <> pretty acc
            <> txt " = ufs }"
      ]


{- | Generate a 'Semigroup' instance encoding protobuf merge semantics:
singular scalar fields take the right value only if non-default
(matching MergeFrom in protoc-gen-go/cpp), repeated and map fields
append/union, submessages and oneofs pick the right value when set.
-}
genSemigroupInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genSemigroupInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fields = extractAllFields ctx scope (msgElements msg)
      conN = scopedTypeName scope
      unknownAcc = unknownFieldAccessor scope
      mergeField fi =
        let acc = fifAccessor fi
            ba = txt "b." <> pretty acc
            aa = txt "a." <> pretty acc
            lastNonDefault def =
              txt "if "
                <> ba
                <> txt " == "
                <> txt def
                <> txt " then "
                <> aa
                <> txt " else "
                <> ba
        in pretty acc <> txt " = " <> case fifKind fi of
            FKMap _ _ -> aa <> txt " <> " <> ba
            FKOneof _ _ -> txt "case " <> ba <> txt " of { Nothing -> " <> aa <> txt "; x -> x }"
            FKScalar (Just Repeated) _ -> aa <> txt " <> " <> ba
            FKNamed (Just Repeated) _ _ -> aa <> txt " <> " <> ba
            FKNamed (Just Optional) _ _ -> txt "case " <> ba <> txt " of { Nothing -> " <> aa <> txt "; x -> x }"
            FKNamed Nothing _ TKMessage -> txt "case " <> ba <> txt " of { Nothing -> " <> aa <> txt "; x -> x }"
            FKNamed Nothing _ TKEnum -> lastNonDefault "(Prelude.toEnum 0)"
            FKScalar (Just Optional) _ -> txt "case " <> ba <> txt " of { Nothing -> " <> aa <> txt "; x -> x }"
            FKScalar Nothing ft -> lastNonDefault (scalarDefaultText ft)
            -- proto2 @required@ fields: the merge rule is "last wins"
            -- (same as a singular scalar), but the field carries no
            -- default-skipping check — required by definition.
            FKScalar (Just Required) _ -> ba
            FKNamed (Just Required) _ _ -> ba
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
      FKMap _ _ ->
        txt "Map.foldlWithKey' (\\s k v -> s `hashWithSalt` k `hashWithSalt` v) (" <> acc <> txt ") " <> fld
      _ ->
        txt "hashWithSalt (" <> acc <> txt ") " <> fld


genOneofHashableInstance :: GenCtx -> [Text] -> OneofDef -> Doc ann
genOneofHashableInstance _ctx scope od =
  let tyN = scopedTypeName scope <> "'" <> snakeToPascal (oneofName od)
      arms = zipWith (genOneofHashArm scope (oneofName od)) [0 :: Int ..] (oneofFields od)
  in vsep
      [ instanceHead "Hashable" tyN
      , indent 2 $ vsep arms
      ]


genOneofHashArm :: [Text] -> Text -> Int -> OneofField -> Doc ann
genOneofHashArm scope ooName tag f =
  let conName = oneofConName scope ooName (oneofFieldName f)
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


{- | Is a top-level message / enum / service whose fully-qualified
proto name appears in 'genExternalTypes' — meaning some other
package already provides Haskell definitions for it and the code
generator should /not/ emit a parallel set in this output file?

Service definitions are never skipped here, because the codegen
emits @Network.GRPC@-flavoured service stubs that aren't covered by
the @TypeRegistry@ surface (@TypeRegistry@ maps type FQNs, not
service FQNs). If a downstream consumer needs to suppress a service
definition they can do it via 'genHooks' instead.
-}
topLevelManagedExternally :: GenerateOpts -> Maybe Text -> TopLevel -> Bool
topLevelManagedExternally opts pkg tl =
  let externals = genExternalTypes opts
   in case tl of
        TLMessage msg -> Map.member (fqProtoName pkg [msgName msg]) externals
        TLEnum ed -> Map.member (fqProtoName pkg [enumName ed]) externals
        TLService _ -> False
        _ -> False


-- ---------------------------------------------------------------------------
-- Field extraction (unified)
-- ---------------------------------------------------------------------------

data FieldKindFull
  = FKScalar (Maybe FieldLabel) ScalarType
  | FKNamed (Maybe FieldLabel) Text TypeKind
  | FKMap ScalarType FieldType
  | FKOneof [Text] OneofDef
  deriving stock (Show, Eq)


data FieldInfoFull = FieldInfoFull
  { fifAccessor :: Text
  , fifFieldNum :: Int
  , fifIndex :: Int
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
        in [FieldInfoFull accessor fn 0 kind]
      MEMapField mf ->
        let accessor = ctxFieldName ctx scope (mapFieldName mf)
            fn = unFieldNumber (mapFieldNum mf)
        in [FieldInfoFull accessor fn 0 (FKMap (mapKeyType mf) (mapValueType mf))]
      MEOneof od ->
        let accessor = ctxFieldName ctx scope (oneofName od)
            fn = 0
        in [FieldInfoFull accessor fn 0 (FKOneof scope od)]
      _ -> []


resolveTypeKindScoped :: GenCtx -> [Text] -> Text -> TypeKind
resolveTypeKindScoped ctx scope name = maybe TKMessage tiKind (resolveTypeWithScope ctx scope name)


-- JSON field info
--
-- 'JFKBytes' is split into bare-bytes and optional-bytes shapes
-- so the bytes ToJSON / FromJSON helpers (which require a
-- 'ByteString' / 'Maybe ByteString' argument respectively) line
-- up with the actual record-slot type. Without the split,
-- proto3 'optional bytes' fields generate code that takes a
-- 'Maybe ByteString' where 'bytesFieldToJSON' wants 'ByteString',
-- which doesn't type-check.
--
-- 'JFKOneof' carries the per-branch JSON info so the writer/reader
-- can inline each branch under its /own/ proto-3-canonical JSON
-- key (the proto3 spec inlines oneofs at the parent message level
-- — there is no enclosing key for the group itself). Each
-- branch's key honours its individual @json_name@ option when
-- present, falling back to the camelCase form of the branch field
-- name.
data JSONFieldKind
  = JFKNormal
  | JFKBytes
  | JFKOptionalBytes
  | JFKBytesMap
  | JFKOneof ![JSONOneofBranch]
  deriving stock (Show, Eq)


{- | One branch of a oneof's JSON projection. The codegen turns each
of these into:

* one arm of the parent's @toJSON@ @case@ expression
  (@\\Just (\<conName\> v) -> [(\<jsonKey\>, \<encode v\>)]@), and
* one tuple of the parent's @parseOneofVariants@ call list
  (@(\<jsonKey\>, \<nullSem\>, \\v -> \<conName\> \<$\> parseJSON v)@).
-}
data JSONOneofBranch = JSONOneofBranch
  { jobConstructor :: !Text
  -- ^ Fully-qualified Haskell-side constructor for this branch,
  --   e.g. @ResetOptions'Target'BuildId@.
  , jobJsonKey :: !Text
  -- ^ Proto-3-canonical JSON key. From @json_name@ if the
  --   branch field carries one; otherwise the camelCase form of
  --   the branch field's proto name.
  , jobShape :: !OneofBranchShape
  -- ^ How to encode\/decode the branch payload.
  }
  deriving stock (Show, Eq)


{- | Branch payload shape, kept narrow on purpose. Proto-3-canonical
JSON has type-specific rules (e.g. @int64@ as string, @bytes@ as
base64) that the writer\/reader will eventually want to dispatch
on, but for the first cut we route everything through
'Aeson.toJSON' \/ 'Aeson.parseJSON' the same way the existing
pure-text codegen does for regular submessage and scalar fields.
The constructors are kept distinct so a future refactor can
discriminate without touching every call site.
-}
data OneofBranchShape
  = OBSScalar
  | OBSMessage
  | OBSEnum
  | -- | @google.protobuf.NullValue@: bare JSON @null@ is the
    --   variant's value, not the "variant unset" marker.
    OBSNullValue
  deriving stock (Show, Eq)


data JSONFieldInfo = JSONFieldInfo
  { jfiAccessor :: Text
  , jfiJsonName :: Text
  -- ^ Empty for 'JFKOneof' fields — the per-branch key lives in
  --   'jobJsonKey' inside the kind payload, since proto3 inlines
  --   the branch keys at the parent level rather than wrapping
  --   them under the oneof group's name.
  , jfiOptional :: Bool
  , jfiKind :: JSONFieldKind
  }
  deriving stock (Show, Eq)


extractAllFieldsJSON :: GenCtx -> [Text] -> [MessageElement] -> [JSONFieldInfo]
extractAllFieldsJSON ctx scope = concatMap go
  where
    go = \case
      MEField fd ->
        let accessor = ctxFieldName ctx scope (fieldName fd)
            jsonName = fromMaybe (protoJsonName (fieldName fd)) (getJsonName (fieldOptions fd))
            kind = case fieldType fd of
              FTScalar SBytes -> case fieldLabel fd of
                Just Optional -> JFKOptionalBytes
                _ -> JFKBytes
              _ -> JFKNormal
        in [JSONFieldInfo accessor jsonName True kind]
      MEMapField mf ->
        let accessor = ctxFieldName ctx scope (mapFieldName mf)
            jsonName = fromMaybe (protoJsonName (mapFieldName mf)) (getJsonName (mapOptions mf))
            kind = case mapValueType mf of FTScalar SBytes -> JFKBytesMap; _ -> JFKNormal
        in [JSONFieldInfo accessor jsonName True kind]
      MEOneof od ->
        let accessor = ctxFieldName ctx scope (oneofName od)
            branches = fmap (oneofBranchToJSON ctx scope (oneofName od)) (oneofFields od)
        in [JSONFieldInfo accessor "" True (JFKOneof branches)]
      _ -> []


-- | Build the JSON projection of one oneof branch.
oneofBranchToJSON :: GenCtx -> [Text] -> Text -> OneofField -> JSONOneofBranch
oneofBranchToJSON ctx scope ooName f =
  let conName = oneofConName scope ooName (oneofFieldName f)
      -- proto3 JSON semantics: the BRANCH's own field name (or its
      -- 'json_name' option if it has one) is the JSON key — NOT the
      -- oneof group's name.
      jsonKey =
        fromMaybe
          (protoJsonName (oneofFieldName f))
          (getJsonName (oneofFieldOptions f))
      shape = case oneofFieldType f of
        FTScalar _ -> OBSScalar
        FTNamed n
          -- 'google.protobuf.NullValue' is the only enum-shaped WKT
          -- where bare JSON @null@ is a valid /value/ (rather than
          -- "this variant is absent"); flag it so the reader/writer
          -- can special-case the null-handling.
          | n == "google.protobuf.NullValue" -> OBSNullValue
          | otherwise -> case resolveType ctx n of
              Just ti | tiKind ti == TKEnum -> OBSEnum
              _ -> OBSMessage
  in JSONOneofBranch
      { jobConstructor = conName
      , jobJsonKey = jsonKey
      , jobShape = shape
      }


getJsonName :: [OptionDef] -> Maybe Text
getJsonName = \case
  [] -> Nothing
  opts -> lookupSimpleOption "json_name" opts >>= optionAsString


-- ---------------------------------------------------------------------------
-- Service generation
-- ---------------------------------------------------------------------------

genServiceTopLevel :: GenCtx -> [Text] -> ServiceDef -> [Doc ann]
genServiceTopLevel ctx scope svc =
  let pkg = fromMaybe "" (gcPkg ctx)
      resolveRpcType name =
        let candidates = [name, pkg <> "." <> name]
            go [] = Nothing
            go (c : cs) = case Map.lookup c (gcRegistry ctx) of
              Just ti -> Just ti
              Nothing -> go cs
        in go candidates
      -- RPC types are now registered in 'collectReferencedTypes', so
      -- the module-level imports block covers them. We only need to
      -- /qualify/ each RPC type when emitting the service's
      -- declarations.
      qualifyRpcType name = case resolveRpcType name of
        Just ti
          | tiModule ti /= gcThisMod ctx -> moduleAlias (tiModule ti) <> "." <> tiHsName ti
          | otherwise -> tiHsName ti
        Nothing -> hsTypeName (lastPart name)
      lastPart t = case T.splitOn "." t of [] -> t; parts -> last parts
      svcScope = scope <> [svcName svc]
      hookCtx =
        ServiceHookCtx
          { shcServiceDef = svc
          , shcScope = svcScope
          , shcHsTypeName = T.intercalate "'" (fmap hsTypeName svcScope)
          , shcOptions = svcOptions svc
          }
      hookOutput = onServiceCodeGen (genHooks (gcOpts ctx)) hookCtx
      hookDocs = fmap pretty hookOutput
  in Service.genServiceDeclsQualified (gcPkg ctx) scope qualifyRpcType svc
      <> case hookDocs of
        [] -> []
        ds -> [mempty, vsep ds]


-- ---------------------------------------------------------------------------
-- Proto2 extension blocks (@extend Foo { optional int32 bar = 123; }@)
-- ---------------------------------------------------------------------------

{- | Emit one top-level 'Proto.Extension.Extension' binding per field
in an @extend@ block. Callers of the generated module use
'Proto.Extension.getExtension' / 'setExtension' / 'clearExtension'
to interact with the extension through the message's
unknown-fields list; the owning message type automatically
satisfies 'Proto.Extension.HasExtensions' via the
per-message instance emitted by 'genHasExtensionsInstance'.

Singular + repeated (packed and unpacked) extensions are
emitted here. Proto2 message-groups never made it into this
codebase's 'FieldType' ADT, so the old-style group extension
is not representable at all — the parser rejects group
fields before they can reach this function.
-}
genExtensionBlock
  :: GenCtx -> [Text] -> Text -> [FieldDef] -> [Doc ann]
genExtensionBlock ctx scope extOwnerName fields =
  let pkg = fromMaybe "" (gcPkg ctx)
      ownerHsType = qualifyExtendedType ctx pkg extOwnerName
      ownerProtoShort = lastDot extOwnerName
      ownerPrefix = lowerFirst (hsTypeName ownerProtoShort)
  in concatMap (genOneExtension ctx scope ownerHsType ownerPrefix) fields


-- Generate one @Extension <owner> <payload>@ (or
-- @RepeatedExtension <owner> <payload>@) binding.
genOneExtension
  :: GenCtx -> [Text] -> Text -> Text -> FieldDef -> [Doc ann]
genOneExtension _ctx _scope ownerHsType ownerPrefix fd =
  let fieldNameHs =
        escapeReserved
          (ownerPrefix <> upperFirst (snakeToCamel (fieldName fd)))
      num = unFieldNumber (fieldNumber fd)
      repeated = fieldLabel fd == Just Repeated
      packed =
        repeated && case fieldType fd of
          FTScalar s -> packableScalar s
          _ -> False
      payload = extensionPayloadCore (fieldType fd)
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
            ]


{- | Project a proto 'FieldType' onto @(Haskell type,
'ExtensionType' constructor name)@. The label-aware split is now
the caller's job: 'genOneExtension' picks Extension vs.
RepeatedExtension based on @fieldLabel@.
-}
extensionPayloadCore :: FieldType -> Maybe (Doc ann, Text)
extensionPayloadCore (FTScalar s) = case s of
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
extensionPayloadCore (FTNamed _) =
  -- Named-type (message / enum) extensions carry raw encoded
  -- bytes; callers decode them lazily via the normal message
  -- decoder for the referenced type.
  Just (txt "ByteString", "ExtMessage")


{- | Whether a scalar can use the packed encoding (proto2 default
false; proto3 default true). Strings and bytes can't.
-}
packableScalar :: ScalarType -> Bool
packableScalar = \case
  SString -> False
  SBytes -> False
  _ -> True


-- Resolve an extended type name to its Haskell module-qualified
-- form. Same logic as 'qualifyRpcType' in 'genServiceTopLevel'.
qualifyExtendedType :: GenCtx -> Text -> Text -> Text
qualifyExtendedType ctx pkg name =
  let candidates = [name, pkg <> "." <> name]
      go [] = Nothing
      go (c : cs) = case Map.lookup c (gcRegistry ctx) of
        Just ti -> Just ti
        Nothing -> go cs
  in case go candidates of
      Just ti
        | tiModule ti /= gcThisMod ctx ->
            moduleAlias (tiModule ti) <> "." <> tiHsName ti
        | otherwise -> tiHsName ti
      Nothing -> hsTypeName (lastDot name)


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
     , genEnumDataDecl scope' ed
     , mempty
     , genEnumToProto scope' ed
     , mempty
     , genEnumFromProto scope' ed
     , mempty
     , genEnumEncodeInstance scope' ed
     , mempty
     , genEnumToJSONInstance scope' ed
     , mempty
     , genEnumFromJSONInstance scope' ed
     , mempty
     , genEnumHashableInstance scope' ed
     ]
      <> case hookDocs of
        [] -> []
        ds -> [mempty, vsep ds]


genEnumDataDecl :: [Text] -> EnumDef -> Doc ann
genEnumDataDecl scope ed =
  let tyN = scopedTypeName scope
      hasAlias = enumHasAliases ed
      primaryVals = enumPrimaryValues ed
      aliasVals = enumAliasValues ed
      deriveLine =
        if hasAlias
          then txt "deriving stock (Show, Eq, Ord, Generic)"
          else txt "deriving stock (Show, Eq, Ord, Prelude.Enum, Prelude.Bounded, Generic)"
      aliasSyns =
        fmap
          ( \ev ->
              txt "pattern "
                <> pretty (scopedEnumCon scope (evName ev))
                <+> txt "::"
                <+> pretty tyN
                <> line
                <> txt "pattern "
                <> pretty (scopedEnumCon scope (evName ev))
                <+> txt "= "
                <> pretty (scopedEnumCon scope (canonicalNameForNumber ed (evNumber ev)))
          )
          aliasVals
  in vsep $
      haddockDoc (enumDoc ed)
        <> [ txt "data " <> pretty tyN
           , indent 2 (vsep (concatMap (\(pfx, v) -> (pfx <+> pretty (scopedEnumCon scope (evName v))) : haddockFieldDoc (evDoc v)) (zip seps primaryVals)))
           , indent 2 deriveLine
           , indent 2 (txt "deriving anyclass NFData")
           ]
        <> aliasSyns
  where
    seps = txt "=" : repeat (txt "|")


genEnumToProto :: [Text] -> EnumDef -> Doc ann
genEnumToProto scope ed =
  let tyN = scopedTypeName scope
      primaryVals = enumPrimaryValues ed
      genCase ev =
        txt "toProtoEnum"
          <> pretty tyN
          <+> pretty (scopedEnumCon scope (evName ev))
          <+> txt "= "
          <> pretty (T.pack (show (evNumber ev)))
  in vsep
      [ txt "toProtoEnum" <> pretty tyN <+> txt "::" <+> pretty tyN <+> txt "-> Int"
      , vsep (fmap genCase primaryVals)
      ]


genEnumFromProto :: [Text] -> EnumDef -> Doc ann
genEnumFromProto scope ed =
  let tyN = scopedTypeName scope
      primaryVals = enumPrimaryValues ed
      genCase ev =
        txt "fromProtoEnum"
          <> pretty tyN
          <+> pretty (T.pack (show (evNumber ev)))
          <+> txt "= Just "
          <> pretty (scopedEnumCon scope (evName ev))
  in vsep
      [ txt "fromProtoEnum" <> pretty tyN <+> txt "::" <+> txt "Int -> Maybe " <> pretty tyN
      , vsep (fmap genCase primaryVals)
      , txt "fromProtoEnum" <> pretty tyN <+> txt "_ = Nothing"
      ]


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
      , indent 2 (txt "buildSized _ = mempty")
      , instanceHead "MessageDecode" tyN
      , indent 2 (txt "messageDecoder = pure (Prelude.toEnum 0)")
      ]


genEnumToJSONInstance :: [Text] -> EnumDef -> Doc ann
genEnumToJSONInstance scope ed =
  let tyN = scopedTypeName scope
      primaryVals = enumPrimaryValues ed
      genCase ev =
        txt "toJSON "
          <> pretty (scopedEnumCon scope (evName ev))
          <+> pretty ("= Aeson.String \"" :: Text)
          <> pretty (evName ev)
          <> pretty ("\"" :: Text)
  in vsep
      [ instanceHead "Aeson.ToJSON" tyN
      , indent 2 (vsep (fmap genCase primaryVals))
      ]


genEnumFromJSONInstance :: [Text] -> EnumDef -> Doc ann
genEnumFromJSONInstance scope ed =
  let tyN = scopedTypeName scope
      hasAlias = enumHasAliases ed
      genCase ev =
        pretty ("  Aeson.String \"" :: Text) <> pretty (evName ev) <> pretty ("\" -> pure " :: Text) <> pretty (scopedEnumCon scope (evName ev))
      fallbackNumCase =
        if hasAlias
          then
            txt "  Aeson.Number n -> case fromProtoEnum"
              <> pretty tyN
              <> pretty (" (round n) of { Just v -> pure v; Nothing -> fail \"Invalid enum\" }" :: Text)
          else txt "  Aeson.Number n -> pure (Prelude.toEnum (round n))"
  in vsep
      [ instanceHead "Aeson.FromJSON" tyN
      , indent 2 $
          vsep
            [ txt "parseJSON = \\case"
            , vsep (fmap genCase (enumValues ed))
            , fallbackNumCase
            , txt "  _ -> fail " <> pretty ("\"Invalid enum value for " :: Text) <> pretty tyN <> pretty ("\"" :: Text)
            ]
      ]


-- ---------------------------------------------------------------------------
-- Haskell type helpers
-- ---------------------------------------------------------------------------

hsFieldType :: GenCtx -> [Text] -> FieldType -> Maybe FieldLabel -> Doc ann
hsFieldType ctx scope ft = \case
  Just Repeated -> hsRepeatedType ctx scope ft
  Just Optional -> txt "!(Maybe " <> hsFieldTypeInner ctx scope ft <> txt ")"
  Just Required -> txt "!" <> hsFieldTypeInner ctx scope ft
  Nothing -> case ft of
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
    | otherwise -> moduleAlias (tiModule ti) <> "." <> tiHsName ti
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
  escapeReserved (snakeToCamel fName)


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


oneofConName :: [Text] -> Text -> Text -> Text
oneofConName scope oneofN fieldName =
  scopedTypeName scope <> "'" <> snakeToPascal oneofN <> "'" <> snakeToPascal fieldName
