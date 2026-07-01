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
  GenerateOpts (..),
  defaultGenerateOpts,
  FieldNaming (..),
  JsonOverride (..),
  defaultJsonOverrides,
  TypeRegistry,
  TypeInfo (..),
  TypeKind (..),
  buildTypeRegistry,
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

import Data.ByteString qualified as BS
import Data.Char (isAsciiUpper, toLower, toUpper)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
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
    [ wkt "google.protobuf.Duration" "Proto.Google.Protobuf.Duration" "Duration"
    , wkt "google.protobuf.Timestamp" "Proto.Google.Protobuf.Timestamp" "Timestamp"
    , wkt "google.protobuf.Empty" "Proto.Google.Protobuf.Empty" "Empty"
    , wkt "google.protobuf.Any" "Proto.Google.Protobuf.Any" "Any"
    , wkt "google.protobuf.Struct" "Proto.Google.Protobuf.Struct" "Struct"
    , wkt "google.protobuf.Value" "Proto.Google.Protobuf.Struct" "Value"
    , wkt "google.protobuf.ListValue" "Proto.Google.Protobuf.Struct" "ListValue"
    , wkt "google.protobuf.NullValue" "Proto.Google.Protobuf.Struct" "NullValue"
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
      localTypes = collectLocalTypes (fromMaybe "" (protoPackage pf)) [] (protoTopLevels pf)
      ctx =
        GenCtx
          { gcOpts = opts
          , gcRegistry = reg
          , gcThisMod = thisMod
          , gcPkg = protoPackage pf
          , gcLocalTypes = localTypes
          }
      body = concatMap (genTopLevel ctx []) (protoTopLevels pf)
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
      candidates =
        [ name
        , pkg <> "." <> name
        ]
          <> fmap (\s -> pkg <> "." <> T.intercalate "." s <> "." <> name) (filter (not . null) (tails' scope))
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
       , txt "{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches -Wno-unused-top-binds #-}"
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
    , txt "import Proto.Internal.JSON (jsonObject, (.=:), parseFieldMaybe, bytesFieldToJSON, parseBytesFieldMaybe, bytesMapFieldToJSON, parseBytesMapFieldMaybe, protoBytesToJSON)"
    , txt "import Data.Proxy (Proxy(..))"
    , txt "import Data.Reflection (Given(..), given)"
    , txt "import Proto.Internal.JSON.Extension qualified as PJExt"
    , txt "import Proto.Schema (ProtoMessage(..), SomeFieldDescriptor(..), FieldDescriptor(..), FieldTypeDescriptor(..), ScalarFieldType(..), FieldLabel'(..))"
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
      <> fmap genQualifiedImport (Set.toAscList externalModules)


genQualifiedImport :: Text -> Doc ann
genQualifiedImport modName =
  txt "import qualified " <> pretty modName <> txt " as " <> pretty (moduleAlias modName)


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
genFileDescriptorBinding :: FilePath -> ProtoFile -> Doc ann
genFileDescriptorBinding filePath pf =
  let fdpBytes = serializeFileDescriptor filePath pf
      escapedLit = byteStringToHsLiteral fdpBytes
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
  MEExtend extName fields -> genExtensionBlock ctx scope extName fields
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


genOneofToJSONInstance :: GenCtx -> [Text] -> OneofDef -> Doc ann
genOneofToJSONInstance _ctx scope od =
  let tyN = scopedTypeName scope <> "'" <> snakeToPascal (oneofName od)
  in vsep
       [ instanceHead "Aeson.ToJSON" tyN
       , indent 2 (txt "toJSON _ = Aeson.Null")
       ]


genOneofFromJSONInstance :: GenCtx -> [Text] -> OneofDef -> Doc ann
genOneofFromJSONInstance _ctx scope od =
  let tyN = scopedTypeName scope <> "'" <> snakeToPascal (oneofName od)
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
        Just ti | tiKind ti == TKEnum -> txt "(toEnum 0)"
        _ -> txt "Nothing"


-- ---------------------------------------------------------------------------
-- Encode instances
-- ---------------------------------------------------------------------------

genEncodeInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genEncodeInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fields = extractAllFields ctx scope (msgElements msg)
      unknownAcc = "msg." <> unknownFieldAccessor scope
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
       FKScalar lbl ft -> genBuildExprScalar fn accessor lbl ft
       FKNamed lbl name tk -> genBuildExprNamed ctx fn accessor lbl name tk
       FKMap keyT valT -> genBuildExprMap ctx fn accessor keyT valT
       FKOneof scope ood -> genBuildExprOneof ctx scope fn accessor ood


genBuildExprScalar :: Text -> Text -> Maybe FieldLabel -> ScalarType -> Doc ann
genBuildExprScalar fn accessor lbl st = case lbl of
  Just Repeated -> genRepeatedScalarBuild fn accessor st
  Just Optional -> txt "(maybe mempty (\\v -> " <> genSingleScalarBuild fn "v" st <> txt ") " <> pretty accessor <> txt ")"
  _ -> txt "(if " <> scalarDefaultCheck accessor st <> txt " then mempty else " <> genSingleScalarBuild fn accessor st <> txt ")"


genBuildExprNamed :: GenCtx -> Text -> Text -> Maybe FieldLabel -> Text -> TypeKind -> Doc ann
genBuildExprNamed _ctx fn accessor lbl _name tk = case tk of
  TKEnum -> case lbl of
    Just Repeated -> txt "V.foldl' (\\acc v -> acc <> archVarint " <> tagLit fn wireVarint <+> txt "(fromIntegral (fromEnum v))) mempty " <> pretty accessor
    Just Optional -> txt "(maybe mempty (\\v -> archVarint " <> tagLit fn wireVarint <+> txt "(fromIntegral (fromEnum v))) " <> pretty accessor <> txt ")"
    _ -> txt "(if fromEnum " <> pretty accessor <> txt " == 0 then mempty else archVarint " <> tagLit fn wireVarint <+> txt "(fromIntegral (fromEnum " <> pretty accessor <> txt ")))"
  TKMessage -> case lbl of
    Just Repeated -> txt "V.foldl' (\\acc v -> let sz = messageSize v in acc <> archSubmessage " <> tagLit fn wireLengthDelimited <+> txt "sz (buildMessage v)) mempty " <> pretty accessor
    Just Optional -> txt "(maybe mempty (\\v -> let sz = messageSize v in archSubmessage " <> tagLit fn wireLengthDelimited <+> txt "sz (buildMessage v)) " <> pretty accessor <> txt ")"
    _ -> txt "(maybe mempty (\\v -> let sz = messageSize v in archSubmessage " <> tagLit fn wireLengthDelimited <+> txt "sz (buildMessage v)) " <> pretty accessor <> txt ")"


genBuildExprMap :: GenCtx -> Text -> Text -> ScalarType -> FieldType -> Doc ann
genBuildExprMap ctx fn accessor keyT valT =
  txt "Map.foldlWithKey' (\\acc k v -> acc <> encodeMapField "
    <> pretty fn
    <> txt " ("
    <> genMapKeyEncode keyT
    <> txt " k) ("
    <> genMapValEncode ctx valT
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


genMapValEncode :: GenCtx -> FieldType -> Doc ann
genMapValEncode ctx = \case
  FTScalar st -> genMapKeyEncode' 2 st
  FTNamed n -> case resolveType ctx n of
    Just ti | tiKind ti == TKEnum -> txt "(\\x -> encodeFieldVarint 2 (fromIntegral (fromEnum x)))"
    _ -> txt "encodeFieldMessage 2"
  where
    genMapKeyEncode' :: Int -> ScalarType -> Doc ann
    genMapKeyEncode' fn st = case st of
      SString -> txt "encodeFieldString " <> pretty (T.pack (show fn))
      SBytes -> txt "encodeFieldBytes " <> pretty (T.pack (show fn))
      SBool -> txt "encodeFieldBool " <> pretty (T.pack (show fn))
      SDouble -> txt "encodeFieldDouble " <> pretty (T.pack (show fn))
      SFloat -> txt "encodeFieldFloat " <> pretty (T.pack (show fn))
      SFixed32 -> txt "encodeFieldFixed32 " <> pretty (T.pack (show fn))
      SFixed64 -> txt "encodeFieldFixed64 " <> pretty (T.pack (show fn))
      SSFixed32 -> txt "encodeFieldFixed32 " <> pretty (T.pack (show fn)) <> txt " . fromIntegral"
      SSFixed64 -> txt "encodeFieldFixed64 " <> pretty (T.pack (show fn)) <> txt " . fromIntegral"
      _ -> txt "encodeFieldVarint " <> pretty (T.pack (show fn)) <> txt " . fromIntegral"


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
                 txt "archVarint " <> tagLit fn wireVarint <+> txt "(fromIntegral (fromEnum v))"
           _ -> txt "(let sz = messageSize v in archSubmessage " <> tagLit fn wireLengthDelimited <+> txt "sz (buildMessage v))"


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


genRepeatedScalarBuild :: Text -> Text -> ScalarType -> Doc ann
genRepeatedScalarBuild fn accessor = \case
  SString ->
    txt "V.foldl' (\\acc v -> acc <> archString " <> tagLit fn wireLengthDelimited <+> txt "v) mempty " <> pretty accessor
  SBytes ->
    txt "V.foldl' (\\acc v -> acc <> archBytes " <> tagLit fn wireLengthDelimited <+> txt "v) mempty " <> pretty accessor
  s -> txt "encode" <> pretty (packedFnName s) <+> pretty fn <+> pretty accessor


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
-- Size instances
-- ---------------------------------------------------------------------------

genSizeInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genSizeInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fields = extractAllFields ctx scope (msgElements msg)
      unknownAcc = "msg." <> unknownFieldAccessor scope
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
       FKScalar lbl ft -> genSizeScalar fn accessor lbl ft
       FKNamed lbl name tk -> genSizeNamed ctx fn accessor lbl name tk
       FKMap keyT valT -> genSizeMap ctx fn accessor keyT valT
       FKOneof scope ood -> genSizeOneof ctx scope fn accessor ood


genSizeScalar :: Text -> Text -> Maybe FieldLabel -> ScalarType -> Doc ann
genSizeScalar fn accessor lbl st = case lbl of
  Just Repeated -> genRepeatedSizeScalar fn accessor st
  Just Optional -> txt "(maybe 0 (\\v -> " <> genSingleSizeScalar fn "v" st <> txt ") " <> pretty accessor <> txt ")"
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
genSingleSizeScalar _fn accessor = \case
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


genSizeNamed :: GenCtx -> Text -> Text -> Maybe FieldLabel -> Text -> TypeKind -> Doc ann
genSizeNamed _ctx _fn accessor lbl _name tk = case tk of
  TKEnum -> case lbl of
    Just Repeated -> txt "(V.foldl' (\\acc v -> acc + archVarintSize (fromIntegral (fromEnum v))) 0 " <> pretty accessor <> txt ")"
    Just Optional -> txt "(maybe 0 (\\v -> archVarintSize (fromIntegral (fromEnum v))) " <> pretty accessor <> txt ")"
    _ -> txt "(if fromEnum " <> pretty accessor <> txt " == 0 then 0 else archVarintSize (fromIntegral (fromEnum " <> pretty accessor <> txt ")))"
  TKMessage -> case lbl of
    Just Repeated -> txt "(V.foldl' (\\acc v -> acc + archSubmessageSize (messageSize v)) 0 " <> pretty accessor <> txt ")"
    Just Optional -> txt "(maybe 0 (\\v -> archSubmessageSize (messageSize v)) " <> pretty accessor <> txt ")"
    _ -> txt "(maybe 0 (\\v -> archSubmessageSize (messageSize v)) " <> pretty accessor <> txt ")"


genSizeMap :: GenCtx -> Text -> Text -> ScalarType -> FieldType -> Doc ann
genSizeMap ctx fn accessor keyT valT =
  let keySizeExpr = mapKeySizeExpr keyT
      valSizeExpr = mapValSizeExpr ctx valT
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


mapValSizeExpr :: GenCtx -> FieldType -> Doc ann
mapValSizeExpr ctx = \case
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
  FTNamed n -> case resolveType ctx n of
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
              let conName = oneofConName scope (oneofName ood) (oneofFieldName f)
                  ffn = T.pack (show (unFieldNumber (oneofFieldNumber f)))
              in txt "; Just ("
                   <> pretty conName
                   <+> txt "v) -> "
                   <> case oneofFieldType f of
                     FTScalar st -> genSingleSizeScalar ffn "v" st
                     FTNamed n -> case resolveType ctx n of
                       Just ti | tiKind ti == TKEnum -> txt "archVarintSize (fromIntegral (fromEnum v))"
                       _ -> txt "archSubmessageSize (messageSize v)"
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
      unknownFieldName = unknownFieldAccessor scope
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
                         , indent 2 $ vsep (concatMap (genFieldDecodeCase ctx allAccsWithUnknown) fields <> [genDefaultDecodeCase scope allAccsWithUnknown]) <> txt ")"
                         ]
                   ]
             ]
       ]


genFieldDecodeCase :: GenCtx -> [Text] -> FieldInfoFull -> [Doc ann]
genFieldDecodeCase ctx allAccs fi =
  case fifKind fi of
    FKScalar lbl ft -> [genScalarDecodeCase allAccs fi lbl ft]
    FKNamed lbl name tk -> [genNamedDecodeCase ctx allAccs fi lbl name tk]
    FKMap keyT valT -> [genMapDecodeCase ctx allAccs fi keyT valT]
    FKOneof scope ood -> concatMap (genOneofDecodeCase ctx scope (oneofName ood) allAccs fi) (oneofFields ood)


genScalarDecodeCase :: [Text] -> FieldInfoFull -> Maybe FieldLabel -> ScalarType -> Doc ann
genScalarDecodeCase allAccs fi lbl st =
  let fn = T.pack (show (fifFieldNum fi))
      idx = fifIndex fi
      accName = "acc_" <> T.pack (show idx)
      singletonFn = if isUnboxableScalar st then "VU.singleton" else "V.singleton"
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
         let appendOne =
               replaceAt idx ("(" <> accName <> " <> " <> singletonFn <> " v)") allAccs
             appendMany =
               replaceAt idx ("(" <> accName <> " <> vs)") allAccs
             packedDecoderE = scalarPackedDecoderExpr st
             unpackedDecoderE = scalarDecoderExpr st
             isPackable = case st of
               SString -> False
               SBytes -> False
               _ -> True
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
                                , txt "loop " <> hsep (fmap pretty appendMany)
                                ]
                            )
                        , txt "_ -> do"
                        , indent
                            2
                            ( vsep
                                [ txt "v <- " <> pretty unpackedDecoderE
                                , txt "loop " <> hsep (fmap pretty appendOne)
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
                        , txt "loop " <> hsep (fmap pretty appendOne)
                        ]
                    )
       Just Optional ->
         -- proto3 'optional <scalar>' fields synthesise a oneof-style
         -- presence bit; the field's record slot is 'Maybe T' (see
         -- 'fieldDefaultText'), so the decoded value has to be
         -- wrapped with 'Just' here.
         let newAccs = replaceAt idx "(Just v)" allAccs
         in pretty fn
              <+> txt "-> do"
              <> line
              <> indent
                2
                ( vsep
                    [ txt "v <- " <> pretty (scalarDecoderExpr st)
                    , txt "loop " <> hsep (fmap pretty newAccs)
                    ]
                )
       _ ->
         let newAccs = replaceAt idx "v" allAccs
         in pretty fn
              <+> txt "-> do"
              <> line
              <> indent
                2
                ( vsep
                    [ txt "v <- " <> pretty (scalarDecoderExpr st)
                    , txt "loop " <> hsep (fmap pretty newAccs)
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


genNamedDecodeCase :: GenCtx -> [Text] -> FieldInfoFull -> Maybe FieldLabel -> Text -> TypeKind -> Doc ann
genNamedDecodeCase _ctx allAccs fi lbl _name tk =
  let fn = T.pack (show (fifFieldNum fi))
      idx = fifIndex fi
      accName = "acc_" <> T.pack (show idx)
      newAccs = case lbl of
        Just Repeated -> replaceAt idx ("(" <> accName <> " <> V.singleton v)") allAccs
        Just Optional -> replaceAt idx "(Just v)" allAccs
        _ -> case tk of
          TKEnum -> replaceAt idx "v" allAccs
          TKMessage -> replaceAt idx "(Just v)" allAccs
      decoderExpr :: Text
      decoderExpr = case tk of
        TKEnum -> "decodeFieldEnum"
        TKMessage -> "decodeFieldMessage"
  in pretty fn
       <+> txt "-> do"
       <> line
       <> indent
         2
         ( vsep
             [ txt "v <- " <> pretty decoderExpr
             , txt "loop " <> hsep (fmap pretty newAccs)
             ]
         )


genMapDecodeCase :: GenCtx -> [Text] -> FieldInfoFull -> ScalarType -> FieldType -> Doc ann
genMapDecodeCase ctx allAccs fi keyT valT =
  let fn = T.pack (show (fifFieldNum fi))
      idx = fifIndex fi
      accName = "acc_" <> T.pack (show idx)
      newAccs = replaceAt idx ("(Map.union " <> accName <> " (Map.singleton mk' mv'))") allAccs
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
                   [ txt "Left _ -> loop " <> hsep (fmap pretty allAccs)
                   , txt "Right (mk', mv') -> loop " <> hsep (fmap pretty newAccs)
                   ]
             ]
         )


genOneofDecodeCase :: GenCtx -> [Text] -> Text -> [Text] -> FieldInfoFull -> OneofField -> [Doc ann]
genOneofDecodeCase ctx scope ooName allAccs fi oof =
  let fn = T.pack (show (unFieldNumber (oneofFieldNumber oof)))
      idx = fifIndex fi
      conName = oneofConName scope ooName (oneofFieldName oof)
      newAccs = replaceAt idx ("(Just (" <> conName <> " v))") allAccs
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
               , txt "loop " <> hsep (fmap pretty newAccs)
               ]
           )
     ]


genDefaultDecodeCase :: [Text] -> [Text] -> Doc ann
genDefaultDecodeCase _scope allAccsWithUnknown =
  let unknownAcc = last allAccsWithUnknown
      fieldAccs = init allAccsWithUnknown
      newAccs = fieldAccs <> ["(uf : " <> unknownAcc <> ")"]
  in txt "_ -> do"
       <> line
       <> indent
         2
         ( vsep
             -- 'wt' comes through as an 'Int' (withTagM's continuation
             -- takes raw Ints), so 'toEnum' it back into a 'WireType'
             -- before handing it to 'captureUnknownField'.
             [ txt "uf <- captureUnknownField fn (toEnum wt)"
             , txt "loop " <> hsep (fmap pretty newAccs)
             ]
         )


fieldDefaultText :: GenCtx -> FieldInfoFull -> Text
fieldDefaultText _ctx fi = case fifKind fi of
  FKScalar lbl ft -> case lbl of
    Just Repeated | isUnboxableScalar ft -> "VU.empty"
    Just Repeated -> "V.empty"
    Just Optional -> "Nothing"
    _ -> scalarDefaultText ft
  FKNamed lbl _ tk -> case lbl of
    Just Repeated -> "V.empty"
    Just Optional -> "Nothing"
    _ -> case tk of
      TKEnum -> "(toEnum 0)"
      TKMessage -> "Nothing"
  FKMap _ _ -> "Map.empty"
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
    Just ti | tiKind ti == TKEnum -> "(toEnum 0)"
    _ -> "undefined"


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


genProtoMessageInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genProtoMessageInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fqn = fqProtoName (gcPkg ctx) scope
      pkg = fromMaybe "" (gcPkg ctx)
      fields = extractMessageFieldsForSchema scope (msgElements msg)
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
       (e : es) ->
         vsep [txt "[ " <> e]
           <> vsep (fmap (\x -> txt ", " <> x) es)
           <> line
           <> txt "]"


genFieldDescriptorEntry :: GenCtx -> [Text] -> SchemaField -> Doc ann
genFieldDescriptorEntry ctx scope sf =
  let accN = ctxFieldName ctx scope (sfName sf)
      -- Under 'UnprefixedFields' the record fields are bare (and
      -- 'NoFieldSelectors' is on), so a bare selector for 'fdGet' does not
      -- exist and an un-annotated update for 'fdSet' is ambiguous when the
      -- same field name recurs across messages in the file. Both are made
      -- concrete by fixing the message type: a record-dot read and an
      -- update on a type-annotated binder. Under 'PrefixedFields' the
      -- selectors are unique, so the plain forms are kept byte-identical.
      tyN = scopedTypeName scope
      (getExpr, setExpr) = case genFieldNaming (gcOpts ctx) of
        UnprefixedFields ->
          ( "\\(m :: " <> tyN <> ") -> m." <> accN
          , "\\v (m :: " <> tyN <> ") -> m { " <> accN <> " = v }"
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
             , txt ", fdTypeDesc = " <> genFieldTypeDesc (sfType sf)
             , txt ", fdLabel = " <> genLabelLit (sfLabel sf)
             , txt ", fdGet = " <> pretty getExpr
             , txt ", fdSet = " <> pretty setExpr
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

hasExtensions :: MessageDef -> Bool
hasExtensions msg = any isExt (msgElements msg)
  where
    isExt (MEExtensions _ _) = True
    isExt _ = False


genToJSONInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genToJSONInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fqn = fqProtoName (gcPkg ctx) scope
      overrides = genJsonOverrides (gcOpts ctx)
      hasExts = hasExtensions msg
      headDoc =
        if hasExts
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
                            then txt "(PJExt.extensionEntriesForJson (given :: PJExt.ExtensionRegistry) " <> pretty ("\"" :: Text) <> pretty fqn <> pretty ("\" msg." :: Text) <> txt (unknownFieldAccessor scope) <> txt ")"
                            else txt "[]"
                        (f0 : fs) ->
                          vsep $
                            ( if hasExts
                                then [txt "(PJExt.extensionEntriesForJson (given :: PJExt.ExtensionRegistry) " <> pretty ("\"" :: Text) <> pretty fqn <> pretty ("\" msg." :: Text) <> txt (unknownFieldAccessor scope) <+> txt "++"]
                                else []
                            )
                              <> [ txt "[ " <> genToJSONField ctx f0
                                 , vsep (fmap (\f -> txt ", " <> genToJSONField ctx f) fs)
                                 , txt "]" <> if hasExts then txt ")" else emptyDoc
                                 ]
                    ]
              ]


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


genFromJSONInstance :: GenCtx -> [Text] -> MessageDef -> Doc ann
genFromJSONInstance ctx scope msg =
  let tyN = scopedTypeName scope
      fqn = fqProtoName (gcPkg ctx) scope
      overrides = genJsonOverrides (gcOpts ctx)
      hasExts = hasExtensions msg
      headDoc =
        if hasExts
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
                            <> indent 4 (vsep (extsBind <> [txt "pure default" <> pretty tyN, indent 2 (braceBlock [txt (unknownFieldAccessor scope) <+> txt "=" <+> extsAssign])]))
                        else txt "parseJSON _ = pure default" <> pretty tyN
                  ]
              (f0 : frest) ->
                -- Under 'UnprefixedFields' a record update whose fields recur
                -- across messages is ambiguous; pin it with a trailing type
                -- annotation. 'PrefixedFields' updates are unique, so they are
                -- left byte-identical.
                let (pureOpen, closeBrace) = case genFieldNaming (gcOpts ctx) of
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
                                           ( txt "{ " <> genFromJSONFieldAssign tyN f0
                                               : fmap (\jfi -> txt ", " <> genFromJSONFieldAssign tyN jfi) frest
                                                 <> [txt ", " <> txt (unknownFieldAccessor scope) <+> txt "=" <+> extsAssign]
                                                 <> [closeBrace]
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


genFromJSONFieldAssign :: Text -> JSONFieldInfo -> Doc ann
genFromJSONFieldAssign tyN jfi = case jfiKind jfi of
  JFKOptionalBytes ->
    -- The slot is already 'Maybe ByteString'; the parse helper
    -- returned 'Maybe ByteString' too, so just plug it in
    -- directly instead of stripping the wrapper.
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
             FKNamed (Just Required) _ TKMessage -> txt "case " <> ba <> txt " of { Nothing -> " <> aa <> txt "; x -> x }"
             FKNamed (Just Required) _ TKEnum -> lastNonDefault "(toEnum 0)"
             FKNamed Nothing _ TKMessage -> txt "case " <> ba <> txt " of { Nothing -> " <> aa <> txt "; x -> x }"
             FKNamed Nothing _ TKEnum -> lastNonDefault "(toEnum 0)"
             FKScalar (Just Optional) _ -> txt "case " <> ba <> txt " of { Nothing -> " <> aa <> txt "; x -> x }"
             FKScalar (Just Required) ft -> lastNonDefault (scalarDefaultText ft)
             FKScalar Nothing ft -> lastNonDefault (scalarDefaultText ft)
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
data JSONFieldKind = JFKNormal | JFKBytes | JFKOptionalBytes | JFKBytesMap
  deriving stock (Show, Eq)


data JSONFieldInfo = JSONFieldInfo
  { jfiAccessor :: Text
  , jfiJsonName :: Text
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
            jsonName = protoJsonName (oneofName od)
        in [JSONFieldInfo accessor jsonName True JFKNormal]
      _ -> []


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
          else txt "deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)"
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
       , indent 2 (txt "buildMessage _ = mempty")
       , instanceHead "MessageSize" tyN
       , indent 2 (txt "messageSize _ = 0")
       , instanceHead "MessageDecode" tyN
       , indent 2 (txt "messageDecoder = pure (toEnum 0)")
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
          else txt "  Aeson.Number n -> pure (toEnum (round n))"
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


replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 x (_ : ys) = x : ys
replaceAt n x (y : ys) = y : replaceAt (n - 1) x ys
