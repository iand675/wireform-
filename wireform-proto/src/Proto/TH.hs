{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-unused-matches -Wno-partial-fields #-}

{- | Template Haskell support for generating protobuf types at compile time.

== Basic usage

@
{\-\# LANGUAGE TemplateHaskell \#-\}
import Proto.TH

\$(loadProto "path/to/message.proto")
@

For each message in the file the splice produces:

  * A record data type plus a @default\<TypeName\>@ value with all fields
    at their proto default values.
  * @MessageEncode@ \/ @MessageSize@ \/ @MessageDecode@ wire codecs
    (via "Proto.Internal.Derive").
  * @HasExtensions@ (proto2 extension support).
  * 'Proto.Schema.ProtoMessage' schema metadata
    (@protoMessageName@ \/ @protoPackageName@ \/ @protoDefaultValue@
    \/ @protoFieldDescriptors@).
  * Proto3 canonical JSON: @Aeson.ToJSON@ + @Aeson.FromJSON@ with
    camelCase keys, base64 bytes, string-encoded 64-bit integers,
    NaN \/ Infinity sentinels for floats.
  * @Hashable@ -- recursive structural hash.

For each enum in the file:

  * A sum data type plus a proto-faithful @Enum@ instance
    using @evNumber@ as the wire number, with an @\<Enum\>'Unknown@
    constructor for open-enum round-tripping.
  * 'Proto.Schema.ProtoEnum' (@protoEnumName@,
    @protoEnumValues@, @toProtoEnumValue@, @fromProtoEnumValue@).
  * @Aeson.ToJSON@ \/ @FromJSON@ -- encode as the primary name
    string; decode from either the name or the wire number.
  * @Hashable@ -- hash by wire number.

== 'LoadOpts' configuration

Control code generation via 'LoadOpts' passed to 'loadProtoWith':

  ['loIncludeDirs'] Directories to search when resolving @import@
    statements in @.proto@ files. Default: @[\"proto\/\", \".\"]@.

  ['loFieldNaming'] How to name generated record fields. The default
    'Proto.CodeGen.PrefixedFields' prefixes each field with the
    lowercased message name (@personName@, @personAge@). Use
    'Proto.CodeGen.UnprefixedFields' for bare names (@name@, @age@),
    which requires @DuplicateRecordFields@ but works well with
    @OverloadedRecordDot@.

  ['loRepConfig'] A 'Proto.Repr.RepConfig' controlling how proto field
    types map to Haskell types. See "Proto.Repr" for the full adapter
    system, per-field overrides, and @.proto@ annotation support.

  ['loTHHooks'] 'Proto.CodeGen.Hooks.THHooks' callbacks that produce
    extra TH declarations based on proto attributes.

== Custom representations

@
\$(loadProtoWith (defaultLoadOpts
      { loRepConfig = defaultRepConfig
          { configFieldOverrides = Map.fromList
              [ (("Person","name"), defaultFieldRep { fieldString = shortTextAdapter })
              ]
          }
      })
    "path/to/file.proto")
@

== Codegen hooks

Use 'loTHHooks' to register 'Proto.CodeGen.Hooks.THHooks' that produce
extra declarations based on proto attributes:

@
import Proto.TH
import Proto.CodeGen.Hooks
import Language.Haskell.TH

-- Generate a @describeX :: String@ function for every message
descrHook :: THHooks
descrHook = mempty
  { thOnMessage = \\ctx -> do
      let name = mkName ("describe" <> T.unpack (mhcHsTypeName ctx))
      sig  \<- sigD name [t| String |]
      body \<- valD (varP name)
               (normalB (litE (stringL (T.unpack (mhcFqProtoName ctx))))) []
      pure [sig, body]
  }

\$(loadProtoWith defaultLoadOpts { loTHHooks = descrHook } "my.proto")
-- Now @describeMyMessage :: String@ is in scope.
@
-}
module Proto.TH (
  loadProto,
  loadProtoWith,
  loadProtoWithTypeNames,
  LoadOpts (..),
  defaultLoadOpts,
  protoFileToDecls,
  messageToDecls,
  enumToDecls,
) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as SBS
import Data.Char qualified
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Maybe (mapMaybe)
import Data.Maybe qualified
import Data.Proxy (Proxy (..))
import Data.Reflection qualified as Reflection
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile, addModFinalizer, lift)
import Proto.CodeGen (
  ExtensionNames (..),
  FieldNaming (..),
  NamedJSONField (..),
  NamedJSONShape (..),
  OneofBindingNames,
  RecordBindingNames (..),
  TypeInfo (tiHsName, tiKind, tiModule),
  TypeKind (..),
  TypeRegistry,
  buildTypeRegistry,
  builtinDescriptorTypes,
  defaultGenerateOpts,
  enumConstructorNames,
  extensionBindingNames,
  hsTypeName,
  fieldLabelNames,
  lookupExtensionNames,
  namedJSONFields,
  oneofBindingNames,
  oneofConstructorName,
  oneofTypeName,
  protoJsonName,
  recordBindingNames,
  registryMessageNames,
  resolveTypeKey,
  restrictOptionImportTypes,
  scopedFieldNameWith,
  scopedTypeName,
  scopedEnumCon,
  snakeToCamel,
  verifiedStringExtensions,
 )
import Proto.CodeGen.Hooks
import Proto.Decode qualified as Decode
import Proto.Extension qualified as Ext
-- Well-Known-Type modules. Imported here so 'lookupWkt' can hand back
-- fully-resolved (quoted) 'Name's for the WKT types it references, which
-- makes the generated code self-contained: the user no longer has to import
-- the matching @Proto.Google.Protobuf.*@ module at the @loadProto@ call site.
import Proto.Google.Protobuf.Any qualified as WktAny
import Proto.Google.Protobuf.Duration qualified as WktDuration
import Proto.Google.Protobuf.Empty qualified as WktEmpty
import Proto.Google.Protobuf.FieldMask qualified as WktFieldMask
import Proto.Google.Protobuf.SourceContext qualified as WktSourceContext
import Proto.Google.Protobuf.Struct qualified as WktStruct
import Proto.Google.Protobuf.Timestamp qualified as WktTimestamp
import Proto.Google.Protobuf.Wrappers qualified as WktWrappers
import Proto.IDL.AST
import Proto.IDL.Annotations (lookupSimpleOption, optionAsString)
import Proto.IDL.Descriptor (descriptorTypeDefinitions, serializeFileDescriptorWithImports)
import Proto.IDL.Options.Custom (emptyCustomOptionRegistry, extractExtensionOptions, registerCustomOption)
import Proto.IDL.Parser (parseProtoFile, renderParseError)
import Proto.IDL.Parser.Resolver (ResolvedProto (..))
import Proto.Internal.Derive qualified as PDI
import Proto.Internal.FieldPresence (enumIsClosed, explicitFieldPresence, messageEncodingDelimited, repeatedFieldPacked, resolveFieldPresence)
import Proto.Internal.JSON qualified as PJ
import Proto.Internal.JSON.Extension qualified as PJExt
import Proto.Internal.JSON.WellKnown qualified as WK
import Proto.Repr
import Proto.Schema qualified as PS
import Proto.TH.Metadata qualified as PTM
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Wireform.Derive.Modifier (MapKeyScalar (..))


{- | Scope-prefixed Haskell type name. Mirrors
'Proto.CodeGen.scopedTypeName': joins the parent chain
with the message's own name using @'@. Empty parent list
collapses to plain 'hsTypeName'.
-}
scopedHsTypeName :: [Text] -> Text -> Text
scopedHsTypeName parents nm = case parents of
  [] -> hsTypeName nm
  _ -> T.intercalate "'" (fmap hsTypeName (parents <> [nm]))


{- | The TH 'Name' of the synthetic @<EnumName>'Unknown !Int32@
constructor every loadProto-generated enum carries to hold
proto3 "open enum" wire values that aren't covered by any
declared name.
-}
unknownConNameFor :: [Text] -> Text -> Name
unknownConNameFor parents enumNm =
  mkName
    ( T.unpack
        (scopedHsTypeName parents enumNm <> T.pack "''Unrecognized")
    )


thFieldName :: ScopeCtx -> Text -> Name
thFieldName scope name = mkName . T.unpack $
  Map.findWithDefault
    (scopedFieldNameWith (scFieldNaming scope) [scopedTypeName parents] name)
    (parents, name)
    (recordSelectorNames (scRecordNames scope))
  where
    parents = scParents scope


{- | Enum constructor name. The proto-side value names already
typically carry an enum-prefixing convention
(@STATUS_UNSPECIFIED@, @STATUS_ACTIVE@, …); we just snake-to-
Pascal them, matching what 'Proto.CodeGen' does for the
single-name (un-scoped) variant. Cross-enum collisions on bare
value names (e.g. two enums each declaring @UNSPECIFIED@) need
to be resolved by the user via proto-side renaming; the TH
bridge stays lean rather than emitting always-prefixed names
nobody asked for.
-}

{- | When a repeated field has a packable scalar element and the user
hasn't overridden the repeated adapter, upgrade to unboxed vector
for zero per-element heap overhead.
-}
autoUnboxRepeated :: Maybe FieldLabel -> FieldType -> FieldRep -> FieldRep
autoUnboxRepeated (Just Repeated) (FTScalar st) rep
  | isUnboxableScalar st
  , repeatedBaseRep (fieldRepeated rep) == VectorRep =
      rep {fieldRepeated = unboxedVectorAdapter}
autoUnboxRepeated _ _ rep = rep


isUnboxableScalar :: ScalarType -> Bool
isUnboxableScalar = \case
  SString -> False
  SBytes -> False
  _ -> True


{- | Options for compile-time proto loading.

Use 'loTHHooks' to register hooks that produce extra TH declarations
based on proto attributes:

@
\$(loadProtoWith defaultLoadOpts
      { loTHHooks = myTHHooks }
    \"path/to/file.proto\")
@
-}
data LoadOpts = LoadOpts
  { loIncludeDirs :: [FilePath]
  {- ^ Directories to search when resolving @import@ statements in
  @.proto@ files. Default: @[\"proto\/\", \".\"]@.
  -}
  , loFieldNaming :: FieldNaming
  {- ^ How to name generated record fields. 'PrefixedFields' (default)
  prefixes each field with the lowercased message name.
  'UnprefixedFields' uses bare names (requires @DuplicateRecordFields@).
  -}
  , loRepConfig :: RepConfig
  {- ^ Controls how proto field types map to Haskell types. See
  "Proto.Repr" for the adapter system and per-field overrides.
  -}
  , loTHHooks :: THHooks
  {- ^ Callbacks that produce extra TH declarations. See
  'Proto.CodeGen.Hooks.THHooks'.
  -}
  }


{- | Sensible defaults: search @proto\/@ and @.@, prefixed field names,
default representations, no hooks.
-}
defaultLoadOpts :: LoadOpts
defaultLoadOpts =
  LoadOpts
    { loIncludeDirs = ["proto/", "."]
    , loFieldNaming = PrefixedFields
    , loRepConfig = defaultRepConfig
    , loTHHooks = defaultTHHooks
    }


{- | Load a @.proto@ file and splice generated Haskell declarations
using 'defaultLoadOpts'. This is the simplest entry point:

@
\$(loadProto \"path\/to\/message.proto\")
@
-}
loadProto :: FilePath -> Q [Dec]
loadProto = loadProtoWith defaultLoadOpts


{- | Like 'loadProto' but with explicit 'LoadOpts' for controlling
field naming, representations, hooks, and include paths.
-}
loadProtoWith :: LoadOpts -> FilePath -> Q [Dec]
loadProtoWith opts = loadProtoWithTypeNames opts Map.empty


{- | Load a schema with explicit Haskell names for imported types. Keys are
fully qualified protobuf names, for example @example.Color@. Use this when
multiple imported schemas generate the same Haskell name or when the type is
in scope only under a custom module alias.
-}
loadProtoWithTypeNames :: LoadOpts -> Map Text Name -> FilePath -> Q [Dec]
loadProtoWithTypeNames opts typeNames path = do
  addDependentFile path
  contents <- runIO (TIO.readFile path)
  case parseProtoFile path contents of
    Left err -> fail (renderParseError err)
    Right pf -> do
      let hooks = loTHHooks opts
          customOpts =
            foldl
              (flip registerCustomOption)
              emptyCustomOptionRegistry
              (extractExtensionOptions pf)
          fileCtx =
            FileHookCtx
              { fhcProtoFile = pf
              , fhcModuleName = T.pack path
              , fhcFileOptions = protoOptions pf
              , fhcCustomOptions = customOpts
              }
      imports <- resolveImportedFiles (loIncludeDirs opts) pf
      decls <- protoFileToDeclsScoped path (loFieldNaming opts) (loRepConfig opts) hooks typeNames imports pf
      hookDecls <- thOnFile hooks fileCtx
      pure (decls <> hookDecls)


{- | Generate declarations for all top-level definitions in a parsed
'ProtoFile', using default options.
-}
protoFileToDecls :: ProtoFile -> Q [Dec]
protoFileToDecls = protoFileToDecls' PrefixedFields defaultRepConfig defaultTHHooks


protoFileToDecls' :: FieldNaming -> RepConfig -> THHooks -> ProtoFile -> Q [Dec]
protoFileToDecls' naming cfg hooks = protoFileToDeclsScoped "" naming cfg hooks Map.empty []


-- | Generate declarations with the source files of the schema's imports.
protoFileToDeclsScoped
  :: FilePath -> FieldNaming -> RepConfig -> THHooks
  -> Map Text Name -> [(FilePath, ProtoFile)] -> ProtoFile -> Q [Dec]
protoFileToDeclsScoped path naming cfg hooks typeNames imports sourceFile = do
  pf <- either (fail . T.unpack) pure (resolveFieldPresence sourceFile)
  verifiedStrings <- either (fail . T.unpack) pure (verifiedStringExtensions pf)
  let importFiles = Map.fromList (map (\(importName, imported) -> (T.pack importName, imported)) imports)
  types <- either (fail . T.unpack) pure $
    restrictOptionImportTypes importFiles pf (thTypeRegistry ((path, pf) : imports))
  -- Share one descriptor per file, with a distinct binding across file splices.
  descriptorName <- case mapMaybe topMessage (protoTopLevels pf) of
    first : _ -> Just <$> newName ("wireformFileDescriptor'" <> T.unpack (scopedHsTypeName [] (msgName first)))
    [] -> pure Nothing
  let scope =
        ScopeCtx
          { scSyntax = protoSyntax pf
          , scFileOptions = protoOptions pf
          , scFileDescriptorName = descriptorName
          , scTypes = types
          , scLocalTypes = Map.keysSet (descriptorTypeDefinitions pf)
          , scTypeNames = typeNameIndex types
          , scImportedTypeNames = Map.mapKeys (T.dropWhile (== '.')) typeNames
          , scPackage = Data.Maybe.fromMaybe T.empty (protoPackage pf)
          , scParents = []
          , scFieldNaming = naming
          , scRecordNames = recordBindingNames naming (pure . scopedTypeName) (protoTopLevels pf)
          , scOneofNames = oneofBindingNames (protoTopLevels pf)
          , scExtensionNames = extensionBindingNames naming (protoPackage pf) (protoTopLevels pf)
          , scVerifiedStringExtensions = verifiedStrings
          , scRegistryMessages = registryMessageNames types
          }
  decls <- concat <$> mapM (topLevelToDecls scope cfg hooks) (protoTopLevels pf)
  case descriptorName of
    Nothing -> pure decls
    Just name -> do
      let definitions = Map.restrictKeys
            (Map.unions (builtinDescriptorTypes : map (descriptorTypeDefinitions . snd) imports))
            (Map.keysSet types)
      descriptor <- either (fail . T.unpack) pure (serializeFileDescriptorWithImports definitions path importFiles sourceFile)
      bytes <- lift descriptor
      pure (SigD name (ConT ''ByteString) : ValD (VarP name) (NormalB bytes) [] : decls)
  where
    topMessage (TLMessage msg) = Just msg
    topMessage _ = Nothing


{- | Transitively resolve a parsed file's @import@s against the
'loIncludeDirs', returning the parsed files so the generator can see
cross-file types and their packages. Built-in types can be resolved
without their source files. Missing or invalid ordinary imports retain
the legacy fallback. Option imports require their schema so that types
defined there can be excluded from name resolution.
-}
resolveImportedFiles :: [FilePath] -> ProtoFile -> Q [(FilePath, ProtoFile)]
resolveImportedFiles includeDirs pf0 = do
  result <- runIO (go Set.empty [] [] (protoImports pf0))
  (files, deps) <- either fail pure result
  mapM_ addDependentFile deps
  pure files
  where
    go _ accFiles accDeps [] = pure (Right (accFiles, accDeps))
    go visited accFiles accDeps (imp : rest)
      | Set.member path visited = go visited accFiles accDeps rest
      | otherwise = do
          mfp <- findInclude includeDirs (T.unpack path)
          case mfp of
            Nothing -> unavailable accDeps ("cannot find option import " <> T.unpack path <> " in loIncludeDirs")
            Just fp -> do
              src <- TIO.readFile fp
              case parseProtoFile fp src of
                Left err -> unavailable (fp : accDeps) (renderParseError err)
                Right ipf ->
                  go
                    (Set.insert path visited)
                    (accFiles <> [(T.unpack path, ipf)])
                    (fp : accDeps)
                    (protoImports ipf <> rest)
      where
        path = importPath imp
        unavailable deps message
          | importModifier imp == Just ImportOption = pure (Left message)
          | otherwise = go visited accFiles deps rest


-- | First existing @dir '</>' rel@ across the include dirs, if any.
findInclude :: [FilePath] -> FilePath -> IO (Maybe FilePath)
findInclude dirs rel = go dirs
  where
    go [] = pure Nothing
    go (d : ds) = do
      let p = d </> rel
      ok <- doesFileExist p
      if ok then pure (Just p) else go ds


{- | Lookup table built once per file: lets the bridge tell whether
a named-type reference points at an enum (which the bridge
encodes as a varint via 'PFEnum') or a message (encoded as a
length-delimited submessage). Without this, every named type
got encoded as a submessage and top-level proto enums silently
broke on the wire.
-}
data ScopeCtx = ScopeCtx
  { scSyntax :: !Syntax
  , scFileOptions :: ![OptionDef]
  , scFileDescriptorName :: !(Maybe Name)
  , scTypes :: !TypeRegistry
  , scLocalTypes :: !(Set.Set Text)
  , scTypeNames :: !(Map Text [Text])
  , scImportedTypeNames :: !(Map Text Name)
  , scPackage :: !Text
  {- ^ Proto package as declared in the file (empty string when
  the file has no @package@ statement). Drives the
  @protoMessageName@ \/ @protoPackageName@ outputs in the
  generated 'PS.ProtoMessage' instance.
  -}
  , scParents :: ![Text]
  {- ^ Parent message names accumulated as we recurse into
  nested types. Used to scope-prefix the generated Haskell
  type / constructor / field names so two messages from
  different .proto files (or different parents within the
  same file) can declare an inner @NestedMessage@ without
  colliding at the Haskell level.
  -}
  , scFieldNaming :: !FieldNaming
  {- ^ How to name record fields (prefixed with message name
  or bare).
  -}
  , scExtensionNames :: !(Map Text ExtensionNames)
  , scRecordNames :: !RecordBindingNames
  , scOneofNames :: !OneofBindingNames
  , scVerifiedStringExtensions :: !(Map Text [Int])
  , scRegistryMessages :: !(Set.Set Text)
  }


thTypeRegistry :: [(FilePath, ProtoFile)] -> TypeRegistry
thTypeRegistry = buildTypeRegistry defaultGenerateOpts . map resolved
  where
    resolved (path, file) = (path, ResolvedProto file path Map.empty)


lookupScopedType :: ScopeCtx -> Text -> Maybe (Text, TypeInfo)
lookupScopedType scope name = do
  let registry = scTypes scope
      pkg = scPackage scope
  fqn <- resolveTypeKey registry (if T.null pkg then Nothing else Just pkg) (scParents scope) name
  info <- Map.lookup fqn registry
  pure (fqn, info)


resolveHsTypeName :: ScopeCtx -> Text -> Q Name
resolveHsTypeName scope name = case lookupScopedType scope name of
  Nothing
    | any (`Set.member` scLocalTypes scope) fallbackTypes ->
        fail ("Proto.TH: cannot resolve " <> T.unpack name
          <> " without shadowing a local type. Add the defining schema's import root to loIncludeDirs.")
    | otherwise -> pure (mkName (T.unpack fallback))
  Just (fqn, info)
    | Set.member fqn (scLocalTypes scope) -> pure (mkName (T.unpack (tiHsName info)))
    | Just explicit <- Map.lookup fqn (scImportedTypeNames scope) -> pure explicit
    | Just (wkt, _) <- lookupWkt fqn -> pure wkt
    | otherwise -> do
        let hsName = tiHsName info
            qualified = tiModule info <> "." <> hsName
            names = Map.findWithDefault [] hsName (scTypeNames scope)
            others = filter (/= fqn) names
            localCollision = any (`Set.member` scLocalTypes scope) others
            importedCollision = any
              (\other -> Set.notMember other (scLocalTypes scope) && Map.notMember other builtinDescriptorTypes)
              others
            ambiguous = fail ("Proto.TH: cannot identify imported type " <> T.unpack fqn
              <> ". Supply its qualified Haskell name with loadProtoWithTypeNames.")
        qualifiedName <- lookupTypeName (T.unpack qualified)
        case qualifiedName of
          Just resolved -> pure resolved
          Nothing
            | importedCollision -> ambiguous
            | otherwise -> do
                unqualified <- lookupTypeName (T.unpack hsName)
                case unqualified of
                  Just resolved -> pure resolved
                  Nothing
                    | localCollision -> ambiguous
                    | otherwise -> pure (mkName (T.unpack hsName))
  where
    fallback = hsTypeName (lastProtoSegment name)
    fallbackTypes = Map.findWithDefault [] fallback (scTypeNames scope)


typeNameIndex :: TypeRegistry -> Map Text [Text]
typeNameIndex =
  Map.foldrWithKey (\fqn info -> Map.insertWith (<>) (tiHsName info) [fqn]) Map.empty


isEnumName :: ScopeCtx -> Text -> Bool
isEnumName scope = maybe False ((== TKEnum) . tiKind . snd) . lookupScopedType scope


topLevelToDecls :: ScopeCtx -> RepConfig -> THHooks -> TopLevel -> Q [Dec]
topLevelToDecls scope cfg hooks = \case
  TLMessage msg -> messageToDecls'' scope cfg hooks msg
  TLEnum ed -> enumToDeclsScoped scope hooks ed
  TLExtend owner fields -> extendToDecls scope owner fields
  _ -> pure []


-- | Generate record type and wire codec instances for a single message definition, using default options.
messageToDecls :: MessageDef -> Q [Dec]
messageToDecls = messageToDecls' defaultRepConfig defaultTHHooks


{- | Backwards-compatible entry point: builds a 'ScopeCtx' that
contains only this one message (so cross-message enum lookups
fall back to PFSubmessage). Prefer 'protoFileToDecls'' (which
builds the scope from the whole file) for new call sites.
-}
messageToDecls' :: RepConfig -> THHooks -> MessageDef -> Q [Dec]
messageToDecls' cfg hooks msg =
  let file = ProtoFile Proto3 Nothing [] [] [TLMessage msg] Nothing
      types = thTypeRegistry [("", file)]
      scope = ScopeCtx
          { scSyntax = Proto3
          , scFileOptions = []
          , scFileDescriptorName = Nothing
          , scTypes = types
          , scLocalTypes = Map.keysSet (descriptorTypeDefinitions file)
          , scTypeNames = typeNameIndex types
          , scImportedTypeNames = Map.empty
          , scPackage = T.empty
          , scParents = []
          , scFieldNaming = PrefixedFields
          , scRecordNames = recordBindingNames PrefixedFields (pure . scopedTypeName) [TLMessage msg]
          , scOneofNames = oneofBindingNames [TLMessage msg]
          , scExtensionNames = extensionBindingNames PrefixedFields Nothing [TLMessage msg]
          , scVerifiedStringExtensions = Map.empty
          , scRegistryMessages = registryMessageNames types
          }
  in messageToDecls'' scope cfg hooks msg


messageToDecls'' :: ScopeCtx -> RepConfig -> THHooks -> MessageDef -> Q [Dec]
messageToDecls'' scopeCtx cfg hooks msg = do
  let
    -- Scope-prefixed Haskell type name. For top-level
    -- messages 'scParents' is empty so this collapses to the
    -- plain 'hsTypeName'; for nested ones it produces e.g.
    -- @TestAllTypesProto3'NestedMessage@, matching the
    -- pure-text codegen in 'Proto.CodeGen' and avoiding
    -- collisions when two parent messages declare an inner
    -- @NestedMessage@.
    hsTy = scopedHsTypeName (scParents scopeCtx) (msgName msg)
    tyName = mkName (T.unpack hsTy)
    fields = extractMessageFields cfg hsTy (msgElements msg)
    scope = [msgName msg]
    hookCtx =
      MessageHookCtx
        { mhcMessageDef = msg
        , mhcScope = scope
        , mhcHsTypeName = hsTy
        , mhcFqProtoName = msgName msg
        , mhcOptions = messageOptions msg
        }
    -- Fields and nested declarations both resolve names inside this message.
    childScope = scopeCtx {scParents = scParents scopeCtx <> [msgName msg]}
    fqName = T.intercalate "." (filter (not . T.null) (scPackage scopeCtx : scParents childScope))

  nestedDecls <-
    concat
      <$> mapM
        ( \case
            MEMessage inner -> messageToDecls'' childScope cfg hooks inner
            MEEnum ed -> enumToDeclsScoped childScope hooks ed
            MEExtend owner fields -> extendToDecls childScope owner fields
            _ -> pure []
        )
        (msgElements msg)

  -- Sum types backing each oneof. Must precede the message data
  -- declaration so that GHC's renamer sees the constructor names
  -- in scope when the wire codecs splice (the codec splice
  -- references @ovConstructor@ at the term level).
  oneofDecs <- mkOneofDataDecs childScope fields
  dataDec <- mkDataDec childScope tyName fields
  defaultDec <- mkDefaultDec childScope tyName fields
  -- All wire codecs (MessageEncode / MessageSize / MessageDecode)
  -- now come from 'Proto.Internal.Derive' via the IDL bridge,
  -- including oneofs (whose sum types are emitted by
  -- 'mkOneofDataDecs' just above). The bridge handles every
  -- 'FieldSpec' shape; if 'fieldSpecToProtoField' reports an
  -- impossible map key (which the parser shouldn't accept) the
  -- splice fails with a clear message rather than silently
  -- generating broken code.
  pfs <- traverse (fieldSpecToProtoField childScope) fields
  codecDecs <- messageCodecsViaBridge childScope
    (Map.findWithDefault [] fqName (scVerifiedStringExtensions scopeCtx)) tyName pfs
  hasExtDec <- mkHasExtensionsInstance childScope tyName
  hookDecls <- thOnMessage hooks hookCtx

  let defName = mkName ("default" <> nameBase tyName)
      metaFields = fmap (fieldSpecToMetaField childScope) fields
      known = Data.Maybe.isJust . lookupScopedType childScope
  canonicalAny <- usesCanonicalAnyJSON fqName fields
  oneofMetadata <- case concatMap (oneofDescriptorE childScope) fields of
    [] -> pure Nothing
    oneofs -> Just . ListE <$> sequence oneofs
  enumMetadata <- case mapMaybe (closedEnumFieldE childScope) fields of
    [] -> pure Nothing
    enums -> Just <$> [|Map.mapMaybe id (Map.fromList $(ListE <$> sequence enums))|]
  childMetadata <- case concatMap (submessageEntriesE childScope) fields of
    [] -> pure Nothing
    children -> Just <$> [|Map.fromList $(ListE <$> sequence children)|]
  mapMetadata <- case mapMaybe (mapValueDecoderE childScope) fields of
    [] -> pure Nothing
    validators -> Just <$> [|Map.fromList $(ListE <$> sequence validators)|]
  jsonNormalize <-
    if canonicalAny
      then Just <$> [|WK.normalizeAnyEnumJSON WK.anyJsonRegistry|]
      else case filter (known . njfType) (namedJSONFields (msgElements msg)) of
        [] -> pure Nothing
        named -> Just <$> [|PJ.normalizeEnumJSONFields $(ListE <$> traverse (namedJSONRule childScope) named)|]
  protoMsgDecs <-
    PTM.mkProtoMessageInstance
      tyName
      fqName
      (scPackage scopeCtx)
      defName
      (scFileDescriptorName scopeCtx)
      metaFields
      oneofMetadata
      enumMetadata
      childMetadata
      mapMetadata
      jsonNormalize
  -- The unknown-fields selector name (always present on
  -- TH-generated messages) — passing it through lets the JSON
  -- splice patch in proto2 extension entries via the runtime
  -- registry. We use the same naming convention as
  -- 'unknownFieldsName'.
  let ufSelName = unknownFieldsName childScope tyName
      hasExts = any isExt (msgElements msg)
        where
          isExt (MEExtensions _ _) = True
          isExt _ = False
      aesonUfSel = if hasExts then Just ufSelName else Nothing
      registryContext =
        if Set.member fqName (scRegistryMessages scopeCtx)
          then [AppT (ConT ''Reflection.Given) (ConT ''PJExt.ExtensionRegistry)]
          else []
  let recDot = scFieldNaming scopeCtx == UnprefixedFields
  aesonDecs <-
    if canonicalAny
      then PTM.mkAnyAesonInstances tyName
      else PTM.mkAesonInstancesForMessage registryContext tyName fqName aesonUfSel defName recDot metaFields
  hashableDec <- PTM.mkHashableInstanceForMessage tyName metaFields

  -- Per-oneof carrier sum: ToJSON / FromJSON / Hashable. The data
  -- declaration was emitted by 'mkOneofDataDecs' just above, so
  -- the constructor names line up with what we splice here.
  oneofSatellites <- fmap concat (mapM (oneofSatelliteDecs childScope) fields)

  addModFinalizer (putDoc (DeclDoc tyName) (messageHaddock msg fields))
  addModFinalizer
    ( putDoc
        (DeclDoc defName)
        ( "Default value for @"
            <> T.unpack (msgName msg)
            <> "@ with all fields at their proto default values."
        )
    )

  -- HasField instances for lens-style access via Proto.Schema.HasField
  hasFieldDecs <- mkHasFieldInstances childScope tyName fields

  -- Monoid: mempty = defaultFoo (must come after Semigroup in codecDecs)
  let monoidDec =
        InstanceD
          Nothing
          []
          (AppT (ConT ''Monoid) (ConT tyName))
          [FunD 'mempty [Clause [] (NormalB (VarE defName)) []]]

  -- IsMessage: marker instance picked up by Proto.Registry's TH
  -- discovery splice. Must come AFTER protoMsgDecs and aesonDecs since
  -- IsMessage requires ProtoMessage / Aeson.ToJSON / Aeson.FromJSON as
  -- superclasses.
  let ismDec = PDI.mkIsMessageInstance registryContext (ConT tyName)

  pure
    ( nestedDecls
        <> oneofDecs
        <> [dataDec]
        <> defaultDec
        <> codecDecs
        <> [monoidDec]
        <> hasFieldDecs
        <> hasExtDec
        <> protoMsgDecs
        <> aesonDecs
        <> [hashableDec]
        <> [ismDec]
        <> oneofSatellites
        <> hookDecls
    )


-- Use the same canonical JSON shape as standalone codegen when splicing
-- the well-known schema itself. Custom field representations still use
-- the general record JSON emitter.
usesCanonicalAnyJSON :: Text -> [FieldSpec] -> Q Bool
usesCanonicalAnyJSON "google.protobuf.Any"
  [FSField "type_url" 1 Nothing (FTScalar SString) urlRep _ _, FSField "value" 2 Nothing (FTScalar SBytes) valueRep _ _] =
    (&&)
      <$> ((== ConT ''Text) <$> stringType (fieldString urlRep))
      <*> ((== ConT ''ByteString) <$> bytesType (fieldBytes valueRep))
usesCanonicalAnyJSON _ _ = pure False


namedJSONRule :: ScopeCtx -> NamedJSONField -> Q Exp
namedJSONRule scope field = do
  ty <- fieldTypeInnerScopedQ scope defaultFieldRep (FTNamed (njfType field))
  let proxy = pure (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) ty))
      clean
        | isEnumName scope (njfType field) = [|PJ.normalizeEnumJSONValue $proxy|]
        | otherwise = [|Just . PS.protoJSONIgnoreUnknownEnumNames $proxy|]
      transform = case njfShape field of
        NamedJSONSingle -> clean
        NamedJSONRepeated -> [|PJ.normalizeEnumJSONArray $clean|]
        NamedJSONMap -> [|PJ.normalizeEnumJSONMap $clean|]
  [|PJ.EnumJSONField $(textLitE (njfJsonName field)) $(textLitE (njfProtoName field)) $transform|]


{- | Synthesise the @MessageEncode \/ MessageSize \/ MessageDecode@
triple via 'Proto.Internal.Derive.synthesiseProtoInstancesWith'
with unknown-field preservation enabled. Used for every
'loadProto'-generated message.
-}
messageCodecsViaBridge :: ScopeCtx -> [Int] -> Name -> [PDI.ProtoField] -> Q [Dec]
messageCodecsViaBridge scope verifiedStrings tyName pfs = do
  let meta =
        PDI.MessageMeta
          { PDI.mmUnknownFieldsSel = Just (unknownFieldsName scope tyName)
          , PDI.mmRecordDotReads = scFieldNaming scope == UnprefixedFields
          }
  enc <- PDI.mkEncodeInstanceWith meta (ConT tyName) pfs
  siz <- PDI.mkSizeInstanceWith meta (ConT tyName) pfs
  dec <- PDI.mkDecodeInstanceWithVerifiedStrings verifiedStrings meta (ConT tyName) tyName pfs
  semi <- PDI.mkSemigroupInstanceWith meta (ConT tyName) tyName pfs
  pure [enc, siz, dec, semi]


messageHaddock :: MessageDef -> [FieldSpec] -> String
messageHaddock msg fields =
  "Protobuf message @"
    <> T.unpack (msgName msg)
    <> "@.\n\n"
    <> "Fields:\n\n"
    <> concatMap fieldHaddock fields


fieldHaddock :: FieldSpec -> String
fieldHaddock (FSField name num lbl ft _ _ _) =
  "* @"
    <> T.unpack name
    <> "@ ("
    <> labelStr lbl
    <> fieldTypeStr ft
    <> ", field "
    <> show num
    <> ")\n"
fieldHaddock (FSMap name num kt vt _ _) =
  "* @"
    <> T.unpack name
    <> "@ (map<"
    <> scalarStr kt
    <> ", "
    <> fieldTypeStr vt
    <> ">, field "
    <> show num
    <> ")\n"
fieldHaddock (FSOneof name ofs) =
  "* @"
    <> T.unpack name
    <> "@ (oneof, "
    <> show (length ofs)
    <> " variants)\n"


labelStr :: Maybe FieldLabel -> String
labelStr Nothing = ""
labelStr (Just Optional) = "optional "
labelStr (Just Required) = "required "
labelStr (Just Repeated) = "repeated "


fieldTypeStr :: FieldType -> String
fieldTypeStr (FTScalar s) = scalarStr s
fieldTypeStr (FTNamed n) = T.unpack n


scalarStr :: ScalarType -> String
scalarStr SDouble = "double"
scalarStr SFloat = "float"
scalarStr SInt32 = "int32"
scalarStr SInt64 = "int64"
scalarStr SUInt32 = "uint32"
scalarStr SUInt64 = "uint64"
scalarStr SSInt32 = "sint32"
scalarStr SSInt64 = "sint64"
scalarStr SFixed32 = "fixed32"
scalarStr SFixed64 = "fixed64"
scalarStr SSFixed32 = "sfixed32"
scalarStr SSFixed64 = "sfixed64"
scalarStr SBool = "bool"
scalarStr SString = "string"
scalarStr SBytes = "bytes"


-- A resolved field spec carrying the concrete representation choices.
data FieldSpec
  = FSField
      { fsName :: Text
      , fsNum :: Int
      , fsLabel :: Maybe FieldLabel
      , fsType :: FieldType
      , fsRep :: FieldRep
      , fsOptions :: [OptionDef]
      , fsLegacyGroup :: Bool
      }
  | FSMap
      { fsName :: Text
      , fsNum :: Int
      , fsMapKey :: ScalarType
      , fsMapVal :: FieldType
      , fsMapRep :: FieldRep
      {- ^ Resolved per-field 'FieldRep'. Drives the bytes / string
      rep of the value type and the JSON helper choice, the
      same way 'fsRep' does for 'FSField'.
      -}
      , fsOptions :: [OptionDef]
      }
  | FSOneof
      { fsName :: Text
      , fsOneofFields :: [(OneofField, FieldRep)]
      {- ^ Each variant paired with its resolved 'FieldRep'.
      The string / bytes / repeated rep choices come from the
      same 'RepConfig' lookup machinery as regular fields, keyed
      by @(parentMessage, oneofFieldName)@. This lets users
      override one variant of a oneof to lazy / short / hsString
      without affecting siblings.
      -}
      }


fsFieldName :: FieldSpec -> Text
fsFieldName (FSField n _ _ _ _ _ _) = n
fsFieldName (FSMap n _ _ _ _ _) = n
fsFieldName (FSOneof n _) = n


extractMessageFields :: RepConfig -> Text -> [MessageElement] -> [FieldSpec]
extractMessageFields cfg msgN = concatMap go
  where
    go (MEField fd) =
      [ FSField
          { fsName = fieldName fd
          , fsNum = unFieldNumber (fieldNumber fd)
          , fsLabel = fieldLabel fd
          , fsType = fieldType fd
          , fsLegacyGroup = isLegacyGroupField fd
          , fsRep =
              ( if configUnboxedRepeated cfg
                  then autoUnboxRepeated (fieldLabel fd) (fieldType fd)
                  else id
              )
                $ wireformFieldOverrides
                  (configAdapterRegistry cfg)
                  (fieldOptions fd)
                  (lookupFieldRep msgN (fieldName fd) cfg)
          , fsOptions = fieldOptions fd
          }
      ]
    go (MEMapField mf) =
      [ FSMap
          { fsName = mapFieldName mf
          , fsNum = unFieldNumber (mapFieldNum mf)
          , fsMapKey = mapKeyType mf
          , fsMapVal = mapValueType mf
          , fsMapRep =
              wireformFieldOverrides
                (configAdapterRegistry cfg)
                (mapOptions mf)
                (lookupFieldRep msgN (mapFieldName mf) cfg)
          , fsOptions = mapOptions mf
          }
      ]
    go (MEOneof od) =
      [ FSOneof
          { fsName = oneofName od
          , fsOneofFields =
              fmap
                ( \f ->
                    ( f
                    , wireformFieldOverrides
                        (configAdapterRegistry cfg)
                        (oneofFieldOptions f)
                        (lookupFieldRep msgN (oneofFieldName f) cfg)
                    )
                )
                (oneofFields od)
          }
      ]
    go _ = []


-- Data type generation: uses fsRep to pick the Haskell type.

mkDataDec :: ScopeCtx -> Name -> [FieldSpec] -> Q Dec
mkDataDec scope tyName fields = do
  recFields <- fmap concat (mapM mkField fields)
  let unknownFieldEntry = mkUnknownFieldsField scope tyName
  let con = recC tyName (fmap pure (recFields <> [unknownFieldEntry]))
  dataD
    (pure [])
    tyName
    []
    Nothing
    [con]
    [ derivClause (Just StockStrategy) [conT ''Show, conT ''Eq, conT ''Generic]
    , derivClause (Just AnyclassStrategy) [conT ''NFData]
    ]
  where
    mkField :: FieldSpec -> Q [VarBangType]
    mkField (FSField name _ lbl ft rep _ _) = do
      let fname = thFieldName scope name
      ty <- fieldTypeToTH scope lbl ft rep
      pure [(fname, Bang NoSourceUnpackedness SourceStrict, ty)]
    mkField (FSMap name _ kt vt rep _) = do
      let fname = thFieldName scope name
      kty <- scalarToTH kt
      vty <- fieldTypeInnerScopedQ scope rep vt
      t <- appT (appT (conT ''Map) (pure kty)) (pure vty)
      pure [(fname, Bang NoSourceUnpackedness SourceStrict, t)]
    mkField (FSOneof name _ofs) = do
      let fname = thFieldName scope name
          oneofTyName = oneofSumName scope name
      ty <- appT (conT ''Maybe) (conT oneofTyName)
      pure [(fname, Bang NoSourceUnpackedness SourceStrict, ty)]


-- ===========================================================
-- HasField instances (Proto.Schema.HasField)
-- ===========================================================

mkHasFieldInstances :: ScopeCtx -> Name -> [FieldSpec] -> Q [Dec]
mkHasFieldInstances scope tyName fields =
  fmap concat (mapM mkOne fields)
  where
    labels = fieldLabelNames (map fsFieldName fields)
    label name = Map.findWithDefault (snakeToCamel name) name labels
    recDot = scFieldNaming scope == UnprefixedFields
    mkOne :: FieldSpec -> Q [Dec]
    mkOne (FSField name _ lbl ft rep _ _) = do
      let fname = thFieldName scope name
      ty <- fieldTypeToTH scope lbl ft rep
      mkHasFieldDec recDot tyName (label name) fname ty
    mkOne (FSMap name _ kt vt rep _) = do
      let fname = thFieldName scope name
      kty <- scalarToTH kt
      vty <- fieldTypeInnerScopedQ scope rep vt
      ty <- appT (appT (conT ''Map) (pure kty)) (pure vty)
      mkHasFieldDec recDot tyName (label name) fname ty
    mkOne (FSOneof name _) = do
      let fname = thFieldName scope name
          oneofTyName = oneofSumName scope name
      ty <- appT (conT ''Maybe) (conT oneofTyName)
      mkHasFieldDec recDot tyName (label name) fname ty


mkHasFieldDec :: Bool -> Name -> Text -> Name -> Type -> Q [Dec]
mkHasFieldDec recDot tyName fnameStr fname ty = do
  msgVar <- newName "msg"
  aVar <- newName "a"
  let nameLit = LitT (StrTyLit (T.unpack fnameStr))
      instTy = foldl AppT (ConT ''PS.HasField) [ConT tyName, nameLit, ty]
      -- Under 'UnprefixedFields' ('NoFieldSelectors', names shared across
      -- messages) the bare selector does not exist and an un-annotated
      -- update is ambiguous: read through the auto-derived record-dot
      -- (GHC 'HasField', distinct from this 'PS.HasField') on a typed
      -- binder, and annotate the update's result. Prefixed keeps the
      -- plain forms.
      msgBinder
        | recDot = SigP (VarP msgVar) (ConT tyName)
        | otherwise = VarP msgVar
      getBody
        | recDot = GetFieldE (VarE msgVar) (nameBase fname)
        | otherwise = AppE (VarE fname) (VarE msgVar)
      updE = RecUpdE (VarE msgVar) [(fname, VarE aVar)]
      setBody
        | recDot = SigE updE (ConT tyName)
        | otherwise = updE
      getFld = FunD 'PS.getField [Clause [msgBinder] (NormalB getBody) []]
      setFld = FunD 'PS.setField [Clause [VarP aVar, msgBinder] (NormalB setBody) []]
      fdesc =
        FunD
          'PS.fieldDescriptor
          [Clause [WildP, WildP] (NormalB (VarE 'undefined)) []]
  pure [InstanceD Nothing [] instTy [getFld, setFld, fdesc]]


-- ===========================================================
-- Oneof sum types
-- ===========================================================
--
-- Every @oneof@ in a message materialises as a sum type whose
-- constructors carry the variant payload. Names follow the
-- convention 'Proto.CodeGen' uses for its pure-text output:
--
-- > <ParentMessage>'<OneofName>           -- the sum type
-- > <ParentMessage>'<OneofName>'<Variant> -- one constructor each
--
-- Scoping by the parent type prevents collisions between two
-- messages that share a oneof name (e.g. @oneof choice@ in two
-- different messages).

-- | Sum type 'Name' for one oneof.
oneofSumName :: ScopeCtx -> Text -> Name
oneofSumName scope = mkName . T.unpack . oneofTypeName (scOneofNames scope) (scParents scope)


-- | Constructor 'Name' for one variant of a oneof's sum type.
oneofConTHName :: ScopeCtx -> Text -> Text -> Name
oneofConTHName scope oneof =
  mkName . T.unpack . oneofConstructorName (scOneofNames scope) (scParents scope) oneof


{- | Emit the sum data declaration for every 'FSOneof' field on the
supplied message.
-}
mkOneofDataDecs :: ScopeCtx -> [FieldSpec] -> Q [Dec]
mkOneofDataDecs scope fields =
  mapM oneofToDec (mapMaybe extractOneof fields)
  where
    extractOneof (FSOneof n ofs) = Just (n, ofs)
    extractOneof _ = Nothing

    oneofToDec :: (Text, [(OneofField, FieldRep)]) -> Q Dec
    oneofToDec (ooName, ofs) = do
      let tyName = oneofSumName scope ooName
      cons <- mapM (mkCon ooName) ofs
      dataD
        (pure [])
        tyName
        []
        Nothing
        (fmap pure cons)
        [ derivClause (Just StockStrategy) [conT ''Show, conT ''Eq, conT ''Generic]
        , derivClause (Just AnyclassStrategy) [conT ''NFData]
        ]

    mkCon :: Text -> (OneofField, FieldRep) -> Q Con
    mkCon ooName (f, rep) = do
      let conName = oneofConTHName scope ooName (oneofFieldName f)
      ty <- fieldTypeInnerScopedQ scope rep (oneofFieldType f)
      pure (NormalC conName [(Bang NoSourceUnpackedness SourceStrict, ty)])


{- | The field name used by TH-generated records for their
unknown-fields list. Derived from the type name by
lower-casing the leading character and appending
@UnknownFields@ (matching the convention the pure-Doc
codegen in "Proto.CodeGen" follows).
-}
unknownFieldsName :: ScopeCtx -> Name -> Name
unknownFieldsName scope tyName =
  mkName . T.unpack $
    Map.findWithDefault fallback (scParents scope) (recordUnknownNames (scRecordNames scope))
  where
    fallback = T.pack (lowerFirstStr (nameBase tyName)) <> "UnknownFields"


lowerFirstStr :: String -> String
lowerFirstStr (c : rest) = Data.Char.toLower c : rest
lowerFirstStr [] = []


-- | The VarBangType entry for a TH record's unknown-fields slot.
mkUnknownFieldsField :: ScopeCtx -> Name -> VarBangType
mkUnknownFieldsField scope tyName =
  ( unknownFieldsName scope tyName
  , Bang NoSourceUnpackedness SourceStrict
  , AppT ListT (ConT ''Decode.UnknownField)
  )


fieldTypeToTH :: ScopeCtx -> Maybe FieldLabel -> FieldType -> FieldRep -> Q Type
fieldTypeToTH scope lbl ft rep = case lbl of
  Just Repeated -> repeatedTypeQ (fieldRepeated rep) (fieldTypeInnerScopedQ scope rep ft)
  _ | explicitFieldPresence lbl -> appT (conT ''Maybe) (fieldTypeInnerScopedQ scope rep ft)
  _ -> case ft of
    -- Singular submessage fields are implicitly optional in proto3
    -- (the sender can omit them, and consumers must distinguish
    -- "absent" from "default" via the carrier). We model that with
    -- Maybe so the data declaration matches what the bridge's
    -- decoder produces. Singular enum fields stay bare, since
    -- enums always have a zero value (the spec mandates a 0-valued
    -- variant).
    FTNamed n
      | not (isEnumName scope n) ->
          appT (conT ''Maybe) (fieldTypeInnerScopedQ scope rep ft)
    _ -> fieldTypeInnerScopedQ scope rep ft


{- | Scope-aware variant used for normal singular / repeated /
optional / oneof-variant fields. Resolves named types through
the file's 'ScopeCtx' so a reference to a nested message gets
the parent-prefixed Haskell type (e.g.
@TestAllTypesProto3'NestedMessage@).
-}
fieldTypeInnerScopedQ :: ScopeCtx -> FieldRep -> FieldType -> Q Type
fieldTypeInnerScopedQ scope rep = \case
  FTScalar SString -> stringTypeQ (fieldString rep)
  FTScalar SBytes -> bytesTypeQ (fieldBytes rep)
  FTScalar s -> scalarToTH s
  FTNamed n -> conT =<< resolveHsTypeName scope n


{- | Well-Known-Type registry. Maps a fully-qualified proto name
(@google.protobuf.Timestamp@) to the splice 'Name' of the
pre-generated Haskell type plus the splice 'Name' of its
@default<TypeName>@ value. Routes imported well-known types to
their existing generated modules.

The returned 'Name's are fully-resolved (produced by the @''Type@ \/
@'value@ name quotes against the imported @Wkt*@ modules above), so the
generated code references the WKT type by its global name and does /not/
require the consumer to import the corresponding
@Proto.Google.Protobuf.*@ module at the @loadProto@ call site. Adding a
WKT is a one-line entry here plus the matching qualified import above.
-}
lookupWkt :: Text -> Maybe (Name, Name)
lookupWkt n = case T.dropWhile (== '.') n of
  -- Single-message WKTs.
  "google.protobuf.Timestamp" -> Just (''WktTimestamp.Timestamp, 'WktTimestamp.defaultTimestamp)
  "google.protobuf.Duration" -> Just (''WktDuration.Duration, 'WktDuration.defaultDuration)
  "google.protobuf.Empty" -> Just (''WktEmpty.Empty, 'WktEmpty.defaultEmpty)
  "google.protobuf.FieldMask" -> Just (''WktFieldMask.FieldMask, 'WktFieldMask.defaultFieldMask)
  "google.protobuf.SourceContext" -> Just (''WktSourceContext.SourceContext, 'WktSourceContext.defaultSourceContext)
  "google.protobuf.Any" -> Just (''WktAny.Any, 'WktAny.defaultAny)
  "google.protobuf.Struct" -> Just (''WktStruct.Struct, 'WktStruct.defaultStruct)
  "google.protobuf.Value" -> Just (''WktStruct.Value, 'WktStruct.defaultValue)
  "google.protobuf.ListValue" -> Just (''WktStruct.ListValue, 'WktStruct.defaultListValue)
  "google.protobuf.NullValue" ->
    Just
      ( ''WktStruct.NullValue
      , -- NullValue is an enum; default is its single value.
        'WktStruct.NullValue'NullValue
      )
  -- Wrapper messages (all in Proto.Google.Protobuf.Wrappers).
  "google.protobuf.DoubleValue" -> Just (''WktWrappers.DoubleValue, 'WktWrappers.defaultDoubleValue)
  "google.protobuf.FloatValue" -> Just (''WktWrappers.FloatValue, 'WktWrappers.defaultFloatValue)
  "google.protobuf.Int64Value" -> Just (''WktWrappers.Int64Value, 'WktWrappers.defaultInt64Value)
  "google.protobuf.UInt64Value" -> Just (''WktWrappers.UInt64Value, 'WktWrappers.defaultUInt64Value)
  "google.protobuf.Int32Value" -> Just (''WktWrappers.Int32Value, 'WktWrappers.defaultInt32Value)
  "google.protobuf.UInt32Value" -> Just (''WktWrappers.UInt32Value, 'WktWrappers.defaultUInt32Value)
  "google.protobuf.BoolValue" -> Just (''WktWrappers.BoolValue, 'WktWrappers.defaultBoolValue)
  "google.protobuf.StringValue" -> Just (''WktWrappers.StringValue, 'WktWrappers.defaultStringValue)
  "google.protobuf.BytesValue" -> Just (''WktWrappers.BytesValue, 'WktWrappers.defaultBytesValue)
  _ -> Nothing


stringTypeQ :: StringAdapter -> Q Type
stringTypeQ = stringType


bytesTypeQ :: BytesAdapter -> Q Type
bytesTypeQ = bytesType


repeatedTypeQ :: RepeatedAdapter -> Q Type -> Q Type
repeatedTypeQ = repeatedType


scalarToTH :: ScalarType -> Q Type
scalarToTH = \case
  SDouble -> conT ''Double
  SFloat -> conT ''Float
  SInt32 -> conT ''Int32
  SInt64 -> conT ''Int64
  SUInt32 -> conT ''Word32
  SUInt64 -> conT ''Word64
  SSInt32 -> conT ''Int32
  SSInt64 -> conT ''Int64
  SFixed32 -> conT ''Word32
  SFixed64 -> conT ''Word64
  SSFixed32 -> conT ''Int32
  SSFixed64 -> conT ''Int64
  SBool -> conT ''Bool
  SString -> conT ''Text
  SBytes -> conT ''ByteString


-- Default values depend on the representation.

mkDefaultDec :: ScopeCtx -> Name -> [FieldSpec] -> Q [Dec]
mkDefaultDec scope tyName fields = do
  let defName = mkName ("default" <> nameBase tyName)
  sig <- sigD defName (conT tyName)
  defFields <-
    mapM
      ( \fs -> do
          val <- defaultValueExpr scope fs
          pure (thFieldName scope (fsFieldName fs), val)
      )
      fields
  -- Every TH-generated record now carries an empty unknown-fields
  -- list. Proto2 extensions travel through this field; see
  -- "Proto.Extension" for the typed accessors.
  let ufDefault = (unknownFieldsName scope tyName, ListE [])
  body <-
    valD
      (varP defName)
      (normalB (recConE tyName (fmap pure (defFields <> [ufDefault]))))
      []
  pure [sig, body]


defaultValueExpr :: ScopeCtx -> FieldSpec -> Q Exp
defaultValueExpr scope (FSField _ _ lbl ft rep _ _) = case lbl of
  Just Repeated -> emptyRepeatedQ (fieldRepeated rep)
  _ | explicitFieldPresence lbl -> conE 'Nothing
  _ -> case ft of
    FTScalar SBool -> conE 'False
    FTScalar SString -> emptyStringQ (fieldString rep)
    FTScalar SBytes -> emptyBytesQ (fieldBytes rep)
    FTScalar _ -> litE (integerL 0)
    FTNamed n
      | isEnumName scope n ->
          -- Enums: pick the constructor with @evNumber == 0@ (the
          -- proto-mandated default). When the enum has no zero
          -- value we fall back to @toEnum 0@; the generated Enum
          -- instance's @toEnum@ catch-all returns the first
          -- declared constructor, which is the closest thing to a
          -- spec-compliant fallback.
          [|toEnum 0|]
      | otherwise -> conE 'Nothing
defaultValueExpr _ (FSMap {}) = [|Map.empty|]
defaultValueExpr _ (FSOneof _ _) = conE 'Nothing


emptyRepeatedQ :: RepeatedAdapter -> Q Exp
emptyRepeatedQ = repeatedEmpty


emptyStringQ :: StringAdapter -> Q Exp
emptyStringQ = stringEmpty


emptyBytesQ :: BytesAdapter -> Q Exp
emptyBytesQ = bytesEmpty


-- ---------------------------------------------------------------------------
-- Wire codec generation lives in 'Proto.Internal.Derive'; the
-- bridge in 'fieldSpecToProtoField' below feeds it. The legacy
-- hand-written 'mkEncodeInstance' \/ 'mkDecodeInstance' \/
-- 'mkSizeInstance' family of helpers used to live here; they were
-- removed once the bridge gained complete coverage of every
-- 'FieldSpec' shape (including 'FSOneof', whose carrier sum types
-- are now generated by 'mkOneofDataDecs').
-- ---------------------------------------------------------------------------

-- | Generate sum type, 'Enum' instance, and wire codec instances for a single enum definition, using default hooks.
enumToDecls :: EnumDef -> Q [Dec]
enumToDecls = enumToDecls' defaultTHHooks


enumToDecls' :: THHooks -> EnumDef -> Q [Dec]
enumToDecls' = enumToDecls'' False T.empty []


enumToDeclsScoped :: ScopeCtx -> THHooks -> EnumDef -> Q [Dec]
enumToDeclsScoped scope hooks ed = do
  closed <- either (fail . T.unpack) pure (enumIsClosed (scSyntax scope) (scFileOptions scope) (enumOptions ed))
  enumToDecls'' closed (scPackage scope) (scParents scope) hooks ed


{- | Enum splice with the proto package + parent-message scope
threaded through. Parent scope drives the Haskell type and
constructor names ('TestAllTypesProto3'NestedEnum' /
'TestAllTypesProto3'NestedEnum'Foo') so two parent messages
can declare identically-named inner enums without colliding.
-}
enumToDecls'' :: Bool -> Text -> [Text] -> THHooks -> EnumDef -> Q [Dec]
enumToDecls'' closed pkg parents hooks ed = do
  let
    -- Scoped Haskell type name (collapses to plain
    -- 'hsTypeName' when 'parents' is empty).
    hsTy = scopedHsTypeName parents (enumName ed)
    tyName = mkName (T.unpack hsTy)
    enumScope = parents <> [enumName ed]
    names = enumConstructorNames enumScope ed
    conNameFor evName' =
      mkName (T.unpack (Map.findWithDefault (scopedEnumCon enumScope evName') evName' names))
    -- An enum declared with @option allow_alias = true@ can
    -- repeat wire numbers; only the FIRST occurrence of each
    -- number becomes a distinct Haskell constructor. Aliases
    -- are still recorded in the @ProtoEnum@ name table
    -- ('protoEnumValues') and in the JSON parser, where they
    -- all dispatch to the primary constructor.
    primaryEvs = primaryValues (enumValues ed)
    -- Open-enum representation: every generated enum carries
    -- an extra @<EnumName>'Unknown !Int32@ constructor so the
    -- proto3 spec's "preserve unknown numeric enum values"
    -- contract can be honoured end-to-end (encode, decode,
    -- JSON round-trip).
    unknownCon =
      let int32Ty = pure (ConT ''Int32) :: Q Type
          bang_ = bang sourceNoUnpack sourceStrict
      in normalC unknownConFullName [bangType bang_ int32Ty]
    cons =
      fmap (\ev -> normalC (conNameFor (evName ev)) []) primaryEvs
        <> [unknownCon]
    unknownConFullName = unknownConNameFor parents (enumName ed)
    hookCtx =
      EnumHookCtx
        { ehcEnumDef = ed
        , ehcScope = parents <> [enumName ed]
        , ehcHsTypeName = hsTy
        , ehcOptions = enumOptions ed
        }
  -- Stock-derive Show/Eq/Ord/Generic only; emit a proto-faithful
  -- 'Enum' instance below so 'PFEnum' encoding (varint via
  -- 'fromEnum' \/ 'toEnum') uses the spec-mandated wire numbers
  -- rather than declaration order.
  dataDec <-
    dataD
      (pure [])
      tyName
      []
      Nothing
      cons
      [ derivClause
          (Just StockStrategy)
          [ conT ''Show
          , conT ''Eq
          , conT ''Ord
          , conT ''Generic
          ]
      , derivClause (Just AnyclassStrategy) [conT ''NFData]
      ]
  enumInst <- mkEnumInstance tyName unknownConFullName (conNameFor . evName) primaryEvs
  let
    -- Map every declared enum value (alias or primary) to the
    -- /primary/ constructor for its wire number, so that the
    -- generated 'ProtoEnum' table and JSON parser both accept
    -- alias names but dispatch to a single Haskell constructor.
    primaryConByNum =
      Map.fromList
        [ (evNumber ev, conNameFor (evName ev))
        | ev <- primaryEvs
        ]
    values =
      fmap
        ( \ev ->
            let con = case Map.lookup (evNumber ev) primaryConByNum of
                  Just c -> c
                  -- Defensive — primaryValues guarantees a primary
                  -- exists for every observed number.
                  Nothing -> conNameFor (evName ev)
            in (con, evName ev, evNumber ev)
        )
        (enumValues ed)
    fqEnumName = T.intercalate "." (filter (not . T.null) (pkg : parents <> [enumName ed]))
  protoEnumDec <-
    PTM.mkProtoEnumInstance
      tyName
      fqEnumName
      values
      closed
      unknownConFullName
  aesonDecs <- PTM.mkEnumAesonInstances tyName values unknownConFullName
  hashableDec <- PTM.mkEnumHashableInstance tyName
  hookDecls <- thOnEnum hooks hookCtx

  addModFinalizer $ putDoc (DeclDoc tyName) (enumHaddock ed)

  pure
    ( [dataDec, enumInst, protoEnumDec]
        <> aesonDecs
        <> [hashableDec]
        <> hookDecls
    )


{- | Synthesise a proto-faithful 'Enum' instance for a generated
enum type. @fromEnum@ returns the @evNumber@ recorded in the
@.proto@ file; @toEnum@ inverts it, falling back to the first
declared value for unknown wire numbers (matches the open-enum
behaviour proto3 mandates).
-}
mkEnumInstance :: Name -> Name -> (EnumValue -> Name) -> [EnumValue] -> Q Dec
mkEnumInstance tyName unknownCon enumConName primaryByNum = do
  -- Only the first declared value for each wire number has a constructor.
  let -- 'toEnum n -> ConName' clauses, one per distinct wire
      -- number. Catch-all wraps the input in the synthetic
      -- @<EnumName>'Unknown !Int32@ constructor (open-enum
      -- semantics — preserves an unrecognised wire value
      -- across encode\/decode\/JSON round-trips).
      toEnumClauses =
        fmap
          ( \ev ->
              Clause
                [LitP (IntegerL (fromIntegral (evNumber ev)))]
                (NormalB (ConE (enumConName ev)))
                []
          )
          primaryByNum
      nVar = mkName "n"
      unknownFallback =
        Clause
          [VarP nVar]
          ( NormalB
              ( AppE
                  (ConE unknownCon)
                  ( SigE
                      (AppE (VarE 'fromIntegral) (VarE nVar))
                      (ConT ''Int32)
                  )
              )
          )
          []
      toEnumDec = FunD 'toEnum (toEnumClauses <> [unknownFallback])

      -- 'fromEnum ConName -> n' clauses, one per primary
      -- constructor + a clause for the Unknown wrapper that
      -- returns the carried int.
      fromEnumClauses =
        fmap
          ( \ev ->
              Clause
                [ConP (enumConName ev) [] []]
                ( NormalB
                    ( LitE
                        ( IntegerL
                            (fromIntegral (evNumber ev))
                        )
                    )
                )
                []
          )
          primaryByNum
          <> [ Clause
                 [ConP unknownCon [] [VarP nVar]]
                 (NormalB (AppE (VarE 'fromIntegral) (VarE nVar)))
                 []
             ]
      fromEnumDec = FunD 'fromEnum fromEnumClauses
  pure $
    InstanceD
      Nothing
      []
      (AppT (ConT ''Enum) (ConT tyName))
      [toEnumDec, fromEnumDec]


{- | Drop later occurrences of any wire number from an enum's
value list; preserves the first declaration. Used by both
the data-decl emitter and the 'Enum' instance to collapse
@allow_alias = true@ declarations onto a single Haskell
constructor per number.
-}
primaryValues :: [EnumValue] -> [EnumValue]
primaryValues = go []
  where
    go _ [] = []
    go seen (ev : evs)
      | evNumber ev `elem` seen = go seen evs
      | otherwise = ev : go (evNumber ev : seen) evs


enumHaddock :: EnumDef -> String
enumHaddock ed =
  "Protobuf enum @"
    <> T.unpack (enumName ed)
    <> "@.\n\n"
    <> "Values:\n\n"
    <> concatMap
      ( \ev ->
          "* @" <> T.unpack (evName ev) <> "@ = " <> show (evNumber ev) <> "\n"
      )
      (enumValues ed)


-- ============================================================
-- Proto2 extensions
-- ============================================================

{- | Emit the @HasExtensions@ instance for a generated record. The
instance's two methods read and write the record's unknown-fields
slot, which is where extension payloads (and any unrecognised tags)
are preserved.
-}
mkHasExtensionsInstance :: ScopeCtx -> Name -> Q [Dec]
mkHasExtensionsInstance scope tyName = do
  let ufName = unknownFieldsName scope tyName
      recDot = scFieldNaming scope == UnprefixedFields
  msgVar <- newName "msg"
  ufsVar <- newName "ufs"
  -- Under 'UnprefixedFields' ('NoFieldSelectors') the bare unknown-fields
  -- selector does not exist, so read it through record-dot on a typed
  -- binder. The selector is unique (always message-prefixed), so the
  -- 'setMessageUnknownFields' record update needs no annotation.
  ufReader <-
    if recDot
      then do
        m <- newName "m"
        pure (LamE [SigP (VarP m) (ConT tyName)] (GetFieldE (VarE m) (nameBase ufName)))
      else pure (VarE ufName)
  inst <-
    instanceD
      (pure [])
      [t|Ext.HasExtensions $(conT tyName)|]
      [ funD
          'Ext.messageUnknownFields
          [clause [] (normalB (pure ufReader)) []]
      , funD
          'Ext.setMessageUnknownFields
          [ clause
              [varP ufsVar, varP msgVar]
              ( normalB
                  ( recUpdE
                      (varE msgVar)
                      [pure (ufName, VarE ufsVar)]
                  )
              )
              []
          ]
      ]
  pure [inst]


{- | Handle a @TLExtend@ top-level declaration: emit one
'Proto.Extension.Extension' binding per extension field in the
block. The owning message's 'HasExtensions' instance is emitted
separately by 'messageToDecls''. Each extension also registers
itself with the runtime JSON-extension registry so the
generated FromJSON\/ToJSON instances pick up the bracket-quoted
@[FQN]@ syntax automatically.
-}
extendToDecls :: ScopeCtx -> Text -> [FieldDef] -> Q [Dec]
extendToDecls scope ownerProtoName fields = do
  ownerHsName <- resolveHsTypeName scope ownerProtoName
  concat <$> mapM (oneExtensionDec scope ownerHsName ownerProtoName parentFqn) fields
  where
    pkg = scPackage scope
    parentFqn = maybe unresolvedName fst (lookupScopedType scope ownerProtoName)
    unresolvedName
      | Just absolute <- T.stripPrefix "." ownerProtoName = absolute
      | T.null pkg = ownerProtoName
      | T.isInfixOf (T.singleton '.') ownerProtoName = ownerProtoName
      | otherwise = pkg <> "." <> ownerProtoName


lastProtoSegment :: Text -> Text
lastProtoSegment t = case T.splitOn "." t of
  [] -> t
  parts -> last parts


{- | Generate the declarations for one extension field. Singular
fields produce a 'Ext.Extension' descriptor; repeated fields
produce a 'Ext.RepeatedExtension' (with the
'Ext.reIsPacked' flag following the schema's repeated-field encoding).
-}
oneExtensionDec :: ScopeCtx -> Name -> Text -> Text -> FieldDef -> Q [Dec]
oneExtensionDec scope ownerHs ownerProtoName parentFqn fd = do
  delimited <-
    if isLegacyGroupField fd
      then pure True
      else messageDelimitedIn scope (fieldTypeToBridge scope (fieldType fd)) (fieldOptions fd)
  payload <-
    if delimited
      then pure (Just (ConT ''ByteString, 'Ext.ExtGroup))
      else thExtensionPayloadCore scope (fieldType fd)
  let extFqn = T.intercalate "." (filter (not . T.null) (scPackage scope : scParents scope <> [fieldName fd]))
      ExtensionNames valueName registration = lookupExtensionNames (scExtensionNames scope) extFqn ownerProtoName fd
      extName = mkName (T.unpack valueName)
      num = unFieldNumber (fieldNumber fd)
  case fieldLabel fd of
    Just Repeated ->
      case payload of
        Nothing -> pure []
        Just (hsTy, extConName) -> do
          let extConE = conE extConName
              packable = case fieldType fd of
                FTScalar s -> packableScalar s
                FTNamed _ -> extConName == 'Ext.ExtEnum
          packed <-
            if packable
              then either (fail . T.unpack) pure (repeatedFieldPacked (scSyntax scope) (scFileOptions scope) (fieldOptions fd))
              else pure False
          sig <-
            sigD
              extName
              [t|Ext.RepeatedExtension $(conT ownerHs) $(pure hsTy)|]
          body <-
            valD
              (varP extName)
              ( normalB
                  [|
                    Ext.RepeatedExtension
                      { Ext.reNumber = $(litE (IntegerL (fromIntegral num)))
                      , Ext.reType = $extConE
                      , Ext.reIsPacked = $(if packed then [|True|] else [|False|])
                      }
                    |]
              )
              []
          pure [sig, body]
    _ ->
      case payload of
        Nothing -> pure []
        Just (hsTy, extConName) -> do
          let extConE = conE extConName
          sig <-
            sigD
              extName
              [t|Ext.Extension $(conT ownerHs) $(pure hsTy)|]
          body <-
            valD
              (varP extName)
              ( normalB
                  [|
                    Ext.Extension
                      { Ext.extNumber = $(litE (IntegerL (fromIntegral num)))
                      , Ext.extType = $extConE
                      }
                    |]
              )
              []
          regDecs <- extensionJsonRegistrationDecs (mkName (T.unpack registration)) parentFqn extFqn extName
          pure (sig : body : regDecs)


-- | Emit a registry value for a generated singular extension.
extensionJsonRegistrationDecs
  :: Name
  -- ^ Registration binding
  -> Text
  -- ^ Parent message FQN
  -> Text
  -- ^ Extension FQN
  -> Name
  -- ^ Typed extension descriptor
  -> Q [Dec]
extensionJsonRegistrationDecs regName parentFqn extFqn extName = do
  sig <- sigD regName [t|PJExt.ExtensionRegistry|]
  body <-
    valD
      (varP regName)
      ( normalB
          [|
            PJExt.registerTypedExtensionJson
              $(textLitE parentFqn)
              $(textLitE extFqn)
              $(varE extName)
            |]
      )
      []
  pure [sig, body]


{- | Core type/constructor mapping shared by singular and
repeated extensions. Returns 'Nothing' only for shapes that
don't yet exist in this codebase's 'FieldType' ADT (proto2
groups, for instance, were dropped from the official spec
and the parser doesn't recognise them); callers treat
'Nothing' as "skip this extension, the rest of the module
still compiles".
-}
thExtensionPayloadCore :: ScopeCtx -> FieldType -> Q (Maybe (Type, Name))
thExtensionPayloadCore _ (FTScalar s) = pure . Just $ case s of
  SDouble -> (ConT ''Double, 'Ext.ExtDouble)
  SFloat -> (ConT ''Float, 'Ext.ExtFloat)
  SInt32 -> (ConT ''Int32, 'Ext.ExtInt32)
  SInt64 -> (ConT ''Int64, 'Ext.ExtInt64)
  SUInt32 -> (ConT ''Word32, 'Ext.ExtUInt32)
  SUInt64 -> (ConT ''Word64, 'Ext.ExtUInt64)
  SSInt32 -> (ConT ''Int32, 'Ext.ExtSInt32)
  SSInt64 -> (ConT ''Int64, 'Ext.ExtSInt64)
  SFixed32 -> (ConT ''Word32, 'Ext.ExtFixed32)
  SFixed64 -> (ConT ''Word64, 'Ext.ExtFixed64)
  SSFixed32 -> (ConT ''Int32, 'Ext.ExtSFixed32)
  SSFixed64 -> (ConT ''Int64, 'Ext.ExtSFixed64)
  SBool -> (ConT ''Bool, 'Ext.ExtBool)
  SString -> (ConT ''Text, 'Ext.ExtString)
  SBytes -> (ConT ''ByteString, 'Ext.ExtBytes)
thExtensionPayloadCore scope (FTNamed name)
  | isEnumName scope name = do
      tyName <- resolveHsTypeName scope name
      pure (Just (ConT tyName, 'Ext.ExtEnum))
  | otherwise = pure (Just (ConT ''ByteString, 'Ext.ExtMessage))


-- | Whether a scalar is permitted to be packed on the wire.
packableScalar :: ScalarType -> Bool
packableScalar = \case
  SString -> False
  SBytes -> False
  _ -> True


-- ===========================================================
-- IDL bridge: FieldSpec → Proto.Internal.Derive.ProtoField
-- ===========================================================

{- | Translate a single 'FieldSpec' to a 'PDI.ProtoField'.

The bridge wants the field's selector, tag, kind (singular /
'Maybe' / repeated / map / oneof), wire encoding, and per-rep
choice of string / bytes encoding. We already have all of this
in the 'FieldSpec' produced by 'extractMessageFields'.

The parent type 'Name' is required for 'FSOneof' so we can
generate the matching sum-type 'Name' (see 'oneofSumName' and
'oneofConTHName').
-}
fieldSpecToProtoField :: ScopeCtx -> FieldSpec -> Q PDI.ProtoField
fieldSpecToProtoField scope (FSField name num lbl ft rep opts legacyGroup) = do
  let sel = thFieldName scope name
      pft = fieldTypeToBridge scope ft
  delimited <- if legacyGroup then pure True else messageDelimitedIn scope pft opts
  mode <- case (lbl, pft) of
    (Just Repeated, PDI.PFScalar sc)
      | PDI.scalarPackable sc -> packedModeFor scope opts
    (Just Repeated, PDI.PFEnum) -> packedModeFor scope opts
    _ -> pure PDI.ModeUnpacked
  -- Singular submessages track presence even without an optional label.
  let kind = case lbl of
        Just Repeated -> PDI.FKRepeated (fieldRepeated rep) mode
        _
          | explicitFieldPresence lbl || pft == PDI.PFSubmessage -> PDI.FKMaybe
          | otherwise -> PDI.FKBare
  inner <- innerHsType scope ft rep
  let base = PDI.protoField sel num kind pft inner
  pure
    base
      { PDI.pfStringAdapter = fieldString rep
      , PDI.pfBytesAdapter = fieldBytes rep
      , PDI.pfSchemaEnum = pft == PDI.PFEnum
      , PDI.pfDelimited = delimited
      }
fieldSpecToProtoField scope (FSMap name num kt vt rep _) =
  case scalarToBridgeMapKey kt of
    Nothing ->
      fail
        ( "Proto.TH: map field '"
            <> T.unpack name
            <> "' has an invalid map-key type ("
            <> scalarStr kt
            <> "). Proto3 only permits integral and string map keys."
        )
    Just mks -> do
      let sel = thFieldName scope name
          pft = fieldTypeToBridge scope vt
      inner <- innerHsType scope vt rep
      let base = PDI.protoField sel num (PDI.FKMap mks) pft inner
      pure
           base
             { PDI.pfStringAdapter = fieldString rep
             , PDI.pfBytesAdapter = fieldBytes rep
             , PDI.pfSchemaEnum = pft == PDI.PFEnum
             }
fieldSpecToProtoField scope (FSOneof name ofs) = do
  let sel = thFieldName scope name
      sumTy = oneofSumName scope name
      carrier = AppT (ConT ''Maybe) (ConT sumTy)
  variants <- traverse (oneofVariantToBridge scope name) ofs
  -- @pfTag@/@pfType@/@pfInnerTy@ are documented as ignored
  -- by the body builders for FKOneof. The carrier type is
  -- still useful for clarity / future debugging.
  pure (PDI.protoField sel 0 (PDI.FKOneof variants) PDI.PFSubmessage carrier)


packedModeFor :: ScopeCtx -> [OptionDef] -> Q PDI.RepeatedMode
packedModeFor scope opts = do
  packed <- either (fail . T.unpack) pure (repeatedFieldPacked (scSyntax scope) (scFileOptions scope) opts)
  pure (if packed then PDI.ModePacked else PDI.ModeUnpacked)


{- | Build the bridge's 'PDI.OneofVariant' for one arm of a oneof.
The constructor name is computed by 'oneofConTHName' so it
matches the splice in 'mkOneofDataDecs' exactly. The variant's
string / bytes rep comes from the resolved 'FieldRep' (looked
up in 'RepConfig' under @(parentMessage, oneofFieldName)@ in
'extractMessageFields'), so per-variant overrides like \"this
variant only is lazy ByteString\" cleanly survive the bridge.
-}
oneofVariantToBridge :: ScopeCtx -> Text -> (OneofField, FieldRep) -> Q PDI.OneofVariant
oneofVariantToBridge scope ooName (f, rep) = do
  inner <- innerHsType scope (oneofFieldType f) rep
  let base =
        PDI.oneofVariant
          (oneofConTHName scope ooName (oneofFieldName f))
          (unFieldNumber (oneofFieldNumber f))
          inner
          (fieldTypeToBridge scope (oneofFieldType f))
  delimited <- messageDelimitedIn scope (PDI.ovType base) (oneofFieldOptions f)
  pure base
       { PDI.ovStringAdapter = fieldString rep
       , PDI.ovBytesAdapter = fieldBytes rep
       , PDI.ovSchemaEnum = PDI.ovType base == PDI.PFEnum
       , PDI.ovDelimited = delimited
       }


messageDelimitedIn :: ScopeCtx -> PDI.ProtoFieldType -> [OptionDef] -> Q Bool
messageDelimitedIn scope PDI.PFSubmessage opts =
  either (fail . T.unpack) pure (messageEncodingDelimited (scSyntax scope) (scFileOptions scope) opts)
messageDelimitedIn _ _ _ = pure False


-- | Project the AST 'ScalarType' onto the bridge's wire 'Scalar'.
scalarTypeToBridge :: ScalarType -> PDI.Scalar
scalarTypeToBridge = \case
  SDouble -> PDI.SDouble
  SFloat -> PDI.SFloat
  SInt32 -> PDI.SInt32
  SInt64 -> PDI.SInt64
  SUInt32 -> PDI.SUInt32
  SUInt64 -> PDI.SUInt64
  SSInt32 -> PDI.SSInt32
  SSInt64 -> PDI.SSInt64
  SFixed32 -> PDI.SFixed32
  SFixed64 -> PDI.SFixed64
  SSFixed32 -> PDI.SSFixed32
  SSFixed64 -> PDI.SSFixed64
  SBool -> PDI.SBool
  SString -> PDI.SString
  SBytes -> PDI.SBytes


{- | Project an AST 'FieldType' onto the bridge's
'PDI.ProtoFieldType'. Named types are looked up against the
file's 'ScopeCtx' so we can distinguish enums (encoded as
varints via @PFEnum@) from submessages (length-delimited).
Without this lookup top-level proto enums silently encoded as
length-delimited submessages and produced bytes that no other
proto implementation could read.
-}
fieldTypeToBridge :: ScopeCtx -> FieldType -> PDI.ProtoFieldType
fieldTypeToBridge scope = \case
  FTScalar s -> PDI.PFScalar (scalarTypeToBridge s)
  FTNamed n
    | isEnumName scope n -> PDI.PFEnum
    | otherwise -> PDI.PFSubmessage


{- | Reconstruct the Haskell type the record selector returns,
so the bridge's encoder can supply the matching @($var :: T)@
ascription.
-}
innerHsType :: ScopeCtx -> FieldType -> FieldRep -> Q Type
innerHsType scope (FTNamed n) _ = ConT <$> resolveHsTypeName scope n
innerHsType _ (FTScalar scalar) rep = pure $ case scalar of
  SString -> stringHsType (fieldString rep)
  SBytes -> bytesHsType (fieldBytes rep)
  SInt32 -> ConT ''Int32
  SInt64 -> ConT ''Int64
  SUInt32 -> ConT ''Word32
  SUInt64 -> ConT ''Word64
  SSInt32 -> ConT ''Int32
  SSInt64 -> ConT ''Int64
  SFixed32 -> ConT ''Word32
  SFixed64 -> ConT ''Word64
  SSFixed32 -> ConT ''Int32
  SSFixed64 -> ConT ''Int64
  SDouble -> ConT ''Double
  SFloat -> ConT ''Float
  SBool -> ConT ''Bool


stringHsType :: StringAdapter -> Type
stringHsType adapter = case stringBaseRep adapter of
  StrictTextRep -> ConT ''Text
  LazyTextRep -> ConT ''TL.Text
  ShortTextRep -> ConT ''SBS.ShortByteString
  HsStringRep -> ConT ''String


bytesHsType :: BytesAdapter -> Type
bytesHsType adapter = case bytesBaseRep adapter of
  StrictBytesRep -> ConT ''ByteString
  LazyBytesRep -> ConT ''BL.ByteString
  ShortBytesRep -> ConT ''SBS.ShortByteString


{- | Map an AST 'ScalarType' onto the bridge's 'MapKeyScalar' if
it's a permitted proto3 map key type. 'Nothing' for the
forbidden ones (double / float / bytes / message / enum); the
caller falls back to the legacy emitter when this fires
(matching the parser, which would have rejected such a map
earlier anyway — so 'Nothing' here is paranoia).
-}
scalarToBridgeMapKey :: ScalarType -> Maybe MapKeyScalar
scalarToBridgeMapKey = \case
  SInt32 -> Just MapKeyInt32
  SInt64 -> Just MapKeyInt64
  SUInt32 -> Just MapKeyUInt32
  SUInt64 -> Just MapKeyUInt64
  SSInt32 -> Just MapKeySInt32
  SSInt64 -> Just MapKeySInt64
  SFixed32 -> Just MapKeyFixed32
  SFixed64 -> Just MapKeyFixed64
  SSFixed32 -> Just MapKeySFixed32
  SSFixed64 -> Just MapKeySFixed64
  SBool -> Just MapKeyBool
  SString -> Just MapKeyString
  SDouble -> Nothing
  SFloat -> Nothing
  SBytes -> Nothing


-- ===========================================================
-- Satellite-instance bridge: FieldSpec -> PTM.MetaField
-- ===========================================================

{- | Translate a 'FieldSpec' into the condensed 'PTM.MetaField'
shape consumed by the @ProtoMessage@ \/ JSON \/ Hashable
emitters in "Proto.TH.Metadata".
-}
fieldSpecToMetaField :: ScopeCtx -> FieldSpec -> PTM.MetaField
fieldSpecToMetaField scope fs = case fs of
  FSField name num lbl ft rep opts legacyGroup ->
    let sel = thFieldName scope name
        jsonNm = jsonNameFromOpts opts (protoJsonName name)
        repeatedKind = case repeatedBaseRep (fieldRepeated rep) of
          VectorRep -> PTM.MFKVector
          ListRep -> PTM.MFKList
          SeqRep -> PTM.MFKSeq
        kind = case lbl of
          Just Repeated -> repeatedKind
          _ | explicitFieldPresence lbl -> PTM.MFKMaybe
          _ -> case ft of
            FTNamed n
              | not (isEnumName scope n) -> PTM.MFKMaybe
            _ -> PTM.MFKBare
        bytesShape = case bytesBaseRep (fieldBytes rep) of
          StrictBytesRep -> PTM.SBStrict
          LazyBytesRep -> PTM.SBLazy
          ShortBytesRep -> PTM.SBShort
        jsonKind = case (lbl, ft) of
          (Just Repeated, FTScalar SBytes) -> case repeatedBaseRep (fieldRepeated rep) of
            VectorRep -> PTM.JKBytesVector
            ListRep -> PTM.JKBytesList
            SeqRep -> PTM.JKBytesSeq
          (_, FTScalar SBytes) -> PTM.JKBytes
          _ -> PTM.JKNormal
        jsonShape = case (lbl, ft) of
          (Just Repeated, FTScalar s) -> PTM.JSRepeatedScalar (jsScalarOf s)
          (Just Repeated, FTNamed n)
            | Just w <- lookupScopedWktShape scope n -> PTM.JSWktRepeated w
            | isEnumName scope n -> PTM.JSRepeatedEnum
            | otherwise -> PTM.JSRepeatedMessage
          (_, FTScalar s) | explicitFieldPresence lbl -> PTM.JSMaybe (jsScalarOf s)
          (_, FTNamed n)
            | explicitFieldPresence lbl
            , Just w <- lookupScopedWktShape scope n -> PTM.JSWktMaybe w
            | explicitFieldPresence lbl
            , isEnumName scope n -> PTM.JSEnumMaybe
            | explicitFieldPresence lbl -> PTM.JSMessage
          (_, FTScalar s) -> PTM.JSScalar (jsScalarOf s)
          (_, FTNamed n)
            | Just PTM.WktNullValue <- lookupScopedWktShape scope n -> PTM.JSEnum
            | Just w <- lookupScopedWktShape scope n -> PTM.JSWkt w
            | isEnumName scope n -> PTM.JSEnum
            | otherwise -> PTM.JSMessage
    in PTM.MetaField
         { PTM.mfSelector = sel
         , PTM.mfProtoName = name
         , PTM.mfJsonName = jsonNm
         , PTM.mfNumber = num
         , PTM.mfTypeDesc = wireFieldTypeDescE scope ft opts legacyGroup
         , PTM.mfLabel = protoLabelE lbl
         , PTM.mfKind = kind
         , PTM.mfJsonKind = jsonKind
         , PTM.mfBytesShape = bytesShape
         , PTM.mfJsonShape = jsonShape
         , PTM.mfRecordDot = scFieldNaming scope == UnprefixedFields
         }
  FSMap name num kt vt rep opts ->
    let sel = thFieldName scope name
        jsonNm = jsonNameFromOpts opts (protoJsonName name)
        bytesShape = case bytesBaseRep (fieldBytes rep) of
          StrictBytesRep -> PTM.SBStrict
          LazyBytesRep -> PTM.SBLazy
          ShortBytesRep -> PTM.SBShort
        jsonKind = case vt of
          FTScalar SBytes -> PTM.JKBytesMap
          _ -> PTM.JKNormal
        jsonShape = case vt of
          FTScalar s -> PTM.JSMapScalar (jsScalarOf kt) (jsScalarOf s)
          FTNamed n
            | Just PTM.WktNullValue <- lookupScopedWktShape scope n -> PTM.JSMapJSON (jsScalarOf kt)
            | isEnumName scope n -> PTM.JSMapEnum (jsScalarOf kt)
            | otherwise -> PTM.JSMapJSON (jsScalarOf kt)
    in PTM.MetaField
         { PTM.mfSelector = sel
         , PTM.mfProtoName = name
         , PTM.mfJsonName = jsonNm
         , PTM.mfNumber = num
         , PTM.mfTypeDesc = mapTypeDescE scope kt vt
         , PTM.mfLabel = [|PS.LabelRepeated|]
         , PTM.mfKind = PTM.MFKMap
         , PTM.mfJsonKind = jsonKind
         , PTM.mfBytesShape = bytesShape
         , PTM.mfJsonShape = jsonShape
         , PTM.mfRecordDot = scFieldNaming scope == UnprefixedFields
         }
  FSOneof name ofs ->
    let sel = thFieldName scope name
        jsonNm = protoJsonName name
        variants = fmap (oneofVariantJson scope name) ofs
        number = case ofs of
          (field, _) : _ -> unFieldNumber (oneofFieldNumber field)
          [] -> 0
    in PTM.MetaField
         { PTM.mfSelector = sel
         , PTM.mfProtoName = name
         , PTM.mfJsonName = jsonNm
         , PTM.mfNumber = number
         , PTM.mfTypeDesc = [|PS.MessageType $(textLitE name)|]
         , PTM.mfLabel = [|PS.LabelOptional|]
         , PTM.mfKind = PTM.MFKOneof
         , PTM.mfJsonKind = PTM.JKNormal
         , PTM.mfBytesShape = PTM.SBStrict
         , PTM.mfJsonShape = PTM.JSOneof variants
         , PTM.mfRecordDot = scFieldNaming scope == UnprefixedFields
         }


oneofDescriptorE :: ScopeCtx -> FieldSpec -> [Q Exp]
oneofDescriptorE scope (FSOneof name members) =
  [[|PS.OneofDescriptor $(textLitE name) $(ListE <$> traverse member members)|]]
  where
    member (field, _) = do
      let ft = oneofFieldType field
          descriptor = case ft of
            FTNamed enum | Just PTM.WktNullValue <- lookupScopedWktShape scope enum ->
              [|PS.EnumType $(textLitE "google.protobuf.NullValue")|]
            _ -> wireFieldTypeDescE scope ft (oneofFieldOptions field) False
          allowed = Data.Maybe.fromMaybe [|Nothing|] (closedEnumValuesE scope ft)
      [|PS.OneofFieldDescriptor
          $(textLitE (oneofFieldName field))
          $(litE (IntegerL (fromIntegral (unFieldNumber (oneofFieldNumber field)))))
          $descriptor
          $allowed|]
oneofDescriptorE _ _ = []

closedEnumFieldE :: ScopeCtx -> FieldSpec -> Maybe (Q Exp)
closedEnumFieldE scope field = case field of
  FSField {fsNum = number, fsType = ft} -> entry number ft
  FSMap {fsNum = number, fsMapVal = ft} -> entry number ft
  FSOneof {} -> Nothing
  where
    entry number ft =
      fmap (\values -> [|($(litE (IntegerL (fromIntegral number))), $values)|]) (closedEnumValuesE scope ft)


closedEnumValuesE :: ScopeCtx -> FieldType -> Maybe (Q Exp)
closedEnumValuesE scope (FTNamed name) | isEnumName scope name = Just $ do
  ty <- resolveHsTypeName scope name
  let proxy = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) (ConT ty))
  [|if PS.protoEnumIsOpen $(pure proxy) then Nothing else Just (map snd (PS.protoEnumValues $(pure proxy)))|]
closedEnumValuesE _ _ = Nothing


submessageEntriesE :: ScopeCtx -> FieldSpec -> [Q Exp]
submessageEntriesE scope field = case field of
  FSField {fsNum = number, fsType = ft} -> maybe [] pure (entry number ft)
  FSOneof _ members -> mapMaybe (\(arm, _) -> entry (unFieldNumber (oneofFieldNumber arm)) (oneofFieldType arm)) members
  FSMap {} -> []
  where
    entry number (FTNamed name)
      | Just (_, info) <- lookupScopedType scope name, tiKind info == TKMessage = Just $ do
          ty <- resolveHsTypeName scope name
          let proxy = SigE (ConE 'Proxy) (AppT (ConT ''Proxy) (ConT ty))
          [|($(litE (IntegerL (fromIntegral number))), PS.SomeProtoMessage $(pure proxy))|]
    entry _ _ = Nothing


mapValueDecoderE :: ScopeCtx -> FieldSpec -> Maybe (Q Exp)
mapValueDecoderE scope FSMap {fsNum = number, fsMapVal = FTNamed name}
  | Just (_, info) <- lookupScopedType scope name
  , tiKind info == TKMessage = Just $ do
      ty <- resolveHsTypeName scope name
      let decoder = SigE (VarE 'Decode.messageDecoder) (AppT (ConT ''Decode.Decoder) (ConT ty))
      [|($(litE (IntegerL (fromIntegral number))), () <$ $(pure decoder))|]
mapValueDecoderE _ _ = Nothing


{- | Build the JSON-side shape for one oneof variant. The variant's
JSON key is the camelCase form of the proto-side field name
(or @json_name@ when the proto declared one); the payload
shape is dispatched on the variant's value type.
-}
oneofVariantJson
  :: ScopeCtx
  -> Text
  -- ^ Oneof field name (used for naming only).
  -> (OneofField, FieldRep)
  -> PTM.OneofVariantJson
oneofVariantJson scope ooName (f, _rep) =
  let conN = oneofConTHName scope ooName (oneofFieldName f)
      jsonKey =
        jsonNameFromOpts
          (oneofFieldOptions f)
          (protoJsonName (oneofFieldName f))
      shape = case oneofFieldType f of
        FTScalar s -> PTM.OVScalar (jsScalarOf s)
        FTNamed n -> PTM.OVNamed $ case lookupScopedWktShape scope n of
          Just PTM.WktNullValue -> True
          Just PTM.WktValue -> True
          _ -> False
  in PTM.OneofVariantJson
       { PTM.ovjConstructor = conN
       , PTM.ovjJsonKey = jsonKey
       , PTM.ovjProtoKey = oneofFieldName f
       , PTM.ovjShape = shape
       }


{- | Project a proto FQN to the metadata-bridge's 'WktShape' tag
when the FQN names a Well-Known-Type the JSON splice has a
canonical encoder for.
-}
lookupScopedWktShape :: ScopeCtx -> Text -> Maybe PTM.WktShape
lookupScopedWktShape scope n = case fmap fst (lookupScopedType scope n) of
  Just "google.protobuf.Timestamp" -> Just PTM.WktTimestamp
  Just "google.protobuf.Duration" -> Just PTM.WktDuration
  Just "google.protobuf.FieldMask" -> Just PTM.WktFieldMask
  Just "google.protobuf.Struct" -> Just PTM.WktStruct
  Just "google.protobuf.Value" -> Just PTM.WktValue
  Just "google.protobuf.ListValue" -> Just PTM.WktListValue
  Just "google.protobuf.Any" -> Just PTM.WktAny
  Just "google.protobuf.Empty" -> Just PTM.WktEmpty
  Just "google.protobuf.NullValue" -> Just PTM.WktNullValue
  Just "google.protobuf.BoolValue" -> Just PTM.WktWrapBool
  Just "google.protobuf.Int32Value" -> Just PTM.WktWrapInt32
  Just "google.protobuf.Int64Value" -> Just PTM.WktWrapInt64
  Just "google.protobuf.UInt32Value" -> Just PTM.WktWrapUInt32
  Just "google.protobuf.UInt64Value" -> Just PTM.WktWrapUInt64
  Just "google.protobuf.FloatValue" -> Just PTM.WktWrapFloat
  Just "google.protobuf.DoubleValue" -> Just PTM.WktWrapDouble
  Just "google.protobuf.StringValue" -> Just PTM.WktWrapString
  Just "google.protobuf.BytesValue" -> Just PTM.WktWrapBytes
  _ -> Nothing


{- | Project an AST 'ScalarType' onto the metadata-bridge's
'JsonScalar' tag.
-}
jsScalarOf :: ScalarType -> PTM.JsonScalar
jsScalarOf = \case
  SDouble -> PTM.JSDouble
  SFloat -> PTM.JSFloat
  SInt32 -> PTM.JSInt32
  SInt64 -> PTM.JSInt64
  SUInt32 -> PTM.JSUInt32
  SUInt64 -> PTM.JSUInt64
  SSInt32 -> PTM.JSSInt32
  SSInt64 -> PTM.JSSInt64
  SFixed32 -> PTM.JSFixed32
  SFixed64 -> PTM.JSFixed64
  SSFixed32 -> PTM.JSSFixed32
  SSFixed64 -> PTM.JSSFixed64
  SBool -> PTM.JSBool
  SString -> PTM.JSString
  SBytes -> PTM.JSBytes


wireFieldTypeDescE :: ScopeCtx -> FieldType -> [OptionDef] -> Bool -> Q Exp
wireFieldTypeDescE scope ft opts legacy = do
  delimited <- messageDelimitedIn scope (fieldTypeToBridge scope ft) opts
  case ft of
    FTNamed name | legacy || delimited -> [|PS.GroupType $(textLitE name)|]
    _ -> fieldTypeDescE scope ft


-- | Per-shape splice for 'PS.FieldTypeDescriptor'.
fieldTypeDescE :: ScopeCtx -> FieldType -> Q Exp
fieldTypeDescE scope ft = case ft of
  FTScalar s -> [|PS.ScalarType $(scalarFieldTypeE s)|]
  FTNamed n
    | isEnumName scope n -> [|PS.EnumType $(textLitE n)|]
    | otherwise -> [|PS.MessageType $(textLitE n)|]


mapTypeDescE :: ScopeCtx -> ScalarType -> FieldType -> Q Exp
mapTypeDescE scope kt vt =
  [|PS.MapType $(scalarFieldTypeE kt) $(fieldTypeDescE scope vt)|]


scalarFieldTypeE :: ScalarType -> Q Exp
scalarFieldTypeE = \case
  SDouble -> [|PS.DoubleField|]
  SFloat -> [|PS.FloatField|]
  SInt32 -> [|PS.Int32Field|]
  SInt64 -> [|PS.Int64Field|]
  SUInt32 -> [|PS.UInt32Field|]
  SUInt64 -> [|PS.UInt64Field|]
  SSInt32 -> [|PS.SInt32Field|]
  SSInt64 -> [|PS.SInt64Field|]
  SFixed32 -> [|PS.Fixed32Field|]
  SFixed64 -> [|PS.Fixed64Field|]
  SSFixed32 -> [|PS.SFixed32Field|]
  SSFixed64 -> [|PS.SFixed64Field|]
  SBool -> [|PS.BoolField|]
  SString -> [|PS.StringField|]
  SBytes -> [|PS.BytesField|]


protoLabelE :: Maybe FieldLabel -> Q Exp
protoLabelE = \case
  Nothing -> [|PS.LabelOptional|]
  Just Optional -> [|PS.LabelOptional|]
  Just Required -> [|PS.LabelRequired|]
  Just Repeated -> [|PS.LabelRepeated|]


textLitE :: Text -> Q Exp
textLitE t = [|T.pack $(litE (StringL (T.unpack t)))|]


{- | Resolve the JSON key for a field. The proto3 default is the
camelCase form of the proto-side name; the @json_name@ option
(declared inline in the @.proto@) overrides it.
-}
jsonNameFromOpts :: [OptionDef] -> Text -> Text
jsonNameFromOpts opts dflt = case lookupSimpleOption (T.pack "json_name") opts of
  Just c -> Data.Maybe.fromMaybe dflt (optionAsString c)
  Nothing -> dflt


{- | For each oneof carrier in the message, emit @ToJSON@ \/
@FromJSON@ \/ @Hashable@ instances on the carrier sum type. The
sum's constructors were emitted by 'mkOneofDataDecs'.
-}
oneofSatelliteDecs :: ScopeCtx -> FieldSpec -> Q [Dec]
oneofSatelliteDecs scope = \case
  FSOneof name ofs -> do
    let sumTy = oneofSumName scope name
        cons =
          fmap
            ( \(f, _) ->
                oneofConTHName scope name (oneofFieldName f)
            )
            ofs
    aeson <- PTM.mkOneofAesonInstances sumTy
    hashable <- PTM.mkOneofHashableInstance sumTy cons
    pure (aeson <> [hashable])
  _ -> pure []
