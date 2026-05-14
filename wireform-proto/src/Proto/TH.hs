{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

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
  * @MessageEncode@ \/ @MessageDecode@ wire codecs
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
  LoadOpts (..),
  defaultLoadOpts,
  protoFileToDecls,
  messageToDecls,
  enumToDecls,
) where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as SBS
import Data.Char qualified
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Maybe qualified
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile, addModFinalizer)
import Proto.CodeGen (
  FieldNaming (..),
  escapeReserved,
  hsTypeName,
  lowerFirst,
  protoJsonName,
  snakeToCamel,
  snakeToPascal,
 )
import Proto.CodeGen.Hooks
import Proto.Internal.Decode qualified as Decode
import Proto.Extension qualified as Ext
import Proto.IDL.AST
import Proto.IDL.Annotations (lookupSimpleOption, optionAsBool, optionAsString)
import Proto.IDL.Options.Custom (emptyCustomOptionRegistry, extractExtensionOptions, registerCustomOption)
import Proto.IDL.Parser (parseProtoFile, renderParseError)
import Proto.Internal.Derive qualified as PDI
import Proto.Internal.JSON.Extension qualified as PJExt
import Proto.Repr
import Proto.Schema qualified as PS
import Proto.TH.Metadata qualified as PTM
import Wireform.Derive.Modifier (MapKeyScalar (..))


{- | Produce a Haskell-valid record-field name from a proto field.
The proto-side name is snake_cased (@file_path@, @num_rows@); we
convert to camelCase and escape reserved keywords with a
trailing prime (@data@ → @data'@, @type@ → @type'@, …). Without
the escape, TH splices produce @data :: Foo -> Bar@ which is a
parse error.
-}
hsFieldName :: Text -> Text
hsFieldName = escapeReserved . snakeToCamel


{- | Message-scoped field name, mirroring the convention the pure-
text codegen in 'Proto.CodeGen.scopedFieldName' uses:

@scopedHsFieldName \"Account\" \"acct_name\" = "accountAcctName"@

Two messages in the same file with overlapping field names
(@ConformanceRequest.protobuf_payload@ vs
@ConformanceResponse.protobuf_payload@) would otherwise both
emit a record selector @protobufPayload@ at the top level,
which GHC rejects.
| Scope-prefixed Haskell type name. Mirrors
'Proto.CodeGen.scopedTypeName': joins the parent chain
with the message's own name using @'@. Empty parent list
collapses to plain 'hsTypeName'.
-}
scopedHsTypeName :: [Text] -> Text -> Text
scopedHsTypeName parents nm = case parents of
  [] -> hsTypeName nm
  _ -> T.intercalate "'" (fmap hsTypeName (parents <> [nm]))


{- | Scope-prefixed enum constructor name. Always qualifies
the value with its enum's Haskell type name so two enums
(in the same file or across files) can declare identical
value names without colliding at the Haskell level. For
top-level enums this gives @EnumName'ValueName@; for nested
enums it gives @Parent'EnumName'ValueName@.
-}
scopedHsEnumCon :: [Text] -> Text -> Text -> Text
scopedHsEnumCon parents enumNm evNm =
  scopedHsTypeName parents enumNm
    <> T.singleton '\''
    <> snakeToPascal evNm


{- | The TH 'Name' of the synthetic @<EnumName>'Unknown !Int32@
constructor every loadProto-generated enum carries to hold
proto3 "open enum" wire values that aren't covered by any
declared name.
-}
unknownConNameFor :: [Text] -> Text -> Name
unknownConNameFor parents enumNm =
  mkName
    ( T.unpack
        (scopedHsTypeName parents enumNm <> T.pack "'Unknown")
    )


scopedHsFieldName :: FieldNaming -> Text -> Text -> Text
scopedHsFieldName PrefixedFields parentMsg fldName =
  let prefix = lowerFirst (hsTypeName parentMsg)
  in escapeReserved (prefix <> upperFirstT (snakeToCamel fldName))
scopedHsFieldName UnprefixedFields _parentMsg fldName =
  escapeReserved (snakeToCamel fldName)


upperFirstT :: Text -> Text
upperFirstT t = case T.uncons t of
  Just (c, rest) -> T.cons (Data.Char.toUpper c) rest
  Nothing -> t


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


hsEnumCon :: Text -> Text -> Text
hsEnumCon _enumName = snakeToPascal


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
  -- ^ Directories to search when resolving @import@ statements in
  -- @.proto@ files. Default: @[\"proto\/\", \".\"]@.
  , loFieldNaming :: FieldNaming
  -- ^ How to name generated record fields. 'PrefixedFields' (default)
  -- prefixes each field with the lowercased message name.
  -- 'UnprefixedFields' uses bare names (requires @DuplicateRecordFields@).
  , loRepConfig :: RepConfig
  -- ^ Controls how proto field types map to Haskell types. See
  -- "Proto.Repr" for the adapter system and per-field overrides.
  , loTHHooks :: THHooks
  -- ^ Callbacks that produce extra TH declarations. See
  -- 'Proto.CodeGen.Hooks.THHooks'.
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
loadProtoWith opts path = do
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
      decls <- protoFileToDecls' (loFieldNaming opts) (loRepConfig opts) hooks pf
      hookDecls <- thOnFile hooks fileCtx
      pure (decls <> hookDecls)


{- | Generate declarations for all top-level definitions in a parsed
'ProtoFile', using default options.
-}
protoFileToDecls :: ProtoFile -> Q [Dec]
protoFileToDecls = protoFileToDecls' PrefixedFields defaultRepConfig defaultTHHooks


protoFileToDecls' :: FieldNaming -> RepConfig -> THHooks -> ProtoFile -> Q [Dec]
protoFileToDecls' naming cfg hooks pf = do
  let scope =
        ScopeCtx
          { scSyntax = protoSyntax pf
          , scTopLevels = protoTopLevels pf
          , scPackage = Data.Maybe.fromMaybe T.empty (protoPackage pf)
          , scParents = []
          , scFieldNaming = naming
          , scFileOptions = protoOptions pf
          }
  concat <$> mapM (topLevelToDecls scope cfg hooks) (protoTopLevels pf)


{- | Lookup table built once per file: lets the bridge tell whether
a named-type reference points at an enum (which the bridge
encodes as a varint via 'PFEnum') or a message (encoded as a
length-delimited submessage). Without this, every named type
got encoded as a submessage and top-level proto enums silently
broke on the wire.
-}
data ScopeCtx = ScopeCtx
  { scSyntax :: !Syntax
  , scTopLevels :: ![TopLevel]
  , scPackage :: !Text
  -- ^ Proto package as declared in the file (empty string when
  -- the file has no @package@ statement). Drives the
  -- @protoMessageName@ \/ @protoPackageName@ outputs in the
  -- generated 'PS.ProtoMessage' instance.
  , scParents :: ![Text]
  -- ^ Parent message names accumulated as we recurse into
  -- nested types. Used to scope-prefix the generated Haskell
  -- type / constructor / field names so two messages from
  -- different .proto files (or different parents within the
  -- same file) can declare an inner @NestedMessage@ without
  -- colliding at the Haskell level.
  , scFieldNaming :: !FieldNaming
  -- ^ How to name record fields (prefixed with message name
  -- or bare).
  , scFileOptions :: ![OptionDef]
  -- ^ File-level option definitions. Used to resolve edition
  -- feature overrides (e.g. @option features.repeated_field_encoding = EXPANDED@).
  }


{- | Resolve a referenced type name to the scope chain it
lives under, so callers can compute the matching Haskell
type name with 'scopedHsTypeName'. The lookup walks the
file's top-level declarations, considering both top-level
and nested messages \/ enums. If the name isn't found, we
fall back to the empty scope (treats it as a top-level
reference, which matches the legacy behaviour).

Proto resolution rules want us to search the lexical scope
inside-out, but for the conformance test (and most real
schemas) the simple "find anywhere in this file" rule is
sufficient: the upstream resolver has already de-aliased
imports so the leaf name is unambiguous within a file.
-}
findTypeScope :: ScopeCtx -> Text -> [Text]
findTypeScope scope t =
  let leaf = leafOf t
      tryTopLevel (TLMessage m) = searchMessage [] m leaf
      tryTopLevel (TLEnum e)
        | enumName e == leaf = Just []
        | otherwise = Nothing
      tryTopLevel _ = Nothing

      -- DFS, returning the parent path (excluding the matched
      -- type's own name).
      searchMessage parents m needle
        | msgName m == needle = Just parents
        | otherwise =
            let parents' = parents <> [msgName m]
                fromElts = foldr step Nothing (msgElements m)
                step elt acc = acc <|> searchElt parents' elt needle
            in fromElts
      searchElt parents (MEMessage inner) needle =
        searchMessage parents inner needle
      searchElt parents (MEEnum e) needle
        | enumName e == needle = Just parents
        | otherwise = Nothing
      searchElt _ _ _ = Nothing

      -- First top-level that matches wins.
      foldTL acc tl = acc <|> tryTopLevel tl
  in Data.Maybe.fromMaybe [] (foldl foldTL Nothing (scTopLevels scope))
  where
    leafOf x = case T.splitOn (T.pack ".") x of
      [] -> x
      ps -> last ps


{- | Compute the scoped Haskell type name for a referenced
proto type, using 'findTypeScope' to discover its nesting
and 'scopedHsTypeName' to assemble the Haskell identifier.
-}
resolveScopedHsType :: ScopeCtx -> Text -> Text
resolveScopedHsType scope t =
  scopedHsTypeName (findTypeScope scope t) (leafOf t)
  where
    leafOf x = case T.splitOn (T.pack ".") x of
      [] -> x
      ps -> last ps


{- | Walk a 'ScopeCtx' looking for an enum named @t@ at any
nesting depth. Used by the bridge to decide PFEnum vs.
PFSubmessage for an 'FTNamed' reference.
-}
isEnumName :: ScopeCtx -> Text -> Bool
isEnumName scope t = anyTopLevel (scTopLevels scope)
  where
    -- Match either by short name (@Color@) or by fully-qualified
    -- nested name (@MyMessage.Color@). The proto resolver upstream
    -- has already de-aliased imports, so a literal Text comparison
    -- is sufficient.
    matchesEnumLeaf n = leafOf n == leafOf t
    leafOf x = case T.splitOn (T.pack ".") x of
      [] -> x
      ps -> last ps
    anyTopLevel = any topMatch
    topMatch (TLEnum ed) = matchesEnumLeaf (enumName ed)
    topMatch (TLMessage msg) = anyMessageElt msg
    topMatch _ = False
    anyMessageElt msg = any eltMatch (msgElements msg)
    eltMatch (MEEnum ed) = matchesEnumLeaf (enumName ed)
    eltMatch (MEMessage m) = anyMessageElt m
    eltMatch _ = False


topLevelToDecls :: ScopeCtx -> RepConfig -> THHooks -> TopLevel -> Q [Dec]
topLevelToDecls scope cfg hooks = \case
  TLMessage msg -> messageToDecls'' scope cfg hooks msg
  TLEnum ed -> enumToDecls'' (scPackage scope) (scParents scope) hooks ed
  TLExtend owner fields -> extendToDecls (scPackage scope) owner fields
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
  let scope =
        ScopeCtx
          { scSyntax = Proto3
          , scTopLevels = [TLMessage msg]
          , scPackage = T.empty
          , scParents = []
          , scFieldNaming = PrefixedFields
          , scFileOptions = []
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
    -- Push this message onto the scope chain for nested types.
    childScope = scopeCtx {scParents = scParents scopeCtx <> [msgName msg]}

  nestedDecls <-
    concat
      <$> mapM
        ( \case
            MEMessage inner -> messageToDecls'' childScope cfg hooks inner
            MEEnum ed ->
              enumToDecls''
                (scPackage childScope)
                (scParents childScope)
                hooks
                ed
            _ -> pure []
        )
        (msgElements msg)

  -- Sum types backing each oneof. Must precede the message data
  -- declaration so that GHC's renamer sees the constructor names
  -- in scope when the wire codecs splice (the codec splice
  -- references @ovConstructor@ at the term level).
  oneofDecs <- mkOneofDataDecs scopeCtx tyName fields
  dataDec <- mkDataDec scopeCtx tyName fields
  defaultDec <- mkDefaultDec scopeCtx tyName fields
  -- All wire codecs (MessageEncode / MessageDecode)
  -- now come from 'Proto.Internal.Derive' via the IDL bridge,
  -- including oneofs (whose sum types are emitted by
  -- 'mkOneofDataDecs' just above). The bridge handles every
  -- 'FieldSpec' shape; if 'fieldSpecToProtoField' reports an
  -- impossible map key (which the parser shouldn't accept) the
  -- splice fails with a clear message rather than silently
  -- generating broken code.
  pfs <- traverse (fieldSpecToProtoField scopeCtx tyName) fields
  codecDecs <- messageCodecsViaBridge tyName pfs
  hasExtDec <- mkHasExtensionsInstance tyName (msgName msg)
  hookDecls <- thOnMessage hooks hookCtx

  let defName = mkName ("default" <> nameBase tyName)
      fqName = case scPackage scopeCtx of
        p
          | T.null p -> msgName msg
          | otherwise -> p <> T.singleton '.' <> msgName msg
      metaFields = fmap (fieldSpecToMetaField scopeCtx tyName) fields
  protoMsgDecs <-
    PTM.mkProtoMessageInstance
      tyName
      fqName
      (scPackage scopeCtx)
      defName
      metaFields
  -- The unknown-fields selector name (always present on
  -- TH-generated messages) — passing it through lets the JSON
  -- splice patch in proto2 extension entries via the runtime
  -- registry. We use the same naming convention as
  -- 'unknownFieldsName'.
  let ufSelName = unknownFieldsName tyName
  aesonDecs <-
    PTM.mkAesonInstancesForMessage
      tyName
      fqName
      (Just ufSelName)
      defName
      metaFields
  hashableDec <- PTM.mkHashableInstanceForMessage tyName metaFields

  -- Per-oneof carrier sum: ToJSON / FromJSON / Hashable. The data
  -- declaration was emitted by 'mkOneofDataDecs' just above, so
  -- the constructor names line up with what we splice here.
  oneofSatellites <- fmap concat (mapM (oneofSatelliteDecs tyName) fields)

  addModFinalizer (putDoc (DeclDoc tyName) (messageHaddock msg fields))
  addModFinalizer
    ( putDoc
        (DeclDoc defName)
        ( "Default value for @"
            <> T.unpack (msgName msg)
            <> "@ with all fields at their proto default values."
        )
    )

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
  let ismDec = PDI.mkIsMessageInstance (ConT tyName)

  pure
    ( nestedDecls
        <> oneofDecs
        <> [dataDec]
        <> defaultDec
        <> codecDecs
        <> [monoidDec]
        <> hasExtDec
        <> protoMsgDecs
        <> aesonDecs
        <> [hashableDec]
        <> [ismDec]
        <> oneofSatellites
        <> hookDecls
    )


{- | Synthesise the @MessageEncode \/ MessageDecode@ pair via
'Proto.Internal.Derive.synthesiseProtoInstancesWith' with
unknown-field preservation enabled. Used for every
'loadProto'-generated message.
-}
messageCodecsViaBridge :: Name -> [PDI.ProtoField] -> Q [Dec]
messageCodecsViaBridge tyName pfs = do
  let meta =
        PDI.MessageMeta
          { PDI.mmUnknownFieldsSel = Just (unknownFieldsName tyName)
          }
  enc <- PDI.mkEncodeInstanceWith meta (ConT tyName) pfs
  dec <- PDI.mkDecodeInstanceWith meta (ConT tyName) tyName pfs
  semi <- PDI.mkSemigroupInstanceWith meta (ConT tyName) tyName pfs
  pure [enc, dec, semi]


messageHaddock :: MessageDef -> [FieldSpec] -> String
messageHaddock msg fields =
  "Protobuf message @"
    <> T.unpack (msgName msg)
    <> "@.\n\n"
    <> "Fields:\n\n"
    <> concatMap fieldHaddock fields


fieldHaddock :: FieldSpec -> String
fieldHaddock (FSField name num lbl ft _ _) =
  "* @"
    <> T.unpack name
    <> "@ ("
    <> labelStr lbl
    <> fieldTypeStr ft
    <> ", field "
    <> show num
    <> ")\n"
fieldHaddock (FSMap name num kt vt _) =
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
      }
  | FSMap
      { fsName :: Text
      , fsNum :: Int
      , fsMapKey :: ScalarType
      , fsMapVal :: FieldType
      , fsMapRep :: FieldRep
      -- ^ Resolved per-field 'FieldRep'. Drives the bytes / string
      -- rep of the value type and the JSON helper choice, the
      -- same way 'fsRep' does for 'FSField'.
      }
  | FSOneof
      { fsName :: Text
      , fsOneofFields :: [(OneofField, FieldRep)]
      -- ^ Each variant paired with its resolved 'FieldRep'.
      -- The string / bytes / repeated rep choices come from the
      -- same 'RepConfig' lookup machinery as regular fields, keyed
      -- by @(parentMessage, oneofFieldName)@. This lets users
      -- override one variant of a oneof to lazy / short / hsString
      -- without affecting siblings.
      }


fsFieldName :: FieldSpec -> Text
fsFieldName (FSField n _ _ _ _ _) = n
fsFieldName (FSMap n _ _ _ _) = n
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
  let unknownFieldEntry = mkUnknownFieldsField tyName
  let con = recC tyName (fmap pure (recFields <> [unknownFieldEntry]))
  dataD
    (pure [])
    tyName
    []
    Nothing
    [con]
    [derivClause (Just StockStrategy) [conT ''Show, conT ''Eq, conT ''Generic]]
  where
    parentName = T.pack (nameBase tyName)
    mkField :: FieldSpec -> Q [VarBangType]
    mkField (FSField name _ lbl ft rep _) = do
      let fname = mkName (T.unpack (scopedHsFieldName (scFieldNaming scope) parentName name))
      ty <- fieldTypeToTH scope lbl ft rep
      pure [(fname, Bang NoSourceUnpackedness SourceStrict, ty)]
    mkField (FSMap name _ kt vt rep) = do
      let fname = mkName (T.unpack (scopedHsFieldName (scFieldNaming scope) parentName name))
      kty <- scalarToTH kt
      vty <- fieldTypeInnerScopedQ scope rep vt
      t <- appT (appT (conT ''Map) (pure kty)) (pure vty)
      pure [(fname, Bang NoSourceUnpackedness SourceStrict, t)]
    mkField (FSOneof name _ofs) = do
      let fname = mkName (T.unpack (scopedHsFieldName (scFieldNaming scope) parentName name))
          oneofTyName = oneofSumName tyName name
      ty <- appT (conT ''Maybe) (conT oneofTyName)
      pure [(fname, Bang NoSourceUnpackedness SourceStrict, ty)]


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

{- | Sum type 'Name' for one oneof. Mirrors 'Proto.CodeGen.scopedTypeName'
conventions but uses the TH parent name as scope.
-}
oneofSumName :: Name -> Text -> Name
oneofSumName parentTy ooName =
  mkName (nameBase parentTy <> "'" <> T.unpack (snakeToPascal ooName))


-- | Constructor 'Name' for one variant of a oneof's sum type.
oneofConTHName :: Name -> Text -> Text -> Name
oneofConTHName parentTy ooName fieldN =
  mkName
    ( nameBase parentTy
        <> "'"
        <> T.unpack (snakeToPascal ooName)
        <> "'"
        <> T.unpack (snakeToPascal fieldN)
    )


{- | Emit the sum data declaration for every 'FSOneof' field on the
supplied message.
-}
mkOneofDataDecs :: ScopeCtx -> Name -> [FieldSpec] -> Q [Dec]
mkOneofDataDecs scope parentTy fields =
  mapM oneofToDec (mapMaybe extractOneof fields)
  where
    extractOneof (FSOneof n ofs) = Just (n, ofs)
    extractOneof _ = Nothing

    oneofToDec :: (Text, [(OneofField, FieldRep)]) -> Q Dec
    oneofToDec (ooName, ofs) = do
      let tyName = oneofSumName parentTy ooName
      cons <- mapM (mkCon ooName) ofs
      dataD
        (pure [])
        tyName
        []
        Nothing
        (fmap pure cons)
        [derivClause (Just StockStrategy) [conT ''Show, conT ''Eq, conT ''Generic]]

    mkCon :: Text -> (OneofField, FieldRep) -> Q Con
    mkCon ooName (f, rep) = do
      let conName = oneofConTHName parentTy ooName (oneofFieldName f)
      ty <- fieldTypeInnerScopedQ scope rep (oneofFieldType f)
      pure (NormalC conName [(Bang NoSourceUnpackedness SourceStrict, ty)])


{- | The field name used by TH-generated records for their
unknown-fields list. Derived from the type name by
lower-casing the leading character and appending
@UnknownFields@ (matching the convention the pure-Doc
codegen in "Proto.CodeGen" follows).
-}
unknownFieldsName :: Name -> Name
unknownFieldsName tyName =
  mkName (lowerFirstStr (nameBase tyName) <> "UnknownFields")


lowerFirstStr :: String -> String
lowerFirstStr (c : rest) = Data.Char.toLower c : rest
lowerFirstStr [] = []


-- | The VarBangType entry for a TH record's unknown-fields slot.
mkUnknownFieldsField :: Name -> VarBangType
mkUnknownFieldsField tyName =
  ( unknownFieldsName tyName
  , Bang NoSourceUnpackedness SourceStrict
  , AppT ListT (ConT ''Decode.UnknownField)
  )


fieldTypeToTH :: ScopeCtx -> Maybe FieldLabel -> FieldType -> FieldRep -> Q Type
fieldTypeToTH scope lbl ft rep = case lbl of
  Just Repeated -> repeatedTypeQ (fieldRepeated rep) (fieldTypeInnerScopedQ scope rep ft)
  Just Optional -> appT (conT ''Maybe) (fieldTypeInnerScopedQ scope rep ft)
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


{- | 'fieldTypeInnerQ' ignores the per-field 'FieldRep'; used for map
keys/values where we haven't threaded a rep config through yet.
Prefer 'fieldTypeInnerQWithRep' for message fields so that
custom bytes/string representations (@fieldBytes@ / @fieldString@)
actually materialize in the generated Haskell type.
-}
fieldTypeInnerQ :: FieldType -> Q Type
fieldTypeInnerQ = fieldTypeInnerQWithRep defaultFieldRep


{- | Scope-unaware variant kept for callers that don't have a
'ScopeCtx' handy (e.g. map key resolution, where the key
type is always a built-in scalar).
-}
fieldTypeInnerQWithRep :: FieldRep -> FieldType -> Q Type
fieldTypeInnerQWithRep rep = \case
  FTScalar SString -> stringTypeQ (fieldString rep)
  FTScalar SBytes -> bytesTypeQ (fieldBytes rep)
  FTScalar s -> scalarToTH s
  FTNamed n
    | Just (tyN, _) <- lookupWkt n -> conT tyN
    | otherwise -> conT (mkName (T.unpack (hsTypeName n)))


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
  FTNamed n
    | Just (tyN, _) <- lookupWkt n -> conT tyN
    | otherwise ->
        conT (mkName (T.unpack (resolveScopedHsType scope n)))


{- | Well-Known-Type registry. Maps a fully-qualified proto name
(@google.protobuf.Timestamp@) to the splice 'Name' of the
pre-generated Haskell type plus the splice 'Name' of its
@default<TypeName>@ value. Routes 'loadProto' through these
existing modules whenever a @.proto@ file references a WKT
(which @loadProto@ doesn't yet follow imports for).

Adding a WKT is a one-line entry here plus an import of the
corresponding pre-generated module from any consumer of
@loadProto@ (the imports are silent because the @ConT@ name we
spit out resolves at GHC's renamer phase, not at TH-splice
time, so consumers just have to make sure the module is in
scope at the call site).
-}
lookupWkt :: Text -> Maybe (Name, Name)
lookupWkt n = case T.unpack n of
  -- Single-message WKTs.
  "google.protobuf.Timestamp" -> Just (mkPGP "Timestamp" "Timestamp", defPGP "Timestamp" "Timestamp")
  "google.protobuf.Duration" -> Just (mkPGP "Duration" "Duration", defPGP "Duration" "Duration")
  "google.protobuf.Empty" -> Just (mkPGP "Empty" "Empty", defPGP "Empty" "Empty")
  "google.protobuf.FieldMask" -> Just (mkPGP "FieldMask" "FieldMask", defPGP "FieldMask" "FieldMask")
  "google.protobuf.Any" -> Just (mkPGP "Any" "Any", defPGP "Any" "Any")
  "google.protobuf.Struct" -> Just (mkPGP "Struct" "Struct", defPGP "Struct" "Struct")
  "google.protobuf.Value" -> Just (mkPGP "Struct" "Value", defPGP "Struct" "Value")
  "google.protobuf.ListValue" -> Just (mkPGP "Struct" "ListValue", defPGP "Struct" "ListValue")
  "google.protobuf.NullValue" ->
    Just
      ( mkPGP "Struct" "NullValue"
      , -- NullValue is an enum; default is its single value.
        mkName "Proto.Google.Protobuf.WellKnownTypes.Struct.NullValue'NullValue"
      )
  -- Wrapper messages (all in Proto.Google.Protobuf.WellKnownTypes.Wrappers).
  "google.protobuf.DoubleValue" -> Just (mkPGP "Wrappers" "DoubleValue", defPGP "Wrappers" "DoubleValue")
  "google.protobuf.FloatValue" -> Just (mkPGP "Wrappers" "FloatValue", defPGP "Wrappers" "FloatValue")
  "google.protobuf.Int64Value" -> Just (mkPGP "Wrappers" "Int64Value", defPGP "Wrappers" "Int64Value")
  "google.protobuf.UInt64Value" -> Just (mkPGP "Wrappers" "UInt64Value", defPGP "Wrappers" "UInt64Value")
  "google.protobuf.Int32Value" -> Just (mkPGP "Wrappers" "Int32Value", defPGP "Wrappers" "Int32Value")
  "google.protobuf.UInt32Value" -> Just (mkPGP "Wrappers" "UInt32Value", defPGP "Wrappers" "UInt32Value")
  "google.protobuf.BoolValue" -> Just (mkPGP "Wrappers" "BoolValue", defPGP "Wrappers" "BoolValue")
  "google.protobuf.StringValue" -> Just (mkPGP "Wrappers" "StringValue", defPGP "Wrappers" "StringValue")
  "google.protobuf.BytesValue" -> Just (mkPGP "Wrappers" "BytesValue", defPGP "Wrappers" "BytesValue")
  _ -> Nothing
  where
    mkPGP modSuffix tyN = mkName ("Proto.Google.Protobuf.WellKnownTypes." <> modSuffix <> "." <> tyN)
    defPGP modSuffix tyN = mkName ("Proto.Google.Protobuf.WellKnownTypes." <> modSuffix <> ".default" <> tyN)


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
      parentName = T.pack (nameBase tyName)
  sig <- sigD defName (conT tyName)
  defFields <-
    mapM
      ( \fs -> do
          val <- defaultValueExpr scope fs
          pure (mkName (T.unpack (scopedHsFieldName (scFieldNaming scope) parentName (fsFieldName fs))), val)
      )
      fields
  -- Every TH-generated record now carries an empty unknown-fields
  -- list. Proto2 extensions travel through this field; see
  -- "Proto.Extension" for the typed accessors.
  let ufDefault = (unknownFieldsName tyName, ListE [])
  body <-
    valD
      (varP defName)
      (normalB (recConE tyName (fmap pure (defFields <> [ufDefault]))))
      []
  pure [sig, body]


defaultValueExpr :: ScopeCtx -> FieldSpec -> Q Exp
defaultValueExpr scope (FSField _ _ lbl ft rep _) = case lbl of
  Just Repeated -> emptyRepeatedQ (fieldRepeated rep)
  Just Optional -> conE 'Nothing
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
          enumZeroDefaultE scope n
      | otherwise -> conE 'Nothing
defaultValueExpr _ (FSMap {}) = [|Map.empty|]
defaultValueExpr _ (FSOneof _ _) = conE 'Nothing


{- | Default value for a singular enum-typed field. Looks up the
enum definition in 'scTopLevels' and returns the constructor
whose proto number is 0; falls back to @toEnum 0@ when no such
constructor exists.
-}
enumZeroDefaultE :: ScopeCtx -> Text -> Q Exp
enumZeroDefaultE scope protoTy = case findEnum (scTopLevels scope) of
  Just ed -> case lookupZero ed of
    Just con -> conE (mkName (T.unpack con))
    Nothing -> [|toEnum 0|]
  Nothing -> [|toEnum 0|]
  where
    leaf t = case T.splitOn (T.pack ".") t of
      [] -> t
      ps -> last ps
    matches t = leaf t == leaf protoTy
    findEnum tls = case tls of
      [] -> Nothing
      (TLEnum ed : _)
        | matches (enumName ed) -> Just ed
      (TLEnum _ : rest) -> findEnum rest
      (TLMessage m : rest) -> case findInMsg (msgElements m) of
        Just ed -> Just ed
        Nothing -> findEnum rest
      (_ : rest) -> findEnum rest
    findInMsg [] = Nothing
    findInMsg (MEEnum ed : _) | matches (enumName ed) = Just ed
    findInMsg (MEEnum _ : rest) = findInMsg rest
    findInMsg (MEMessage m : rest) = case findInMsg (msgElements m) of
      Just ed -> Just ed
      Nothing -> findInMsg rest
    findInMsg (_ : rest) = findInMsg rest

    enumParents = findTypeScope scope protoTy

    lookupZero ed =
      let zeros = filter (\ev -> evNumber ev == 0) (enumValues ed)
      in case zeros of
          (ev : _) -> Just (scopedHsEnumCon enumParents (enumName ed) (evName ev))
          [] -> Nothing


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
enumToDecls' = enumToDecls'' T.empty []


{- | Enum splice with the proto package + parent-message scope
threaded through. Parent scope drives the Haskell type and
constructor names ('TestAllTypesProto3'NestedEnum' /
'TestAllTypesProto3'NestedEnum'Foo') so two parent messages
can declare identically-named inner enums without colliding.
-}
enumToDecls'' :: Text -> [Text] -> THHooks -> EnumDef -> Q [Dec]
enumToDecls'' pkg parents hooks ed = do
  let
    -- Scoped Haskell type name (collapses to plain
    -- 'hsTypeName' when 'parents' is empty).
    hsTy = scopedHsTypeName parents (enumName ed)
    tyName = mkName (T.unpack hsTy)
    conNameFor evName' =
      mkName (T.unpack (scopedHsEnumCon parents (enumName ed) evName'))
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
      ]
  enumInst <- mkEnumInstance tyName parents ed
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
    fqEnumName =
      if T.null pkg
        then enumName ed
        else pkg <> T.singleton '.' <> enumName ed
  protoEnumDec <-
    PTM.mkProtoEnumInstance
      tyName
      fqEnumName
      values
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
mkEnumInstance :: Name -> [Text] -> EnumDef -> Q Dec
mkEnumInstance tyName parents ed = do
  -- Collapse aliases so multiple constructors with the same wire
  -- number don't introduce overlapping @fromEnum@ clauses (proto
  -- @allow_alias = true@ is rare but legal). The first occurrence
  -- in declaration order wins, matching what the pure-text codegen
  -- does in 'enumPrimaryValues'.
  let primaryByNum = primaryValues (enumValues ed)
      unknownCon = unknownConNameFor parents (enumName ed)
      -- 'toEnum n -> ConName' clauses, one per distinct wire
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
  where
    enumConName ev =
      mkName (T.unpack (scopedHsEnumCon parents (enumName ed) (evName ev)))


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
instance's two methods read and write the record's
unknown-fields slot, which is where extension payloads (and any
other forward-compatible unknown tags) live.

The class and method names are referenced via their bound 'Name's
('Ext.messageUnknownFields' / 'Ext.setMessageUnknownFields') so
the generated splice doesn't require the user's module to
already have @import qualified Proto.Extension@ in scope.
-}
mkHasExtensionsInstance :: Name -> Text -> Q [Dec]
mkHasExtensionsInstance tyName _protoName = do
  let ufName = unknownFieldsName tyName
  msgVar <- newName "msg"
  ufsVar <- newName "ufs"
  inst <-
    instanceD
      (pure [])
      [t|Ext.HasExtensions $(conT tyName)|]
      [ funD
          'Ext.messageUnknownFields
          [clause [] (normalB (varE ufName)) []]
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
extendToDecls :: Text -> Text -> [FieldDef] -> Q [Dec]
extendToDecls pkg ownerProtoName fields =
  concat <$> mapM (oneExtensionDec ownerHsName ownerPrefix parentFqn pkg) fields
  where
    ownerHsName = mkName (T.unpack (hsTypeName (lastProtoSegment ownerProtoName)))
    ownerPrefix = lowerFirst (hsTypeName (lastProtoSegment ownerProtoName))
    parentFqn
      | T.null pkg = ownerProtoName
      | T.isInfixOf (T.singleton '.') ownerProtoName = ownerProtoName
      | otherwise = pkg <> T.singleton '.' <> ownerProtoName


lastProtoSegment :: Text -> Text
lastProtoSegment t = case T.splitOn "." t of
  [] -> t
  parts -> last parts


{- | Generate the declarations for one extension field. Singular
fields produce a 'Ext.Extension' descriptor; repeated fields
produce a 'Ext.RepeatedExtension' (with the
'Ext.reIsPacked' flag set per the field's @[packed = ...]@
option, defaulting to 'False' for proto2 / 'True' for fixed-width
packable scalars in proto3).
-}
oneExtensionDec :: Name -> Text -> Text -> Text -> FieldDef -> Q [Dec]
oneExtensionDec ownerHs ownerPrefix parentFqn pkg fd = case fieldLabel fd of
  Just Repeated ->
    case thExtensionPayloadCore (fieldType fd) of
      Nothing -> pure []
      Just (hsTy, extConName) -> do
        let extName =
              mkName
                ( T.unpack
                    ( escapeReserved
                        (ownerPrefix <> upperFirst (snakeToCamel (fieldName fd)))
                    )
                )
            num = unFieldNumber (fieldNumber fd)
            extConE = conE (mkName ("Ext." <> T.unpack extConName))
            packed = case fieldType fd of
              FTScalar s -> packableScalar s
              _ -> False
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
    case thExtensionPayloadCore (fieldType fd) of
      Nothing -> pure []
      Just (hsTy, extConName) -> do
        let extName =
              mkName
                ( T.unpack
                    ( escapeReserved
                        (ownerPrefix <> upperFirst (snakeToCamel (fieldName fd)))
                    )
                )
            num = unFieldNumber (fieldNumber fd)
            extConE = conE (mkName ("Ext." <> T.unpack extConName))
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
        regDecs <-
          extensionJsonRegistrationDecs
            parentFqn
            pkg
            (fieldName fd)
            num
            extConName
        pure (sig : body : regDecs)


{- | Emit a top-level @register<ExtName>Json :: IO ()@ binding
that, when called, registers the extension's JSON codec in
the runtime registry from "Proto.Internal.JSON.Extension". The user
calls 'forceLoadProtoExtensionRegistrations' (also generated
by 'loadProto', collected per-file at the bottom) to drain
the file's registrations on startup.
-}
extensionJsonRegistrationDecs
  :: Text
  -- ^ Parent message FQN
  -> Text
  -- ^ Extension's own proto package (for the FQN)
  -> Text
  -- ^ Extension proto field name
  -> Int
  -- ^ Wire field number
  -> Text
  -- ^ ExtensionType constructor name (e.g. "ExtInt32")
  -> Q [Dec]
extensionJsonRegistrationDecs parentFqn pkg extLeaf num extConName = do
  let extFqn = (if T.null pkg then extLeaf else pkg <> T.singleton '.' <> extLeaf)
      regName =
        mkName
          ( "registerExt_"
              <> T.unpack (T.replace (T.singleton '.') (T.pack "_") extFqn)
          )
      extConE = ConE (mkName ("Ext." <> T.unpack extConName))
  sig <- sigD regName [t|PJExt.ExtensionRegistry|]
  body <-
    valD
      (varP regName)
      ( normalB
          [|
            PJExt.registerExtensionJson
              $(textLitE parentFqn)
              PJExt.ExtJsonCodec
                { PJExt.ejcExtensionFqn = $(textLitE extFqn)
                , PJExt.ejcFieldNumber = $(litE (IntegerL (fromIntegral num)))
                , PJExt.ejcParseValue =
                    PJExt.parseExtValueViaConstructor
                      $(pure extConE)
                      $(litE (IntegerL (fromIntegral num)))
                , PJExt.ejcEncodeValue =
                    PJExt.encodeExtValueViaConstructor
                      $(pure extConE)
                }
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
thExtensionPayloadCore :: FieldType -> Maybe (Type, Text)
thExtensionPayloadCore (FTScalar s) = Just $ case s of
  SDouble -> (ConT ''Double, "ExtDouble")
  SFloat -> (ConT ''Float, "ExtFloat")
  SInt32 -> (ConT ''Int32, "ExtInt32")
  SInt64 -> (ConT ''Int64, "ExtInt64")
  SUInt32 -> (ConT ''Word32, "ExtUInt32")
  SUInt64 -> (ConT ''Word64, "ExtUInt64")
  SSInt32 -> (ConT ''Int32, "ExtSInt32")
  SSInt64 -> (ConT ''Int64, "ExtSInt64")
  SFixed32 -> (ConT ''Word32, "ExtFixed32")
  SFixed64 -> (ConT ''Word64, "ExtFixed64")
  SSFixed32 -> (ConT ''Int32, "ExtSFixed32")
  SSFixed64 -> (ConT ''Int64, "ExtSFixed64")
  SBool -> (ConT ''Bool, "ExtBool")
  SString -> (ConT ''Text, "ExtString")
  SBytes -> (ConT ''ByteString, "ExtBytes")
thExtensionPayloadCore (FTNamed _) =
  -- Named-type extensions round-trip their raw encoded bytes;
  -- callers decode lazily through the matching message decoder.
  Just (ConT ''ByteString, "ExtMessage")


-- | Whether a scalar is permitted to be packed on the wire.
packableScalar :: ScalarType -> Bool
packableScalar = \case
  SString -> False
  SBytes -> False
  _ -> True


upperFirst :: Text -> Text
upperFirst t = case T.uncons t of
  Just (c, rest) -> T.cons (Data.Char.toUpper c) rest
  Nothing -> t


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
fieldSpecToProtoField :: ScopeCtx -> Name -> FieldSpec -> Q PDI.ProtoField
fieldSpecToProtoField scope parentTy (FSField name num lbl ft rep opts) = do
  let parentName = T.pack (nameBase parentTy)
      sel = mkName (T.unpack (scopedHsFieldName (scFieldNaming scope) parentName name))
      pft = fieldTypeToBridge scope ft
      mode = case (lbl, pft) of
        (Just Repeated, PDI.PFScalar sc)
          | PDI.scalarPackable sc -> packedModeFor scope opts
        -- Enums are also packable in proto3 (the wire is a varint
        -- per element); default-packed unless the user wrote
        -- @[packed = false]@ explicitly.
        (Just Repeated, PDI.PFEnum) -> packedModeFor scope opts
        _ -> PDI.ModeUnpacked
      -- Singular submessage fields are implicitly Maybe-wrapped at
      -- the data-declaration layer (see 'fieldTypeToTH'), so they
      -- decode/encode as 'FKMaybe' too. Singular enum fields stay
      -- bare; enums have a zero value and follow scalar default-
      -- skip semantics.
      isImplicitOptional = case (lbl, pft) of
        (Nothing, PDI.PFSubmessage) -> True
        _ -> False
      kind = case lbl of
        Just Repeated -> PDI.FKRepeated (fieldRepeated rep) mode
        Just Optional -> PDI.FKMaybe
        _
          | isImplicitOptional -> PDI.FKMaybe
          | otherwise -> PDI.FKBare
      inner = innerHsType scope ft rep
      base = PDI.protoField sel num kind pft inner
  pure
    base
      { PDI.pfStringAdapter = fieldString rep
      , PDI.pfBytesAdapter = fieldBytes rep
      }
fieldSpecToProtoField scope parentTy (FSMap name num kt vt rep) =
  case scalarToBridgeMapKey kt of
    Nothing ->
      fail
        ( "Proto.TH: map field '"
            <> T.unpack name
            <> "' has an invalid map-key type ("
            <> scalarStr kt
            <> "). Proto3 only permits integral and string map keys."
        )
    Just mks ->
      let parentName = T.pack (nameBase parentTy)
          sel = mkName (T.unpack (scopedHsFieldName (scFieldNaming scope) parentName name))
          pft = fieldTypeToBridge scope vt
          inner = innerHsType scope vt rep
          base = PDI.protoField sel num (PDI.FKMap mks) pft inner
      in pure
          base
            { PDI.pfStringAdapter = fieldString rep
            , PDI.pfBytesAdapter = fieldBytes rep
            }
fieldSpecToProtoField scope parentTy (FSOneof name ofs) = do
  let parentName = T.pack (nameBase parentTy)
      sel = mkName (T.unpack (scopedHsFieldName (scFieldNaming scope) parentName name))
      sumTy = oneofSumName parentTy name
      carrier = AppT (ConT ''Maybe) (ConT sumTy)
      variants = fmap (oneofVariantToBridge scope parentTy name) ofs
  -- @pfTag@/@pfType@/@pfInnerTy@ are documented as ignored
  -- by the body builders for FKOneof. The carrier type is
  -- still useful for clarity / future debugging.
  pure (PDI.protoField sel 0 (PDI.FKOneof variants) PDI.PFSubmessage carrier)


{- | Pick packed vs. unpacked for a repeated packable scalar field.

* Proto3: packed by default; @[packed = false]@ flips to unpacked.
* Proto2: unpacked by default; @[packed = true]@ flips to packed.
* Editions: defer to 'featureRepeatedFieldEncoding' on the
  resolved feature set; the caller usually wants 'PackedEncoding'
  (the post-2023 default) but @[packed = false]@ still wins.
-}
packedModeFor :: ScopeCtx -> [OptionDef] -> PDI.RepeatedMode
packedModeFor scope opts =
  let explicit = lookupSimpleOption (T.pack "packed") opts >>= optionAsBool
      -- For editions, apply file-level then field-level features.* overrides.
      defaultPacked = case scSyntax scope of
        Proto3 -> True
        Proto2 -> False
        Editions ed ->
          let fileFs  = resolveFileFeatures (Editions ed) (scFileOptions scope)
              fieldFs = resolveFieldFeatures fileFs opts
          in case featureRepeatedFieldEncoding fieldFs of
               PackedEncoding   -> True
               ExpandedEncoding -> False
      packed = Data.Maybe.fromMaybe defaultPacked explicit
  in if packed then PDI.ModePacked else PDI.ModeUnpacked


{- | Build the bridge's 'PDI.OneofVariant' for one arm of a oneof.
The constructor name is computed by 'oneofConTHName' so it
matches the splice in 'mkOneofDataDecs' exactly. The variant's
string / bytes rep comes from the resolved 'FieldRep' (looked
up in 'RepConfig' under @(parentMessage, oneofFieldName)@ in
'extractMessageFields'), so per-variant overrides like \"this
variant only is lazy ByteString\" cleanly survive the bridge.
-}
oneofVariantToBridge :: ScopeCtx -> Name -> Text -> (OneofField, FieldRep) -> PDI.OneofVariant
oneofVariantToBridge scope parentTy ooName (f, rep) =
  let base =
        PDI.oneofVariant
          (oneofConTHName parentTy ooName (oneofFieldName f))
          (unFieldNumber (oneofFieldNumber f))
          (innerHsType scope (oneofFieldType f) rep)
          (fieldTypeToBridge scope (oneofFieldType f))
  in base
      { PDI.ovStringAdapter = fieldString rep
      , PDI.ovBytesAdapter = fieldBytes rep
      }


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
innerHsType :: ScopeCtx -> FieldType -> FieldRep -> Type
innerHsType scope ft rep = case ft of
  FTScalar SString -> stringHsType (fieldString rep)
  FTScalar SBytes -> bytesHsType (fieldBytes rep)
  FTScalar SInt32 -> ConT ''Int32
  FTScalar SInt64 -> ConT ''Int64
  FTScalar SUInt32 -> ConT ''Word32
  FTScalar SUInt64 -> ConT ''Word64
  FTScalar SSInt32 -> ConT ''Int32
  FTScalar SSInt64 -> ConT ''Int64
  FTScalar SFixed32 -> ConT ''Word32
  FTScalar SFixed64 -> ConT ''Word64
  FTScalar SSFixed32 -> ConT ''Int32
  FTScalar SSFixed64 -> ConT ''Int64
  FTScalar SDouble -> ConT ''Double
  FTScalar SFloat -> ConT ''Float
  FTScalar SBool -> ConT ''Bool
  FTNamed n
    | Just (tyN, _) <- lookupWkt n -> ConT tyN
    | otherwise -> ConT (mkName (T.unpack (resolveScopedHsType scope n)))


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
fieldSpecToMetaField :: ScopeCtx -> Name -> FieldSpec -> PTM.MetaField
fieldSpecToMetaField scope parentTy fs = case fs of
  FSField name num lbl ft rep opts ->
    let sel = mkName (T.unpack (scopedHsFieldName (scFieldNaming scope) parentName name))
        jsonNm = jsonNameFromOpts opts (protoJsonName name)
        repeatedKind = case repeatedBaseRep (fieldRepeated rep) of
          VectorRep -> PTM.MFKVector
          ListRep -> PTM.MFKList
          SeqRep -> PTM.MFKSeq
        kind = case lbl of
          Just Repeated -> repeatedKind
          Just Optional -> PTM.MFKMaybe
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
            | Just w <- lookupWktShape n -> PTM.JSWktRepeated w
            | isEnumName scope n -> PTM.JSRepeatedEnum
            | otherwise -> PTM.JSRepeatedMessage
          (Just Optional, FTScalar s) -> PTM.JSMaybe (jsScalarOf s)
          (Just Optional, FTNamed n)
            | Just w <- lookupWktShape n -> PTM.JSWktMaybe w
            | isEnumName scope n -> PTM.JSEnumMaybe
            | otherwise -> PTM.JSMessage
          (_, FTScalar s) -> PTM.JSScalar (jsScalarOf s)
          (_, FTNamed n)
            | Just w <- lookupWktShape n -> PTM.JSWkt w
            | isEnumName scope n -> PTM.JSEnum
            | otherwise -> PTM.JSMessage
    in PTM.MetaField
        { PTM.mfSelector = sel
        , PTM.mfProtoName = name
        , PTM.mfJsonName = jsonNm
        , PTM.mfNumber = num
        , PTM.mfTypeDesc = fieldTypeDescE scope ft
        , PTM.mfLabel = protoLabelE lbl
        , PTM.mfKind = kind
        , PTM.mfJsonKind = jsonKind
        , PTM.mfBytesShape = bytesShape
        , PTM.mfJsonShape = jsonShape
        }
  FSMap name num kt vt rep ->
    let sel = mkName (T.unpack (scopedHsFieldName (scFieldNaming scope) parentName name))
        jsonNm = protoJsonName name
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
            | isEnumName scope n -> PTM.JSMapEnum (jsScalarOf kt)
            | otherwise -> PTM.JSMapMessage (jsScalarOf kt)
    in PTM.MetaField
        { PTM.mfSelector = sel
        , PTM.mfProtoName = name
        , PTM.mfJsonName = jsonNm
        , PTM.mfNumber = num
        , PTM.mfTypeDesc = mapTypeDescE scope kt vt
        , PTM.mfLabel = [|PS.LabelOptional|]
        , PTM.mfKind = PTM.MFKMap
        , PTM.mfJsonKind = jsonKind
        , PTM.mfBytesShape = bytesShape
        , PTM.mfJsonShape = jsonShape
        }
  FSOneof name ofs ->
    let sel = mkName (T.unpack (scopedHsFieldName (scFieldNaming scope) parentName name))
        jsonNm = protoJsonName name
        variants = fmap (oneofVariantJson scope parentTy name) ofs
    in PTM.MetaField
        { PTM.mfSelector = sel
        , PTM.mfProtoName = name
        , PTM.mfJsonName = jsonNm
        , PTM.mfNumber = 0
        , PTM.mfTypeDesc = [|PS.MessageType $(textLitE name)|]
        , PTM.mfLabel = [|PS.LabelOptional|]
        , PTM.mfKind = PTM.MFKOneof
        , PTM.mfJsonKind = PTM.JKNormal
        , PTM.mfBytesShape = PTM.SBStrict
        , PTM.mfJsonShape = PTM.JSOneof variants
        }
  where
    parentName = T.pack (nameBase parentTy)


{- | Build the JSON-side shape for one oneof variant. The variant's
JSON key is the camelCase form of the proto-side field name
(or @json_name@ when the proto declared one); the payload
shape is dispatched on the variant's value type.
-}
oneofVariantJson
  :: ScopeCtx
  -> Name
  -- ^ Parent type name.
  -> Text
  -- ^ Oneof field name (used for naming only).
  -> (OneofField, FieldRep)
  -> PTM.OneofVariantJson
oneofVariantJson scope parentTy _ooName (f, _rep) =
  let conN = oneofConTHName parentTy _ooName (oneofFieldName f)
      jsonKey =
        jsonNameFromOpts
          (oneofFieldOptions f)
          (protoJsonName (oneofFieldName f))
      shape = case oneofFieldType f of
        FTScalar s -> PTM.OVScalar (jsScalarOf s)
        FTNamed n
          -- Special-case the NullValue WKT: in JSON it's a
          -- bare 'null', not a quoted enum name, so the oneof
          -- parser/encoder needs to know.
          | Just PTM.WktNullValue <- lookupWktShape n ->
              PTM.OVNullValue
          | isEnumName scope n -> PTM.OVEnum
          | otherwise -> PTM.OVMessage
  in PTM.OneofVariantJson
      { PTM.ovjConstructor = conN
      , PTM.ovjJsonKey = jsonKey
      , PTM.ovjShape = shape
      }


{- | Project a proto FQN to the metadata-bridge's 'WktShape' tag
when the FQN names a Well-Known-Type the JSON splice has a
canonical encoder for.
-}
lookupWktShape :: Text -> Maybe PTM.WktShape
lookupWktShape n = case T.unpack n of
  "google.protobuf.Timestamp" -> Just PTM.WktTimestamp
  "google.protobuf.Duration" -> Just PTM.WktDuration
  "google.protobuf.FieldMask" -> Just PTM.WktFieldMask
  "google.protobuf.Struct" -> Just PTM.WktStruct
  "google.protobuf.Value" -> Just PTM.WktValue
  "google.protobuf.ListValue" -> Just PTM.WktListValue
  "google.protobuf.Any" -> Just PTM.WktAny
  "google.protobuf.Empty" -> Just PTM.WktEmpty
  "google.protobuf.NullValue" -> Just PTM.WktNullValue
  "google.protobuf.BoolValue" -> Just PTM.WktWrapBool
  "google.protobuf.Int32Value" -> Just PTM.WktWrapInt32
  "google.protobuf.Int64Value" -> Just PTM.WktWrapInt64
  "google.protobuf.UInt32Value" -> Just PTM.WktWrapUInt32
  "google.protobuf.UInt64Value" -> Just PTM.WktWrapUInt64
  "google.protobuf.FloatValue" -> Just PTM.WktWrapFloat
  "google.protobuf.DoubleValue" -> Just PTM.WktWrapDouble
  "google.protobuf.StringValue" -> Just PTM.WktWrapString
  "google.protobuf.BytesValue" -> Just PTM.WktWrapBytes
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
oneofSatelliteDecs :: Name -> FieldSpec -> Q [Dec]
oneofSatelliteDecs parentTy = \case
  FSOneof name ofs -> do
    let sumTy = oneofSumName parentTy name
        cons =
          fmap
            ( \(f, _) ->
                oneofConTHName parentTy name (oneofFieldName f)
            )
            ofs
    aeson <- PTM.mkOneofAesonInstances sumTy
    hashable <- PTM.mkOneofHashableInstance sumTy cons
    pure (aeson <> [hashable])
  _ -> pure []
