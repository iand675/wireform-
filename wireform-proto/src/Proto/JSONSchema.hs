{-# LANGUAGE OverloadedStrings #-}

{- | Proto → JSON Schema (draft 2020-12, the dialect OpenAPI 3.1 embeds).

This is the /transport-agnostic/ half of schema-derived doc generation: it
turns a parsed '.proto' schema into the @#\/components\/schemas@ objects an
OpenAPI (or bare JSON Schema) document references. It knows nothing about
HTTP, Connect, gRPC, paths, or content-types — a gRPC-Web or Twirp OpenAPI
emitter would reuse it verbatim. The Connect-specific HTTP shaping lives in
@Network.Connect.OpenAPI@ (in @wireform-connect@).

== Fidelity contract

OpenAPI describes the /JSON codec/, not the protobuf wire format, so every
schema here must match the bytes the proto3-canonical-JSON codec actually
emits (see "Proto.Internal.JSON" / "Proto.Internal.JSON.WellKnown"). The
load-bearing consequences:

* Property names are the proto3 JSON names — 'protoJsonName' (lowerCamelCase,
  the exact function the codegen uses), overridden by an explicit
  @[json_name = "..."]@ field option.
* 64-bit integers (@int64@\/@uint64@\/@fixed64@\/@sfixed64@\/@sint64@) are
  __strings__, not numbers (JS precision), matching the codec.
* @bytes@ is a base64 string (@format: byte@).
* Well-known types are __inlined__ at their reference sites with their
  canonical JSON shape (@Timestamp@→date-time string, @Duration@→string,
  @Struct@→object, @Value@→any, wrappers→their bare inner value, …) rather
  than emitted as components.
* Enums render as a string @enum@ of value names (the canonical JSON form).
* @oneof@ members flatten into the enclosing object as optional properties
  (how the codec serialises them); proto3 has no JSON-required fields, so no
  @required@ list is emitted.
-}
module Proto.JSONSchema (
  -- * Building the type environment
  SchemaEnv,
  buildSchemaEnv,

  -- * Emitting schemas
  componentSchemas,
  componentSchemasWith,
  messageSchemaFor,
  messageSchemaForWith,
  enumSchemaFor,
  fieldTypeSchema,

  -- * External annotation (composable — 'SchemaOptions' is a 'Monoid')
  SchemaOptions (..),
  defaultSchemaOptions,
  FieldConstraints (..),
  fieldConstraints,
  jsonNameOf,

  -- * Built-in annotators
  deprecationSchemaOptions,

  -- * Name resolution
  resolveTypeFqn,
  refForFqn,

  -- * Well-known types
  wellKnownSchema,
  isWellKnown,
) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Types (Pair)
import Data.Aeson.Key qualified as AKey
import Data.Aeson.KeyMap qualified as AKM
import Data.List (tails)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as V
import Proto.CodeGen (protoJsonName)
import Proto.IDL.AST
import Proto.IDL.Annotations (lookupSimpleOption, optionAsString)
import Proto.IDL.Options (isDeprecated)


-- ---------------------------------------------------------------------------
-- Type environment
-- ---------------------------------------------------------------------------

{- | A resolved-name environment: every message and enum reachable from the
target file(s), keyed by fully-qualified proto name (@pkg.Outer.Inner@),
carrying the scope needed to resolve /relative/ type references that appear
inside it.
-}
newtype SchemaEnv = SchemaEnv {seTypes :: Map Text TypeEntry}


data TypeEntry
  = -- | message def + package + scope-including-own-name
    TEMessage MessageDef Text [Text]
  | -- | enum def + package + scope-including-own-name
    TEEnum EnumDef Text [Text]


{- | Build a 'SchemaEnv' from a set of parsed proto files (typically the
target file plus its transitive imports, flattened). Nested messages and
enums are recorded under their own fully-qualified names.
-}
buildSchemaEnv :: [ProtoFile] -> SchemaEnv
buildSchemaEnv files =
  SchemaEnv (Map.unions (fmap fileTypes files))
  where
    fileTypes pf =
      let pkg = fromMaybe "" (protoPackage pf)
      in Map.unions (fmap (topLevelTypes pkg []) (protoTopLevels pf))


topLevelTypes :: Text -> [Text] -> TopLevel -> Map Text TypeEntry
topLevelTypes pkg scope = \case
  TLMessage m -> messageTypes pkg scope m
  TLEnum e -> enumTypes pkg scope e
  _ -> Map.empty


messageTypes :: Text -> [Text] -> MessageDef -> Map Text TypeEntry
messageTypes pkg scope m =
  let scope' = scope <> [msgName m]
      fqn = qualify pkg scope'
      nested = Map.unions (fmap (elementTypes pkg scope') (msgElements m))
  in Map.insert fqn (TEMessage m pkg scope') nested


elementTypes :: Text -> [Text] -> MessageElement -> Map Text TypeEntry
elementTypes pkg scope = \case
  MEMessage inner -> messageTypes pkg scope inner
  MEEnum e -> enumTypes pkg scope e
  _ -> Map.empty


enumTypes :: Text -> [Text] -> EnumDef -> Map Text TypeEntry
enumTypes pkg scope e =
  let scope' = scope <> [enumName e]
  in Map.singleton (qualify pkg scope') (TEEnum e pkg scope')


qualify :: Text -> [Text] -> Text
qualify pkg scope
  | T.null pkg = T.intercalate "." scope
  | otherwise = pkg <> "." <> T.intercalate "." scope


-- ---------------------------------------------------------------------------
-- Name resolution (mirrors Proto.CodeGen.resolveTypeWithScope)
-- ---------------------------------------------------------------------------

{- | Resolve a (possibly relative) proto type name to a fully-qualified name,
given the package and enclosing scope of the reference site. Tries a
fully-qualified match, then package-prefixed, then each enclosing-scope
prefix, then a unique dotted-suffix match — the same candidate order the
Haskell codegen uses.
-}
resolveTypeFqn :: SchemaEnv -> Text -> [Text] -> Text -> Maybe Text
resolveTypeFqn env pkg scope name =
  let reg = seTypes env
      scopedCandidates =
        fmap (\s -> pkg <> "." <> T.intercalate "." s <> "." <> name) (nonEmptyTails scope)
      candidates = name : (pkg <> "." <> name) : scopedCandidates
  in case firstMember reg candidates of
       Just fqn -> Just fqn
       Nothing ->
         let suffix = "." <> name
             matches = filter (\k -> k == name || suffix `T.isSuffixOf` k) (Map.keys reg)
         in case matches of
              (fqn : _) -> Just fqn
              [] -> Nothing
  where
    nonEmptyTails xs = filter (not . null) (tails xs)
    firstMember _ [] = Nothing
    firstMember reg (c : cs)
      | Map.member c reg = Just c
      | otherwise = firstMember reg cs


-- | A @$ref@ to a named component schema by fully-qualified proto name.
refForFqn :: Text -> Value
refForFqn fqn = object ["$ref" .= ("#/components/schemas/" <> fqn)]


-- ---------------------------------------------------------------------------
-- Component emission
-- ---------------------------------------------------------------------------

{- | The @#\/components\/schemas@ map: one entry per user-defined (non
well-known) message and enum in the environment, keyed by fully-qualified
proto name. Well-known types are inlined at reference sites, so they never
appear here.
-}
componentSchemas :: SchemaEnv -> [(Text, Value)]
componentSchemas = componentSchemasWith defaultSchemaOptions


-- | Like 'componentSchemas' but merges external per-field / per-message
-- annotations (e.g. protovalidate rules → JSON Schema validation keywords).
componentSchemasWith :: SchemaOptions -> SchemaEnv -> [(Text, Value)]
componentSchemasWith opts env =
  mapMaybe component (Map.toList (seTypes env))
  where
    component (fqn, entry)
      | isWellKnown fqn = Nothing
      | otherwise = Just (fqn, schemaForEntry opts env fqn entry)

-- | How an external annotator contributes to a field's schema: extra JSON
-- Schema keywords merged into the field's schema object, and whether the field
-- is required (added to the message's @required@ list).
data FieldConstraints = FieldConstraints
  { fcKeywords :: ![Pair]
  -- ^ JSON Schema keywords to merge into the field schema (e.g. @minLength@).
  , fcRequired :: !Bool
  -- ^ Whether the field is required (contributes to the object's @required@).
  }
  deriving stock (Show)


-- | Combine two field contributions: concatenate keywords (left wins on key
-- clash at merge time), OR the required flags.
instance Semigroup FieldConstraints where
  FieldConstraints k1 r1 <> FieldConstraints k2 r2 = FieldConstraints (k1 <> k2) (r1 || r2)


instance Monoid FieldConstraints where
  mempty = FieldConstraints [] False


-- | Smart constructor for a field contribution.
fieldConstraints :: [Pair] -> Bool -> FieldConstraints
fieldConstraints = FieldConstraints


{- | Hooks for enriching the emitted schema with information the proto AST alone
doesn't carry (protovalidate rules, deprecation, arbitrary @x-@ extensions).
All are keyed by the fully-qualified type name; the field hook additionally by
the /proto/ field name (not the JSON name).

'SchemaOptions' is a 'Monoid': compose independent annotators with @<>@ (field
contributions combine via 'FieldConstraints'' 'Semigroup'; message / enum
keyword lists concatenate). 'mempty' / 'defaultSchemaOptions' is the no-op that
leaves the base walk untouched.
-}
data SchemaOptions = SchemaOptions
  { soFieldAnnotator :: Text -> Text -> Maybe FieldConstraints
  -- ^ @messageFqn -> protoFieldName -> constraints@.
  , soMessageAnnotator :: Text -> [Pair]
  -- ^ @messageFqn -> extra message-level keywords@ (e.g. @x-cel@).
  , soEnumAnnotator :: Text -> [Pair]
  -- ^ @enumFqn -> extra enum-level keywords@ (e.g. @deprecated@).
  }


instance Semigroup SchemaOptions where
  a <> b =
    SchemaOptions
      { soFieldAnnotator = \fqn fld -> soFieldAnnotator a fqn fld <> soFieldAnnotator b fqn fld
      , soMessageAnnotator = \fqn -> soMessageAnnotator a fqn <> soMessageAnnotator b fqn
      , soEnumAnnotator = \fqn -> soEnumAnnotator a fqn <> soEnumAnnotator b fqn
      }


instance Monoid SchemaOptions where
  mempty = SchemaOptions (\_ _ -> Nothing) (const []) (const [])


-- | No external annotations (@= 'mempty'@).
defaultSchemaOptions :: SchemaOptions
defaultSchemaOptions = mempty


{- | A built-in annotator that carries the standard proto @deprecated@ option
through as OpenAPI\/JSON-Schema @deprecated: true@, on messages, fields, and
enums. Pass the same files used to build the 'SchemaEnv'. Compose it with
others (e.g. protovalidate) via @<>@:

> deprecationSchemaOptions files <> protovalidateSchemaOptions rules
-}
deprecationSchemaOptions :: [ProtoFile] -> SchemaOptions
deprecationSchemaOptions files =
  mempty
    { soFieldAnnotator = \fqn fld ->
        if Set.member (fqn, fld) depFields
          then Just (fieldConstraints ["deprecated" .= True] False)
          else Nothing
    , soMessageAnnotator = \fqn -> if Set.member fqn depMsgs then ["deprecated" .= True] else []
    , soEnumAnnotator = \fqn -> if Set.member fqn depEnums then ["deprecated" .= True] else []
    }
  where
    (depMsgs, depFields, depEnums) = collectDeprecated files


-- | Collect the FQNs of deprecated messages / enums and @(msgFqn, fieldName)@
-- of deprecated fields, walking nested types (same scoping as 'buildSchemaEnv').
collectDeprecated :: [ProtoFile] -> (Set Text, Set (Text, Text), Set Text)
collectDeprecated files =
  mconcat (fmap fileDep files)
  where
    fileDep pf = foldMap (topDep (fromMaybe "" (protoPackage pf)) []) (protoTopLevels pf)
    topDep pkg scope = \case
      TLMessage m -> msgDep pkg scope m
      TLEnum e -> enumDep pkg scope e
      _ -> mempty
    msgDep pkg scope m =
      let scope' = scope <> [msgName m]
          fqn = qualify pkg scope'
          msgs = if any msgOptionDeprecated (msgElements m) then Set.singleton fqn else Set.empty
          flds = Set.fromList (fmap ((,) fqn) (deprecatedFieldNames m))
          nested = foldMap (elemDep pkg scope') (msgElements m)
      in (msgs, flds, Set.empty) <> nested
    elemDep pkg scope = \case
      MEMessage inner -> msgDep pkg scope inner
      MEEnum e -> enumDep pkg scope e
      _ -> mempty
    enumDep pkg scope e =
      let fqn = qualify pkg (scope <> [enumName e])
      in (Set.empty, Set.empty, if isDeprecated (enumOptions e) then Set.singleton fqn else Set.empty)


msgOptionDeprecated :: MessageElement -> Bool
msgOptionDeprecated (MEOption o) = isDeprecated [o]
msgOptionDeprecated _ = False


-- | The /proto/ names of a message's deprecated fields (regular, map, oneof).
deprecatedFieldNames :: MessageDef -> [Text]
deprecatedFieldNames m = concatMap go (msgElements m)
  where
    go = \case
      MEField fd -> keepIf (isDeprecated (fieldOptions fd)) (fieldName fd)
      MEMapField mf -> keepIf (isDeprecated (mapOptions mf)) (mapFieldName mf)
      MEOneof od -> mapMaybe oneofDep (oneofFields od)
      _ -> []
    keepIf cond nm = if cond then [nm] else []
    oneofDep f = if isDeprecated (oneofFieldOptions f) then Just (oneofFieldName f) else Nothing


schemaForEntry :: SchemaOptions -> SchemaEnv -> Text -> TypeEntry -> Value
schemaForEntry opts env fqn = \case
  TEMessage m pkg scope -> messageSchema opts env fqn pkg scope m
  TEEnum e _ _ -> enumSchema opts fqn e



-- | Build the object schema for a message, given its resolution scope.
messageSchemaFor :: SchemaEnv -> Text -> [Text] -> MessageDef -> Value
messageSchemaFor env pkg scope m =
  messageSchema defaultSchemaOptions env (qualify pkg scope) pkg scope m


-- | 'messageSchemaFor' with external annotations.
messageSchemaForWith :: SchemaOptions -> SchemaEnv -> Text -> [Text] -> MessageDef -> Value
messageSchemaForWith opts env pkg scope m =
  messageSchema opts env (qualify pkg scope) pkg scope m


messageSchema :: SchemaOptions -> SchemaEnv -> Text -> Text -> [Text] -> MessageDef -> Value
messageSchema opts env fqn pkg scope m =
  withDescription (msgDoc m) $
    Object (AKM.fromList (base <> requiredKw <> soMessageAnnotator opts fqn))
  where
    emits = concatMap (fieldEmits opts env fqn pkg scope) (msgElements m)
    base =
      [ ("type", String "object")
      , ("properties", Object (AKM.fromList (fmap (\(k, v, _) -> (k, v)) emits)))
      ]
    reqNames = mapMaybe (\(k, _, req) -> if req then Just (String (AKey.toText k)) else Nothing) emits
    requiredKw = if null reqNames then [] else [("required", Array (V.fromList reqNames))]


{- | Emit the @(jsonKey, schema, required)@ triples contributed by one message
element, applying any external field annotations.
-}
fieldEmits :: SchemaOptions -> SchemaEnv -> Text -> Text -> [Text] -> MessageElement -> [(AKey.Key, Value, Bool)]
fieldEmits opts env fqn pkg scope = \case
  MEField fd ->
    [annotated (fieldName fd) (jsonKey (fieldName fd) (fieldOptions fd)) (fieldSchema env pkg scope fd)]
  MEMapField mf ->
    [annotated (mapFieldName mf) (jsonKey (mapFieldName mf) (mapOptions mf)) (mapSchema env pkg scope mf)]
  MEOneof od ->
    -- oneof members flatten into the enclosing object (codec serialises the
    -- set member as an ordinary field); at most one is present at a time.
    fmap oneofEmit (oneofFields od)
  _ -> []
  where
    oneofEmit f =
      annotated
        (oneofFieldName f)
        (jsonKey (oneofFieldName f) (oneofFieldOptions f))
        (withDescription (oneofFieldDoc f) (fieldTypeSchema env pkg scope (oneofFieldType f)))
    annotated protoName key base = case soFieldAnnotator opts fqn protoName of
      Nothing -> (key, base, False)
      Just fc -> (key, mergeKeywords base (fcKeywords fc), fcRequired fc)


-- | Merge extra keywords into a schema object (no-op if the schema isn't an
-- object or the keyword list is empty; existing keys win).
mergeKeywords :: Value -> [Pair] -> Value
mergeKeywords v [] = v
mergeKeywords (Object o) kws = Object (AKM.union o (AKM.fromList kws))
mergeKeywords v _ = v


-- | The proto3 JSON property name for a field (honouring @json_name@).
jsonKey :: Text -> [OptionDef] -> AKey.Key
jsonKey nm opts = AKey.fromText (jsonNameOf nm opts)


-- | The proto3 JSON name for a field name + its options (honouring @json_name@).
jsonNameOf :: Text -> [OptionDef] -> Text
jsonNameOf nm opts =
  fromMaybe (protoJsonName nm) (lookupSimpleOption "json_name" opts >>= optionAsString)


-- | Schema for a single (non-map) field, applying the repeated wrapper.
fieldSchema :: SchemaEnv -> Text -> [Text] -> FieldDef -> Value
fieldSchema env pkg scope fd =
  withDescription (fieldDoc fd) $
    case fieldLabel fd of
      Just Repeated -> object ["type" .= ("array" :: Text), "items" .= base]
      _ -> base
  where
    base = fieldTypeSchema env pkg scope (fieldType fd)


-- | @map\<K,V\>@ → a JSON object with string keys and @additionalProperties@
-- describing the value type. (proto3 JSON always stringifies map keys.)
mapSchema :: SchemaEnv -> Text -> [Text] -> MapField -> Value
mapSchema env pkg scope mf =
  withDescription (mapDoc mf) $
    object
      [ "type" .= ("object" :: Text)
      , "additionalProperties" .= fieldTypeSchema env pkg scope (mapValueType mf)
      ]


-- | Schema for a field's declared type: a scalar, an inlined WKT, or a
-- @$ref@ to a named component.
fieldTypeSchema :: SchemaEnv -> Text -> [Text] -> FieldType -> Value
fieldTypeSchema env pkg scope = \case
  FTScalar s -> scalarSchema s
  FTNamed nm ->
    -- Well-known types inline by name, whether or not their .proto was parsed
    -- into the environment (they are recognised by fully-qualified name).
    case wellKnownSchema (stripLeadingDot nm) of
      Just wkt -> wkt
      Nothing -> case resolveTypeFqn env pkg scope nm of
        Just fqn -> case wellKnownSchema fqn of
          Just wkt -> wkt
          Nothing -> refForFqn fqn
        Nothing ->
          -- Unresolved (e.g. a type from a not-supplied import): emit a
          -- permissive, clearly-labelled schema rather than crash.
          object ["description" .= ("unresolved type: " <> nm)]


-- | Enum schema without external annotations.
enumSchemaFor :: EnumDef -> Value
enumSchemaFor = enumSchema defaultSchemaOptions ""


{- | Proto3 JSON renders enums as their value /name/ (a string); it also
accepts the integer form on input, but the canonical output is the name, so
the schema is a string @enum@ of the declared value names. External
enum-level keywords (from 'soEnumAnnotator', keyed by @fqn@) are merged in.
-}
enumSchema :: SchemaOptions -> Text -> EnumDef -> Value
enumSchema opts fqn e =
  withDescription (enumDoc e) $
    Object
      ( AKM.fromList
          ( [ ("type", String "string")
            , ("enum", Array (V.fromList (fmap (String . evName) (enumValues e))))
            ]
              <> soEnumAnnotator opts fqn
          )
      )


-- ---------------------------------------------------------------------------
-- Scalars
-- ---------------------------------------------------------------------------

{- | proto3-canonical-JSON scalar schemas. The 64-bit integer types map to
__strings__ (JS number precision), and @bytes@ to a base64 string, matching
what the codec emits. 32-bit ints stay JSON numbers.
-}
scalarSchema :: ScalarType -> Value
scalarSchema = \case
  SDouble -> numberWith "double"
  SFloat -> numberWith "float"
  SInt32 -> integerWith "int32" False
  SSInt32 -> integerWith "int32" False
  SSFixed32 -> integerWith "int32" False
  SUInt32 -> integerWith "int64" True
  SFixed32 -> integerWith "int64" True
  SInt64 -> stringWith "int64"
  SSInt64 -> stringWith "int64"
  SSFixed64 -> stringWith "int64"
  SUInt64 -> stringWith "uint64"
  SFixed64 -> stringWith "uint64"
  SBool -> object ["type" .= ("boolean" :: Text)]
  SString -> object ["type" .= ("string" :: Text)]
  SBytes -> object ["type" .= ("string" :: Text), "format" .= ("byte" :: Text)]
  where
    numberWith fmt = object ["type" .= ("number" :: Text), "format" .= (fmt :: Text)]
    stringWith fmt = object ["type" .= ("string" :: Text), "format" .= (fmt :: Text)]
    integerWith fmt unsigned =
      object $
        ["type" .= ("integer" :: Text), "format" .= (fmt :: Text)]
          <> (if unsigned then ["minimum" .= (0 :: Int)] else [])


-- ---------------------------------------------------------------------------
-- Well-known types (inlined at reference sites)
-- ---------------------------------------------------------------------------

-- | Is this fully-qualified name a protobuf well-known type?
isWellKnown :: Text -> Bool
isWellKnown fqn = fqn `Map.member` wellKnownTable


{- | The canonical proto3-JSON schema for a well-known type, or 'Nothing' if
the name is a user type. These are __inlined__ wherever referenced (rather
than emitted as components) because their JSON shape is not their message
structure.
-}
wellKnownSchema :: Text -> Maybe Value
wellKnownSchema fqn = Map.lookup fqn wellKnownTable


wellKnownTable :: Map Text Value
wellKnownTable =
  Map.fromList
    [ ("google.protobuf.Timestamp", str "date-time")
    , ("google.protobuf.Duration", strDesc "A duration string, e.g. \"3.5s\".")
    , ("google.protobuf.FieldMask", strDesc "Comma-separated lowerCamelCase field paths.")
    , ("google.protobuf.Struct", object ["type" .= ("object" :: Text)])
    , ("google.protobuf.Value", object [])
    , ("google.protobuf.ListValue", object ["type" .= ("array" :: Text), "items" .= object []])
    , ("google.protobuf.NullValue", object ["type" .= ("null" :: Text)])
    , ("google.protobuf.Empty", object ["type" .= ("object" :: Text)])
    , ("google.protobuf.Any", anySchema)
    , ("google.protobuf.BoolValue", object ["type" .= ("boolean" :: Text)])
    , ("google.protobuf.Int32Value", scalarSchema SInt32)
    , ("google.protobuf.UInt32Value", scalarSchema SUInt32)
    , ("google.protobuf.Int64Value", scalarSchema SInt64)
    , ("google.protobuf.UInt64Value", scalarSchema SUInt64)
    , ("google.protobuf.FloatValue", scalarSchema SFloat)
    , ("google.protobuf.DoubleValue", scalarSchema SDouble)
    , ("google.protobuf.StringValue", object ["type" .= ("string" :: Text)])
    , ("google.protobuf.BytesValue", scalarSchema SBytes)
    ]
  where
    str fmt = object ["type" .= ("string" :: Text), "format" .= (fmt :: Text)]
    strDesc d = object ["type" .= ("string" :: Text), "description" .= (d :: Text)]


anySchema :: Value
anySchema =
  object
    [ "type" .= ("object" :: Text)
    , "properties" .= object ["@type" .= object ["type" .= ("string" :: Text)]]
    , "additionalProperties" .= True
    ]


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------


-- | Drop a single leading @.@ from a fully-qualified proto name (@.pkg.Msg@).
stripLeadingDot :: Text -> Text
stripLeadingDot t = fromMaybe t (T.stripPrefix "." t)


-- | Attach a @description@ to an object schema when the proto doc-comment is
-- present (and non-empty). No-op for non-object schemas.
withDescription :: Maybe Text -> Value -> Value
withDescription mdoc v = case (fmap T.strip mdoc, v) of
  (Just d, Object o) | not (T.null d) -> Object (AKM.insert "description" (String d) o)
  _ -> v
