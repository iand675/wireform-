{-# LANGUAGE OverloadedStrings #-}

{- | Extract protovalidate 'MessageRules' from @buf.validate@ annotations on a
parsed @.proto@ file.

This closes the gap between annotated schemas and the validation engine:
rather than constructing 'MessageRules' by hand, parse a @.proto@ that uses
@(buf.validate.field)@ / @(buf.validate.message)@ options and read the rules
straight out of the @wireform-proto@ IDL AST.

Supported annotations:

  * scalar / numeric / bool / bytes / enum / duration / timestamp rules,
    e.g. @(buf.validate.field).string.min_len = 3@,
    @(buf.validate.field).int32.gt = 0@;
  * @repeated@ rules (@min_items@, @max_items@, @unique@) and per-element
    @repeated.items.<type>.<rule>@ rules;
  * @map@ rules (@min_pairs@, @max_pairs@) plus @map.keys@ / @map.values@
    sub-rules;
  * @enum.defined_only@ (resolving the enum's declared values) and
    @string.well_known_regex@ (resolving the @KnownRegex@ enum + @strict@),
    on scalar fields, repeated items, and map members;
  * @timestamp@ / @duration@ bounds whose option value is a
    @{seconds:.., nanos:..}@ message literal (and @timestamp.within@, a span);
  * oneof @required@ (@(buf.validate.oneof).required@);
  * @required@ and @ignore@ / @ignore_empty@;
  * field-level and message-level custom CEL
    (@(buf.validate.field).cel@ / @(buf.validate.message).cel@);
  * nested and recursive message validation (resolved against the other
    messages in the file).

Reading the equivalent options out of a compiled @FileDescriptorSet@
(extension #1159 on @FieldOptions@) additionally requires @descriptor.proto@
options support in @wireform-proto@; the @.proto@ AST is the in-repo source
of truth used here.
-}
module Protovalidate.Schema (
  parseProtoRules,
  fileMessageRules,
  extractMessageRules,
) where

import CEL.Value (Duration (..), Timestamp (..), Value (..), celMapFromList)
import Data.List (stripPrefix)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import GHC.Float (double2Float, float2Double)
import Proto.Google.Protobuf.Descriptor (fdpFieldType)
import Proto.IDL.AST
import Proto.IDL.Annotations (lookupSimpleOption)
import Proto.IDL.Descriptor (astToFieldDescriptor, resolveDescriptorName, resolveFieldPresence)
import Proto.IDL.Inspect (fieldOptionsOf, messageMapFields, messageOneofs, messageOptions)
import Proto.IDL.Parser (parseProtoFile)
import Protovalidate.Constraint (Constraint, mkAnnotationConstraint)
import Protovalidate.Rules


{- | Parse @.proto@ source text and extract validation rules for every message
it defines, keyed by (simple) message name.
-}
parseProtoRules :: Text -> Either Text [(Text, MessageRules)]
parseProtoRules src = case parseProtoFile "<protovalidate>" src of
  Left err -> Left (T.pack (show err))
  Right pf -> fileMessageRules pf


-- | Extract validation rules for every message in a parsed file.
fileMessageRules :: ProtoFile -> Either Text [(Text, MessageRules)]
fileMessageRules pf = resolveFieldPresence pf >>= extractFile
  where
    extractFile file =
      let definitions = scopedDefinitions file
          resolve = resolveDescriptorName (Map.fromList definitions)
          base = SchemaContext (protoSyntax file) resolve Map.empty
          shapes = Map.fromList (mapMaybe (fmap (uncurry (messageShape base)) . asMessage) definitions)
          schema = base {schemaShapes = shapes}
      in traverse (extract schema) (mapMaybe asMessage definitions)
    extract schema (scope, msg) = (,) (msgName msg) <$> extractMessageRulesFor schema Set.empty scope msg
    asMessage (scope, MessageDefinition msg) = Just (scope, msg)
    asMessage _ = Nothing


data SchemaDefinition = MessageDefinition !MessageDef | EnumDefinition !EnumDef

data SchemaContext = SchemaContext
  { schemaSyntax :: !Syntax
  , schemaResolve :: !(Text -> Text -> Maybe (Text, SchemaDefinition))
  , schemaShapes :: !ProtoSchema
  }


{- | Extract the rules for a single message, given the file's enum + message
tables (for enum-value and nested-message resolution) and the set of messages
currently being expanded (cycle guard).
-}
extractMessageRules :: [EnumDef] -> [MessageDef] -> Set Text -> MessageDef -> Either Text MessageRules
extractMessageRules enums msgs visiting msg =
  extractMessageRulesFor schema visiting (msgName msg) msg
  where
    messages = msg : msgs
    definitions = map (\m -> (msgName m, MessageDefinition m)) messages
      ++ map (\e -> (enumName e, EnumDefinition e)) enums
    base = SchemaContext Proto3 resolveSimple Map.empty
    schema = base {schemaShapes = Map.fromList (map (\m -> messageShape base (msgName m) m) messages)}
    resolveSimple _ name =
      let simple = last (T.splitOn "." name)
      in (simple,) <$> lookup simple definitions


extractMessageRulesFor :: SchemaContext -> Set Text -> Text -> MessageDef -> Either Text MessageRules
extractMessageRulesFor schema visiting scope msg = do
  let vis = Set.insert scope visiting
      ordinary = validationFields msg
      maps = messageMapFields msg
      known = Set.fromList (map fieldName ordinary ++ map mapFieldName maps)
      options = concatMap messageRuleOptions (messageOptions msg)
  groups <- traverse (messageOneofOption scope known) (map snd (filter ((== ["oneof"]) . fst) options))
  let members = Set.fromList (concatMap messageOneofFields groups)
  fields <- traverse (extractField schema vis scope members) ordinary
  mapFields <- traverse (extractMapField schema vis scope members) maps
  customs <- customConstraints options
  let rules = messageRules (fields ++ mapFields) (customs ++ oneofConstraints msg)
      shapes = schemaShapes schema
  Right rules {mrProtoSchema = Just (scope, shapes), mrMessageOneofs = groups}


messageOneofOption :: Text -> Set Text -> Constant -> Either Text MessageOneof
messageOneofOption scope known = \case
  CAggregate options -> do
    fields <- traverse fieldNameOption (map snd (filter ((== "fields") . fst) options))
    required <- case lookup "required" (reverse options) of
      Nothing -> Right False
      Just (CBool value) -> Right value
      Just _ -> Left "buf.validate message.oneof required must be a boolean"
    makeMessageOneof scope known fields required
  _ -> Left "buf.validate message.oneof must be a message literal"
  where
    fieldNameOption (CString name) = Right name
    fieldNameOption _ = Left "buf.validate message.oneof fields must be strings"


defaultOneofIgnore :: Set Text -> Text -> [([Text], Constant)] -> [([Text], Constant)]
defaultOneofIgnore members name options
  | Set.member name members, not (isJust (lookup ["ignore"] options)) = (["ignore"], CInt 1) : options
  | otherwise = options


validationFields :: MessageDef -> [FieldDef]
validationFields = concatMap fields . msgElements
  where
    fields (MEField fd) = [fd]
    fields (MEOneof oneof) = map oneofMember (oneofFields oneof)
    fields _ = []
    oneofMember f = FieldDef
      { fieldExt = oneofFieldExt f
      , fieldDoc = oneofFieldDoc f
      , fieldLabel = Just Optional
      , fieldType = oneofFieldType f
      , fieldName = oneofFieldName f
      , fieldNumber = oneofFieldNumber f
      , fieldOptions = oneofFieldOptions f
      }


messageShape :: SchemaContext -> Text -> MessageDef -> (Text, Map.Map Text ProtoFieldShape)
messageShape schema scope msg =
  (scope, Map.fromList (map ordinary (validationFields msg) ++ map mapped (messageMapFields msg)))
  where
    ordinary fd = (fieldName fd, shape)
      where
        defaultOption = lookupSimpleOption "default" (fieldOptionsOf fd)
        implicit = fieldLabel fd == Nothing && schemaSyntax schema /= Proto2
        valueShape = sourceValueShape schema scope implicit defaultOption (fieldType fd)
        shape
          | fieldLabel fd == Just Repeated = ListShape valueShape
          | otherwise = valueShape
    mapped mf = (mapFieldName mf, MapShape (sourceValueShape schema scope False Nothing (mapValueType mf)))


sourceValueShape :: SchemaContext -> Text -> Bool -> Maybe Constant -> FieldType -> ProtoFieldShape
sourceValueShape schema scope implicit supplied = \case
  FTScalar scalar ->
    let fd = FieldDef () Nothing Nothing (FTScalar scalar) "" (FieldNumber 0) []
        fallback = fromMaybe VNull (scalarDefaultValue (fdpFieldType (astToFieldDescriptor fd)))
        value = fromMaybe fallback (supplied >>= scalarDefault scalar fallback)
    in withPresence value
  FTNamed name -> case schemaResolve schema scope name of
    Just (qualified, MessageDefinition _) -> namedMessageShape qualified
    Just (_, EnumDefinition ed) ->
      let values = enumValues ed
          declared = case supplied of
            Just (CIdent symbol) -> lookup symbol (map (\e -> (evName e, evNumber e)) values)
            _ -> Nothing
          first = case values of
            value : _ -> evNumber value
            [] -> 0
      in withPresence (VInt (fromIntegral (fromMaybe first declared)))
    Nothing -> namedMessageShape (T.dropWhile (== '.') name)
  where
    withPresence = if implicit then ImplicitDefault else ExplicitDefault
    scalarDefault scalar fallback value = case (fallback, value) of
      (VInt _, CInt n) -> Just (VInt (fromInteger n))
      (VUInt _, CInt n) -> Just (VUInt (fromInteger n))
      (VBool _, CBool b) -> Just (VBool b)
      (VString _, CString t) -> Just (VString t)
      (VBytes _, CBytes bytes) -> Just (VBytes bytes)
      (VBytes _, CString t) -> Just (VBytes (TE.encodeUtf8 t))
      (VDouble _, _) -> VDouble . roundFloat scalar <$> asDouble value
      _ -> Nothing
    roundFloat SFloat = float2Double . double2Float
    roundFloat _ = id


-- | A @(buf.validate.oneof).required@ constraint for each oneof so annotated.
oneofConstraints :: MessageDef -> [Constraint]
oneofConstraints = mapMaybe constraint . messageOneofs
  where
    constraint od = case mapMaybe requiredOption (reverse (oneofOptions od)) of
      True : _ -> Just (oneofRequired (oneofName od) (map oneofFieldName (oneofFields od)))
      _ -> Nothing

    requiredOption od = case (optNameParts (optName od), optValue od) of
      ([ExtensionOption "buf.validate.oneof", SimpleOption "required"], value) -> asBoolMaybe value
      ([ExtensionOption "buf.validate.oneof"], CAggregate values) -> lookup "required" (reverse values) >>= asBoolMaybe
      _ -> Nothing


{- | Extract rules for a @map@ field (which is a distinct AST element), including
@map.keys@ / @map.values@ sub-rules.
-}
extractMapField :: SchemaContext -> Set Text -> Text -> Set Text -> MapField -> Either Text (Text, FieldRules)
extractMapField schema visiting scope members mf
  | ignoresAlways options = Right (mapFieldName mf, emptyFieldRules)
  | otherwise = extractMapRules mf nested enumValues options
  where
    options = defaultOneofIgnore members (mapFieldName mf) (concatMap fieldRuleOptions (mapOptions mf))
    nested = resolveMessage schema visiting scope (mapValueType mf)
    enumValues = enumValuesFor schema scope (mapValueType mf)


extractMapRules :: MapField -> Maybe NestedMessage -> Maybe [Integer] -> [([Text], Constant)] -> Either Text (Text, FieldRules)
extractMapRules mf nested enumValues options = do
  let keyOptions = subruleOptions ["map", "keys"] options
      valueOptions = subruleOptions ["map", "values"] options
      items = map classify options
  customs <- customConstraints options
  keyCustoms <- customConstraints keyOptions
  valCustoms <- customConstraints valueOptions
  keyContext <- contextConstraints Nothing keyOptions
  valueContext <- contextConstraints enumValues valueOptions
  let mapRs = mergeRuleValues [(f, v) | IMapRule f v <- items]
      keyTyped = [(k, f, v) | IMapKeyTyped k f v <- items]
      valTyped = [(k, f, v) | IMapValueTyped k f v <- items]
      subRules subOptions nestedRules tps cs
        | ignoresAlways subOptions = Just emptyFieldRules
        | null subOptions && not (isJust nestedRules) = Nothing
        | otherwise =
            Just $
              withNestedMessage nestedRules emptyFieldRules
                { frIgnoreEmpty = collectionIgnoreEmpty subOptions
                , frKind = firstKindOf tps
                , frRules = mergeRuleValues [(f, v) | (_, f, v) <- tps]
                , frCustom = cs
                }
  Right
    ( mapFieldName mf
    , emptyFieldRules
        { frKind = Just KMap
        , frImplicitDefault = Just (VMap (celMapFromList []))
        , frRules = mapRs
        , frMapKeys = subRules keyOptions Nothing keyTyped (keyCustoms ++ keyContext)
        , frMapValues = subRules valueOptions nested valTyped (valCustoms ++ valueContext)
        , frCustom = customs
        , frRequired = any isReqItem items
        , frIgnoreEmpty = any isIgnItem items
        }
    )


firstKindOf :: [(RuleKind, Text, Value)] -> Maybe RuleKind
firstKindOf ts = case ts of ((k, _, _) : _) -> Just k; _ -> Nothing


isReqItem :: Item -> Bool
isReqItem IRequired = True
isReqItem _ = False


isIgnItem :: Item -> Bool
isIgnItem IIgnoreEmpty = True
isIgnItem IIgnoreZero = True
isIgnItem _ = False


----------------------------------------------------------------------
-- Custom CEL annotations
----------------------------------------------------------------------

messageRuleOptions :: OptionDef -> [([Text], Constant)]
messageRuleOptions od = case (optNameParts (optName od), optValue od) of
  ([ExtensionOption "buf.validate.message", SimpleOption name], value) -> [([name], value)]
  ([ExtensionOption "buf.validate.message"], CAggregate values) -> map (\(name, value) -> ([name], value)) values
  _ -> []


customConstraints :: [([Text], Constant)] -> Either Text [Constraint]
customConstraints options
  | ignoresAlways options = Right []
  | otherwise = do
      shorthand <- traverse expressionConstraint (optionValues ["cel_expression"])
      explicit <- traverse constraintFromAggregate (optionValues ["cel"])
      Right (shorthand ++ explicit)
  where
    optionValues name = map snd (filter ((== name) . fst) options)
    expressionConstraint (CString expression) = mkAnnotationConstraint expression "" expression
    expressionConstraint _ = Left "buf.validate cel_expression must be a string"


----------------------------------------------------------------------
-- Field-level extraction
----------------------------------------------------------------------

-- An interpreted field option.
data Item
  = IRequired
  | IIgnoreEmpty
  | IIgnoreZero
  | ITyped !RuleKind !Text !Value
  | IRepeatedRule !Text !Value
  | IItemTyped !RuleKind !Text !Value
  | IMapRule !Text !Value
  | IMapKeyTyped !RuleKind !Text !Value
  | IMapValueTyped !RuleKind !Text !Value
  | ISkip


extractField :: SchemaContext -> Set Text -> Text -> Set Text -> FieldDef -> Either Text (Text, FieldRules)
extractField schema visiting scope members fd
  | ignoresAlways options = Right (fieldName fd, emptyFieldRules)
  | otherwise = extractFieldRules schema visiting scope fd options
  where
    options = defaultOneofIgnore members (fieldName fd) (concatMap fieldRuleOptions (fieldOptionsOf fd))


extractFieldRules :: SchemaContext -> Set Text -> Text -> FieldDef -> [([Text], Constant)] -> Either Text (Text, FieldRules)
extractFieldRules schema visiting scope fd options = do
  let itemOptions = subruleOptions ["repeated", "items"] options
      items = map classify options
  customRules <- customConstraints options
  itemCustomRules <- customConstraints itemOptions
  contextCustoms <- contextConstraints enumValues options
  itemContextCustoms <- contextConstraints enumValues itemOptions
  let typed = [(k, f, v) | ITyped k f v <- items]
      repeatedRs = mergeRuleValues [(f, v) | IRepeatedRule f v <- items]
      itemTyped = [(k, f, v) | IItemTyped k f v <- items]
      mapRs = mergeRuleValues [(f, v) | IMapRule f v <- items]
      customs = customRules ++ contextCustoms
      itemCustoms = itemCustomRules ++ itemContextCustoms
      isRequired = any isReqItem items
      isIgnoreEmpty = any ignoresZero items
      isRepeated = fieldLabel fd == Just Repeated
      namedMsg = resolveMessage schema visiting scope (fieldType fd)

      scalarKind = firstKind typed
      scalarRules = mergeRuleValues [(f, v) | (_, f, v) <- typed]

      itemsRules
        | ignoresAlways itemOptions = Just emptyFieldRules
        | isRepeated =
            Just $
              withNestedMessage namedMsg emptyFieldRules
                { frIgnoreEmpty = collectionIgnoreEmpty itemOptions
                , frKind = firstKind itemTyped
                , frRules = mergeRuleValues [(f, v) | (_, f, v) <- itemTyped]
                , frCustom = itemCustoms
                }
        | not (null itemTyped) || not (null itemCustoms) =
            Just
              emptyFieldRules
                { frKind = firstKind itemTyped
                , frRules = mergeRuleValues [(f, v) | (_, f, v) <- itemTyped]
                , frCustom = itemCustoms
                }
        | otherwise = Nothing

      base
        | isRepeated || not (null repeatedRs) =
            emptyFieldRules
              { frKind = Just KRepeated
              , frRules = repeatedRs
              , frItems = itemsRules
              }
        | not (null mapRs) =
            emptyFieldRules {frKind = Just KMap, frRules = mapRs}
        | otherwise =
            withNestedMessage namedMsg emptyFieldRules
              { frKind = scalarKind
              , frRules = scalarRules
              }

  Right
    ( fieldName fd
    , base
        { frRequired = isRequired
        , frImplicitDefault = implicitDefault
        , frIgnoreEmpty = isIgnoreEmpty
        , frCustom = customs
        }
    )
  where
    ignoresZero IIgnoreZero = isJust implicitDefault
    ignoresZero item = isIgnItem item
    implicitDefault
      | fieldLabel fd == Just Repeated = Just (VList V.empty)
      | fieldLabel fd == Nothing, schemaSyntax schema /= Proto2 = case fieldType fd of
          FTScalar _ -> scalarDefaultValue (fdpFieldType (astToFieldDescriptor fd))
          FTNamed _ -> case enumValues of
            Just (first : _) -> Just (VInt (fromIntegral first))
            _ -> Nothing
      | otherwise = Nothing
    enumValues = enumValuesFor schema scope (fieldType fd)
    firstKind ts = case ts of ((k, _, _) : _) -> Just k; _ -> Nothing


-- Normalize dotted and aggregate spellings of rule options. CEL constraints
-- and message-valued bounds are leaves and retain their aggregate values.
fieldRuleOptions :: OptionDef -> [([Text], Constant)]
fieldRuleOptions od = case (optNameParts (optName od), optValue od) of
  (ExtensionOption "buf.validate.field" : rest, value) -> expand (map partName rest) value
  _ -> []
  where
    partName (SimpleOption t) = t
    partName (ExtensionOption t) = t

    expand parts (CAggregate values)
      | ruleContainer parts = concatMap (\(key, value) -> expand (parts ++ [key]) value) values
    expand parts value = [(parts, value)]

    ruleContainer [] = True
    ruleContainer ["repeated"] = True
    ruleContainer ["map"] = True
    ruleContainer ["repeated", "items"] = True
    ruleContainer ["map", "keys"] = True
    ruleContainer ["map", "values"] = True
    ruleContainer [kind] = isJust (kindFromName kind)
    ruleContainer ["repeated", "items", kind] = isJust (kindFromName kind)
    ruleContainer ["map", side, kind] = (side == "keys" || side == "values") && isJust (kindFromName kind)
    ruleContainer _ = False


subruleOptions :: [Text] -> [([Text], Constant)] -> [([Text], Constant)]
subruleOptions parent = mapMaybe (\(path, value) -> (, value) <$> stripPrefix parent path)


collectionIgnoreEmpty :: [([Text], Constant)] -> Bool
collectionIgnoreEmpty options =
  case lookup ["ignore"] (reverse options) of
    Just mode -> isIgnItem (classify (["ignore"], mode))
    Nothing -> False


classify :: ([Text], Constant) -> Item
classify (parts, value) = case parts of
  ["required"] -> if isTrue value then IRequired else ISkip
  ["ignore_empty"] -> if isTrue value then IIgnoreEmpty else ISkip
  ["ignore"] -> case value of
    CIdent "IGNORE_IF_ZERO_VALUE" -> IIgnoreZero
    CInt 1 -> IIgnoreZero
    CIdent "IGNORE_IF_UNPOPULATED" -> IIgnoreEmpty
    CIdent "IGNORE_IF_DEFAULT_VALUE" -> IIgnoreEmpty
    _ -> ISkip
  ["repeated", "items", k, f]
    | Just kind <- kindFromName k -> maybe ISkip (IItemTyped kind f) (ruleValue kind f value)
  ["repeated", f]
    | f `elem` repeatedRuleFields -> maybe ISkip (IRepeatedRule f) (repeatedRuleValue f value)
  ["map", "keys", k, f]
    | Just kind <- kindFromName k -> maybe ISkip (IMapKeyTyped kind f) (ruleValue kind f value)
  ["map", "values", k, f]
    | Just kind <- kindFromName k -> maybe ISkip (IMapValueTyped kind f) (ruleValue kind f value)
  ["map", f]
    | f `elem` ["min_pairs", "max_pairs"] -> maybe ISkip (IMapRule f) (asUInt value)
  [k, f]
    | Just kind <- kindFromName k -> maybe ISkip (ITyped kind f) (ruleValue kind f value)
  _ -> ISkip


contextConstraints :: Maybe [Integer] -> [([Text], Constant)] -> Either Text [Constraint]
contextConstraints numbers options
  | ignoresAlways options = Right []
  | otherwise = do
      enumRules <- case lookup ["enum", "defined_only"] latest of
        Just value | isTrue value ->
          maybe (Left "buf.validate enum.defined_only needs the enum definition") (Right . pure . definedOnly) numbers
        _ -> Right []
      let strict = fromMaybe True (lookup ["string", "strict"] latest >>= asBoolMaybe)
          regex = lookup ["string", "well_known_regex"] latest >>= wellKnownRegexCode
      Right (enumRules ++ maybe [] (\code -> [wellKnownRegex code strict]) regex)
  where
    latest = reverse options


-- The @buf.validate.KnownRegex@ enum: HTTP header name = 1, header value = 2.
wellKnownRegexCode :: Constant -> Maybe Int
wellKnownRegexCode = \case
  CIdent "KNOWN_REGEX_HTTP_HEADER_NAME" -> Just 1
  CIdent "KNOWN_REGEX_HTTP_HEADER_VALUE" -> Just 2
  CInt 1 -> Just 1
  CInt 2 -> Just 2
  _ -> Nothing


asBoolMaybe :: Constant -> Maybe Bool
asBoolMaybe (CBool b) = Just b
asBoolMaybe _ = Nothing


repeatedRuleFields :: [Text]
repeatedRuleFields = ["min_items", "max_items", "unique"]


repeatedRuleValue :: Text -> Constant -> Maybe Value
repeatedRuleValue "unique" v = asBool v
repeatedRuleValue _ v = asUInt v -- min_items / max_items


----------------------------------------------------------------------
-- Nested message resolution
----------------------------------------------------------------------

resolveMessage :: SchemaContext -> Set Text -> Text -> FieldType -> Maybe NestedMessage
resolveMessage schema visiting scope ft = case ft of
  FTNamed name
    | Just (qualified, MessageDefinition msg) <- schemaResolve schema scope name ->
        if Set.member qualified visiting
          then Just (RecursiveMessage qualified)
          else either (const Nothing) (Just . InlineMessage) (extractMessageRulesFor schema visiting qualified msg)
  _ -> Nothing


scopedDefinitions :: ProtoFile -> [(Text, SchemaDefinition)]
scopedDefinitions pf = concatMap topLevel (protoTopLevels pf)
  where
    package = fromMaybe "" (protoPackage pf)
    qualify scope name = if T.null scope then name else scope <> "." <> name

    topLevel (TLMessage msg) = message package msg
    topLevel (TLEnum ed) = enum package ed
    topLevel _ = []

    message scope msg =
      let name = qualify scope (msgName msg)
      in (name, MessageDefinition msg) : concatMap (nested name) (msgElements msg)

    nested scope (MEMessage msg) = message scope msg
    nested scope (MEEnum ed) = enum scope ed
    nested _ _ = []

    enum scope ed = [(qualify scope (enumName ed), EnumDefinition ed)]


enumValuesFor :: SchemaContext -> Text -> FieldType -> Maybe [Integer]
enumValuesFor schema scope ft = case ft of
  FTNamed name
    | Just (_, EnumDefinition ed) <- schemaResolve schema scope name ->
        Just (map (fromIntegral . evNumber) (enumValues ed))
  _ -> Nothing


----------------------------------------------------------------------
-- Value / constant helpers
----------------------------------------------------------------------

constraintFromAggregate :: Constant -> Either Text Constraint
constraintFromAggregate (CAggregate kvs) =
  let str k = case lookup k (reverse kvs) of Just (CString s) -> s; _ -> ""
  in mkAnnotationConstraint (str "id") (str "message") (str "expression")
constraintFromAggregate _ = Left "buf.validate cel option must be an aggregate {id:..,message:..,expression:..}"


kindFromName :: Text -> Maybe RuleKind
kindFromName = \case
  "float" -> Just KFloat
  "double" -> Just KDouble
  "int32" -> Just KInt32
  "int64" -> Just KInt64
  "uint32" -> Just KUint32
  "uint64" -> Just KUint64
  "sint32" -> Just KSint32
  "sint64" -> Just KSint64
  "fixed32" -> Just KFixed32
  "fixed64" -> Just KFixed64
  "sfixed32" -> Just KSfixed32
  "sfixed64" -> Just KSfixed64
  "bool" -> Just KBool
  "string" -> Just KString
  "bytes" -> Just KBytes
  "enum" -> Just KEnum
  "duration" -> Just KDuration
  "timestamp" -> Just KTimestamp
  _ -> Nothing


countFields :: [Text]
countFields = ["min_len", "max_len", "len", "min_bytes", "max_bytes", "len_bytes"]


formatFlagFields :: [Text]
formatFlagFields =
  [ "email"
  , "hostname"
  , "ip"
  , "ipv4"
  , "ipv6"
  , "ip_prefix"
  , "ipv4_prefix"
  , "ipv6_prefix"
  , "ip_with_prefixlen"
  , "ipv4_with_prefixlen"
  , "ipv6_with_prefixlen"
  , "uri"
  , "uri_ref"
  , "address"
  , "host_and_port"
  , "uuid"
  , "tuuid"
  , "finite"
  , "lt_now"
  , "gt_now"
  ]


-- Interpret a rule value given the rule kind and the rule field name.
ruleValue :: RuleKind -> Text -> Constant -> Maybe Value
ruleValue kind field con
  | kind == KEnum && field == "defined_only" = Nothing
  | kind == KString && (field == "well_known_regex" || field == "strict") = Nothing
  | field `elem` countFields = asUInt con
  | field `elem` formatFlagFields = asBool con
  | otherwise = case kind of
      KString -> case con of
        CString s -> Just (VString s)
        CBool b -> Just (VBool b)
        _ -> Nothing
      KBytes -> case field of
        _ -> asBytes con
      KBool -> asBool con
      -- Duration / timestamp bounds are message literals (@{seconds:.., nanos:..}@).
      KDuration -> uncurry mkDuration <$> secondsNanos con
      KTimestamp
        -- @within@ on a timestamp is itself a duration span.
        | field == "within" -> uncurry mkDuration <$> secondsNanos con
        | otherwise -> uncurry mkTimestamp <$> secondsNanos con
      _
        | isUnsigned kind -> asUInt con
        | kind == KFloat -> VDouble . float2Double . double2Float <$> asDouble con
        | kind == KDouble -> VDouble <$> asDouble con
        | otherwise -> asInt con


isUnsigned :: RuleKind -> Bool
isUnsigned k = k `elem` [KUint32, KUint64, KFixed32, KFixed64]


asUInt :: Constant -> Maybe Value
asUInt (CInt i) | i >= 0 = Just (VUInt (fromInteger i))
asUInt _ = Nothing


asInt :: Constant -> Maybe Value
asInt (CInt i) = Just (VInt (fromInteger i))
asInt _ = Nothing


asDouble :: Constant -> Maybe Double
asDouble (CFloat d) = Just d
asDouble (CInt i) = Just (fromInteger i)
asDouble _ = Nothing


asBool :: Constant -> Maybe Value
asBool (CBool b) = Just (VBool b)
asBool _ = Nothing


asBytes :: Constant -> Maybe Value
asBytes (CBytes bytes) = Just (VBytes bytes)
asBytes (CString s) = Just (VBytes (TE.encodeUtf8 s))
asBytes _ = Nothing


-- Read @{seconds: .., nanos: ..}@ (either component optional, default 0) from a
-- @google.protobuf.Duration@ / @Timestamp@ message literal.
secondsNanos :: Constant -> Maybe (Integer, Integer)
secondsNanos (CAggregate kvs) =
  let intAt k = case lookup k kvs of Just (CInt i) -> Just i; Nothing -> Just 0; _ -> Nothing
  in (,) <$> intAt "seconds" <*> intAt "nanos"
secondsNanos _ = Nothing


mkDuration :: Integer -> Integer -> Value
mkDuration s n = VDuration (Duration (fromInteger s) (fromInteger n))


mkTimestamp :: Integer -> Integer -> Value
mkTimestamp s n = VTimestamp (Timestamp (fromInteger s) (fromInteger n))


isTrue :: Constant -> Bool
isTrue (CBool b) = b
isTrue _ = False


ignoresAlways :: [([Text], Constant)] -> Bool
ignoresAlways options = case lookup ["ignore"] (reverse options) of
  Just (CIdent "IGNORE_ALWAYS") -> True
  Just (CInt 3) -> True
  _ -> False


-- Merge raw rule values: @in@ / @not_in@ accumulate into a list, others keep
-- the last value.
mergeRuleValues :: [(Text, Value)] -> [(Text, Value)]
mergeRuleValues entries = foldr step [] keys
  where
    keys = nubOrd (map fst entries)
    nubOrd = go Set.empty
      where
        go _ [] = []
        go seen (x : xs)
          | Set.member x seen = go seen xs
          | otherwise = x : go (Set.insert x seen) xs
    step k acc =
      let vs = [v | (k', v) <- entries, k' == k]
      in (k, if k == "in" || k == "not_in" then VList (V.fromList vs) else last vs) : acc
