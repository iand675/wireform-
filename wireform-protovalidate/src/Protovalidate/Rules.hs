{-# LANGUAGE OverloadedStrings #-}

{- | The protovalidate rule model and the table of standard constraints
expressed as CEL.

Like reference protovalidate, each standard rule (e.g. @string.min_len@,
@int32.gt@, @repeated.unique@) is just a CEL expression evaluated with the
field value bound to @this@ and the rule message bound to @rules@. This
module enumerates those expressions per 'RuleKind' and provides a small
builder vocabulary for assembling 'FieldRules' / 'MessageRules'.
-}
module Protovalidate.Rules (
  RuleKind (..),
  FieldRules (..),
  MessageRules (..),
  MessageOneof (..),
  makeMessageOneof,
  NestedMessage (..),
  withNestedMessage,
  ProtoSchema,
  ProtoFieldShape (..),
  namedMessageShape,
  emptyFieldRules,
  fieldRules,
  messageRules,
  standardConstraints,
  scalarDefaultValue,

  -- * Rule-value builders
  constV,
  ltV,
  lteV,
  gtV,
  gteV,
  inV,
  notInV,
  minLen,
  maxLen,
  lenV,
  prefix,
  suffix,
  contains,
  pattern,
  email,
  hostname,
  ip,
  uri,
  uuid,
  minItems,
  maxItems,
  unique,
  finite,

  -- * Constraint builders for rules that need extra context
  definedOnly,
  oneofRequired,
  wellKnownRegex,
  mapKeys,
  mapValues,
  predefined,
) where

import CEL.Value (Duration (..), Timestamp (..), Value (..), celMapFromList)
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Protovalidate.Constraint (Constraint, unsafeConstraint)


{- | The protobuf type a set of field rules applies to. Determines which
standard constraints are available.
-}
data RuleKind
  = KFloat
  | KDouble
  | KInt32
  | KInt64
  | KUint32
  | KUint64
  | KSint32
  | KSint64
  | KFixed32
  | KFixed64
  | KSfixed32
  | KSfixed64
  | KBool
  | KString
  | KBytes
  | KEnum
  | KRepeated
  | KMap
  | KDuration
  | KTimestamp
  deriving stock (Eq, Show)


-- | Validation rules for a single field.
data FieldRules = FieldRules
  { frRequired :: !Bool
  -- ^ The field must be present (set / non-default).
  , frImplicitDefault :: !(Maybe Value)
  -- ^ Default of a protobuf field without presence tracking.
  , frIgnoreEmpty :: !Bool
  -- ^ Skip field validation when the value is the type's zero value.
  , frKind :: !(Maybe RuleKind)
  -- ^ Which standard-constraint family applies (if any).
  , frRules :: ![(Text, Value)]
  -- ^ Set rule values, keyed by rule field name (e.g. @("min_len", VUInt 3)@).
  , frCustom :: ![Constraint]
  -- ^ Field-level custom CEL constraints (@(buf.validate.field).cel@).
  , frItems :: !(Maybe FieldRules)
  -- ^ Rules applied to each element of a repeated field.
  , frMapKeys :: !(Maybe FieldRules)
  -- ^ Rules applied to each key of a map field (@(buf.validate.field).map.keys@).
  , frMapValues :: !(Maybe FieldRules)
  -- ^ Rules applied to each value of a map field (@…map.values@).
  , frPredefined :: ![(Constraint, Value)]
  {- ^ Predefined constraints: a CEL constraint plus the @rule@ value bound for
  it (@(buf.validate.predefined)@).
  -}
  , frMessage :: !(Maybe MessageRules)
  -- ^ Rules applied to a (possibly repeated) message-typed field, recursively.
  , frMessageReference :: !(Maybe Text)
  -- ^ Qualified name of an enclosing message plan to reuse on a recursive field.
  }
  deriving stock (Show)


{- | Validation rules for a whole message: per-field rules plus message-level
custom CEL (@(buf.validate.message).cel@).
-}
data MessageRules = MessageRules
  { mrFields :: ![(Text, FieldRules)]
  , mrCustom :: ![Constraint]
  , mrMessageOneofs :: ![MessageOneof]
  , mrProtoSchema :: !(Maybe (Text, ProtoSchema))
  }
  deriving stock (Show)


data MessageOneof = MessageOneof
  { messageOneofFields :: ![Text]
  , messageOneofRequired :: !Bool
  }
  deriving stock (Show)


makeMessageOneof :: Text -> Set Text -> [Text] -> Bool -> Either Text MessageOneof
makeMessageOneof name known fields required
  | null fields = failure "requires at least one field"
  | Set.size unique /= length fields = failure "contains duplicate fields"
  | Just unknown <- Set.lookupMin (Set.difference unique known) = failure ("has no field named " <> unknown)
  | otherwise = Right (MessageOneof fields required)
  where
    unique = Set.fromList fields
    failure reason = Left ("buf.validate message.oneof for " <> name <> " " <> reason)


-- | Keep rule extraction finite while retaining edges to enclosing types.
data NestedMessage
  = InlineMessage !MessageRules
  | RecursiveMessage !Text


withNestedMessage :: Maybe NestedMessage -> FieldRules -> FieldRules
withNestedMessage Nothing rules = rules
withNestedMessage (Just (InlineMessage nested)) rules = rules {frMessage = Just nested}
withNestedMessage (Just (RecursiveMessage name)) rules = rules {frMessageReference = Just name}


-- | The fields of each message in a schema, indexed by qualified type name.
-- Message references use names so recursive types need only one definition.
type ProtoSchema = Map.Map Text (Map.Map Text ProtoFieldShape)


data ProtoFieldShape
  = ExplicitDefault !Value
  | ImplicitDefault !Value
  | MessageShape !Text
  | ListShape !ProtoFieldShape
  | MapShape !ProtoFieldShape
  deriving stock (Show)


namedMessageShape :: Text -> ProtoFieldShape
namedMessageShape name = case name of
  "google.protobuf.Timestamp" -> ExplicitDefault (VTimestamp (Timestamp 0 0))
  "google.protobuf.Duration" -> ExplicitDefault (VDuration (Duration 0 0))
  "google.protobuf.Struct" -> ExplicitDefault (VMap (celMapFromList []))
  "google.protobuf.ListValue" -> ExplicitDefault (VList V.empty)
  "google.protobuf.Value" -> ExplicitDefault VNull
  "google.protobuf.BoolValue" -> ExplicitDefault VNull
  "google.protobuf.BytesValue" -> ExplicitDefault VNull
  "google.protobuf.DoubleValue" -> ExplicitDefault VNull
  "google.protobuf.FloatValue" -> ExplicitDefault VNull
  "google.protobuf.Int32Value" -> ExplicitDefault VNull
  "google.protobuf.Int64Value" -> ExplicitDefault VNull
  "google.protobuf.StringValue" -> ExplicitDefault VNull
  "google.protobuf.UInt32Value" -> ExplicitDefault VNull
  "google.protobuf.UInt64Value" -> ExplicitDefault VNull
  _ -> MessageShape name


-- | Field rules with everything empty/disabled.
emptyFieldRules :: FieldRules
emptyFieldRules =
  FieldRules
    { frRequired = False
    , frImplicitDefault = Nothing
    , frIgnoreEmpty = False
    , frKind = Nothing
    , frRules = []
    , frCustom = []
    , frItems = Nothing
    , frMapKeys = Nothing
    , frMapValues = Nothing
    , frPredefined = []
    , frMessage = Nothing
    , frMessageReference = Nothing
    }


-- | The implicit default of a scalar, indexed by FieldDescriptorProto.Type.
-- Enum defaults are resolved from the defining enum where available.
scalarDefaultValue :: Int32 -> Maybe Value
scalarDefaultValue = \case
  1 -> Just (VDouble 0)
  2 -> Just (VDouble 0)
  3 -> Just (VInt 0)
  4 -> Just (VUInt 0)
  5 -> Just (VInt 0)
  6 -> Just (VUInt 0)
  7 -> Just (VUInt 0)
  8 -> Just (VBool False)
  9 -> Just (VString "")
  12 -> Just (VBytes "")
  13 -> Just (VUInt 0)
  14 -> Just (VInt 0)
  15 -> Just (VInt 0)
  16 -> Just (VInt 0)
  17 -> Just (VInt 0)
  18 -> Just (VInt 0)
  _ -> Nothing


-- | Build field rules of a given kind from a list of set rule values.
fieldRules :: RuleKind -> [(Text, Value)] -> FieldRules
fieldRules k rs = emptyFieldRules {frKind = Just k, frRules = rs}


-- | Build message rules.
messageRules :: [(Text, FieldRules)] -> [Constraint] -> MessageRules
messageRules fields customs = MessageRules fields customs [] Nothing


----------------------------------------------------------------------
-- Rule-value builders
----------------------------------------------------------------------

constV, ltV, lteV, gtV, gteV :: Value -> (Text, Value)
constV v = ("const", v)
ltV v = ("lt", v)
lteV v = ("lte", v)
gtV v = ("gt", v)
gteV v = ("gte", v)


inV, notInV :: [Value] -> (Text, Value)
inV vs = ("in", VList (V.fromList vs))
notInV vs = ("not_in", VList (V.fromList vs))


minLen, maxLen, lenV :: Word -> (Text, Value)
minLen n = ("min_len", VUInt (fromIntegral n))
maxLen n = ("max_len", VUInt (fromIntegral n))
lenV n = ("len", VUInt (fromIntegral n))


minItems, maxItems :: Word -> (Text, Value)
minItems n = ("min_items", VUInt (fromIntegral n))
maxItems n = ("max_items", VUInt (fromIntegral n))


prefix, suffix, contains, pattern :: Text -> (Text, Value)
prefix s = ("prefix", VString s)
suffix s = ("suffix", VString s)
contains s = ("contains", VString s)
pattern s = ("pattern", VString s)


email, hostname, uri, uuid, unique, finite :: (Text, Value)
email = ("email", VBool True)
hostname = ("hostname", VBool True)
uri = ("uri", VBool True)
uuid = ("uuid", VBool True)
unique = ("unique", VBool True)
finite = ("finite", VBool True)


-- | @ip@ / @ipv4@ / @ipv6@ format selector. Use @ip@ for any version.
ip :: (Text, Value)
ip = ("ip", VBool True)


----------------------------------------------------------------------
-- Constraint builders for rules needing extra context
----------------------------------------------------------------------

{- | @enum.defined_only@: the value must be one of the enum's declared numbers.
Pass the enum's values (the validator can't know them otherwise).
-}
definedOnly :: [Integer] -> Constraint
definedOnly vals =
  unsafeConstraint
    "enum.defined_only"
    "value must be a defined enum value"
    ("this in [" <> T.intercalate ", " (map (T.pack . show) vals) <> "]")


{- | @(buf.validate.oneof).required@: at least one of the named oneof member
fields must be set. The first argument is the oneof name (used as the
constraint id).
-}
oneofRequired :: Text -> [Text] -> Constraint
oneofRequired name fields =
  unsafeConstraint
    name
    ("at least one field of oneof " <> name <> " must be set")
    (case fields of
      [] -> "false"
      _ -> T.intercalate " || " (map (\f -> "has(this." <> f <> ")") fields))


{- | @string.well_known_regex@: @kind@ is 1 (@HEADER_NAME@) or 2
(@HEADER_VALUE@). Loose validation forbids NUL, LF and CR. Header names
must be nonempty in either mode.
-}
wellKnownRegex :: Int -> Bool -> Constraint
wellKnownRegex kind strict =
  unsafeConstraint
    "string.well_known_regex"
    "value must match the well-known regex"
    ("this.matches(r\"" <> regex <> "\")")
  where
    regex
      | kind == 1 && strict = "^:?[0-9a-zA-Z!#$%&'*+.^_|~`-]+$"
      | kind == 1 = "^[^\\x00\\x0a\\x0d]+$"
      | strict = "^[^\\x00-\\x08\\x0a-\\x1f\\x7f]*$"
      | otherwise = "^[^\\x00\\x0a\\x0d]*$"


-- | Apply rules to each key of a map field.
mapKeys :: FieldRules -> FieldRules -> FieldRules
mapKeys keyRules fr = fr {frMapKeys = Just keyRules}


-- | Apply rules to each value of a map field.
mapValues :: FieldRules -> FieldRules -> FieldRules
mapValues valRules fr = fr {frMapValues = Just valRules}


{- | A predefined constraint: a CEL constraint plus the @rule@ value bound for
it. Add to 'frPredefined'.
-}
predefined :: Constraint -> Value -> (Constraint, Value)
predefined = (,)


----------------------------------------------------------------------
-- Standard constraint table
----------------------------------------------------------------------

{- | The standard constraints available for a 'RuleKind', as
@(ruleFieldName, constraint)@ pairs. A constraint applies to a field when
the corresponding rule value is set (and, for boolean flag rules, is
@true@).
-}
standardConstraints :: RuleKind -> [(Text, Constraint)]
standardConstraints = \case
  KString -> stringConstraints
  KBytes -> bytesConstraints
  KBool -> [("const", c "bool.const" "value must equal the configured constant" "this == rules.const")]
  KRepeated -> repeatedConstraints
  KMap -> mapConstraints
  KEnum -> numericConstraints "enum"
  KFloat -> floatConstraints "float"
  KDouble -> floatConstraints "double"
  KInt32 -> numericConstraints "int32"
  KInt64 -> numericConstraints "int64"
  KUint32 -> numericConstraints "uint32"
  KUint64 -> numericConstraints "uint64"
  KSint32 -> numericConstraints "sint32"
  KSint64 -> numericConstraints "sint64"
  KFixed32 -> numericConstraints "fixed32"
  KFixed64 -> numericConstraints "fixed64"
  KSfixed32 -> numericConstraints "sfixed32"
  KSfixed64 -> numericConstraints "sfixed64"
  KDuration -> numericConstraints "duration"
  KTimestamp -> timestampConstraints


-- A constraint with the given id / message / CEL source.
c :: Text -> Text -> Text -> Constraint
c = unsafeConstraint


numericConstraints :: Text -> [(Text, Constraint)]
numericConstraints kind =
  [ ("const", c (kind <> ".const") "value must equal the configured constant" "this == rules.const")
  , ("lt", c (kind <> ".lt") "value must be less than the configured bound" "this < rules.lt")
  , ("lte", c (kind <> ".lte") "value must be less than or equal to the configured bound" "this <= rules.lte")
  , ("gt", c (kind <> ".gt") "value must be greater than the configured bound" "this > rules.gt")
  , ("gte", c (kind <> ".gte") "value must be greater than or equal to the configured bound" "this >= rules.gte")
  , ("in", c (kind <> ".in") "value must be in the allowed set" "this in rules.`in`")
  , ("not_in", c (kind <> ".not_in") "value must not be in the forbidden set" "!(this in rules.not_in)")
  ]


-- Floating-point kinds add the @finite@ rule on top of the numeric rules.
floatConstraints :: Text -> [(Text, Constraint)]
floatConstraints kind =
  numericConstraints kind
    ++ [("finite", c (kind <> ".finite") "value must be finite" "!isInf(this) && !isNan(this)")]


-- Timestamps add the time-relative rules (which reference the @now@ binding;
-- see 'Protovalidate.Eval.validateAt').
timestampConstraints :: [(Text, Constraint)]
timestampConstraints =
  numericConstraints "timestamp"
    ++ [ ("lt_now", c "timestamp.lt_now" "value must be in the past" "this < now")
       , ("gt_now", c "timestamp.gt_now" "value must be in the future" "this > now")
       ,
         ( "within"
         , c
             "timestamp.within"
             "value must be within the configured duration of now"
             "this - now <= rules.within && now - this <= rules.within"
         )
       ]


stringConstraints :: [(Text, Constraint)]
stringConstraints =
  [ ("const", c "string.const" "value must equal the configured constant" "this == rules.const")
  , ("len", c "string.len" "value must be the configured number of characters" "uint(size(this)) == rules.len")
  , ("min_len", c "string.min_len" "value is too short" "uint(size(this)) >= rules.min_len")
  , ("max_len", c "string.max_len" "value is too long" "uint(size(this)) <= rules.max_len")
  , ("min_bytes", c "string.min_bytes" "value has too few bytes" "uint(size(bytes(this))) >= rules.min_bytes")
  , ("max_bytes", c "string.max_bytes" "value has too many bytes" "uint(size(bytes(this))) <= rules.max_bytes")
  , ("len_bytes", c "string.len_bytes" "value must be the configured number of bytes" "uint(size(bytes(this))) == rules.len_bytes")
  , ("pattern", c "string.pattern" "value does not match the required pattern" "this.matches(rules.pattern)")
  , ("prefix", c "string.prefix" "value does not have the required prefix" "this.startsWith(rules.prefix)")
  , ("suffix", c "string.suffix" "value does not have the required suffix" "this.endsWith(rules.suffix)")
  , ("contains", c "string.contains" "value does not contain the required substring" "this.contains(rules.contains)")
  , ("not_contains", c "string.not_contains" "value contains a forbidden substring" "!this.contains(rules.not_contains)")
  , ("in", c "string.in" "value must be in the allowed set" "this in rules.`in`")
  , ("not_in", c "string.not_in" "value must not be in the forbidden set" "!(this in rules.not_in)")
  , ("email", c "string.email" "value must be a valid email address" "this.isEmail()")
  , ("hostname", c "string.hostname" "value must be a valid hostname" "this.isHostname()")
  , ("ip", c "string.ip" "value must be a valid IP address" "this.isIp()")
  , ("ipv4", c "string.ipv4" "value must be a valid IPv4 address" "this.isIp(4)")
  , ("ipv6", c "string.ipv6" "value must be a valid IPv6 address" "this.isIp(6)")
  , ("ip_prefix", c "string.ip_prefix" "value must be a valid IP prefix" "this.isIpPrefix()")
  , ("ipv4_prefix", c "string.ipv4_prefix" "value must be a valid IPv4 network prefix" "this.isIpPrefix(4, true)")
  , ("ipv6_prefix", c "string.ipv6_prefix" "value must be a valid IPv6 network prefix" "this.isIpPrefix(6, true)")
  , ("ip_with_prefixlen", c "string.ip_with_prefixlen" "value must be a valid IP with prefix length" "this.isIpPrefix()")
  , ("ipv4_with_prefixlen", c "string.ipv4_with_prefixlen" "value must be a valid IPv4 with prefix length" "this.isIpPrefix(4)")
  , ("ipv6_with_prefixlen", c "string.ipv6_with_prefixlen" "value must be a valid IPv6 with prefix length" "this.isIpPrefix(6)")
  , ("uri", c "string.uri" "value must be a valid URI" "this.isUri()")
  , ("uri_ref", c "string.uri_ref" "value must be a valid URI reference" "this.isUriRef()")
  , ("address", c "string.address" "value must be a valid hostname or IP address" "this.isIp() || this.isHostname()")
  , ("host_and_port", c "string.host_and_port" "value must be a valid host and port" "this.isHostAndPort(true)")
  , ("uuid", c "string.uuid" "value must be a valid UUID" "this == '' || this.matches('^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$')")
  , ("tuuid", c "string.tuuid" "value must be a valid trimmed UUID" "this == '' || this.matches('^[0-9a-fA-F]{32}$')")
  ]


bytesConstraints :: [(Text, Constraint)]
bytesConstraints =
  [ ("const", c "bytes.const" "value must equal the configured constant" "this == rules.const")
  , ("len", c "bytes.len" "value must be the configured number of bytes" "uint(size(this)) == rules.len")
  , ("min_len", c "bytes.min_len" "value has too few bytes" "uint(size(this)) >= rules.min_len")
  , ("max_len", c "bytes.max_len" "value has too many bytes" "uint(size(this)) <= rules.max_len")
  , ("prefix", c "bytes.prefix" "value does not have the required prefix" "this.startsWith(rules.prefix)")
  , ("suffix", c "bytes.suffix" "value does not have the required suffix" "this.endsWith(rules.suffix)")
  , ("contains", c "bytes.contains" "value does not contain the required subsequence" "this.contains(rules.contains)")
  , ("in", c "bytes.in" "value must be in the allowed set" "this in rules.`in`")
  , ("not_in", c "bytes.not_in" "value must not be in the forbidden set" "!(this in rules.not_in)")
  , ("ip", c "bytes.ip" "value must be a valid IP address" "this.isIp()")
  , ("ipv4", c "bytes.ipv4" "value must be a valid IPv4 address" "this.isIp(4)")
  , ("ipv6", c "bytes.ipv6" "value must be a valid IPv6 address" "this.isIp(6)")
  ]


repeatedConstraints :: [(Text, Constraint)]
repeatedConstraints =
  [ ("min_items", c "repeated.min_items" "value has too few items" "uint(size(this)) >= rules.min_items")
  , ("max_items", c "repeated.max_items" "value has too many items" "uint(size(this)) <= rules.max_items")
  , ("unique", c "repeated.unique" "value must contain unique items" "unique(this)")
  ]


mapConstraints :: [(Text, Constraint)]
mapConstraints =
  [ ("min_pairs", c "map.min_pairs" "value has too few pairs" "uint(size(this)) >= rules.min_pairs")
  , ("max_pairs", c "map.max_pairs" "value has too many pairs" "uint(size(this)) <= rules.max_pairs")
  ]
