{-# LANGUAGE OverloadedStrings #-}

{- | The @.proto@ source used by "Test.Protovalidate.TH". It lives in its own
module so it can be referenced from a Template Haskell splice (a top-level
binding cannot be spliced in the module that defines it).
-}
module Test.Protovalidate.UserProto (
  userProto, eventProto, ignoreProto, parityProto, predefinedRules, descriptorScalarProto, descriptorCollectionProto,
  descriptorContextProto, shorthandProto, presenceProto, editionPresenceProto,
  messageCelProto, explicitDefaultsProto, recursiveProto, messageOneofProto,
) where

import CEL (CelType (..), Value (..), celMapFromList)
import CEL.Syntax (Expr (..), Literal (..))
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Protovalidate
import Protovalidate.Constraint (unsafeConstraint)


userProto :: Text
userProto =
  "syntax = \"proto3\";\n\
  \package test.v1;\n\
  \message User {\n\
  \  string id = 1 [(buf.validate.field).string.min_len = 2];\n\
  \  uint32 age = 2 [(buf.validate.field).uint32.lte = 150];\n\
  \  string email = 3 [(buf.validate.field).string.email = true];\n\
  \  option (buf.validate.message).cel = {\n\
  \    id: \"id_required_with_age\"\n\
  \    message: \"id must be set when age is set\"\n\
  \    expression: \"this.age == 0u || this.id != ''\"\n\
  \  };\n\
  \}\n"


{- | A proto exercising message-literal time bounds (@timestamp@/@duration@) and
enum @defined_only@ through the compile-time path.
-}
eventProto :: Text
eventProto =
  "syntax = \"proto3\";\n\
  \package test.v1;\n\
  \enum Kind { K_UNSPECIFIED = 0; K_A = 1; K_B = 2; }\n\
  \message Event {\n\
  \  google.protobuf.Timestamp at = 1 [(buf.validate.field).timestamp.gt = { seconds: 1000 nanos: 500 }];\n\
  \  google.protobuf.Duration ttl = 2 [(buf.validate.field).duration.lte = { seconds: 60 }];\n\
  \  Kind kind = 3 [(buf.validate.field).enum.defined_only = true];\n\
  \}\n"


ignoreProto :: Text
ignoreProto =
  "syntax = \"proto3\";\n\
  \message Ignore {\n\
  \  string implicit = 1 [(buf.validate.field).ignore = IGNORE_IF_ZERO_VALUE, (buf.validate.field).string.min_len = 3];\n\
  \  optional string explicit = 2 [(buf.validate.field).ignore = IGNORE_IF_ZERO_VALUE, (buf.validate.field).string.min_len = 3];\n\
  \  string always = 3 [(buf.validate.field) = { ignore: IGNORE_ALWAYS }, (buf.validate.field).required = true, (buf.validate.field).string.min_len = 3];\n\
  \}\n"


parityProto :: Text
parityProto =
  T.unlines
    [ "syntax = \"proto3\";"
    , "message Child { string code = 1 [(buf.validate.field).required = true]; }"
    , "message Parent {"
    , "  bool flag = 1 [(buf.validate.field).bool.const = false];"
    , "  Child child = 2;"
    , "  repeated string labels = 3 [(buf.validate.field).repeated.items.string.min_len = 2];"
    , "  map<string, int32> scores = 4 [(buf.validate.field).map.keys.string.min_len = 2,"
    , "    (buf.validate.field).map.values.int32.gt = 0];"
    , "  string name = 5 [(buf.validate.field).string.min_len = 2,"
    , "    (buf.validate.field).cel = { id: \"uses_rules\", message: \"unexpected rule\", expression: \"rules.min_len == 2u\" }];"
    , "  string uuid = 6 [(buf.validate.field).string.uuid = true];"
    , "}"
    ]


descriptorScalarProto :: Text
descriptorScalarProto =
  T.unlines
    [ "syntax = \"proto3\";"
    , "import \"buf/validate/validate.proto\";"
    , "import \"google/protobuf/timestamp.proto\";"
    , "import \"google/protobuf/duration.proto\";"
    , "enum Choice { CHOICE_UNSPECIFIED = 0; CHOICE_ONE = 1; CHOICE_TWO = 2; }"
    , "message ScalarRules {"
    , "  google.protobuf.Timestamp at = 1 [(buf.validate.field).timestamp.gt = { seconds: 1000 nanos: 500 }];"
    , "  google.protobuf.Duration ttl = 2 [(buf.validate.field).duration.lte = { seconds: -60 nanos: -10 }];"
    , "  google.protobuf.Timestamp recent = 3 [(buf.validate.field).timestamp.within = { nanos: 50 }];"
    , "  Choice choice = 4 [(buf.validate.field).enum.in = 1, (buf.validate.field).enum.not_in = 2];"
    , "  bytes payload = 5 [(buf.validate.field).bytes.prefix = \"a\", (buf.validate.field).bytes.in = \"ab\","
    , "    (buf.validate.field).bytes.in = \"ax\", (buf.validate.field).bytes.not_in = \"ax\"];"
    , "  sfixed32 delta = 6 [(buf.validate.field).sfixed32.lte = -2,"
    , "    (buf.validate.field).sfixed32.in = -2, (buf.validate.field).sfixed32.in = -3];"
    , "  float fraction = 7 [(buf.validate.field).float.const = 0.1];"
    , "  double real = 8 [(buf.validate.field).double.finite = true];"
    , "  string label = 9 [(buf.validate.field).string.len_bytes = 2];"
    , "  string network = 10 [(buf.validate.field).string.ipv4_with_prefixlen = true];"
    , "}"
    ]


descriptorCollectionProto :: Text
descriptorCollectionProto =
  T.unlines
    [ "syntax = \"proto3\";"
    , "package fixture.v1;"
    , "import \"buf/validate/validate.proto\";"
    , "message Child { string code = 1 [(buf.validate.field).required = true]; }"
    , "message CollectionRules {"
    , "  repeated string labels = 1 [(buf.validate.field).repeated = {"
    , "    min_items: 1 items: { string: { min_len: 2 }"
    , "      cel: { id: \"item.prefix\" message: \"item must start with a\" expression: \"this.startsWith('a')\" } } }];"
    , "  map<string, int32> scores = 2 [(buf.validate.field) = {"
    , "    map: { min_pairs: 1 keys: { string: { min_len: 2 } } values: { int32: { gt: 0 } } } }];"
    , "  repeated Child children = 3;"
    , "  map<string, Child> lookup = 4;"
    , "  repeated Child ignored_items = 5 [(buf.validate.field).repeated.items.ignore = IGNORE_ALWAYS];"
    , "  map<string, Child> ignored_values = 6 [(buf.validate.field).map.values = {"
    , "    ignore: IGNORE_ALWAYS cel: { id: \"ignored\" expression: \"this +\" } }];"
    , "  string tail = 7 [(buf.validate.field).string.min_len = 2];"
    , "  oneof identity {"
    , "    option (buf.validate.oneof) = { required: true };"
    , "    string alias = 8;"
    , "    int32 code = 9;"
    , "  }"
    , "}"
    ]


descriptorContextProto :: Text
descriptorContextProto =
  T.unlines
    [ "syntax = \"proto3\";"
    , "package fixture.v1;"
    , "import \"buf/validate/validate.proto\";"
    , "enum Kind { GLOBAL_DEFAULT = 0; GLOBAL_ONE = 1; }"
    , "message ContextRules {"
    , "  enum Kind { LOCAL_DEFAULT = 0; LOCAL_NEGATIVE = -2; }"
    , "  Kind local = 1 [(buf.validate.field).enum.defined_only = true];"
    , "  .fixture.v1.Kind global = 2 [(buf.validate.field).enum.defined_only = true];"
    , "  repeated Kind kinds = 3 [(buf.validate.field).repeated.items.enum.defined_only = true];"
    , "  map<string, Kind> lookup = 4 [(buf.validate.field).map.values.enum.defined_only = true];"
    , "  string header = 5 [(buf.validate.field).string.well_known_regex = KNOWN_REGEX_HTTP_HEADER_NAME];"
    , "  string header_value = 6 [(buf.validate.field).string.well_known_regex = KNOWN_REGEX_HTTP_HEADER_VALUE];"
    , "  repeated string loose = 7 [(buf.validate.field).repeated.items.string = {"
    , "    well_known_regex: KNOWN_REGEX_HTTP_HEADER_NAME strict: false }];"
    , "  map<string, string> headers = 8 [(buf.validate.field).map = {"
    , "    keys: { string: { well_known_regex: KNOWN_REGEX_HTTP_HEADER_NAME } }"
    , "    values: { string: { well_known_regex: KNOWN_REGEX_HTTP_HEADER_VALUE strict: false } } }];"
    , "}"
    ]


shorthandProto :: Text
shorthandProto =
  T.unlines
    [ "syntax = \"proto3\";"
    , "package fixture.v1;"
    , "import \"buf/validate/validate.proto\";"
    , "message Shorthand {"
    , "  option (buf.validate.message) = {"
    , "    cel: { id: \"message.explicit\" expression: \"this.code == 2\" }"
    , "    cel_expression: [\"this.code != 0\", \"this.code >= 2\"] };"
    , "  int32 code = 1 [(buf.validate.field).cel = { id: \"code.explicit\" message: \"code is too small\" expression: \"this >= 1\" },"
    , "    (buf.validate.field).cel_expression = \"this > 0\","
    , "    (buf.validate.field).cel_expression = \"this == 2 ? '' : 'code must equal 2'\"];"
    , "  repeated string labels = 2 [(buf.validate.field).repeated.items = { cel_expression: \"this.size() > 1\" }];"
    , "  map<string, int32> scores = 3 [(buf.validate.field).map = {"
    , "    keys: { cel_expression: \"this != 'bad'\" } values: { cel_expression: \"this > 0\" } }];"
    , "  int32 ignored = 4 [(buf.validate.field) = { ignore: IGNORE_ALWAYS cel_expression: \"this +\" }];"
    , "}"
    ]


presenceProto :: Text
presenceProto =
  T.unlines
    [ "syntax = \"proto3\";"
    , "import \"buf/validate/validate.proto\";"
    , "message Presence {"
    , "  string implicit = 1 [(buf.validate.field).required = true];"
    , "  optional string explicit = 2 [(buf.validate.field).required = true];"
    , "  repeated string entries = 3 [(buf.validate.field).required = true];"
    , "  map<string, int32> counts = 4 [(buf.validate.field).required = true];"
    , "  string ordinary = 5 [(buf.validate.field).string.min_len = 1];"
    , "  oneof selection {"
    , "    string selected = 6 [(buf.validate.field).string.min_len = 1];"
    , "  }"
    , "  double signed_zero = 7 [(buf.validate.field).required = true];"
    , "}"
    ]


editionPresenceProto :: Text
editionPresenceProto =
  T.unlines
    [ "edition = \"2023\";"
    , "import \"buf/validate/validate.proto\";"
    , "option features.field_presence = IMPLICIT;"
    , "message EditionPresence {"
    , "  int32 inherited = 1 [(buf.validate.field).required = true];"
    , "  int32 explicit = 2 [features.field_presence = EXPLICIT, (buf.validate.field).required = true];"
    , "  int32 ordinary = 3 [(buf.validate.field).int32.gt = 0];"
    , "}"
    ]


messageCelProto :: Text
messageCelProto =
  T.unlines
    [ "syntax = \"proto3\";"
    , "package fixture.v1;"
    , "import \"buf/validate/validate.proto\";"
    , "message Projection {"
    , "  option (buf.validate.message).cel_expression = \"this.code == 0 && !has(this.code)\";"
    , "  option (buf.validate.message).cel_expression = \"this.optional_code == 0 && has(this.optional_code) == this.expect_present\";"
    , "  option (buf.validate.message).cel_expression = \"this.chosen == 0 && has(this.chosen) == this.expect_present\";"
    , "  option (buf.validate.message).cel_expression = \"this.child.next.next.count == 0 && this.child.count == 0\";"
    , "  option (buf.validate.message).cel_expression = \"has(this.children) == (this.children.size() > 0) && this.children.all(c, c.count == 0)\";"
    , "  option (buf.validate.message).cel_expression = \"has(this.lookup) == (this.lookup.size() > 0) && this.lookup.all(k, this.lookup[k].count == 0)\";"
    , "  int32 code = 1 [(buf.validate.field).ignore = IGNORE_ALWAYS];"
    , "  optional int32 optional_code = 2;"
    , "  repeated Child children = 3 [(buf.validate.field).repeated.items.cel_expression = \"this.count == 0\"];"
    , "  map<string, Child> lookup = 4;"
    , "  Child child = 5 [(buf.validate.field).cel_expression = \"this.count == 0\"];"
    , "  oneof selection { int32 chosen = 6; }"
    , "  bool expect_present = 7;"
    , "}"
    , "message Child { int32 count = 1; Child next = 2; }"
    ]


explicitDefaultsProto :: Text
explicitDefaultsProto =
  T.unlines
    [ "syntax = \"proto2\";"
    , "import \"buf/validate/validate.proto\";"
    , "message ExplicitDefaults {"
    , "  enum State { FIRST = 1; SECOND = 2; }"
    , "  option (buf.validate.message).cel_expression = \"this.count == 7 && this.state == 2 && this.label == 'é😀' && this.data == b'\\\\377\\\\303\\\\251'\";"
    , "  option (buf.validate.message).cel_expression = \"has(this.count) == this.expect_present && has(this.state) == this.expect_present\";"
    , "  optional int32 count = 1 [default = 7];"
    , "  optional State state = 2 [default = SECOND];"
    , "  option (buf.validate.message).cel_expression = \"1.0 / this.zero < 0.0 && this.limit > 1e300\";"
    , "  optional string label = 3 [default = '\\303' \"\\xA9\\uD83D\\uDE00\"];"
    , "  optional bytes data = 4 [default = '\\777é', (buf.validate.field).bytes.prefix = '\\377'];"
    , "  optional bool expect_present = 5;"
    , "  optional double zero = 6 [default = -0x0];"
    , "  optional double limit = 7 [default = inf];"
    , "  oneof selection { State chosen = 8 [default = SECOND]; }"
    , "  option (buf.validate.message).cel_expression = \"this.chosen == 2 && has(this.chosen) == this.expect_present\";"
    , "}"
    ]


recursiveProto :: Text
recursiveProto =
  T.unlines
    [ "syntax = \"proto3\";"
    , "package fixture.recursive;"
    , "import \"buf/validate/validate.proto\";"
    , "message Node {"
    , "  int32 code = 1 [(buf.validate.field).int32.gte = 0];"
    , "  Node next = 2;"
    , "  repeated Node children = 3;"
    , "  map<string, Node> lookup = 4;"
    , "  Peer peer = 5;"
    , "  Node ignored = 6 [(buf.validate.field).ignore = IGNORE_ALWAYS];"
    , "  repeated Node ignored_items = 7 [(buf.validate.field).repeated.items.ignore = IGNORE_ALWAYS];"
    , "  map<string, Node> ignored_values = 8 [(buf.validate.field).map.values.ignore = IGNORE_ALWAYS];"
    , "  oneof edge { Node selected = 9; }"
    , "}"
    , "message Peer { Node node = 1; }"
    ]


messageOneofProto :: Text
messageOneofProto =
  T.unlines
    [ "syntax = \"proto3\";"
    , "import \"buf/validate/validate.proto\";"
    , "message Exclusive {"
    , "  option (buf.validate.message) = {"
    , "    oneof: { fields: [\"name\", \"labels\", \"flag\", \"entries\"], required: true }"
    , "    oneof: { fields: [\"status\", \"region\"] }"
    , "  };"
    , "  string name = 1 [(buf.validate.field).string.min_len = 2];"
    , "  repeated string labels = 2 [(buf.validate.field).repeated.min_items = 1];"
    , "  optional bool flag = 3;"
    , "  map<string, int32> entries = 4 [(buf.validate.field).map.min_pairs = 1];"
    , "  string status = 5 [(buf.validate.field).string.min_len = 1, (buf.validate.field).ignore = IGNORE_UNSPECIFIED];"
    , "  uint32 region = 6;"
    , "}"
    ]


predefinedRules :: MessageRules
predefinedRules = messageRules [("data", emptyFieldRules {frPredefined = [(constraint, literal)]})] []
  where
    -- A caller can construct a Constraint directly. The AST governs execution.
    constraint =
      parsed
        { constraintSource = "false"
        , constraintExpr = EAnd (constraintExpr parsed) (ECall Nothing "isNan" [ELit (LDouble (0 / 0))])
        }
    parsed =
      unsafeConstraint
        "value_matches"
        "configured value differs"
        "this == rule.payload && rule.tag == int && isInf(rule.limit) && isNan(rule.nan) && 1.0 / rule.zero < 0.0"
    literal =
      VMap $
        celMapFromList
          [ (VString "payload", VBytes (BS.pack [0, 255]))
          , (VString "tag", VType TyInt)
          , (VString "limit", VDouble (1 / 0))
          , (VString "nan", VDouble (0 / 0))
          , (VString "zero", VDouble (-0.0))
          ]
