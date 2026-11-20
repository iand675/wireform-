module Test.Parser (parserTests) where

import Data.ByteString qualified as BS
import Data.Either (isLeft, isRight)
import Data.List (isInfixOf)
import Data.Text (Text)
import Proto.IDL.AST
import Proto.IDL.Parser
import Proto.IDL.Print (printConstant)
import Test.Syd


parserTests :: Spec
parserTests =
  describe
    "Parser"
    $ sequence_
      [ describe
          "Syntax declaration"
          $ sequence_
            [ it "proto3 syntax" $ do
                let input = "syntax = \"proto3\";\n"
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> protoSyntax pf `shouldBe` Proto3
            , it "proto2 syntax" $ do
                let input = "syntax = \"proto2\";\n"
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> protoSyntax pf `shouldBe` Proto2
            , it "default syntax is proto3" $ do
                case parseProtoFile "<test>" "" of
                  Left e -> expectationFailure (show e)
                  Right pf -> protoSyntax pf `shouldBe` Proto3
            ]
      , describe
          "Package declaration"
          $ sequence_
            [ it "simple package" $ do
                let input = "syntax = \"proto3\";\npackage mypackage;\n"
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> protoPackage pf `shouldBe` Just "mypackage"
            , it "dotted package" $ do
                let input = "syntax = \"proto3\";\npackage com.example.api;\n"
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> protoPackage pf `shouldBe` Just "com.example.api"
            ]
      , describe
          "Import declarations"
          $ sequence_
            [ it "simple import" $ do
                let input = "syntax = \"proto3\";\nimport \"other.proto\";\n"
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoImports pf of
                    [imp] -> importPath imp `shouldBe` "other.proto"
                    _ -> expectationFailure "Expected exactly one import"
            , it "public import" $ do
                let input = "syntax = \"proto3\";\nimport public \"other.proto\";\n"
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoImports pf of
                    [imp] -> importModifier imp `shouldBe` Just ImportPublic
                    _ -> expectationFailure "Expected exactly one import"
            ]
      , describe
          "Message definitions"
          $ sequence_
            [ it "simple message with scalar fields" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "message Person {"
                        , "  string name = 1;"
                        , "  int32 age = 2;"
                        , "  bool active = 3;"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLMessage msg] -> do
                      msgName msg `shouldBe` "Person"
                      length (msgElements msg) `shouldBe` 3
                    _ -> expectationFailure "Expected one message"
            , it "message with all scalar types" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "message AllTypes {"
                        , "  double f1 = 1;"
                        , "  float f2 = 2;"
                        , "  int32 f3 = 3;"
                        , "  int64 f4 = 4;"
                        , "  uint32 f5 = 5;"
                        , "  uint64 f6 = 6;"
                        , "  sint32 f7 = 7;"
                        , "  sint64 f8 = 8;"
                        , "  fixed32 f9 = 9;"
                        , "  fixed64 f10 = 10;"
                        , "  sfixed32 f11 = 11;"
                        , "  sfixed64 f12 = 12;"
                        , "  bool f13 = 13;"
                        , "  string f14 = 14;"
                        , "  bytes f15 = 15;"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLMessage msg] -> length (msgElements msg) `shouldBe` 15
                    _ -> expectationFailure "Expected one message"
            , it "nested message" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "message Outer {"
                        , "  message Inner {"
                        , "    int32 value = 1;"
                        , "  }"
                        , "  Inner inner = 1;"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLMessage msg] -> do
                      msgName msg `shouldBe` "Outer"
                      let hasNestedMsg = any isNestedMsg (msgElements msg)
                      (hasNestedMsg) `shouldBe` True
                    _ -> expectationFailure "Expected one message"
            , it "repeated fields" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "message Msg {"
                        , "  repeated string tags = 1;"
                        , "  repeated int32 values = 2;"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLMessage msg] ->
                      let fields = extractFieldDefs (msgElements msg)
                      in all (\f -> fieldLabel f == Just Repeated) fields `shouldBe` True
                    _ -> expectationFailure "Expected one message"
            , it "map fields" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "message Msg {"
                        , "  map<string, int32> counts = 1;"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLMessage msg] -> case msgElements msg of
                      [MEMapField mf] -> do
                        mapKeyType mf `shouldBe` SString
                        mapValueType mf `shouldBe` FTScalar SInt32
                        mapFieldName mf `shouldBe` "counts"
                      _ -> expectationFailure "Expected one map field"
                    _ -> expectationFailure "Expected one message"
            ]
      , describe
          "Oneof"
          $ sequence_
            [ it "oneof definition" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "message Msg {"
                        , "  oneof value {"
                        , "    string name = 1;"
                        , "    int32 id = 2;"
                        , "  }"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLMessage msg] -> case msgElements msg of
                      [MEOneof od] -> do
                        oneofName od `shouldBe` "value"
                        length (oneofFields od) `shouldBe` 2
                      _ -> expectationFailure "Expected one oneof"
                    _ -> expectationFailure "Expected one message"
            ]
      , describe
          "Enum definitions"
          $ sequence_
            [ it "simple enum" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "enum Status {"
                        , "  UNKNOWN = 0;"
                        , "  ACTIVE = 1;"
                        , "  INACTIVE = 2;"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLEnum ed] -> do
                      enumName ed `shouldBe` "Status"
                      length (enumValues ed) `shouldBe` 3
                    _ -> expectationFailure "Expected one enum"
            , it "enum with allow_alias option" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "enum Status {"
                        , "  option allow_alias = true;"
                        , "  UNKNOWN = 0;"
                        , "  ACTIVE = 1;"
                        , "  RUNNING = 1;"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLEnum ed] -> do
                      length (enumOptions ed) `shouldBe` 1
                      length (enumValues ed) `shouldBe` 3
                    _ -> expectationFailure "Expected one enum"
            ]
      , describe
          "Service definitions"
          $ sequence_
            [ it "simple service" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "service Greeter {"
                        , "  rpc SayHello (HelloRequest) returns (HelloReply);"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLService svc] -> do
                      svcName svc `shouldBe` "Greeter"
                      case svcRpcs svc of
                        [rpc] -> do
                          rpcName rpc `shouldBe` "SayHello"
                          rpcInput rpc `shouldBe` "HelloRequest"
                          rpcOutput rpc `shouldBe` "HelloReply"
                          rpcInputStr rpc `shouldBe` NoStream
                          rpcOutputStr rpc `shouldBe` NoStream
                        _ -> expectationFailure "Expected one RPC"
                    _ -> expectationFailure "Expected one service"
            , it "streaming rpc" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "service Chat {"
                        , "  rpc Stream (stream Msg) returns (stream Msg);"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLService svc] -> case svcRpcs svc of
                      [rpc] -> do
                        rpcInputStr rpc `shouldBe` Streaming
                        rpcOutputStr rpc `shouldBe` Streaming
                      _ -> expectationFailure "Expected one RPC"
                    _ -> expectationFailure "Expected one service"
            ]
      , describe
          "Options and annotations"
          $ sequence_
            [ it "file-level option" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "option java_package = \"com.example\";"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoOptions pf of
                    [opt] -> do
                      optValue opt `shouldBe` CString "com.example"
                    _ -> expectationFailure "Expected one option"
            , it "custom extension option" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "option (my_custom_opt) = true;"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoOptions pf of
                    [opt] -> do
                      case optNameParts (optName opt) of
                        [ExtensionOption n] -> n `shouldBe` "my_custom_opt"
                        _ -> expectationFailure "Expected extension option"
                      optValue opt `shouldBe` CBool True
                    _ -> expectationFailure "Expected one option"
            , it "field options" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "message Msg {"
                        , "  string name = 1 [deprecated = true, json_name = \"Name\"];"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLMessage msg] -> case msgElements msg of
                      [MEField fd] -> length (fieldOptions fd) `shouldBe` 2
                      _ -> expectationFailure "Expected one field"
                    _ -> expectationFailure "Expected one message"
            , it "aggregate option value" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "option (my_opt) = { foo: 1 bar: \"hello\" };"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoOptions pf of
                    [opt] -> case optValue opt of
                      CAggregate kvs -> length kvs `shouldBe` 2
                      _ -> expectationFailure "Expected aggregate constant"
                    _ -> expectationFailure "Expected one option"
            ]
      , describe
          "Reserved"
          $ sequence_
            [ it "reserved field numbers" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "message Msg {"
                        , "  reserved 2, 15, 9 to 11;"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLMessage msg] -> case msgElements msg of
                      [MEReserved (ReservedNumbers ranges)] ->
                        length ranges `shouldBe` 3
                      _ -> expectationFailure "Expected reserved numbers"
                    _ -> expectationFailure "Expected one message"
            , it "reserved field names" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "message Msg {"
                        , "  reserved \"foo\", \"bar\";"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLMessage msg] -> case msgElements msg of
                      [MEReserved (ReservedNames names)] ->
                        names `shouldBe` [QuotedReservedName "foo", QuotedReservedName "bar"]
                      _ -> expectationFailure "Expected reserved names"
                    _ -> expectationFailure "Expected one message"
            ]
      , describe
          "Empty statements (trailing/stray semicolons)"
          $ sequence_
            [ it "trailing ';' after a message block (protoc tolerates '};')" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "message Foo {"
                        , "  string name = 1;"
                        , "};"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLMessage msg] -> do
                      msgName msg `shouldBe` "Foo"
                      length (msgElements msg) `shouldBe` 1
                    _ -> expectationFailure "Expected exactly one message"
            , it "trailing ';' after an enum block" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "enum Status {"
                        , "  UNKNOWN = 0;"
                        , "  ACTIVE = 1;"
                        , "};"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLEnum ed] -> do
                      enumName ed `shouldBe` "Status"
                      length (enumValues ed) `shouldBe` 2
                    _ -> expectationFailure "Expected exactly one enum"
            , it "trailing ';' after a service block" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "service Greeter {"
                        , "  rpc SayHello (HelloRequest) returns (HelloReply);"
                        , "};"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLService svc] -> svcName svc `shouldBe` "Greeter"
                    _ -> expectationFailure "Expected exactly one service"
            , it "trailing ';' after a nested message inside a message body" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "message Outer {"
                        , "  message Inner {"
                        , "    int32 value = 1;"
                        , "  };"
                        , "  enum E {"
                        , "    A = 0;"
                        , "  };"
                        , "  Inner inner = 1;"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLMessage msg] -> do
                      msgName msg `shouldBe` "Outer"
                      let nested = filter isNestedMsg (msgElements msg)
                      length nested `shouldBe` 1
                    _ -> expectationFailure "Expected exactly one message"
            , it "trailing ';' after a oneof block" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "message Msg {"
                        , "  oneof value {"
                        , "    string name = 1;"
                        , "    int32 id = 2;"
                        , "  };"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLMessage msg] -> case msgElements msg of
                      [MEOneof od] -> length (oneofFields od) `shouldBe` 2
                      _ -> expectationFailure "Expected exactly one oneof"
                    _ -> expectationFailure "Expected exactly one message"
            , it "stray ';' between top-level declarations" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , ";"
                        , "message A { int32 x = 1; };"
                        , ";"
                        , "message B { int32 y = 1; }"
                        , ";"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> length (protoTopLevels pf) `shouldBe` 2
            , it "stray ';' between message elements" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "message Msg {"
                        , "  ;"
                        , "  int32 x = 1;"
                        , "  ;"
                        , "  int32 y = 2;"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLMessage msg] ->
                      length (extractFieldDefs (msgElements msg)) `shouldBe` 2
                    _ -> expectationFailure "Expected exactly one message"
            , it "stray ';' inside an enum body" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "enum E {"
                        , "  ;"
                        , "  A = 0;"
                        , "  ;"
                        , "  B = 1;"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLEnum ed] -> length (enumValues ed) `shouldBe` 2
                    _ -> expectationFailure "Expected exactly one enum"
            , it "stray ';' inside a service body and after an rpc block" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "service S {"
                        , "  ;"
                        , "  rpc A (Req) returns (Resp) {"
                        , "    option deprecated = true;"
                        , "    ;"
                        , "  };"
                        , "  rpc B (Req) returns (Resp);"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLService svc] -> length (svcRpcs svc) `shouldBe` 2
                    _ -> expectationFailure "Expected exactly one service"
            , it "trailing ';' after an extend block" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto2\";"
                        , "extend Foo {"
                        , "  optional int32 bar = 100;"
                        , "};"
                        ]
                (isRight (parseProtoFile "<test>" input)) `shouldBe` True
            , it "still rejects a genuinely missing closing brace" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "message Foo {"
                        , "  string name = 1;"
                        , ";"
                        ]
                (isLeft (parseProtoFile "<test>" input)) `shouldBe` True
            ]
      , describe
          "Lexical & grammar conformance"
          $ sequence_
            [ it "enum reserved numeric ranges (to / to max)" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "enum E {"
                        , "  A = 0;"
                        , "  reserved 2, 5 to 9, 100 to max;"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLEnum ed] -> length (enumValues ed) `shouldBe` 1
                    _ -> expectationFailure "Expected one enum"
            , it "identifier prefixed by 'true' is an identifier, not a bool" $
                fileOptValue "trueish" `shouldBe` Just (CIdent "trueish")
            , it "identifier prefixed by 'inf' is an identifier, not infinity" $
                fileOptValue "information" `shouldBe` Just (CIdent "information")
            , it "bare 'true' is still a boolean" $
                fileOptValue "true" `shouldBe` Just (CBool True)
            , it "bare 'inf' is still infinity" $
                case fileOptValue "inf" of
                  Just (CFloat d) -> (isInfinite d && d > 0) `shouldBe` True
                  other -> expectationFailure ("expected +inf, got " <> show other)
            , it "bare 'nan' is still NaN" $
                case fileOptValue "nan" of
                  Just (CFloat d) -> isNaN d `shouldBe` True
                  other -> expectationFailure ("expected NaN, got " <> show other)
            , it "leading-dot float (.5)" $
                fileOptValue ".5" `shouldBe` Just (CFloat 0.5)
            , it "trailing-dot float (5.)" $
                fileOptValue "5." `shouldBe` Just (CFloat 5.0)
            , it "negative leading-dot float (-.25)" $
                fileOptValue "-.25" `shouldBe` Just (CFloat (-0.25))
            , it "single-hex-digit escape (\\xA)" $
                fileOptValue "\"\\xA\"" `shouldBe` Just (CString "\n")
            , it "two-hex-digit escape still works (\\x41)" $
                fileOptValue "\"\\x41\"" `shouldBe` Just (CString "A")
            , it "BMP unicode escape (\\u00e9)" $
                fileOptValue "\"caf\\u00e9\"" `shouldBe` Just (CString "caf\233")
            , it "full unicode escape (\\U0001F600)" $
                fileOptValue "\"\\U0001F600\"" `shouldBe` Just (CString "\128512")
            , it "decodes escaped octets after concatenating adjacent literals" $
                fileOptValue "\"\\303\" /* split */ '\\xA9'" `shouldBe` Just (CString "é")
            , it "joins UTF-16 surrogate pairs in Unicode escapes" $
                fileOptValue "\"\\uD83D\\uDE00\"" `shouldBe` Just (CString "\128512")
            , it "keeps non-UTF-8 octets available for bytes options" $
                fileOptValue "\"\\777\\uD83D\"" `shouldBe` Just (CBytes (BS.pack [255, 237, 160, 189]))
            , it "rejects non-UTF-8 string defaults without substituting characters" $
                parseProtoFile "<test>" "syntax = \"proto2\"; message X { optional string x = 1 [default = '\\377']; }"
                  `shouldSatisfy` isLeft
            , it "prints an escaped NUL in a string literal" $
                fileOptValue (printConstant (CString "\NUL7")) `shouldBe` Just (CString "\NUL7")
            , it "nested extension option name (a).(b).c" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "option (a).(b).c = 1;"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoOptions pf of
                    [opt] ->
                      optNameParts (optName opt)
                        `shouldBe` [ExtensionOption "a", ExtensionOption "b", SimpleOption "c"]
                    _ -> expectationFailure "Expected one option"
            ]
      , describe
          "Groups and extension-range options"
          $ sequence_
            [ it "proto2 group desugars to a nested message plus a field" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto2\";"
                        , "message M {"
                        , "  repeated group Result = 1 {"
                        , "    optional int32 x = 1;"
                        , "  }"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLMessage m] -> case msgElements m of
                      [MEMessage nested, MEField fld] -> do
                        msgName nested `shouldBe` "Result"
                        length (msgElements nested) `shouldBe` 1
                        fieldName fld `shouldBe` "result"
                        fieldType fld `shouldBe` FTNamed "Result"
                        fieldNumber fld `shouldBe` FieldNumber 1
                        fieldLabel fld `shouldBe` Just Repeated
                      _ -> expectationFailure "Expected a nested message followed by a field"
                    _ -> expectationFailure "Expected one message"
            , it "unlabeled group parses" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto2\";"
                        , "message M {"
                        , "  group G = 2 {"
                        , "    optional int32 y = 1;"
                        , "  }"
                        , "}"
                        ]
                (isRight (parseProtoFile "<test>" input)) `shouldBe` True
            , it "a field whose name starts with 'group' is still a field" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "message M {"
                        , "  int32 grouping = 1;"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLMessage m] -> case msgElements m of
                      [MEField fld] -> fieldName fld `shouldBe` "grouping"
                      _ -> expectationFailure "Expected a single field"
                    _ -> expectationFailure "Expected one message"
            , it "extensions with a verification option" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto2\";"
                        , "message M {"
                        , "  extensions 4 to 8 [verification = UNVERIFIED];"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLMessage m] -> case msgElements m of
                      [MEExtensions ranges opts] -> do
                        length ranges `shouldBe` 1
                        length opts `shouldBe` 1
                      _ -> expectationFailure "Expected an extensions declaration"
                    _ -> expectationFailure "Expected one message"
            , it "extensions with a declaration aggregate option" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto2\";"
                        , "message M {"
                        , "  extensions 100 to max [declaration = { number: 100 full_name: \".foo.bar\" type: \".Baz\" }];"
                        , "}"
                        ]
                (isRight (parseProtoFile "<test>" input)) `shouldBe` True
            , it "extensions without options still parse (empty option list)" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto2\";"
                        , "message M {"
                        , "  extensions 4 to 8;"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLMessage m] -> case msgElements m of
                      [MEExtensions _ opts] -> length opts `shouldBe` 0
                      _ -> expectationFailure "Expected an extensions declaration"
                    _ -> expectationFailure "Expected one message"
            ]
      , describe
          "Nested extends"
          $ sequence_
            [ it "extend inside a message body is retained as MEExtend" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto2\";"
                        , "message M {"
                        , "  extend N {"
                        , "    optional int32 e = 100;"
                        , "  }"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLMessage m] -> case msgElements m of
                      [MEExtend owner fields] -> do
                        owner `shouldBe` "N"
                        length fields `shouldBe` 1
                      _ -> expectationFailure "Expected a single MEExtend element"
                    _ -> expectationFailure "Expected one message"
            , it "group inside a nested extend hoists the group message" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto2\";"
                        , "message M {"
                        , "  extend N {"
                        , "    optional group G = 100 {"
                        , "      optional int32 x = 1;"
                        , "    }"
                        , "  }"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLMessage m] -> do
                      let exts = filter isExtendElem (msgElements m)
                          msgs = filter isMsgElem (msgElements m)
                      length exts `shouldBe` 1
                      length msgs `shouldBe` 1
                    _ -> expectationFailure "Expected one message"
            , it "group inside a top-level extend hoists a top-level message" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto2\";"
                        , "extend N {"
                        , "  optional group G = 1 {"
                        , "    optional int32 x = 1;"
                        , "  }"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> do
                    let tops = protoTopLevels pf
                    length tops `shouldBe` 2
                    any isTopExtend tops `shouldBe` True
                    any isTopMessage tops `shouldBe` True
            ]
      , describe
          "Rich aggregate option values"
          $ sequence_
            [ it "list values desugar to repeated key/value pairs" $
                fileOptValue "{ tags: [\"a\", \"b\"] }"
                  `shouldBe` Just (CAggregate [("tags", CString "a"), ("tags", CString "b")])
            , it "extension keys keep their brackets" $
                fileOptValue "{ [foo.bar]: 1 }"
                  `shouldBe` Just (CAggregate [("[foo.bar]", CInt 1)])
            , it "Any-URL keys are accepted" $
                (isRight (parseProtoFile "<test>" (unlines' ["syntax = \"proto3\";", "option (x) = { [type.googleapis.com/foo.Bar]: { a: 1 } };"])))
                  `shouldBe` True
            , it "angle-bracket message values are accepted" $
                case fileOptValue "{ sub < a: 1 > }" of
                  Just (CAggregate [("sub", CAggregate [("a", CInt 1)])]) -> pure ()
                  other -> expectationFailure ("unexpected: " <> show other)
            ]
      , describe
          "Editions reserved identifiers"
          $ sequence_
            [ it "message reserved accepts bare identifiers" $ do
                let input =
                      unlines'
                        [ "edition = \"2023\";"
                        , "message M {"
                        , "  reserved foo, bar;"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> case protoTopLevels pf of
                    [TLMessage m] -> case msgElements m of
                      [MEReserved (ReservedNames names)] ->
                        names `shouldBe` [IdentReservedName "foo", IdentReservedName "bar"]
                      _ -> expectationFailure "Expected reserved names"
                    _ -> expectationFailure "Expected one message"
            , it "enum reserved accepts a bare identifier" $ do
                let input =
                      unlines'
                        [ "edition = \"2023\";"
                        , "enum E {"
                        , "  A = 0;"
                        , "  reserved FOO;"
                        , "}"
                        ]
                (isRight (parseProtoFile "<test>" input)) `shouldBe` True
            ]
      , describe
          "Complex proto files"
          $ sequence_
            [ it "full proto file" $ do
                let input = complexProto
                (isRight (parseProtoFile "<test>" input)) `shouldBe` True
            ]
      , describe
          "Comments"
          $ sequence_
            [ it "line comments" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\"; // this is a comment"
                        , "// another comment"
                        , "message Msg {"
                        , "  int32 x = 1; // field comment"
                        , "}"
                        ]
                (isRight (parseProtoFile "<test>" input)) `shouldBe` True
            , it "block comments" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "/* block comment */"
                        , "message Msg {"
                        , "  /* multi"
                        , "     line"
                        , "     comment */"
                        , "  int32 x = 1;"
                        , "}"
                        ]
                (isRight (parseProtoFile "<test>" input)) `shouldBe` True
            ]
      , describe
          "Error message quality"
          $ sequence_
            [ it "missing semicolon points to correct location" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "message Foo {"
                        , "  string name = 1"
                        , "}"
                        ]
                case parseProtoFile "test.proto" input of
                  Left e -> do
                    let msg = renderParseError e
                    ("';'" `isInfixOf` msg) `shouldBe` True
                    ("test.proto:4:" `isInfixOf` msg) `shouldBe` True
                    ("string name = 1" `isInfixOf` msg) `shouldBe` True
                    ("^" `isInfixOf` msg) `shouldBe` True
                  Right _ -> expectationFailure "Should have failed to parse"
            , it "missing field number gives helpful message" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "message Foo {"
                        , "  string name = ;"
                        , "}"
                        ]
                case parseProtoFile "test.proto" input of
                  Left e -> do
                    let msg = renderParseError e
                    ("field number" `isInfixOf` msg || "integer" `isInfixOf` msg) `shouldBe` True
                    ("string name" `isInfixOf` msg) `shouldBe` True
                  Right _ -> expectationFailure "Should have failed to parse"
            , it "missing equals sign gives helpful message" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "message Foo {"
                        , "  string name 1;"
                        , "}"
                        ]
                case parseProtoFile "test.proto" input of
                  Left e -> do
                    let msg = renderParseError e
                    ("'='" `isInfixOf` msg) `shouldBe` True
                    ("test.proto:3:15" `isInfixOf` msg) `shouldBe` True
                  Right _ -> expectationFailure "Should have failed to parse"
            , it "invalid syntax version gives clear message" $ do
                let input = "syntax = \"proto4\";\n"
                case parseProtoFile "test.proto" input of
                  Left e -> do
                    let msg = renderParseError e
                    ("proto4" `isInfixOf` msg) `shouldBe` True
                    ("proto2" `isInfixOf` msg && "proto3" `isInfixOf` msg) `shouldBe` True
                  Right _ -> expectationFailure "Should have failed to parse"
            , it "unclosed string literal gives clear message" $ do
                let input = "syntax = \"proto3;\n"
                case parseProtoFile "test.proto" input of
                  Left e -> do
                    let msg = renderParseError e
                    ("newline" `isInfixOf` msg) `shouldBe` True
                    ("test.proto:1:" `isInfixOf` msg) `shouldBe` True
                  Right _ -> expectationFailure "Should have failed to parse"
            , it "missing message name gives clear message" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "message {"
                        , "  string name = 1;"
                        , "}"
                        ]
                case parseProtoFile "test.proto" input of
                  Left e -> do
                    let msg = renderParseError e
                    ("message name" `isInfixOf` msg) `shouldBe` True
                  Right _ -> expectationFailure "Should have failed to parse"
            , it "unexpected token at top level gives clear message" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "12345"
                        ]
                case parseProtoFile "test.proto" input of
                  Left e -> do
                    let msg = renderParseError e
                    ("message" `isInfixOf` msg || "top-level" `isInfixOf` msg) `shouldBe` True
                  Right _ -> expectationFailure "Should have failed to parse"
            , it "missing comma in map type gives clear message" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "message Foo {"
                        , "  map<string int32> x = 1;"
                        , "}"
                        ]
                case parseProtoFile "test.proto" input of
                  Left e -> do
                    let msg = renderParseError e
                    ("','" `isInfixOf` msg) `shouldBe` True
                    ("test.proto:3:14" `isInfixOf` msg) `shouldBe` True
                  Right _ -> expectationFailure "Should have failed to parse"
            , it "error messages include source context with line numbers" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "package myapp;"
                        , ""
                        , "message User {"
                        , "  string name = 1;"
                        , "  int32 age = 2;"
                        , "  string email = 3"
                        , "}"
                        ]
                case parseProtoFile "api/user.proto" input of
                  Left e -> do
                    let msg = renderParseError e
                    ("api/user.proto:" `isInfixOf` msg) `shouldBe` True
                    ("-->" `isInfixOf` msg) `shouldBe` True
                    (" | " `isInfixOf` msg) `shouldBe` True
                    ("^" `isInfixOf` msg) `shouldBe` True
                  Right _ -> expectationFailure "Should have failed to parse"
            , it "error output rejects invalid inputs" $ do
                let cases =
                      [ ("empty message body missing brace", "syntax = \"proto3\";\nmessage Foo {\n")
                      , ("unknown keyword at top level", "syntax = \"proto3\";\nfoobar baz;\n")
                      , ("missing closing brace for enum", "syntax = \"proto3\";\nenum Foo {\n  A = 0;\n")
                      ]
                mapM_
                  ( \(desc, input) ->
                      (if (isLeft (parseProtoFile "test.proto" input)) then pure () else expectationFailure (desc))
                  )
                  cases
            ]
      ]


complexProto :: Text
complexProto =
  unlines'
    [ "syntax = \"proto3\";"
    , "package example.api;"
    , ""
    , "import \"google/protobuf/timestamp.proto\";"
    , "import public \"common.proto\";"
    , ""
    , "option java_package = \"com.example.api\";"
    , "option (custom_file_opt) = true;"
    , ""
    , "enum Status {"
    , "  STATUS_UNKNOWN = 0;"
    , "  STATUS_ACTIVE = 1;"
    , "  STATUS_INACTIVE = 2;"
    , "}"
    , ""
    , "message Person {"
    , "  string name = 1;"
    , "  int32 id = 2;"
    , "  string email = 3;"
    , ""
    , "  enum PhoneType {"
    , "    MOBILE = 0;"
    , "    HOME = 1;"
    , "    WORK = 2;"
    , "  }"
    , ""
    , "  message PhoneNumber {"
    , "    string number = 1;"
    , "    PhoneType type = 2;"
    , "  }"
    , ""
    , "  repeated PhoneNumber phones = 4;"
    , "  Status status = 5;"
    , "  map<string, string> metadata = 6;"
    , ""
    , "  oneof contact {"
    , "    string phone = 7;"
    , "    string email_alt = 8;"
    , "  }"
    , ""
    , "  reserved 10, 12 to 15;"
    , "  reserved \"old_field\";"
    , "}"
    , ""
    , "service PersonService {"
    , "  rpc GetPerson (GetPersonRequest) returns (Person);"
    , "  rpc ListPersons (ListRequest) returns (stream Person);"
    , "  rpc UpdatePerson (stream Person) returns (UpdateResponse) {"
    , "    option deprecated = true;"
    , "  }"
    , "}"
    ]


isNestedMsg :: MessageElement -> Bool
isNestedMsg (MEMessage _) = True
isNestedMsg _ = False


isExtendElem :: MessageElement -> Bool
isExtendElem (MEExtend _ _) = True
isExtendElem _ = False


isMsgElem :: MessageElement -> Bool
isMsgElem (MEMessage _) = True
isMsgElem _ = False


isTopExtend :: TopLevel -> Bool
isTopExtend (TLExtend _ _) = True
isTopExtend _ = False


isTopMessage :: TopLevel -> Bool
isTopMessage (TLMessage _) = True
isTopMessage _ = False


extractFieldDefs :: [MessageElement] -> [FieldDef]
extractFieldDefs = concatMap go
  where
    go (MEField fd) = [fd]
    go _ = []


unlines' :: [Text] -> Text
unlines' = mconcat . fmap (<> "\n")


{- | Parse a single file-level @option (x) = <rhs>;@ and return the parsed
constant value. Used to exercise the constant/literal lexer in isolation.
-}
fileOptValue :: Text -> Maybe Constant
fileOptValue rhs =
  let input = unlines' ["syntax = \"proto3\";", "option (x) = " <> rhs <> ";"]
  in case parseProtoFile "<test>" input of
       Right pf -> case protoOptions pf of
         [opt] -> Just (optValue opt)
         _ -> Nothing
       Left _ -> Nothing
