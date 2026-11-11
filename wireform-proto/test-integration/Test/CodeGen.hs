module Test.CodeGen (codeGenTests) where

import Control.Exception (IOException)
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime)
import Data.Vector qualified as V
import Proto.CodeGen
import Proto.CodeGen.Types
import Proto.IDL.AST
import Proto.IDL.Annotations
import Proto.IDL.Descriptor (astToFileDescriptor, fileDescriptorToAST)
import Proto.IDL.Parser
import Proto.IDL.Parser.Resolver (ResolvedProto (..))
import Proto.IDL.Print (printProtoFile)
import Proto.Decode (UnknownField (..))
import Proto.Google.Protobuf.Descriptor qualified as Descriptor
import Proto.Setup (ProtoGenConfig (..), defaultProtoGenConfig, generateProtoFile, generateProtos)
import System.Directory (createDirectory, createDirectoryIfMissing, doesDirectoryExist, getModificationTime, setModificationTime)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Syd


codeGenTests :: Spec
codeGenTests =
  describe
    "Code Generation"
    $ sequence_
      [ describe
          "Type name conversion"
          $ sequence_
            [ it "simple name" $
                hsTypeName "person" `shouldBe` "Person"
            , it "already capitalized" $
                hsTypeName "Person" `shouldBe` "Person"
            , it "field name conversion" $
                hsFieldName "first_name" `shouldBe` "firstName"
            , it "field name no underscore" $
                hsFieldName "name" `shouldBe` "name"
            , it "enum constructor" $
                hsEnumCon "Status" "STATUS_ACTIVE" `shouldBe` "StatusActive"
            , it "module name" $
                hsModuleName "com.example.api" `shouldBe` "Com.Example.Api"
            , it "preserves distinct module names when proto path spelling would collapse" $ do
                schema <- either (fail . show) pure (parseProtoFile "example.proto" "syntax = 'proto3';")
                let name path = moduleNameForProto defaultGenerateOpts path schema
                    paths = ["my_file.proto", "my__file.proto", "myFile.proto", "my-file.proto", "a/my_file.proto", "a__b/my_file.proto"]
                name "my_file.proto" `shouldBe` "Proto.Gen.MyFile"
                name "my__file.proto" `shouldBe` "Proto.Gen.MyFile'Path'6d795f5f66696c65"
                moduleNameConflict defaultGenerateOpts (map (,schema) paths) `shouldBe` Nothing
                moduleNameConflict defaultGenerateOpts [("my_file.proto", schema), ("myfile.proto", schema)]
                  `shouldSatisfy` maybe False (T.isInfixOf "differing only by case")
            , it "escapes lowercase namespace components consistently through descriptors" $ do
                schema <- either (fail . show) pure $
                  parseProtoFile "schema.proto" "syntax = 'proto3'; option csharp_namespace = 'lower.Tools';"
                let moduleName = moduleNameForProto defaultGenerateOpts "schema.proto"
                    descriptorSchema = fileDescriptorToAST (astToFileDescriptor "schema.proto" schema)
                moduleName schema `shouldBe` "Proto.Gen.Lower'Path'6c6f776572.Tools.Schema"
                moduleName descriptorSchema `shouldBe` moduleName schema
            ]
      , describe
          "Code generation from parsed proto"
          $ sequence_
            [ it "generates module for simple message" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "package test;"
                        , "message Person {"
                        , "  string name = 1;"
                        , "  int32 age = 2;"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> do
                    let emptyReg = Map.empty :: TypeRegistry
                        code = generateModuleText defaultGenerateOpts emptyReg "<test>" pf
                    (T.isInfixOf "data Person" code) `shouldBe` True
                    (T.isInfixOf "name" code) `shouldBe` True
                    (T.isInfixOf "module" code) `shouldBe` True
            , it "proto3 optional scalar gets Maybe wrapper" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "package test;"
                        , "message Presence {"
                        , "  int32 required_field = 1;"
                        , "  optional int32 optional_field = 2;"
                        , "  optional string optional_name = 3;"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> do
                    let emptyReg = Map.empty :: TypeRegistry
                        code = generateModuleText defaultGenerateOpts emptyReg "<test>" pf
                    (T.isInfixOf "Maybe Int32" code) `shouldBe` True
                    (T.isInfixOf "Maybe Text" code) `shouldBe` True
                    (T.isInfixOf "Int32" code) `shouldBe` True
            , it "resolves same-file enum fields even without a supplied registry" $ do
                let input = "syntax = 'proto3'; package test; enum Mode { OFF = 0; ON = 1; } message Settings { Mode mode = 1; }"
                file <- either (fail . show) pure (parseProtoFile "settings.proto" input)
                let code = generateModuleText defaultGenerateOpts Map.empty "settings.proto" file
                code `shouldSatisfy` T.isInfixOf "settingsMode :: !Mode"
            , it "generates enum" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "enum Status {"
                        , "  UNKNOWN = 0;"
                        , "  ACTIVE = 1;"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> do
                    let emptyReg = Map.empty :: TypeRegistry
                        code = generateModuleText defaultGenerateOpts emptyReg "<test>" pf
                    (T.isInfixOf "data Status" code) `shouldBe` True
                    (T.isInfixOf "Active" code) `shouldBe` True
            , it "allocates distinct qualifiers for imported modules with the same short alias" $ do
                let parse path source = either (fail . show) pure (parseProtoFile path source)
                    opts = defaultGenerateOpts {genModulePrefix = "Proto"}
                    resolve path schema = (path, ResolvedProto schema path Map.empty)
                alpha <- parse "left/types.proto" "syntax = 'proto3'; package alpha; option csharp_namespace = 'Alpha.One'; message Thing {}"
                beta <- parse "right/types.proto" "syntax = 'proto3'; package beta; option csharp_namespace = 'Alpine.One'; message Thing {}"
                envelope <- parse "envelope.proto" "syntax = 'proto3'; message Envelope { .alpha.Thing left = 1; .beta.Thing right = 2; }"
                let descriptorBeta = fileDescriptorToAST (astToFileDescriptor "right/types.proto" beta)
                    registry = buildTypeRegistry opts [resolve "left/types.proto" alpha, resolve "right/types.proto" descriptorBeta, resolve "envelope.proto" envelope]
                    code = generateModuleText opts registry "envelope.proto" envelope
                code `shouldSatisfy` T.isInfixOf "import qualified Proto.Alpha.One.Types as AL_One_Types\n"
                code `shouldSatisfy` T.isInfixOf "import qualified Proto.Alpine.One.Types as AL_One_Types''Proto'Alpine'One'Types\n"
                code `shouldSatisfy` T.isInfixOf "envelopeRight :: !(Maybe AL_One_Types''Proto'Alpine'One'Types.Thing)"
            , it "prints and preserves option imports separately from public dependency indexes" $ do
                let source = "edition = '2024'; import public 'types.proto'; import option 'policy.proto';"
                schema <- either (fail . show) pure (parseProtoFile "schema.proto" source)
                let descriptor = astToFileDescriptor "schema.proto" schema
                Descriptor.fdpDependency descriptor `shouldBe` V.singleton "types.proto"
                protoImports (fileDescriptorToAST descriptor) `shouldBe` protoImports schema
                printProtoFile schema `shouldSatisfy` T.isInfixOf "import option \"policy.proto\";"
                parseProtoFile "schema.proto" "edition = '2023'; import option 'policy.proto';"
                  `shouldSatisfy` isLeft
                parseProtoFile "schema.proto" "edition = '2024'; import option 'policy.proto'; import 'types.proto';"
                  `shouldSatisfy` isLeft
                parseProtoFile "schema.proto" "edition = '2024'; import 'policy.proto'; import option 'policy.proto';"
                  `shouldSatisfy` isLeft
            , it "keeps resolved custom options in generated protoc descriptors" $ do
                schema <- either (fail . show) pure $
                  parseProtoFile "schema.proto" "edition = '2024'; import option 'policy.proto'; message Schema {}"
                let descriptor = astToFileDescriptor "schema.proto" schema
                    options = Descriptor.defaultMessageOptions {Descriptor.moUnknownFields = [UnknownVarint 51000 1]}
                    withOptions message = message {Descriptor.dpOptions = Just options}
                    supplied = descriptor {Descriptor.fdpMessageType = V.map withOptions (Descriptor.fdpMessageType descriptor)}
                    code = generateModuleTextFromDescriptor defaultGenerateOpts Map.empty supplied
                code `shouldSatisfy` T.isInfixOf "\\xc0\\xf3\\x18\\x01"
            , it "resolves custom options through public imports and rejects private transitive definitions" $ do
                let parse path source = either (fail . show) pure (parseProtoFile path source)
                consumer <- parse "consumer.proto" $
                  "edition = '2024'; import option 'forward.proto'; message Consumer { option (audit.enabled) = true; }"
                policy <- parse "policy.proto" $
                  "edition = '2024'; package audit; extend .google.protobuf.MessageOptions { bool enabled = 51000; }"
                public <- parse "forward.proto" "edition = '2024'; import public 'policy.proto';"
                private <- parse "forward.proto" "edition = '2024'; import 'policy.proto';"
                let generate forward = generateModuleTextWithImports defaultGenerateOpts Map.empty "consumer.proto"
                      (Map.fromList [("forward.proto", forward), ("policy.proto", policy)]) consumer
                generate public `shouldSatisfy` either (const False) (T.isInfixOf "\\xc0\\xf3\\x18\\x01")
                generate private `shouldSatisfy` either (T.isInfixOf "not visible") (const False)
            , it "validates the spelling of numeric custom options in context" $ do
                let parse path source = either (Left . T.pack . show) Right (parseProtoFile path source)
                    policy = parse "policy.proto" $
                      "edition = '2024'; package audit; message Payload { float number = 1; uint32 count = 2; } "
                        <> "extend .google.protobuf.MessageOptions { Payload payload = 51000; uint32 count = 51001; }"
                    generate assignment = do
                      definitions <- policy
                      consumer <- parse "consumer.proto" $
                        "edition = '2024'; import option 'policy.proto'; message Consumer { option "
                          <> assignment <> "; }"
                      generateModuleTextWithImports defaultGenerateOpts Map.empty "consumer.proto"
                        (Map.singleton "policy.proto" definitions) consumer
                    failedWith message = either (T.isInfixOf message) (const False)
                generate "(audit.payload) = { number: -0x0 }" `shouldSatisfy` failedWith "Expected a decimal"
                generate "(audit.payload) = { number: -00 }" `shouldSatisfy` failedWith "Expected a decimal"
                generate "(audit.payload) = { count: -0 }" `shouldSatisfy` failedWith "Unsigned field"
                generate "(audit.count) = -0" `shouldSatisfy` failedWith "Unsigned field"
            , it "requires named boolean and enum values outside option aggregates" $ do
                let parse path source = either (Left . T.pack . show) Right (parseProtoFile path source)
                    policy = parse "policy.proto" $
                      "edition = '2024'; package audit; enum Mode { MODE_UNSPECIFIED = 0; MODE_ON = 1; } "
                        <> "message Settings { bool enabled = 1; Mode mode = 2; } "
                        <> "extend .google.protobuf.MessageOptions { Settings settings = 51000; bool enabled = 51001; Mode mode = 51002; }"
                    generate assignment = do
                      definitions <- policy
                      consumer <- parse "consumer.proto" $
                        "edition = '2024'; import option 'policy.proto'; message Consumer { option "
                          <> assignment <> "; }"
                      generateModuleTextWithImports defaultGenerateOpts Map.empty "consumer.proto"
                        (Map.singleton "policy.proto" definitions) consumer
                    failedWith message = either (T.isInfixOf message) (const False)
                generate "(audit.enabled) = 1" `shouldSatisfy` failedWith "Boolean option"
                generate "(audit.enabled) = True" `shouldSatisfy` failedWith "Boolean option"
                generate "(audit.mode) = 1" `shouldSatisfy` failedWith "Enum option"
                generate "(audit.settings).enabled = 1" `shouldSatisfy` failedWith "Boolean option"
                generate "(audit.settings).mode = 1" `shouldSatisfy` failedWith "Enum option"
                generate "(audit.settings) = { enabled: 1 mode: 1 }"
                  `shouldSatisfy` either (const False) (T.isInfixOf "\\x08\\x01\\x10\\x01")
            , it "writes numeric field defaults in descriptor form" $ do
                schema <- either (fail . show) pure $
                  parseProtoFile "schema.proto" "syntax = 'proto2'; message Schema { optional float f = 1 [default = -0]; optional int32 n = 2 [default = 0x20]; }"
                let descriptor = astToFileDescriptor "schema.proto" schema
                    fields = Descriptor.dpField (V.head (Descriptor.fdpMessageType descriptor))
                V.map Descriptor.fdpFieldDefault fields `shouldBe` V.fromList ["-0", "32"]
            ]
      , describe "Cabal code generation" $ do
          it "resolves includes and repairs stale output without rewriting identical modules" $
            withSystemTempDirectory "wireform-setup" $ \dir -> do
              let source = dir </> "schema"
                  includes = dir </> "includes"
                  out = dir </> "gen"
                  file = source </> "settings.proto"
                  dependency = includes </> "state.proto"
                  generated = out </> "Proto" </> "Gen" </> "Settings.hs"
                  cfg = defaultProtoGenConfig
                    {pgcProtoDir = source, pgcIncludeDirs = [includes], pgcOutputDir = out}
                  epoch = read "2000-01-01 00:00:00 UTC" :: UTCTime
              createDirectory source
              createDirectory includes
              createDirectoryIfMissing True (out </> "Proto" </> "Gen")
              TIO.writeFile file "syntax = 'proto3'; package test; import 'state.proto'; enum Mode { OFF = 0; ON = 1; } message Settings { Mode mode = 1; .dep.State state = 2; }"
              TIO.writeFile dependency "syntax = 'proto3'; package dep; enum State { EMPTY = 0; READY = 1; }"
              TIO.writeFile generated "-- previously generated with an empty registry"
              generateProtoFile cfg file
              code <- TIO.readFile generated
              code `shouldSatisfy` T.isInfixOf "settingsMode :: !Mode"
              code `shouldSatisfy` T.isInfixOf "settingsState :: !GE_State.State"
              setModificationTime generated epoch
              generateProtos cfg
              getModificationTime generated >>= (`shouldBe` epoch)
              TIO.writeFile dependency "syntax = 'proto3'; package dep; message State {}"
              generateProtos cfg
              revised <- TIO.readFile generated
              revised `shouldSatisfy` T.isInfixOf "settingsState :: !(Maybe GE_State.State)"
          it "detects colliding module names before writing output" $
            withSystemTempDirectory "wireform-setup-collision" $ \dir -> do
              let source = dir </> "schema"
                  out = dir </> "gen"
                  cfg = defaultProtoGenConfig {pgcProtoDir = source, pgcOutputDir = out}
              createDirectory source
              createDirectory (source </> "first")
              createDirectory (source </> "second")
              TIO.writeFile (source </> "first" </> "types.proto")
                "syntax = 'proto3'; option csharp_namespace = 'Shared.Schema'; message First {}"
              TIO.writeFile (source </> "second" </> "types.proto")
                "syntax = 'proto3'; option csharp_namespace = 'Shared.Schema'; message Second {}"
              generateProtos cfg `shouldThrow` (\err -> "both generate" `T.isInfixOf` T.pack (show (err :: IOException)))
              doesDirectoryExist out >>= (`shouldBe` False)
          it "keeps option-only types hidden and allows types imported through a public path" $
            withSystemTempDirectory "wireform-setup-option-import" $ \dir -> do
              let out = dir </> "gen"
                  consumer = dir </> "consumer.proto"
                  cfg = defaultProtoGenConfig {pgcProtoDir = dir, pgcOutputDir = out}
                  writeConsumer imports = TIO.writeFile consumer $
                    "edition = '2024'; " <> imports <> " import option 'policy.proto'; "
                      <> "message Consumer { .policy.Hidden hidden = 1; }"
              TIO.writeFile (dir </> "policy.proto") "edition = '2024'; package policy; message Hidden {}"
              writeConsumer ""
              generateProtoFile cfg consumer `shouldThrow` (\err ->
                "only available through an option import" `T.isInfixOf` T.pack (show (err :: IOException)))
              doesDirectoryExist out >>= (`shouldBe` False)
              TIO.writeFile (dir </> "forward.proto") "edition = '2024'; import public 'policy.proto';"
              writeConsumer "import 'forward.proto';"
              generateProtoFile cfg consumer
              code <- TIO.readFile (out </> "Proto" </> "Gen" </> "Consumer.hs")
              code `shouldSatisfy` T.isInfixOf "consumerHidden :: !(Maybe GE_Policy.Hidden)"
      , describe
          "Annotations"
          $ sequence_
            [ it "extract custom annotation" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "option (my_annotation) = true;"
                        , "option (another_opt) = { key: \"value\" };"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> do
                    let anns = extractAnnotations (protoOptions pf)
                    length anns `shouldBe` 2
                    case lookupAnnotation "my_annotation" anns of
                      Just (CBool True) -> pure ()
                      other -> expectationFailure ("Expected CBool True, got: " <> show other)
            , it "lookup simple option" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "option java_package = \"com.example\";"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> do
                    case lookupSimpleOption "java_package" (protoOptions pf) of
                      Just (CString "com.example") -> pure ()
                      other -> expectationFailure ("Expected CString, got: " <> show other)
            , it "extension option lookup" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "option (custom.opt) = 42;"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> do
                    case lookupExtensionOption "custom.opt" (protoOptions pf) of
                      Just (CInt 42) -> pure ()
                      other -> expectationFailure ("Expected CInt 42, got: " <> show other)
            , it "hasOption check" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "option deprecated = true;"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> do
                    (hasOption "deprecated" (protoOptions pf)) `shouldBe` True
                    (not (hasOption "java_package" (protoOptions pf))) `shouldBe` True
            , it "typed option extraction" $ do
                optionAsInt (CInt 42) `shouldBe` Just 42
                optionAsFloat (CFloat 3.14) `shouldBe` Just 3.14
                optionAsBool (CBool True) `shouldBe` Just True
                optionAsString (CString "hello") `shouldBe` Just "hello"
                optionAsIdent (CIdent "FOO") `shouldBe` Just "FOO"
                optionAsAggregate (CAggregate [("k", CInt 1)]) `shouldBe` Just [("k", CInt 1)]
                optionAsInt (CString "nope") `shouldBe` Nothing
            ]
      ]


unlines' :: [Text] -> Text
unlines' = mconcat . fmap (<> "\n")
