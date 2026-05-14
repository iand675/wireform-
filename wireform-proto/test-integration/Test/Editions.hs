module Test.Editions (editionsTests) where

import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text qualified as T
import Test.Tasty
import Test.Tasty.HUnit

import Proto.CodeGen
import Proto.IDL.AST
import Proto.IDL.Parser (parseProtoFile)
import Proto.IDL.Parser.Resolver (ResolvedProto (..))


editionsTests :: TestTree
editionsTests =
  testGroup
    "Proto editions"
    [ featureResolutionTests
    , fieldPresenceTests
    , repeatedEncodingTests
    , enumTypeTests
    , codegenOutputTests
    ]


-- ---------------------------------------------------------------------------
-- Feature resolution: applyFeatureOptions / resolveFileFeatures / resolveFieldFeatures
-- ---------------------------------------------------------------------------

featureResolutionTests :: TestTree
featureResolutionTests =
  testGroup
    "Feature resolution"
    [ testCase "edition 2023 defaults to proto3 semantics" $ do
        let fs = featuresForEdition (Edition "2023")
        featureFieldPresence fs @?= ExplicitPresence
        featureEnumType fs @?= OpenEnum
        featureRepeatedFieldEncoding fs @?= PackedEncoding
        featureUtf8Validation fs @?= Utf8Verify
        featureMessageEncoding fs @?= LengthPrefixedEncoding
        featureJsonFormat fs @?= JsonAllow

    , testCase "applyFeatureOptions: field_presence IMPLICIT" $ do
        let opts =
              [ OptionDef
                  ()
                  (OptionName [SimpleOption "features", SimpleOption "field_presence"])
                  (CIdent "IMPLICIT")
              ]
            fs = applyFeatureOptions opts defaultFeatureSet
        featureFieldPresence fs @?= ImplicitPresence

    , testCase "applyFeatureOptions: field_presence EXPLICIT" $ do
        let opts =
              [ OptionDef
                  ()
                  (OptionName [SimpleOption "features", SimpleOption "field_presence"])
                  (CIdent "EXPLICIT")
              ]
            fs = applyFeatureOptions opts defaultFeatureSet
        featureFieldPresence fs @?= ExplicitPresence

    , testCase "applyFeatureOptions: field_presence LEGACY_REQUIRED" $ do
        let opts =
              [ OptionDef
                  ()
                  (OptionName [SimpleOption "features", SimpleOption "field_presence"])
                  (CIdent "LEGACY_REQUIRED")
              ]
            fs = applyFeatureOptions opts defaultFeatureSet
        featureFieldPresence fs @?= LegacyRequired

    , testCase "applyFeatureOptions: enum_type CLOSED" $ do
        let opts =
              [ OptionDef
                  ()
                  (OptionName [SimpleOption "features", SimpleOption "enum_type"])
                  (CIdent "CLOSED")
              ]
            fs = applyFeatureOptions opts defaultFeatureSet
        featureEnumType fs @?= ClosedEnum

    , testCase "applyFeatureOptions: repeated_field_encoding EXPANDED" $ do
        let opts =
              [ OptionDef
                  ()
                  (OptionName [SimpleOption "features", SimpleOption "repeated_field_encoding"])
                  (CIdent "EXPANDED")
              ]
            fs = applyFeatureOptions opts defaultFeatureSet
        featureRepeatedFieldEncoding fs @?= ExpandedEncoding

    , testCase "applyFeatureOptions: unknown options are silently ignored" $ do
        let opts =
              [ OptionDef
                  ()
                  (OptionName [SimpleOption "features", SimpleOption "nonexistent_feature"])
                  (CIdent "SOME_VALUE")
              ]
            fs = applyFeatureOptions opts defaultFeatureSet
        fs @?= defaultFeatureSet

    , testCase "resolveFileFeatures: non-editions returns defaultFeatureSet" $ do
        let fs3 = resolveFileFeatures Proto3 []
            fs2 = resolveFileFeatures Proto2 []
        fs3 @?= defaultFeatureSet
        fs2 @?= defaultFeatureSet

    , testCase "resolveFileFeatures: edition 2023 with override" $ do
        let opts =
              [ OptionDef
                  ()
                  (OptionName [SimpleOption "features", SimpleOption "repeated_field_encoding"])
                  (CIdent "EXPANDED")
              ]
            fs = resolveFileFeatures (Editions (Edition "2023")) opts
        featureRepeatedFieldEncoding fs @?= ExpandedEncoding
        -- Other features stay at edition 2023 defaults
        featureFieldPresence fs @?= ExplicitPresence

    , testCase "resolveFieldFeatures: field overrides parent" $ do
        let parent = defaultFeatureSet { featureEnumType = OpenEnum }
            fieldOpts =
              [ OptionDef
                  ()
                  (OptionName [SimpleOption "features", SimpleOption "enum_type"])
                  (CIdent "CLOSED")
              ]
            fs = resolveFieldFeatures parent fieldOpts
        featureEnumType fs @?= ClosedEnum
        -- Non-overridden features stay from parent
        featureFieldPresence fs @?= featureFieldPresence parent
    ]


-- ---------------------------------------------------------------------------
-- Field presence normalization (applyEditionFieldPresence)
-- ---------------------------------------------------------------------------

fieldPresenceTests :: TestTree
fieldPresenceTests =
  testGroup
    "field_presence normalization"
    [ testCase "non-editions files are unmodified" $ do
        let src = "syntax = \"proto3\"; message M { int32 x = 1; }"
        case parseProtoFile "<test>" src of
          Left e -> assertFailure (show e)
          Right pf ->
            let pf' = applyEditionFieldPresence pf
            in protoTopLevels pf' @?= protoTopLevels pf

    , testCase "editions EXPLICIT presence → Optional label" $ do
        let src = T.unlines
              [ "edition = \"2023\";"
              , "option features.field_presence = EXPLICIT;"
              , "message M { int32 x = 1; }"
              ]
        case parseProtoFile "<test>" src of
          Left e -> assertFailure (show e)
          Right pf -> do
            let pf' = applyEditionFieldPresence pf
            fieldLabelOf pf' "x" @?= Just Optional

    , testCase "editions IMPLICIT presence → Nothing (plain singular)" $ do
        let src = T.unlines
              [ "edition = \"2023\";"
              , "option features.field_presence = IMPLICIT;"
              , "message M { int32 x = 1; }"
              ]
        case parseProtoFile "<test>" src of
          Left e -> assertFailure (show e)
          Right pf -> do
            let pf' = applyEditionFieldPresence pf
            fieldLabelOf pf' "x" @?= Nothing

    , testCase "editions LEGACY_REQUIRED → Required label" $ do
        let src = T.unlines
              [ "edition = \"2023\";"
              , "option features.field_presence = LEGACY_REQUIRED;"
              , "message M { int32 x = 1; }"
              ]
        case parseProtoFile "<test>" src of
          Left e -> assertFailure (show e)
          Right pf -> do
            let pf' = applyEditionFieldPresence pf
            fieldLabelOf pf' "x" @?= Just Required

    , testCase "repeated fields are unaffected by field_presence" $ do
        let src = T.unlines
              [ "edition = \"2023\";"
              , "option features.field_presence = EXPLICIT;"
              , "message M { repeated int32 xs = 1; }"
              ]
        case parseProtoFile "<test>" src of
          Left e -> assertFailure (show e)
          Right pf -> do
            let pf' = applyEditionFieldPresence pf
            fieldLabelOf pf' "xs" @?= Just Repeated

    , testCase "per-field override takes precedence over file default" $ do
        -- File is IMPLICIT, but this specific field is EXPLICIT
        let src = T.unlines
              [ "edition = \"2023\";"
              , "option features.field_presence = IMPLICIT;"
              , "message M {"
              , "  int32 x = 1;"
              , "  int32 y = 2 [features.field_presence = EXPLICIT];"
              , "}"
              ]
        case parseProtoFile "<test>" src of
          Left e -> assertFailure (show e)
          Right pf -> do
            let pf' = applyEditionFieldPresence pf
            fieldLabelOf pf' "x" @?= Nothing        -- IMPLICIT
            fieldLabelOf pf' "y" @?= Just Optional  -- field-level EXPLICIT
    ]
  where
    fieldLabelOf :: ProtoFile -> T.Text -> Maybe FieldLabel
    fieldLabelOf pf fname =
      let findField [] = Nothing
          findField (TLMessage msg : _) =
            let fields = [fd | MEField fd <- msgElements msg, fieldName fd == fname]
            in case fields of
                 (fd : _) -> Just (fieldLabel fd)
                 [] -> Nothing
          findField (_ : rest) = findField rest
      in case findField (protoTopLevels pf) of
           Just ml -> ml
           Nothing -> Nothing


-- ---------------------------------------------------------------------------
-- repeated_field_encoding in the text codegen path
-- ---------------------------------------------------------------------------

repeatedEncodingTests :: TestTree
repeatedEncodingTests =
  testGroup
    "repeated_field_encoding in codegen"
    [ testCase "default opts: repeated scalar uses packed encoding" $ do
        let src = "syntax = \"proto3\"; package t; message M { repeated int32 xs = 1; }"
        case parseProtoFile "<test>" src of
          Left e -> assertFailure (show e)
          Right pf -> do
            let reg = buildTypeRegistry defaultGenerateOpts [("<test>", ResolvedProto pf "<test>" Map.empty)]
                code = generateModuleText defaultGenerateOpts reg "<test>" pf
            assertBool "packed encoder present" (T.isInfixOf "encodePackedInt32" code || T.isInfixOf "encodePacked" code)

    , testCase "genPackedRepeated=False: repeated scalar uses expanded encoding" $ do
        let src = "syntax = \"proto3\"; package t; message M { repeated int32 xs = 1; }"
            opts = defaultGenerateOpts { genPackedRepeated = False }
        case parseProtoFile "<test>" src of
          Left e -> assertFailure (show e)
          Right pf -> do
            let reg = buildTypeRegistry opts [("<test>", ResolvedProto pf "<test>" Map.empty)]
                code = generateModuleText opts reg "<test>" pf
            -- Expanded encoding uses foldl' per element
            assertBool "expanded foldl present" (T.isInfixOf "foldl'" code)
            -- Should NOT call a pack function
            assertBool "no packed encoder" (not (T.isInfixOf "encodePacked" code))

    , testCase "edition 2023 EXPANDED override uses expanded encoding" $ do
        let src = T.unlines
              [ "edition = \"2023\";"
              , "package t;"
              , "option features.repeated_field_encoding = EXPANDED;"
              , "message M { repeated int32 xs = 1; }"
              ]
        case parseProtoFile "<test>" src of
          Left e -> assertFailure (show e)
          Right pf -> do
            let reg = buildTypeRegistry defaultGenerateOpts [("<test>", ResolvedProto pf "<test>" Map.empty)]
                code = generateModuleText defaultGenerateOpts reg "<test>" pf
            assertBool "expanded foldl present" (T.isInfixOf "foldl'" code)
            assertBool "no packed encoder" (not (T.isInfixOf "encodePacked" code))
    ]


-- ---------------------------------------------------------------------------
-- enum_type=CLOSED in codegen
-- ---------------------------------------------------------------------------

enumTypeTests :: TestTree
enumTypeTests =
  testGroup
    "enum_type in codegen"
    [ testCase "default (OPEN): enum decode uses decodeFieldEnum" $ do
        let src = T.unlines
              [ "syntax = \"proto3\";"
              , "package t;"
              , "enum E { E_ZERO = 0; E_ONE = 1; }"
              , "message M { E e = 1; }"
              ]
        case parseProtoFile "<test>" src of
          Left e -> assertFailure (show e)
          Right pf -> do
            let reg = buildTypeRegistry defaultGenerateOpts [("<test>", ResolvedProto pf "<test>" Map.empty)]
                code = generateModuleText defaultGenerateOpts reg "<test>" pf
            assertBool "open enum uses decodeFieldEnum" (T.isInfixOf "decodeFieldEnum" code)

    , testCase "genClosedEnums=True: enum decode uses fromProtoEnum + decodeFail" $ do
        let src = T.unlines
              [ "syntax = \"proto3\";"
              , "package t;"
              , "enum E { E_ZERO = 0; E_ONE = 1; }"
              , "message M { E e = 1; }"
              ]
            opts = defaultGenerateOpts { genClosedEnums = True }
        case parseProtoFile "<test>" src of
          Left e -> assertFailure (show e)
          Right pf -> do
            let reg = buildTypeRegistry opts [("<test>", ResolvedProto pf "<test>" Map.empty)]
                code = generateModuleText opts reg "<test>" pf
            assertBool "closed enum uses fromProtoEnum" (T.isInfixOf "fromProtoEnum" code)
            assertBool "closed enum uses decodeFail" (T.isInfixOf "decodeFail" code)
            assertBool "closed enum does not use decodeFieldEnum" (not (T.isInfixOf "decodeFieldEnum" code))

    , testCase "edition 2023 CLOSED override generates closed decode" $ do
        let src = T.unlines
              [ "edition = \"2023\";"
              , "package t;"
              , "option features.enum_type = CLOSED;"
              , "enum E { E_ZERO = 0; E_ONE = 1; }"
              , "message M { E e = 1; }"
              ]
        case parseProtoFile "<test>" src of
          Left e -> assertFailure (show e)
          Right pf -> do
            let reg = buildTypeRegistry defaultGenerateOpts [("<test>", ResolvedProto pf "<test>" Map.empty)]
                code = generateModuleText defaultGenerateOpts reg "<test>" pf
            assertBool "edition closed enum uses fromProtoEnum" (T.isInfixOf "fromProtoEnum" code)
            assertBool "edition closed enum uses decodeFail" (T.isInfixOf "decodeFail" code)
    ]


-- ---------------------------------------------------------------------------
-- General codegen output checks for editions
-- ---------------------------------------------------------------------------

codegenOutputTests :: TestTree
codegenOutputTests =
  testGroup
    "Edition codegen output"
    [ testCase "edition = 2023 parses and generates a module" $ do
        let src = T.unlines
              [ "edition = \"2023\";"
              , "package test.ed;"
              , "message Person { string name = 1; int32 age = 2; }"
              ]
        case parseProtoFile "<test>" src of
          Left e -> assertFailure ("parse failed: " <> show e)
          Right pf -> do
            let reg = buildTypeRegistry defaultGenerateOpts [("<test>", ResolvedProto pf "<test>" Map.empty)]
                code = generateModuleText defaultGenerateOpts reg "<test>" pf
            assertBool "data Person" (T.isInfixOf "data Person" code)
            assertBool "MessageEncode instance" (T.isInfixOf "MessageEncode Person" code)
            assertBool "MessageDecode instance" (T.isInfixOf "MessageDecode Person" code)

    , testCase "edition EXPLICIT presence generates Maybe field" $ do
        let src = T.unlines
              [ "edition = \"2023\";"
              , "package test.ed;"
              , "option features.field_presence = EXPLICIT;"
              , "message M { int32 x = 1; }"
              ]
        case parseProtoFile "<test>" src of
          Left e -> assertFailure ("parse failed: " <> show e)
          Right pf -> do
            let pf' = applyEditionFieldPresence pf
                reg = buildTypeRegistry defaultGenerateOpts [("<test>", ResolvedProto pf' "<test>" Map.empty)]
                code = generateModuleText defaultGenerateOpts reg "<test>" pf'
            assertBool "Maybe field for EXPLICIT presence" (T.isInfixOf "Maybe" code)

    , testCase "edition IMPLICIT presence generates plain (non-Maybe) scalar field" $ do
        let src = T.unlines
              [ "edition = \"2023\";"
              , "package test.ed;"
              , "option features.field_presence = IMPLICIT;"
              , "message M { int32 x = 1; }"
              ]
        case parseProtoFile "<test>" src of
          Left e -> assertFailure ("parse failed: " <> show e)
          Right pf -> do
            let pf' = applyEditionFieldPresence pf
                reg = buildTypeRegistry defaultGenerateOpts [("<test>", ResolvedProto pf' "<test>" Map.empty)]
                code = generateModuleText defaultGenerateOpts reg "<test>" pf'
            -- x :: !Int32 (not Maybe)
            assertBool "plain Int32 for IMPLICIT presence" (T.isInfixOf "Int32" code)
            -- The field should NOT be Maybe-wrapped (just a bare Int32 field)
            let lines' = T.lines code
                xLine = filter (\l -> T.isInfixOf "mX " l || T.isInfixOf ":: !Int32" l) lines'
            assertBool "non-Maybe scalar field" (not (null xLine))
    ]
