{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module Main (main) where

import Control.Exception (ErrorCall, evaluate)
import Control.Monad (forM_)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Either (isLeft)
import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Reflection qualified as Reflection
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Generated.Edition2023 qualified as G23
import Generated.Edition2024 qualified as G24
import Generated.EditionDelimited qualified as GD
import Generated.ExtensionUtf8 qualified as GU
import Generated.GoogleImport qualified as GGoogle
import Generated.Groups qualified as GG
import Generated.Integermaps'Path'696e74656765724d617073 qualified as GM
import Generated.ImportScope qualified as GI
import Generated.JsonCollections qualified as GC
import Generated.JsonTemporal qualified as GT
import Generated.MyFile qualified as GPath
import Generated.OptionImport qualified as GOption
import Generated.PackingLegacy qualified as GL
import Generated.Rpc qualified as RPC
import Proto.CodeGen
import Proto.Decode
import Proto.Decode.Collect qualified as Collect
import Proto.Decode.Stream qualified as Stream
import Proto.Dynamic qualified as Dynamic
import Proto.Encode
import Proto.Extension qualified as Ext
import Proto.Google.Protobuf.Any qualified as Any
import Proto.Google.Protobuf.Compiler.Plugin qualified as Plugin
import Proto.Google.Protobuf.Descriptor qualified as Descriptor
import Proto.Google.Protobuf.Empty (Empty)
import Proto.Google.Protobuf.FieldMask qualified as Mask
import Proto.Google.Protobuf.Wrappers qualified as Wrappers
import Proto.Google.Protobuf.Struct qualified as Struct
import Proto.IDL.Parser
import Proto.IDL.AST qualified as AST
import Proto.IDL.Descriptor (astToFileDescriptor, descriptorDefaultConstant, fileDescriptorToAST)
import Proto.IDL.Print (printProtoFile)
import Proto.IDL.Parser.Resolver (ResolvedProto (..), importClosure)
import Proto.Internal.JSON qualified as JSON
import Proto.Internal.JSON.Extension (emptyExtensionRegistry)
import Proto.Internal.Wire (WireType (..))
import Proto.Internal.Wire.Decode qualified as Wire
import Proto.Registry qualified as Registry
import Proto.Schema qualified as Schema
import Proto.TextFormat qualified as TextFormat
import THFixtures qualified as TH
import THGoogleImports qualified as THGoogle
import THImportScope qualified as THI
import THImportScopeExplicit qualified as THIE
import THOptionImports qualified as THOption
import Test.Syd


main :: IO ()
main = do
  Registry.registerGlobalAnyCodecs $
    Registry.registerMessage (Proxy @GC.ChoicePayload)
      (Registry.registerMessage (Proxy @GC.JsonEnumMode) mempty)
  sydTest spec


spec :: Spec
spec = do
  describe "Protobuf TextFormat rendering" $ do
    it "honors the unknown-field option in both generated message types" $ do
      let fields =
            [ UnknownVarint 100 42
            , UnknownLenDelim 101 (BS.pack [8, 7])
            , UnknownGroup 102 (BS.pack [8, 9])
            , UnknownLenDelim 103 (BS.pack [0, 255, 34, 92, 10])
            , UnknownFixed32 104 10
            , UnknownVarint 100 43
            ]
          known = "label: \"abc\"\n"
          printed =
            known <> "100: 42\n101 {\n  1: 7\n}\n102 {\n  1: 9\n}\n"
              <> "103: \"\\000\\377\\\"\\\\\\n\"\n104: 0x0000000a\n100: 43\n"
          textual = Ext.setMessageUnknownFields fields GC.defaultJsonEnumMode {GC.jsonEnumModeLabel = "abc"}
          th = Ext.setMessageUnknownFields fields TH.defaultJsonEnumMode {TH.jsonEnumModeLabel = "abc"}
      TextFormat.typedToTextPrettyWithUnknownFields False (Proxy @GC.JsonEnumMode) textual `shouldBe` known
      TextFormat.typedToTextPrettyWithUnknownFields False (Proxy @TH.JsonEnumMode) th `shouldBe` known
      TextFormat.typedToTextPrettyWithUnknownFields True (Proxy @GC.JsonEnumMode) textual `shouldBe` printed
      TextFormat.typedToTextPrettyWithUnknownFields True (Proxy @TH.JsonEnumMode) th `shouldBe` printed
      encodeMessage textual `shouldBe` encodeMessage th
  describe "Protobuf TextFormat parsing" $ do
    it "preserves named fields, comments, nested messages and lists" $ do
      let input =
            "# heading\nouter: < name: 'ab' \"\\x63\"; score: 1.e2 >\n"
              <> "[ example . ext ]: [1, 0x2, 03]\nitems [{x: true}, <x: false>]"
      TextFormat.parseTextFields input
        `shouldBe` Right
          [ TextFormat.TextField "outer" (TextFormat.TVMessage
              [TextFormat.TextField "name" (TextFormat.TVString "abc"), TextFormat.TextField "score" (TextFormat.TVNumber 100)])
          , TextFormat.TextField "[example.ext]" (TextFormat.TVList (map TextFormat.TVInteger [1, 2, 3]))
          , TextFormat.TextField "items" (TextFormat.TVList
              [ TextFormat.TVMessage [TextFormat.TextField "x" (TextFormat.TVBool True)]
              , TextFormat.TVMessage [TextFormat.TextField "x" (TextFormat.TVBool False)]
              ])
          ]
    it "preserves byte escapes, Unicode escapes and repeated numeric fields" $ do
      let input = "1: 1; 2: '\\xff\\000'; 1: [2, 3]; 3: '\\u00e9\\U0001f600'"
          fields = Map.fromList
            [ (1, Dynamic.DynRepeated (map Dynamic.DynVarint [1, 2, 3]))
            , (2, Dynamic.DynBytes (BS.pack [255, 0]))
            , (3, Dynamic.DynString "é😀")
            ]
      TextFormat.textToDynamic input `shouldBe` Right (Dynamic.DynamicMessage fields [])
    it "rejects invalid escapes, malformed lists and trailing delimiters" $
      forM_ ["1: '\\q'", "1: '\\777'", "1: '\\uD800'", "1: [1, {}]", "1: [1,]", "1: 1 } junk", "1: 08", "1: ."] $ \input ->
        isLeft (TextFormat.parseTextFields input) `shouldBe` True
    it "allows whitespace and comments between a minus sign and its literal" $
      TextFormat.parseTextFields "a: - # comment\n 0x2 b: - .5 c: - infinity"
        `shouldBe` Right
          [ TextFormat.TextField "a" (TextFormat.TVInteger (-2))
          , TextFormat.TextField "b" (TextFormat.TVNumber (-0.5))
          , TextFormat.TextField "c" (TextFormat.TVIdent "-infinity")
          ]
    it "bounds message nesting and rejects a nested list before descending" $ do
      let nested count = T.replicate count "child { " <> T.replicate count "} "
      isLeft (TextFormat.parseTextFields (nested 100)) `shouldBe` False
      isLeft (TextFormat.parseTextFields (nested 101)) `shouldBe` True
      isLeft (TextFormat.parseTextFields ("items: " <> T.replicate 100000 "[")) `shouldBe` True
    it "rounds subnormal and long decimal floats without exponent wraparound" $ do
      let check input expected =
            TextFormat.parseTextFields ("value: " <> input)
              `shouldBe` Right [TextFormat.TextField "value" (TextFormat.TVNumber expected)]
          largeExponent = "18446744073709551616"
          midpoint = "1.00000000000000011102230246251565404236316680908203125"
      check ("1e" <> largeExponent) (1 / 0)
      check ("0e" <> largeExponent) 0
      check ("1e-" <> largeExponent) 0
      check "5e-324" (encodeFloat 1 (-1074))
      check midpoint 1
      check (midpoint <> T.replicate 100000 "0" <> "1") (encodeFloat 4503599627370497 (-52))
      case TextFormat.parseTextFields ("value: -1e-" <> largeExponent) of
        Right [TextFormat.TextField _ (TextFormat.TVNumber value)] -> isNegativeZero value `shouldBe` True
        actual -> expectationFailure ("Expected a negative zero literal, got " <> show actual)
    it "rejects names and overflowing numbers in schema-free dynamic messages" $
      forM_ ["named: 1", "0: 1", "536870912: 1", "1: 18446744073709551616"] $ \input ->
        isLeft (TextFormat.textToDynamic input) `shouldBe` True
  describe "Schema-aware Protobuf TextFormat" $ do
    let schema = do
          files <- traverse readSchema
            [ "test-regression/proto/json_collections.proto"
            , "test-regression/proto/packing_legacy.proto"
            , "data/proto/google/protobuf/any.proto"
            ]
          either fail pure (TextFormat.buildTextFormatSchema files)
        readSchema file = TIO.readFile file >>= either (fail . show) pure . parseProtoFile file
    it "decodes nested fields, repeated enum aliases and map entries through both generators" $ do
      s <- schema
      let input = "label: 'hello' child <label: 'nested'> colors: [CHOICE_COLOR_CRIMSON, 99] "
            <> "by_name { key: 'color' value: CHOICE_COLOR_RETIRED } selected_color: CHOICE_COLOR_RED"
          expected = Aeson.object
            [ "label" Aeson..= Aeson.String "hello"
            , "child" Aeson..= Aeson.object ["label" Aeson..= Aeson.String "nested"]
            , "colors" Aeson..= [Aeson.String "CHOICE_COLOR_RED", Aeson.Number 99]
            , "byName" Aeson..= Aeson.object ["color" Aeson..= Aeson.String "CHOICE_COLOR_RETIRED"]
            , "selectedColor" Aeson..= Aeson.String "CHOICE_COLOR_RED"
            ]
      textual <- either fail pure (TextFormat.textToTyped @GC.JsonEnumMode s input)
      th <- either fail pure (TextFormat.textToTyped @TH.JsonEnumMode s input)
      Aeson.toJSON textual `shouldBe` expected
      Aeson.toJSON th `shouldBe` expected
      encodeMessage textual `shouldBe` encodeMessage th
      let rendered = T.unlines
            [ "label: \"hello\""
            , "colors: CHOICE_COLOR_RED"
            , "colors: 99"
            , "by_name {"
            , "  key: \"color\""
            , "  value: CHOICE_COLOR_RETIRED"
            , "}"
            , "child {"
            , "  label: \"nested\""
            , "}"
            , "selected_color: CHOICE_COLOR_RED"
            ]
      TextFormat.typedToTextWithSchema s False textual `shouldBe` Right rendered
      TextFormat.typedToTextWithSchema s False th `shouldBe` Right rendered
      fmap encodeMessage (TextFormat.textToTyped @GC.JsonEnumMode s rendered) `shouldBe` Right (encodeMessage textual)
      fmap encodeMessage (TextFormat.textToTyped @TH.JsonEnumMode s rendered) `shouldBe` Right (encodeMessage th)
    it "encodes expanded Any and typed extensions through both generators" $ do
      s <- schema
      let anyInput = "payload { [example.org/ChoicePayload] { count: 7 } }"
          anyWire = encodeMessage (GC.defaultJsonAny {GC.jsonAnyPayload = Just (Any.Any "example.org/ChoicePayload" (BS.pack [8, 7]) [])})
          extInput = "[singular_extra]: -7"
          extWire = BS.pack [176, 6, 249, 255, 255, 255, 255, 255, 255, 255, 255, 1]
      fmap encodeMessage (TextFormat.textToTyped @GC.JsonAny s anyInput) `shouldBe` Right anyWire
      fmap encodeMessage (TextFormat.textToTyped @TH.JsonAny s anyInput) `shouldBe` Right anyWire
      fmap encodeMessage (TextFormat.textToTyped @GL.PackingLegacy s extInput) `shouldBe` Right extWire
      fmap encodeMessage (TextFormat.textToTyped @TH.PackingLegacy s extInput) `shouldBe` Right extWire
      let withAnyText = T.unlines
            ["payload {", "  [example.org/ChoicePayload] {", "    count: 7", "  }", "}"]
      (TextFormat.textToTyped @GC.JsonAny s anyInput >>= TextFormat.typedToTextWithSchema s False) `shouldBe` Right withAnyText
      (TextFormat.textToTyped @TH.JsonAny s anyInput >>= TextFormat.typedToTextWithSchema s False) `shouldBe` Right withAnyText
      (TextFormat.textToTyped @GL.PackingLegacy s extInput >>= TextFormat.typedToTextWithSchema s False) `shouldBe` Right (extInput <> "\n")
      (TextFormat.textToTyped @TH.PackingLegacy s extInput >>= TextFormat.typedToTextWithSchema s False) `shouldBe` Right (extInput <> "\n")
    it "prints packed and expanded values, bytes and signed numeric values" $ do
      s <- schema
      let repeatedInput = "expanded: [-1, 0, 3] packed_override: [7, -8]"
          repeatedText = T.unlines
            ["expanded: -1", "expanded: 0", "expanded: 3", "packed_override: 7", "packed_override: -8"]
          bytesInput = "blobs: ['\\xff\\000', 'text']"
          bytesText = "blobs: \"\\377\\000\"\nblobs: \"text\"\n"
          numericInput = "signed_value: -1 small: -5 optional_small: 0x20 fraction: -0 ratio: -inf"
          numericText = T.unlines
            ["signed_value: -1", "small: -5", "optional_small: 32", "fraction: -0", "ratio: -inf"]
      (TextFormat.textToTyped @GL.PackingLegacy s repeatedInput >>= TextFormat.typedToTextWithSchema s False) `shouldBe` Right repeatedText
      (TextFormat.textToTyped @TH.PackingLegacy s repeatedInput >>= TextFormat.typedToTextWithSchema s False) `shouldBe` Right repeatedText
      (TextFormat.textToTyped @GC.JsonCollections s bytesInput >>= TextFormat.typedToTextWithSchema s False) `shouldBe` Right bytesText
      (TextFormat.textToTyped @TH.JsonCollections s bytesInput >>= TextFormat.typedToTextWithSchema s False) `shouldBe` Right bytesText
      (TextFormat.textToTyped @GC.JsonScalars s numericInput >>= TextFormat.typedToTextWithSchema s False) `shouldBe` Right numericText
      (TextFormat.textToTyped @TH.JsonScalars s numericInput >>= TextFormat.typedToTextWithSchema s False) `shouldBe` Right numericText
    it "omits nested unknowns and mistyped known tags unless debug printing is requested" $ do
      s <- schema
      let childUnknown = [UnknownFixed32 101 15]
          unknown = [UnknownVarint 1 9, UnknownGroup 105 (BS.pack [8, 4])]
          textualChild = Ext.setMessageUnknownFields childUnknown GC.defaultJsonEnumMode {GC.jsonEnumModeLabel = "nested"}
          thChild = Ext.setMessageUnknownFields childUnknown TH.defaultJsonEnumMode {TH.jsonEnumModeLabel = "nested"}
          textual = Ext.setMessageUnknownFields unknown GC.defaultJsonEnumMode {GC.jsonEnumModeChild = Just textualChild}
          th = Ext.setMessageUnknownFields unknown TH.defaultJsonEnumMode {TH.jsonEnumModeChild = Just thChild}
          plain = T.unlines ["child {", "  label: \"nested\"", "}"]
          debug = T.unlines ["child {", "  label: \"nested\"", "  101: 0x0000000f", "}", "1: 9", "105 {", "  1: 4", "}"]
      TextFormat.typedToTextWithSchema s False textual `shouldBe` Right plain
      TextFormat.typedToTextWithSchema s False th `shouldBe` Right plain
      TextFormat.typedToTextWithSchema s True textual `shouldBe` Right debug
      TextFormat.typedToTextWithSchema s True th `shouldBe` Right debug
    it "checks singular and oneof cardinality, integer spelling and required fields" $ do
      s <- schema
      forM_ ["label: ['x']", "label: 'a' label: 'b'", "label: '\\xff'", "primary_color: MISSING", "colors []",
              "by_name { key: 'a' key: 'b' }", "selected_color: 0 selected_child {}"] $ \input -> do
        TextFormat.textToTyped @GC.JsonEnumMode s input `shouldSatisfy` isLeft
        TextFormat.textToTyped @TH.JsonEnumMode s input `shouldSatisfy` isLeft
      forM_ ["optional_small: -0", "ratio: 0x10", "small: 2147483648"] $ \input -> do
        TextFormat.textToTyped @GC.JsonScalars s input `shouldSatisfy` isLeft
        TextFormat.textToTyped @TH.JsonScalars s input `shouldSatisfy` isLeft
      TextFormat.textToTyped @GL.RequiredCases s "count: 1" `shouldSatisfy` isLeft
      TextFormat.textToTyped @TH.RequiredCases s "count: 1" `shouldSatisfy` isLeft
    it "rounds a float field directly to binary32 and preserves signed zero" $ do
      s <- schema
      let check input expected = do
            fmap encodeMessage (TextFormat.textToTyped @GC.JsonChoices s input) `shouldBe` Right expected
            fmap encodeMessage (TextFormat.textToTyped @TH.JsonChoices s input) `shouldBe` Right expected
      check "fraction: -0" (BS.pack [29, 0, 0, 0, 128])
      check "fraction: 1.000000059604644775390625000000000000000000000000001" (BS.pack [29, 1, 0, 128, 63])
    it "preserves implicit negative zero when encoding, sizing and merging both generated types" $ do
      s <- schema
      let input = "fraction: -0 ratio: -1e-400"
          wire = BS.pack [45, 0, 0, 0, 128, 49, 0, 0, 0, 0, 0, 0, 0, 128]
      textual <- either fail pure (TextFormat.textToTyped @GC.JsonScalars s input)
      th <- either fail pure (TextFormat.textToTyped @TH.JsonScalars s input)
      encodeMessage textual `shouldBe` wire
      encodeMessage th `shouldBe` wire
      encodeMessageSized textual `shouldBe` wire
      encodeMessageSized th `shouldBe` wire
      messageSize textual `shouldBe` BS.length wire
      messageSize th `shouldBe` BS.length wire
      encodeMessage (GC.defaultJsonScalars {GC.jsonScalarsFraction = 1, GC.jsonScalarsRatio = 2} <> textual) `shouldBe` wire
      encodeMessage (TH.defaultJsonScalars {TH.jsonScalarsFraction = 1, TH.jsonScalarsRatio = 2} <> th) `shouldBe` wire
      fmap encodeMessage (TextFormat.textToTyped @GC.JsonScalars s "fraction: 0 ratio: 0") `shouldBe` Right BS.empty
      fmap encodeMessage (TextFormat.textToTyped @TH.JsonScalars s "fraction: 0 ratio: 0") `shouldBe` Right BS.empty
  describe "Generated record selectors" $ do
    it "keeps wire tags and JSON names distinct when record labels collide" $ do
      let wire = BS.pack [8, 1, 16, 2, 26, 5, 10, 1, 107, 16, 3, 32, 4, 40, 5, 48, 6, 58, 1, 255]
          input = Aeson.object
            [ "chosen" Aeson..= (1 :: Int)
            , "alternate" Aeson..= (2 :: Int)
            , "fooBar" Aeson..= Map.singleton ("k" :: T.Text) (3 :: Int)
            , "otherFoo" Aeson..= (4 :: Int)
            , "picked" Aeson..= (5 :: Int)
            , "selection" Aeson..= (6 :: Int)
            , "unknownFields" Aeson..= ("/w==" :: T.Text)
            ]
          unknown = UnknownVarint 100 8
          withUnknown = wire <> BS.pack [160, 6, 8]
      textual <- either fail pure (Aeson.parseEither (Aeson.parseJSON @GC.SelectorCollision) input)
      th <- either fail pure (Aeson.parseEither (Aeson.parseJSON @TH.SelectorCollision) input)
      Aeson.toJSON textual `shouldBe` input
      Aeson.toJSON th `shouldBe` input
      encodeMessageSized textual `shouldBe` wire
      encodeMessageSized th `shouldBe` wire
      GC.selectorCollisionChosen textual `shouldBe` 1
      GC.selectorCollisionChosen''_chosen textual `shouldBe` 2
      TH.selectorCollisionChosen th `shouldBe` 1
      TH.selectorCollisionChosen''_chosen th `shouldBe` 2
      Schema.getField @TH.SelectorCollision @"fooBar''foo_BAR" th `shouldBe` (4 :: Int32)
      decodeMessage @GC.SelectorCollision withUnknown `shouldBe`
        Right (Ext.setMessageUnknownFields [unknown] textual)
      decodeMessage @TH.SelectorCollision withUnknown `shouldBe`
        Right (Ext.setMessageUnknownFields [unknown] th)
    it "reserves prefixed field selectors and unknown slots across nested records" $ do
      let input = "syntax = \"proto3\"; message Outer { message Inner { int32 value = 1; } } message OuterInner { int32 value = 1; }"
      file <- either (fail . show) pure (parseProtoFile "nested_selectors.proto" input)
      let names = recordBindingNames PrefixedFields id (AST.protoTopLevels file)
      Map.lookup (["Outer", "Inner"], "value") (recordSelectorNames names) `shouldBe` Just "outerInnerValue"
      Map.lookup (["OuterInner"], "value") (recordSelectorNames names) `shouldBe` Just "outerInnerValue''value"
      Map.lookup ["OuterInner"] (recordUnknownNames names) `shouldBe` Just "outerInnerUnknownFields''OuterInner"
    it "reads JSON defaults and unknown slots through record dot for bare labels" $ do
      let source = "syntax = \"proto3\"; message Bare { int32 chosen = 1; int32 _chosen = 2 [json_name = 'alternate']; }"
          fileName = "bare.proto"
          opts = defaultGenerateOpts {genFieldNaming = UnprefixedFields}
      ast <- either (fail . show) pure (parseProtoFile fileName source)
      let registry = buildTypeRegistry opts [(fileName, ResolvedProto ast fileName Map.empty)]
          code = generateModuleText opts registry fileName ast
      code `shouldSatisfy` T.isInfixOf "defaultBare.chosen''_chosen"
      code `shouldSatisfy` T.isInfixOf "messageUnknownFields msg = msg.bareUnknownFields"
      code `shouldSatisfy` T.isInfixOf "m { chosen''_chosen = v } :: Bare"
  describe "Generated oneof names" $ do
    it "keeps colliding variants and carriers distinct from each other and nested types" $ do
      let cases =
            [ ("name", 7 :: Int, BS.pack [8, 7, 26, 1, 120])
            , ("alternate", 9, BS.pack [16, 9, 26, 1, 120])
            ]
      forM_ cases $ \(key, value, wire) -> do
        let input = Aeson.object [key Aeson..= value, "note" Aeson..= ("x" :: T.Text)]
        textual <- either fail pure (Aeson.parseEither (Aeson.parseJSON @GC.OneofCollision) input)
        th <- either fail pure (Aeson.parseEither (Aeson.parseJSON @TH.OneofCollision) input)
        Aeson.toJSON textual `shouldBe` input
        Aeson.toJSON th `shouldBe` input
        encodeMessage textual `shouldBe` wire
        encodeMessage th `shouldBe` wire
        encodeMessageSized textual `shouldBe` wire
        encodeMessageSized th `shouldBe` wire
        messageSize textual `shouldBe` BS.length wire
        messageSize th `shouldBe` BS.length wire
        decodeMessage @GC.OneofCollision wire `shouldBe` Right textual
        decodeMessage @TH.OneofCollision wire `shouldBe` Right th
    it "keeps last-arm-wins decoding with a second carrier present" $ do
      let wire = BS.pack [8, 7, 16, 9, 26, 1, 120]
          expected = Aeson.object ["alternate" Aeson..= (9 :: Int), "note" Aeson..= ("x" :: T.Text)]
      fmap Aeson.toJSON (decodeMessage @GC.OneofCollision wire) `shouldBe` Right expected
      fmap Aeson.toJSON (decodeMessage @TH.OneofCollision wire) `shouldBe` Right expected
  describe "Generated imported type resolution" $ do
    it "rejects an unresolved imported type that would bind to a local type" $
      THI.unresolvedImportRejected `shouldBe` True
    it "resolves a source enum and a built-in message under google/protobuf" $ do
      let wire = BS.pack [8, 1, 18, 3, 10, 1, 120]
          expected = Aeson.object
            [ "mode" Aeson..= ("AUDIT_MODE_ENABLED" :: T.Text)
            , "context" Aeson..= Aeson.object ["fileName" Aeson..= ("x" :: T.Text)]
            ]
      sizedRoundtrip @GGoogle.AuditEnvelope wire
      sizedRoundtrip @THGoogle.AuditEnvelope wire
      jsonParity @GGoogle.AuditEnvelope @THGoogle.AuditEnvelope expected expected
    it "uses absolute and parent-package enum references despite a local message with the same name" $ do
      let wire = BS.pack [8, 1, 16, 1, 26, 5, 10, 1, 120, 16, 1, 32, 1, 42, 0]
          ready = Aeson.String "IMPORT_STATUS_READY"
          expected = Aeson.object
            [ "relative" Aeson..= ready
            , "absolute" Aeson..= ready
            , "byKey" Aeson..= Aeson.object ["x" Aeson..= ready]
            , "selected" Aeson..= ready
            , "local" Aeson..= Aeson.object []
            ]
      sizedRoundtrip @GI.ImportScopeEnvelope wire
      sizedRoundtrip @THI.ImportScopeEnvelope wire
      sizedRoundtrip @THIE.ImportScopeEnvelope wire
      fmap Aeson.toJSON (decodeMessage @GI.ImportScopeEnvelope wire) `shouldBe` Right expected
      fmap Aeson.toJSON (decodeMessage @THI.ImportScopeEnvelope wire) `shouldBe` Right expected
      fmap Aeson.toJSON (decodeMessage @THIE.ImportScopeEnvelope wire) `shouldBe` Right expected
      jsonParity @GI.ImportScopeEnvelope @THI.ImportScopeEnvelope expected expected
      jsonParity @GI.ImportScopeEnvelope @THIE.ImportScopeEnvelope expected expected
  describe "Generated module paths" $
    it "imports a file with a distinct spelling and preserves its nested message" $ do
      let wire = BS.pack [10, 3, 10, 1, 120]
          expected = Aeson.object ["child" Aeson..= Aeson.object ["label" Aeson..= ("x" :: T.Text)]]
      sizedRoundtrip @GPath.CollisionOuter wire
      sizedRoundtrip @TH.CollisionOuter wire
      fmap Aeson.toJSON (decodeMessage @GPath.CollisionOuter wire) `shouldBe` Right expected
      fmap Aeson.toJSON (decodeMessage @TH.CollisionOuter wire) `shouldBe` Right expected
  describe "Generated descriptor metadata" $ do
    it "round-trips declared defaults and JSON names through the wire and AST" $ do
      let file = "metadata.proto"
          source = T.unlines
            [ "syntax = \"proto2\";"
            , "message Metadata {"
            , "  enum Choice { FIRST = 1; SECOND = 2; }"
            , "  optional int64 number = 1 [default = -0x10];"
            , "  optional string text = 2 [default = '\\303\\251', json_name = ''];"
            , "  optional bytes data = 3 [default = 'abc\\t\\377'];"
            , "  optional string empty = 4 [default = ''];"
            , "  optional bytes empty_data = 5 [default = ''];"
            , "  optional Choice choice = 6 [default = SECOND];"
            , "  optional bool flag = 7 [default = false];"
            , "  optional double ratio = 8 [default = -0x0];"
            , "  optional float fraction = 9 [default = 0.1];"
            , "  optional float limit = 10 [default = -inf];"
            , "  optional string absent = 11;"
            , "}"
            ]
      ast <- either (fail . show) pure (parseProtoFile file source)
      let descriptor = astToFileDescriptor file ast
          fields = Descriptor.dpField (V.head (Descriptor.fdpMessageType descriptor))
      V.toList (V.map Descriptor.fdpFieldDefault fields)
        `shouldBe` ["-16", "é", "abc\\t\\377", "", "", "SECOND", "false", "-0", "0.1", "-inf", ""]
      map descriptorDefaultConstant [fields V.! 3, fields V.! 4, fields V.! 10]
        `shouldBe` [Just (AST.CString ""), Just (AST.CBytes BS.empty), Nothing]
      Descriptor.fdpFieldHasEmptyJsonName (fields V.! 1) `shouldBe` True
      decodeMessage (encodeMessage descriptor) `shouldBe` Right descriptor
      astToFileDescriptor file (fileDescriptorToAST descriptor) `shouldBe` descriptor
    it "uses a separate collision-free synthetic oneof for each proto3 optional field" $ do
      let file = "presence.proto"
          source = T.unlines
            [ "syntax = \"proto3\";"
            , "message Presence {"
            , "  optional int32 chosen = 1;"
            , "  int32 _chosen = 2 [json_name = 'ordinary'];"
            , "  oneof X_chosen { string named = 3 [json_name = 'label']; }"
            , "  optional Sub _opt = 4;"
            , "  map<string, int32> lookup = 5 [json_name = 'values'];"
            , "  message Sub { optional bool nested = 1; }"
            , "}"
            ]
      ast <- either (fail . show) pure (parseProtoFile file source)
      let descriptor = astToFileDescriptor file ast
          message = V.head (Descriptor.fdpMessageType descriptor)
          fields = Descriptor.dpField message
      V.toList (V.map Descriptor.odpName (Descriptor.dpOneofDecl message))
        `shouldBe` ["X_chosen", "XX_chosen", "X_opt"]
      V.toList (V.map Descriptor.fdpFieldOneofIdx fields) `shouldBe` [1, -1, 0, 2, -1]
      V.toList (V.map Descriptor.fdpFieldProto3Optional fields) `shouldBe` [True, False, False, True, False]
      V.toList (V.map Descriptor.fdpFieldJsonName fields) `shouldBe` ["", "ordinary", "label", "", "values"]
      astToFileDescriptor file (fileDescriptorToAST descriptor) `shouldBe` descriptor
    it "embeds equivalent metadata in textual and TH codecs while preserving absence" $ do
      let checkParity textual th = do
            file <- either (fail . show) pure (decodeMessage textual)
            compiled <- either (fail . show) pure (decodeMessage th)
            compiled {Descriptor.fdpName = Descriptor.fdpName file} `shouldBe` file
            pure file
      groups <- checkParity
        (Schema.protoFileDescriptorBytes (Proxy @GG.GroupEnvelope))
        (Schema.protoFileDescriptorBytes (Proxy @TH.GroupEnvelope))
      aliases <- checkParity
        (Schema.protoFileDescriptorBytes (Proxy @GC.JsonAliases))
        (Schema.protoFileDescriptorBytes (Proxy @TH.JsonAliases))
      let groupFields name = concatMap (V.toList . Descriptor.dpField)
            (V.filter ((== name) . Descriptor.dpName)
              (Descriptor.dpNestedType (V.head (Descriptor.fdpMessageType groups))))
          defaults = map descriptorDefaultConstant (take 1 (groupFields "Entry") <> groupFields "Items")
          aliasFields = V.concatMap Descriptor.dpField
            (V.filter ((== "JsonAliases") . Descriptor.dpName) (Descriptor.fdpMessageType aliases))
      defaults `shouldBe` [Just (AST.CInt 7), Just (AST.CString "")]
      V.map Descriptor.fdpFieldJsonName (V.filter ((== "original_id") . Descriptor.fdpFieldName) aliasFields)
        `shouldBe` V.singleton "id"
      encodeMessage GG.defaultGroupEnvelope'Entry `shouldBe` BS.empty
      encodeMessage TH.defaultGroupEnvelope'Entry `shouldBe` BS.empty
    it "preserves proto2 oneof defaults in both generated descriptors without setting the oneof" $ do
      let check bytes = do
            file <- either (fail . show) pure (decodeMessage bytes)
            message <- maybe (fail "missing OneofDefaults descriptor") pure $
              V.find ((== "OneofDefaults") . Descriptor.dpName) (Descriptor.fdpMessageType file)
            let fields = V.toList (Descriptor.dpField message)
            map descriptorDefaultConstant fields `shouldBe`
              [Just (AST.CIdent "SECOND"), Just (AST.CBytes BS.empty), Just (AST.CInt (-15))]
            map Descriptor.fdpFieldOneofIdx fields `shouldBe` [0, 0, 0]
            astToFileDescriptor (T.unpack (Descriptor.fdpName file)) (fileDescriptorToAST file)
              `shouldBe` file
            pure message
      textual <- check (Schema.protoFileDescriptorBytes (Proxy @GG.OneofDefaults))
      compiled <- check (Schema.protoFileDescriptorBytes (Proxy @TH.OneofDefaults))
      textual `shouldBe` compiled
      encodeMessage GG.defaultOneofDefaults `shouldBe` BS.empty
      encodeMessage TH.defaultOneofDefaults `shouldBe` BS.empty
    it "preserves public and weak dependency indexes in both generated descriptors" $ do
      let expected =
            [ AST.ImportDef () (Just AST.ImportPublic) "json_collections.proto"
            , AST.ImportDef () (Just AST.ImportWeak) "google/protobuf/struct.proto"
            ]
      forM_
        [ Schema.protoFileDescriptorBytes (Proxy @GL.PackingLegacy)
        , Schema.protoFileDescriptorBytes (Proxy @TH.PackingLegacy)
        ] $ \bytes -> do
          file <- either (fail . show) pure (decodeMessage bytes)
          Descriptor.fdpUnknownFields file `shouldBe` [UnknownVarint 10 0, UnknownVarint 11 1]
          AST.protoImports (fileDescriptorToAST file) `shouldBe` expected
    it "resolves runtime custom options and strips source-retained values in both generated descriptors" $ do
      let expected = [AST.ImportDef () (Just AST.ImportOption) "option_policy.proto"]
          wire = BS.pack [10, 1, 120]
          json = Aeson.object ["payload" Aeson..= ("x" :: T.Text)]
      THOption.optionOnlyTypeRejected `shouldBe` True
      THOption.missingOptionImportRejected `shouldBe` True
      sizedRoundtrip @GOption.WithOptionImport wire
      sizedRoundtrip @THOption.WithOptionImport wire
      Reflection.give emptyExtensionRegistry $
        jsonParity @GOption.WithOptionImport @THOption.WithOptionImport json json
      forM_
        [ Schema.protoFileDescriptorBytes (Proxy @GOption.WithOptionImport)
        , Schema.protoFileDescriptorBytes (Proxy @THOption.WithOptionImport)
        ] $ \bytes -> do
          file <- either (fail . show) pure (decodeMessage bytes)
          Descriptor.fdpDependency file `shouldBe` V.empty
          Descriptor.fdpUnknownFields file `shouldBe` [UnknownLenDelim 15 "option_policy.proto"]
          AST.protoImports (fileDescriptorToAST file) `shouldBe` expected
          let message = V.head (Descriptor.fdpMessageType file)
              enum = V.head (Descriptor.fdpEnumType file)
              service = V.head (Descriptor.fdpService file)
              policy = BS.pack [10, 4, 107, 101, 112, 116]
              aggregate = BS.pack [10, 9] <> "aggregate" <> BS.pack [26, 2, 1, 4]
              defaults = BS.pack
                [ 32, 0
                , 42, 5, 10, 1, 122, 16, 0
                , 42, 4, 10, 0, 16, 4
                , 53, 0, 0, 0, 128
                , 58, 0
                , 66, 4, 8, 0, 18, 0
                , 74, 4, 8, 0, 16, 0
                , 90, 1, 0
                , 96, 0
                , 117, 0, 0, 0, 128
                ]
          fmap Descriptor.foUnknownFields (Descriptor.fdpOptions file)
            `shouldBe` Just [UnknownLenDelim 51003 "runtime"]
          fmap Descriptor.moUnknownFields (Descriptor.dpOptions message)
            `shouldBe` Just
              [ UnknownVarint 51000 1, UnknownLenDelim 51001 policy
              , UnknownLenDelim 51011 (BS.pack [5, 6]), UnknownLenDelim 51012 aggregate
              , UnknownLenDelim 51013 (BS.pack [162, 6, 1, 120])
              , UnknownLenDelim 51014 defaults, UnknownLenDelim 51015 (BS.pack [8, 0])
              , UnknownFixed32 51016 0, UnknownVarint 51017 0
              , UnknownLenDelim 51018 (BS.pack [29, 0, 0, 0, 0])
              ]
          fmap Descriptor.fldoUnknownFields (Descriptor.fdpFieldOptions (V.head (Descriptor.dpField message)))
            `shouldBe` Just [UnknownVarint 51004 13]
          fmap Descriptor.oneofoUnknownFields (Descriptor.odpOptions (V.head (Descriptor.dpOneofDecl message)))
            `shouldBe` Just [UnknownVarint 51005 1]
          fmap Descriptor.eroUnknownFields (Descriptor.dperOptions (V.head (Descriptor.dpExtensionRange message)))
            `shouldBe` Just [UnknownVarint 51010 1]
          fmap Descriptor.enoUnknownFields (Descriptor.edpOptions enum)
            `shouldBe` Just [UnknownVarint 51006 8]
          fmap Descriptor.evoUnknownFields (Descriptor.evdpOptions (V.head (Descriptor.edpValue enum)))
            `shouldBe` Just [UnknownLenDelim 51007 (BS.singleton 255)]
          fmap Descriptor.svoUnknownFields (Descriptor.sdpOptions service)
            `shouldBe` Just [UnknownVarint 51008 1]
          fmap Descriptor.mtoUnknownFields (Descriptor.mdpOptions (V.head (Descriptor.sdpMethod service)))
            `shouldBe` Just [UnknownLenDelim 51009 "method"]
    it "recovers modifiers from packed and unpacked descriptor indexes in dependency order" $ do
      let file =
            Descriptor.defaultFileDescriptorProto
              { Descriptor.fdpDependency = V.fromList ["weak.proto", "plain.proto", "public.proto", "more.proto"]
              , Descriptor.fdpUnknownFields =
                  [ UnknownLenDelim 10 (BS.pack [2, 9])
                  , UnknownVarint 11 0
                  , UnknownVarint 10 3
                  , UnknownLenDelim 11 (BS.singleton 128)
                  ]
              }
          ast = fileDescriptorToAST file
      AST.protoImports ast `shouldBe`
        [ AST.ImportDef () (Just AST.ImportWeak) "weak.proto"
        , AST.ImportDef () Nothing "plain.proto"
        , AST.ImportDef () (Just AST.ImportPublic) "public.proto"
        , AST.ImportDef () (Just AST.ImportPublic) "more.proto"
        ]
      Descriptor.fdpUnknownFields (astToFileDescriptor "imports.proto" ast)
        `shouldBe` [UnknownVarint 11 0, UnknownVarint 10 2, UnknownVarint 10 3]
    it "qualifies imported message and enum types and preserves enum descriptor kinds" $ do
      let checkField name kind typeName fields =
            fmap (\field -> (Descriptor.fdpFieldType field, Descriptor.fdpFieldTypeName field))
              (V.find ((== name) . Descriptor.fdpFieldName) fields)
              `shouldBe` Just (kind, typeName)
          messageFields name file = V.concatMap Descriptor.dpField
            (V.filter ((== name) . Descriptor.dpName) (Descriptor.fdpMessageType file))
      forM_
        [ Schema.protoFileDescriptorBytes (Proxy @GL.PackingLegacy)
        , Schema.protoFileDescriptorBytes (Proxy @TH.PackingLegacy)
        ] $ \bytes -> do
          file <- either (fail . show) pure (decodeMessage bytes)
          checkField "open" 14 ".ChoiceColor" (messageFields "ClosedCases" file)
          checkField "color_extra" 14 ".ChoiceColor" (Descriptor.fdpExtension file)
          checkField "null_extra" 14 ".google.protobuf.NullValue" (Descriptor.fdpExtension file)
      forM_
        [ Schema.protoFileDescriptorBytes (Proxy @GT.TemporalDuration)
        , Schema.protoFileDescriptorBytes (Proxy @TH.TemporalDuration)
        ] $ \bytes -> do
          file <- either (fail . show) pure (decodeMessage bytes)
          checkField "interval" 11 ".google.protobuf.Duration" (messageFields "TemporalDuration" file)
    it "qualifies packaged RPC types and paths while preserving streaming metadata" $ do
      let rpcTypeNames method = (Descriptor.mdpInputType method, Descriptor.mdpOutputType method)
          streaming method = (Descriptor.mdpClientStreaming method, Descriptor.mdpServerStreaming method)
          expectedTypes = replicate 2 (".example.api.RpcEnvelope.Request", ".google.protobuf.Empty")
      RPC.proxyServiceName `shouldBe` "example.api.Proxy"
      RPC.proxyMethodPaths `shouldBe` ["/example.api.Proxy/Pipe", "/example.api.Proxy/Fetch"]
      textual <- either (fail . show) pure (decodeMessage RPC.fileDescriptorProtoBytes)
      th <- either (fail . show) pure
        (decodeMessage (Schema.protoFileDescriptorBytes (Proxy @TH.RpcEnvelope)))
      th {Descriptor.fdpName = Descriptor.fdpName textual} `shouldBe` textual
      let proxy = V.filter ((== "Proxy") . Descriptor.sdpName) (Descriptor.fdpService textual)
          methods = V.toList (V.concatMap Descriptor.sdpMethod proxy)
      map rpcTypeNames methods `shouldBe` expectedTypes
      map streaming methods `shouldBe` [(True, True), (False, False)]
    it "allocates distinct selectors and constructors while retaining each RPC's original name" $ do
      map RPC.proxyMethodName [minBound .. maxBound] `shouldBe` ["Pipe", "Fetch"]
      map RPC.mirrorMethodName [minBound .. maxBound] `shouldBe` ["Pipe"]
      RPC.mirrorMethodPaths `shouldBe` ["/example.api.Mirror/Pipe"]
      let server = RPC.MirrorServer''Mirror {RPC.pipeHandler''Mirror'Pipe = Just}
          request = RPC.defaultRpcEnvelope'Request
      RPC.pipeHandler''Mirror'Pipe server request `shouldBe` Just request
    it "represents an empty service without inventing a method" $ do
      RPC.idleServiceName `shouldBe` "example.api.Idle"
      RPC.idleMethodPaths `shouldBe` []
      file <- either (fail . show) pure (decodeMessage RPC.fileDescriptorProtoBytes)
      let idle = V.find ((== "Idle") . Descriptor.sdpName) (Descriptor.fdpService file)
      fmap Descriptor.sdpMethod idle `shouldBe` Just V.empty
  describe "Proto2 delimited groups" $ do
    let file = "test-regression/proto/groups.proto"
        readSchema = TIO.readFile file >>= either (fail . show) pure . parseProtoFile file
        compileSchema source = either fail pure (TextFormat.buildTextFormatSchema [source])
        repeated = BS.pack [131, 1, 10, 1, 97, 132, 1, 131, 1, 132, 1]
        canonical = BS.pack [11, 8, 1, 16, 2, 27, 8, 1, 28, 12] <> repeated
        fragmented = BS.pack [11, 8, 1, 12, 11, 16, 2, 27, 8, 1, 28, 12] <> repeated
    it "merges singular occurrences and preserves repeated and nested groups in both generators" $ do
      sizedCanonical @GG.GroupEnvelope fragmented canonical
      sizedCanonical @TH.GroupEnvelope fragmented canonical
    it "rejects truncated groups and mismatched end tags in both generators" $
      forM_ [[11], [11, 20], [11, 27, 12], [131, 1, 12]] $ \bytes -> do
        let input = BS.pack bytes
        decodeMessage @GG.GroupEnvelope input `shouldSatisfy` isLeft
        decodeMessage @TH.GroupEnvelope input `shouldSatisfy` isLeft
    it "preserves legacy groups and group extensions through source and descriptor round-trips" $ do
      source <- readSchema
      printed <- either (fail . show) pure (parseProtoFile file (printProtoFile source))
      let descriptor = fileDescriptorToAST (astToFileDescriptor file source)
          input = "entry { first: 1 second: 2 Nested { present: true } } Items [{text: 'a'}, {}]"
          extText = "[note] { first: 9 } [extra] { count: 7 }"
          extWire = BS.pack [163, 6, 8, 9, 164, 6, 171, 6, 8, 7, 172, 6]
          rendering = T.unlines
            [ "Entry {", "  first: 1", "  second: 2", "  Nested {", "    present: true", "  }", "}"
            , "Items {", "  text: \"a\"", "}", "Items {", "}"
            ]
      forM_ [source, printed, descriptor] $ \schemaSource -> do
        schema <- compileSchema schemaSource
        textual <- either fail pure (TextFormat.textToTyped @GG.GroupEnvelope schema input)
        th <- either fail pure (TextFormat.textToTyped @TH.GroupEnvelope schema input)
        encodeMessage textual `shouldBe` canonical
        encodeMessage th `shouldBe` canonical
        TextFormat.typedToTextWithSchema schema False textual `shouldBe` Right rendering
        TextFormat.typedToTextWithSchema schema False th `shouldBe` Right rendering
        fmap encodeMessage (TextFormat.textToTyped @GG.GroupEnvelope schema extText) `shouldBe` Right extWire
        fmap encodeMessage (TextFormat.textToTyped @TH.GroupEnvelope schema extText) `shouldBe` Right extWire
    it "merges group extensions and leaves a length-delimited field of the same number unknown" $ do
      let input = BS.pack [163, 6, 8, 1, 164, 6, 163, 6, 16, 2, 164, 6, 171, 6, 8, 7, 172, 6, 170, 6, 1, 0]
          check :: (MessageDecode a, MessageEncode a, Ext.HasExtensions a)
                => Ext.Extension a BS.ByteString -> Ext.RepeatedExtension a BS.ByteString -> IO ()
          check note extras = do
            msg <- either (fail . show) pure (decodeMessage input)
            Ext.getExtension note msg `shouldBe` Just (BS.pack [8, 1, 16, 2])
            Ext.getRepeatedExtensionChecked extras msg `shouldBe` Right [BS.pack [8, 7]]
            encodeMessage (Ext.clearRepeatedExtension extras (Ext.clearExtension note msg))
              `shouldBe` BS.pack [170, 6, 1, 0]
      check GG.groupEnvelopeNote GG.groupEnvelopeExtra
      check TH.groupEnvelopeNote TH.groupEnvelopeExtra
  describe "Editions message encoding" $ do
    let file = "test-regression/proto/edition_delimited.proto"
        readSource = TIO.readFile file >>= either (fail . show) pure . parseProtoFile file
        schema = readSource >>= either fail pure . TextFormat.buildTextFormatSchema . pure
        ordinary = BS.pack
          [ 11, 8, 7, 12, 19, 8, 1, 20, 26, 2, 16, 2, 35, 8, 3, 36, 35, 36
          , 42, 7, 10, 1, 107, 18, 2, 8, 9, 48, 1
          ]
        oneofCanonical = BS.pack [59, 8, 1, 16, 2, 60]
        oneofFragments = BS.pack [59, 8, 1, 60, 59, 16, 2, 60]
        extensionWire = BS.pack [163, 6, 8, 3, 164, 6, 171, 6, 16, 4, 172, 6, 178, 6, 2, 8, 5]
    it "honors file defaults, field overrides, maps, enums, and oneof merges in both generators" $ do
      sizedCanonical @GD.EditionEnvelope (ordinary <> oneofFragments) (ordinary <> oneofCanonical)
      sizedCanonical @TH.EditionEnvelope (ordinary <> oneofFragments) (ordinary <> oneofCanonical)
      -- A different arm between two messages discards the earlier message.
      let switched = BS.pack [59, 8, 1, 60, 64, 9, 59, 16, 2, 60]
          lastArm = BS.pack [59, 16, 2, 60]
      sizedCanonical @GD.EditionEnvelope switched lastArm
      sizedCanonical @TH.EditionEnvelope switched lastArm
      forM_ [[59, 8, 1], [59, 68]] $ \bytes -> do
        decodeMessage @GD.EditionEnvelope (BS.pack bytes) `shouldSatisfy` isLeft
        decodeMessage @TH.EditionEnvelope (BS.pack bytes) `shouldSatisfy` isLeft
    it "honors a field-only delimited override in Edition 2024 in both generators" $ do
      let wire = BS.pack [11, 12, 18, 0]
      sizedCanonical @G24.FieldEncoding24 wire wire
      sizedCanonical @TH.FieldEncoding24 wire wire
    it "keeps ProtoJSON field names and values independent of the wire encoding" $ Reflection.give emptyExtensionRegistry $ do
      let input = "{\"grouplike\":{\"value\":7},\"regularName\":{\"first\":1},\"chosen\":{\"second\":2}}"
          wire = BS.pack [11, 8, 7, 12, 19, 8, 1, 20, 59, 16, 2, 60]
      textual <- either fail pure (Aeson.eitherDecode @GD.EditionEnvelope input)
      th <- either fail pure (Aeson.eitherDecode @TH.EditionEnvelope input)
      encodeMessage textual `shouldBe` wire
      encodeMessage th `shouldBe` wire
      Aeson.eitherDecode input `shouldBe` Right (Aeson.toJSON textual)
      Aeson.toJSON th `shouldBe` Aeson.toJSON textual
    it "uses type spelling only for group-like fields and reads both spellings" $ do
      s <- schema
      let input = "grouplike { value: 7 } regular_name { first: 1 } prefixed { second: 2 } "
            <> "items [{ first: 3 }, {}] children { key: 'k' value { first: 9 } } mode: WIRE_MODE_ACTIVE "
            <> "chosen { first: 1 second: 2 }"
          expected = T.unlines
            [ "GroupLike {", "  value: 7", "}", "regular_name {", "  first: 1", "}"
            , "prefixed {", "  second: 2", "}", "items {", "  first: 3", "}", "items {", "}"
            , "children {", "  key: \"k\"", "  value {", "    first: 9", "  }", "}"
            , "mode: WIRE_MODE_ACTIVE", "chosen {", "  first: 1", "  second: 2", "}"
            ]
          wire = ordinary <> oneofCanonical
      forM_ [input, T.replace "grouplike" "GroupLike" input] $ \source -> do
        textual <- either fail pure (TextFormat.textToTyped @GD.EditionEnvelope s source)
        th <- either fail pure (TextFormat.textToTyped @TH.EditionEnvelope s source)
        encodeMessage textual `shouldBe` wire
        encodeMessage th `shouldBe` wire
        TextFormat.typedToTextWithSchema s False textual `shouldBe` Right expected
        TextFormat.typedToTextWithSchema s False th `shouldBe` Right expected
    it "encodes delimited and length-prefixed extension payloads as declared" $ do
      s <- schema
      let input = "[extra] { first: 3 } [extras] { second: 4 } [prefixed_extra] { first: 5 }"
          check :: (MessageDecode a, MessageEncode a, Schema.ProtoMessage a, Ext.HasExtensions a)
                => Ext.Extension a BS.ByteString -> Ext.RepeatedExtension a BS.ByteString
                -> Ext.Extension a BS.ByteString -> IO ()
          check extra extras prefixedExtra = do
            msg <- either fail pure (TextFormat.textToTyped s input)
            encodeMessage msg `shouldBe` extensionWire
            Ext.getExtension extra msg `shouldBe` Just (BS.pack [8, 3])
            Ext.getRepeatedExtensionChecked extras msg `shouldBe` Right [BS.pack [16, 4]]
            Ext.getExtension prefixedExtra msg `shouldBe` Just (BS.pack [8, 5])
      check GD.editionEnvelopeExtra GD.editionEnvelopeExtras GD.editionEnvelopePrefixedExtra
      check TH.editionEnvelopeExtra TH.editionEnvelopeExtras TH.editionEnvelopePrefixedExtra
    it "keeps record selectors and same-named scoped extension bindings distinct" $ do
      s <- schema
      let input = "GroupLike { value: 7 } [extra] { second: 3 } [grouplike] { first: 6 } [ExtScope.extra]: 4"
          knownField = BS.pack [11, 8, 7, 12]
          wire = knownField <> BS.pack [163, 6, 16, 3, 164, 6, 187, 6, 8, 6, 188, 6, 192, 6, 4]
          check :: (MessageDecode a, MessageEncode a, Schema.ProtoMessage a, Ext.HasExtensions a)
                => Ext.Extension a BS.ByteString -> Ext.Extension a BS.ByteString -> Ext.Extension a Int32 -> IO ()
          check group extra scopedExtra = do
            message <- either fail pure (TextFormat.textToTyped s input)
            encodeMessage message `shouldBe` wire
            Ext.getExtension group message `shouldBe` Just (BS.pack [8, 6])
            Ext.getExtension extra message `shouldBe` Just (BS.pack [16, 3])
            Ext.getExtension scopedExtra message `shouldBe` Just 4
            encodeMessage (Ext.clearExtension scopedExtra (Ext.clearExtension extra (Ext.clearExtension group message)))
              `shouldBe` knownField
      check GD.editionEnvelopeGrouplike''grouplike GD.editionEnvelopeExtra GD.editionEnvelopeExtra''ExtScope'extra
      check TH.editionEnvelopeGrouplike''grouplike TH.editionEnvelopeExtra TH.editionEnvelopeExtra''ExtScope'extra
    it "registers distinct JSON extensions whose qualified names flatten identically" $ do
      let textualRegistry = GD.registerExt_ExtScope_sub_alias <> GD.registerExt_ExtScope_sub_alias''ExtScope_sub'alias
          thRegistry = TH.registerExt_ExtScope_sub_alias <> TH.registerExt_ExtScope_sub_alias''ExtScope_sub'alias
          json = Aeson.object
            [ "[ExtScope.sub_alias]" Aeson..= (5 :: Int)
            , "[ExtScope_sub.alias]" Aeson..= (6 :: Int)
            ]
          wire = BS.pack [200, 6, 5, 208, 6, 6]
      textual <- either fail pure (Reflection.give textualRegistry (Aeson.parseEither (Aeson.parseJSON @GD.EditionEnvelope) json))
      th <- either fail pure (Reflection.give thRegistry (Aeson.parseEither (Aeson.parseJSON @TH.EditionEnvelope) json))
      encodeMessage textual `shouldBe` wire
      encodeMessage th `shouldBe` wire
      Reflection.give textualRegistry (Aeson.toJSON textual) `shouldBe` json
      Reflection.give thRegistry (Aeson.toJSON th) `shouldBe` json
    it "passes the JSON registry through recursive messages, collections, and oneofs" $ do
      let textualRegistry = GD.registerExt_ExtScope_sub_alias
          thRegistry = TH.registerExt_ExtScope_sub_alias
          child = Aeson.object ["corecursive" Aeson..= Aeson.object ["[ExtScope.sub_alias]" Aeson..= (5 :: Int)]]
          json = Aeson.object
            [ "singular" Aeson..= child
            , "entries" Aeson..= [child]
            , "entriesByKey" Aeson..= Aeson.object ["key" Aeson..= child]
            , "entry" Aeson..= child
            ]
      textual <- either fail pure (Reflection.give textualRegistry (Aeson.parseEither (Aeson.parseJSON @GD.RegistryWrapper) json))
      th <- either fail pure (Reflection.give thRegistry (Aeson.parseEither (Aeson.parseJSON @TH.RegistryWrapper) json))
      encodeMessage textual `shouldBe` encodeMessage th
      Reflection.give textualRegistry (Aeson.toJSON textual) `shouldBe` json
      Reflection.give thRegistry (Aeson.toJSON th) `shouldBe` json
    it "preserves Editions features, maps, and oneofs through embedded descriptor bytes" $ do
      descriptor <- either (fail . show) pure (decodeMessage GD.fileDescriptorProtoBytes)
      Descriptor.fdpEdition descriptor `shouldBe` "2023"
      let ast = fileDescriptorToAST descriptor
          input = "GroupLike { value: 7 } regular_name { first: 1 } prefixed { second: 2 } "
            <> "items [{ first: 3 }, {}] children { key: 'k' value { first: 9 } } mode: WIRE_MODE_ACTIVE "
            <> "chosen { first: 1 second: 2 } [extra] { first: 3 } [extras] { second: 4 } "
            <> "[prefixed_extra] { first: 5 }"
          wire = ordinary <> oneofCanonical <> extensionWire
      converted <- either fail pure (TextFormat.buildTextFormatSchema [ast])
      fmap encodeMessage (TextFormat.textToTyped @GD.EditionEnvelope converted input) `shouldBe` Right wire
      fmap encodeMessage (TextFormat.textToTyped @TH.EditionEnvelope converted input) `shouldBe` Right wire
      edition2024 <- either (fail . show) pure (decodeMessage G24.fileDescriptorProtoBytes)
      converted2024 <- either fail pure (TextFormat.buildTextFormatSchema [fileDescriptorToAST edition2024])
      fmap encodeMessage (TextFormat.textToTyped @G24.FieldEncoding24 converted2024 "delimited {} standard {}")
        `shouldBe` Right (BS.pack [11, 12, 18, 0])
      fmap encodeMessage (TextFormat.textToTyped @TH.FieldEncoding24 converted2024 "delimited {} standard {}")
        `shouldBe` Right (BS.pack [11, 12, 18, 0])
  describe "Binary string extension UTF-8" $ do
    let badChecked = BS.pack [170, 6, 1, 255]
        badRepeated = BS.pack [178, 6, 1, 255]
        validChecked = BS.pack [170, 6, 4, 240, 159, 154, 128]
        checkRejected :: forall a. MessageDecode a => BS.ByteString -> IO ()
        checkRejected wire = fmap (const ()) (decodeMessage @a wire) `shouldBe` Left InvalidUtf8
    it "rejects malformed verified occurrences, even before a later valid value" $ do
      forM_ [badChecked, badChecked <> validChecked, badRepeated] $ \wire -> do
        checkRejected @GU.Utf8Extensions wire
        checkRejected @TH.Utf8Extensions wire
      let invalidDefault = BS.pack [218, 6, 1, 255]
      checkRejected @GD.EditionEnvelope invalidDefault
      checkRejected @TH.EditionEnvelope invalidDefault
    it "round-trips verified strings, unchecked strings, bytes, and wrong-wire records" $ do
      let wire = BS.pack [162, 6, 1, 255] <> validChecked
            <> BS.pack [178, 6, 0, 178, 6, 2, 195, 169, 186, 6, 1, 255, 168, 6, 1]
          uncheckedOverride = BS.pack [226, 6, 1, 255]
          check :: (MessageDecode a, Ext.HasExtensions a)
                => Ext.Extension a T.Text -> Ext.RepeatedExtension a T.Text -> IO ()
          check checked repeated = do
            message <- either (fail . show) pure (decodeMessage wire)
            Ext.getExtension checked message `shouldBe` Just "🚀"
            Ext.getRepeatedExtensionChecked repeated message `shouldBe` Right ["", "é"]
      sizedRoundtrip @GU.Utf8Extensions wire
      sizedRoundtrip @TH.Utf8Extensions wire
      sizedRoundtrip @GD.EditionEnvelope uncheckedOverride
      sizedRoundtrip @TH.EditionEnvelope uncheckedOverride
      check GU.utf8ExtensionsChecked GU.utf8ExtensionsRepeatedChecked
      check TH.utf8ExtensionsChecked TH.utf8ExtensionsRepeatedChecked
    it "validates the lexical and absolute extendees independently" $ do
      let lexical = BS.pack [194, 6, 1, 255]
          absolute = BS.pack [202, 6, 1, 255]
      checkRejected @GU.Utf8Scope'Utf8Extensions lexical
      checkRejected @TH.Utf8Scope'Utf8Extensions lexical
      checkRejected @GU.Utf8Extensions absolute
      checkRejected @TH.Utf8Extensions absolute
      sizedRoundtrip @GU.Utf8Extensions lexical
      sizedRoundtrip @TH.Utf8Extensions lexical
      sizedRoundtrip @GU.Utf8Scope'Utf8Extensions absolute
      sizedRoundtrip @TH.Utf8Scope'Utf8Extensions absolute
  describe "protoc descriptor wire format" $ do
    it "writes numeric editions and reads descriptors written by older Wireform releases" $ do
      let syntax = BS.pack [98, 8] <> "editions"
          current = syntax <> BS.pack [112, 232, 7]
          legacy = syntax <> BS.pack [114, 4] <> "2023"
          unknown = syntax <> BS.pack [112, 234, 7]
          readDescriptor = decodeMessage @Descriptor.FileDescriptorProto
      forM_ [current, legacy] $ \input -> do
        descriptor <- either (fail . show) pure (readDescriptor input)
        AST.protoSyntax (fileDescriptorToAST descriptor) `shouldBe` AST.Editions (AST.Edition "2023")
        encodeMessage descriptor `shouldBe` current
      fmap encodeMessage (readDescriptor unknown) `shouldBe` Right unknown
    it "uses proto2 syntax when protoc omits the descriptor's syntax field" $ do
      AST.protoSyntax (fileDescriptorToAST Descriptor.defaultFileDescriptorProto) `shouldBe` AST.Proto2
    it "distinguishes ordinary proto3 fields from optional fields and real oneofs" $ do
      let descriptorField name number = Descriptor.defaultFieldDescriptorProto
            { Descriptor.fdpFieldName = name
            , Descriptor.fdpFieldNumber = number
            , Descriptor.fdpFieldType = 5
            , Descriptor.fdpFieldLabel = 1
            }
          ordinary = descriptorField "ordinary" 1
          explicit = (descriptorField "explicit" 2)
            { Descriptor.fdpFieldOneofIdx = 1, Descriptor.fdpFieldProto3Optional = True }
          choice = (descriptorField "arm" 3) {Descriptor.fdpFieldOneofIdx = 0}
          message = Descriptor.defaultDescriptorProto
            { Descriptor.dpName = "Presence"
            , Descriptor.dpField = V.fromList [ordinary, explicit, choice]
            , Descriptor.dpOneofDecl = V.fromList (fmap (\name -> Descriptor.defaultOneofDescriptorProto
                {Descriptor.odpName = name}) ["choice", "_explicit"])
            }
          file = Descriptor.defaultFileDescriptorProto
            { Descriptor.fdpSyntax = "proto3", Descriptor.fdpMessageType = V.singleton message }
      case AST.protoTopLevels (fileDescriptorToAST file) of
        [AST.TLMessage ast] -> case AST.msgElements ast of
          [AST.MEField ordinaryField, AST.MEField explicitField, AST.MEOneof realChoice] -> do
            AST.fieldLabel ordinaryField `shouldBe` Nothing
            AST.fieldLabel explicitField `shouldBe` Just AST.Optional
            AST.oneofName realChoice `shouldBe` "choice"
            fmap AST.oneofFieldName (AST.oneofFields realChoice) `shouldBe` ["arm"]
          fields -> expectationFailure ("Unexpected proto3 descriptor fields " <> show fields)
        definitions -> expectationFailure ("Unexpected proto3 descriptor definitions " <> show definitions)
    it "encodes the protoc plugin's supported Editions range using enum numbers" $ do
      let response = Plugin.defaultCodeGeneratorResponse
            { Plugin.cgrsSupportedFeatures = 3
            , Plugin.cgrsMinimumEdition = 1000
            , Plugin.cgrsMaximumEdition = 1001
            }
          wire = BS.pack [16, 3, 24, 232, 7, 32, 233, 7]
      encodeMessage response `shouldBe` wire
      decodeMessage wire `shouldBe` Right response
  describe "ProtoJSON unknown enum names" $ do
    let parseTextual = Aeson.parseEither (JSON.parseJSONIgnoringUnknownEnumNames @GC.JsonEnumMode)
        parseTH = Aeson.parseEither (JSON.parseJSONIgnoringUnknownEnumNames @TH.JsonEnumMode)
        unknown = Aeson.String "UNKNOWN_ENUM_VALUE"
        red = Aeson.String "CHOICE_COLOR_RED"
        alias = Aeson.String "CHOICE_COLOR_CRIMSON"
        open = Aeson.Number 99
        check input expected = do
          textual <- either fail pure (parseTextual input)
          th <- either fail pure (parseTH input)
          Aeson.toJSON textual `shouldBe` expected
          Aeson.toJSON th `shouldBe` expected
          encodeMessage textual `shouldBe` encodeMessage th
        rejects input = do
          isLeft (parseTextual input) `shouldBe` True
          isLeft (parseTH input) `shouldBe` True
    it "drops only unknown enum names in fields, vectors, maps, and oneofs" $ do
      let input = Aeson.object
            [ "label" Aeson..= unknown
            , "primary_color" Aeson..= unknown
            , "colors" Aeson..= [unknown, alias, Aeson.String "99"]
            , "moreColors" Aeson..= [red, unknown]
            , "byName" Aeson..= Aeson.object ["unknown" Aeson..= unknown, "valid" Aeson..= alias]
            , "selectedColor" Aeson..= unknown
            ]
          expected = Aeson.object
            [ "label" Aeson..= unknown
            , "colors" Aeson..= [red, open]
            , "moreColors" Aeson..= [red]
            , "byName" Aeson..= Aeson.object ["valid" Aeson..= red]
            ]
      check input expected
      isLeft (Aeson.parseEither (Aeson.parseJSON @GC.JsonEnumMode) input) `shouldBe` True
      isLeft (Aeson.parseEither (Aeson.parseJSON @TH.JsonEnumMode) input) `shouldBe` True
    it "accepts quoted decimal enum numbers in strict mode" $
      jsonParity @GC.JsonEnumMode @TH.JsonEnumMode
        (Aeson.object ["colors" Aeson..= map Aeson.String ["-3", "+7", "00099"]])
        (Aeson.object ["colors" Aeson..= [Aeson.String "CHOICE_COLOR_RETIRED", red, open]])
    it "omits implicit scalar defaults and retains explicitly present defaults" $ do
      let implicit =
            [ "label" Aeson..= Aeson.String ""
            , "flag" Aeson..= False
            , "blob" Aeson..= Aeson.String ""
            , "implicitColor" Aeson..= Aeson.String "CHOICE_COLOR_UNSPECIFIED"
            ]
          explicit =
            [ "explicitFlag" Aeson..= False
            , "explicitBlob" Aeson..= Aeson.String ""
            , "explicitLabel" Aeson..= Aeson.String ""
            , "primaryColor" Aeson..= Aeson.String "CHOICE_COLOR_UNSPECIFIED"
            ]
      jsonParity @GC.JsonEnumMode @TH.JsonEnumMode (Aeson.object implicit) (Aeson.object [])
      jsonParity @GC.JsonEnumMode @TH.JsonEnumMode (Aeson.object (implicit <> explicit)) (Aeson.object explicit)
    it "applies the same mode inside singular, repeated, map, and oneof messages" $ do
      let inner enumName = Aeson.object ["primaryColor" Aeson..= enumName, "label" Aeson..= unknown]
          nested child = Aeson.object
            [ "child" Aeson..= child
            , "children" Aeson..= [child]
            , "namedChildren" Aeson..= Aeson.object ["a" Aeson..= child]
            , "selectedChild" Aeson..= child
            ]
      check (nested (inner unknown)) (nested (Aeson.object ["label" Aeson..= unknown]))
      check (nested (inner alias)) (nested (inner red))
    it "rejects wrong enum types and keeps duplicate aliases detectable" $ do
      forM_ [Aeson.Null, Aeson.Bool True, Aeson.Number 1.5] $ \value ->
        rejects (Aeson.object ["colors" Aeson..= [value]])
      rejects (Aeson.object ["primary_color" Aeson..= unknown, "primaryColor" Aeson..= red])
      let nullItem = Aeson.object ["colors" Aeson..= [Aeson.Null]]
      isLeft (Aeson.parseEither (Aeson.parseJSON @GC.JsonEnumMode) nullItem) `shouldBe` True
      isLeft (Aeson.parseEither (Aeson.parseJSON @TH.JsonEnumMode) nullItem) `shouldBe` True
  describe "ProtoJSON wrappers and FieldMask" $ do
    let check = jsonParity @GC.JsonWrappers @TH.JsonWrappers
        rejects json = do
          isLeft (Aeson.parseEither (Aeson.parseJSON @GC.JsonWrappers) json) `shouldBe` True
          isLeft (Aeson.parseEither (Aeson.parseJSON @TH.JsonWrappers) json) `shouldBe` True
    it "uses bare scalars for all nine wrappers with shared number and byte parsing" $ do
      let common =
            [ "flag" Aeson..= False
            , "int32Value" Aeson..= Aeson.Number (-2147483648)
            , "uint32Value" Aeson..= Aeson.Number 4294967295
            , "uint64Value" Aeson..= Aeson.String "18446744073709551615"
            , "text" Aeson..= Aeson.String ""
            , "mask" Aeson..= Aeson.String "user.displayName,inner3"
            , "selectedMask" Aeson..= Aeson.String ""
            ]
          input = Aeson.object $
            common <>
              [ "int64Value" Aeson..= Aeson.String "1e2"
              , "floatValue" Aeson..= Aeson.String "1.25e0"
              , "doubleValue" Aeson..= Aeson.String "-Infinity"
              , "blob" Aeson..= Aeson.String "-_8"
              , "blobs" Aeson..= [Aeson.String "-_8", Aeson.String ""]
              , "counts" Aeson..= Aeson.object ["n" Aeson..= Aeson.Number (-9223372036854775808)]
              ]
          expected = Aeson.object $
            common <>
              [ "int64Value" Aeson..= Aeson.String "100"
              , "floatValue" Aeson..= Aeson.Number 1.25
              , "doubleValue" Aeson..= Aeson.String "-Infinity"
              , "blob" Aeson..= Aeson.String "+/8="
              , "blobs" Aeson..= [Aeson.String "+/8=", Aeson.String ""]
              , "counts" Aeson..= Aeson.object ["n" Aeson..= Aeson.String "-9223372036854775808"]
              ]
      check input expected
      Aeson.fromJSON (Aeson.String "-_8")
        `shouldBe` Aeson.Success (Wrappers.BytesValue (BS.pack [251, 255]) [])
    it "accepts both base64 alphabets with or without padding" $
      forM_ ["+/8", "+/8=", "-_8", "-_8="] $ \bytes -> do
        let json value = Aeson.object ["blob" Aeson..= Aeson.String value]
        check (json bytes) (json "+/8=")
    it "preserves present defaults and treats null fields as absent" $ do
      let json = Aeson.object ["selectedUint32" Aeson..= Aeson.Number 0]
      check json json
      check (Aeson.object ["mask" Aeson..= Aeson.Null, "int32Value" Aeson..= Aeson.Null]) (Aeson.object [])
      sizedRoundtrip @GC.JsonWrappers (BS.pack [114, 0])
      sizedRoundtrip @TH.JsonWrappers (BS.pack [114, 0])
    it "rejects wrapper overflow, fractions, invalid bytes and malformed FieldMask text" $
      forM_
        [ ("int32Value", Aeson.Number 2147483648)
        , ("int32Value", Aeson.Number 1.5)
        , ("int64Value", Aeson.String "9223372036854775808")
        , ("uint32Value", Aeson.Number (-1))
        , ("uint64Value", Aeson.String "18446744073709551616")
        , ("floatValue", Aeson.String "1e50")
        , ("doubleValue", Aeson.String "1e999")
        , ("blob", Aeson.String "???")
        , ("blob", Aeson.String "A")
        , ("blob", Aeson.String "A=QI")
        , ("blob", Aeson.String "AAAA=")
        , ("blob", Aeson.String "AA==AA")
        , ("blob", Aeson.String "-_+/")
        , ("blobs", Aeson.toJSON [Aeson.Null])
        , ("mask", Aeson.String "foo,bar_bar")
        , ("mask", Aeson.String "foo bar")
        ]
        (\(field, value) -> rejects (Aeson.object [field Aeson..= value]))
    it "rejects wire FieldMask paths that would change through JSON" $
      forM_ ["tail_", "bad,_path", "bad_2", "bad__path", "upperCase", ""] $ \path ->
        evaluate (Aeson.toJSON (Mask.FieldMask (V.singleton path) []))
          `shouldThrow` (\(_ :: ErrorCall) -> True)
  describe "ProtoJSON Any" $ do
    let typed url fields = Aeson.object (("@type" Aeson..= (url :: T.Text)) : fields)
        empty = Aeson.object []
        packedEmpty = typed "type.googleapis.com/google.protobuf.Empty" []
        payload = typed "example.org/v1/ChoicePayload" ["count" Aeson..= (7 :: Int)]
        nested = typed "type.googleapis.com/google.protobuf.Any" ["value" Aeson..= payload]
        opaque = typed "example.org/external.Message" ["value" Aeson..= Aeson.String "CAc="]
        check = jsonParity @GC.JsonAny @TH.JsonAny
        enumPayload = typed "example.org/v1/JsonEnumMode"
        wrapAny inner = typed "type.googleapis.com/google.protobuf.Any" ["value" Aeson..= inner]
        parseAnyIgnoring = Aeson.parseEither (JSON.parseJSONIgnoringUnknownEnumNames @Any.Any)
        parseTHAnyIgnoring = Aeson.parseEither (JSON.parseJSONIgnoringUnknownEnumNames @TH.Any)
        parseTextualIgnoring = Aeson.parseEither (JSON.parseJSONIgnoringUnknownEnumNames @GC.JsonAny)
        parseTHIgnoring = Aeson.parseEither (JSON.parseJSONIgnoringUnknownEnumNames @TH.JsonAny)
    it "ignores enum names inside registered Any payloads in both generators" $ do
      let unknown = Aeson.String "UNKNOWN_ENUM_VALUE"
          raw = enumPayload
            [ "label" Aeson..= unknown
            , "primary_color" Aeson..= unknown
            , "colors" Aeson..= [unknown, Aeson.String "CHOICE_COLOR_CRIMSON", Aeson.Number 99]
            , "child" Aeson..= Aeson.object ["primary_color" Aeson..= unknown]
            ]
          cleaned = enumPayload
            [ "label" Aeson..= unknown
            , "colors" Aeson..= [Aeson.String "CHOICE_COLOR_RED", Aeson.Number 99]
            , "child" Aeson..= empty
            ]
          enclosing inner = Aeson.object
            [ "payload" Aeson..= inner
            , "items" Aeson..= [wrapAny inner, packedEmpty]
            , "entries" Aeson..= Aeson.object ["key" Aeson..= wrapAny (wrapAny inner), "opaque" Aeson..= opaque]
            , "selected" Aeson..= inner
            ]
          input = enclosing raw
          expected = enclosing cleaned
      anyValue <- either fail pure (parseAnyIgnoring raw)
      thAnyValue <- either fail pure (parseTHAnyIgnoring raw)
      textual <- either fail pure (parseTextualIgnoring input)
      th <- either fail pure (parseTHIgnoring input)
      Aeson.toJSON anyValue `shouldBe` cleaned
      Aeson.toJSON thAnyValue `shouldBe` cleaned
      encodeMessage anyValue `shouldBe` encodeMessage thAnyValue
      Aeson.toJSON textual `shouldBe` expected
      Aeson.toJSON th `shouldBe` expected
      encodeMessage textual `shouldBe` encodeMessage th
      isLeft (Aeson.parseEither (Aeson.parseJSON @Any.Any) raw) `shouldBe` True
      isLeft (Aeson.parseEither (Aeson.parseJSON @TH.Any) raw) `shouldBe` True
      isLeft (Aeson.parseEither (Aeson.parseJSON @GC.JsonAny) input) `shouldBe` True
      isLeft (Aeson.parseEither (Aeson.parseJSON @TH.JsonAny) input) `shouldBe` True
    it "keeps numeric and envelope validation in enum-ignore mode" $ do
      let invalid =
            [ enumPayload ["primaryColor" Aeson..= (2147483648 :: Integer)]
            , enumPayload ["primaryColor" Aeson..= Aeson.String "UNKNOWN", "primary_color" Aeson..= Aeson.String "UNKNOWN"]
            , typed "type.googleapis.com/google.protobuf.Any" ["value" Aeson..= opaque, "extra" Aeson..= True]
            , typed "example.org/.JsonEnumMode" ["primaryColor" Aeson..= Aeson.String "UNKNOWN"]
            ]
      forM_ invalid $ \inner -> do
        let input = Aeson.object ["payload" Aeson..= inner]
        isLeft (parseAnyIgnoring inner) `shouldBe` True
        isLeft (parseTHAnyIgnoring inner) `shouldBe` True
        isLeft (parseTextualIgnoring input) `shouldBe` True
        isLeft (parseTHIgnoring input) `shouldBe` True
    it "round-trips an explicitly present default Any as an empty object" $ do
      let json = Aeson.object ["payload" Aeson..= empty]
      Aeson.toJSON Any.defaultAny `shouldBe` empty
      Aeson.fromJSON empty `shouldBe` Aeson.Success Any.defaultAny
      check json json
      sizedRoundtrip @GC.JsonAny (BS.pack [10, 0])
      sizedRoundtrip @TH.JsonAny (BS.pack [10, 0])
    it "uses canonical JSON for singular, repeated, map and oneof fields" $ do
      let json =
            Aeson.object
              [ "payload" Aeson..= payload
              , "items" Aeson..= [packedEmpty, nested, empty]
              , "entries" Aeson..= Aeson.object ["key" Aeson..= nested]
              , "selected" Aeson..= packedEmpty
              ]
      check json json
      Aeson.fromJSON payload
        `shouldBe` Aeson.Success (Any.Any "example.org/v1/ChoicePayload" (BS.pack [8, 7]) [])
      Aeson.fromJSON payload
        `shouldBe` Aeson.Success (TH.Any "example.org/v1/ChoicePayload" (BS.pack [8, 7]) [])
    it "retains the explicit base64 fallback for an unregistered type" $ do
      let json = Aeson.object ["payload" Aeson..= opaque]
      check json json
      Aeson.fromJSON opaque
        `shouldBe` Aeson.Success (Any.Any "example.org/external.Message" (BS.pack [8, 7]) [])
    it "rejects unexpected fields in opaque and custom WKT envelopes" $ do
      let invalid =
            [ typed "example.org/external.Message" ["count" Aeson..= (7 :: Int)]
            , typed "type.googleapis.com/google.protobuf.StringValue"
                ["value" Aeson..= Aeson.String "hello", "extra" Aeson..= True]
            ]
      forM_ invalid $ \inner -> do
        let json = Aeson.object ["payload" Aeson..= inner]
        isLeft (Aeson.parseEither (Aeson.parseJSON @GC.JsonAny) json) `shouldBe` True
        isLeft (Aeson.parseEither (Aeson.parseJSON @TH.JsonAny) json) `shouldBe` True
    it "rejects nonempty envelopes without a canonical type URL" $ do
      let badTypes = ["", "ChoicePayload", "example.org/", "example.org/.ChoicePayload"]
          invalid = Aeson.object ["value" Aeson..= Aeson.String ""] : fmap (`typed` []) badTypes
      forM_ invalid $ \inner -> do
        let json = Aeson.object ["payload" Aeson..= inner]
        isLeft (Aeson.parseEither (Aeson.parseJSON @Any.Any) inner) `shouldBe` True
        isLeft (Aeson.parseEither (Aeson.parseJSON @GC.JsonAny) json) `shouldBe` True
        isLeft (Aeson.parseEither (Aeson.parseJSON @TH.JsonAny) json) `shouldBe` True
  describe "Editions field presence" $ do
    it "standalone codegen preserves every explicitly present scalar default" $
      forM_ scalarDefaults $
        roundtrip @G23.Fields23
    it "loadProto preserves every explicitly present scalar default" $
      forM_ scalarDefaults $
        roundtrip @TH.Fields23
    it "absent and explicitly zero fields remain distinct in both generators" $ do
      fmap G23.fields23Count (decodeMessage BS.empty) `shouldBe` Right Nothing
      fmap G23.fields23Count (decodeMessage (BS.pack [8, 0])) `shouldBe` Right (Just 0)
      fmap TH.fields23Count (decodeMessage BS.empty) `shouldBe` Right Nothing
      fmap TH.fields23Count (decodeMessage (BS.pack [8, 0])) `shouldBe` Right (Just 0)
    it "file and field overrides, aggregate options and nested messages agree" $ do
      forM_ [BS.pack [16, 0], BS.pack [26, 2, 16, 0]] $ roundtrip @G24.Fields24
      forM_ [BS.pack [16, 0], BS.pack [26, 2, 16, 0]] $ roundtrip @TH.Fields24
      fmap G24.fields24Count (decodeMessage (BS.pack [8, 0])) `shouldBe` Right 0
      fmap TH.fields24Count (decodeMessage (BS.pack [8, 0])) `shouldBe` Right 0
      fmap encodeMessage (decodeMessage (BS.pack [8, 0]) :: Either DecodeError G24.Fields24) `shouldBe` Right BS.empty
      fmap encodeMessage (decodeMessage (BS.pack [8, 0]) :: Either DecodeError TH.Fields24) `shouldBe` Right BS.empty
      fmap encodeMessage (decodeMessage (BS.pack [64, 0, 112, 0]) :: Either DecodeError G23.Fields23) `shouldBe` Right BS.empty
      fmap encodeMessage (decodeMessage (BS.pack [64, 0, 112, 0]) :: Either DecodeError TH.Fields23) `shouldBe` Right BS.empty
    it "repeated, map, oneof and nested fields retain their wire shapes" $ do
      forM_ collectionValues $ roundtrip @G23.Fields23
      forM_ collectionValues $ roundtrip @TH.Fields23
    it "merge preserves an explicitly present zero over a nonzero value" $ do
      case (decodeMessage (BS.pack [8, 7]), decodeMessage (BS.pack [8, 0])) of
        (Right a, Right b) -> encodeMessage (a <> b :: G23.Fields23) `shouldBe` BS.pack [8, 0]
        _ -> expectationFailure "standalone fixture decode failed"
      case (decodeMessage (BS.pack [8, 7]), decodeMessage (BS.pack [8, 0])) of
        (Right a, Right b) -> encodeMessage (a <> b :: TH.Fields23) `shouldBe` BS.pack [8, 0]
        _ -> expectationFailure "TH fixture decode failed"
    it "rejects field presence in scopes that protoc forbids" $ do
      let file = "invalid_presence.proto"
          invalid =
            [ "option features.field_presence = LEGACY_REQUIRED; message M { int32 x = 1; }"
            , "message M { repeated int32 x = 1 [features.field_presence = LEGACY_REQUIRED]; }"
            , "message M { map<string, int32> x = 1 [features.field_presence = IMPLICIT]; }"
            , "message M { oneof choice { int32 x = 1 [features = {field_presence: LEGACY_REQUIRED}]; } }"
            , "message M { extensions 100 to 110; } extend M { int32 x = 101 [features.field_presence = EXPLICIT]; }"
            ]
      forM_ invalid $ \declarations -> do
        pf <- either (fail . show) pure (parseProtoFile file ("edition = \"2023\"; " <> declarations))
        isLeft (TextFormat.buildTextFormatSchema [pf]) `shouldBe` True
        evaluate (T.length (generateModuleText defaultGenerateOpts Map.empty file pf))
          `shouldThrow` (\(_ :: ErrorCall) -> True)
    it "tracks required scalar, bytes, enum, and delimited message presence in both generators" $ do
      let wire = BS.pack [8, 0, 18, 0, 27, 8, 0, 18, 0, 28, 32, 0]
          fragments = BS.pack [8, 0, 18, 0, 27, 8, 1, 28, 27, 8, 0, 18, 0, 28, 32, 0, 40, 0]
          json = Aeson.object
            [ "count" Aeson..= (0 :: Int)
            , "payload" Aeson..= Aeson.String ""
            , "child" Aeson..= Aeson.object ["value" Aeson..= (0 :: Int), "label" Aeson..= Aeson.String ""]
            , "state" Aeson..= Aeson.String "CLOSED_EDITION_UNSPECIFIED"
            ]
      sizedCanonical @G24.Required24 fragments wire
      sizedCanonical @TH.Required24 fragments wire
      sizedRoundtrip @G24.Required24 BS.empty
      sizedRoundtrip @TH.Required24 BS.empty
      jsonParity @G24.Required24 @TH.Required24 json json
      fmap encodeMessage (Aeson.parseEither (Aeson.parseJSON @G24.Required24) json) `shouldBe` Right wire
      fmap encodeMessage (Aeson.parseEither (Aeson.parseJSON @TH.Required24) json) `shouldBe` Right wire
    it "reports required metadata and rejects missing TextFormat fields after descriptor round-trip" $ do
      let file = "test-regression/proto/edition2024.proto"
          requiredLabels = replicate 4 (Just Schema.LabelRequired) <> [Just Schema.LabelOptional]
          label proxy number = case Schema.fieldDescriptorByNumber number proxy of
            Just (Schema.SomeField fd) -> Just (Schema.fdLabel fd)
            Nothing -> Nothing
          complete = "count: 0 payload: '' child { value: 0 label: '' } state: CLOSED_EDITION_UNSPECIFIED"
          missing = ["", "count: 0 payload: '' child { value: 0 } state: CLOSED_EDITION_UNSPECIFIED"]
          wire = BS.pack [8, 0, 18, 0, 27, 8, 0, 18, 0, 28, 32, 0]
      fmap (label (Proxy @G24.Required24)) [1 .. 5] `shouldBe` requiredLabels
      fmap (label (Proxy @TH.Required24)) [1 .. 5] `shouldBe` requiredLabels
      source <- TIO.readFile file >>= either (fail . show) pure . parseProtoFile file
      descriptor <- either (fail . show) pure (decodeMessage G24.fileDescriptorProtoBytes)
      forM_ [source, fileDescriptorToAST descriptor] $ \ast -> do
        schema <- either fail pure (TextFormat.buildTextFormatSchema [ast])
        fmap encodeMessage (TextFormat.textToTyped @G24.Required24 schema complete) `shouldBe` Right wire
        fmap encodeMessage (TextFormat.textToTyped @TH.Required24 schema complete) `shouldBe` Right wire
        forM_ missing $ \input -> do
          TextFormat.textToTyped @G24.Required24 schema input `shouldSatisfy` isLeft
          TextFormat.textToTyped @TH.Required24 schema input `shouldSatisfy` isLeft
    it "committed standalone fixtures exactly match the generator" $ do
      let names = ["edition2023", "edition2024", "integerMaps", "json_temporal", "json_collections", "packing_legacy", "groups", "edition_delimited", "extension_utf8", "rpc", "import_status", "import_scope", "my_file", "my__file", "google/protobuf/audit_mode", "google_import", "option_import"]
          readSchema name = do
            let file = name <> ".proto"
            source <- TIO.readFile ("test-regression/proto/" <> file)
            pf <- either (fail . show) pure (parseProtoFile file source)
            pure (file, ResolvedProto pf file Map.empty)
      files <- mapM readSchema names
      optionSchemas <- mapM readSchema ["option_policy", "option_defaults"]
      let opts = defaultGenerateOpts {genModulePrefix = "Generated"}
          registry = buildTypeRegistry opts files
          byName = Map.fromList (map (\(path, rp) -> (T.pack path, rpFile rp)) (optionSchemas <> files))
      forM_ files $ \(file, rp) -> do
        let pf = rpFile rp
            imports = Map.restrictKeys byName (importClosure byName (const True) (map AST.importPath (AST.protoImports pf)))
        actual <- TIO.readFile ("test-regression/" <> T.unpack (T.replace "." "/" (moduleNameForProto opts file pf)) <> ".hs")
        generateModuleTextWithImports opts registry file imports pf `shouldBe` Right actual
  describe "Protobuf enum names" $
    it "preserves case-distinct aliases and canonicalizes both to the first name" $ do
      let wrap name = Aeson.object ["value" Aeson..= (name :: T.Text)]
          canonical = wrap "CASE_ALIAS_CANONICAL"
          wire = BS.pack [8, 2]
          parseTextual name = Aeson.parseEither (Aeson.parseJSON @G23.EnumAlias23) (wrap name)
          parseTH name = Aeson.parseEither (Aeson.parseJSON @TH.EnumAlias23) (wrap name)
      forM_ ["CASE_ALIAS_CANONICAL", "MOO", "moo"] $ \name -> do
        textual <- either fail pure (parseTextual name)
        th <- either fail pure (parseTH name)
        encodeMessage textual `shouldBe` wire
        encodeMessage th `shouldBe` wire
        Aeson.toJSON textual `shouldBe` canonical
        Aeson.toJSON th `shouldBe` canonical
      G23.CaseAlias23'Moo `shouldBe` G23.CaseAlias23'CaseAliasCanonical
      G23.CaseAlias23'Moo''moo `shouldBe` G23.CaseAlias23'CaseAliasCanonical
      descriptor <- either (fail . show) pure (decodeMessage G23.fileDescriptorProtoBytes)
      let converted = astToFileDescriptor "edition2023.proto" (fileDescriptorToAST descriptor)
          aliasOptions = fmap Descriptor.edpOptions . V.filter ((== "CaseAlias23") . Descriptor.edpName)
            . Descriptor.fdpEnumType
      forM_ [descriptor, converted] $ \fdp ->
        fmap (fmap Descriptor.enoAllowAlias) (aliasOptions fdp) `shouldBe` V.singleton (Just True)
  describe "Standalone integer maps" $ do
    it "integer value functions compile and agree with TH for limits and zero" $
      forM_ integerMapValues $ \message -> do
        let wire = encodeMessage message
        decodeMessage wire `shouldBe` Right message
        messageSize message `shouldBe` BS.length wire
        encodeMessageSized message `shouldBe` wire
        fmap encodeMessage (decodeMessage wire :: Either DecodeError TH.IntegerMaps) `shouldBe` Right wire
    it "sint32 and sint64 map values use canonical ZigZag and exact sizes" $ do
      let small = GM.defaultIntegerMaps {GM.integerMapsSint32Values = Map.singleton "a" (-1)}
          large = GM.defaultIntegerMaps {GM.integerMapsSint64Values = Map.singleton "a" (-1)}
      encodeMessage small `shouldBe` BS.pack [42, 5, 10, 1, 97, 16, 1]
      encodeMessage large `shouldBe` BS.pack [50, 5, 10, 1, 97, 16, 1]
      messageSize small `shouldBe` 7
      messageSize large `shouldBe` 7
    it "the last entry for a key wins on the wire in both generators" $ do
      let wire = BS.pack [10, 5, 10, 1, 97, 16, 1, 10, 5, 10, 1, 97, 16, 2]
      fmap GM.integerMapsInt32Values (decodeMessage wire) `shouldBe` Right (Map.singleton "a" 2)
      fmap TH.integerMapsInt32Values (decodeMessage wire) `shouldBe` Right (Map.singleton "a" 2)
    it "merging maps prefers the incoming value and preserves other entries" $ do
      let first = Map.fromList [("a", 1), ("b", 3)]
          second = Map.singleton "a" 2
          expected = Map.fromList [("a", 2), ("b", 3)]
          textual entries = GM.defaultIntegerMaps {GM.integerMapsInt32Values = entries}
          th entries = TH.defaultIntegerMaps {TH.integerMapsInt32Values = entries}
      GM.integerMapsInt32Values (textual first <> textual second) `shouldBe` expected
      TH.integerMapsInt32Values (th first <> th second) `shouldBe` expected
    it "the shipped Struct codec also uses the last value for a key" $ do
      let value n = Struct.defaultValue {Struct.valueKind = Just (Struct.Value'Kind'NumberValue n)}
          struct n = Struct.defaultStruct {Struct.structFields = Map.singleton "a" (value n)}
          first = struct 1
          second = struct 2
      decodeMessage (encodeMessage first <> encodeMessage second) `shouldBe` Right second
      (first <> second) `shouldBe` second
    it "message-valued maps decode explicit and omitted values in both generators" $ do
      let missingValue = BS.pack [10, 3, 10, 1, 97]
          explicitValue = BS.pack [10, 7, 10, 1, 97, 18, 2, 8, 3]
          textualCounts = fmap (fmap GM.mapPayloadCount . GM.messageMapEntries) . decodeMessage
          thCounts = fmap (fmap TH.mapPayloadCount . TH.messageMapEntries) . decodeMessage
      forM_ [(missingValue, 0), (explicitValue, 3)] $ \(wire, count) -> do
        textualCounts wire `shouldBe` Right (Map.singleton "a" count)
        thCounts wire `shouldBe` Right (Map.singleton "a" count)
  describe "ProtoJSON quoted numbers" $ do
    it "generated Float and Double accept e/E and signed exponents" $
      forM_ (["1e2", "1E+2", "10000e-2", "1.0e2"] :: [T.Text]) $ \number -> do
        let json = Aeson.object ["value" Aeson..= number, "small" Aeson..= number]
        fmap TH.jsonNumbersValue (Aeson.parseEither Aeson.parseJSON json) `shouldBe` Right 100
        fmap TH.jsonNumbersSmall (Aeson.parseEither Aeson.parseJSON json) `shouldBe` Right 100
    it "optional and oneof floating fields use the same number grammar" $ do
      let json = "{\"optionalValue\":\"1e2\",\"selected\":\"1E2\"}"
      case Aeson.eitherDecode json of
        Left err -> expectationFailure err
        Right result -> do
          TH.jsonNumbersOptionalValue result `shouldBe` Just 100
          case TH.jsonNumbersChoice result of
            Just (TH.JsonNumbers'Choice'Selected value) -> value `shouldBe` 100
            _ -> expectationFailure "quoted oneof number was dropped"
    it "runtime float/double and int64 helpers accept quoted exponents" $ do
      Aeson.parseEither JSON.protoDoubleFromJSON (Aeson.String "1e2") `shouldBe` Right 100
      Aeson.parseEither JSON.protoFloatFromJSON (Aeson.String "1E2") `shouldBe` Right 100
      Aeson.parseEither JSON.protoInt64FromJSON (Aeson.String "1e2") `shouldBe` Right 100
      Aeson.parseEither JSON.protoWord64FromJSON (Aeson.String "1E2") `shouldBe` Right 100
    it "rejects whitespace, trailing junk, noncanonical sentinels and overflow" $
      forM_ ["", " 1", "1 ", "1e", "1e2x", "nan", "NAN", "+nan", "-nan", " NaN", "NaN ", "inf", "INF", "+Infinity", " Infinity", "Infinity ", "-inf", "-INF", " -Infinity", "-Infinity ", "1e1000"] $ \number -> do
        isLeft (Aeson.parseEither JSON.protoDoubleFromJSON (Aeson.String number)) `shouldBe` True
        isLeft (Aeson.parseEither JSON.protoFloatFromJSON (Aeson.String number)) `shouldBe` True
        isLeft (Aeson.parseEither Aeson.parseJSON (Aeson.object ["value" Aeson..= number]) :: Either String TH.JsonNumbers) `shouldBe` True
    it "retains canonical NaN/Infinity strings" $ do
      fmap isNaN (Aeson.parseEither JSON.protoDoubleFromJSON (Aeson.String "NaN")) `shouldBe` Right True
      fmap isInfinite (Aeson.parseEither JSON.protoDoubleFromJSON (Aeson.String "Infinity")) `shouldBe` Right True
      Aeson.parseEither JSON.protoDoubleFromJSON (Aeson.String "-Infinity") `shouldBe` Right (negate (1 / 0))
  describe "ProtoJSON generated timestamp fields" $ do
    let stamp text = Aeson.object ["stamp" Aeson..= Aeson.String text]
        decode = Aeson.parseEither (Aeson.parseJSON @TH.JsonTimestamp)
    it "roundtrips an offset that normalizes to a pre-epoch instant" $
      fmap Aeson.toJSON (decode (stamp "1970-01-01T00:59:59.1+01:00"))
        `shouldBe` Right (stamp "1969-12-31T23:59:59.100Z")
    it "rejects invalid dates and offsets instead of dropping the field" $
      forM_ ["2023-02-29T00:00:00Z", "2026-11-10T00:00:00+00:99"] $ \text ->
        isLeft (decode (stamp text)) `shouldBe` True

  describe "ProtoJSON temporal parity" $ do
    let temporal field input = Aeson.object [field Aeson..= Aeson.String input]
    it "both generators produce the same Duration wire bytes and canonical JSON" $
      forM_ [("1.25s", "1.250s"), ("-0.1s", "-0.100s"), ("315576000000.999999999s", "315576000000.999999999s")] $ \(input, expected) ->
        jsonParity @GT.TemporalDuration @TH.TemporalDuration
          (temporal "interval" input) (temporal "interval" expected)
    it "both generators produce the same normalized Timestamp wire bytes and JSON" $
      jsonParity @GT.TemporalTimestamp @TH.TemporalTimestamp
        (temporal "instant" "1970-01-01T00:59:59.1+01:00") (temporal "instant" "1969-12-31T23:59:59.100Z")
    it "both generators reject invalid temporal fields" $ do
      forM_ ["garbage", "1.25garbages", "18446744073709551616s", "315576000001s"] $ \input -> do
        let value = temporal "interval" input
        isLeft (Aeson.parseEither (Aeson.parseJSON @GT.TemporalDuration) value) `shouldBe` True
        isLeft (Aeson.parseEither (Aeson.parseJSON @TH.TemporalDuration) value) `shouldBe` True
      forM_ ["2023-02-29T00:00:00Z", "2026-11-10T00:00:00+00:99"] $ \input -> do
        let value = temporal "instant" input
        isLeft (Aeson.parseEither (Aeson.parseJSON @GT.TemporalTimestamp) value) `shouldBe` True
        isLeft (Aeson.parseEither (Aeson.parseJSON @TH.TemporalTimestamp) value) `shouldBe` True

  describe "ProtoJSON collection parity" $ do
    it "both generators omit empty scalar, message and enum vectors and maps" $ do
      let empty = Aeson.object []
          array = Aeson.Array mempty
          explicit = Aeson.object ["ints" Aeson..= array, "messages" Aeson..= array, "colors" Aeson..= array, "values" Aeson..= empty, "blobs" Aeson..= empty]
      jsonParity @GC.JsonContainers @TH.JsonContainers explicit empty
      jsonParity @GC.JsonContainers @TH.JsonContainers empty empty
    it "both generators emit populated vectors and maps" $ do
      let populated =
            Aeson.object
              [ "ints" Aeson..= [1, 2 :: Int]
              , "messages" Aeson..= [Aeson.object ["count" Aeson..= (7 :: Int)]]
              , "colors" Aeson..= [Aeson.String "CHOICE_COLOR_RED"]
              , "values" Aeson..= Aeson.object ["a" Aeson..= (7 :: Int)]
              , "blobs" Aeson..= Aeson.object ["a" Aeson..= Aeson.String "/w=="]
              ]
      jsonParity @GC.JsonContainers @TH.JsonContainers populated populated
    it "both generators decode and emit an array of Base64 strings" $ do
      let json = Aeson.object ["blobs" Aeson..= map Aeson.String ["", "AA==", "/w=="]]
      jsonParity @GC.JsonCollections @TH.JsonCollections json json
    it "both generators omit an empty repeated bytes field" $
      jsonParity @GC.JsonCollections @TH.JsonCollections
        (Aeson.object ["blobs" Aeson..= ([] :: [T.Text])]) (Aeson.object [])
    it "both generators reject invalid Base64 array members" $ do
      let json = Aeson.object ["blobs" Aeson..= [Aeson.String "!"]]
      isLeft (Aeson.parseEither (Aeson.parseJSON @GC.JsonCollections) json) `shouldBe` True
      isLeft (Aeson.parseEither (Aeson.parseJSON @TH.JsonCollections) json) `shouldBe` True
  describe "ProtoJSON repeated numbers" $ do
    let check = jsonParity @GC.JsonRepeatedNumbers @TH.JsonRepeatedNumbers
        arrayField key members = Aeson.object [key Aeson..= (members :: [Aeson.Value])]
    it "uses scalar JSON codecs with boxed, unboxed, list and sequence carriers" $ do
      let fields =
            [ "limits" Aeson..= map Aeson.String ["18446744073709551615", "0"]
            , "ratios" Aeson..= [Aeson.String "Infinity", Aeson.String "-Infinity", Aeson.Number 1.25]
            , "offsets" Aeson..= map Aeson.String ["-9223372036854775808", "2"]
            , "fractions" Aeson..= [Aeson.Number 1.5, Aeson.String "-Infinity"]
            , "sampleCounts" Aeson..= map Aeson.Number [100, 0, 4294967295]
            ]
          canonical = Aeson.object fields
          alternate = Aeson.object
            [ "limits" Aeson..= [Aeson.String "18446744073709551615", Aeson.Number 0]
            , "ratios" Aeson..= map Aeson.String ["Infinity", "-Infinity", "1.25e0"]
            , "offsets" Aeson..= map Aeson.String ["-9223372036854775808", "2e0"]
            , "fractions" Aeson..= map Aeson.String ["15e-1", "-Infinity"]
            , "sample_counts" Aeson..= [Aeson.String "1e2", Aeson.Number 0, Aeson.String "4294967295"]
            ]
          wire = BS.pack
            [ 10, 11, 255, 255, 255, 255, 255, 255, 255, 255, 255, 1, 0
            , 18, 24, 0, 0, 0, 0, 0, 0, 240, 127, 0, 0, 0, 0, 0, 0, 240, 255, 0, 0, 0, 0, 0, 0, 244, 63
            , 26, 11, 255, 255, 255, 255, 255, 255, 255, 255, 255, 1, 4
            , 34, 8, 0, 0, 192, 63, 0, 0, 128, 255
            , 42, 12, 100, 0, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255
            ]
      check canonical canonical
      check alternate canonical
      fmap encodeMessage (Aeson.parseEither (Aeson.parseJSON @GC.JsonRepeatedNumbers) alternate) `shouldBe` Right wire
      fmap encodeMessage (Aeson.parseEither (Aeson.parseJSON @TH.JsonRepeatedNumbers) alternate) `shouldBe` Right wire
      fmap Aeson.toJSON (decodeMessage @GC.JsonRepeatedNumbers wire) `shouldBe` Right canonical
      fmap Aeson.toJSON (decodeMessage @TH.JsonRepeatedNumbers wire) `shouldBe` Right canonical
    it "round-trips NaN and omits null and empty arrays" $ do
      let nan = Aeson.object ["ratios" Aeson..= [Aeson.String "NaN"], "fractions" Aeson..= [Aeson.String "NaN"]]
          unset = Aeson.object ["limits" Aeson..= Aeson.Null, "fractions" Aeson..= Aeson.Array mempty]
      check nan nan
      check unset (Aeson.object [])
    it "rejects overflow, fractions in integer fields, null members and non-arrays" $
      forM_
        [ arrayField "limits" [Aeson.String "18446744073709551616"]
        , arrayField "limits" [Aeson.Null]
        , arrayField "offsets" [Aeson.String "-9223372036854775809"]
        , arrayField "sampleCounts" [Aeson.Number 1.5]
        , arrayField "ratios" [Aeson.String "1e400"]
        , arrayField "fractions" [Aeson.String "3.5e38"]
        , Aeson.object ["limits" Aeson..= Aeson.String "1"]
        ] $ \json -> do
          isLeft (Aeson.parseEither (Aeson.parseJSON @GC.JsonRepeatedNumbers) json) `shouldBe` True
          isLeft (Aeson.parseEither (Aeson.parseJSON @TH.JsonRepeatedNumbers) json) `shouldBe` True
  describe "ProtoJSON typed maps" $ do
    let check = jsonParity @GC.JsonTypedMaps @TH.JsonTypedMaps
        wireFor json expected = do
          let wire = BS.pack expected
          fmap encodeMessage (Aeson.parseEither (Aeson.parseJSON @GC.JsonTypedMaps) json) `shouldBe` Right wire
          fmap encodeMessage (Aeson.parseEither (Aeson.parseJSON @TH.JsonTypedMaps) json) `shouldBe` Right wire
          sizedRoundtrip @GC.JsonTypedMaps wire
          sizedRoundtrip @TH.JsonTypedMaps wire
        entry field key value = Aeson.object [field Aeson..= Aeson.object [key Aeson..= value]]
    it "uses typed integer and boolean keys with enum and bytes values" $ do
      let json = Aeson.object
            [ "enumsById" Aeson..= Aeson.object ["-7" Aeson..= Aeson.String "CHOICE_COLOR_RED"]
            , "enumsByFlag" Aeson..= Aeson.object ["true" Aeson..= Aeson.Number 99]
            , "bytesById" Aeson..= Aeson.object ["-7" Aeson..= Aeson.String "AA=="]
            , "bytesByFlag" Aeson..= Aeson.object ["false" Aeson..= Aeson.String "/w=="]
            ]
      check json json
      wireFor json
        [ 10, 13, 8, 249, 255, 255, 255, 255, 255, 255, 255, 255, 1, 16, 7
        , 18, 4, 8, 1, 16, 99
        , 26, 14, 8, 249, 255, 255, 255, 255, 255, 255, 255, 255, 1, 18, 1, 0
        , 34, 5, 8, 0, 18, 1, 255
        ]
    it "applies the ProtoJSON numeric codecs to map values at the limits" $ do
      let canonical = Aeson.object
            [ "unsignedById" Aeson..= Aeson.object ["-9223372036854775808" Aeson..= Aeson.String "18446744073709551615"]
            , "signedById" Aeson..= Aeson.object ["18446744073709551615" Aeson..= Aeson.String "-9223372036854775808"]
            , "ratioByFlag" Aeson..= Aeson.object ["true" Aeson..= Aeson.String "Infinity"]
            ]
      check canonical canonical
      wireFor canonical
        [ 42, 22, 8, 128, 128, 128, 128, 128, 128, 128, 128, 128, 1, 16, 255, 255, 255, 255, 255, 255, 255, 255, 255, 1
        , 50, 22, 8, 255, 255, 255, 255, 255, 255, 255, 255, 255, 1, 16, 128, 128, 128, 128, 128, 128, 128, 128, 128, 1
        , 58, 11, 8, 1, 17, 0, 0, 0, 0, 0, 0, 240, 127
        ]
      check (entry "signedById" "1" (Aeson.String "1e2")) (entry "signedById" "1" (Aeson.String "100"))
    it "rejects invalid map keys, values and non-object forms" $
      forM_
        [ entry "enumsById" "2147483648" (Aeson.Number 7)
        , entry "enumsByFlag" "TRUE" (Aeson.Number 7)
        , entry "enumsByFlag" "true" Aeson.Null
        , entry "bytesByFlag" "true" (Aeson.String "!")
        , entry "unsignedById" "0" (Aeson.String "-1")
        , Aeson.object ["bytesByFlag" Aeson..= ([] :: [Aeson.Value])]
        ] $ \json -> do
          isLeft (Aeson.parseEither (Aeson.parseJSON @GC.JsonTypedMaps) json) `shouldBe` True
          isLeft (Aeson.parseEither (Aeson.parseJSON @TH.JsonTypedMaps) json) `shouldBe` True
  describe "ProtoJSON field spellings" $ do
    it "accepts original and canonical keys across field shapes and emits canonical names" $ do
      let tags = Aeson.object ["a" Aeson..= (4 :: Int)]
          blobs = Aeson.object ["b" Aeson..= Aeson.String "/w=="]
          fields =
            [ ("original_id", "id", Aeson.String "7")
            , ("byte_chunks", "byteChunks", Aeson.toJSON [Aeson.String "AA=="])
            , ("tags_by_name", "tags", tags)
            , ("open_color", "openColor", Aeson.String "CHOICE_COLOR_RED")
            , ("raw_value", "rawValue", Aeson.Null)
            , ("scalar_bytes", "scalarBytes", Aeson.String "")
            , ("blobs_by_name", "blobs", blobs)
            ]
          original = Aeson.object (map (\(proto, _, value) -> proto Aeson..= value) fields)
          canonical = Aeson.object (map (\(_, json, value) -> json Aeson..= value) fields)
          wire = BS.pack [8, 7, 18, 1, 0, 26, 5, 10, 1, 97, 16, 4, 32, 7, 42, 2, 8, 0, 50, 0, 58, 6, 10, 1, 98, 18, 1, 255]
      jsonParity @GC.JsonAliases @TH.JsonAliases original canonical
      jsonParity @GC.JsonAliases @TH.JsonAliases canonical canonical
      fmap encodeMessage (Aeson.parseEither (Aeson.parseJSON @GC.JsonAliases) original) `shouldBe` Right wire
      fmap encodeMessage (Aeson.parseEither (Aeson.parseJSON @TH.JsonAliases) original) `shouldBe` Right wire
    it "rejects simultaneous spellings, including an otherwise absent null" $
      forM_
        [ Aeson.object ["id" Aeson..= Aeson.Number 1, "original_id" Aeson..= Aeson.Number 2]
        , Aeson.object ["id" Aeson..= Aeson.Null, "original_id" Aeson..= Aeson.Number 2]
        , Aeson.object ["byteChunks" Aeson..= [Aeson.String "AA=="], "byte_chunks" Aeson..= Aeson.Null]
        , Aeson.object ["tags" Aeson..= Aeson.object [], "tags_by_name" Aeson..= Aeson.object []]
        , Aeson.object ["rawValue" Aeson..= Aeson.Null, "raw_value" Aeson..= Aeson.Null]
        ] $ \json -> do
          isLeft (Aeson.parseEither (Aeson.parseJSON @GC.JsonAliases) json) `shouldBe` True
          isLeft (Aeson.parseEither (Aeson.parseJSON @TH.JsonAliases) json) `shouldBe` True
  describe "ProtoJSON null payloads" $ do
    let check = jsonParity @GC.JsonNulls @TH.JsonNulls
        nullField name = Aeson.object [name Aeson..= Aeson.Null]
        empty = Aeson.object []
        wireFor json expected = do
          fmap encodeMessage (Aeson.parseEither (Aeson.parseJSON @GC.JsonNulls) json) `shouldBe` Right (BS.pack expected)
          fmap encodeMessage (Aeson.parseEither (Aeson.parseJSON @TH.JsonNulls) json) `shouldBe` Right (BS.pack expected)
    it "distinguishes an explicit WKT null from an omitted field" $ do
      let json = Aeson.object ["sentinel" Aeson..= Aeson.Null, "dynamic" Aeson..= Aeson.Null]
      check json json
      wireFor json [8, 0, 18, 2, 8, 0]
      check empty empty
      check (Aeson.object ["plain" Aeson..= Aeson.Null, "implicitSentinel" Aeson..= Aeson.Null]) empty
    it "retains null oneof selections and treats numeric NullValue as the open enum" $ do
      let selectedSentinel = nullField "selectedSentinel"
          selectedDynamic = nullField "selectedDynamic"
          numericSentinel = Aeson.object ["sentinel" Aeson..= (55 :: Int)]
      check selectedSentinel selectedSentinel
      wireFor selectedSentinel [56, 0]
      let numericSelection = Aeson.object ["selectedSentinel" Aeson..= (55 :: Int)]
      check numericSelection selectedSentinel
      wireFor numericSelection [56, 55]
      check selectedDynamic selectedDynamic
      wireFor selectedDynamic [66, 2, 8, 0]
      check numericSentinel (nullField "sentinel")
      wireFor numericSentinel [8, 55]
    it "preserves null payloads in arrays and maps with string and integer keys" $ do
      let json = Aeson.object
            [ "sentinels" Aeson..= [Aeson.Null]
            , "dynamics" Aeson..= [Aeson.Null]
            , "sentinelMap" Aeson..= nullField "a"
            , "dynamicMap" Aeson..= nullField "1"
            ]
      check json json
      wireFor json [26, 1, 0, 34, 2, 8, 0, 42, 5, 10, 1, 97, 16, 0, 50, 6, 8, 1, 18, 2, 8, 0]
      check (Aeson.object ["sentinels" Aeson..= Aeson.Null, "dynamicMap" Aeson..= Aeson.Null]) empty
    it "uses native nested objects and arrays for Struct and ListValue" $ do
      let json = Aeson.object
            [ "structured" Aeson..= Aeson.object ["a" Aeson..= [Aeson.Null, Aeson.Number 2]]
            , "listed" Aeson..= [Aeson.Null, Aeson.Bool True]
            ]
      check json json
      wireFor json [82, 24, 10, 22, 10, 1, 97, 18, 17, 50, 15, 10, 2, 8, 0, 10, 9, 17, 0, 0, 0, 0, 0, 0, 0, 64, 90, 8, 10, 2, 8, 0, 10, 2, 32, 1]
    it "rejects invalid NullValue payloads and conflicting null oneof selections" $
      forM_
        [ Aeson.object ["sentinel" Aeson..= Aeson.Bool False]
        , Aeson.object ["selectedSentinel" Aeson..= Aeson.Null, "selectedDynamic" Aeson..= Aeson.Null]
        ] $ \json -> do
          isLeft (Aeson.parseEither (Aeson.parseJSON @GC.JsonNulls) json) `shouldBe` True
          isLeft (Aeson.parseEither (Aeson.parseJSON @TH.JsonNulls) json) `shouldBe` True
  describe "Protobuf multi-byte tags" $ do
    it "both generators encode exact tags and sizes for each field shape" $
      forM_
        [ [128, 1, 1]
        , [138, 1, 0]
        , [130, 128, 1, 1, 97, 130, 128, 1, 1, 98]
        , [138, 128, 1, 1, 255]
        , [144, 1, 7]
        , [154, 1, 2, 8, 1]
        , [162, 1, 2, 8, 1, 162, 1, 2, 8, 2]
        , [168, 1, 7, 168, 1, 42]
        , [178, 1, 2, 1, 2]
        , [186, 1, 5, 10, 1, 97, 16, 1]
        , [193, 1, 1, 0, 0, 0, 0, 0, 0, 0]
        , [202, 1, 2, 8, 1]
        , [208, 1, 7]
        , [218, 1, 1, 255]
        , [250, 255, 255, 255, 15, 1, 97]
        ] $ \bytes -> do
          let wire = BS.pack bytes
          sizedRoundtrip @GC.WideTags wire
          sizedRoundtrip @TH.WideTags wire

  describe "Protobuf repeated-field encoding" $ do
    it "proto3 packs enums and signed fixed-width fields and honors packed=false" $
      forM_
        [ ([128, 1, 7, 130, 1, 1, 42], [130, 1, 2, 7, 42])
        , ([138, 1, 4, 255, 255, 255, 255], [138, 1, 4, 255, 255, 255, 255])
        , ([146, 1, 8] <> replicate 8 255, [146, 1, 8] <> replicate 8 255)
        , ([154, 1, 2, 1, 4], [152, 1, 1, 152, 1, 4])
        , ([162, 1, 2, 7, 42], [160, 1, 7, 160, 1, 42])
        ] $ \(input, expected) -> do
          sizedCanonical @GC.Packing3 (BS.pack input) (BS.pack expected)
          sizedCanonical @TH.Packing3 (BS.pack input) (BS.pack expected)
    it "proto2 defaults to expanded and accepts a packed=true override" $
      forM_
        [ ([130, 1, 2, 1, 2], [128, 1, 1, 128, 1, 2])
        , ([136, 1, 1, 136, 1, 2], [138, 1, 2, 1, 2])
        ] $ \(input, expected) -> do
          sizedCanonical @GL.PackingLegacy (BS.pack input) (BS.pack expected)
          sizedCanonical @TH.PackingLegacy (BS.pack input) (BS.pack expected)
    it "Editions uses file defaults and dotted and aggregate field overrides" $
      forM_ [[128, 1, 1], [138, 1, 1, 1], [154, 1, 4, 146, 1, 1, 1]] $ \bytes -> do
        sizedRoundtrip @G24.Packing24 (BS.pack bytes)
        sizedRoundtrip @TH.Packing24 (BS.pack bytes)
    it "typed extensions follow the same proto2 packing rules" $ do
      let textual = Ext.setRepeatedExtension GL.packingLegacyPackedExtra [1, 2]
            (Ext.setRepeatedExtension GL.packingLegacyExpandedExtra [1, 2] GL.defaultPackingLegacy)
          th = Ext.setRepeatedExtension TH.packingLegacyPackedExtra [1, 2]
            (Ext.setRepeatedExtension TH.packingLegacyExpandedExtra [1, 2] TH.defaultPackingLegacy)
          expected = BS.pack [160, 6, 1, 160, 6, 2, 170, 6, 2, 1, 2]
      encodeMessage textual `shouldBe` expected
      encodeMessage th `shouldBe` expected
      sizedRoundtrip @GL.PackingLegacy expected
      sizedRoundtrip @TH.PackingLegacy expected

  describe "Protobuf extension wire types" $ do
    it "singular accessors ignore incompatible records and retain them across set and clear" $ do
      let wrong = BS.pack [178, 6, 0, 179, 6, 180, 6]
          first = BS.pack [176, 6, 7]
          lastValue = BS.pack [176, 6, 0]
          replacement = BS.pack [176, 6, 11]
          check :: forall msg. (MessageDecode msg, MessageEncode msg, Ext.HasExtensions msg) => Ext.Extension msg Int32 -> IO ()
          check ext = do
            fmap (Ext.hasExtension ext) (decodeMessage wrong) `shouldBe` Right False
            fmap (Ext.getExtension ext) (decodeMessage wrong) `shouldBe` Right Nothing
            fmap (Ext.getExtension ext) (decodeMessage (first <> wrong)) `shouldBe` Right (Just 7)
            msg <- either (fail . show) pure (decodeMessage @msg (first <> lastValue <> wrong))
            Ext.hasExtension ext msg `shouldBe` True
            Ext.getExtension ext msg `shouldBe` Just 0
            encodeMessage (Ext.clearExtension ext msg) `shouldBe` wrong
            encodeMessage (Ext.setExtension ext 11 msg) `shouldBe` (wrong <> replacement)
      check GL.packingLegacySingularExtra
      check TH.packingLegacySingularExtra
    it "repeated accessors handle packed and expanded records without deleting unrelated wire types" $ do
      let wrong = BS.pack [171, 6, 172, 6]
          expanded = BS.pack [168, 6, 7]
          packed = BS.pack [170, 6, 2, 8, 9]
          check :: forall msg. (MessageDecode msg, MessageEncode msg, Ext.HasExtensions msg) => Ext.RepeatedExtension msg Int32 -> IO ()
          check ext = do
            msg <- either (fail . show) pure (decodeMessage @msg (expanded <> wrong <> packed))
            Ext.getRepeatedExtension ext msg `shouldBe` [7, 8, 9]
            Ext.getRepeatedExtensionChecked ext msg `shouldBe` Right [7, 8, 9]
            encodeMessage (Ext.clearRepeatedExtension ext msg) `shouldBe` wrong
            encodeMessage (Ext.setRepeatedExtension ext [8, 9] msg) `shouldBe` (wrong <> packed)
      check GL.packingLegacyPackedExtra
      check TH.packingLegacyPackedExtra
    it "reports malformed packed extension records without yielding partial values" $ do
      let field tag payload = BS.pack ([tag, 6, fromIntegral (length payload)] <> payload)
          check
            :: (MessageDecode msg, MessageEncode msg, Ext.HasExtensions msg, Eq a, Show a)
            => Ext.RepeatedExtension msg a -> BS.ByteString -> BS.ByteString -> DecodeError -> [a] -> IO ()
          check ext good malformed expected values = do
            let wire = good <> malformed <> good
            msg <- either (fail . show) pure (decodeMessage wire)
            Ext.getRepeatedExtension ext msg `shouldBe` (values <> values)
            Ext.getRepeatedExtensionChecked ext msg `shouldBe` Left expected
            encodeMessage msg `shouldBe` wire
          varint = field 170 [128, 1]
          fixed32 = field 194 [1, 2, 3, 4]
          fixed64 = field 202 [1, 2, 3, 4, 5, 6, 7, 8]
          string = field 210 [97]
      forM_ [([1, 128], UnexpectedEnd), (replicate 9 128 <> [2], InvalidVarint)] $ \(bytes, expected) -> do
        check GL.packingLegacyPackedExtra varint (field 170 bytes) expected [128]
        check TH.packingLegacyPackedExtra varint (field 170 bytes) expected [128]
      check GL.packingLegacyFixed32Extra fixed32 (field 194 [1, 2, 3, 4, 5]) UnexpectedEnd [0x04030201]
      check TH.packingLegacyFixed32Extra fixed32 (field 194 [1, 2, 3, 4, 5]) UnexpectedEnd [0x04030201]
      check GL.packingLegacyFixed64Extra fixed64 (field 202 [1, 2, 3, 4, 5, 6, 7, 8, 9]) UnexpectedEnd [0x0807060504030201]
      check TH.packingLegacyFixed64Extra fixed64 (field 202 [1, 2, 3, 4, 5, 6, 7, 8, 9]) UnexpectedEnd [0x0807060504030201]
      check GL.packingLegacyStringExtra string (field 210 [255]) InvalidUtf8 ["a"]
      check TH.packingLegacyStringExtra string (field 210 [255]) InvalidUtf8 ["a"]
    it "message extensions merge valid occurrences and keep their wire boundaries" $ do
      let entry bytes = BS.pack ([186, 6, fromIntegral (length bytes)] <> bytes)
          wrong = BS.pack [184, 6, 9]
          first = [8, 7, 16, 4]
          second = [8, 0]
          check :: (MessageDecode msg, Ext.HasExtensions msg) => Ext.Extension msg BS.ByteString -> IO ()
          check ext = do
            let get bytes = fmap (Ext.getExtension ext) (decodeMessage bytes)
            get (entry first <> wrong <> entry second <> wrong) `shouldBe` Right (Just (BS.pack (first <> second)))
            get (entry [] <> entry []) `shouldBe` Right (Just BS.empty)
            get (entry [10] <> entry [8, 1, 18, 5, 1, 2, 3, 4, 5]) `shouldBe` Right Nothing
      check GL.packingLegacyPayloadExtra
      check TH.packingLegacyPayloadExtra
      sizedCanonical @GL.ExtensionPayload (BS.pack (first <> second)) (BS.pack [8, 0, 16, 4])
      sizedCanonical @TH.ExtensionPayload (BS.pack (first <> second)) (BS.pack [8, 0, 16, 4])

  describe "Proto2 required field presence" $ do
    let wire = BS.pack [8, 0, 16, 0, 24, 0, 34, 0, 40, 0, 50, 0, 130, 1, 0]
        json = Aeson.object
          [ "count" Aeson..= Aeson.Number 0
          , "large" Aeson..= Aeson.String "0"
          , "enabled" Aeson..= Aeson.Bool False
          , "payload" Aeson..= Aeson.String ""
          , "state" Aeson..= Aeson.String "UNSPECIFIED"
          , "child" Aeson..= Aeson.object []
          , "name" Aeson..= Aeson.String ""
          ]
    it "emits zero scalars and enum values and empty bytes, strings, and submessages" $ do
      sizedRoundtrip @GL.RequiredCases wire
      sizedRoundtrip @TH.RequiredCases wire
    it "emits required defaults in ProtoJSON and keeps absent optional bytes absent" $ do
      jsonParity @GL.RequiredCases @TH.RequiredCases json json
      fmap encodeMessage (Aeson.parseEither (Aeson.parseJSON @GL.RequiredCases) json) `shouldBe` Right wire
      fmap encodeMessage (Aeson.parseEither (Aeson.parseJSON @TH.RequiredCases) json) `shouldBe` Right wire
    it "preserves omitted fields, explicit zeros and unknown wire values in partial messages" $ do
      forM_ [BS.empty, BS.pack [8, 0], BS.pack [40, 99, 10, 0]] $ \partial -> do
        sizedRoundtrip @GL.RequiredCases partial
        sizedRoundtrip @TH.RequiredCases partial
      fmap GL.requiredCasesCount (decodeMessage BS.empty) `shouldBe` Right Nothing
      fmap TH.requiredCasesCount (decodeMessage BS.empty) `shouldBe` Right Nothing
      fmap GL.requiredCasesCount (decodeMessage (BS.pack [8, 0])) `shouldBe` Right (Just 0)
      fmap TH.requiredCasesCount (decodeMessage (BS.pack [8, 0])) `shouldBe` Right (Just 0)
    it "keeps null and omitted required fields absent in ProtoJSON" $ do
      let empty = Aeson.object []
          nulls = Aeson.object ["count" Aeson..= Aeson.Null, "state" Aeson..= Aeson.Null, "payload" Aeson..= Aeson.Null]
      jsonParity @GL.RequiredCases @TH.RequiredCases nulls empty
      encodeMessage GL.defaultRequiredCases `shouldBe` BS.empty
      encodeMessage TH.defaultRequiredCases `shouldBe` BS.empty
    it "merges partial embedded messages and lets an explicitly present zero replace a value" $ do
      let input = BS.pack [10, 2, 8, 7, 10, 4, 16, 3, 8, 0]
          expected = BS.pack [10, 4, 8, 0, 16, 3]
          first = BS.pack [8, 7]
          second = BS.pack [8, 0]
      sizedCanonical @GL.RequiredEnvelope input expected
      sizedCanonical @TH.RequiredEnvelope input expected
      fmap (encodeMessage @GL.RequiredCases) ((<>) <$> decodeMessage first <*> decodeMessage second) `shouldBe` Right second
      fmap (encodeMessage @TH.RequiredCases) ((<>) <$> decodeMessage first <*> decodeMessage second) `shouldBe` Right second
  describe "Protobuf closed enum fields" $ do
    let check input expected = do
          sizedCanonical @GL.ClosedCases input expected
          sizedCanonical @TH.ClosedCases input expected
    it "preserves unknown singular and oneof enum values without assigning them" $ do
      let unknown = BS.pack [8, 99, 24, 98]
          input = BS.pack [8, 7, 8, 99, 32, 5, 24, 98]
      fmap GL.closedCasesSingular (decodeMessage unknown) `shouldBe` Right Nothing
      fmap TH.closedCasesSingular (decodeMessage unknown) `shouldBe` Right Nothing
      fmap GL.closedCasesSelection (decodeMessage unknown) `shouldBe` Right Nothing
      fmap TH.closedCasesSelection (decodeMessage unknown) `shouldBe` Right Nothing
      check unknown unknown
      check input (BS.pack [8, 7, 32, 5, 8, 99, 24, 98])
    it "moves unknown values from packed and expanded records into the unknown fields" $ do
      check (BS.pack [18, 4, 0, 99, 7, 98]) (BS.pack [18, 2, 0, 7, 16, 99, 16, 98])
      check (BS.pack [48, 7, 48, 99, 50, 2, 0, 98]) (BS.pack [48, 7, 48, 0, 48, 99, 48, 98])
    it "uses the final enum value inside a map entry and preserves entries whose final value is unknown" $ do
      let entry bytes = BS.pack ([42, fromIntegral (length bytes)] <> bytes)
          key = [10, 1, 97]
          known = entry (key <> [16, 7])
          missing = entry key
          unknown = entry (key <> [16, 99])
          knownLast = entry (key <> [16, 99, 16, 0])
          unknownLast = entry (key <> [16, 7, 16, 99])
          resolved = entry (key <> [16, 0])
      check (unknown <> known <> knownLast <> unknownLast) (resolved <> unknown <> unknownLast)
      check missing resolved
      fmap GL.closedCasesEntries (decodeMessage (unknown <> unknownLast)) `shouldBe` Right Map.empty
      fmap TH.closedCasesEntries (decodeMessage (unknown <> unknownLast)) `shouldBe` Right Map.empty
    it "uses the enum declaration's openness across file imports and Editions overrides" $ do
      let open = BS.pack [56, 99]
          edition = BS.pack [8, 99, 16, 99]
          editionExpected = BS.pack [16, 99, 8, 99]
      fmap GL.closedCasesOpen (decodeMessage open) `shouldBe` Right (Just (GC.ChoiceColor''Unrecognized 99))
      fmap TH.closedCasesOpen (decodeMessage open) `shouldBe` Right (Just (TH.ChoiceColor''Unrecognized 99))
      sizedCanonical @G24.EditionEnums edition editionExpected
      sizedCanonical @TH.EditionEnums edition editionExpected
    it "rejects unrecognized closed enum numbers in ProtoJSON but accepts open enum numbers" $ do
      let value = Aeson.Number 99
          invalid =
            [ Aeson.object ["singular" Aeson..= value]
            , Aeson.object ["variant" Aeson..= value]
            , Aeson.object ["packed" Aeson..= [value]]
            , Aeson.object ["entries" Aeson..= Aeson.object ["a" Aeson..= value]]
            ]
          open = Aeson.object ["open" Aeson..= value]
      forM_ invalid $ \json -> do
        Aeson.parseEither (Aeson.parseJSON @GL.ClosedCases) json `shouldSatisfy` isLeft
        Aeson.parseEither (Aeson.parseJSON @TH.ClosedCases) json `shouldSatisfy` isLeft
      jsonParity @GL.ClosedCases @TH.ClosedCases open open
      jsonParity @GL.ClosedCases @TH.ClosedCases
        (Aeson.object ["singular" Aeson..= Aeson.Null]) (Aeson.object [])
      jsonParity @GL.ClosedCases @TH.ClosedCases
        (Aeson.object ["singular" Aeson..= (7 :: Int)])
        (Aeson.object ["singular" Aeson..= Aeson.String "READY"])

  describe "Protobuf enum extensions" $ do
    it "both generators honor Editions file defaults and per-enum openness overrides" $ do
      Schema.fromProtoEnumValue @G24.ClosedEdition 99 `shouldBe` Nothing
      Schema.fromProtoEnumValue @TH.ClosedEdition 99 `shouldBe` Nothing
      Schema.fromProtoEnumValue @G24.OpenEdition 99 `shouldBe` Just (G24.OpenEdition''Unrecognized 99)
      Schema.fromProtoEnumValue @TH.OpenEdition 99 `shouldBe` Just (TH.OpenEdition''Unrecognized 99)
    it "reads closed enum varints and preserves unrecognized values through mutation" $ do
      let known = BS.pack [216, 6, 7]
          unknown = BS.pack [216, 6, 99]
          negative = BS.pack ([216, 6, 253] <> replicate 8 255 <> [1])
          check
            :: (MessageDecode msg, MessageEncode msg, Ext.HasExtensions msg, Eq a, Show a)
            => Ext.Extension msg a -> a -> a -> IO ()
          check ext ready retired = do
            fmap (Ext.getExtension ext) (decodeMessage negative) `shouldBe` Right (Just retired)
            fmap (Ext.hasExtension ext) (decodeMessage unknown) `shouldBe` Right False
            msg <- either (fail . show) pure (decodeMessage (known <> unknown))
            Ext.getExtension ext msg `shouldBe` Just ready
            encodeMessage msg `shouldBe` (known <> unknown)
            encodeMessage (Ext.clearExtension ext msg) `shouldBe` unknown
            encodeMessage (Ext.setExtension ext retired msg) `shouldBe` (unknown <> negative)
      check GL.packingLegacyStateExtra GL.ExtensionPayload'State'Ready GL.ExtensionPayload'State'Retired
      check TH.packingLegacyStateExtra TH.ExtensionPayload'State'Ready TH.ExtensionPayload'State'Retired
    it "reads both repeated encodings and keeps packed closed enum unknowns on clear" $ do
      let packed = BS.pack [226, 6, 3, 7, 99, 0]
          expanded = BS.pack [224, 6, 100, 224, 6, 7]
          unknowns = BS.pack [224, 6, 99, 224, 6, 100]
          check
            :: (MessageDecode msg, MessageEncode msg, Ext.HasExtensions msg, Eq a, Show a)
            => Ext.RepeatedExtension msg a -> Ext.RepeatedExtension msg a -> a -> a -> IO ()
          check packedExt expandedExt ready unspecified = do
            Ext.reIsPacked packedExt `shouldBe` True
            Ext.reIsPacked expandedExt `shouldBe` False
            msg <- either (fail . show) pure (decodeMessage (packed <> expanded))
            Ext.getRepeatedExtensionChecked packedExt msg `shouldBe` Right [ready, unspecified, ready]
            encodeMessage (Ext.clearRepeatedExtension packedExt msg) `shouldBe` unknowns
            encodeMessage (Ext.setRepeatedExtension packedExt [ready] msg) `shouldBe` (unknowns <> BS.pack [226, 6, 1, 7])
            encodeMessage (Ext.setRepeatedExtension expandedExt [ready] (Ext.clearRepeatedExtension packedExt msg))
              `shouldBe` (unknowns <> BS.pack [232, 6, 7])
      check GL.packingLegacyPackedStateExtra GL.packingLegacyExpandedStateExtra GL.ExtensionPayload'State'Ready GL.ExtensionPayload'State'Unspecified
      check TH.packingLegacyPackedStateExtra TH.packingLegacyExpandedStateExtra TH.ExtensionPayload'State'Ready TH.ExtensionPayload'State'Unspecified
    it "reads unrecognized values from an imported open enum" $ do
      let wire = BS.pack [240, 6, 99]
      fmap (Ext.getExtension GL.packingLegacyColorExtra) (decodeMessage wire) `shouldBe` Right (Just (GC.ChoiceColor''Unrecognized 99))
      fmap (Ext.getExtension TH.packingLegacyColorExtra) (decodeMessage wire) `shouldBe` Right (Just (TH.ChoiceColor''Unrecognized 99))
      sizedRoundtrip @GL.PackingLegacy wire
      sizedRoundtrip @TH.PackingLegacy wire
    it "uses enum names and numeric values in registered ProtoJSON" $ do
      let textualReg = GL.registerExt_state_extra <> GL.registerExt_color_extra <> GL.registerExt_null_extra
          thReg = TH.registerExt_state_extra <> TH.registerExt_color_extra <> TH.registerExt_null_extra
          source = Aeson.object ["[state_extra]" Aeson..= Aeson.String "ALIAS", "[color_extra]" Aeson..= (99 :: Int), "[null_extra]" Aeson..= Aeson.Null]
          canonical = Aeson.object ["[state_extra]" Aeson..= Aeson.String "READY", "[color_extra]" Aeson..= (99 :: Int), "[null_extra]" Aeson..= Aeson.Null]
          unknownClosed = Aeson.object ["[state_extra]" Aeson..= (99 :: Int)]
          textual json = Reflection.give textualReg (Aeson.parseEither (Aeson.parseJSON @GL.PackingLegacy) json)
          th json = Reflection.give thReg (Aeson.parseEither (Aeson.parseJSON @TH.PackingLegacy) json)
      textual unknownClosed `shouldSatisfy` isLeft
      th unknownClosed `shouldSatisfy` isLeft
      textualMsg <- either fail pure (textual source)
      thMsg <- either fail pure (th source)
      Reflection.give textualReg (Aeson.toJSON textualMsg) `shouldBe` canonical
      Reflection.give thReg (Aeson.toJSON thMsg) `shouldBe` canonical
      encodeMessage textualMsg `shouldBe` encodeMessage thMsg
    it "uses the ordinary integer grammar for registered ProtoJSON extensions" $ do
      let textualReg = GL.registerExt_singular_extra
          thReg = TH.registerExt_singular_extra
          input number = Aeson.object ["[singular_extra]" Aeson..= Aeson.String number]
          textual json = Reflection.give textualReg (Aeson.parseEither (Aeson.parseJSON @GL.PackingLegacy) json)
          th json = Reflection.give thReg (Aeson.parseEither (Aeson.parseJSON @TH.PackingLegacy) json)
      textualMsg <- either fail pure (textual (input "-1.25E+2"))
      thMsg <- either fail pure (th (input "-1.25E+2"))
      Ext.getExtension GL.packingLegacySingularExtra textualMsg `shouldBe` Just (-125)
      Ext.getExtension TH.packingLegacySingularExtra thMsg `shouldBe` Just (-125)
      encodeMessage textualMsg `shouldBe` encodeMessage thMsg
      forM_ [" 12", "12 ", "1.25", "2147483648", "1e18446744073709551616"] $ \number -> do
        textual (input number) `shouldSatisfy` isLeft
        th (input number) `shouldSatisfy` isLeft
    it "leaves null extensions unset except for the explicit NullValue sentinel" $ do
      let textualReg = GL.registerExt_singular_extra <> GL.registerExt_state_extra <> GL.registerExt_payload_extra <> GL.registerExt_null_extra
          thReg = TH.registerExt_singular_extra <> TH.registerExt_state_extra <> TH.registerExt_payload_extra <> TH.registerExt_null_extra
          input = Aeson.object
            [ "[singular_extra]" Aeson..= Aeson.Null
            , "[state_extra]" Aeson..= Aeson.Null
            , "[payload_extra]" Aeson..= Aeson.Null
            , "[null_extra]" Aeson..= Aeson.Null
            ]
          expected = Aeson.object ["[null_extra]" Aeson..= Aeson.Null]
      textual <- either fail pure (Reflection.give textualReg (Aeson.parseEither (Aeson.parseJSON @GL.PackingLegacy) input))
      th <- either fail pure (Reflection.give thReg (Aeson.parseEither (Aeson.parseJSON @TH.PackingLegacy) input))
      Reflection.give textualReg (Aeson.toJSON textual) `shouldBe` expected
      Reflection.give thReg (Aeson.toJSON th) `shouldBe` expected
      encodeMessage textual `shouldBe` BS.pack [248, 6, 0]
      encodeMessage th `shouldBe` BS.pack [248, 6, 0]

  describe "Protobuf submessage merging" $ do
    let field tag payload = BS.pack [tag, fromIntegral (BS.length payload)] <> payload
        first = BS.pack [8, 1, 26, 1, 3, 32, 5]
        second = BS.pack [16, 2, 26, 1, 4, 32, 0]
        merged = BS.pack [8, 1, 16, 2, 26, 2, 3, 4, 32, 0]
        key = BS.pack [10, 1, 97]
        check input expected = do
          sizedCanonical @GC.MergeCases input expected
          sizedCanonical @TH.MergeCases input expected
    it "singular submessages merge while preserving explicit defaults and repeated order" $ do
      check (field 10 first <> field 10 second) (field 10 merged)
      check (field 10 first <> field 10 BS.empty) (field 10 first)
      check (field 10 BS.empty) (field 10 BS.empty)
      check (field 10 first <> field 10 (BS.pack [8, 0])) (field 10 (BS.drop 2 first))
    it "the same message oneof arm merges and a different arm resets it" $ do
      check (field 18 first <> field 18 second) (field 18 merged)
      check (field 18 first <> BS.pack [24, 0] <> field 18 second) (field 18 second)
    it "values within a map entry merge and distinct entries keep the last value" $ do
      check (field 34 (key <> field 18 first <> field 18 second)) (field 34 (key <> field 18 merged))
      let secondEntry = field 34 (key <> field 18 second)
      check (field 34 (key <> field 18 first) <> secondEntry) secondEntry
    it "a malformed later submessage or map entry reports the bounded decode error" $
      forM_ [field 10 first <> BS.pack [10, 1, 8], BS.pack [34, 2, 10, 2]] $ \wire -> do
        decodeMessage @GC.MergeCases wire `shouldBe` Left (SubMessageError UnexpectedEnd)
        decodeMessage @TH.MergeCases wire `shouldBe` Left (SubMessageError UnexpectedEnd)

  describe "Protobuf mismatched wire types" $ do
    it "preserves mismatched singular and optional fields as unknown" $ do
      let input = BS.pack [10, 1, 99, 8, 7]
          expected = BS.pack [8, 7, 10, 1, 99]
          optional = BS.pack [34, 0]
      sizedCanonical @GC.MergePart input expected
      sizedCanonical @TH.MergePart input expected
      sizedRoundtrip @GC.MergePart optional
      sizedRoundtrip @TH.MergePart optional
    it "preserves mismatched messages, maps and oneofs without resetting the active arm" $
      forM_ [[8, 0], [18, 2, 8, 1, 16, 0, 29, 1, 0, 0, 0, 32, 0]] $ \bytes -> do
        sizedRoundtrip @GC.MergeCases (BS.pack bytes)
        sizedRoundtrip @TH.MergeCases (BS.pack bytes)
    it "preserves mismatched repeated fields and legacy NullValue fields" $ do
      let packed = BS.pack [133, 1, 1, 2, 3, 4, 136, 1, 1]
          repeated = BS.pack [128, 128, 1, 0, 160, 1, 0]
          nullValue = BS.pack [53, 1, 2, 3, 4]
      sizedRoundtrip @GC.Packing3 packed
      sizedRoundtrip @TH.Packing3 packed
      sizedRoundtrip @GC.WideTags repeated
      sizedRoundtrip @TH.WideTags repeated
      sizedRoundtrip @GC.JsonChoices nullValue
      sizedRoundtrip @TH.JsonChoices nullValue
    it "skips mismatched key and value records inside scalar and message map entries" $ do
      let messageMap = BS.pack [34, 11, 8, 99, 16, 0, 10, 1, 97, 18, 2, 8, 1]
          messageExpected = BS.pack [34, 7, 10, 1, 97, 18, 2, 8, 1]
          scalarMap = BS.pack [186, 1, 10, 8, 0, 18, 1, 99, 10, 1, 97, 16, 7]
          scalarExpected = BS.pack [186, 1, 5, 10, 1, 97, 16, 7]
      sizedCanonical @GC.MergeCases messageMap messageExpected
      sizedCanonical @TH.MergeCases messageMap messageExpected
      sizedCanonical @GC.WideTags scalarMap scalarExpected
      sizedCanonical @TH.WideTags scalarMap scalarExpected
    it "rejects truncated payloads even when the wire type is mismatched" $ do
      let wire = BS.pack [10, 2, 0]
      decodeMessage @GC.MergePart wire `shouldBe` Left UnexpectedEnd
      decodeMessage @TH.MergePart wire `shouldBe` Left UnexpectedEnd

  describe "Protobuf legacy groups" $ do
    let child = BS.pack [10, 2, 8, 1]
        group = BS.pack [11, 16, 7, 19, 26, 1, 97, 20, 12]
    it "preserves unknown groups, including nested groups and groups on known field numbers" $ do
      let extra = BS.pack [147, 3, 148, 3]
          input = group <> child <> extra
          expected = child <> group <> extra
      sizedCanonical @GC.MergeCases input expected
      sizedCanonical @TH.MergeCases input expected
    it "handles deeply nested groups without recursively decoding their bodies" $ do
      let wire = BS.replicate 10000 11 <> BS.replicate 10000 12
      sizedRoundtrip @GC.MergeCases wire
      sizedRoundtrip @TH.MergeCases wire
    it "rejects stray, mismatched and truncated group boundaries" $
      forM_
        [ ([12], InvalidWireType 4)
        , ([11, 20], InvalidTag 20)
        , ([11, 19, 12, 20], InvalidTag 12)
        , ([11, 16, 7], UnexpectedEnd)
        , ([11, 18, 3, 0], UnexpectedEnd)
        ] $ \(bytes, expected) -> do
          let wire = BS.pack bytes
          decodeMessage @GC.MergeCases wire `shouldBe` Left expected
          decodeMessage @TH.MergeCases wire `shouldBe` Left expected
    it "skips well-formed groups inside map entries and checks their closing field number" $ do
      let wire = BS.pack [34, 15, 11, 16, 0, 12, 19, 8, 5, 20, 10, 1, 97, 18, 2, 8, 1]
          expected = BS.pack [34, 7, 10, 1, 97, 18, 2, 8, 1]
          malformed = BS.pack [34, 2, 11, 20]
      sizedCanonical @GC.MergeCases wire expected
      sizedCanonical @TH.MergeCases wire expected
      decodeMessage @GC.MergeCases malformed `shouldBe` Left (SubMessageError (InvalidTag 20))
      decodeMessage @TH.MergeCases malformed `shouldBe` Left (SubMessageError (InvalidTag 20))

  describe "Protobuf malformed packed varints" $ do
    let overflow = replicate 9 128 <> [2, 1]
        frame bytes = BS.pack (fromIntegral (length bytes) : bytes)
        invalid =
          [ ([1, 128], UnexpectedEnd)
          , ([128, 128, 128], UnexpectedEnd)
          , (overflow, InvalidVarint)
          , (replicate 12 1 <> overflow, InvalidVarint)
          ]
        rejects :: DecodeError -> Either DecodeError a -> IO ()
        rejects expected = \case
          Left actual -> actual `shouldBe` expected
          Right _ -> expectationFailure "accepted malformed packed varints"
    it "propagates truncation and overflow from every packed varint decoder" $
      forM_ invalid $ \(bytes, expected) -> do
        let wire = frame bytes
        rejects expected (runDecoder decodePackedVarint wire)
        rejects expected (runDecoder decodePackedSVarint32 wire)
        rejects expected (runDecoder decodePackedSVarint64 wire)
    it "both generators reject malformed packed scalar, enum and ZigZag fields" $
      forM_ invalid $ \(bytes, expected) -> do
        let payload = frame bytes
            varint = BS.cons 26 payload
            enum = BS.pack [130, 1] <> payload
            zigzag = BS.pack [154, 1] <> payload
        rejects expected (decodeMessage @GC.MergePart varint)
        rejects expected (decodeMessage @TH.MergePart varint)
        rejects expected (decodeMessage @GC.Packing3 enum)
        rejects expected (decodeMessage @TH.Packing3 enum)
        rejects expected (decodeMessage @GC.Packing3 zigzag)
        rejects expected (decodeMessage @TH.Packing3 zigzag)

  describe "Protobuf length and varint bounds" $ do
    let huge = BS.pack (replicate 8 255 <> [127])
        negative = BS.pack (replicate 9 128 <> [1])
        overflowed = BS.pack (replicate 9 128 <> [2])
    it "oversized slices and skipped lengths are rejected before advancing" $
      forM_ [(huge, UnexpectedEnd), (negative, NegativeLength), (overflowed, InvalidVarint)] $ \(prefix, expected) -> do
        runDecoder getLengthDelimited prefix `shouldBe` Left expected
        runDecoder (skipField WireLengthDelimited) prefix `shouldBe` Left expected
        runDecoder (skipWireType 2) prefix `shouldBe` Left expected
        let wire = BS.cons 10 prefix
        decodeMessage @GC.MergeCases wire `shouldBe` Left expected
        decodeMessage @TH.MergeCases wire `shouldBe` Left expected
    it "ten-byte varints accept the uint64 maximum and reject overflow when read or skipped" $ do
      runDecoder getVarint (BS.pack (replicate 9 255 <> [1])) `shouldBe` Right maxBound
      runDecoder getVarint overflowed `shouldBe` Left InvalidVarint
      runDecoder (skipField WireVarint) overflowed `shouldBe` Left InvalidVarint
    it "lazy, incremental and pull-based frames reject lengths that overflow" $
      forM_ [(negative, NegativeLength, "negative message length"), (overflowed, InvalidVarint, "varint overflow")] $
        \(prefix, expected, expectedMessage) -> do
          let (firstChunk, secondChunk) = BS.splitAt 5 prefix
          Stream.decodeMessageStream @GC.MergeCases (BL.fromStrict prefix) `shouldBe` [Left expected]
          case Stream.feedChunk (Stream.feedChunk (Stream.decodeMessageIncremental @GC.MergeCases) firstChunk) secondChunk of
            Stream.IFail err _ -> err `shouldBe` expected
            _ -> expectationFailure "incremental decoder accepted a malformed frame length"
          case Stream.feedMore (Stream.streamDecode @GC.MergeCases firstChunk) secondChunk of
            Stream.Fail err -> err `shouldBe` expectedMessage
            _ -> expectationFailure "pull-based decoder accepted a malformed frame length"
    it "all tag readers reject field zero, oversized fields and reserved wire types" $
      forM_
        [ ([0, 0], 0)
        , ([2, 0], 2)
        , ([14, 0], 14)
        , ([127, 0], 127)
        , ([128, 128, 128, 128, 16, 0], 4294967296)
        ] $ \(bytes, rawTag) -> do
          let wire = BS.pack bytes
              expected = InvalidTag rawTag
              cpsTag = Wire.Decoder $ \bs off ->
                withTag bs off
                  (\end -> (# (# (), end #) | #))
                  (\_ _ end -> (# (# (), end #) | #))
                  (\err -> (# | err #))
          runDecoder getTag wire `shouldBe` Left expected
          runDecoder cpsTag wire `shouldBe` Left expected
          decodeMessage @GC.MergeCases wire `shouldBe` Left expected
          decodeMessage @TH.MergeCases wire `shouldBe` Left expected
    it "accepts padded tags up to five bytes and rejects wider tags" $ do
      let payload = BS.pack [0xd2, 9]
          paddedTag width = BS.pack (0x88 : replicate (width - 2) 0x80 <> [0])
          proto = Proxy @GC.ChoicePayload
          table = Dynamic.compileParseTable proto
          emptyTable = Dynamic.compileParseTable (Proxy @Empty)
          cpsTag = Wire.Decoder $ \bs off ->
            withTag bs off
              (\end -> (# (# (), end #) | #))
              (\_ _ end -> (# (# (), end #) | #))
              (\err -> (# | err #))
          wide = paddedTag 5 <> payload
          canonical = BS.cons 8 payload
      fmap encodeMessage (decodeMessage @GC.ChoicePayload wide) `shouldBe` Right canonical
      fmap encodeMessage (decodeMessage @TH.ChoicePayload wide) `shouldBe` Right canonical
      Collect.collectIssues proto wide `shouldBe` []
      Dynamic.decodeDynamic wide `shouldBe` Dynamic.decodeDynamic canonical
      Dynamic.decodeDynamicWithSchema table wide `shouldBe` Dynamic.decodeDynamicWithSchema table canonical
      Dynamic.decodeDynamicWithSchema emptyTable wide `shouldBe` Dynamic.decodeDynamicWithSchema emptyTable canonical
      forM_ [6 .. 10] $ \width -> do
        let wire = paddedTag width <> payload
        runDecoder getTag wire `shouldBe` Left InvalidVarint
        runDecoder cpsTag wire `shouldBe` Left InvalidVarint
        runDecoder (withTagM (pure ()) (\_ _ -> pure ())) wire `shouldBe` Left InvalidVarint
        decodeMessage @GC.ChoicePayload wire `shouldBe` Left InvalidVarint
        decodeMessage @TH.ChoicePayload wire `shouldBe` Left InvalidVarint
        Collect.collectIssues proto wire `shouldBe` [Collect.DecodeIssue [] InvalidVarint]
        Dynamic.decodeDynamic wire `shouldBe` Left InvalidVarint
        Dynamic.decodeDynamicWithSchema table wire `shouldBe` Left InvalidVarint
        Dynamic.decodeDynamicWithSchema emptyTable wire `shouldBe` Left InvalidVarint

  describe "Protobuf dynamic wire errors" $ do
    let check
          :: forall textual th
           . (Schema.ProtoMessage textual, MessageDecode textual, Schema.ProtoMessage th, MessageDecode th)
          => BS.ByteString -> DecodeError -> IO ()
        check wire expected = do
          let textual = Proxy @textual
              th = Proxy @th
          fmap (const ()) (decodeMessage @textual wire) `shouldBe` Left expected
          fmap (const ()) (decodeMessage @th wire) `shouldBe` Left expected
          Dynamic.decodeDynamicWithSchema (Dynamic.compileParseTable textual) wire `shouldBe` Left expected
          Dynamic.decodeDynamicWithSchema (Dynamic.compileParseTable th) wire `shouldBe` Left expected
          map Collect.issueError (Collect.collectIssues textual wire) `shouldBe` [expected]
          map Collect.issueError (Collect.collectIssues th wire) `shouldBe` [expected]
        checkRaw wire expected = do
          Dynamic.decodeDynamic wire `shouldBe` Left expected
          Dynamic.decodeDynamicWithSchema (Dynamic.compileParseTable (Proxy @Empty)) wire `shouldBe` Left expected
    it "reports truncated known values and preserves submessage error context" $ do
      forM_ [[8, 128], [37, 1, 2, 3], [49, 1, 2, 3, 4, 5, 6, 7]] $ \bytes -> do
        let wire = BS.pack bytes
        check @GC.JsonScalars @TH.JsonScalars wire UnexpectedEnd
        checkRaw wire UnexpectedEnd
      check @GC.WideTags @TH.WideTags (BS.pack [138, 1, 1, 255]) InvalidUtf8
      check @GC.MergeCases @TH.MergeCases (BS.pack [10, 2, 8, 128]) (SubMessageError UnexpectedEnd)
    it "rejects truncated, overflowing and backwards lengths when skipping unknown fields" $
      forM_
        [ (BS.pack [16, 128], UnexpectedEnd)
        , (BS.pack [18, 3, 97], UnexpectedEnd)
        , (BS.pack (18 : replicate 9 128 <> [1]), NegativeLength)
        , (BS.pack (18 : replicate 9 128 <> [2]), InvalidVarint)
        , (BS.pack (16 : replicate 9 128 <> [2]), InvalidVarint)
        ] $ \(wire, expected) -> do
          check @GC.ChoicePayload @TH.ChoicePayload wire expected
          checkRaw wire expected
    it "validates group endings and reaches fields after a well-formed group" $ do
      let malformed = BS.pack [19, 8, 7, 28]
          complete = BS.pack [19, 8, 7, 20, 8, 42]
          table = Dynamic.compileParseTable (Proxy @GC.ChoicePayload)
          emptyTable = Dynamic.compileParseTable (Proxy @Empty)
      check @GC.ChoicePayload @TH.ChoicePayload malformed (InvalidTag 28)
      checkRaw malformed (InvalidTag 28)
      fmap GC.choicePayloadCount (decodeMessage @GC.ChoicePayload complete) `shouldBe` Right 42
      fmap TH.choicePayloadCount (decodeMessage @TH.ChoicePayload complete) `shouldBe` Right 42
      Collect.collectIssues (Proxy @GC.ChoicePayload) complete `shouldBe` []
      fmap (Dynamic.dynamicField 1) (Dynamic.decodeDynamicWithSchema table complete)
        `shouldBe` Right (Just (Dynamic.DynVarint 42))
      fmap (Dynamic.dynamicField 1) (Dynamic.decodeDynamicWithSchema emptyTable complete)
        `shouldBe` Right (Just (Dynamic.DynVarint 42))

  describe "Protobuf dynamic wire retention" $ do
    it "supports positional and record construction and record updates" $ do
      let original = Dynamic.DynamicMessage
            { Dynamic.dynFields = Map.singleton 1 (Dynamic.DynVarint 1)
            , Dynamic.dynUnknownFields = []
            }
          updated = original {Dynamic.dynFields = Map.singleton 1 (Dynamic.DynVarint 2)}
      original `shouldBe` Dynamic.DynamicMessage (Map.singleton 1 (Dynamic.DynVarint 1)) []
      Dynamic.encodeDynamic updated `shouldBe` BS.pack [8, 2]
    it "keeps interleaved occurrences and nested groups with or without an empty parse table" $ do
      let firstGroup = BS.pack [35, 8, 0, 36, 156, 0]
          groups = BS.singleton 27 <> firstGroup <> BS.pack [27, 28]
          wire = BS.pack [8, 1, 18, 1, 97, 8, 2, 18, 1, 98, 8, 3] <> groups
          reencoded = BS.pack [8, 1, 8, 2, 8, 3, 18, 1, 97, 18, 1, 98] <> groups
          expected = Dynamic.DynamicMessage
            (Map.fromList
              [ (1, Dynamic.DynRepeated (map Dynamic.DynVarint [1, 2, 3]))
              , (2, Dynamic.DynRepeated (map Dynamic.DynBytes ["a", "b"]))
              ])
            [(3, WireStartGroup, firstGroup), (3, WireStartGroup, BS.singleton 28)]
      Dynamic.decodeDynamic wire `shouldBe` Right expected
      Dynamic.decodeDynamicWithSchema (Dynamic.compileParseTable (Proxy @Empty)) wire `shouldBe` Right expected
      Dynamic.encodeDynamic expected `shouldBe` reencoded
      Dynamic.decodeDynamic reencoded `shouldBe` Right expected
    it "retains all unknown wire types and mismatched tags under both generated schemas" $ do
      let unknown =
            [ (2, WireVarint, BS.pack [129, 0])
            , (2, WireLengthDelimited, BS.pack [129, 0, 255])
            , (1, Wire32Bit, BS.pack [1, 2, 3, 4])
            , (2, Wire64Bit, BS.pack [5, 6, 7, 8, 9, 10, 11, 12])
            , (2, WireStartGroup, BS.pack [27, 28, 148, 0])
            ]
          wire = BS.pack
            [8, 42, 16, 129, 0, 18, 129, 0, 255, 13, 1, 2, 3, 4, 17, 5, 6, 7, 8, 9, 10, 11, 12, 19, 27, 28, 148, 0]
          expected = Dynamic.DynamicMessage (Map.singleton 1 (Dynamic.DynVarint 42)) unknown
          verify :: forall a. (Schema.ProtoMessage a, MessageDecode a, MessageEncode a) => Proxy a -> IO BS.ByteString
          verify proxy = do
            typed <- either (fail . show) pure (decodeMessage @a wire)
            dynamic <- either (fail . show) pure (Dynamic.decodeDynamicWithSchema (Dynamic.compileParseTable proxy) wire)
            dynamic `shouldBe` expected
            Dynamic.encodeDynamic dynamic `shouldBe` wire
            fmap encodeMessage (decodeMessage @a (Dynamic.encodeDynamic dynamic)) `shouldBe` Right (encodeMessage typed)
            pure (encodeMessage typed)
      textual <- verify (Proxy @GC.ChoicePayload)
      th <- verify (Proxy @TH.ChoicePayload)
      textual `shouldBe` th
    it "preserves nested unknown groups inside a declared submessage" $ do
      let wire = BS.pack [10, 5, 8, 7, 43, 172, 0]
          child = Dynamic.DynamicMessage (Map.singleton 1 (Dynamic.DynVarint 7)) [(5, WireStartGroup, BS.pack [172, 0])]
          expected = Dynamic.setDynamicField 1 (Dynamic.DynMessage child) Dynamic.emptyDynamic
          verify :: Schema.ProtoMessage a => Proxy a -> IO ()
          verify proxy =
            Dynamic.decodeDynamicWithSchema (Dynamic.compileParseTable proxy) wire `shouldBe` Right expected
      verify (Proxy @GC.MergeCases)
      verify (Proxy @TH.MergeCases)
      Dynamic.encodeDynamic expected `shouldBe` wire
    it "merges singular embedded records and keeps repeated embedded records separate" $ do
      let first = BS.pack [8, 1, 24, 7, 43, 44]
          second = BS.pack [16, 2, 24, 8, 32, 0]
          child field body = BS.pack [field, fromIntegral (BS.length body)] <> body
          wire = child 10 first <> child 50 first <> child 10 second <> child 50 second
          mergedBody = BS.pack [8, 1, 16, 2, 24, 7, 24, 8, 32, 0, 43, 44]
          mergedWire = child 10 mergedBody <> child 50 first <> child 50 second
          malformed = BS.pack [10, 1, 8, 10, 1, 1]
          verify :: forall a. (Schema.ProtoMessage a, MessageDecode a, MessageEncode a) => Proxy a -> IO BS.ByteString
          verify proxy = do
            let table = Dynamic.compileParseTable proxy
            typed <- either (fail . show) pure (decodeMessage @a wire)
            dynamic <- either (fail . show) pure (Dynamic.decodeDynamicWithSchema table wire)
            merged <- either (fail . show) pure (Dynamic.decodeDynamic (first <> second))
            firstChild <- either (fail . show) pure (Dynamic.decodeDynamic first)
            secondChild <- either (fail . show) pure (Dynamic.decodeDynamic second)
            Dynamic.dynamicField 1 dynamic `shouldBe` Just (Dynamic.DynMessage merged)
            Dynamic.dynamicField 6 dynamic `shouldBe`
              Just (Dynamic.DynRepeated [Dynamic.DynMessage firstChild, Dynamic.DynMessage secondChild])
            Dynamic.encodeDynamic dynamic `shouldBe` mergedWire
            fmap encodeMessage (decodeMessage @a mergedWire) `shouldBe` Right (encodeMessage typed)
            Dynamic.decodeDynamicWithSchema table malformed `shouldBe` Left (SubMessageError UnexpectedEnd)
            pure (encodeMessage typed)
      textual <- verify (Proxy @GC.MergeCases)
      th <- verify (Proxy @TH.MergeCases)
      textual `shouldBe` th
    it "preserves a nested oneof winner across split records and unrelated edits" $ do
      let bytesArm = BS.pack [18, 1, 97]
          integerArm = BS.pack [8, 1]
          initialLabel = BS.pack [58, 1, 120]
          editedLabel = BS.pack [58, 1, 121]
          child body = BS.pack [42, fromIntegral (BS.length body)] <> body
          full = child (bytesArm <> initialLabel <> integerArm)
          split = child (bytesArm <> initialLabel) <> child integerArm
          editedWire = child (integerArm <> editedLabel)
          removedWire = child integerArm
          expected = child (integerArm <> initialLabel)
          verify :: forall a. (Schema.ProtoMessage a, MessageDecode a, MessageEncode a) => Proxy a -> IO ()
          verify proxy = do
            let table = Dynamic.compileParseTable proxy
            forM_ [full, split] $ \wire -> do
              fmap encodeMessage (decodeMessage @a wire) `shouldBe` Right expected
              dynamic <- either (fail . show) pure (Dynamic.decodeDynamicWithSchema table wire)
              Dynamic.encodeDynamic dynamic `shouldBe` expected
              case Dynamic.dynamicField 5 dynamic of
                Just (Dynamic.DynMessage nested) -> do
                  Dynamic.dynamicField 1 nested `shouldBe` Just (Dynamic.DynVarint 1)
                  Dynamic.dynamicField 2 nested `shouldBe` Nothing
                  Dynamic.dynamicField 7 nested `shouldBe` Just (Dynamic.DynString "x")
                  let encodeNested value = Dynamic.encodeDynamic (Dynamic.setDynamicField 5 (Dynamic.DynMessage value) dynamic)
                  encodeNested (Dynamic.setDynamicField 7 (Dynamic.DynString "y") nested) `shouldBe` editedWire
                  encodeNested (Dynamic.removeDynamicField 7 nested) `shouldBe` removedWire
                actual -> expectationFailure ("expected an embedded message, got " <> show actual)
      verify (Proxy @GC.MergeCases)
      verify (Proxy @TH.MergeCases)
      fmap Dynamic.encodeDynamic (Dynamic.decodeDynamic (bytesArm <> integerArm)) `shouldBe` Right (bytesArm <> integerArm)
    it "decodes imported child scalars with their own field types" $ do
      let wire = BS.pack [10, 2, 8, 2]
          canonical = BS.pack [10, 2, 8, 1]
          child = Dynamic.setDynamicField 1 (Dynamic.DynBool True) Dynamic.emptyDynamic
          expected = Dynamic.setDynamicField 1 (Dynamic.DynMessage child) Dynamic.emptyDynamic
          verify :: forall a. (Schema.ProtoMessage a, MessageDecode a, MessageEncode a, MessageSize a) => Proxy a -> IO ()
          verify proxy = do
            let table = Dynamic.compileParseTable proxy
            Dynamic.decodeDynamicWithSchema table wire `shouldBe` Right expected
            fmap Dynamic.encodeDynamic (Dynamic.decodeDynamicWithSchema table wire) `shouldBe` Right canonical
            sizedCanonical @a wire canonical
      verify (Proxy @GC.JsonWrappers)
      verify (Proxy @TH.JsonWrappers)
    it "validates a malformed child even if its oneof arm is replaced" $ do
      let wire = BS.pack [58, 3, 58, 1, 255, 24, 1]
          expected = SubMessageError InvalidUtf8
          verify :: forall a. (Schema.ProtoMessage a, MessageDecode a, Show a, Eq a) => Proxy a -> IO ()
          verify proxy = do
            decodeMessage @a wire `shouldBe` Left expected
            Dynamic.decodeDynamicWithSchema (Dynamic.compileParseTable proxy) wire `shouldBe` Left expected
      verify (Proxy @GC.MergeCases)
      verify (Proxy @TH.MergeCases)
    it "resolves recursive child tables only when their fields are encountered" $ do
      let child fn body = BS.pack [fn, fromIntegral (BS.length body)] <> body
          group fn body = BS.singleton fn <> body <> BS.singleton (fn + 1)
          envelope = group 75 (group 11 (group 11 (group 11 (BS.pack [8, 7]))))
          wrapped = child 26 (BS.pack [8, 7])
          field fn value = Dynamic.setDynamicField fn value Dynamic.emptyDynamic
          groupValue fn = Dynamic.DynGroup . field fn
          expected = field 9 (groupValue 1 (groupValue 1 (groupValue 1 (groupValue 1 (Dynamic.DynVarint 7)))))
          verify :: forall a. (Schema.ProtoMessage a, MessageDecode a, MessageEncode a, MessageSize a) => Proxy a -> IO ()
          verify proxy = do
            let table = Dynamic.compileParseTable proxy
            Dynamic.decodeDynamicWithSchema table envelope `shouldBe` Right expected
            Dynamic.decodeDynamicWithSchema table wrapped `shouldBe`
              Right (field 3 (Dynamic.DynMessage (field 1 (Dynamic.DynVarint 7))))
            sizedCanonical @a envelope envelope
            sizedCanonical @a wrapped wrapped
      verify (Proxy @GD.EditionEnvelope)
      verify (Proxy @TH.EditionEnvelope)

  describe "Protobuf dynamic oneofs and closed enums" $ do
    let check
          :: forall textual th
           . (Schema.ProtoMessage textual, MessageDecode textual, MessageEncode textual,
              Schema.ProtoMessage th, MessageDecode th, MessageEncode th)
          => BS.ByteString -> Map.Map Int Dynamic.DynamicValue -> [(Int, WireType, BS.ByteString)] -> IO ()
        check wire fields unknown = do
          let textualTable = Dynamic.compileParseTable (Proxy @textual)
              thTable = Dynamic.compileParseTable (Proxy @th)
          Schema.protoOneofDescriptors (Proxy @textual) `shouldBe` Schema.protoOneofDescriptors (Proxy @th)
          Schema.protoClosedEnumFields (Proxy @textual) `shouldBe` Schema.protoClosedEnumFields (Proxy @th)
          let describeField (Schema.SomeField field) =
                (Schema.fdName field, Schema.fdNumber field, Schema.fdTypeDesc field, Schema.fdLabel field)
          fmap describeField (Schema.protoFieldDescriptors (Proxy @textual))
            `shouldBe` fmap describeField (Schema.protoFieldDescriptors (Proxy @th))
          V.all ((> 0) . Dynamic.fpFieldNum) (Dynamic.ptFields textualTable) `shouldBe` True
          V.all ((> 0) . Dynamic.fpFieldNum) (Dynamic.ptFields thTable) `shouldBe` True
          typed <- either (fail . show) pure (decodeMessage @textual wire)
          th <- either (fail . show) pure (decodeMessage @th wire)
          let canonical = encodeMessage typed
          encodeMessage th `shouldBe` canonical
          dynamic <- either (fail . show) pure (Dynamic.decodeDynamicWithSchema textualTable wire)
          Dynamic.dynFields dynamic `shouldBe` fields
          Dynamic.dynUnknownFields dynamic `shouldBe` unknown
          Dynamic.decodeDynamicWithSchema thTable wire `shouldBe` Right dynamic
          let encoded = Dynamic.encodeDynamic dynamic
          fmap encodeMessage (decodeMessage @textual encoded) `shouldBe` Right canonical
          fmap encodeMessage (decodeMessage @th encoded) `shouldBe` Right canonical
    it "clears a message arm on reset and merges only consecutive occurrences of that arm" $ do
      let nested fields = Dynamic.DynMessage (Dynamic.DynamicMessage (Map.fromList fields) [])
      check @GC.MergeCases @TH.MergeCases
        (BS.pack [18, 2, 8, 1, 24, 5, 18, 2, 16, 2])
        (Map.singleton 2 (nested [(2, Dynamic.DynVarint 2)])) []
      check @GC.MergeCases @TH.MergeCases
        (BS.pack [18, 2, 8, 1, 18, 2, 16, 2])
        (Map.singleton 2 (nested [(1, Dynamic.DynVarint 1), (2, Dynamic.DynVarint 2)])) []
      check @GC.MergeCases @TH.MergeCases
        (BS.pack [18, 2, 8, 1, 24, 0])
        (Map.singleton 3 (Dynamic.DynVarint 0)) []
    it "tracks independent groups and preserves explicit zero and mismatched wire records" $ do
      check @GC.JsonChoices @TH.JsonChoices
        (BS.pack [8, 7, 58, 1, 120, 29, 0, 0, 128, 63])
        (Map.fromList [(3, Dynamic.DynFloat 1), (7, Dynamic.DynString "x")]) []
      check @GC.JsonChoices @TH.JsonChoices
        (BS.pack [29, 0, 0, 128, 63, 8, 0])
        (Map.singleton 1 (Dynamic.DynVarint 0)) []
      check @GC.JsonChoices @TH.JsonChoices
        (BS.pack [29, 0, 0, 128, 63, 10, 0])
        (Map.singleton 3 (Dynamic.DynFloat 1)) [(1, WireLengthDelimited, BS.singleton 0)]
    it "keeps the active arm for unknown closed enum values and switches for open or recognized values" $ do
      check @GL.ClosedCases @TH.ClosedCases
        (BS.pack [32, 5, 24, 99])
        (Map.singleton 4 (Dynamic.DynVarint 5)) [(3, WireVarint, BS.singleton 99)]
      check @GL.ClosedCases @TH.ClosedCases
        (BS.pack [32, 5, 24, 253, 255, 255, 255, 15])
        (Map.singleton 3 (Dynamic.DynEnum (-3))) []
      check @GC.JsonChoices @TH.JsonChoices
        (BS.pack [8, 7, 32, 99])
        (Map.singleton 4 (Dynamic.DynEnum 99)) []
    it "uses the enum declaration's openness for ordinary fields" $ do
      Map.keys (Schema.protoClosedEnumFields (Proxy @GL.ClosedCases)) `shouldBe` [1, 2, 5, 6]
      check @GL.ClosedCases @TH.ClosedCases
        (BS.pack [8, 7, 8, 99, 56, 99])
        (Map.fromList [(1, Dynamic.DynEnum 7), (7, Dynamic.DynEnum 99)])
        [(1, WireVarint, BS.singleton 99)]
      check @G24.EditionEnums @TH.EditionEnums
        (BS.pack [8, 99, 16, 99])
        (Map.singleton 2 (Dynamic.DynEnum 99)) [(1, WireVarint, BS.singleton 99)]
    it "moves unrecognized packed and expanded enum values into unknown fields in wire order" $ do
      let negative = BS.pack [252, 255, 255, 255, 15]
          wire = BS.pack [16, 99, 18, 7, 0] <> negative <> BS.pack [7, 48, 7, 48, 99, 50, 2, 0, 98]
          expected = Map.fromList
            [ (2, Dynamic.DynRepeated [Dynamic.DynEnum 0, Dynamic.DynEnum 7])
            , (6, Dynamic.DynRepeated [Dynamic.DynEnum 7, Dynamic.DynEnum 0])
            ]
      check @GL.ClosedCases @TH.ClosedCases wire expected
        [(2, WireVarint, BS.singleton 99), (2, WireVarint, negative),
         (6, WireVarint, BS.singleton 99), (6, WireVarint, BS.singleton 98)]
      check @GL.ClosedCases @TH.ClosedCases
        (BS.pack [8, 99, 18, 1, 99]) Map.empty
        [(1, WireVarint, BS.singleton 99), (2, WireVarint, BS.singleton 99)]
    it "preserves map entries whose final closed-enum value is unknown" $ do
      let known = BS.pack [10, 1, 97, 16, 99, 16, 7]
          unknown = BS.pack [7, 10, 1, 97, 16, 7, 16, 99]
          wire = BS.pack [42, 7] <> known <> BS.singleton 42 <> unknown
      check @GL.ClosedCases @TH.ClosedCases wire
        (Map.singleton 5 (Dynamic.DynBytes known)) [(5, WireLengthDelimited, unknown)]
    it "keeps empty length-delimited records unknown outside a well-known NullValue oneof" $ do
      check @GC.UserNullChoice @TH.UserNullChoice
        (BS.pack [16, 7, 10, 0])
        (Map.singleton 2 (Dynamic.DynVarint 7)) [(1, WireLengthDelimited, BS.singleton 0)]
      check @GC.UserNullChoice @TH.UserNullChoice
        (BS.pack [16, 7, 8, 1])
        (Map.singleton 1 (Dynamic.DynEnum 1)) []
      check @GC.JsonNulls @TH.JsonNulls
        (BS.pack [10, 0]) Map.empty [(1, WireLengthDelimited, BS.singleton 0)]
    it "decodes declared groups and preserves the DELIMITED wire encoding" $ do
      let nested ctor fields = ctor (Dynamic.DynamicMessage (Map.fromList (map (fmap Dynamic.DynVarint) fields)) [])
          group = nested Dynamic.DynGroup
          message = nested Dynamic.DynMessage
      check @GG.GroupEnvelope @TH.GroupEnvelope (BS.pack [11, 8, 1, 12])
        (Map.singleton 1 (group [(1, 1)])) []
      check @GD.EditionEnvelope @TH.EditionEnvelope
        (BS.pack [19, 8, 1, 20, 26, 2, 16, 2, 35, 8, 3, 36, 35, 36])
        (Map.fromList
          [(2, group [(1, 1)]), (3, message [(2, 2)]),
           (4, Dynamic.DynRepeated [group [(1, 3)], group []])]) []
      check @GD.EditionEnvelope @TH.EditionEnvelope
        (BS.pack [59, 8, 1, 60, 59, 16, 2, 60])
        (Map.singleton 7 (group [(1, 1), (2, 2)])) []
      check @GD.EditionEnvelope @TH.EditionEnvelope
        (BS.pack [59, 8, 1, 60, 64, 9, 59, 16, 2, 60])
        (Map.singleton 7 (group [(2, 2)])) []
      check @GD.EditionEnvelope @TH.EditionEnvelope
        (BS.pack [64, 9, 58, 2, 8, 1])
        (Map.singleton 8 (Dynamic.DynVarint 9)) [(7, WireLengthDelimited, BS.pack [2, 8, 1])]
      let textual = Dynamic.compileParseTable (Proxy @GD.EditionEnvelope)
          th = Dynamic.compileParseTable (Proxy @TH.EditionEnvelope)
      forM_ [([59, 68], InvalidTag 68), ([59, 8, 7], UnexpectedEnd)] $ \(bytes, err) -> do
        let wire = BS.pack bytes
        fmap (const ()) (decodeMessage @GD.EditionEnvelope wire) `shouldBe` Left err
        fmap (const ()) (decodeMessage @TH.EditionEnvelope wire) `shouldBe` Left err
        Dynamic.decodeDynamicWithSchema textual wire `shouldBe` Left err
        Dynamic.decodeDynamicWithSchema th wire `shouldBe` Left err

  describe "Protobuf collected decode diagnostics" $ do
    let check
          :: forall textual th
           . (Schema.ProtoMessage textual, MessageDecode textual, Schema.ProtoMessage th, MessageDecode th)
          => BS.ByteString -> [Collect.DecodeIssue] -> IO ()
        check input expected = do
          let result
                :: forall a. (Schema.ProtoMessage a, MessageDecode a)
                => Proxy a -> ([Collect.DecodeIssue], Bool)
              result _ =
                let (issues, value) = Collect.decodeCollecting @a input
                in (issues, maybe False (const True) value)
          result (Proxy @textual) `shouldBe` (expected, null expected)
          result (Proxy @th) `shouldBe` (expected, null expected)
    it "uses the actual oneof member types and wire names" $ do
      check @GC.JsonChoices @TH.JsonChoices (BS.pack [58, 1, 120, 18, 1, 255]) []
      check @GC.JsonChoices @TH.JsonChoices (BS.pack [10, 1, 0]) []
      check @GC.JsonChoices @TH.JsonChoices (BS.pack [58, 1, 255, 42, 1, 8])
        [Collect.DecodeIssue ["label"] InvalidUtf8, Collect.DecodeIssue ["child"] (SubMessageError UnexpectedEnd)]
    it "indexes only records with the declared wire type for repeated strings" $
      check @GC.WideTags @TH.WideTags (BS.pack [128, 128, 1, 0, 130, 128, 1, 1, 255])
        [Collect.DecodeIssue ["names[0]"] InvalidUtf8]

  describe "Protobuf dynamic maps" $ do
    let entries = Map.fromList
          [ (Dynamic.DynString "a", Dynamic.DynVarint 7)
          , (Dynamic.DynString "b", Dynamic.DynVarint 8)
          ]
        message = Dynamic.setDynamicField 1 (Dynamic.DynMap entries) Dynamic.emptyDynamic
        wire = BS.pack [10, 5, 10, 1, 97, 16, 7, 10, 5, 10, 1, 98, 16, 8]
        values = Map.fromList [("a", 7), ("b", 8)]
        checkError
          :: forall textual th
           . (Schema.ProtoMessage textual, MessageDecode textual, Schema.ProtoMessage th, MessageDecode th)
          => BS.ByteString -> DecodeError -> IO ()
        checkError input expected = do
          fmap (const ()) (decodeMessage @textual input) `shouldBe` Left expected
          fmap (const ()) (decodeMessage @th input) `shouldBe` Left expected
          Dynamic.decodeDynamicWithSchema (Dynamic.compileParseTable (Proxy @textual)) input `shouldBe` Left expected
          Dynamic.decodeDynamicWithSchema (Dynamic.compileParseTable (Proxy @th)) input `shouldBe` Left expected
        checkWire
          :: forall textual th
           . ( Schema.ProtoMessage textual, MessageDecode textual, MessageEncode textual
             , Schema.ProtoMessage th, MessageDecode th, MessageEncode th )
          => BS.ByteString -> IO ()
        checkWire input = do
          canonical <- either (fail . show) (pure . encodeMessage) (decodeMessage @textual input)
          fmap encodeMessage (decodeMessage @th input) `shouldBe` Right canonical
          let textual = Dynamic.decodeDynamicWithSchema (Dynamic.compileParseTable (Proxy @textual)) input
              th = Dynamic.decodeDynamicWithSchema (Dynamic.compileParseTable (Proxy @th)) input
          th `shouldBe` textual
          fmap Dynamic.encodeDynamic textual `shouldBe` Right input
    it "rejects malformed scalar entries with the generated decoder's bounded errors" $ do
      forM_
        [ ([10, 2, 10, 3], SubMessageError UnexpectedEnd)
        , ([10, 3, 10, 1, 255], SubMessageError InvalidUtf8)
        , ([10, 2, 16, 128], SubMessageError UnexpectedEnd)
        , ([10, 2, 11, 20], SubMessageError (InvalidTag 20))
        , ([98, 3, 18, 1, 255], SubMessageError InvalidUtf8)
        , ([10, 2, 10], UnexpectedEnd)
        ] $ \(bytes, expected) ->
          checkError @GM.IntegerMaps @TH.IntegerMaps (BS.pack bytes) expected
    it "validates enum-map keys even when the final value is unrecognized" $ do
      let input = BS.pack [42, 5, 10, 1, 255, 16, 99]
          expected = SubMessageError InvalidUtf8
      checkError @GL.ClosedCases @TH.ClosedCases input expected
      checkError @GC.JsonEnumMode @TH.JsonEnumMode input expected
    it "validates typed map message bodies before a later value can replace them" $ do
      checkError @GC.JsonEnumMode @TH.JsonEnumMode
        (BS.pack [66, 7, 18, 3, 10, 1, 255, 18, 0])
        (SubMessageError (SubMessageError InvalidUtf8))
      checkError @GM.MessageMap @TH.MessageMap
        (BS.pack [10, 3, 18, 1, 8])
        (SubMessageError (SubMessageError UnexpectedEnd))
      Dynamic.decodeDynamicWithSchema (Dynamic.compileParseTable (Proxy @Struct.Struct))
        (BS.pack [10, 5, 18, 3, 26, 1, 255])
        `shouldBe` Left (SubMessageError (SubMessageError InvalidUtf8))
    it "keeps valid raw entries, including duplicate and mismatched members" $ do
      checkWire @GM.IntegerMaps @TH.IntegerMaps
        (wire <> BS.pack [10, 9, 10, 1, 97, 11, 12, 16, 7, 16, 9])
      checkWire @GM.IntegerMaps @TH.IntegerMaps
        (BS.pack [98, 3, 10, 1, 255])
      checkWire @GC.MergeCases @TH.MergeCases
        (BS.pack [34, 11, 8, 99, 16, 0, 10, 1, 97, 18, 2, 8, 1])
    it "writes each map entry as an embedded message that both generated decoders read" $ do
      let encoded = Dynamic.encodeDynamic message
      encoded `shouldBe` wire
      fmap GM.integerMapsInt32Values (decodeMessage encoded) `shouldBe` Right values
      fmap TH.integerMapsInt32Values (decodeMessage encoded) `shouldBe` Right values
      Dynamic.encodeDynamic (Dynamic.setDynamicField 1 (Dynamic.DynMap Map.empty) Dynamic.emptyDynamic)
        `shouldBe` BS.empty
    it "preserves map keys and values in field-number JSON" $ do
      let insert number key value =
            Dynamic.setDynamicField number (Dynamic.DynMap (Map.singleton key value))
          jsonMessage =
            insert 2 (Dynamic.DynBool False) (Dynamic.DynString "boolean")
            $ insert 3 (Dynamic.DynSVarint minBound) (Dynamic.DynString "signed")
            $ insert 4 (Dynamic.DynVarint maxBound) (Dynamic.DynString "unsigned")
            $ insert 5 (Dynamic.DynFixed32 32) (Dynamic.DynString "fixed32")
            $ insert 6 (Dynamic.DynFixed64 64) (Dynamic.DynString "fixed64") message
          one key value = Aeson.object [key Aeson..= (value :: T.Text)]
      Dynamic.dynamicToJson jsonMessage `shouldBe` Aeson.object
        [ "1" Aeson..= values
        , "2" Aeson..= one "false" "boolean"
        , "3" Aeson..= one "-9223372036854775808" "signed"
        , "4" Aeson..= one "18446744073709551615" "unsigned"
        , "5" Aeson..= one "32" "fixed32"
        , "6" Aeson..= one "64" "fixed64"
        ]
    it "renders every entry in numeric TextFormat" $ do
      let compact = TextFormat.dynamicToText message
          pretty = TextFormat.dynamicToTextPretty message
      compact `shouldBe` "1 { 1: \"a\" 2: 7 } 1 { 1: \"b\" 2: 8 } "
      pretty `shouldBe` "1 {\n  1: \"a\"\n  2: 7\n}\n1 {\n  1: \"b\"\n  2: 8\n}\n"
      forM_ [compact, pretty] $ \rendered ->
        fmap Dynamic.encodeDynamic (TextFormat.textToDynamic rendered) `shouldBe` Right wire
    it "uses the protobuf JSON spellings for nonfinite map values" $ do
      let ratios = Dynamic.DynMap (Map.fromList
            [ (Dynamic.DynBool False, Dynamic.DynDouble ((-1) / 0))
            , (Dynamic.DynBool True, Dynamic.DynDouble (1 / 0))
            ])
          withRatios = Dynamic.setDynamicField 7 ratios Dynamic.emptyDynamic
          messageWithNaN = Dynamic.setDynamicField 1 (Dynamic.DynFloat (0 / 0)) withRatios
          ratioJSON = Aeson.object ["false" Aeson..= Aeson.String "-Infinity", "true" Aeson..= Aeson.String "Infinity"]
          wireWithRatios = Dynamic.encodeDynamic withRatios
      Dynamic.dynamicToJson messageWithNaN `shouldBe` Aeson.object ["1" Aeson..= Aeson.String "NaN", "7" Aeson..= ratioJSON]
      fmap Aeson.toJSON (decodeMessage @GC.JsonTypedMaps wireWithRatios)
        `shouldBe` Right (Aeson.object ["ratioByFlag" Aeson..= ratioJSON])
      fmap Aeson.toJSON (decodeMessage @TH.JsonTypedMaps wireWithRatios)
        `shouldBe` Right (Aeson.object ["ratioByFlag" Aeson..= ratioJSON])
    it "emits matching map descriptors and retains every wire entry in both dynamic parse tables" $ do
      let describeField (Schema.SomeField field) = (Schema.fdTypeDesc field, Schema.fdLabel field)
          checkDescriptor :: Schema.ProtoMessage a => Proxy a -> Int -> Schema.FieldTypeDescriptor -> IO ()
          checkDescriptor proxy number expected =
            fmap describeField (Schema.fieldDescriptorByNumber number proxy)
              `shouldBe` Just (Schema.MapType Schema.StringField expected, Schema.LabelRepeated)
          check :: Schema.ProtoMessage a => Proxy a -> IO ()
          check proxy = do
            checkDescriptor proxy 1 (Schema.ScalarType Schema.Int32Field)
            fmap Dynamic.encodeDynamic (Dynamic.decodeDynamicWithSchema (Dynamic.compileParseTable proxy) wire)
              `shouldBe` Right wire
      check (Proxy @GM.IntegerMaps)
      check (Proxy @TH.IntegerMaps)
      checkDescriptor (Proxy @GL.ClosedCases) 5 (Schema.EnumType "ExtensionPayload.State")
      checkDescriptor (Proxy @TH.ClosedCases) 5 (Schema.EnumType "ExtensionPayload.State")
      checkDescriptor (Proxy @Struct.Struct) 1 (Schema.MessageType "Value")
      let malformed = BS.pack [10, 2, 10, 3]
          issues = Collect.collectIssues (Proxy @GM.IntegerMaps) malformed
      issues `shouldSatisfy` (not . null)
      Collect.collectIssues (Proxy @TH.IntegerMaps) malformed `shouldBe` issues

  describe "Protobuf dynamic packed fields" $ do
    let check
          :: forall textual th
           . (Schema.ProtoMessage textual, MessageDecode textual, MessageEncode textual,
              Schema.ProtoMessage th, MessageDecode th, MessageEncode th)
          => BS.ByteString -> Map.Map Int Dynamic.DynamicValue -> IO ()
        check wire expected = do
          textual <- either (fail . show) pure (decodeMessage @textual wire)
          th <- either (fail . show) pure (decodeMessage @th wire)
          let canonical = encodeMessage textual
              textualTable = Dynamic.compileParseTable (Proxy @textual)
              thTable = Dynamic.compileParseTable (Proxy @th)
          encodeMessage th `shouldBe` canonical
          dynamic <- either (fail . show) pure (Dynamic.decodeDynamicWithSchema textualTable wire)
          Dynamic.dynFields dynamic `shouldBe` expected
          Dynamic.decodeDynamicWithSchema thTable wire `shouldBe` Right dynamic
          let encoded = Dynamic.encodeDynamic dynamic
          fmap encodeMessage (decodeMessage @textual encoded) `shouldBe` Right canonical
          fmap encodeMessage (decodeMessage @th encoded) `shouldBe` Right canonical
    it "decodes enum varints as signed 32-bit values using both generated schemas" $ do
      let negative = BS.pack [253, 255, 255, 255, 15]
          wire = BS.singleton 8 <> negative <> BS.pack [18, 6] <> negative <> BS.pack [7, 16] <> negative
          expected = Map.fromList
            [ (1, Dynamic.DynEnum (-3))
            , (2, Dynamic.DynRepeated [Dynamic.DynEnum (-3), Dynamic.DynEnum 7, Dynamic.DynEnum (-3)])
            ]
      check @GL.ClosedCases @TH.ClosedCases wire expected
    it "preserves encounter order across packed and expanded numeric records" $ do
      let wire = BS.pack
            ( [8, 1, 10, 0, 10, 3, 128, 1, 2, 8, 3]
                <> [18, 16, 0, 0, 0, 0, 0, 0, 240, 63, 0, 0, 0, 0, 0, 0, 0, 64]
                <> [24, 1, 26, 11, 4] <> replicate 9 255 <> [1]
                <> [34, 8, 0, 0, 0, 63, 0, 0, 128, 63]
                <> [42, 8, 7, 0, 0, 0, 8, 0, 0, 0]
                <> [50, 2, 1, 0, 48, 1]
            )
          expected = Map.fromList
            [ (1, Dynamic.DynRepeated (map Dynamic.DynVarint [1, 128, 2, 3]))
            , (2, Dynamic.DynRepeated (map Dynamic.DynDouble [1, 2]))
            , (3, Dynamic.DynRepeated (map Dynamic.DynSVarint [-1, 2, minBound]))
            , (4, Dynamic.DynRepeated (map Dynamic.DynFloat [0.5, 1]))
            , (5, Dynamic.DynRepeated (map Dynamic.DynFixed32 [7, 8]))
            , (6, Dynamic.DynRepeated (map Dynamic.DynBool [True, False, True]))
            ]
      check @GC.JsonRepeatedNumbers @TH.JsonRepeatedNumbers wire expected
    it "accepts packed enum, signed fixed and ZigZag fields at multi-byte tags" $ do
      let wire = BS.pack
            ( [128, 1, 7, 130, 1, 1, 42]
                <> [138, 1, 4, 255, 255, 255, 255]
                <> [146, 1, 8] <> replicate 8 255
                <> [154, 1, 7, 1, 4, 255, 255, 255, 255, 15]
                <> [162, 1, 2, 7, 42]
            )
          expected = Map.fromList
            [ (16, Dynamic.DynRepeated (map Dynamic.DynEnum [7, 42]))
            , (17, Dynamic.DynFixed32 maxBound)
            , (18, Dynamic.DynFixed64 maxBound)
            , (19, Dynamic.DynRepeated (map Dynamic.DynSVarint [-1, 2, -2147483648]))
            , (20, Dynamic.DynRepeated (map Dynamic.DynEnum [7, 42]))
            ]
      check @GC.Packing3 @TH.Packing3 wire expected
    it "treats empty and singleton runs alike in either spelling" $ do
      check @GC.JsonRepeatedNumbers @TH.JsonRepeatedNumbers (BS.pack [10, 0]) Map.empty
      forM_ [[10, 0, 8, 42], [10, 1, 42, 10, 0]] $ \bytes ->
        check @GC.JsonRepeatedNumbers @TH.JsonRepeatedNumbers (BS.pack bytes) (Map.singleton 1 (Dynamic.DynVarint 42))
    it "encodes a singular signed dynamic value using ZigZag" $
      check @GC.JsonScalars @TH.JsonScalars (BS.pack [24, 5]) (Map.singleton 3 (Dynamic.DynSVarint (-3)))
    it "reports the generated readers' packed-payload errors" $
      forM_
        [ (BS.pack [10, 2, 1, 128], UnexpectedEnd)
        , (BS.pack (10 : 10 : replicate 9 128 <> [2]), InvalidVarint)
        , (BS.pack [34, 1, 0], CustomError "packed float: byte length not multiple of 4")
        , (BS.pack [18, 1, 0], CustomError "packed double: byte length not multiple of 8")
        ] $ \(wire, expected) -> do
          fmap (const ()) (decodeMessage @GC.JsonRepeatedNumbers wire) `shouldBe` Left expected
          fmap (const ()) (decodeMessage @TH.JsonRepeatedNumbers wire) `shouldBe` Left expected
          Dynamic.decodeDynamicWithSchema (Dynamic.compileParseTable (Proxy @GC.JsonRepeatedNumbers)) wire
            `shouldBe` Left expected
          Dynamic.decodeDynamicWithSchema (Dynamic.compileParseTable (Proxy @TH.JsonRepeatedNumbers)) wire
            `shouldBe` Left expected

  describe "ProtoJSON oneof parity" $ do
    it "both generators flatten variants and use the canonical payload encoding" $ do
      let signedZero = Aeson.object ["signed" Aeson..= Aeson.String "0", "label" Aeson..= Aeson.String ""]
          blob = Aeson.object ["bytePayload" Aeson..= Aeson.String "/w=="]
          fraction = Aeson.object ["fraction" Aeson..= Aeson.String "-Infinity"]
          color = Aeson.object ["color" Aeson..= Aeson.String "CHOICE_COLOR_RED"]
          child = Aeson.object ["child" Aeson..= Aeson.object ["count" Aeson..= (1 :: Int)]]
          nil = Aeson.object ["nil" Aeson..= Aeson.Null]
      forM_ [signedZero, blob, fraction, color, child, nil] $ \json ->
        jsonParity @GC.JsonChoices @TH.JsonChoices json json
      let bytes = BS.pack [48, 0]
      fmap encodeMessage (Aeson.parseEither (Aeson.parseJSON @GC.JsonChoices) nil) `shouldBe` Right bytes
      fmap encodeMessage (Aeson.parseEither (Aeson.parseJSON @TH.JsonChoices) nil) `shouldBe` Right bytes
      roundtrip @GC.JsonChoices bytes
      roundtrip @TH.JsonChoices bytes
    it "the old empty-submessage NullValue still decodes and re-encodes as a varint" $ do
      let check
            :: forall a. (Schema.ProtoMessage a, MessageDecode a, MessageEncode a)
            => Proxy a -> BS.ByteString -> BS.ByteString -> BS.ByteString -> IO ()
          check proxy previous tag canonical = do
            let table = Dynamic.compileParseTable proxy
                old = previous <> tag <> BS.singleton 0
            fmap (encodeMessage @a) (decodeMessage old) `shouldBe` Right canonical
            fmap Dynamic.encodeDynamic (Dynamic.decodeDynamicWithSchema table old) `shouldBe` Right canonical
            forM_ [([1, 0], ExtraBytes), ([2, 0], UnexpectedEnd)] $ \(payload, err) -> do
              let malformed = previous <> tag <> BS.pack payload
              fmap (encodeMessage @a) (decodeMessage malformed) `shouldBe` Left err
              Dynamic.decodeDynamicWithSchema table malformed `shouldBe` Left err
          previous = BS.pack [8, 1]
          nilTag = BS.singleton 50
          canonical = BS.pack [48, 0]
      check (Proxy @GC.JsonChoices) previous nilTag canonical
      check (Proxy @TH.JsonChoices) previous nilTag canonical
      check (Proxy @Struct.Value) (BS.pack [26, 1, 120]) (BS.singleton 10) (BS.pack [8, 0])
      roundtrip @Struct.Value (BS.pack [8, 7])
    it "enum variants retain declared and unknown wire numbers across both generators" $ do
      forM_ [BS.pack [32, 7], BS.pack [32, 42], BS.pack [48, 7]] $ \wire -> do
        roundtrip @GC.JsonChoices wire
        roundtrip @TH.JsonChoices wire
      let red = Aeson.object ["color" Aeson..= Aeson.String "CHOICE_COLOR_RED"]
      jsonParity @GC.JsonChoices @TH.JsonChoices
        (Aeson.object ["color" Aeson..= Aeson.String "CHOICE_COLOR_CRIMSON"]) red
      jsonParity @GC.JsonChoices @TH.JsonChoices (Aeson.object ["color" Aeson..= Aeson.Number 7]) red
      let unknown = Aeson.object ["color" Aeson..= Aeson.Number 42]
      jsonParity @GC.JsonChoices @TH.JsonChoices unknown unknown
      let retired = Aeson.object ["color" Aeson..= Aeson.String "CHOICE_COLOR_RETIRED"]
      jsonParity @GC.JsonChoices @TH.JsonChoices (Aeson.object ["color" Aeson..= Aeson.Number (-3)]) retired
    it "both generators accept proto spellings and ignore ordinary null variants" $ do
      jsonParity @GC.JsonChoices @TH.JsonChoices
        (Aeson.object ["signed_value" Aeson..= Aeson.String "1e2", "byte_payload" Aeson..= Aeson.Null])
        (Aeson.object ["signed" Aeson..= Aeson.String "100"])
      jsonParity @GC.JsonChoices @TH.JsonChoices
        (Aeson.object ["byte_payload" Aeson..= Aeson.String "/w=="])
        (Aeson.object ["bytePayload" Aeson..= Aeson.String "/w=="])
      jsonParity @GC.JsonChoices @TH.JsonChoices
        (Aeson.object ["signed" Aeson..= Aeson.Null]) (Aeson.object [])
    it "both generators reject conflicting selections, duplicate spellings and malformed payloads" $
      forM_
        [ Aeson.object ["signed" Aeson..= Aeson.Number 1, "fraction" Aeson..= Aeson.Number 2]
        , Aeson.object ["signed" Aeson..= Aeson.Number 1, "signed_value" Aeson..= Aeson.Number 2]
        , Aeson.object ["signed" Aeson..= Aeson.Null, "signed_value" Aeson..= Aeson.Number 2]
        , Aeson.object ["nil" Aeson..= Aeson.Null, "signed" Aeson..= Aeson.Number 1]
        , Aeson.object ["nil" Aeson..= Aeson.Number 2147483648]
        , Aeson.object ["color" Aeson..= Aeson.Number 1.5]
        , Aeson.object ["color" Aeson..= Aeson.Number 2147483648]
        , Aeson.object ["bytePayload" Aeson..= Aeson.String "!"]
        , Aeson.object ["signed" Aeson..= Aeson.String "9223372036854775808"]
        ] $ \json -> do
          isLeft (Aeson.parseEither (Aeson.parseJSON @GC.JsonChoices) json) `shouldBe` True
          isLeft (Aeson.parseEither (Aeson.parseJSON @TH.JsonChoices) json) `shouldBe` True

  describe "ProtoJSON numeric scalar parity" $ do
    it "both generators accept quoted exponents and emit canonical 64-bit strings" $ do
      let input =
            Aeson.object
              [ "signedValue" Aeson..= Aeson.String "-9223372036854775808"
              , "unsignedValue" Aeson..= Aeson.String "18446744073709551615"
              , "small" Aeson..= Aeson.String "-1e2"
              , "optionalSmall" Aeson..= Aeson.String "0"
              , "fraction" Aeson..= Aeson.String "1.25e1"
              , "ratio" Aeson..= Aeson.String "2.5E+1"
              ]
          expected =
            Aeson.object
              [ "signedValue" Aeson..= Aeson.String "-9223372036854775808"
              , "unsignedValue" Aeson..= Aeson.String "18446744073709551615"
              , "small" Aeson..= Aeson.Number (-100)
              , "optionalSmall" Aeson..= Aeson.Number 0
              , "fraction" Aeson..= Aeson.Number 12.5
              , "ratio" Aeson..= Aeson.Number 25
              ]
      jsonParity @GC.JsonScalars @TH.JsonScalars input expected
      jsonParity @GC.JsonScalars @TH.JsonScalars
        (Aeson.object ["signedValue" Aeson..= Aeson.Number 9007199254740993])
        (Aeson.object ["signedValue" Aeson..= Aeson.String "9007199254740993"])
    it "checks quoted exponents before converting to a protobuf number" $ do
      let overflow = "18446744073709551616"
          padded = "12" <> T.replicate 100000 "0"
          integer number = Aeson.object ["signedValue" Aeson..= Aeson.String number]
          parseTextual = Aeson.parseEither (Aeson.parseJSON @GC.JsonScalars)
          parseTH = Aeson.parseEither (Aeson.parseJSON @TH.JsonScalars)
          invalid =
            [ integer ("1e" <> overflow)
            , integer ("1e-" <> overflow)
            , integer (padded <> "1e-100001")
            , Aeson.object ["ratio" Aeson..= Aeson.String ("1e" <> overflow)]
            ]
      forM_ invalid $ \input -> do
        parseTextual input `shouldSatisfy` isLeft
        parseTH input `shouldSatisfy` isLeft
      jsonParity @GC.JsonScalars @TH.JsonScalars
        (integer (padded <> "e-100000")) (integer "12")
      jsonParity @GC.JsonScalars @TH.JsonScalars
        (Aeson.object ["optionalSmall" Aeson..= Aeson.String ("0e" <> overflow)])
        (Aeson.object ["optionalSmall" Aeson..= Aeson.Number 0])
    it "rounds quoted floats and preserves an explicit negative zero on the wire" $ do
      let smallest = encodeFloat 1 (-1074) :: Double
          smallestInput = Aeson.object ["ratio" Aeson..= Aeson.String "5e-324"]
      fmap GC.jsonScalarsRatio (Aeson.parseEither Aeson.parseJSON smallestInput) `shouldBe` Right smallest
      fmap TH.jsonScalarsRatio (Aeson.parseEither Aeson.parseJSON smallestInput) `shouldBe` Right smallest
      forM_ ["-0", "-1e-18446744073709551616"] $ \number -> do
        let input = Aeson.object ["fraction" Aeson..= Aeson.String number]
            expected = BS.pack [29, 0, 0, 0, 128]
        fmap encodeMessage (Aeson.parseEither (Aeson.parseJSON @GC.JsonChoices) input) `shouldBe` Right expected
        fmap encodeMessage (Aeson.parseEither (Aeson.parseJSON @TH.JsonChoices) input) `shouldBe` Right expected
    it "checks unquoted exponents before Aeson turns the input into a Value" $ do
      let parseTextual = JSON.decodeProtoJSON @GC.JsonChoices
          parseTH = JSON.decodeProtoJSON @TH.JsonChoices
      forM_ ["{\"fraction\":1e18446744073709551616}", "{\"fraction\":1e-18446744073709551616}", "{\"fraction\":1.1e-9223372036854775807}"] $ \input -> do
        parseTextual input `shouldSatisfy` isLeft
        parseTH input `shouldSatisfy` isLeft
      let valid = "{\"label\":\"escaped \\\"1e18446744073709551616\\\"\",\"fraction\":1.25E+2}"
          expected = Aeson.object ["label" Aeson..= Aeson.String "escaped \"1e18446744073709551616\"", "fraction" Aeson..= Aeson.Number 125]
      fmap Aeson.toJSON (parseTextual valid) `shouldBe` Right expected
      fmap Aeson.toJSON (parseTH valid) `shouldBe` Right expected
    it "both generators preserve Float and Double sentinels" $
      forM_ ["NaN", "Infinity", "-Infinity"] $ \sentinel -> do
        let json = Aeson.object ["fraction" Aeson..= Aeson.String sentinel, "ratio" Aeson..= Aeson.String sentinel]
        jsonParity @GC.JsonScalars @TH.JsonScalars json json
    it "the shared Float encoder emits the exact sentinel strings" $ do
      JSON.protoFloatToJSON (0 / 0) `shouldBe` Aeson.String "NaN"
      JSON.protoFloatToJSON (1 / 0) `shouldBe` Aeson.String "Infinity"
      JSON.protoFloatToJSON (-1 / 0) `shouldBe` Aeson.String "-Infinity"
    it "both generators omit implicit zero and null but retain explicit zero" $
      jsonParity @GC.JsonScalars @TH.JsonScalars
        (Aeson.object ["signedValue" Aeson..= Aeson.Number 0, "fraction" Aeson..= Aeson.Null, "optionalSmall" Aeson..= Aeson.Number 0])
        (Aeson.object ["optionalSmall" Aeson..= Aeson.Number 0])
    it "both generators reject out-of-range and non-integral numeric scalars" $
      forM_ [("signedValue", "9223372036854775808"), ("unsignedValue", "-1"), ("small", "0.5"), ("optionalSmall", "4294967296"), ("fraction", "1e40")] $ \(key, number) -> do
        let json = Aeson.object [key Aeson..= Aeson.String number]
        isLeft (Aeson.parseEither (Aeson.parseJSON @GC.JsonScalars) json) `shouldBe` True
        isLeft (Aeson.parseEither (Aeson.parseJSON @TH.JsonScalars) json) `shouldBe` True


jsonParity
  :: forall textual th. (Aeson.FromJSON textual, Aeson.ToJSON textual, MessageEncode textual,
                         Aeson.FromJSON th, Aeson.ToJSON th, MessageEncode th)
  => Aeson.Value -> Aeson.Value -> IO ()
jsonParity input expected = do
  textual <- either fail pure (Aeson.parseEither (Aeson.parseJSON @textual) input)
  th <- either fail pure (Aeson.parseEither (Aeson.parseJSON @th) input)
  Aeson.toJSON textual `shouldBe` expected
  Aeson.toJSON th `shouldBe` expected
  encodeMessage textual `shouldBe` encodeMessage th


scalarDefaults :: [BS.ByteString]
scalarDefaults = fmap BS.pack [[8, 0], [18, 0], [26, 0], [32, 0], [40, 0], [49, 0, 0, 0, 0, 0, 0, 0, 0], [61, 0, 0, 0, 0]]


collectionValues :: [BS.ByteString]
collectionValues = fmap BS.pack [[74, 2, 1, 2], [82, 2, 8, 0], [90, 6, 10, 1, 97, 18, 1, 98], [96, 0], [106, 0]]


roundtrip :: forall a. (MessageDecode a, MessageEncode a) => BS.ByteString -> IO ()
roundtrip wire = fmap (encodeMessage @a) (decodeMessage wire) `shouldBe` Right wire


sizedRoundtrip :: forall a. (MessageDecode a, MessageEncode a, MessageSize a) => BS.ByteString -> IO ()
sizedRoundtrip wire = sizedCanonical @a wire wire


sizedCanonical :: forall a. (MessageDecode a, MessageEncode a, MessageSize a) => BS.ByteString -> BS.ByteString -> IO ()
sizedCanonical input expected = do
  decoded <- either (fail . show) pure (decodeMessage @a input)
  encodeMessage decoded `shouldBe` expected
  messageSize decoded `shouldBe` BS.length expected
  encodeMessageSized decoded `shouldBe` expected


integerMapValues :: [GM.IntegerMaps]
integerMapValues =
  [ GM.defaultIntegerMaps {GM.integerMapsInt32Values = Map.fromList [("a", minBound), ("b", 0), ("c", maxBound)]}
  , GM.defaultIntegerMaps {GM.integerMapsInt64Values = Map.fromList [("a", minBound), ("b", 0), ("c", maxBound)]}
  , GM.defaultIntegerMaps {GM.integerMapsUint32Values = Map.fromList [("a", 0), ("b", maxBound)]}
  , GM.defaultIntegerMaps {GM.integerMapsUint64Values = Map.fromList [("a", 0), ("b", maxBound)]}
  , GM.defaultIntegerMaps {GM.integerMapsSint32Values = Map.fromList [("a", minBound), ("b", 0), ("c", maxBound)]}
  , GM.defaultIntegerMaps {GM.integerMapsSint64Values = Map.fromList [("a", minBound), ("b", 0), ("c", maxBound)]}
  , GM.defaultIntegerMaps {GM.integerMapsSfixed32Values = Map.fromList [("a", minBound), ("b", 0), ("c", maxBound)]}
  , GM.defaultIntegerMaps {GM.integerMapsSfixed64Values = Map.fromList [("a", minBound), ("b", 0), ("c", maxBound)]}
  , GM.defaultIntegerMaps {GM.integerMapsFixed32Values = Map.fromList [("a", 0), ("b", maxBound)]}
  , GM.defaultIntegerMaps {GM.integerMapsFixed64Values = Map.fromList [("a", 0), ("b", maxBound)]}
  , GM.defaultIntegerMaps {GM.integerMapsIntegerKeys = Map.fromList [(minBound, minBound), (0, 0), (maxBound, maxBound)]}
  , GM.defaultIntegerMaps {GM.integerMapsZigzagKeys = Map.fromList [(minBound, "min"), (0, "zero"), (maxBound, "max")]}
  ]
