{-# LANGUAGE OverloadedStrings #-}

-- | Contract tests for the OpenAPI 3.1 generator: the transport-agnostic
-- proto → JSON-Schema walk ("Proto.JSONSchema") and the Connect HTTP shaping
-- ("Network.Connect.OpenAPI").
--
-- Two fixtures drive the suite:
--
-- * @shapes.proto@ (parsed at runtime) exercises every JSON-Schema shape —
--   64-bit ints, bytes, repeated, map, enum, nested message, oneof, json_name
--   override, and a @google.protobuf.Timestamp@ reference — without needing
--   generated Haskell types.
-- * @eliza.proto@'s already-spliced @Connect.TestProto@ types provide the
--   /codec oracle/: their @Aeson.ToJSON@ instances emit proto3-canonical JSON,
--   so the schema's property names must match the keys the codec actually
--   produces.
module Test.OpenAPI (tests) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AKey
import Data.Aeson.KeyMap qualified as AKM
import Data.ByteString.Lazy qualified as BL
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd
import Test.Syd.Hedgehog ()

import Connect.TestProto
import Network.Connect.Error (allConnectCodes, connectCodeName)
import Network.Connect.OpenAPI
import Proto.IDL.AST (FieldType (..), ProtoFile, ScalarType (..))
import Proto.IDL.Parser (parseProtoFile)
import Proto.JSONSchema
  ( SchemaOptions (..)
  , buildSchemaEnv
  , componentSchemas
  , fieldTypeSchema
  , isWellKnown
  , refForFqn
  , resolveTypeFqn
  , wellKnownSchema
  )

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | Read a fixture proto, tolerating either the repo-root or package-root cwd.
readFixture :: FilePath -> IO Text
readFixture name = go ["wireform-connect/test/proto/" <> name, "test/proto/" <> name]
  where
    go [] = error ("Test.OpenAPI: cannot locate fixture proto/" <> name)
    go (p : ps) = do
      r <- try (TIO.readFile p) :: IO (Either SomeException Text)
      either (const (go ps)) pure r

parseFixture :: FilePath -> Text -> ProtoFile
parseFixture fp src = either (const (error ("Test.OpenAPI: parse failed for " <> fp))) id (parseProtoFile fp src)

-- | The proto3-JSON message builders for the eliza fixture, paired with the
-- fully-qualified name and the single JSON key each carries. The codec's
-- 'Aeson.ToJSON' is the independent oracle for the schema's property names.
elizaBuilders :: [(Text, Text, Text -> Value)]
elizaBuilders =
  [ ("connectrpc.eliza.v1.SayRequest", "sentence", \t -> toJSON defaultSayRequest {sayRequestSentence = t})
  , ("connectrpc.eliza.v1.SayResponse", "sentence", \t -> toJSON defaultSayResponse {sayResponseSentence = t})
  , ("connectrpc.eliza.v1.ConverseRequest", "sentence", \t -> toJSON defaultConverseRequest {converseRequestSentence = t})
  , ("connectrpc.eliza.v1.ConverseResponse", "sentence", \t -> toJSON defaultConverseResponse {converseResponseSentence = t})
  , ("connectrpc.eliza.v1.IntroduceRequest", "name", \t -> toJSON defaultIntroduceRequest {introduceRequestName = t})
  , ("connectrpc.eliza.v1.IntroduceResponse", "sentence", \t -> toJSON defaultIntroduceResponse {introduceResponseSentence = t})
  ]

-- ---------------------------------------------------------------------------
-- Value navigation helpers
-- ---------------------------------------------------------------------------

lookupKey :: Text -> Value -> Maybe Value
lookupKey k (Object o) = AKM.lookup (AKey.fromText k) o
lookupKey _ _ = Nothing

atPath :: [Text] -> Value -> Maybe Value
atPath ks v0 = foldl step (Just v0) ks
  where
    step acc k = acc >>= lookupKey k

grab :: [Text] -> Value -> IO Value
grab ks v = maybe (expectationFailure ("Test.OpenAPI: no value at path " <> show ks)) pure (atPath ks v)

objectKeys :: Value -> [Text]
objectKeys (Object o) = map AKey.toText (AKM.keys o)
objectKeys _ = []

asKeyMap :: Value -> AKM.KeyMap Value
asKeyMap (Object o) = o
asKeyMap _ = AKM.empty

-- Expected-schema constructors (Text-typed so OverloadedStrings stays honest).
typed :: Text -> Value
typed t = object ["type" .= t]

typedFmt :: Text -> Text -> Value
typedFmt t f = object ["type" .= t, "format" .= f]

arrOf :: Value -> Value
arrOf items = object ["type" .= ("array" :: Text), "items" .= items]

mapOf :: Value -> Value
mapOf val = object ["type" .= ("object" :: Text), "additionalProperties" .= val]

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

tests :: Spec
tests = describe "OpenAPI" $ do
  elizaSrc <- runIO (readFixture "eliza.proto")
  shapesSrc <- runIO (readFixture "shapes.proto")
  depSrc <- runIO (readFixture "deprecated.proto")
  let elizaPf = parseFixture "eliza.proto" elizaSrc
      shapesPf = parseFixture "shapes.proto" shapesSrc
      depPf = parseFixture "deprecated.proto" depSrc
      shapesEnv = buildSchemaEnv [shapesPf]
      shapesDoc =
        connectOpenApi
          defaultOpenApiOptions {ooTitle = "Shapes API", ooVersion = "2.0.0"}
          [shapesPf]
          [shapesPf]
      elizaDoc = connectOpenApi defaultOpenApiOptions [elizaPf] [elizaPf]
      shapePath m = "/shapes.v1.ShapeService/" <> m
      -- The deprecation fixture, run through the built-in deprecation
      -- annotator (schema + operation) and, for contrast, with none.
      depDoc = connectOpenApiAnnotated (deprecationAnnotators [depPf]) defaultOpenApiOptions [depPf] [depPf]
      depPlain = connectOpenApi defaultOpenApiOptions [depPf] [depPf]
      itemPath m = "/dep.v1.ItemService/" <> m
      legacyPath m = "/dep.v1.LegacyService/" <> m

  it "fixtures parse cleanly" $ do
    case parseProtoFile "eliza.proto" elizaSrc of
      Right _ -> pure ()
      Left _ -> expectationFailure "eliza.proto failed to parse"
    case parseProtoFile "shapes.proto" shapesSrc of
      Right _ -> pure ()
      Left _ -> expectationFailure "shapes.proto failed to parse"

  describe "fidelity: proto3-JSON schema shapes (shapes.proto)" $ do
    it "property keys are lowerCamelCase, json_name honoured, oneof flattened" $ do
      props <- grab ["components", "schemas", "shapes.v1.Shape", "properties"] shapesDoc
      -- product_id → productId (camel), legacy_field → legacyName (json_name),
      -- oneof members alpha/beta flatten in (no "choice" wrapper).
      sort (objectKeys props)
        `shouldBe` sort
          [ "name"
          , "bigId"
          , "huge"
          , "small"
          , "blob"
          , "flag"
          , "tags"
          , "counts"
          , "color"
          , "nested"
          , "createdAt"
          , "productId"
          , "legacyName"
          , "alpha"
          , "beta"
          ]
    it "field type schemas match the proto3-JSON codec encoding" $ do
      props <- grab ["components", "schemas", "shapes.v1.Shape", "properties"] shapesDoc
      let f k = lookupKey k props
      f "name" `shouldBe` Just (typed "string")
      f "bigId" `shouldBe` Just (typedFmt "string" "int64") -- 64-bit int → string
      f "huge" `shouldBe` Just (typedFmt "string" "uint64") -- uint64 → string
      f "small" `shouldBe` Just (typedFmt "integer" "int32") -- 32-bit stays a number
      f "blob" `shouldBe` Just (typedFmt "string" "byte") -- bytes → base64 string
      f "flag" `shouldBe` Just (typed "boolean")
      f "tags" `shouldBe` Just (arrOf (typed "string")) -- repeated → array
      f "counts" `shouldBe` Just (mapOf (typedFmt "integer" "int32")) -- map → additionalProperties
      f "color" `shouldBe` Just (refForFqn "shapes.v1.Color") -- enum ref
      f "nested" `shouldBe` Just (refForFqn "shapes.v1.Shape.Nested") -- nested message ref
      f "createdAt" `shouldBe` Just (typedFmt "string" "date-time") -- WKT inlined
      f "productId" `shouldBe` Just (typed "string")
      f "legacyName" `shouldBe` Just (typed "string")
      f "alpha" `shouldBe` Just (typed "string")
      f "beta" `shouldBe` Just (typedFmt "integer" "int32")
    it "enum renders as a string enum in declared order" $ do
      color <- grab ["components", "schemas", "shapes.v1.Color"] shapesDoc
      color
        `shouldBe` object
          [ "type" .= ("string" :: Text)
          , "enum" .= (["COLOR_UNSPECIFIED", "COLOR_RED", "COLOR_GREEN", "COLOR_BLUE"] :: [Text])
          ]

  describe "well-known types inline by name" $ do
    it "Timestamp inlines to a date-time string with the import absent from env" $
      fieldTypeSchema shapesEnv "shapes.v1" [] (FTNamed "google.protobuf.Timestamp")
        `shouldBe` typedFmt "string" "date-time"
    it "leading-dot FQN also inlines" $
      fieldTypeSchema shapesEnv "shapes.v1" [] (FTNamed ".google.protobuf.Timestamp")
        `shouldBe` typedFmt "string" "date-time"
    it "Duration inlines to wellKnownSchema's canonical value" $
      Just (fieldTypeSchema shapesEnv "shapes.v1" [] (FTNamed "google.protobuf.Duration"))
        `shouldBe` wellKnownSchema "google.protobuf.Duration"
    it "int64 scalar is a string, int32 stays an integer" $ do
      fieldTypeSchema shapesEnv "shapes.v1" [] (FTScalar SInt64) `shouldBe` typedFmt "string" "int64"
      fieldTypeSchema shapesEnv "shapes.v1" [] (FTScalar SInt32) `shouldBe` typedFmt "integer" "int32"

  describe "name resolution (resolveTypeFqn)" $ do
    it "resolves package-relative, scoped, fully-qualified, and unique-suffix names" $ do
      resolveTypeFqn shapesEnv "shapes.v1" [] "Shape" `shouldBe` Just "shapes.v1.Shape"
      resolveTypeFqn shapesEnv "shapes.v1" ["Shape"] "Nested" `shouldBe` Just "shapes.v1.Shape.Nested"
      resolveTypeFqn shapesEnv "shapes.v1" [] "shapes.v1.Color" `shouldBe` Just "shapes.v1.Color"
      resolveTypeFqn shapesEnv "shapes.v1" [] "Nested" `shouldBe` Just "shapes.v1.Shape.Nested"
      resolveTypeFqn shapesEnv "shapes.v1" [] "DoesNotExist" `shouldBe` Nothing
    it "resolves cross-package by fully-qualified and package-prefixed name" $ do
      let env2 = buildSchemaEnv [shapesPf, elizaPf]
      resolveTypeFqn env2 "shapes.v1" [] "connectrpc.eliza.v1.SayRequest"
        `shouldBe` Just "connectrpc.eliza.v1.SayRequest"
      resolveTypeFqn env2 "connectrpc.eliza.v1" [] "SayRequest"
        `shouldBe` Just "connectrpc.eliza.v1.SayRequest"

  describe "components" $ do
    it "componentSchemas covers user messages/enums (incl. nested), excludes WKTs" $ do
      sort (map fst (componentSchemas shapesEnv))
        `shouldBe` [ "shapes.v1.ChatMsg"
                   , "shapes.v1.Color"
                   , "shapes.v1.CreateResult"
                   , "shapes.v1.GetShapeRequest"
                   , "shapes.v1.Shape"
                   , "shapes.v1.Shape.Nested"
                   ]
      isWellKnown "google.protobuf.Timestamp" `shouldBe` True
      isWellKnown "shapes.v1.Shape" `shouldBe` False
    it "document components add the error envelope and never emit an inlined WKT" $ do
      docComps <- grab ["components", "schemas"] shapesDoc
      let ks = objectKeys docComps
      ("connect.Error" `elem` ks) `shouldBe` True
      ("connect.Error.Detail" `elem` ks) `shouldBe` True
      ("google.protobuf.Timestamp" `elem` ks) `shouldBe` False

  describe "Connect HTTP shaping" $ do
    it "emits one path per method as /pkg.Service/Method" $ do
      ps <- grab ["paths"] shapesDoc
      sort (objectKeys ps)
        `shouldBe` [ shapePath "Chat"
                   , shapePath "CreateShape"
                   , shapePath "GetShape"
                   , shapePath "StreamShapes"
                   , shapePath "UploadShapes"
                   ]
    it "NO_SIDE_EFFECTS unary method gets get+post; every other method is post-only" $ do
      getShape <- grab ["paths", shapePath "GetShape"] shapesDoc
      sort (objectKeys getShape) `shouldBe` ["get", "post"]
      forM_ ["CreateShape", "StreamShapes", "UploadShapes", "Chat"] $ \m -> do
        item <- grab ["paths", shapePath m] shapesDoc
        objectKeys item `shouldBe` ["post"]
    it "operationId is Service_Method" $
      atPath ["paths", shapePath "GetShape", "post", "operationId"] shapesDoc
        `shouldBe` Just (String "ShapeService_GetShape")
    it "unary body/response are application/json with an undecorated $ref" $ do
      reqContent <- grab ["paths", shapePath "CreateShape", "post", "requestBody", "content"] shapesDoc
      objectKeys reqContent `shouldBe` ["application/json"]
      reqSchema <- grab ["application/json", "schema"] reqContent
      reqSchema `shouldBe` refForFqn "shapes.v1.Shape" -- exact ⇒ no x-connect-streaming
      respSchema <-
        grab
          ["paths", shapePath "CreateShape", "post", "responses", "200", "content", "application/json", "schema"]
          shapesDoc
      respSchema `shouldBe` refForFqn "shapes.v1.Shape"
    it "server-streaming body is application/connect+json tagged x-connect-streaming=server" $
      assertStreaming shapesDoc (shapePath "StreamShapes") "server"
    it "client-streaming body is application/connect+json tagged x-connect-streaming=client" $
      assertStreaming shapesDoc (shapePath "UploadShapes") "client"
    it "bidi body is application/connect+json tagged x-connect-streaming=bidi" $
      assertStreaming shapesDoc (shapePath "Chat") "bidi"
    it "GET carries the five Connect query params in order with correct required flags" $ do
      paramsV <- grab ["paths", shapePath "GetShape", "get", "parameters"] shapesDoc
      case paramsV of
        Array ps -> do
          let field k (Object p) = AKM.lookup (AKey.fromText k) p
              field _ _ = Nothing
              ps' = V.toList ps
          map (field "name") ps'
            `shouldBe` map (Just . String) ["message", "encoding", "base64", "compression", "connect"]
          map (field "required") ps'
            `shouldBe` [Just (Bool True), Just (Bool True), Just (Bool False), Just (Bool False), Just (Bool False)]
          all ((== Just (String "query")) . field "in") ps' `shouldBe` True
        _ -> expectationFailure "parameters is not a JSON array"
    it "every operation has a default response → connect.Error" $ do
      ps <- grab ["paths"] shapesDoc
      let ops = concatMap (AKM.elems . asKeyMap) (AKM.elems (asKeyMap ps))
      ops `shouldSatisfy` (not . null)
      forM_ ops $ \op ->
        atPath ["responses", "default", "content", "application/json", "schema"] op
          `shouldBe` Just (refForFqn "connect.Error")
    it "connect.Error.code enum is the 16 Connect codes in canonical order" $ do
      enumV <- grab ["components", "schemas", "connect.Error", "properties", "code", "enum"] shapesDoc
      enumV `shouldBe` Array (V.fromList (map (String . connectCodeName) allConnectCodes))

  describe "document envelope" $ do
    it "openapi is 3.1.0 and info comes from the options" $ do
      atPath ["openapi"] shapesDoc `shouldBe` Just (String "3.1.0")
      atPath ["info", "title"] shapesDoc `shouldBe` Just (String "Shapes API")
      atPath ["info", "version"] shapesDoc `shouldBe` Just (String "2.0.0")
    it "servers omitted when empty, present when set" $ do
      let d0 = connectOpenApi defaultOpenApiOptions [shapesPf] [shapesPf]
          d1 = connectOpenApi defaultOpenApiOptions {ooServers = ["https://api.example.com"]} [shapesPf] [shapesPf]
      lookupKey "servers" d0 `shouldBe` Nothing
      atPath ["servers"] d1
        `shouldBe` Just (Array (V.fromList [object ["url" .= ("https://api.example.com" :: Text)]]))

  describe "annotators: the no-op contract" $ do
    it "a deprecation annotator over a fixture with nothing deprecated leaves the document unchanged" $
      -- shapes.proto has no `deprecated` anywhere: the annotator must be a
      -- true no-op — never emit `deprecated: false`, never touch a live item.
      connectOpenApiAnnotated (deprecationAnnotators [shapesPf]) defaultOpenApiOptions [shapesPf] [shapesPf]
        `shouldBe` connectOpenApi defaultOpenApiOptions [shapesPf] [shapesPf]
    it "an independently-built empty bundle (schemaAnnotators mempty) reduces to the base generator" $
      -- Not a syntactic identity: schemaAnnotators is a real call whose result
      -- must *behave* as the no-op that connectOpenApi uses.
      connectOpenApiAnnotated (schemaAnnotators mempty) defaultOpenApiOptions [depPf] [depPf]
        `shouldBe` connectOpenApi defaultOpenApiOptions [depPf] [depPf]
    it "the un-annotated walk emits no deprecated key on any deprecated-fixture schema or operation" $ do
      -- Deprecation is contributed *only* by the annotator, never the base walk.
      oldMsg <- grab ["components", "schemas", "dep.v1.OldMsg"] depPlain
      lookupKey "deprecated" oldMsg `shouldBe` Nothing
      oldEnum <- grab ["components", "schemas", "dep.v1.OldEnum"] depPlain
      lookupKey "deprecated" oldEnum `shouldBe` Nothing
      props <- grab ["components", "schemas", "dep.v1.Item", "properties"] depPlain
      atPath ["oldField", "deprecated"] props `shouldBe` Nothing
      getPost <- grab ["paths", itemPath "GetItem", "post"] depPlain
      lookupKey "deprecated" getPost `shouldBe` Nothing

  describe "deprecation annotator (deprecationAnnotators): deprecated → deprecated:true" $ do
    it "a deprecated message component carries deprecated:true" $ do
      oldMsg <- grab ["components", "schemas", "dep.v1.OldMsg"] depDoc
      lookupKey "deprecated" oldMsg `shouldBe` Just (Bool True)
    it "a deprecated enum component carries deprecated:true; a live enum does not" $ do
      oldEnum <- grab ["components", "schemas", "dep.v1.OldEnum"] depDoc
      lookupKey "deprecated" oldEnum `shouldBe` Just (Bool True)
      liveEnum <- grab ["components", "schemas", "dep.v1.LiveEnum"] depDoc
      lookupKey "deprecated" liveEnum `shouldBe` Nothing
    it "deprecated regular/map/oneof fields carry deprecated:true; live siblings do not" $ do
      props <- grab ["components", "schemas", "dep.v1.Item", "properties"] depDoc
      let dep k = atPath [k, "deprecated"] props
      dep "oldField" `shouldBe` Just (Bool True) -- regular field
      dep "oldMap" `shouldBe` Just (Bool True) -- map field
      dep "oldAlt" `shouldBe` Just (Bool True) -- oneof member
      dep "name" `shouldBe` Nothing
      dep "liveMap" `shouldBe` Nothing
      dep "newAlt" `shouldBe` Nothing
    it "a deprecated RPC deprecates both its post and its idempotent get" $ do
      post <- grab ["paths", itemPath "GetItem", "post"] depDoc
      getOp <- grab ["paths", itemPath "GetItem", "get"] depDoc
      lookupKey "deprecated" post `shouldBe` Just (Bool True)
      lookupKey "deprecated" getOp `shouldBe` Just (Bool True)
    it "a non-deprecated RPC operation has no deprecated key" $ do
      post <- grab ["paths", itemPath "CreateItem", "post"] depDoc
      lookupKey "deprecated" post `shouldBe` Nothing
    it "a deprecated service deprecates every method's operation(s)" $ do
      ping <- grab ["paths", legacyPath "Ping", "post"] depDoc
      lookupKey "deprecated" ping `shouldBe` Just (Bool True)
      peekPost <- grab ["paths", legacyPath "Peek", "post"] depDoc
      peekGet <- grab ["paths", legacyPath "Peek", "get"] depDoc
      lookupKey "deprecated" peekPost `shouldBe` Just (Bool True)
      lookupKey "deprecated" peekGet `shouldBe` Just (Bool True)

  describe "custom hooks fire on every target" $ do
    it "soEnumAnnotator adds its keyword to every enum component" $ do
      let ann = schemaAnnotators (mempty {soEnumAnnotator = \_ -> ["x-foo" .= True]})
          doc = connectOpenApiAnnotated ann defaultOpenApiOptions [depPf] [depPf]
      forM_ ["dep.v1.LiveEnum", "dep.v1.OldEnum"] $ \e -> do
        enumV <- grab ["components", "schemas", e] doc
        lookupKey "x-foo" enumV `shouldBe` Just (Bool True)
    it "anOperation adds a per-method keyword to every operation (post and idempotent get)" $ do
      let ann = mempty {anOperation = \_svc meth -> ["x-method" .= meth]}
          doc = connectOpenApiAnnotated ann defaultOpenApiOptions [depPf] [depPf]
      ps <- grab ["paths"] doc
      let ops = concatMap (AKM.elems . asKeyMap) (AKM.elems (asKeyMap ps))
      ops `shouldSatisfy` (not . null)
      -- the hook fires on *every* operation object,
      filter (\op -> lookupKey "x-method" op /= Nothing) ops `shouldSatisfy` ((== length ops) . length)
      -- carrying that operation's own method name (both the post and the get).
      getItemPost <- grab ["paths", itemPath "GetItem", "post"] doc
      lookupKey "x-method" getItemPost `shouldBe` Just (String "GetItem")
      getItemGet <- grab ["paths", itemPath "GetItem", "get"] doc
      lookupKey "x-method" getItemGet `shouldBe` Just (String "GetItem")
      createPost <- grab ["paths", itemPath "CreateItem", "post"] doc
      lookupKey "x-method" createPost `shouldBe` Just (String "CreateItem")

  describe "annotators compose (Monoid): a <> b contributes both" $ do
    it "deprecationAnnotators <> a custom enum+operation annotator yields all four contributions" $ do
      let extra =
            Annotators
              { anSchema = mempty {soEnumAnnotator = \_ -> ["x-foo" .= True]}
              , anOperation = \_svc meth -> ["x-method" .= meth]
              }
          doc =
            connectOpenApiAnnotated
              (deprecationAnnotators [depPf] <> extra)
              defaultOpenApiOptions
              [depPf]
              [depPf]
      -- deprecation carries through on the operation...
      getPost <- grab ["paths", itemPath "GetItem", "post"] doc
      lookupKey "deprecated" getPost `shouldBe` Just (Bool True)
      -- ...alongside the custom operation keyword,
      lookupKey "x-method" getPost `shouldBe` Just (String "GetItem")
      -- deprecation carries through on a schema (message)...
      oldMsg <- grab ["components", "schemas", "dep.v1.OldMsg"] doc
      lookupKey "deprecated" oldMsg `shouldBe` Just (Bool True)
      -- ...and the deprecated enum carries BOTH deprecated and the custom keyword.
      oldEnum <- grab ["components", "schemas", "dep.v1.OldEnum"] doc
      lookupKey "deprecated" oldEnum `shouldBe` Just (Bool True)
      lookupKey "x-foo" oldEnum `shouldBe` Just (Bool True)
    it "mempty is a left and right identity for <> on a non-trivial annotator" $ do
      let ann = deprecationAnnotators [depPf]
          base = connectOpenApiAnnotated ann defaultOpenApiOptions [depPf] [depPf]
      connectOpenApiAnnotated (mempty <> ann) defaultOpenApiOptions [depPf] [depPf] `shouldBe` base
      connectOpenApiAnnotated (ann <> mempty) defaultOpenApiOptions [depPf] [depPf] `shouldBe` base

  describe "renderOpenApi" $
    it "renders with deterministically sorted top-level keys and round-trips" $ do
      let bytes = renderOpenApi shapesDoc
          txt = TE.decodeUtf8 (BL.toStrict bytes)
          idxOf n = T.length (fst (T.breakOn n txt))
      -- Sorted keys: "components" precedes "openapi" (insertion order is reversed).
      (idxOf "\n  \"components\"" < idxOf "\n  \"openapi\"") `shouldBe` True
      Aeson.decode bytes `shouldBe` Just shapesDoc

  describe "codec fidelity cross-check (eliza generated types)" $ do
    it "the codec's proto3-JSON keys are exactly the schema's declared property names" $
      H.property $ do
        t <- H.forAll (Gen.text (Range.linear 0 24) Gen.unicode)
        forM_ elizaBuilders $ \(fqn, jsonName, build) ->
          case atPath ["components", "schemas", fqn, "properties"] elizaDoc of
            Nothing -> H.annotate ("no schema properties for " <> T.unpack fqn) >> H.failure
            Just propsV -> do
              let props = objectKeys propsV
                  emitted = objectKeys (build t)
              -- The schema declares the field the codec knows about,
              H.assert (jsonName `elem` props)
              -- the codec never emits a key absent from the schema,
              H.assert (all (`elem` props) emitted)
              -- and a set (non-default) field serialises under exactly that key.
              when (not (T.null t)) (emitted H.=== [jsonName])
    it "plain unary Say gets POST only (no GET without NO_SIDE_EFFECTS)" $ do
      say <- grab ["paths", "/connectrpc.eliza.v1.ElizaService/Say"] elizaDoc
      objectKeys say `shouldBe` ["post"]

-- | Assert a streaming method's request and response bodies both use the
-- Connect stream content-type and carry the expected @x-connect-streaming@ tag.
assertStreaming :: Value -> Text -> Text -> IO ()
assertStreaming doc path kind = do
  reqContent <- grab ["paths", path, "post", "requestBody", "content"] doc
  objectKeys reqContent `shouldBe` ["application/connect+json"]
  reqSchema <- grab ["application/connect+json", "schema"] reqContent
  lookupKey "x-connect-streaming" reqSchema `shouldBe` Just (String kind)
  respContent <- grab ["paths", path, "post", "responses", "200", "content"] doc
  objectKeys respContent `shouldBe` ["application/connect+json"]
  respSchema <- grab ["application/connect+json", "schema"] respContent
  lookupKey "x-connect-streaming" respSchema `shouldBe` Just (String kind)
