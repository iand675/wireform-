{-# LANGUAGE OverloadedStrings #-}

{- | Tests for "Protovalidate.OpenAPI": the mapping from protovalidate
@buf.validate@ rules onto JSON Schema (OpenAPI 3.1) validation keywords, plus
the lossless @x-protovalidate@ / @x-cel@ capture of everything without a clean
standard analogue.

Most coverage is unit-level, calling 'fieldConstraintsFor' on hand-built
'FieldRules' and asserting on the emitted @fcKeywords@ (as a 'KeyMap.KeyMap')
and @fcRequired@. A handful of integration tests run the full path
(@parse .proto → fileMessageRules → protovalidateSchemaOptions →
componentSchemasWith@) to prove the annotators wire keywords, the object
@required@ list, and message-level @x-cel@ into the component schema.
-}
module Test.Protovalidate.OpenAPI (tests) where

import CEL.Value qualified as CV
import Data.Aeson (Object, Value (..), object, (.=))
import Data.Aeson.Key qualified as AKey
import Data.Aeson.KeyMap qualified as AKM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Proto.IDL.Parser (parseProtoFile, renderParseError)
import Proto.JSONSchema (FieldConstraints (..), buildSchemaEnv, componentSchemasWith)
import Protovalidate.Constraint (Constraint, unsafeConstraint)
import Protovalidate.OpenAPI (fieldConstraintsFor, protovalidateSchemaOptions)
import Protovalidate.Rules
import Protovalidate.Schema (fileMessageRules)
import Test.Syd
import Test.Syd.Hedgehog ()


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | The emitted keyword map for a field, or 'Nothing' when the field
-- contributes nothing ('fieldConstraintsFor' returns 'Nothing').
keywordsOf :: FieldRules -> Maybe Object
keywordsOf = fmap (AKM.fromList . fcKeywords) . fieldConstraintsFor


-- | The @required@ flag for a field (defaulting to 'False' when the field
-- contributes nothing).
requiredOf :: FieldRules -> Bool
requiredOf = maybe False fcRequired . fieldConstraintsFor


-- | An 'Object' literal for comparison.
obj :: [(Text, Value)] -> Object
obj = AKM.fromList . map (\(k, v) -> (AKey.fromText k, v))


-- ---------------------------------------------------------------------------
-- Standard keyword mapping (unit)
-- ---------------------------------------------------------------------------

-- | Each row: name, the field rules, and the exact keyword object it must emit.
standardRows :: [(String, FieldRules, Object)]
standardRows =
  [ -- string length + pattern
    ("string.min_len → minLength", fieldRules KString [minLen 3], obj [("minLength", Number 3)])
  , ("string.max_len → maxLength", fieldRules KString [maxLen 7], obj [("maxLength", Number 7)])
  , ("string.len → minLength+maxLength", fieldRules KString [lenV 5], obj [("minLength", Number 5), ("maxLength", Number 5)])
  , ("string.pattern → pattern", fieldRules KString [pattern "^a"], obj [("pattern", String "^a")])
  , -- const / in are kind-agnostic
    ("string.const → const", fieldRules KString [constV (CV.VString "x")], obj [("const", String "x")])
  , ("string.in → enum", fieldRules KString [inV [CV.VString "a", CV.VString "b"]], obj [("enum", Array (V.fromList [String "a", String "b"]))])
  , ("int32.const → const", fieldRules KInt32 [constV (CV.VInt 42)], obj [("const", Number 42)])
  , ("int32.in → enum", fieldRules KInt32 [inV [CV.VInt 1, CV.VInt 2]], obj [("enum", Array (V.fromList [Number 1, Number 2]))])
  , -- string formats
    ("string.email → format email", fieldRules KString [email], obj [("format", String "email")])
  , ("string.hostname → format hostname", fieldRules KString [hostname], obj [("format", String "hostname")])
  , ("string.uri → format uri", fieldRules KString [uri], obj [("format", String "uri")])
  , ("string.uri_ref → format uri-reference", fieldRules KString [("uri_ref", CV.VBool True)], obj [("format", String "uri-reference")])
  , ("string.ipv4 → format ipv4", fieldRules KString [("ipv4", CV.VBool True)], obj [("format", String "ipv4")])
  , ("string.ipv6 → format ipv6", fieldRules KString [("ipv6", CV.VBool True)], obj [("format", String "ipv6")])
  , ("string.uuid → format uuid", fieldRules KString [uuid], obj [("format", String "uuid")])
  , -- numeric bounds
    ("int32.gt → exclusiveMinimum", fieldRules KInt32 [gtV (CV.VInt 0)], obj [("exclusiveMinimum", Number 0)])
  , ("int32.gte → minimum", fieldRules KInt32 [gteV (CV.VInt 1)], obj [("minimum", Number 1)])
  , ("int32.lt → exclusiveMaximum", fieldRules KInt32 [ltV (CV.VInt 10)], obj [("exclusiveMaximum", Number 10)])
  , ("int32.lte → maximum", fieldRules KInt32 [lteV (CV.VInt 9)], obj [("maximum", Number 9)])
  , ("double.gt → exclusiveMinimum", fieldRules KDouble [gtV (CV.VDouble 0.5)], obj [("exclusiveMinimum", Number 0.5)])
  , -- repeated
    ("repeated.min_items → minItems", fieldRules KRepeated [minItems 1], obj [("minItems", Number 1)])
  , ("repeated.max_items → maxItems", fieldRules KRepeated [maxItems 5], obj [("maxItems", Number 5)])
  , ("repeated.unique → uniqueItems", fieldRules KRepeated [unique], obj [("uniqueItems", Bool True)])
  , -- map
    ("map.min_pairs → minProperties", fieldRules KMap [("min_pairs", CV.VUInt 1)], obj [("minProperties", Number 1)])
  , ("map.max_pairs → maxProperties", fieldRules KMap [("max_pairs", CV.VUInt 4)], obj [("maxProperties", Number 4)])
  ]


-- ---------------------------------------------------------------------------
-- x-protovalidate fallback + kind sensitivity (unit)
-- ---------------------------------------------------------------------------

-- | Rules with no standard analogue that must land under @x-protovalidate@
-- with the field's @kind@ and a @rules@ sub-object.
fallbackRows :: [(String, FieldRules, Object)]
fallbackRows =
  [ ("string.prefix → x-protovalidate", fieldRules KString [prefix "p"], xproto "string" [("prefix", String "p")])
  , ("string.suffix → x-protovalidate", fieldRules KString [suffix "s"], xproto "string" [("suffix", String "s")])
  , ("string.contains → x-protovalidate", fieldRules KString [contains "c"], xproto "string" [("contains", String "c")])
  , ("string.not_in → x-protovalidate", fieldRules KString [notInV [CV.VString "x"]], xproto "string" [("not_in", Array (V.fromList [String "x"]))])
  , ("bytes.len → x-protovalidate (byte length ≠ minLength)", fieldRules KBytes [("len", CV.VUInt 4)], xproto "bytes" [("len", Number 4)])
  ]
  where
    xproto kind rules =
      obj [("x-protovalidate", object [("kind", String kind), ("rules", Object (obj rules))])]


-- ---------------------------------------------------------------------------
-- The test tree
-- ---------------------------------------------------------------------------

tests :: Spec
tests =
  describe "OpenAPI" $ do
    describe "standard keyword mapping" $
      mapM_
        (\(name, fr, expected) -> it name (keywordsOf fr `shouldBe` Just expected))
        standardRows

    describe "x-protovalidate fallback" $ do
      mapM_
        (\(name, fr, expected) -> it name (keywordsOf fr `shouldBe` Just expected))
        fallbackRows

      it "a keyword rule on the wrong kind falls back (numeric gt on a string field)" $
        -- gt only becomes exclusiveMinimum for numeric kinds; on a string it
        -- is preserved losslessly rather than mis-mapped.
        keywordsOf (fieldRules KString [gtV (CV.VInt 3)])
          `shouldBe` Just (obj [("x-protovalidate", object [("kind", String "string"), ("rules", Object (obj [("gt", Number 3)]))])])

    describe "empty-sub-object regression" $ do
      it "a repeated field with a vacuous frItems emits minItems ONLY (no empty items key)" $
        -- Regression: repeated fields carry a vacuous frItems; it must not
        -- surface as an empty `items` object under x-protovalidate.
        keywordsOf ((fieldRules KRepeated [minItems 2]) {frItems = Just emptyFieldRules})
          `shouldBe` Just (obj [("minItems", Number 2)])

      it "a map field with vacuous frMapKeys/frMapValues emits minProperties ONLY" $
        keywordsOf ((fieldRules KMap [("min_pairs", CV.VUInt 1)]) {frMapKeys = Just emptyFieldRules, frMapValues = Just emptyFieldRules})
          `shouldBe` Just (obj [("minProperties", Number 1)])

      it "a repeated field with real item rules nests them under x-protovalidate.items" $
        keywordsOf ((fieldRules KRepeated [minItems 1]) {frItems = Just (fieldRules KString [minLen 2])})
          `shouldBe` Just
            ( obj
                [ ("minItems", Number 1)
                , ("x-protovalidate", object [("items", Object (obj [("minLength", Number 2)]))])
                ]
            )

      it "a map field with real value rules nests them under x-protovalidate.mapValues" $
        keywordsOf (mapValues (fieldRules KString [minLen 2]) (fieldRules KMap [("max_pairs", CV.VUInt 3)]))
          `shouldBe` Just
            ( obj
                [ ("maxProperties", Number 3)
                , ("x-protovalidate", object [("mapValues", Object (obj [("minLength", Number 2)]))])
                ]
            )

    describe "required" $ do
      it "frRequired=True with no other rules → fcRequired True and empty keywords" $ do
        let fr = emptyFieldRules {frRequired = True}
        keywordsOf fr `shouldBe` Just (obj [])
        requiredOf fr `shouldBe` True

      it "no rules and not required → Nothing (contributes nothing)" $
        keywordsOf emptyFieldRules `shouldBe` Nothing

      it "required alongside a rule keeps both the keyword and the required flag" $ do
        let fr = (fieldRules KString [minLen 2]) {frRequired = True}
        keywordsOf fr `shouldBe` Just (obj [("minLength", Number 2)])
        requiredOf fr `shouldBe` True

    describe "field CEL → x-cel" $ do
      it "a CEL-only field emits x-cel (and is NOT dropped to Nothing)" $
        keywordsOf (emptyFieldRules {frKind = Just KInt32, frCustom = [demoCel]})
          `shouldBe` Just (obj [("x-cel", demoCelXCel)])

      it "CEL alongside a standard keyword emits BOTH the keyword and x-cel" $
        keywordsOf (emptyFieldRules {frKind = Just KInt32, frRules = [gteV (CV.VInt 0)], frCustom = [demoCel]})
          `shouldBe` Just (obj [("minimum", Number 0), ("x-cel", demoCelXCel)])

      it "CEL alongside a non-standard rule emits BOTH x-protovalidate and x-cel" $
        keywordsOf (emptyFieldRules {frKind = Just KString, frRules = [prefix "x"], frCustom = [demoCel]})
          `shouldBe` Just
            ( obj
                [ ("x-protovalidate", object [("kind", String "string"), ("rules", Object (obj [("prefix", String "x")]))])
                , ("x-cel", demoCelXCel)
                ]
            )

      it "the x-cel object carries id, message and expression from the constraint" $
        keywordsOf (emptyFieldRules {frKind = Just KString, frCustom = [demoCel]})
          `shouldBe` Just
            ( obj
                [ ( "x-cel"
                  , Array
                      ( V.singleton
                          ( object
                              [ "id" .= ("myid" :: Text)
                              , "message" .= ("must hold" :: Text)
                              , "expression" .= ("this > 0" :: Text)
                              ]
                          )
                      )
                  )
                ]
            )

    describe "numeric bounds (property)" $
      it "gt/gte/lt/lte map to their distinct JSON Schema keyword carrying the exact bound" $
        boundsProperty

    describe "integration (full doc path)" $ do
      it "component schema merges keywords, required list and message x-cel" $
        userSchemaSpec


-- ---------------------------------------------------------------------------
-- CEL fixtures
-- ---------------------------------------------------------------------------

demoCel :: Constraint
demoCel = unsafeConstraint "myid" "must hold" "this > 0"


-- | The @x-cel@ array value that 'demoCel' must serialise to.
demoCelXCel :: Value
demoCelXCel =
  Array
    ( V.singleton
        ( object
            [ "id" .= ("myid" :: Text)
            , "message" .= ("must hold" :: Text)
            , "expression" .= ("this > 0" :: Text)
            ]
        )
    )


-- ---------------------------------------------------------------------------
-- Property: numeric bounds map to distinct, faithful keywords
-- ---------------------------------------------------------------------------

boundsProperty :: Property
boundsProperty = property $ do
  n <- forAll (Gen.integral (Range.linearFrom 0 (-1000000) 1000000) :: Gen Integer)
  let v = CV.VInt (fromIntegral n)
      num = Number (fromIntegral n)
      only name = Just (obj [(name, num)])
  keywordsOf (fieldRules KInt64 [gtV v]) === only "exclusiveMinimum"
  keywordsOf (fieldRules KInt64 [gteV v]) === only "minimum"
  keywordsOf (fieldRules KInt64 [ltV v]) === only "exclusiveMaximum"
  keywordsOf (fieldRules KInt64 [lteV v]) === only "maximum"


-- ---------------------------------------------------------------------------
-- Integration: parse .proto → rules → SchemaOptions → component schema
-- ---------------------------------------------------------------------------

userProto :: Text
userProto =
  T.unlines
    [ "syntax = \"proto3\";"
    , "package test.v1;"
    , "message User {"
    , "  string id = 1 [(buf.validate.field).string.min_len = 2, (buf.validate.field).required = true];"
    , "  uint32 age = 2 [(buf.validate.field).uint32.lte = 150];"
    , "  string email = 3 [(buf.validate.field).string.email = true];"
    , "  string nick = 4 [(buf.validate.field).string.prefix = \"n_\"];"
    , "  option (buf.validate.message).cel = {"
    , "    id: \"id_required_with_age\""
    , "    message: \"id must be set when age is set\""
    , "    expression: \"this.age == 0u || this.id != ''\""
    , "  };"
    , "}"
    ]


-- | The @test.v1.User@ component schema, built through the full annotator path.
userSchema :: Value
userSchema =
  case parseProtoFile "<test>" userProto of
    Left err -> error (renderParseError err)
    Right pf -> case fileMessageRules pf of
      Left e -> error (T.unpack e)
      Right rules ->
        let opts = protovalidateSchemaOptions rules
            env = buildSchemaEnv [pf]
        in case lookup "test.v1.User" (componentSchemasWith opts env) of
             Just s -> s
             Nothing -> error "no component schema for test.v1.User"


userSchemaSpec :: IO ()
userSchemaSpec = do
  let props = lookupObj "properties" userSchema
  -- minLength from string.min_len merged onto the field's own schema.
  fieldKeyword props "id" "minLength" `shouldBe` Just (Number 2)
  -- maximum from uint32.lte.
  fieldKeyword props "age" "maximum" `shouldBe` Just (Number 150)
  -- string.email → format.
  fieldKeyword props "email" "format" `shouldBe` Just (String "email")
  -- non-standard string.prefix preserved under x-protovalidate on the field.
  fieldKeyword props "nick" "x-protovalidate"
    `shouldBe` Just (object [("kind", String "string"), ("rules", Object (obj [("prefix", String "n_")]))])
  -- required=true joins the object's required list.
  requiredNames userSchema `shouldBe` Just [String "id"]
  -- message-level CEL surfaces as a top-level x-cel array.
  lookupKey "x-cel" userSchema
    `shouldBe` Just
      ( Array
          ( V.singleton
              ( object
                  [ "id" .= ("id_required_with_age" :: Text)
                  , "message" .= ("id must be set when age is set" :: Text)
                  , "expression" .= ("this.age == 0u || this.id != ''" :: Text)
                  ]
              )
          )
      )


-- | Look up a key on a JSON object 'Value' (Nothing when absent / not an object).
lookupKey :: Text -> Value -> Maybe Value
lookupKey k (Object o) = AKM.lookup (AKey.fromText k) o
lookupKey _ _ = Nothing


-- | Look up a key that must be an object, erroring loudly otherwise.
lookupObj :: Text -> Value -> Value
lookupObj k v = case lookupKey k v of
  Just o -> o
  Nothing -> error ("missing object key: " <> T.unpack k)


-- | A single keyword value on the schema of a named property.
fieldKeyword :: Value -> Text -> Text -> Maybe Value
fieldKeyword props field kw = lookupKey field props >>= lookupKey kw


-- | The @required@ array of a message schema, as a list of values.
requiredNames :: Value -> Maybe [Value]
requiredNames v = case lookupKey "required" v of
  Just (Array xs) -> Just (V.toList xs)
  _ -> Nothing
