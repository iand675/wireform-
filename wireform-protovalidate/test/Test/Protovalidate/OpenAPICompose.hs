{-# LANGUAGE OverloadedStrings #-}

{- | Contract tests for the /composable annotator seam/ (the "additional
annotations" story) that lives in "Proto.JSONSchema":

* 'FieldConstraints' and 'SchemaOptions' are 'Monoid's, and composing two
  annotators contributes BOTH — keywords concatenate, @required@ is OR'd.
* 'mempty' / 'defaultSchemaOptions' is the no-op: @componentSchemasWith mempty@
  reproduces the base 'componentSchemas' walk exactly.
* The headline: @'deprecationSchemaOptions' files '<>' 'protovalidateSchemaOptions'
  rules@ applied to a field that is BOTH @[deprecated = true]@ AND carries a
  validate rule → that field's schema carries BOTH @deprecated: true@ AND the
  validate keyword (@minLength@). Neither annotator clobbers the other.

This suite lives in @wireform-protovalidate@ because the headline needs both
@wireform-proto@ (deprecation) and @wireform-protovalidate@ (rule mapping) in
scope. It uses the DOTTED @buf.validate@ option form and does not import
@buf/validate/validate.proto@.
-}
module Test.Protovalidate.OpenAPICompose (tests) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.Key qualified as AKey
import Data.Aeson.KeyMap qualified as AKM
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Proto.IDL.AST (ProtoFile)
import Proto.IDL.Parser (parseProtoFile, renderParseError)
import Proto.JSONSchema
  ( FieldConstraints (..)
  , SchemaEnv
  , SchemaOptions (..)
  , buildSchemaEnv
  , componentSchemas
  , componentSchemasWith
  , defaultSchemaOptions
  , deprecationSchemaOptions
  , fieldConstraints
  )
import Protovalidate.OpenAPI (protovalidateSchemaOptions)
import Protovalidate.Schema (fileMessageRules)
import Protovalidate.Rules (MessageRules)
import Test.Syd
import Test.Syd.Hedgehog ()


-- ---------------------------------------------------------------------------
-- The test tree
-- ---------------------------------------------------------------------------

tests :: Spec
tests = describe "OpenAPI annotator composition" $ do
  describe "FieldConstraints Monoid" $ do
    it "left identity: mempty <> a ≡ a" $ hedgehogProperty fcLeftIdentity
    it "right identity: a <> mempty ≡ a" $ hedgehogProperty fcRightIdentity
    it "associativity: (a <> b) <> c ≡ a <> (b <> c)" $ hedgehogProperty fcAssociative
    it "combine: keywords concatenate and required is OR'd" $ do
      let a = fieldConstraints ["x-a" .= True] False
          b = fieldConstraints ["x-b" .= Number 1] True
          ab = a <> b
      fcKeywords ab `shouldBe` ["x-a" .= True, "x-b" .= Number 1]
      fcRequired ab `shouldBe` True
    it "required OR is not AND: False <> False stays False, any True wins" $ do
      fcRequired (fieldConstraints [] False <> fieldConstraints [] False) `shouldBe` False
      fcRequired (fieldConstraints [] True <> fieldConstraints [] False) `shouldBe` True
      fcRequired (fieldConstraints [] False <> fieldConstraints [] True) `shouldBe` True

  describe "SchemaOptions Monoid" $ do
    it "mempty is the no-op: componentSchemasWith mempty ≡ componentSchemas" $
      componentSchemasWith mempty composeEnv `shouldBe` componentSchemas composeEnv
    it "defaultSchemaOptions is the no-op too" $
      componentSchemasWith defaultSchemaOptions composeEnv `shouldBe` componentSchemas composeEnv
    it "two field annotators compose: the field carries BOTH keywords and required is OR'd" $ do
      -- annA adds x-a (not required); annB adds x-b + marks required. Both key
      -- the SAME field ("a") so we prove <> merges rather than overwrites.
      let annA = mempty {soFieldAnnotator = \_ fld -> if fld == "a" then Just (fieldConstraints ["x-a" .= True] False) else Nothing}
          annB = mempty {soFieldAnnotator = \_ fld -> if fld == "a" then Just (fieldConstraints ["x-b" .= Number 7] True) else Nothing}
          props = propsOf "cmp.v1.Rec" (annA <> annB)
      fieldKw props "a" "x-a" `shouldBe` Just (Bool True)
      fieldKw props "a" "x-b" `shouldBe` Just (Number 7)
      -- OR'd required flag surfaces on the object's required list.
      requiredNames (schemaOf "cmp.v1.Rec" (annA <> annB)) `shouldBe` Just [String "a"]
      -- field "b" is untouched by either annotator.
      fieldKw props "b" "x-a" `shouldBe` Nothing
    it "an enum annotator composes onto the enum component" $ do
      let ann = mempty {soEnumAnnotator = \_ -> ["x-flag" .= True]}
          e = schemaOf "cmp.v1.E" ann
      lookupKey "x-flag" e `shouldBe` Just (Bool True)

  describe "headline: deprecation ⊗ protovalidate on the same field" $ do
    it "a field that is [deprecated=true] AND has a validate rule carries BOTH deprecated:true and minLength" $ do
      let props = headlineProps (deprecationSchemaOptions [headlinePf] <> protovalidateSchemaOptions headlineRules)
      -- both annotators land on the same property, neither clobbering the other:
      fieldKw props "code" "deprecated" `shouldBe` Just (Bool True) -- from deprecationSchemaOptions
      fieldKw props "code" "minLength" `shouldBe` Just (Number 3) -- from protovalidateSchemaOptions
    it "each annotator alone contributes only its own keyword (proving the merge, not a coincidence)" $ do
      let depOnly = headlineProps (deprecationSchemaOptions [headlinePf])
          valOnly = headlineProps (protovalidateSchemaOptions headlineRules)
      fieldKw depOnly "code" "deprecated" `shouldBe` Just (Bool True)
      fieldKw depOnly "code" "minLength" `shouldBe` Nothing
      fieldKw valOnly "code" "minLength" `shouldBe` Just (Number 3)
      fieldKw valOnly "code" "deprecated" `shouldBe` Nothing
    it "composition is commutative in effect: the two orders agree on the field schema" $ do
      let l = headlineProps (deprecationSchemaOptions [headlinePf] <> protovalidateSchemaOptions headlineRules)
          r = headlineProps (protovalidateSchemaOptions headlineRules <> deprecationSchemaOptions [headlinePf])
      -- same key set on the field, in particular both deprecated and minLength present.
      sort (objKeys (fieldSchema l "code")) `shouldBe` sort (objKeys (fieldSchema r "code"))
      fieldKw r "code" "deprecated" `shouldBe` Just (Bool True)
      fieldKw r "code" "minLength" `shouldBe` Just (Number 3)


-- ---------------------------------------------------------------------------
-- FieldConstraints Monoid properties
-- ---------------------------------------------------------------------------

-- | FieldConstraints has no 'Eq'; compare via its two projections.
fcEq :: (Monad m) => FieldConstraints -> FieldConstraints -> PropertyT m ()
fcEq x y = do
  fcKeywords x === fcKeywords y
  fcRequired x === fcRequired y


-- | A small generator over the value space the annotators actually produce:
-- @deprecated@/@x-@ boolean-or-number keyword pairs and a required flag.
genFC :: Gen FieldConstraints
genFC = do
  ks <- Gen.list (Range.linear 0 3) genPair
  req <- Gen.bool
  pure (fieldConstraints ks req)
  where
    genPair = do
      name <- Gen.element ["deprecated", "minLength", "x-a", "x-b", "maximum"]
      val <- Gen.choice [Bool <$> Gen.bool, Number . fromIntegral <$> Gen.int (Range.linear 0 99)]
      pure (AKey.fromText name .= val)


fcLeftIdentity :: Property
fcLeftIdentity = property $ do
  a <- forAll genFC
  fcEq (mempty <> a) a


fcRightIdentity :: Property
fcRightIdentity = property $ do
  a <- forAll genFC
  fcEq (a <> mempty) a


fcAssociative :: Property
fcAssociative = property $ do
  a <- forAll genFC
  b <- forAll genFC
  c <- forAll genFC
  fcEq ((a <> b) <> c) (a <> (b <> c))


hedgehogProperty :: Property -> IO ()
hedgehogProperty p = do
  ok <- check p
  ok `shouldBe` True


-- ---------------------------------------------------------------------------
-- Fixtures for the SchemaOptions composition tests
-- ---------------------------------------------------------------------------

-- | A tiny message (two string fields) + an enum, to exercise field- and
-- enum-level annotators without needing any validate rules.
composeProto :: Text
composeProto =
  T.unlines
    [ "syntax = \"proto3\";"
    , "package cmp.v1;"
    , "message Rec {"
    , "  string a = 1;"
    , "  string b = 2;"
    , "}"
    , "enum E {"
    , "  E_UNSPECIFIED = 0;"
    , "  E_ONE = 1;"
    , "}"
    ]


composeEnv :: SchemaEnv
composeEnv = buildSchemaEnv [parseOrDie "cmp.proto" composeProto]


-- | The component schema for a named type, built with the given options.
schemaOf :: Text -> SchemaOptions -> Value
schemaOf fqn opts = case lookup fqn (componentSchemasWith opts composeEnv) of
  Just s -> s
  Nothing -> error ("Test.Protovalidate.OpenAPICompose: no component " <> T.unpack fqn)


-- | The @properties@ object of a message component.
propsOf :: Text -> SchemaOptions -> Value
propsOf fqn opts = lookupObj "properties" (schemaOf fqn opts)


-- ---------------------------------------------------------------------------
-- Headline fixture: a field both deprecated AND validated
-- ---------------------------------------------------------------------------

-- | @code@ is @[deprecated = true]@ AND has @string.min_len = 3@ (dotted
-- buf.validate form, no import needed).
headlineProto :: Text
headlineProto =
  T.unlines
    [ "syntax = \"proto3\";"
    , "package hl.v1;"
    , "message Rec {"
    , "  string code = 1 ["
    , "    deprecated = true,"
    , "    (buf.validate.field).string.min_len = 3"
    , "  ];"
    , "  string plain = 2;"
    , "}"
    ]


headlinePf :: ProtoFile
headlinePf = parseOrDie "hl.proto" headlineProto


headlineRules :: [(Text, MessageRules)]
headlineRules = case fileMessageRules headlinePf of
  Left e -> error ("Test.Protovalidate.OpenAPICompose: fileMessageRules failed: " <> T.unpack e)
  Right rs -> rs


-- | The @hl.v1.Rec@ @properties@ object under the given options.
headlineProps :: SchemaOptions -> Value
headlineProps opts =
  let env = buildSchemaEnv [headlinePf]
  in case lookup "hl.v1.Rec" (componentSchemasWith opts env) of
       Just s -> lookupObj "properties" s
       Nothing -> error "Test.Protovalidate.OpenAPICompose: no hl.v1.Rec component"


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

parseOrDie :: FilePath -> Text -> ProtoFile
parseOrDie fp src = case parseProtoFile fp src of
  Left err -> error (renderParseError err)
  Right pf -> pf


lookupKey :: Text -> Value -> Maybe Value
lookupKey k (Object o) = AKM.lookup (AKey.fromText k) o
lookupKey _ _ = Nothing


lookupObj :: Text -> Value -> Value
lookupObj k v = case lookupKey k v of
  Just o -> o
  Nothing -> error ("missing object key: " <> T.unpack k)


-- | A single keyword value on the schema of a named property.
fieldKw :: Value -> Text -> Text -> Maybe Value
fieldKw props field kw = lookupKey field props >>= lookupKey kw


-- | The schema object of a named property (error if absent).
fieldSchema :: Value -> Text -> Value
fieldSchema props field = case lookupKey field props of
  Just s -> s
  Nothing -> error ("missing property: " <> T.unpack field)


objKeys :: Value -> [Text]
objKeys (Object o) = map AKey.toText (AKM.keys o)
objKeys _ = []


-- | The @required@ array of a message schema, as a list of values.
requiredNames :: Value -> Maybe [Value]
requiredNames v = case lookupKey "required" v of
  Just (Array xs) -> Just (V.toList xs)
  _ -> Nothing
