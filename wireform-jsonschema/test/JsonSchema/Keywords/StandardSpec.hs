{-# LANGUAGE PatternSynonyms #-}
module JsonSchema.Keywords.StandardSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..), object, toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Vector (fromList)

import JsonSchema.Keywords.Standard
import JsonSchema.Keyword.Types
import JsonSchema.Keyword (emptyKeywordRegistry)
import JsonSchema.Keyword.Compile (compileKeyword)
import JsonSchema.Types
  ( Schema(..)
  , SchemaCore(..)
  , pattern ValidationSuccess
  )
import JsonSchema.TestHelpers (runValidationErrors)

-- Helper to create a minimal schema
mkSchema :: Value -> Schema
mkSchema v = Schema
  { schemaVersion = Nothing
  , schemaMetaschemaURI = Nothing
  , schemaId = Nothing
  , schemaVocabulary = Nothing
  , schemaCore = ObjectSchema $ error "not used in tests"
  , schemaExtensions = Map.singleton "test-keyword" v
  , schemaRawKeywords = Map.singleton "test-keyword" v
  }

-- Helper to create a compilation context
mkContext :: Schema -> CompilationContext
mkContext schema = CompilationContext
  { contextRegistry = Map.empty
  , contextResolveRef = \_ -> Left "No refs in tests"
  , contextCurrentSchema = schema
  , contextParentPath = []
  , contextKeywordRegistry = emptyKeywordRegistry
  , contextParseSubschema = \_ -> Left "No subschema parsing in tests"
  }


spec :: Spec
spec = describe "Standard Keywords" $ do
  describe "const keyword" $ do
    it "compiles const value" $ do
      let value = toJSON ("hello" :: Text)
          schema = mkSchema value
          ctx = mkContext schema

      case compileKeyword constKeyword value schema ctx of
        Left err -> expectationFailure $ "Compilation failed: " ++ T.unpack err
        Right compiled ->
          compiledKeywordName compiled `shouldBe` "const"

    it "validates matching const value" $ do
      let value = toJSON ("hello" :: Text)
          schema = mkSchema value
          ctx = mkContext schema

      case compileKeyword constKeyword value schema ctx of
        Left err -> expectationFailure $ "Compilation failed: " ++ T.unpack err
        Right compiled -> do
          let recursiveValidator = \_ _ -> ValidationSuccess mempty
              vCtx = ValidationContext' { kwContextInstancePath = [], kwContextSchemaPath = [] }
          runValidationErrors (compiledValidate compiled recursiveValidator vCtx (toJSON ("hello" :: Text)))
            `shouldBe` []

    it "rejects non-matching const value" $ do
      let value = toJSON ("hello" :: Text)
          schema = mkSchema value
          ctx = mkContext schema

      case compileKeyword constKeyword value schema ctx of
        Left err -> expectationFailure $ "Compilation failed: " ++ T.unpack err
        Right compiled -> do
          let recursiveValidator = \_ _ -> ValidationSuccess mempty
              vCtx = ValidationContext' { kwContextInstancePath = [], kwContextSchemaPath = [] }
          let errors = runValidationErrors (compiledValidate compiled recursiveValidator vCtx (toJSON ("world" :: Text)))
          length errors `shouldBe` 1
          head errors `shouldSatisfy` T.isInfixOf "does not match const"

    it "validates const with number" $ do
      let value = toJSON (42 :: Int)
          schema = mkSchema value
          ctx = mkContext schema

      case compileKeyword constKeyword value schema ctx of
        Left err -> expectationFailure $ "Compilation failed: " ++ T.unpack err
        Right compiled -> do
          let recursiveValidator = \_ _ -> ValidationSuccess mempty
              vCtx = ValidationContext' { kwContextInstancePath = [], kwContextSchemaPath = [] }
          runValidationErrors (compiledValidate compiled recursiveValidator vCtx (toJSON (42 :: Int))) `shouldBe` []
          let errors = runValidationErrors (compiledValidate compiled recursiveValidator vCtx (toJSON (43 :: Int)))
          length errors `shouldBe` 1

  describe "enum keyword" $ do
    it "compiles enum array" $ do
      let value = Array $ fromList [toJSON ("red" :: Text), toJSON ("green" :: Text), toJSON ("blue" :: Text)]
          schema = mkSchema value
          ctx = mkContext schema

      case compileKeyword enumKeyword value schema ctx of
        Left err -> expectationFailure $ "Compilation failed: " ++ T.unpack err
        Right compiled -> do
          let recursiveValidator = \_ _ -> ValidationSuccess mempty
              vCtx = ValidationContext' { kwContextInstancePath = [], kwContextSchemaPath = [] }
          compiledKeywordName compiled `shouldBe` "enum"

    it "validates value in enum" $ do
      let value = Array $ fromList [toJSON ("red" :: Text), toJSON ("green" :: Text), toJSON ("blue" :: Text)]
          schema = mkSchema value
          ctx = mkContext schema

      case compileKeyword enumKeyword value schema ctx of
        Left err -> expectationFailure $ "Compilation failed: " ++ T.unpack err
        Right compiled -> do
          let recursiveValidator = \_ _ -> ValidationSuccess mempty
              vCtx = ValidationContext' { kwContextInstancePath = [], kwContextSchemaPath = [] }
          runValidationErrors (compiledValidate compiled recursiveValidator vCtx (toJSON ("red" :: Text))) `shouldBe` []
          runValidationErrors (compiledValidate compiled recursiveValidator vCtx (toJSON ("green" :: Text))) `shouldBe` []
          runValidationErrors (compiledValidate compiled recursiveValidator vCtx (toJSON ("blue" :: Text))) `shouldBe` []

    it "rejects value not in enum" $ do
      let value = Array $ fromList [toJSON ("red" :: Text), toJSON ("green" :: Text), toJSON ("blue" :: Text)]
          schema = mkSchema value
          ctx = mkContext schema

      case compileKeyword enumKeyword value schema ctx of
        Left err -> expectationFailure $ "Compilation failed: " ++ T.unpack err
        Right compiled -> do
          let recursiveValidator = \_ _ -> ValidationSuccess mempty
              vCtx = ValidationContext' { kwContextInstancePath = [], kwContextSchemaPath = [] }
          let errors = runValidationErrors (compiledValidate compiled recursiveValidator vCtx (toJSON ("yellow" :: Text)))
          length errors `shouldBe` 1
          head errors `shouldSatisfy` T.isInfixOf "not in enum"

    it "fails compilation if enum is not an array" $ do
      let value = toJSON ("not-an-array" :: Text)
          schema = mkSchema value
          ctx = mkContext schema

      case compileKeyword enumKeyword value schema ctx of
        Left err -> err `shouldSatisfy` T.isInfixOf "must be an array"
        Right _ -> expectationFailure "Should have failed compilation"

  describe "type keyword" $ do
    it "compiles single type" $ do
      let value = toJSON ("string" :: Text)
          schema = mkSchema value
          ctx = mkContext schema

      case compileKeyword typeKeyword value schema ctx of
        Left err -> expectationFailure $ "Compilation failed: " ++ T.unpack err
        Right compiled -> do
          let recursiveValidator = \_ _ -> ValidationSuccess mempty
              vCtx = ValidationContext' { kwContextInstancePath = [], kwContextSchemaPath = [] }
          compiledKeywordName compiled `shouldBe` "type"

    it "validates string type" $ do
      let value = toJSON ("string" :: Text)
          schema = mkSchema value
          ctx = mkContext schema

      case compileKeyword typeKeyword value schema ctx of
        Left err -> expectationFailure $ "Compilation failed: " ++ T.unpack err
        Right compiled -> do
          let recursiveValidator = \_ _ -> ValidationSuccess mempty
              vCtx = ValidationContext' { kwContextInstancePath = [], kwContextSchemaPath = [] }
          runValidationErrors (compiledValidate compiled recursiveValidator vCtx (toJSON ("hello" :: Text))) `shouldBe` []
          let errors = runValidationErrors (compiledValidate compiled recursiveValidator vCtx (toJSON (42 :: Int)))
          length errors `shouldBe` 1

    it "validates number type" $ do
      let value = toJSON ("number" :: Text)
          schema = mkSchema value
          ctx = mkContext schema

      case compileKeyword typeKeyword value schema ctx of
        Left err -> expectationFailure $ "Compilation failed: " ++ T.unpack err
        Right compiled -> do
          let recursiveValidator = \_ _ -> ValidationSuccess mempty
              vCtx = ValidationContext' { kwContextInstancePath = [], kwContextSchemaPath = [] }
          runValidationErrors (compiledValidate compiled recursiveValidator vCtx (toJSON (42 :: Int))) `shouldBe` []
          runValidationErrors (compiledValidate compiled recursiveValidator vCtx (toJSON (3.14 :: Double))) `shouldBe` []

    it "validates integer type" $ do
      let value = toJSON ("integer" :: Text)
          schema = mkSchema value
          ctx = mkContext schema

      case compileKeyword typeKeyword value schema ctx of
        Left err -> expectationFailure $ "Compilation failed: " ++ T.unpack err
        Right compiled -> do
          let recursiveValidator = \_ _ -> ValidationSuccess mempty
              vCtx = ValidationContext' { kwContextInstancePath = [], kwContextSchemaPath = [] }
          runValidationErrors (compiledValidate compiled recursiveValidator vCtx (toJSON (42 :: Int))) `shouldBe` []
          let errors = runValidationErrors (compiledValidate compiled recursiveValidator vCtx (toJSON (3.14 :: Double)))
          length errors `shouldBe` 1

    it "validates boolean type" $ do
      let value = toJSON ("boolean" :: Text)
          schema = mkSchema value
          ctx = mkContext schema

      case compileKeyword typeKeyword value schema ctx of
        Left err -> expectationFailure $ "Compilation failed: " ++ T.unpack err
        Right compiled -> do
          let recursiveValidator = \_ _ -> ValidationSuccess mempty
              vCtx = ValidationContext' { kwContextInstancePath = [], kwContextSchemaPath = [] }
          runValidationErrors (compiledValidate compiled recursiveValidator vCtx (toJSON True)) `shouldBe` []
          runValidationErrors (compiledValidate compiled recursiveValidator vCtx (toJSON False)) `shouldBe` []

    it "validates null type" $ do
      let value = toJSON ("null" :: Text)
          schema = mkSchema value
          ctx = mkContext schema

      case compileKeyword typeKeyword value schema ctx of
        Left err -> expectationFailure $ "Compilation failed: " ++ T.unpack err
        Right compiled -> do
          let recursiveValidator = \_ _ -> ValidationSuccess mempty
              vCtx = ValidationContext' { kwContextInstancePath = [], kwContextSchemaPath = [] }
          runValidationErrors (compiledValidate compiled recursiveValidator vCtx Null) `shouldBe` []

    it "validates object type" $ do
      let value = toJSON ("object" :: Text)
          schema = mkSchema value
          ctx = mkContext schema

      case compileKeyword typeKeyword value schema ctx of
        Left err -> expectationFailure $ "Compilation failed: " ++ T.unpack err
        Right compiled -> do
          let recursiveValidator = \_ _ -> ValidationSuccess mempty
              vCtx = ValidationContext' { kwContextInstancePath = [], kwContextSchemaPath = [] }
          runValidationErrors (compiledValidate compiled recursiveValidator vCtx (object [])) `shouldBe` []

    it "validates array type" $ do
      let value = toJSON ("array" :: Text)
          schema = mkSchema value
          ctx = mkContext schema

      case compileKeyword typeKeyword value schema ctx of
        Left err -> expectationFailure $ "Compilation failed: " ++ T.unpack err
        Right compiled -> do
          let recursiveValidator = \_ _ -> ValidationSuccess mempty
              vCtx = ValidationContext' { kwContextInstancePath = [], kwContextSchemaPath = [] }
          runValidationErrors (compiledValidate compiled recursiveValidator vCtx (Array $ fromList [])) `shouldBe` []

    it "compiles type union" $ do
      let value = Array $ fromList [toJSON ("string" :: Text), toJSON ("number" :: Text)]
          schema = mkSchema value
          ctx = mkContext schema

      case compileKeyword typeKeyword value schema ctx of
        Left err -> expectationFailure $ "Compilation failed: " ++ T.unpack err
        Right compiled -> do
          let recursiveValidator = \_ _ -> ValidationSuccess mempty
              vCtx = ValidationContext' { kwContextInstancePath = [], kwContextSchemaPath = [] }
          runValidationErrors (compiledValidate compiled recursiveValidator vCtx (toJSON ("hello" :: Text))) `shouldBe` []
          runValidationErrors (compiledValidate compiled recursiveValidator vCtx (toJSON (42 :: Int))) `shouldBe` []
          let errors = runValidationErrors (compiledValidate compiled recursiveValidator vCtx (toJSON True))
          length errors `shouldBe` 1

    it "fails compilation for unknown type" $ do
      let value = toJSON ("unknown-type" :: Text)
          schema = mkSchema value
          ctx = mkContext schema

      case compileKeyword typeKeyword value schema ctx of
        Left err -> err `shouldSatisfy` T.isInfixOf "Unknown type"
        Right _ -> expectationFailure "Should have failed compilation"

  describe "standardKeywordRegistry" $ do
    it "contains all three keywords" $ do
      let regStr = show standardKeywordRegistry
      (T.pack regStr `shouldSatisfy` (\s ->
        T.isInfixOf "const" s &&
        T.isInfixOf "enum" s &&
        T.isInfixOf "type" s))
