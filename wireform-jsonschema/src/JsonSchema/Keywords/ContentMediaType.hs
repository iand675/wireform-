{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Pluggable keyword definition for the 'contentMediaType' keyword (Draft-07+)
--
-- Validates that string content matches the specified media type.
-- When contentEncoding is also present, validates the decoded content.
module JsonSchema.Keywords.ContentMediaType
  ( contentMediaTypeKeyword
  , compileContentMediaType
  , ContentMediaTypeData(..)
  ) where

import Data.Aeson (Value(..))
import qualified Data.Map.Strict as Map
import Control.Monad.Reader (ask)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as Base64
import Data.Typeable (Typeable)

import JsonSchema.Types 
  ( ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  , ValidationConfig(..), ValidationErrors(..), emptyPointer
  , JsonSchemaVersion(..), Schema(..), SchemaCore(..), SchemaObject(..)
  , schemaValidation, validationContentEncoding, schemaRawKeywords
  )
import JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..)
  )
import qualified JsonSchema.Validator.Result as VR

-- | Compiled data for contentMediaType keyword
-- Stores the media type and optionally the encoding if contentEncoding is also present
data ContentMediaTypeData = ContentMediaTypeData
  { mediaType :: Text
  , contentEncoding :: Maybe Text  -- Encoding from contentEncoding keyword, if present
  }
  deriving (Show, Eq, Typeable)

-- | Compile the contentMediaType keyword
compileContentMediaType :: CompileFunc ContentMediaTypeData
compileContentMediaType (String mediaType) schema _ctx =
  -- Check if contentEncoding is also present in the schema
  -- First check schemaRawKeywords (raw JSON), then check parsed schemaValidation
  let encoding = case Map.lookup "contentEncoding" (schemaRawKeywords schema) of
        Just (String enc) -> Just enc
        _ -> case schemaCore schema of
          ObjectSchema obj -> validationContentEncoding (schemaValidation obj)
          _ -> Nothing
  in Right $ ContentMediaTypeData mediaType encoding
compileContentMediaType _ _ _ = Left "contentMediaType must be a string"

-- | Validate contentMediaType
-- 
-- For Draft-07, contentMediaType always validates (assertion mode).
-- For 2019-09+, it's an annotation unless content-assertion is enabled.
-- When contentEncoding is present, decodes the content before validating.
validateContentMediaTypeKeyword :: ValidateFunc ContentMediaTypeData
validateContentMediaTypeKeyword _recursiveValidator (ContentMediaTypeData mediaType encoding) _ctx (String content) = do
  config <- ask
  -- Draft-07 always validates content (assertion mode)
  -- 2019-09+ uses content-assertion flag
  let version = validationVersion config
      shouldAssert = case version of
        Draft07 -> True  -- Draft-07 always validates content
        _ -> validationContentAssertion config  -- Use config flag for other versions
      -- If contentEncoding is present, decode the content first
      contentToValidate = case encoding of
        Just "base64" -> decodeBase64 content
        _ -> Right content
      result = case contentToValidate of
        Left err -> validationFailure "contentMediaType" err
        Right decoded -> validateMediaType mediaType decoded
  pure $
    if shouldAssert
      then result
      else ValidationSuccess mempty  -- Annotation mode
validateContentMediaTypeKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to strings

-- | Decode base64 content
decodeBase64 :: Text -> Either Text Text
decodeBase64 text =
  -- Remove whitespace before decoding
  let isWhitespace c = c == ' ' || c == '\n' || c == '\r' || c == '\t'
      cleaned = T.filter (not . isWhitespace) text
  in case Base64.decode (TE.encodeUtf8 cleaned) of
    Left _ -> Left "Invalid base64 encoding"
    Right bytes -> Right (TE.decodeUtf8 bytes)

-- | Validate media type
validateMediaType :: Text -> Text -> ValidationResult
validateMediaType "application/json" content =
  case Aeson.eitherDecodeStrict (TE.encodeUtf8 content) of
    Left err -> validationFailure "contentMediaType" $ "Invalid JSON document: " <> T.pack err
    Right (_ :: Value) -> ValidationSuccess mempty
validateMediaType _ _ =
  -- Unknown media type - just pass (media types are optional to support)
  ValidationSuccess mempty

validationFailure :: Text -> Text -> ValidationResult
validationFailure keyword msg = ValidationFailure $ ValidationErrors $ pure $ VR.ValidationError
  { VR.errorKeyword = keyword
  , VR.errorSchemaPath = emptyPointer
  , VR.errorInstancePath = emptyPointer
  , VR.errorMessage = msg
  }

-- | Keyword definition for contentMediaType
contentMediaTypeKeyword :: KeywordDefinition
contentMediaTypeKeyword = KeywordDefinition
  { keywordName = "contentMediaType"
  , keywordCompile = compileContentMediaType
  , keywordValidate = validateContentMediaTypeKeyword
  , keywordNavigation = NoNavigation  -- No subschemas
  , keywordPostValidate = Nothing
  }

