{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Pluggable keyword definition for the 'contentEncoding' keyword (Draft-07+)
--
-- Validates that string content is properly encoded (e.g., base64).
module JsonSchema.Keywords.ContentEncoding
  ( contentEncodingKeyword
  , compileContentEncoding
  , ContentEncodingData(..)
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (ask)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base64 as Base64
import Data.Char (isAscii, isAlphaNum)
import Data.Typeable (Typeable)

import JsonSchema.Types 
  ( ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  , ValidationConfig(..), ValidationErrors(..), emptyPointer
  , JsonSchemaVersion(..)
  )
import JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..)
  )
import qualified JsonSchema.Validator.Result as VR

-- | Compiled data for contentEncoding keyword
newtype ContentEncodingData = ContentEncodingData Text
  deriving (Show, Eq, Typeable)

-- | Compile the contentEncoding keyword
compileContentEncoding :: CompileFunc ContentEncodingData
compileContentEncoding (String encoding) _schema _ctx =
  Right $ ContentEncodingData encoding
compileContentEncoding _ _ _ = Left "contentEncoding must be a string"

-- | Validate contentEncoding
-- 
-- For Draft-07, contentEncoding always validates (assertion mode).
-- For 2019-09+, it's an annotation unless content-assertion is enabled.
validateContentEncodingKeyword :: ValidateFunc ContentEncodingData
validateContentEncodingKeyword _recursiveValidator (ContentEncodingData encoding) _ctx (String content) = do
  config <- ask
  -- Draft-07 always validates content (assertion mode)
  -- 2019-09+ uses content-assertion flag
  let version = validationVersion config
      shouldAssert = case version of
        Draft07 -> True  -- Draft-07 always validates content
        _ -> validationContentAssertion config  -- Use config flag for other versions
      result = validateEncoding encoding content
  pure $
    if shouldAssert
      then result
      else ValidationSuccess mempty  -- Annotation mode
validateContentEncodingKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to strings

-- | Validate encoding
validateEncoding :: Text -> Text -> ValidationResult
validateEncoding "base64" text =
  -- First check that the string only contains valid base64 characters
  -- Base64 alphabet: A-Z, a-z, 0-9, +, /, and = for padding
  -- Also allow whitespace (which should be ignored)
  let isWhitespace c = c == ' ' || c == '\n' || c == '\r' || c == '\t'
      isValidBase64Char c = isAscii c && (isAlphaNum c || c == '+' || c == '/' || c == '=' || isWhitespace c)
      isValidBase64Chars t = T.all isValidBase64Char t
  in if isValidBase64Chars text
    then
      -- Remove whitespace and validate decoding
      let cleaned = T.filter (not . isWhitespace) text
      in case Base64.decode (TE.encodeUtf8 cleaned) of
        Left _ -> validationFailure "contentEncoding" "Invalid base64 encoding"
        Right _ -> ValidationSuccess mempty
    else validationFailure "contentEncoding" "Invalid base64 encoding: contains invalid characters"
validateEncoding _ _ =
  -- Unknown encoding - just pass (encodings are optional to support)
  ValidationSuccess mempty

validationFailure :: Text -> Text -> ValidationResult
validationFailure keyword msg = ValidationFailure $ ValidationErrors $ pure $ VR.ValidationError
  { VR.errorKeyword = keyword
  , VR.errorSchemaPath = emptyPointer
  , VR.errorInstancePath = emptyPointer
  , VR.errorMessage = msg
  }

-- | Keyword definition for contentEncoding
contentEncodingKeyword :: KeywordDefinition
contentEncodingKeyword = KeywordDefinition
  { keywordName = "contentEncoding"
  , keywordCompile = compileContentEncoding
  , keywordValidate = validateContentEncodingKeyword
  , keywordNavigation = NoNavigation  -- No subschemas
  , keywordPostValidate = Nothing
  }

