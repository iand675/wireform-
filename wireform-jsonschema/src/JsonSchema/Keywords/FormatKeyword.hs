{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Pluggable keyword definition for the 'format' keyword
--
-- Wraps the existing format validation logic (in JsonSchema.Keywords.Format)
-- into the pluggable keyword system.
module JsonSchema.Keywords.FormatKeyword
  ( formatKeyword
  , compileFormat
  , FormatData(..)
  ) where

import Data.Aeson (Value(..), FromJSON(..))
import Control.Monad.Reader (ask)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Aeson as Aeson

import JsonSchema.Types 
  ( Format(..)
  , FormatBehavior(..)
  , ValidationConfig(..)
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  )
import JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..)
  )
import qualified JsonSchema.Keywords.Format as FormatImpl

-- | Compiled data for format keyword
newtype FormatData = FormatData Format
  deriving (Show, Eq, Typeable)

-- | Compile the format keyword
compileFormat :: CompileFunc FormatData
compileFormat value _schema _ctx =
  -- Use the existing FromJSON instance for Format to avoid code duplication
  case Aeson.fromJSON value of
    Aeson.Success format -> Right $ FormatData format
    Aeson.Error err -> Left $ "Invalid format value: " <> T.pack err

validateFormatKeyword :: ValidateFunc FormatData
validateFormatKeyword _recursiveValidator (FormatData format) _ctx (String txt) = do
  config <- ask
  let result = FormatImpl.validateFormatValue format txt
      assertionMode = case validationDialectFormatBehavior config of
        Just FormatAssertion -> True
        Just FormatAnnotation -> False
        Nothing -> validationFormatAssertion config
  pure $
    if assertionMode
      then result
      else ValidationSuccess $ case result of
             ValidationSuccess anns -> anns
             ValidationFailure _ -> mempty
validateFormatKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to strings

-- | Keyword definition for format
formatKeyword :: KeywordDefinition
formatKeyword = KeywordDefinition
  { keywordName = "format"
  , keywordCompile = compileFormat
  , keywordValidate = validateFormatKeyword
  , keywordNavigation = NoNavigation  -- No subschemas
  , keywordPostValidate = Nothing
  }

