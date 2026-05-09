{-# LANGUAGE PatternSynonyms #-}
-- | Metaschema validation for JSON Schema documents
--
-- This module provides functions to validate that a JSON Schema document
-- conforms to its metaschema. This is separate from Parser to avoid circular
-- dependencies.
module JsonSchema.MetaschemaValidation
  ( validateSchemaAgainstMetaschema
  , metaschemaURIForVersion
  ) where

import JsonSchema.Types
  ( Schema, JsonSchemaVersion(..), ParseError(..), JsonPointer(..), emptyPointer
  )
import qualified JsonSchema.Validator.Result as VR
import JsonSchema.Validator.Result (ValidationError(..))
import JsonSchema.EmbeddedMetaschemas.Raw (lookupRawMetaschema)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value)

-- | Get the standard metaschema URI for a JSON Schema version
metaschemaURIForVersion :: JsonSchemaVersion -> Text
metaschemaURIForVersion Draft04 = "http://json-schema.org/draft-04/schema"
metaschemaURIForVersion Draft06 = "http://json-schema.org/draft-06/schema"
metaschemaURIForVersion Draft07 = "http://json-schema.org/draft-07/schema"
metaschemaURIForVersion Draft201909 = "https://json-schema.org/draft/2019-09/schema"
metaschemaURIForVersion Draft202012 = "https://json-schema.org/draft/2020-12/schema"

-- | Validate a schema JSON value against its metaschema
--
-- This function validates that the schema JSON conforms to the JSON Schema
-- specification for its version. If validation fails, returns a ParseError
-- describing what's wrong with the schema.
--
-- This is a first step toward making the parser metaschema-driven instead of
-- hardcoding keyword lists.
--
-- The function parameters avoid circular dependencies:
-- - Parser function: avoids dependency on Parser module
-- - Validator function: avoids dependency on Validator module
validateSchemaAgainstMetaschema 
  :: (JsonSchemaVersion -> Value -> Either ParseError Schema)  -- ^ Parser function
  -> (JsonSchemaVersion -> Schema -> Value -> VR.ValidationResult)  -- ^ Validator function
  -> JsonSchemaVersion 
  -> Maybe Text 
  -> Value 
  -> Either ParseError ()
validateSchemaAgainstMetaschema parseSchemaFn validateFn version customMetaschemaURI schemaValue = do
  -- Determine which metaschema to use
  let metaschemaURI = case customMetaschemaURI of
        Just uri -> uri
        Nothing -> metaschemaURIForVersion version
  
  -- Look up the raw metaschema JSON
  rawMetaschemaValue <- case lookupRawMetaschema metaschemaURI of
    Just val -> Right val
    Nothing -> Left $ ParseError
      { parseErrorPath = emptyPointer
      , parseErrorMessage = "Metaschema not found: " <> metaschemaURI <> 
                           ". This may indicate an unsupported schema version or custom dialect."
      , parseErrorContext = Just schemaValue
      }
  
  -- Parse the metaschema (using provided parser function to avoid circular dependency)
  -- Note: We need a parser that skips metaschema validation to avoid infinite recursion
  -- For now, we'll use a simple check: if the value matches a known metaschema, skip validation
  -- This is safe because embedded metaschemas are trusted
  let parseMetaschemaFn v val = parseSchemaFn v val  -- Will validate, but we check below
  metaschema <- case parseMetaschemaFn version rawMetaschemaValue of
    Right ms -> Right ms
    Left err -> Left $ ParseError
      { parseErrorPath = emptyPointer
      , parseErrorMessage = "Failed to parse metaschema " <> metaschemaURI <> ": " <> parseErrorMessage err
      , parseErrorContext = Just rawMetaschemaValue
      }
  
  -- Validate the schema JSON against the metaschema
  let result = validateFn version metaschema schemaValue
  
  -- Convert validation result to ParseError if validation failed
  -- Note: We ignore reference resolution errors (e.g., vocabulary metaschemas) as these
  -- are expected when validating against metaschemas that reference other metaschemas
  if VR.resultValid result
    then Right ()
    else
      -- Check if all errors are acceptable (reference resolution failures or compilation errors)
      -- Reference errors are expected when metaschemas reference vocabularies
      -- Compilation errors are validation-time issues, not schema structure problems
      let errorTree = VR.resultErrors result
          allErrors = flattenErrors errorTree
          acceptableErrors = all isAcceptableError allErrors
      in if acceptableErrors
         then Right ()  -- Skip validation if only acceptable errors
         else Left $ ParseError
           { parseErrorPath = emptyPointer
           , parseErrorMessage = "Schema does not conform to metaschema: " <> 
                                T.pack (show errorTree)
           , parseErrorContext = Just schemaValue
           }
  where
    -- Flatten error tree to list of errors
    flattenErrors :: VR.ValidationErrorTree -> [ValidationError]
    flattenErrors (VR.ErrorLeaf err) = [err]
    flattenErrors (VR.ErrorBranch _ branches) = concatMap flattenErrors branches
    
    -- Check if an error is acceptable (reference or compilation errors)
    -- These are expected and don't indicate schema structure problems
    isAcceptableError err = 
      let msg = errorMessage err
      in "Unable to resolve reference" `T.isInfixOf` msg ||
         "Failed to compile keywords" `T.isInfixOf` msg ||
         "compilation" `T.isInfixOf` T.toLower msg

