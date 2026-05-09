{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of the 'anyOf' composition keyword
--
-- The anyOf keyword requires that the instance validates against AT LEAST ONE
-- schema in the provided array. This is an applicator keyword that recursively
-- validates subschemas and collects annotations from all passing branches.
module JsonSchema.Keywords.AnyOf
  ( validateAnyOf
  , anyOfKeyword
  , compileAnyOf
  , AnyOfData(..)
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (Reader)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)

import JsonSchema.Types (Schema(..), SchemaCore(..), SchemaObject(..), ValidationResult, pattern ValidationSuccess, pattern ValidationFailure, ValidationAnnotations, validationFailure, schemaAnyOf, schemaRawKeywords, schemaVersion, JsonSchemaVersion(..))
import JsonSchema.Keyword.Types (KeywordDefinition(..), CompileFunc, ValidateFunc, ValidationContext'(..), KeywordNavigation(..), CompilationContext(..))
import JsonSchema.Keyword.Compile (compileKeyword)
import JsonSchema.Parser.Internal (parseSchema)
import qualified JsonSchema.Parser.Internal as ParserInternal
import JsonSchema.Types (ParseError)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Foldable (toList)

-- | Compiled data for anyOf keyword
newtype AnyOfData = AnyOfData (NonEmpty Schema)
  deriving (Typeable)

-- | Compile the anyOf keyword
compileAnyOf :: CompileFunc AnyOfData
compileAnyOf value schema ctx =
  case schemaCore schema of
    ObjectSchema obj ->
      case schemaAnyOf obj of
        Just schemas -> Right (AnyOfData schemas)
        Nothing -> parseFromValue
    _ -> parseFromValue
  where
    parseFromValue = case value of
      Array arr | not (V.null arr) -> do
        parsedSchemas <- mapM parseSchemaElem (V.toList arr)
        case NE.nonEmpty parsedSchemas of
          Just schemas' -> Right (AnyOfData schemas')
          Nothing -> Left "anyOf must contain at least one schema"
      _ -> Left "anyOf must be a non-empty array"

    parseSchemaElem v = case contextParseSubschema ctx v of
      Left err -> Left $ "Invalid schema in anyOf: " <> err
      Right s -> Right s

-- | Validate anyOf using the pluggable keyword system
validateAnyOfKeyword :: ValidateFunc AnyOfData
validateAnyOfKeyword recursiveValidator (AnyOfData schemas) _ctx value =
  pure $ validateAnyOf recursiveValidator schemas value

-- | Validate that a value satisfies AT LEAST ONE schema in anyOf
--
-- Parameters:
-- - validateSchema: Recursive validation function for subschemas
-- - schemas: The list of schemas, at least one must validate
-- - value: The instance value to validate
--
-- Returns:
-- - ValidationSuccess with combined annotations from ALL passing branches
-- - ValidationFailure if NO branches pass
validateAnyOf
  :: (Schema -> Value -> ValidationResult)  -- ^ Recursive validator
  -> NonEmpty Schema                        -- ^ Schemas, at least one must validate
  -> Value                                   -- ^ Value to validate
  -> ValidationResult
validateAnyOf validateSchema schemas value =
  let results = map (flip validateSchema value) (NE.toList schemas)
      successes = do
        ValidationSuccess anns <- results
        pure anns
  in if null successes
    then validationFailure "anyOf" "Value does not match any schema in anyOf"
    -- Collect annotations from ALL passing branches in anyOf
    else ValidationSuccess $ mconcat successes

-- | Keyword definition for anyOf
anyOfKeyword :: KeywordDefinition
anyOfKeyword = KeywordDefinition
  { keywordName = "anyOf"
  , keywordCompile = compileAnyOf
  , keywordValidate = validateAnyOfKeyword
  , keywordNavigation = SchemaArray $ \schema -> case schemaCore schema of
      ObjectSchema obj -> 
        -- Check pre-parsed first, then parse on-demand
        case schemaAnyOf obj of
          Just anyOfSchemas -> Just (NE.toList anyOfSchemas)
          Nothing -> parseAnyOfFromRaw schema
      _ -> Nothing
  , keywordPostValidate = Nothing
  }
  where
    parseAnyOfFromRaw :: Schema -> Maybe [Schema]
    parseAnyOfFromRaw s = case Map.lookup "anyOf" (schemaRawKeywords s) of
      Just (Array arr) ->
        let version = fromMaybe Draft202012 (schemaVersion s)
            schemas = do
              val <- toList arr
              Right schema <- pure $ ParserInternal.parseSchemaValue version val
              pure schema
        in if null schemas && not (null arr)
           then Nothing  -- Had items but all failed to parse
           else Just schemas
      _ -> Nothing
