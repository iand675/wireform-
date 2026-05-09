{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of the 'allOf' composition keyword
--
-- The allOf keyword requires that the instance validates against ALL schemas
-- in the provided array. This is an applicator keyword that recursively 
-- validates subschemas and collects annotations from all branches.
module JsonSchema.Keywords.AllOf
  ( validateAllOf
  , allOfKeyword
  , compileAllOf
  , AllOfData(..)
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (Reader)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (sconcat)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)

import JsonSchema.Types (Schema(..), SchemaCore(..), SchemaObject(..), ValidationResult, pattern ValidationSuccess, pattern ValidationFailure, ValidationErrors, ValidationAnnotations, schemaAllOf, schemaRawKeywords, schemaVersion, JsonSchemaVersion(..))
import JsonSchema.Keyword.Types (KeywordDefinition(..), CompileFunc, ValidateFunc, ValidationContext'(..), KeywordNavigation(..), CompilationContext(..))
import JsonSchema.Keyword.Compile (compileKeyword)
import JsonSchema.Parser.Internal (parseSchema)
import qualified JsonSchema.Parser.Internal as ParserInternal
import JsonSchema.Types (ParseError)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Foldable (toList)

-- | Compiled data for allOf keyword
newtype AllOfData = AllOfData (NonEmpty Schema)
  deriving (Typeable)

-- | Compile the allOf keyword
compileAllOf :: CompileFunc AllOfData
compileAllOf value schema ctx = case value of
  Array arr | not (V.null arr) -> do
    -- Parse each element as a schema using context-aware parser
    parsedSchemas <- mapM (contextParseSubschema ctx) (V.toList arr)
    case NE.nonEmpty parsedSchemas of
      Just schemas' -> Right (AllOfData schemas')
      Nothing -> Left "allOf must contain at least one schema"
  _ -> Left "allOf must be a non-empty array"

-- | Validate allOf using the pluggable keyword system
validateAllOfKeyword :: ValidateFunc AllOfData
validateAllOfKeyword recursiveValidator (AllOfData schemas) _ctx value =
  pure $ validateAllOf recursiveValidator schemas value

-- | Validate that a value satisfies ALL schemas in allOf
--
-- Parameters:
-- - validateSchema: Recursive validation function for subschemas
-- - schemas: The list of schemas that all must validate
-- - value: The instance value to validate
--
-- Returns:
-- - ValidationSuccess with combined annotations from all branches (all must pass)
-- - ValidationFailure with combined errors if any branch fails
validateAllOf 
  :: (Schema -> Value -> ValidationResult)  -- ^ Recursive validator
  -> NonEmpty Schema                        -- ^ Schemas that all must validate
  -> Value                                   -- ^ Value to validate
  -> ValidationResult
validateAllOf validateSchema schemas value =
  let results = map (flip validateSchema value) (NE.toList schemas)
      failures = do
        ValidationFailure errs <- results
        pure errs
      annotations = do
        ValidationSuccess anns <- results
        pure anns
  in case failures of
    -- Collect annotations from ALL branches in allOf (all must pass)
    [] -> ValidationSuccess $ mconcat annotations
    failures' -> ValidationFailure $ sconcat (NE.fromList failures')

-- | Keyword definition for allOf
allOfKeyword :: KeywordDefinition
allOfKeyword = KeywordDefinition
  { keywordName = "allOf"
  , keywordCompile = compileAllOf
  , keywordValidate = validateAllOfKeyword
  , keywordNavigation = SchemaArray $ \schema -> case schemaCore schema of
      ObjectSchema obj -> 
        -- Check pre-parsed first, then parse on-demand
        case schemaAllOf obj of
          Just allOfSchemas -> Just (NE.toList allOfSchemas)
          Nothing -> parseAllOfFromRaw schema
      _ -> Nothing
  , keywordPostValidate = Nothing
  }
  where
    parseAllOfFromRaw :: Schema -> Maybe [Schema]
    parseAllOfFromRaw s = case Map.lookup "allOf" (schemaRawKeywords s) of
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
