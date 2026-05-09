-- | Extended parser with custom keyword support
--
-- This module extends the standard parser to support custom keywords
-- from a keyword registry. It handles:
-- - Recognizing custom keywords during parsing
-- - Compiling custom keywords immediately
-- - Storing compiled keywords alongside the schema
module JsonSchema.Parser.Extended
  ( -- * Extended Parsing
    parseSchemaWithRegistry
  , parseSchemaWithRegistryAndVersion
    -- * Extended Schema Type
  , ExtendedSchema(..)
  , toExtendedSchema
  , fromExtendedSchema
  ) where

import Data.Aeson (Value(..), Object)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

import JsonSchema.Types
  ( Schema
  , JsonSchemaVersion
  , schemaExtensions
  , emptyPointer
  )
import JsonSchema.Parser (parseSchema, parseSchemaWithVersion, ParseError(..))
import JsonSchema.Keyword (KeywordRegistry, lookupKeyword, getRegisteredKeywords)
import JsonSchema.Keyword.Compile
  ( CompiledKeywords
  , emptyCompiledKeywords
  , compileKeywords
  , buildCompilationContext
  )

-- | Schema with compiled custom keywords
--
-- This wraps a standard Schema and adds compiled custom keywords.
-- The compiled keywords are ready for validation without re-compilation.
data ExtendedSchema = ExtendedSchema
  { extendedBaseSchema :: Schema
    -- ^ The standard schema without custom keywords
  , extendedCompiledKeywords :: CompiledKeywords
    -- ^ Compiled custom keywords from the registry
  , extendedCustomKeywordValues :: Map Text Value
    -- ^ Raw values of custom keywords (for introspection)
  }
  deriving (Show)

-- | Convert a standard Schema to an ExtendedSchema with no custom keywords
toExtendedSchema :: Schema -> ExtendedSchema
toExtendedSchema schema = ExtendedSchema
  { extendedBaseSchema = schema
  , extendedCompiledKeywords = emptyCompiledKeywords
  , extendedCustomKeywordValues = Map.empty
  }

-- | Extract the base Schema from an ExtendedSchema
fromExtendedSchema :: ExtendedSchema -> Schema
fromExtendedSchema = extendedBaseSchema

-- | Parse a schema with custom keyword support
--
-- This function:
-- 1. Parses the schema using the standard parser
-- 2. Extracts custom keyword values from schemaExtensions
-- 3. Compiles custom keywords using the registry
-- 4. Returns an ExtendedSchema with compiled keywords
parseSchemaWithRegistry
  :: KeywordRegistry         -- ^ Registry of custom keywords
  -> Map Text Schema         -- ^ Schema registry for $ref resolution
  -> Value                   -- ^ JSON value to parse
  -> Either ParseError ExtendedSchema
parseSchemaWithRegistry registry schemaRegistry value = do
  -- Parse using standard parser
  baseSchema <- parseSchema value

  -- Compile custom keywords
  compiledKeywords <- compileCustomKeywords registry schemaRegistry baseSchema

  return $ ExtendedSchema
    { extendedBaseSchema = baseSchema
    , extendedCompiledKeywords = compiledKeywords
    , extendedCustomKeywordValues = extractCustomKeywords registry baseSchema
    }

-- | Parse with explicit version and custom keywords
parseSchemaWithRegistryAndVersion
  :: KeywordRegistry         -- ^ Registry of custom keywords
  -> Map Text Schema         -- ^ Schema registry for $ref resolution
  -> JsonSchemaVersion       -- ^ JSON Schema version
  -> Value                   -- ^ JSON value to parse
  -> Either ParseError ExtendedSchema
parseSchemaWithRegistryAndVersion registry schemaRegistry version value = do
  -- Parse using standard parser with version
  baseSchema <- parseSchemaWithVersion version value

  -- Compile custom keywords
  compiledKeywords <- compileCustomKeywords registry schemaRegistry baseSchema

  return $ ExtendedSchema
    { extendedBaseSchema = baseSchema
    , extendedCompiledKeywords = compiledKeywords
    , extendedCustomKeywordValues = extractCustomKeywords registry baseSchema
    }

-- | Extract custom keyword values from a schema
--
-- Checks schemaExtensions for keywords that are registered in the registry.
extractCustomKeywords :: KeywordRegistry -> Schema -> Map Text Value
extractCustomKeywords registry schema =
  let extensions = schemaExtensions schema
      registeredNames = Set.fromList (getRegisteredKeywords registry)
  in Map.filterWithKey (\k _ -> Set.member k registeredNames) extensions

-- | Compile custom keywords found in a schema
--
-- Takes the schema's extensions, filters for registered keywords,
-- and compiles them using the keyword registry.
compileCustomKeywords
  :: KeywordRegistry
  -> Map Text Schema
  -> Schema
  -> Either ParseError CompiledKeywords
compileCustomKeywords registry schemaRegistry schema = do
  -- Extract custom keyword values
  let customKeywordValues = extractCustomKeywords registry schema

  -- Build compilation context
  let ctx = buildCompilationContext schemaRegistry registry schema []

  -- Get keyword definitions from registry
  let definitions = Map.fromList
        [ (name, def)
        | name <- Map.keys customKeywordValues
        , Just def <- [lookupKeyword name registry]
        ]

  -- Compile keywords
  case compileKeywords definitions customKeywordValues schema ctx of
    Left err -> Left $ ParseError
      { parseErrorPath = emptyPointer
      , parseErrorMessage = "Failed to compile custom keyword: " <> err
      , parseErrorContext = Nothing
      }
    Right compiled -> Right compiled

