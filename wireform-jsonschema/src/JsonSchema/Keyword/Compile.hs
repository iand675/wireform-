-- | Keyword compilation phase
--
-- This module implements the compile phase of the two-phase keyword system.
-- During compilation, keyword values are processed and prepared for efficient
-- validation. The compile phase has access to the schema registry for
-- resolving references.
module JsonSchema.Keyword.Compile
  ( -- * Compilation
    compileKeyword
  , compileKeywords
  , buildCompilationContext
    -- * Compiled Results
  , CompiledKeywords(..)
  , emptyCompiledKeywords
  , addCompiledKeyword
  , lookupCompiledKeyword
  ) where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable, cast)
import Data.Traversable (traverse)
import Data.Foldable (foldl')

import JsonSchema.Keyword.Types
import JsonSchema.Types (Schema, schemaVersion, JsonSchemaVersion(..))
import qualified JsonSchema.Parser.Internal as ParserInternal
import Data.Maybe (fromMaybe)

-- | Collection of compiled keywords
--
-- Stores the results of compiling all keywords in a schema.
-- Used during validation to look up compiled keyword data.
data CompiledKeywords = CompiledKeywords
  { compiledKeywordMap :: Map Text CompiledKeyword
    -- ^ Map from keyword name to compiled data
  }

instance Show CompiledKeywords where
  show (CompiledKeywords m) =
    "CompiledKeywords{keywords=" ++ show (Map.keys m) ++ "}"

-- | Empty compiled keywords collection
emptyCompiledKeywords :: CompiledKeywords
emptyCompiledKeywords = CompiledKeywords Map.empty

-- | Add a compiled keyword to the collection
addCompiledKeyword :: CompiledKeyword -> CompiledKeywords -> CompiledKeywords
addCompiledKeyword ck@(CompiledKeyword name _ _ _ _) (CompiledKeywords m) =
  CompiledKeywords (Map.insert name ck m)

-- | Look up compiled keyword data by name
lookupCompiledKeyword :: Text -> CompiledKeywords -> Maybe CompiledKeyword
lookupCompiledKeyword name (CompiledKeywords m) = Map.lookup name m

-- | Build a compilation context from a schema registry
--
-- Creates the context that will be passed to keyword compile functions.
-- The context provides access to the schema registry and a reference
-- resolution function.
buildCompilationContext
  :: Map Text Schema         -- ^ Schema registry
  -> KeywordRegistry         -- ^ Keyword registry
  -> Schema                  -- ^ Current schema being compiled
  -> [Text]                  -- ^ Parent path for error reporting
  -> CompilationContext
buildCompilationContext registry keywordRegistry schema parentPath =
  CompilationContext
    { contextRegistry = registry
    , contextResolveRef = resolveRef registry
    , contextCurrentSchema = schema
    , contextParentPath = parentPath
    , contextKeywordRegistry = keywordRegistry
    , contextParseSubschema = parseSubschemaWithContext schema
    }
  where
    -- Simple reference resolution - looks up URI in registry
    resolveRef :: Map Text Schema -> Text -> Either Text Schema
    resolveRef reg uri =
      case Map.lookup uri reg of
        Just s -> Right s
        Nothing -> Left $ "Reference not found: " <> uri
    
    -- Parse a subschema value with the parent schema's version and base URI context
    parseSubschemaWithContext :: Schema -> Value -> Either Text Schema
    parseSubschemaWithContext parentSchema value =
      -- Inherit parent version unless subschema declares its own $schema
      let parentVersion = fromMaybe Draft202012 (schemaVersion parentSchema)
      in case ParserInternal.parseSchemaValue parentVersion value of
        Left err -> Left $ T.pack (show err)
        Right s -> Right s

-- | Compile a single keyword
--
-- Executes the keyword's compile function with the provided value,
-- schema, and context. Returns either an error or the compiled keyword.
compileKeyword
  :: KeywordDefinition       -- ^ Keyword definition with compile function
  -> Value                   -- ^ Keyword value from schema
  -> Schema                  -- ^ Schema containing the keyword
  -> CompilationContext      -- ^ Compilation context
  -> Either Text CompiledKeyword
compileKeyword (KeywordDefinition name compile validate _nav postValidate) value schema ctx = do
  -- Execute the compile function
  compiledData <- compile value schema ctx

  -- Wrap in existential type
  let someData = SomeCompiledData compiledData

  -- Create type-erased validate function (closure over compiled data)
  -- The recursive validator will be provided at validation time
  let validateErased recursiveValidator valCtx val = validate recursiveValidator compiledData valCtx val

  -- Create type-erased post-validation function if provided
  let postValidateErased = case postValidate of
        Just postVal -> Just $ \recursiveValidator valCtx val anns -> postVal recursiveValidator compiledData valCtx val anns
        Nothing -> Nothing

  -- Create compiled keyword (no adjacent data collection yet)
  return $ CompiledKeyword
    { compiledKeywordName = name
    , compiledData = someData
    , compiledValidate = validateErased
    , compiledPostValidate = postValidateErased
    , compiledAdjacentData = Map.empty  -- TODO: Extract adjacent keywords
    }

-- | Compile all keywords in a schema
--
-- Takes a map of keyword values from a schema and compiles each one
-- using the provided keyword definitions. Returns either the first error
-- encountered or a collection of all compiled keywords.
compileKeywords
  :: Map Text KeywordDefinition  -- ^ Available keyword definitions
  -> Map Text Value              -- ^ Keyword values from schema
  -> Schema                      -- ^ Schema being compiled
  -> CompilationContext          -- ^ Compilation context
  -> Either Text CompiledKeywords
compileKeywords definitions keywordValues schema ctx =
  -- Traverse over keyword values, compiling each one
  -- This is clearer than foldl and stops on first error
  fmap (foldl' (flip (maybe id addCompiledKeyword)) emptyCompiledKeywords) $
    traverse compileKeywordIfRegistered (Map.toList keywordValues)
  where
    compileKeywordIfRegistered :: (Text, Value) -> Either Text (Maybe CompiledKeyword)
    compileKeywordIfRegistered (name, value) =
      case Map.lookup name definitions of
        Nothing ->
          -- Keyword not registered - skip it (allows unknown keywords)
          Right Nothing
        Just def -> do
          -- Compile the keyword
          ck <- compileKeyword def value schema ctx
          Right (Just ck)
