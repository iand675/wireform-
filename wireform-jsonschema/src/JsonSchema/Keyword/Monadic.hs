{-# LANGUAGE ScopedTypeVariables #-}
-- | Monadic compilation helpers for keyword compilation
--
-- This module provides helper functions for monadic keyword compilation,
-- including lazy on-demand compilation of adjacent keywords.
module JsonSchema.Keyword.Monadic
  ( -- * Adjacent Keyword Access
    compileAdjacent
  , getAdjacentData
  , getAdjacentValue
  , getKeywordValue
    -- * Initialization
  , initCompilationState
  ) where

import Control.Monad (when)
import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Typeable (Typeable, cast)

import JsonSchema.Keyword.Types
import JsonSchema.Types (Schema(..))

-- | Initialize compilation state for a schema
initCompilationState :: Schema -> CompilationContext -> CompilationState
initCompilationState schema ctx = CompilationState
  { stateCompiled = Map.empty
  , stateCompiling = Set.empty
  , stateSchema = schema
  , stateContext = ctx
  }

-- | Get the value of a keyword from the current schema
--
-- Returns Nothing if the keyword is not present in the schema.
-- This function extracts keyword values from schemaRawKeywords, which stores
-- all keywords in their original Value form for use during monadic compilation.
--
-- This works with dynamically registered keywords since schemaRawKeywords contains
-- ALL keywords from the parsed JSON, not just known/structured ones.
getKeywordValue :: Text -> Schema -> Maybe Value
getKeywordValue keywordName schema =
  Map.lookup keywordName (schemaRawKeywords schema)

-- | Request compilation of an adjacent keyword
--
-- This function implements lazy, memoized compilation with cycle detection:
-- 1. Check if already compiled (memoization)
-- 2. Check for circular dependencies
-- 3. Compile the keyword if needed
-- 4. Store result and return
compileAdjacent :: Text -> CompileM (Maybe CompiledKeyword)
compileAdjacent keywordName = do
  state <- getCompilationState

  -- Check if already compiled (memoization)
  case Map.lookup keywordName (stateCompiled state) of
    Just compiled -> pure (Just compiled)
    Nothing -> do
      -- Check for circular dependency
      when (Set.member keywordName (stateCompiling state)) $
        liftEither $ Left $ "Circular keyword dependency detected: " <> keywordName

      -- Try to get the keyword value from schema
      case getKeywordValue keywordName (stateSchema state) of
        Nothing -> pure Nothing  -- Keyword not present
        Just value -> do
          -- Look up keyword definition in registry
          let ctx = stateContext state
              registry = contextKeywordRegistry ctx

          case Map.lookup keywordName (keywordMap registry) of
            Nothing -> pure Nothing  -- Keyword not registered
            Just (KeywordDefinition _name compile validate _nav _) -> do
              -- Mark as currently compiling
              modifyCompilationState $ \s ->
                s { stateCompiling = Set.insert keywordName (stateCompiling s) }

              -- Compile the keyword
              result <- liftEither $ compile value (stateSchema state) ctx

              -- Create the compiled keyword
              let someData = SomeCompiledData result
                  -- Create closure over compiled data, recursive validator will be provided at validation time
                  validateErased recursiveValidator valCtx val = validate recursiveValidator result valCtx val
                  compiled = CompiledKeyword
                    { compiledKeywordName = keywordName
                    , compiledData = someData
                    , compiledValidate = validateErased
                    , compiledPostValidate = Nothing  -- Monadic compilation doesn't support post-validation yet
                    , compiledAdjacentData = Map.empty
                    }

              -- Store the compiled keyword
              modifyCompilationState $ \s ->
                s { stateCompiled = Map.insert keywordName compiled (stateCompiled s)
                  , stateCompiling = Set.delete keywordName (stateCompiling s)
                  }

              pure (Just compiled)

-- | Get typed compiled data from an adjacent keyword
--
-- This is the main API for keywords that need to access adjacent keywords.
-- It handles:
-- - Lazy compilation of the adjacent keyword
-- - Type-safe extraction of the compiled data
-- - Graceful handling of missing or mistyped keywords
getAdjacentData :: forall a. Typeable a => Text -> CompileM (Maybe a)
getAdjacentData keywordName = do
  mCompiled <- compileAdjacent keywordName
  pure $ mCompiled >>= extractTypedData
  where
    extractTypedData :: CompiledKeyword -> Maybe a
    extractTypedData (CompiledKeyword _ (SomeCompiledData dat) _ _ _) = cast dat

-- | Get the raw Value of an adjacent keyword from the schema
--
-- This is useful for keywords that just need to check if another keyword
-- is present, without needing its compiled data.
getAdjacentValue :: Text -> CompileM (Maybe Value)
getAdjacentValue keywordName = do
  state <- getCompilationState
  pure $ getKeywordValue keywordName (stateSchema state)
