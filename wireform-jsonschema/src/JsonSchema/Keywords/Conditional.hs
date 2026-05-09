{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of conditional keywords (if/then/else)
--
-- The if/then/else keywords provide conditional schema application (Draft-07+).
-- If the 'if' schema validates, apply 'then' (if present).
-- If the 'if' schema fails, apply 'else' (if present).
--
-- This is an applicator keyword that recursively validates subschemas and
-- properly collects annotations from the applied branch.
module JsonSchema.Keywords.Conditional
  ( validateConditional
  , ifKeyword
  , thenKeyword
  , elseKeyword
  , compileIf
  , IfData(..)
  ) where

import Data.Aeson (Value)
import Control.Monad.Reader (Reader)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)

import JsonSchema.Types 
  ( Schema(..), SchemaCore(..), SchemaObject(..)
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure, ValidationAnnotations
  , schemaIf, schemaThen, schemaElse, schemaRawKeywords
  , schemaVersion, JsonSchemaVersion(..)
  )
import JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..)
  , CompilationContext(..), contextParseSubschema
  )
import JsonSchema.Parser.Internal (parseSchema)
import qualified JsonSchema.Parser.Internal as ParserInternal
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

-- | Validate conditional keywords (if/then/else)
--
-- Parameters:
-- - validateSchema: Recursive validation function for subschemas
-- - ifSchema: The condition schema to test
-- - thenSchema: Optional schema to apply if condition validates
-- - elseSchema: Optional schema to apply if condition fails
-- - value: The instance value to validate
--
-- Returns:
-- - If 'if' validates and 'then' is present: result of 'then' with 'if' annotations
-- - If 'if' validates and 'then' is absent: 'if' annotations only
-- - If 'if' fails and 'else' is present: result of 'else'
-- - If 'if' fails and 'else' is absent: success with no annotations
validateConditional
  :: (Schema -> Value -> ValidationResult)  -- ^ Recursive validator
  -> Schema                                  -- ^ The 'if' condition schema
  -> Maybe Schema                            -- ^ Optional 'then' schema
  -> Maybe Schema                            -- ^ Optional 'else' schema
  -> Value                                   -- ^ Value to validate
  -> ValidationResult
validateConditional validateSchema ifSchema mThenSchema mElseSchema value =
  case validateSchema ifSchema value of
    ValidationSuccess ifAnns ->
      -- If validates, apply then (if present) and combine annotations
      case mThenSchema of
        Just thenSchema ->
          case validateSchema thenSchema value of
            ValidationSuccess thenAnns -> ValidationSuccess (ifAnns <> thenAnns)
            ValidationFailure errs -> ValidationFailure errs
        Nothing -> ValidationSuccess ifAnns  -- No then, keep if annotations
    ValidationFailure _ ->
      -- If fails, apply else (if present)
      case mElseSchema of
        Just elseSchema -> validateSchema elseSchema value
        Nothing -> ValidationSuccess mempty  -- No else, succeed with no annotations

-- | Compiled data for the 'if' keyword
data IfData = IfData
  { ifConditionSchema :: Schema
  , ifThenSchema :: Maybe Schema  -- Optional 'then' from adjacent keyword
  , ifElseSchema :: Maybe Schema  -- Optional 'else' from adjacent keyword
  }
  deriving (Typeable)

-- | Compiled data for 'then' and 'else' keywords (no-op, handled by 'if')
newtype ThenData = ThenData Schema deriving (Typeable)
newtype ElseData = ElseData Schema deriving (Typeable)

-- | Compile the 'if' keyword
compileIf :: CompileFunc IfData
compileIf value schema ctx = do
  -- Parse the 'if' schema
  ifSchema' <- case contextParseSubschema ctx value of
    Left err -> Left $ "Invalid schema in if: " <> err
    Right s -> Right s
  
  -- Read adjacent 'then' and 'else' keywords from raw keywords or pre-parsed fields
  let (thenSchema', elseSchema') = case schemaCore schema of
        ObjectSchema obj -> 
          -- Check if pre-parsed (for backward compatibility)
          case (schemaThen obj, schemaElse obj) of
            (Just thenS, Just elseS) -> (Just thenS, Just elseS)
            (Just thenS, Nothing) -> (Just thenS, parseElseFromRaw schema ctx)
            (Nothing, Just elseS) -> (parseThenFromRaw schema ctx, Just elseS)
            (Nothing, Nothing) -> (parseThenFromRaw schema ctx, parseElseFromRaw schema ctx)
        _ -> (Nothing, Nothing)
  
  Right $ IfData ifSchema' thenSchema' elseSchema'
  where
    parseThenFromRaw s c = Map.lookup "then" (schemaRawKeywords s) >>= \v ->
      case contextParseSubschema c v of
        Right schema -> Just schema
        Left _ -> Nothing
    parseElseFromRaw s c = Map.lookup "else" (schemaRawKeywords s) >>= \v ->
      case contextParseSubschema c v of
        Right schema -> Just schema
        Left _ -> Nothing

-- | Compile the 'then' keyword (no-op, handled by 'if')
compileThen :: CompileFunc ThenData
compileThen value _schema _ctx = case parseSchema value of
  Left err -> Left $ "Invalid schema in then: " <> T.pack (show err)
  Right s -> Right $ ThenData s

-- | Compile the 'else' keyword (no-op, handled by 'if')
compileElse :: CompileFunc ElseData
compileElse value _schema _ctx = case parseSchema value of
  Left err -> Left $ "Invalid schema in else: " <> T.pack (show err)
  Right s -> Right $ ElseData s

-- | Validate using the 'if' keyword (handles all conditional logic)
validateIfKeyword :: ValidateFunc IfData
validateIfKeyword recursiveValidator (IfData ifSchema' thenSchema' elseSchema') _ctx val =
  pure $ validateConditional recursiveValidator ifSchema' thenSchema' elseSchema' val

-- | Validate 'then' keyword (no-op, handled by 'if')
validateThenKeyword :: ValidateFunc ThenData
validateThenKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- No-op, logic is in 'if'

-- | Validate 'else' keyword (no-op, handled by 'if')
validateElseKeyword :: ValidateFunc ElseData
validateElseKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- No-op, logic is in 'if'

-- | Keyword definition for 'if'
ifKeyword :: KeywordDefinition
ifKeyword = KeywordDefinition
  { keywordName = "if"
  , keywordCompile = compileIf
  , keywordValidate = validateIfKeyword
  , keywordNavigation = SingleSchema $ \schema -> case schemaCore schema of
      ObjectSchema obj -> 
        -- Check pre-parsed first, then parse on-demand
        case schemaIf obj of
          Just ifSchema -> Just ifSchema
          Nothing -> parseIfFromRaw schema
      _ -> Nothing
  , keywordPostValidate = Nothing
  }
  where
    parseIfFromRaw :: Schema -> Maybe Schema
    parseIfFromRaw s = case Map.lookup "if" (schemaRawKeywords s) of
      Just val ->
        let version = fromMaybe Draft202012 (schemaVersion s)
        in case ParserInternal.parseSchemaValue version val of
          Right schema -> Just schema
          Left _ -> Nothing
      _ -> Nothing

-- | Keyword definition for 'then'
thenKeyword :: KeywordDefinition
thenKeyword = KeywordDefinition
  { keywordName = "then"
  , keywordCompile = compileThen
  , keywordValidate = validateThenKeyword
  , keywordNavigation = SingleSchema $ \schema -> case schemaCore schema of
      ObjectSchema obj -> 
        -- Check pre-parsed first, then parse on-demand
        case schemaThen obj of
          Just thenSchema -> Just thenSchema
          Nothing -> parseThenFromRaw schema
      _ -> Nothing
  , keywordPostValidate = Nothing
  }
  where
    parseThenFromRaw :: Schema -> Maybe Schema
    parseThenFromRaw s = case Map.lookup "then" (schemaRawKeywords s) of
      Just val ->
        let version = fromMaybe Draft202012 (schemaVersion s)
        in case ParserInternal.parseSchemaValue version val of
          Right schema -> Just schema
          Left _ -> Nothing
      _ -> Nothing

-- | Keyword definition for 'else'
elseKeyword :: KeywordDefinition
elseKeyword = KeywordDefinition
  { keywordName = "else"
  , keywordCompile = compileElse
  , keywordValidate = validateElseKeyword
  , keywordNavigation = SingleSchema $ \schema -> case schemaCore schema of
      ObjectSchema obj -> 
        -- Check pre-parsed first, then parse on-demand
        case schemaElse obj of
          Just elseSchema -> Just elseSchema
          Nothing -> parseElseFromRaw schema
      _ -> Nothing
  , keywordPostValidate = Nothing
  }
  where
    parseElseFromRaw :: Schema -> Maybe Schema
    parseElseFromRaw s = case Map.lookup "else" (schemaRawKeywords s) of
      Just val ->
        let version = fromMaybe Draft202012 (schemaVersion s)
        in case ParserInternal.parseSchemaValue version val of
          Right schema -> Just schema
          Left _ -> Nothing
      _ -> Nothing
