{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'contains', 'minContains', and 'maxContains' keywords
--
-- The contains keyword requires that at least one array item validates against
-- the specified schema. By default, at least 1 item must match (minContains = 1).
-- minContains and maxContains can adjust this requirement.
module JsonSchema.Keywords.Contains
  ( containsKeyword
  , minContainsKeyword
  , maxContainsKeyword
  , compileContains
  , ContainsData(..)
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (Reader)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Foldable (toList)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Data.Scientific as Sci
import Data.Maybe (fromMaybe)

import JsonSchema.Types 
  ( Schema(..), SchemaCore(..), SchemaObject(..)
  , ValidationResult, validationFailure, pattern ValidationSuccess, pattern ValidationFailure
  , validationContains, validationMinContains, validationMaxContains, schemaValidation
  , schemaRawKeywords, schemaVersion, JsonSchemaVersion(..)
  )
import qualified Data.Aeson.Types as AesonTypes
import qualified JsonSchema.Parser.Internal as ParserInternal
import qualified Data.Map.Strict as Map
import JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..)
  )
import JsonSchema.Parser.Internal (parseSchema)
import JsonSchema.Validator.Annotations
  ( annotateItems
  , arrayIndexPointer
  , shiftAnnotations
  )

-- | Compiled data for contains keyword
data ContainsData = ContainsData 
  { containsSchema :: Schema
  , containsMinCount :: Natural  -- minContains value (default 1 when contains is present)
  , containsMaxCount :: Maybe Natural  -- maxContains value (if specified)
  }
  deriving (Typeable)

-- | Compiled data for minContains keyword
newtype MinContainsData = MinContainsData Natural
  deriving (Show, Eq, Typeable)

-- | Compiled data for maxContains keyword
newtype MaxContainsData = MaxContainsData Natural
  deriving (Show, Eq, Typeable)

-- | Compile the contains keyword
compileContains :: CompileFunc ContainsData
compileContains value schema _ctx = case parseSchema value of
  Left err -> Left $ "Invalid schema in contains: " <> T.pack (show err)
  Right containsSchema ->
    -- Read minContains and maxContains from adjacent keywords in the schema
    -- Parse on-demand from raw keywords if not pre-parsed
    case schemaCore schema of
      ObjectSchema obj ->
        let validation = schemaValidation obj
            minVal = case validationMinContains validation of
              Just m -> Just m
              Nothing -> parseMinContainsFromRaw schema
            maxVal = case validationMaxContains validation of
              Just m -> Just m
              Nothing -> parseMaxContainsFromRaw schema
            minCount = maybe 1 fromIntegral minVal
            maxCount = fmap fromIntegral maxVal
        in Right $ ContainsData containsSchema minCount maxCount
      _ -> Left "contains can only appear in object schemas"
  where
    parseMinContainsFromRaw :: Schema -> Maybe Natural
    parseMinContainsFromRaw s = case Map.lookup "minContains" (schemaRawKeywords s) of
      Just (Number n) | Sci.isInteger n && n >= 0 ->
        Just (fromInteger $ truncate n)
      _ -> Nothing
    
    parseMaxContainsFromRaw :: Schema -> Maybe Natural
    parseMaxContainsFromRaw s = case Map.lookup "maxContains" (schemaRawKeywords s) of
      Just (Number n) | Sci.isInteger n && n >= 0 ->
        Just (fromInteger $ truncate n)
      _ -> Nothing

-- | Compile the minContains keyword
compileMinContains :: CompileFunc MinContainsData
compileMinContains value _schema _ctx = case value of
  Number n | Sci.isInteger n && n >= 0 ->
    Right $ MinContainsData (fromInteger $ truncate n)
  _ -> Left "minContains must be a non-negative integer"

-- | Compile the maxContains keyword
compileMaxContains :: CompileFunc MaxContainsData
compileMaxContains value _schema _ctx = case value of
  Number n | Sci.isInteger n && n >= 0 ->
    Right $ MaxContainsData (fromInteger $ truncate n)
  _ -> Left "maxContains must be a non-negative integer"

-- | Validate contains using the pluggable keyword system
--
-- This keyword checks that at least minContains (default 1) items match
-- the schema, and at most maxContains items match (if maxContains is specified).
validateContainsKeyword :: ValidateFunc ContainsData
validateContainsKeyword recursiveValidator (ContainsData schema' minCount maxCount) _ctx (Array arr) =
  let evaluations =
        [ (idx, recursiveValidator schema' item)
        | (idx, item) <- zip [0..] (toList arr)
        ]
      matchingIndices = Set.fromList $ do
        (idx, ValidationSuccess _) <- evaluations
        pure idx
      matchCount = fromIntegral (Set.size matchingIndices) :: Natural
      minCheck = matchCount >= minCount
      maxCheck = maybe True (matchCount <=) maxCount
      shiftedAnnotations =
        [ shiftAnnotations (arrayIndexPointer idx) anns
        | (idx, ValidationSuccess anns) <- evaluations
        ]
  in pure $
    if not minCheck
      then validationFailure "contains" $
        "Array has " <> T.pack (show matchCount)
        <> " items matching contains, but minContains requires at least "
        <> T.pack (show minCount)
    else if not maxCheck
      then validationFailure "contains" $
        "Array has " <> T.pack (show matchCount)
        <> " items matching contains, but maxContains allows at most "
        <> maybe "0" (T.pack . show) maxCount
    else ValidationSuccess (annotateItems matchingIndices <> mconcat shiftedAnnotations)

validateContainsKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to arrays

-- | Validate minContains (no-op, behavior is enforced by contains keyword)
validateMinContainsKeyword :: ValidateFunc MinContainsData
validateMinContainsKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Behavior handled by contains keyword

-- | Validate maxContains (no-op, behavior is enforced by contains keyword)
validateMaxContainsKeyword :: ValidateFunc MaxContainsData
validateMaxContainsKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Behavior handled by contains keyword

-- | Keyword definition for contains
containsKeyword :: KeywordDefinition
containsKeyword = KeywordDefinition
  { keywordName = "contains"
  , keywordCompile = compileContains
  , keywordValidate = validateContainsKeyword
  , keywordNavigation = SingleSchema $ \schema -> case schemaCore schema of
      ObjectSchema obj -> 
        -- Check pre-parsed first, then parse on-demand
        case validationContains (schemaValidation obj) of
          Just containsSchema -> Just containsSchema
          Nothing -> parseContainsFromRaw schema
      _ -> Nothing
  , keywordPostValidate = Nothing
  }
  where
    parseContainsFromRaw :: Schema -> Maybe Schema
    parseContainsFromRaw s = case Map.lookup "contains" (schemaRawKeywords s) of
      Just val ->
        let version = fromMaybe Draft202012 (schemaVersion s)
        in case ParserInternal.parseSchemaValue version val of
          Right schema -> Just schema
          Left _ -> Nothing
      _ -> Nothing

-- | Keyword definition for minContains
minContainsKeyword :: KeywordDefinition
minContainsKeyword = KeywordDefinition
  { keywordName = "minContains"
  , keywordCompile = compileMinContains
  , keywordValidate = validateMinContainsKeyword
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

-- | Keyword definition for maxContains
maxContainsKeyword :: KeywordDefinition
maxContainsKeyword = KeywordDefinition
  { keywordName = "maxContains"
  , keywordCompile = compileMaxContains
  , keywordValidate = validateMaxContainsKeyword
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

