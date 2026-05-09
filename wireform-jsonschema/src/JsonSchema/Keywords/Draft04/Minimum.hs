{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Draft-04 style minimum keyword with exclusiveMinimum support
--
-- In Draft-04, exclusiveMinimum is a boolean that modifies the behavior
-- of the minimum keyword. This module implements the Draft-04 minimum
-- keyword that checks for adjacent exclusiveMinimum.
module JsonSchema.Keywords.Draft04.Minimum
  ( minimumKeyword
  , exclusiveMinimumKeyword
  ) where

import JsonSchema.Keyword.Types
import JsonSchema.Types (Schema(..), schemaRawKeywords, ValidationAnnotations(..), pattern ValidationSuccess)
import Data.Aeson (Value(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Scientific as Sci
import Data.Typeable (Typeable)
import qualified Data.Map.Strict as Map

-- | Compiled data for Draft-04 'minimum' keyword
data MinimumData = MinimumData
  { minimumValue :: Sci.Scientific
  , minimumExclusive :: Bool  -- from adjacent exclusiveMinimum
  } deriving (Show, Eq, Typeable)

-- | Compile function for Draft-04 'minimum' keyword
--
-- Checks for adjacent 'exclusiveMinimum' boolean in the same schema
-- to determine whether to use exclusive (>) or inclusive (>=) comparison.
compileMinimum :: CompileFunc MinimumData
compileMinimum value schema _ctx = case value of
  Number n ->
    -- Check for exclusiveMinimum boolean in schemaRawKeywords
    -- In Draft-04, exclusiveMinimum is a boolean that modifies minimum behavior
    let exclusive = case Map.lookup "exclusiveMinimum" (schemaRawKeywords schema) of
          Just (Bool True) -> True
          _ -> False
    in Right $ MinimumData n exclusive
  _ -> Left "minimum must be a number"

-- | Validate function for Draft-04 'minimum' keyword
validateMinimum :: ValidateFunc MinimumData
validateMinimum = legacyValidate "minimum" validateMinimumLegacy

validateMinimumLegacy :: LegacyValidateFunc MinimumData
validateMinimumLegacy _recursiveValidator (MinimumData minVal exclusive) _ctx (Number n) =
  if exclusive
    then if n > minVal
         then pure []
         else pure ["Value " <> T.pack (show n) <> " must be greater than minimum " <> T.pack (show minVal)]
    else if n >= minVal
         then pure []
         else pure ["Value " <> T.pack (show n) <> " is less than minimum " <> T.pack (show minVal)]
validateMinimumLegacy _ _ _ _ = pure []  -- Only applies to numbers

-- | The Draft-04 'minimum' keyword definition
minimumKeyword :: KeywordDefinition
minimumKeyword = KeywordDefinition
  { keywordName = "minimum"
  , keywordCompile = compileMinimum
  , keywordValidate = validateMinimum
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

-- | Compiled data for Draft-04 'exclusiveMinimum' keyword
--
-- This is a boolean modifier that affects the 'minimum' keyword.
newtype ExclusiveMinimumData = ExclusiveMinimumData Bool
  deriving (Show, Eq, Typeable)

-- | Compile function for Draft-04 'exclusiveMinimum' keyword
compileExclusiveMinimum :: CompileFunc ExclusiveMinimumData
compileExclusiveMinimum value _schema _ctx = case value of
  Bool b -> Right $ ExclusiveMinimumData b
  _ -> Left "exclusiveMinimum must be a boolean (Draft-04)"

-- | Validate function for Draft-04 'exclusiveMinimum' keyword
--
-- This keyword doesn't validate on its own - it only modifies
-- the behavior of the 'minimum' keyword via adjacent data.
validateExclusiveMinimum :: ValidateFunc ExclusiveMinimumData
validateExclusiveMinimum _ _ _ _ = pure (ValidationSuccess mempty)

-- | The Draft-04 'exclusiveMinimum' keyword definition
exclusiveMinimumKeyword :: KeywordDefinition
exclusiveMinimumKeyword = KeywordDefinition
  { keywordName = "exclusiveMinimum"
  , keywordCompile = compileExclusiveMinimum
  , keywordValidate = validateExclusiveMinimum
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }
