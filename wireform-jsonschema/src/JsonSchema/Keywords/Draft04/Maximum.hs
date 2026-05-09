{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Draft-04 style maximum keyword with exclusiveMaximum support
--
-- In Draft-04, exclusiveMaximum is a boolean that modifies the behavior
-- of the maximum keyword. This module implements the Draft-04 maximum
-- keyword that checks for adjacent exclusiveMaximum.
module JsonSchema.Keywords.Draft04.Maximum
  ( maximumKeyword
  , exclusiveMaximumKeyword
  ) where

import JsonSchema.Keyword.Types
import JsonSchema.Types (Schema(..), schemaRawKeywords, ValidationAnnotations(..), pattern ValidationSuccess)
import Data.Aeson (Value(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Scientific as Sci
import Data.Typeable (Typeable)
import qualified Data.Map.Strict as Map

-- | Compiled data for Draft-04 'maximum' keyword
data MaximumData = MaximumData
  { maximumValue :: Sci.Scientific
  , maximumExclusive :: Bool  -- from adjacent exclusiveMaximum
  } deriving (Show, Eq, Typeable)

-- | Compile function for Draft-04 'maximum' keyword
--
-- Checks for adjacent 'exclusiveMaximum' boolean in the same schema
-- to determine whether to use exclusive (<) or inclusive (<=) comparison.
compileMaximum :: CompileFunc MaximumData
compileMaximum value schema _ctx = case value of
  Number n ->
    -- Check for exclusiveMaximum boolean in schemaRawKeywords
    -- In Draft-04, exclusiveMaximum is a boolean that modifies maximum behavior
    let exclusive = case Map.lookup "exclusiveMaximum" (schemaRawKeywords schema) of
          Just (Bool True) -> True
          _ -> False
    in Right $ MaximumData n exclusive
  _ -> Left "maximum must be a number"

-- | Validate function for Draft-04 'maximum' keyword
validateMaximum :: ValidateFunc MaximumData
validateMaximum = legacyValidate "maximum" validateMaximumLegacy

validateMaximumLegacy :: LegacyValidateFunc MaximumData
validateMaximumLegacy _recursiveValidator (MaximumData maxVal exclusive) _ctx (Number n) =
  if exclusive
    then if n < maxVal
         then pure []
         else pure ["Value " <> T.pack (show n) <> " must be less than maximum " <> T.pack (show maxVal)]
    else if n <= maxVal
         then pure []
         else pure ["Value " <> T.pack (show n) <> " is greater than maximum " <> T.pack (show maxVal)]
validateMaximumLegacy _ _ _ _ = pure []  -- Only applies to numbers

-- | The Draft-04 'maximum' keyword definition
maximumKeyword :: KeywordDefinition
maximumKeyword = KeywordDefinition
  { keywordName = "maximum"
  , keywordCompile = compileMaximum
  , keywordValidate = validateMaximum
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

-- | Compiled data for Draft-04 'exclusiveMaximum' keyword
--
-- This is a boolean modifier that affects the 'maximum' keyword.
newtype ExclusiveMaximumData = ExclusiveMaximumData Bool
  deriving (Show, Eq, Typeable)

-- | Compile function for Draft-04 'exclusiveMaximum' keyword
compileExclusiveMaximum :: CompileFunc ExclusiveMaximumData
compileExclusiveMaximum value _schema _ctx = case value of
  Bool b -> Right $ ExclusiveMaximumData b
  _ -> Left "exclusiveMaximum must be a boolean (Draft-04)"

-- | Validate function for Draft-04 'exclusiveMaximum' keyword
--
-- This keyword doesn't validate on its own - it only modifies
-- the behavior of the 'maximum' keyword via adjacent data.
validateExclusiveMaximum :: ValidateFunc ExclusiveMaximumData
validateExclusiveMaximum _ _ _ _ = pure (ValidationSuccess mempty)

-- | The Draft-04 'exclusiveMaximum' keyword definition
exclusiveMaximumKeyword :: KeywordDefinition
exclusiveMaximumKeyword = KeywordDefinition
  { keywordName = "exclusiveMaximum"
  , keywordCompile = compileExclusiveMaximum
  , keywordValidate = validateExclusiveMaximum
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

