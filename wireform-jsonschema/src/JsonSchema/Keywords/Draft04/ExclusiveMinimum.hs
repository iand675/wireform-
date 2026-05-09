{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Draft-04 style exclusiveMinimum keyword
--
-- In Draft-04, exclusiveMinimum is a boolean that modifies the behavior
-- of the minimum keyword. When true, minimum becomes exclusive (>)
-- instead of inclusive (>=).
--
-- This is different from Draft-06+ where exclusiveMinimum is a standalone
-- numeric value.
module JsonSchema.Keywords.Draft04.ExclusiveMinimum
  ( exclusiveMinimumKeyword
  ) where

import JsonSchema.Keyword.Types
import Data.Aeson (Value(..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import JsonSchema.Types (ValidationAnnotations(..), pattern ValidationSuccess)

-- | Compiled data for Draft-04 'exclusiveMinimum' keyword
--
-- In Draft-04, this is a boolean flag that requires the presence
-- of the 'minimum' keyword to have any effect.
newtype ExclusiveMinimumData = ExclusiveMinimumData Bool
  deriving (Show, Eq, Typeable)

-- | Compile function for Draft-04 'exclusiveMinimum' keyword
compileExclusiveMinimum :: CompileFunc ExclusiveMinimumData
compileExclusiveMinimum value _schema _ctx = case value of
  Bool b -> Right $ ExclusiveMinimumData b
  _ -> Left "exclusiveMinimum must be a boolean (Draft-04)"

-- | Validate function for Draft-04 'exclusiveMinimum' keyword
--
-- NOTE: This keyword doesn't validate on its own - it only modifies
-- the behavior of the 'minimum' keyword. In Draft-04, the minimum
-- keyword needs to check for the presence of exclusiveMinimum
-- and adjust its validation accordingly.
--
-- For the pluggable keyword architecture, this presents a challenge
-- because keywords are validated independently. The Draft-06+ style
-- (standalone numeric exclusiveMinimum) is better suited for the
-- pluggable architecture.
--
-- This implementation is provided for completeness but may need
-- coordination with the minimum keyword validator.
validateExclusiveMinimum :: ValidateFunc ExclusiveMinimumData
validateExclusiveMinimum _ _ _ _ = pure (ValidationSuccess mempty)  -- No validation on its own

-- | The Draft-04 'exclusiveMinimum' keyword definition
exclusiveMinimumKeyword :: KeywordDefinition
exclusiveMinimumKeyword = KeywordDefinition
  { keywordName = "exclusiveMinimum"
  , keywordCompile = compileExclusiveMinimum
  , keywordValidate = validateExclusiveMinimum
  , keywordNavigation = NoNavigation
  }
