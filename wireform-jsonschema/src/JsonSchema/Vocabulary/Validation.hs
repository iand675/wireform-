-- | Vocabulary validation and requirement checking
--
-- This module validates that schemas with $vocabulary declarations
-- only use vocabularies that are understood (registered).
module JsonSchema.Vocabulary.Validation
  ( -- * Vocabulary Validation
    validateSchemaVocabularies
  , checkRequiredVocabularies
  , VocabularyError(..)
    -- * Vocabulary Requirements
  , getRequiredVocabularies
  , getOptionalVocabularies
  , isVocabularyRequired
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import JsonSchema.Types (Schema(..), schemaVocabulary)
import JsonSchema.Vocabulary.Types

-- | Errors that can occur during vocabulary validation
data VocabularyError
  = UnknownRequiredVocabulary VocabularyURI
    -- ^ A required vocabulary is not registered
  | ConflictingVocabularies [VocabularyURI]
    -- ^ Multiple vocabularies define conflicting keywords
  deriving (Show, Eq)

-- | Validate that a schema's vocabulary requirements are met
--
-- Checks that:
-- 1. All required vocabularies (where Bool is True) are registered
-- 2. No conflicts exist between the vocabularies
--
-- Returns either a list of errors or unit on success.
validateSchemaVocabularies
  :: Schema
  -> VocabularyRegistry
  -> Either [VocabularyError] ()
validateSchemaVocabularies schema registry =
  case schemaVocabulary schema of
    Nothing -> Right ()  -- No vocabulary requirements
    Just vocabDeclarations ->
      -- Check required vocabularies
      let required = getRequiredVocabularies schema
      in checkRequiredVocabularies required registry

-- | Check that all required vocabularies are registered
--
-- Returns a list of UnknownRequiredVocabulary errors for any
-- required vocabulary that is not in the registry.
checkRequiredVocabularies
  :: [VocabularyURI]
  -> VocabularyRegistry
  -> Either [VocabularyError] ()
checkRequiredVocabularies required registry =
  let missing = filter (`notInRegistry` registry) required
      errors = map UnknownRequiredVocabulary missing
  in if null errors
       then Right ()
       else Left errors
  where
    notInRegistry :: VocabularyURI -> VocabularyRegistry -> Bool
    notInRegistry uri reg = Map.notMember uri (registryVocabularies reg)

-- | Get list of required vocabularies from a schema
--
-- Returns URIs where the $vocabulary value is true.
getRequiredVocabularies :: Schema -> [VocabularyURI]
getRequiredVocabularies schema =
  case schemaVocabulary schema of
    Nothing -> []
    Just vocabs -> [uri | (uri, True) <- Map.toList vocabs]

-- | Get list of optional vocabularies from a schema
--
-- Returns URIs where the $vocabulary value is false.
getOptionalVocabularies :: Schema -> [VocabularyURI]
getOptionalVocabularies schema =
  case schemaVocabulary schema of
    Nothing -> []
    Just vocabs -> [uri | (uri, False) <- Map.toList vocabs]

-- | Check if a specific vocabulary is required in a schema
isVocabularyRequired :: VocabularyURI -> Schema -> Bool
isVocabularyRequired uri schema =
  case schemaVocabulary schema of
    Nothing -> False
    Just vocabs -> Map.lookup uri vocabs == Just True
