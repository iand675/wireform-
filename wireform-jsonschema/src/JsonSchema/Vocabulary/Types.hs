-- | Core types for the vocabulary system
--
-- Vocabularies organize related keywords into composable units that can be
-- versioned and registered. This enables custom JSON Schema dialects.
module JsonSchema.Vocabulary.Types
  ( -- * Vocabulary
    Vocabulary(..)
  , VocabularyURI
    -- * Vocabulary Registry
  , VocabularyRegistry(..)
  , emptyVocabularyRegistry
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

-- | URI identifying a vocabulary (represented as Text)
--
-- Must be an absolute URI to ensure global uniqueness.
-- Standard vocabularies use URIs like:
-- - https://json-schema.org/draft/2020-12/vocab/core
-- - https://json-schema.org/draft/2020-12/vocab/validation
--
-- Custom vocabularies should use domain-specific URIs:
-- - https://example.com/vocab/business/v1
type VocabularyURI = Text

-- | A vocabulary is a collection of related keywords
--
-- Vocabularies can be:
-- - Required: Schema MUST NOT be processed if vocabulary is not recognized
-- - Optional: Unknown vocabulary is ignored, keywords from it are ignored
data Vocabulary = Vocabulary
  { vocabularyURI :: VocabularyURI
    -- ^ Unique identifier for this vocabulary
  , vocabularyRequired :: Bool
    -- ^ Whether this vocabulary is required for schema processing
  , vocabularyKeywords :: Set Text
    -- ^ Set of keyword names defined by this vocabulary
  }

instance Show Vocabulary where
  show v = "Vocabulary{uri=" ++ show (vocabularyURI v) ++
           ", required=" ++ show (vocabularyRequired v) ++
           ", keywords=" ++ show (Set.toList (vocabularyKeywords v)) ++ "}"

-- | Registry of available vocabularies
--
-- The registry maintains:
-- - All registered vocabularies by URI
-- - Quick lookup for keyword conflicts
-- - Dependency resolution between vocabularies
data VocabularyRegistry = VocabularyRegistry
  { registryVocabularies :: Map VocabularyURI Vocabulary
    -- ^ All registered vocabularies
  , registryKeywordIndex :: Map Text VocabularyURI
    -- ^ Index mapping keyword names to their vocabulary
    -- (for detecting conflicts)
  }
  deriving (Show)

-- | Empty vocabulary registry
emptyVocabularyRegistry :: VocabularyRegistry
emptyVocabularyRegistry = VocabularyRegistry
  { registryVocabularies = Map.empty
  , registryKeywordIndex = Map.empty
  }
