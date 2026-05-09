-- | Vocabulary registration and management
--
-- This module provides the API for registering vocabularies and managing
-- the vocabulary registry, including conflict detection and keyword lookup.
module JsonSchema.Vocabulary.Registry
  ( -- * Registration
    registerVocabulary
  , unregisterVocabulary
  , lookupVocabulary
    -- * Keyword Lookup
  , lookupKeywordVocabulary
  , getVocabularyKeywords
    -- * Conflict Detection
  , detectKeywordConflicts
  , hasConflicts
  , ConflictInfo(..)
    -- * Bulk Operations
  , registerVocabularies
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (foldl')

import JsonSchema.Vocabulary.Types

-- | Information about a keyword conflict between vocabularies
data ConflictInfo = ConflictInfo
  { conflictKeyword :: Text
    -- ^ The keyword name that conflicts
  , conflictVocabularies :: [VocabularyURI]
    -- ^ The vocabularies that define this keyword
  }
  deriving (Show, Eq)

-- | Register a vocabulary in the registry
--
-- If a vocabulary with the same URI already exists, it will be replaced.
-- This function also updates the keyword index for fast conflict detection.
registerVocabulary :: Vocabulary -> VocabularyRegistry -> VocabularyRegistry
registerVocabulary vocab registry =
  let uri = vocabularyURI vocab
      keywords = Set.toList (vocabularyKeywords vocab)

      -- Remove old index entries for this vocabulary
      oldKeywordIndex = registryKeywordIndex registry
      cleanedIndex = Map.filter (/= uri) oldKeywordIndex

      -- Add new index entries
      newIndexEntries = Map.fromList [(k, uri) | k <- keywords]
      newIndex = Map.union newIndexEntries cleanedIndex

      -- Add vocabulary
      newVocabs = Map.insert uri vocab (registryVocabularies registry)
  in VocabularyRegistry
       { registryVocabularies = newVocabs
       , registryKeywordIndex = newIndex
       }

-- | Unregister a vocabulary from the registry
unregisterVocabulary :: VocabularyURI -> VocabularyRegistry -> VocabularyRegistry
unregisterVocabulary uri registry =
  let newVocabs = Map.delete uri (registryVocabularies registry)
      newIndex = Map.filter (/= uri) (registryKeywordIndex registry)
  in VocabularyRegistry
       { registryVocabularies = newVocabs
       , registryKeywordIndex = newIndex
       }

-- | Look up a vocabulary by URI
lookupVocabulary :: VocabularyURI -> VocabularyRegistry -> Maybe Vocabulary
lookupVocabulary uri registry = Map.lookup uri (registryVocabularies registry)

-- | Find which vocabulary defines a keyword
lookupKeywordVocabulary :: Text -> VocabularyRegistry -> Maybe VocabularyURI
lookupKeywordVocabulary keyword registry =
  Map.lookup keyword (registryKeywordIndex registry)

-- | Get all keywords from a vocabulary
getVocabularyKeywords :: VocabularyURI -> VocabularyRegistry -> Maybe (Set Text)
getVocabularyKeywords uri registry = do
  vocab <- lookupVocabulary uri registry
  return $ vocabularyKeywords vocab

-- | Detect keyword conflicts in a list of vocabularies
--
-- Returns conflicts where multiple vocabularies define the same keyword.
-- This is useful when composing dialects from multiple vocabularies.
detectKeywordConflicts :: [Vocabulary] -> [ConflictInfo]
detectKeywordConflicts vocabs =
  let -- Build map of keyword -> [vocabulary URIs]
      keywordMap = foldl' addVocabKeywords Map.empty vocabs

      addVocabKeywords :: Map Text [VocabularyURI] -> Vocabulary -> Map Text [VocabularyURI]
      addVocabKeywords acc vocab =
        let uri = vocabularyURI vocab
            keywords = Set.toList (vocabularyKeywords vocab)
        in foldl' (\m k -> Map.insertWith (++) k [uri] m) acc keywords

      -- Find keywords with multiple vocabularies
      conflicts = Map.filter (\uris -> length uris > 1) keywordMap
  in [ ConflictInfo { conflictKeyword = k, conflictVocabularies = uris }
     | (k, uris) <- Map.toList conflicts
     ]

-- | Check if vocabularies have any conflicts
hasConflicts :: [Vocabulary] -> Bool
hasConflicts = not . null . detectKeywordConflicts

-- | Register multiple vocabularies at once
--
-- Returns either an error message (if conflicts detected) or the updated registry.
-- This function checks for conflicts before registration.
registerVocabularies
  :: [Vocabulary]
  -> VocabularyRegistry
  -> Either Text VocabularyRegistry
registerVocabularies vocabs registry =
  case detectKeywordConflicts vocabs of
    [] -> Right $ foldl' (flip registerVocabulary) registry vocabs
    conflicts ->
      let conflictText = T.intercalate ", " $
            map (\c -> conflictKeyword c <> " (in: " <>
                       T.intercalate ", " (conflictVocabularies c) <> ")")
                conflicts
      in Left $ "Keyword conflicts detected: " <> conflictText

