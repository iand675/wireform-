-- | Extensible vocabulary system
--
-- Define and register custom keywords for domain-specific validation.
-- Implements the vocabulary system for JSON Schema 2019-09 and 2020-12.
module JsonSchema.Vocabulary
  ( -- * Vocabulary Types
    Vocabulary(..)
  , KeywordValue(..)
  
    -- * Registry
  , VocabularyRegistry(..)
  , emptyVocabularyRegistry
  , standardRegistry
  , standardDialectRegistry
  , registerVocabulary
  , registerDialect
  , unregisterDialect
  , registerDialects
  , lookupVocabulary
  , lookupDialect
  , hasDialect
  , listDialects
  , getDialectByVersion
  , composeDialect
  , extractKeywordValue
  , detectKeywordConflicts
  
    -- * Standard Vocabularies
  , coreVocabulary
  , applicatorVocabulary
  , validationVocabulary
  , metadataVocabulary
  , formatAnnotationVocabulary
  , unevaluatedVocabulary
  ) where

import JsonSchema.Types
import JsonSchema.Dialect
import Data.Text (Text)
import qualified Data.Text
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable, eqT, (:~:)(Refl))


-- | Opaque keyword value with type safety via existential
data KeywordValue where
  KeywordValue :: (Eq a, Show a, Typeable a) => a -> KeywordValue

instance Eq KeywordValue where
  (KeywordValue (a :: a)) == (KeywordValue (b :: b)) =
    case eqT @a @b of
      Just Refl -> a == b
      Nothing -> False

instance Show KeywordValue where
  show (KeywordValue a) = show a

-- | Vocabulary definition
data Vocabulary = Vocabulary
  { vocabularyURI :: Text
  , vocabularyRequired :: Bool
  , vocabularyKeywords :: Set Text
    -- ^ Set of keyword names defined by this vocabulary
    -- The keyword name is sufficient since vocabularies only track which keywords exist,
    -- not their implementation details (which are handled by the actual keyword registry)
  }
  deriving (Eq, Show)

-- | Vocabulary registry
data VocabularyRegistry = VocabularyRegistry
  { registeredVocabularies :: Map Text Vocabulary
  , registeredDialects :: Map Text Dialect
  }
  deriving (Eq, Show)

-- | Empty registry
emptyVocabularyRegistry :: VocabularyRegistry
emptyVocabularyRegistry = VocabularyRegistry Map.empty Map.empty

-- | Standard registry with all standard vocabularies and dialects
standardRegistry :: VocabularyRegistry
standardRegistry = foldr registerDialect
  (foldr registerVocabulary emptyVocabularyRegistry standardVocabularies)
  standardDialects
  where
    standardVocabularies =
      [ coreVocabulary
      , applicatorVocabulary
      , validationVocabulary
      , metadataVocabulary
      , formatAnnotationVocabulary
      , unevaluatedVocabulary
      ]
    standardDialects =
      [ draft04Dialect
      , draft06Dialect
      , draft07Dialect
      , draft201909Dialect
      , draft202012Dialect
      ]

-- | Register a vocabulary
registerVocabulary :: Vocabulary -> VocabularyRegistry -> VocabularyRegistry
registerVocabulary vocab reg = reg
  { registeredVocabularies = Map.insert (vocabularyURI vocab) vocab (registeredVocabularies reg)
  }

-- | Register a dialect
registerDialect :: Dialect -> VocabularyRegistry -> VocabularyRegistry
registerDialect dialect reg = reg
  { registeredDialects = Map.insert (dialectURI dialect) dialect (registeredDialects reg)
  }

-- | Lookup vocabulary by URI
lookupVocabulary :: Text -> VocabularyRegistry -> Maybe Vocabulary
lookupVocabulary uri reg = Map.lookup uri (registeredVocabularies reg)

-- | Lookup dialect by URI
lookupDialect :: Text -> VocabularyRegistry -> Maybe Dialect
lookupDialect uri reg = Map.lookup uri (registeredDialects reg)

-- | Unregister a dialect from the registry
--
-- Removes a dialect by its URI. If the dialect is not registered, this is a no-op.
unregisterDialect :: Text -> VocabularyRegistry -> VocabularyRegistry
unregisterDialect uri reg = reg
  { registeredDialects = Map.delete uri (registeredDialects reg)
  }

-- | Check if a dialect is registered
hasDialect :: Text -> VocabularyRegistry -> Bool
hasDialect uri reg = Map.member uri (registeredDialects reg)

-- | List all registered dialects
--
-- Returns a list of all dialects in the registry.
listDialects :: VocabularyRegistry -> [Dialect]
listDialects reg = Map.elems (registeredDialects reg)

-- | Lookup a dialect by JSON Schema version
--
-- Returns the first dialect matching the given version.
-- Note: Multiple dialects can exist for the same version (e.g., custom dialects).
-- This returns the standard dialect for the version if multiple exist.
getDialectByVersion :: JsonSchemaVersion -> VocabularyRegistry -> Maybe Dialect
getDialectByVersion version reg =
  case filter (\d -> dialectVersion d == version) (listDialects reg) of
    [] -> Nothing
    (d:_) -> Just d

-- | Register multiple dialects at once
--
-- Returns either an error message (if conflicts detected) or the updated registry.
-- Conflicts occur when:
--
-- * A dialect URI is already registered (unless it's being replaced)
-- * Multiple dialects in the input list have the same URI
registerDialects
  :: [Dialect]
  -> VocabularyRegistry
  -> Either Text VocabularyRegistry
registerDialects dialects reg = do
  -- Check for duplicate URIs in input
  let uris = map dialectURI dialects
      duplicates = findDuplicates uris
  case duplicates of
    (dup:_) -> Left $ "Duplicate dialect URI in input: " <> dup
    [] -> Right $ foldr registerDialect reg dialects
  where
    findDuplicates xs = 
      Map.keys $ Map.filter (> 1) $ Map.fromListWith (+) [(x, 1 :: Int) | x <- xs]

-- | Standard dialect registry
--
-- A registry containing only the standard JSON Schema dialects (Draft-04 through 2020-12).
-- Useful as a starting point for custom registries.
standardDialectRegistry :: VocabularyRegistry
standardDialectRegistry = foldr registerDialect emptyVocabularyRegistry
  [ draft04Dialect
  , draft06Dialect
  , draft07Dialect
  , draft201909Dialect
  , draft202012Dialect
  ]

-- | Compose a dialect from a list of vocabulary URIs
-- Returns Left with error message if:
-- - A required vocabulary is not found in the registry
-- - There are keyword conflicts between vocabularies
composeDialect 
  :: Text                      -- ^ Dialect name
  -> JsonSchemaVersion         -- ^ Schema version
  -> [(Text, Bool)]            -- ^ (vocabulary URI, required?)
  -> FormatBehavior            -- ^ Default format behavior
  -> UnknownKeywordMode        -- ^ Unknown keyword mode
  -> VocabularyRegistry        -- ^ Registry to lookup vocabularies
  -> Either Text Dialect       -- ^ Composed dialect or error
composeDialect name version vocabSpecs formatBehavior unknownMode reg = do
  -- Lookup all vocabularies
  vocabs <- mapM lookupVocabWithCheck vocabSpecs
  
  -- Detect keyword conflicts
  case detectKeywordConflicts vocabs of
    [] -> do
      -- No conflicts, create dialect
      let vocabMap = Map.fromList vocabSpecs
      Right $ Dialect
        { dialectURI = defaultDialectURI version
        , dialectVersion = version
        , dialectName = name
        , dialectVocabularies = vocabMap
        , dialectDefaultFormat = formatBehavior
        , dialectUnknownKeywords = unknownMode
        }
    conflicts -> Left $ "Keyword conflicts detected: " <> Data.Text.intercalate ", " conflicts
  where
    lookupVocabWithCheck (vocabURI, required) =
      case lookupVocabulary vocabURI reg of
        Just vocab -> Right vocab
        Nothing -> 
          if required
            then Left $ "Required vocabulary not found: " <> vocabURI
            else Left $ "Vocabulary not found: " <> vocabURI

-- | Detect keyword conflicts between vocabularies
-- Returns list of conflicting keyword names
detectKeywordConflicts :: [Vocabulary] -> [Text]
detectKeywordConflicts vocabs =
  let allKeywords = concatMap (Set.toList . vocabularyKeywords) vocabs
      keywordCounts = Map.fromListWith (+) [(k, 1 :: Int) | k <- allKeywords]
      conflicts = Map.keys $ Map.filter (> 1) keywordCounts
  in conflicts

-- | Extract a typed value from KeywordValue
-- Returns Nothing if the type doesn't match
extractKeywordValue :: forall a. Typeable a => KeywordValue -> Maybe a
extractKeywordValue (KeywordValue (val :: b)) =
  case eqT @a @b of
    Just Refl -> Just val
    Nothing -> Nothing

-- | Core vocabulary (2020-12)
coreVocabulary :: Vocabulary
coreVocabulary = Vocabulary
  { vocabularyURI = "https://json-schema.org/draft/2020-12/vocab/core"
  , vocabularyRequired = True
  , vocabularyKeywords = Set.fromList
      [ "$schema"
      , "$id"
      , "$ref"
      , "$anchor"
      , "$dynamicRef"
      , "$dynamicAnchor"
      , "$vocabulary"
      , "$comment"
      , "$defs"
      ]
  }

-- | Applicator vocabulary (2020-12)
applicatorVocabulary :: Vocabulary
applicatorVocabulary = Vocabulary
  { vocabularyURI = "https://json-schema.org/draft/2020-12/vocab/applicator"
  , vocabularyRequired = True
  , vocabularyKeywords = Set.fromList
      [ "prefixItems"
      , "items"
      , "contains"
      , "additionalProperties"
      , "properties"
      , "patternProperties"
      , "dependentSchemas"
      , "propertyNames"
      , "if"
      , "then"
      , "else"
      , "allOf"
      , "anyOf"
      , "oneOf"
      , "not"
      ]
  }

-- | Validation vocabulary (2020-12)
validationVocabulary :: Vocabulary
validationVocabulary = Vocabulary
  { vocabularyURI = "https://json-schema.org/draft/2020-12/vocab/validation"
  , vocabularyRequired = True
  , vocabularyKeywords = Set.fromList
      [ "type"
      , "enum"
      , "const"
      , "multipleOf"
      , "maximum"
      , "exclusiveMaximum"
      , "minimum"
      , "exclusiveMinimum"
      , "maxLength"
      , "minLength"
      , "pattern"
      , "maxItems"
      , "minItems"
      , "uniqueItems"
      , "maxContains"
      , "minContains"
      , "maxProperties"
      , "minProperties"
      , "required"
      , "dependentRequired"
      ]
  }

-- | Metadata vocabulary (2020-12)
metadataVocabulary :: Vocabulary
metadataVocabulary = Vocabulary
  { vocabularyURI = "https://json-schema.org/draft/2020-12/vocab/meta-data"
  , vocabularyRequired = False
  , vocabularyKeywords = Set.fromList
      [ "title"
      , "description"
      , "default"
      , "deprecated"
      , "readOnly"
      , "writeOnly"
      , "examples"
      ]
  }

-- | Format annotation vocabulary (2020-12)
formatAnnotationVocabulary :: Vocabulary
formatAnnotationVocabulary = Vocabulary
  { vocabularyURI = "https://json-schema.org/draft/2020-12/vocab/format-annotation"
  , vocabularyRequired = False
  , vocabularyKeywords = Set.fromList
      [ "format"
      ]
  }

-- | Unevaluated vocabulary (2020-12)
unevaluatedVocabulary :: Vocabulary
unevaluatedVocabulary = Vocabulary
  { vocabularyURI = "https://json-schema.org/draft/2020-12/vocab/unevaluated"
  , vocabularyRequired = False
  , vocabularyKeywords = Set.fromList
      [ "unevaluatedItems"
      , "unevaluatedProperties"
      ]
  }

