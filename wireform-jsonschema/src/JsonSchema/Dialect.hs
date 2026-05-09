-- | JSON Schema dialects
--
-- Dialect definitions for each supported JSON Schema version.
-- A dialect specifies which vocabularies are enabled and how keywords behave.
--
-- = Dialect Composition
--
-- Dialects compose vocabularies (collections of keywords) with specific validation behaviors:
--
-- * **Format behavior**: Whether 'format' keyword is an assertion or annotation
-- * **Unknown keyword mode**: How to handle unrecognized keywords
-- * **Vocabulary requirements**: Which vocabularies are required vs optional
--
-- = Invariants
--
-- A valid dialect must satisfy:
--
-- 1. Dialect URI must be an absolute URI with a valid scheme (http:\/\/, https:\/\/, file:\/\/, urn:, etc.)
-- 2. All vocabulary URIs must be resolvable (when used with a VocabularyRegistry)
-- 3. No keyword conflicts between composed vocabularies
module JsonSchema.Dialect
  ( -- * Core Types
    Dialect(..)
  , FormatBehavior(..)
  , UnknownKeywordMode(..)
    -- * Predefined Dialects
  , draft04Dialect
  , draft06Dialect
  , draft07Dialect
  , draft201909Dialect
  , draft202012Dialect
    -- * Validation
  , validateDialect
  , validateDialectURI
  , DialectError(..)
    -- * Helpers
  , defaultDialectURI
  , applyDialectToConfig
  ) where

import JsonSchema.Types
  ( JsonSchemaVersion(..)
  , FormatBehavior(..)
  , UnknownKeywordMode(..)
  , ValidationConfig(..)
  )
import JsonSchema.Vocabulary.Types (VocabularyURI, VocabularyRegistry)
import qualified JsonSchema.Vocabulary.Registry as VocabReg
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (intercalate)

-- Re-export for convenience (types are defined in Types.hs to avoid circular dependencies)

-- | A dialect is a complete configuration of vocabularies
--
-- A dialect defines a specific JSON Schema variant by composing vocabularies
-- and specifying validation behavior (format handling, unknown keywords, etc.)
--
-- Invariants (checked by 'validateDialect'):
--
-- * 'dialectURI' must be an absolute URI (with a valid scheme)
-- * All vocabulary URIs in 'dialectVocabularies' must be resolvable
-- * No keyword conflicts between composed vocabularies
data Dialect = Dialect
  { dialectURI :: Text
    -- ^ Unique identifier for this dialect (must be absolute URI with scheme: http:\/\/, https:\/\/, file:\/\/, urn:, etc.)
  , dialectVersion :: JsonSchemaVersion
    -- ^ JSON Schema version this dialect is based on
  , dialectName :: Text
    -- ^ Human-readable name
  , dialectVocabularies :: Map VocabularyURI Bool
    -- ^ Vocabularies in this dialect (URI -> required?)
    -- Required vocabularies cause schema processing to fail if not recognized
  , dialectDefaultFormat :: FormatBehavior
    -- ^ How to handle 'format' keyword by default
  , dialectUnknownKeywords :: UnknownKeywordMode
    -- ^ How to handle unrecognized keywords
  }
  deriving (Eq, Show)

-- | Errors that can occur when validating or composing dialects
data DialectError
  = InvalidDialectURI Text Text
    -- ^ URI is not absolute (dialect URI, reason)
  | UnresolvableVocabulary VocabularyURI
    -- ^ Vocabulary URI cannot be resolved
  | KeywordConflicts [(Text, [VocabularyURI])]
    -- ^ Keyword conflicts between vocabularies (keyword, conflicting vocab URIs)
  deriving (Eq, Show)

-- | Validate that a dialect satisfies all invariants
--
-- Checks:
--
-- 1. Dialect URI is absolute
-- 2. All vocabulary URIs can be resolved in the given registry
-- 3. No keyword conflicts between vocabularies
validateDialect :: VocabularyRegistry -> Dialect -> Either DialectError ()
validateDialect registry dialect = do
  -- Check dialect URI is absolute
  validateDialectURI (dialectURI dialect)
  
  -- Check all vocabulary URIs are resolvable
  let vocabURIs = Map.keys (dialectVocabularies dialect)
  mapM_ (checkVocabularyResolvable registry) vocabURIs
  
  -- Check for keyword conflicts
  checkNoKeywordConflicts registry vocabURIs

-- | Validate that a dialect URI is absolute
--
-- An absolute URI must have a scheme (e.g., http://, https://, file://, urn:)
-- This accepts any URI with a scheme, not just HTTP/HTTPS.
--
-- Valid examples:
--
-- * https://json-schema.org/draft/2020-12/schema
-- * http://json-schema.org/draft-04/schema#
-- * file:///path/to/dialect.json
-- * urn:example:dialect:v1
validateDialectURI :: Text -> Either DialectError ()
validateDialectURI uri
  | T.null uri = Left $ InvalidDialectURI uri "URI is empty"
  | hasScheme uri = Right ()
  | otherwise = Left $ InvalidDialectURI uri "URI must be absolute (must have a scheme like http://, https://, file://, or urn:)"
  where
    -- Check if URI has a valid scheme (characters before ':')
    -- Scheme must start with letter and contain only letters, digits, +, -, .
    hasScheme u = case T.breakOn ":" u of
      (scheme, rest)
        | T.null rest -> False  -- No ':' found
        | T.null scheme -> False  -- Empty scheme
        | not (isValidScheme scheme) -> False
        | otherwise -> True
    
    isValidScheme scheme =
      let schemeChars = T.unpack scheme
      in case schemeChars of
        (c:cs) -> isLetter c && all (\ch -> isAlphaNum ch || ch `elem` ['+', '-', '.']) cs
        [] -> False
    
    isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    isAlphaNum c = isLetter c || (c >= '0' && c <= '9')

-- | Check if a vocabulary can be resolved in the registry
checkVocabularyResolvable :: VocabularyRegistry -> VocabularyURI -> Either DialectError ()
checkVocabularyResolvable registry uri =
  case VocabReg.lookupVocabulary uri registry of
    Just _ -> Right ()
    Nothing -> Left $ UnresolvableVocabulary uri

-- | Check that vocabularies don't have keyword conflicts
checkNoKeywordConflicts :: VocabularyRegistry -> [VocabularyURI] -> Either DialectError ()
checkNoKeywordConflicts registry uris = do
  let vocabs = [v | uri <- uris, Just v <- [VocabReg.lookupVocabulary uri registry]]
      conflicts = VocabReg.detectKeywordConflicts vocabs
  case conflicts of
    [] -> Right ()
    cs -> Left $ KeywordConflicts
            [ (VocabReg.conflictKeyword c, VocabReg.conflictVocabularies c)
            | c <- cs
            ]

-- | Get the default dialect URI for a JSON Schema version
--
-- Returns the standard URI for each JSON Schema version:
--
-- * Draft-04: @http:\/\/json-schema.org\/draft-04\/schema#@
-- * Draft-06: @http:\/\/json-schema.org\/draft-06\/schema#@
-- * Draft-07: @http:\/\/json-schema.org\/draft-07\/schema#@
-- * Draft 2019-09: @https:\/\/json-schema.org\/draft\/2019-09\/schema@
-- * Draft 2020-12: @https:\/\/json-schema.org\/draft\/2020-12\/schema@
--
-- This is useful when composing dialects from vocabularies without a custom URI.
-- For custom dialects, you can use any absolute URI (http:\/\/, https:\/\/, file:\/\/, urn:, etc.)
defaultDialectURI :: JsonSchemaVersion -> Text
defaultDialectURI Draft04 = "http://json-schema.org/draft-04/schema#"
defaultDialectURI Draft06 = "http://json-schema.org/draft-06/schema#"
defaultDialectURI Draft07 = "http://json-schema.org/draft-07/schema#"
defaultDialectURI Draft201909 = "https://json-schema.org/draft/2019-09/schema"
defaultDialectURI Draft202012 = "https://json-schema.org/draft/2020-12/schema"

-- | Compose a dialect from vocabularies with validation
--
-- This is the recommended way to construct a dialect. It validates that:
--
-- * The dialect URI is absolute
-- * All vocabularies can be resolved
-- * There are no keyword conflicts
--
-- Example:
--
-- @
-- composeDialect
--   registry
--   "https://example.com/schema/2024"
--   Draft202012
--   "Example Dialect"
--   [ ("https://json-schema.org/draft/2020-12/vocab/core", True)
--   , ("https://json-schema.org/draft/2020-12/vocab/validation", True)
--   , ("https://example.com/vocab/business/v1", False)
--   ]
--   FormatAssertion
--   ErrorOnUnknown
-- @
composeDialect
  :: VocabularyRegistry
  -> Text  -- ^ Dialect URI
  -> JsonSchemaVersion
  -> Text  -- ^ Dialect name
  -> [(VocabularyURI, Bool)]  -- ^ Vocabularies (URI, required?)
  -> FormatBehavior
  -> UnknownKeywordMode
  -> Either DialectError Dialect
composeDialect registry uri version name vocabs formatBehavior unknownMode = do
  let dialect = Dialect
        { dialectURI = uri
        , dialectVersion = version
        , dialectName = name
        , dialectVocabularies = Map.fromList vocabs
        , dialectDefaultFormat = formatBehavior
        , dialectUnknownKeywords = unknownMode
        }
  validateDialect registry dialect
  return dialect

-- | Draft-04 dialect
--
-- JSON Schema Draft-04 predates the vocabulary system.
-- Format is annotation-only by default.
draft04Dialect :: Dialect
draft04Dialect = Dialect
  { dialectURI = "http://json-schema.org/draft-04/schema#"
  , dialectVersion = Draft04
  , dialectName = "JSON Schema Draft-04"
  , dialectVocabularies = Map.empty  -- No $vocabulary keyword in draft-04
  , dialectDefaultFormat = FormatAnnotation
  , dialectUnknownKeywords = CollectUnknown
  }

-- | Draft-06 dialect (adds const, propertyNames)
--
-- JSON Schema Draft-06 predates the vocabulary system.
-- Format is annotation-only by default.
draft06Dialect :: Dialect
draft06Dialect = Dialect
  { dialectURI = "http://json-schema.org/draft-06/schema#"
  , dialectVersion = Draft06
  , dialectName = "JSON Schema Draft-06"
  , dialectVocabularies = Map.empty  -- No $vocabulary keyword in draft-06
  , dialectDefaultFormat = FormatAnnotation
  , dialectUnknownKeywords = CollectUnknown
  }

-- | Draft-07 dialect (adds if/then/else, readOnly/writeOnly, $comment)
--
-- JSON Schema Draft-07 predates the vocabulary system.
-- Format is annotation-only by default.
draft07Dialect :: Dialect
draft07Dialect = Dialect
  { dialectURI = "http://json-schema.org/draft-07/schema#"
  , dialectVersion = Draft07
  , dialectName = "JSON Schema Draft-07"
  , dialectVocabularies = Map.empty  -- No $vocabulary keyword in draft-07
  , dialectDefaultFormat = FormatAnnotation
  , dialectUnknownKeywords = CollectUnknown
  }

-- | Draft 2019-09 dialect (adds $vocabulary, unevaluated*, dependent*)
--
-- First version to support the $vocabulary keyword.
-- Includes core, applicator, and validation vocabularies as required.
-- Format is annotation-only by default (use format-assertion vocabulary for validation).
draft201909Dialect :: Dialect
draft201909Dialect = Dialect
  { dialectURI = "https://json-schema.org/draft/2019-09/schema"
  , dialectVersion = Draft201909
  , dialectName = "JSON Schema 2019-09"
  , dialectVocabularies = Map.fromList
      [ ("https://json-schema.org/draft/2019-09/vocab/core", True)
      , ("https://json-schema.org/draft/2019-09/vocab/applicator", True)
      , ("https://json-schema.org/draft/2019-09/vocab/validation", True)
      , ("https://json-schema.org/draft/2019-09/vocab/meta-data", False)
      , ("https://json-schema.org/draft/2019-09/vocab/format", False)
      , ("https://json-schema.org/draft/2019-09/vocab/content", False)
      ]
  , dialectDefaultFormat = FormatAnnotation
  , dialectUnknownKeywords = CollectUnknown
  }

-- | Draft 2020-12 dialect (adds prefixItems, $dynamicRef)
--
-- Latest JSON Schema specification with improved vocabulary system.
-- Separates format into annotation and assertion vocabularies.
-- Format is annotation-only by default (use format-assertion vocabulary for validation).
draft202012Dialect :: Dialect
draft202012Dialect = Dialect
  { dialectURI = "https://json-schema.org/draft/2020-12/schema"
  , dialectVersion = Draft202012
  , dialectName = "JSON Schema 2020-12"
  , dialectVocabularies = Map.fromList
      [ ("https://json-schema.org/draft/2020-12/vocab/core", True)
      , ("https://json-schema.org/draft/2020-12/vocab/applicator", True)
      , ("https://json-schema.org/draft/2020-12/vocab/validation", True)
      , ("https://json-schema.org/draft/2020-12/vocab/unevaluated", False)
      , ("https://json-schema.org/draft/2020-12/vocab/meta-data", False)
      , ("https://json-schema.org/draft/2020-12/vocab/format-annotation", False)
      , ("https://json-schema.org/draft/2020-12/vocab/format-assertion", False)
      , ("https://json-schema.org/draft/2020-12/vocab/content", False)
      ]
  , dialectDefaultFormat = FormatAnnotation
  , dialectUnknownKeywords = CollectUnknown
  }

-- | Apply dialect settings to a validation configuration
--
-- Updates the given 'ValidationConfig' with dialect-specific behavior:
--
-- * Sets 'validationDialectFormatBehavior' to the dialect's format behavior
-- * Sets 'validationVersion' to the dialect's schema version
--
-- This allows validating schemas with dialect-specific semantics after parsing
-- them with 'parseSchemaWithDialectRegistry'.
--
-- __Usage Example__:
--
-- @
-- import JsonSchema.Parser (parseSchemaWithDialectRegistry)
-- import JsonSchema.Validator (validateValue, defaultValidationConfig)
-- import JsonSchema.Vocabulary (standardDialectRegistry)
-- import JsonSchema.Dialect (applyDialectToConfig)
-- import JsonSchema.Parser (resolveDialectFromSchema)
--
-- -- Parse schema with dialect
-- schema <- parseSchemaWithDialectRegistry standardDialectRegistry schemaJson
--
-- -- Get the dialect and apply it to config
-- case resolveDialectFromSchema standardDialectRegistry schema of
--   Just dialect ->
--     let config = applyDialectToConfig dialect defaultValidationConfig
--     in validateValue config schema value
--   Nothing ->
--     validateValue defaultValidationConfig schema value
-- @
--
-- __Behavior__:
--
-- * __Format behavior__: The dialect's 'dialectDefaultFormat' overrides the
--   config's 'validationFormatAssertion' via 'validationDialectFormatBehavior'
--
-- * __Schema version__: The dialect's 'dialectVersion' is set as 'validationVersion'
--
-- * __Unknown keywords__: Currently not applied (requires parser-level integration)
--
-- @since 0.1.0.0
applyDialectToConfig :: Dialect -> ValidationConfig -> ValidationConfig
applyDialectToConfig dialect config = config
  { validationDialectFormatBehavior = Just (dialectDefaultFormat dialect)
  , validationVersion = dialectVersion dialect
  }

