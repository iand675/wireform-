{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Core types for JSON Schema representation
--
-- This module defines the complete AST for JSON Schema (draft-04 through 2020-12)
-- with support for all standard keywords and extensible vocabulary system.
--
-- Key types:
--
-- * 'Schema' - Top-level schema with version and metadata
-- * 'SchemaCore' - Boolean shorthand or full object schema
-- * 'SchemaObject' - Complete schema object with all keyword groups
-- * 'SchemaValidation' - All validation keywords grouped by type
-- * 'ValidationResult' - Success with annotations or failure with errors
module JsonSchema.Types
  ( -- * Core Schema Types
    Schema(..)
  , JsonSchemaVersion(..)
  , SchemaCore(..)
  , SchemaObject(..)
  , ObjectSchemaData(..)  -- The record type for object schemas
  -- Pattern synonyms BooleanSchemaObject and ObjectSchemaObject are available but not exported
  -- Use accessor functions (schemaType, schemaRef, etc.) for backward compatibility
  , SchemaType(..)
  , OneOrMany(..)
  
    -- * Validation Keywords
  , SchemaValidation(..)
  , ArrayItemsValidation(..)
  , Dependency(..)
  , Format(..)
  , Regex(..)
  
    -- * Annotations and Metadata
  , SchemaAnnotations(..)
  , CodegenAnnotations(..)
  , NewtypeSpec(..)
  
    -- * References and Pointers
  , Reference(..)
  , JsonPointer(..)
  , emptyPointer
  , (/.)
  , renderPointer
  , parsePointer
  
    -- * Validation Types
  , ValidationResult
  , pattern ValidationSuccess
  , pattern ValidationFailure
  
    -- * Parse Errors
  , ParseError(..)
  , ParseWarning(..)
  , ValidationAnnotations(..)
  , ValidationErrors(..)
  , ValidationError(..)
  , ValidationContext(..)
  , ValidationConfig(..)
  , FormatBehavior(..)
  , UnknownKeywordMode(..)
  , SchemaRegistry(..)
  , emptyRegistry
  , registerSchemaInRegistry
  , buildRegistryWithExternalRefs
  , schemaEffectiveBase
  , resolveAgainstBaseURI
  , SchemaFingerprint(..)
  , CustomValidator
  , ReferenceLoader
  
    -- * Helper Functions
  , isSuccess
  , isFailure
  , validationError
  , validationFailure
  , splitUriFragment
  
    -- * Empty / default builders
  , defaultSchemaValidation
  , defaultSchemaAnnotations
  , defaultObjectSchemaData
  , booleanSchema
  , objectSchema

    -- * SchemaObject Accessors (backward compatibility)
  , schemaType
  , schemaEnum
  , schemaConst
  , schemaRef
  , schemaDynamicRef
  , schemaAnchor
  , schemaDynamicAnchor
  , schemaRecursiveRef
  , schemaRecursiveAnchor
  , schemaAllOf
  , schemaAnyOf
  , schemaOneOf
  , schemaNot
  , schemaIf
  , schemaThen
  , schemaElse
  , schemaValidation
  , schemaAnnotations
  , schemaDefs
  ) where

import Data.Aeson (Value, ToJSON(..), FromJSON(..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Vector (fromList)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Control.Monad (guard)
import Data.Scientific (Scientific)
import Data.Hashable (Hashable)
import Data.Typeable (typeOf, cast)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Language.Haskell.TH.Syntax (Lift)
import Network.URI (parseURIReference, uriScheme, uriToString, relativeTo)

-- Import JSON Pointer from separate module (avoids circular dependency)
import JsonPointer (JsonPointer(..), emptyPointer, (/.), renderPointer, parsePointer)

-- Import new validation result types
import qualified JsonSchema.Validator.Result as VR

-- | Error during schema parsing
data ParseError = ParseError
  { parseErrorPath :: JsonPointer      -- ^ Where in the schema
  , parseErrorMessage :: Text           -- ^ What went wrong
  , parseErrorContext :: Maybe Value    -- ^ Problematic value
  } deriving (Eq, Show, Generic)
  deriving stock Lift

-- | Warning during schema parsing (for unknown keywords in WarnUnknown mode)
data ParseWarning = ParseWarning
  { parseWarningPath :: JsonPointer
  , parseWarningMessage :: Text
  , parseWarningKeyword :: Text
  } deriving (Eq, Show, Generic)
  deriving stock Lift

-- | Reference to another schema ($ref, $dynamicRef)
newtype Reference = Reference Text
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, Hashable)
  deriving stock Lift

-- | Regular expression pattern
newtype Regex = Regex Text
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, Hashable)
  deriving stock Lift

-- | JSON Schema specification versions
data JsonSchemaVersion
  = Draft04        -- ^ http://json-schema.org/draft-04/schema#
  | Draft06        -- ^ http://json-schema.org/draft-06/schema#
  | Draft07        -- ^ http://json-schema.org/draft-07/schema#
  | Draft201909    -- ^ https://json-schema.org/draft/2019-09/schema
  | Draft202012    -- ^ https://json-schema.org/draft/2020-12/schema
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving stock Lift

instance ToJSON JsonSchemaVersion where
  toJSON Draft04 = "http://json-schema.org/draft-04/schema#"
  toJSON Draft06 = "http://json-schema.org/draft-06/schema#"
  toJSON Draft07 = "http://json-schema.org/draft-07/schema#"
  toJSON Draft201909 = "https://json-schema.org/draft/2019-09/schema"
  toJSON Draft202012 = "https://json-schema.org/draft/2020-12/schema"

instance FromJSON JsonSchemaVersion where
  parseJSON = Aeson.withText "JsonSchemaVersion" $ \case
    "http://json-schema.org/draft-04/schema#" -> pure Draft04
    "http://json-schema.org/draft-06/schema#" -> pure Draft06
    "http://json-schema.org/draft-07/schema#" -> pure Draft07
    "https://json-schema.org/draft/2019-09/schema" -> pure Draft201909
    "https://json-schema.org/draft/2020-12/schema" -> pure Draft202012
    other -> fail $ "Unknown schema version: " <> T.unpack other

-- | One or many values (for type unions)
data OneOrMany a
  = One a
  | Many (NonEmpty a)
  deriving (Eq, Show, Ord, Functor, Foldable, Traversable, Generic)
  deriving stock Lift

-- | JSON Schema primitive types
data SchemaType
  = NullType
  | BooleanType
  | ObjectType
  | ArrayType
  | NumberType
  | StringType
  | IntegerType
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving stock Lift

instance ToJSON SchemaType where
  toJSON NullType = "null"
  toJSON BooleanType = "boolean"
  toJSON ObjectType = "object"
  toJSON ArrayType = "array"
  toJSON NumberType = "number"
  toJSON StringType = "string"
  toJSON IntegerType = "integer"

instance FromJSON SchemaType where
  parseJSON = Aeson.withText "SchemaType" $ \case
    "null" -> pure NullType
    "boolean" -> pure BooleanType
    "object" -> pure ObjectType
    "array" -> pure ArrayType
    "number" -> pure NumberType
    "string" -> pure StringType
    "integer" -> pure IntegerType
    other -> fail $ "Unknown schema type: " <> T.unpack other

-- | Semantic format specifiers
data Format
  = DateTime | Date | Time | Duration
  | Email | IDNEmail
  | Hostname | IDNHostname
  | IPv4 | IPv6
  | URI | URIRef | IRI | IRIRef
  | URITemplate
  | JSONPointerFormat | RelativeJSONPointerFormat
  | RegexFormat
  | UUID
  | CustomFormat Text
  deriving (Eq, Show, Ord, Generic)
  deriving stock Lift

instance ToJSON Format where
  toJSON DateTime = "date-time"
  toJSON Date = "date"
  toJSON Time = "time"
  toJSON Duration = "duration"
  toJSON Email = "email"
  toJSON IDNEmail = "idn-email"
  toJSON Hostname = "hostname"
  toJSON IDNHostname = "idn-hostname"
  toJSON IPv4 = "ipv4"
  toJSON IPv6 = "ipv6"
  toJSON URI = "uri"
  toJSON URIRef = "uri-reference"
  toJSON IRI = "iri"
  toJSON IRIRef = "iri-reference"
  toJSON URITemplate = "uri-template"
  toJSON JSONPointerFormat = "json-pointer"
  toJSON RelativeJSONPointerFormat = "relative-json-pointer"
  toJSON RegexFormat = "regex"
  toJSON UUID = "uuid"
  toJSON (CustomFormat t) = Aeson.String t

instance FromJSON Format where
  parseJSON = Aeson.withText "Format" $ \case
    "date-time" -> pure DateTime
    "date" -> pure Date
    "time" -> pure Time
    "duration" -> pure Duration
    "email" -> pure Email
    "idn-email" -> pure IDNEmail
    "hostname" -> pure Hostname
    "idn-hostname" -> pure IDNHostname
    "ipv4" -> pure IPv4
    "ipv6" -> pure IPv6
    "uri" -> pure URI
    "uri-reference" -> pure URIRef
    "iri" -> pure IRI
    "iri-reference" -> pure IRIRef
    "uri-template" -> pure URITemplate
    "json-pointer" -> pure JSONPointerFormat
    "relative-json-pointer" -> pure RelativeJSONPointerFormat
    "regex" -> pure RegexFormat
    "uuid" -> pure UUID
    other -> pure (CustomFormat other)

-- | Array items validation (version-dependent)
data ArrayItemsValidation
  = ItemsSchema Schema
    -- ^ All items must match this schema
  | ItemsTuple (NonEmpty Schema) (Maybe Schema)
    -- ^ Tuple: positional schemas + optional additional items schema
  deriving (Eq, Show, Generic)
  deriving stock Lift

instance ToJSON ArrayItemsValidation where
  toJSON (ItemsSchema schema) = toJSON schema
  toJSON (ItemsTuple schemas Nothing) = Aeson.Array $ fromList $ map toJSON $ NE.toList schemas
  toJSON (ItemsTuple schemas (Just additional)) = Aeson.object
    [ "items" .= NE.toList schemas
    , "additionalItems" .= additional
    ]

instance FromJSON ArrayItemsValidation where
  parseJSON v@(Aeson.Object _) = ItemsSchema <$> parseJSON v
  parseJSON (Aeson.Array arr) = do
    schemas <- traverse parseJSON (toList arr)
    case NE.nonEmpty schemas of
      Just ne -> pure $ ItemsTuple ne Nothing
      Nothing -> fail "items array cannot be empty"
  parseJSON _ = fail "items must be object or array"

-- | Dependency specification (draft-04 through draft-07, deprecated in 2019-09+)
data Dependency
  = DependencyProperties (Set Text)
    -- ^ If property exists, these properties must exist
  | DependencySchema Schema
    -- ^ If property exists, this schema must validate
  deriving (Eq, Show, Generic)
  deriving stock Lift

instance ToJSON Dependency where
  toJSON (DependencyProperties props) = toJSON $ Set.toList props
  toJSON (DependencySchema schema) = toJSON schema

instance FromJSON Dependency where
  parseJSON v@(Aeson.Object _) = DependencySchema <$> parseJSON v
  parseJSON v@(Aeson.Bool _) = DependencySchema <$> parseJSON v  -- Boolean schemas (true/false)
  parseJSON (Aeson.Array arr) = DependencyProperties . Set.fromList <$> traverse parseJSON (toList arr)
  parseJSON _ = fail "dependency must be object, boolean, or array"

-- | Schema validation keywords grouped by category
data SchemaValidation = SchemaValidation
  { -- === Numeric Validation ===
    validationMultipleOf :: Maybe Scientific
    -- ^ Number must be multiple of this value (must be > 0)
  
  , validationMaximum :: Maybe Scientific
    -- ^ Maximum value (inclusive unless exclusiveMaximum set)
  
  , validationExclusiveMaximum :: Maybe (Either Bool Scientific)
    -- ^ Left Bool: draft-04 (modifies maximum)
    -- ^ Right Scientific: draft-06+ (standalone constraint)
  
  , validationMinimum :: Maybe Scientific
    -- ^ Minimum value (inclusive unless exclusiveMinimum set)
  
  , validationExclusiveMinimum :: Maybe (Either Bool Scientific)
    -- ^ Left Bool: draft-04, Right Scientific: draft-06+

    -- === String Validation ===
  , validationMaxLength :: Maybe Natural
    -- ^ Maximum string length (in Unicode characters)
  
  , validationMinLength :: Maybe Natural
    -- ^ Minimum string length (in Unicode characters)
  
  , validationPattern :: Maybe Regex
    -- ^ Regular expression pattern (ECMA-262)
  
  , validationFormat :: Maybe Format
    -- ^ Semantic format (email, uri, date-time, etc.)

  , validationContentEncoding :: Maybe Text
    -- ^ Content encoding (e.g., "base64", "base64url") (draft-07+)

  , validationContentMediaType :: Maybe Text
    -- ^ Content media type (e.g., "application/json") (draft-07+)

    -- === Array Validation ===
  , validationItems :: Maybe ArrayItemsValidation
    -- ^ Items validation (version-dependent structure)
  
  , validationPrefixItems :: Maybe (NonEmpty Schema)
    -- ^ Tuple validation (2020-12+)
  
  , validationContains :: Maybe Schema
    -- ^ At least one item must match
  
  , validationMaxItems :: Maybe Natural
    -- ^ Maximum array length
  
  , validationMinItems :: Maybe Natural
    -- ^ Minimum array length
  
  , validationUniqueItems :: Maybe Bool
    -- ^ All items must be unique
  
  , validationMaxContains :: Maybe Natural
    -- ^ Maximum items matching 'contains' (2019-09+)
  
  , validationMinContains :: Maybe Natural
    -- ^ Minimum items matching 'contains' (2019-09+)
  
  , validationUnevaluatedItems :: Maybe Schema
    -- ^ Schema for unevaluated items (2019-09+)

    -- === Object Validation ===
  , validationProperties :: Maybe (Map Text Schema)
    -- ^ Property-specific schemas
  
  , validationPatternProperties :: Maybe (Map Regex Schema)
    -- ^ Regex pattern matching for property names
  
  , validationAdditionalProperties :: Maybe Schema
    -- ^ Schema for additional properties
  
  , validationUnevaluatedProperties :: Maybe Schema
    -- ^ Schema for unevaluated properties (2019-09+)
  
  , validationPropertyNames :: Maybe Schema
    -- ^ Schema that property names must satisfy
  
  , validationMaxProperties :: Maybe Natural
    -- ^ Maximum number of properties
  
  , validationMinProperties :: Maybe Natural
    -- ^ Minimum number of properties
  
  , validationRequired :: Maybe (Set Text)
    -- ^ Set of required property names
  
  , validationDependentRequired :: Maybe (Map Text (Set Text))
    -- ^ Property dependencies for required fields (2019-09+)
  
  , validationDependentSchemas :: Maybe (Map Text Schema)
    -- ^ Property dependencies with schemas (2019-09+)
  
  , validationDependencies :: Maybe (Map Text Dependency)
    -- ^ Property dependencies (draft-04/06/07, deprecated in 2019-09+)
  }
  deriving (Eq, Show, Generic)
  deriving stock Lift

-- | Code generation annotations (custom vocabulary)
data CodegenAnnotations = CodegenAnnotations
  { codegenTypeName :: Maybe Text
    -- ^ Override generated type name
  
  , codegenNewtype :: Maybe NewtypeSpec
    -- ^ Generate newtype instead of type alias
  
  , codegenFieldMapping :: Maybe (Map Text Text)
    -- ^ Map JSON field names to Haskell field names
  
  , codegenOmitEmpty :: Maybe Bool
    -- ^ Omit null/empty values in ToJSON
  
  , codegenStrictFields :: Maybe Bool
    -- ^ Use strict fields (BangPatterns)
  
  , codegenCustomStrategy :: Maybe Text
    -- ^ Name of custom generation strategy
  
  , codegenImports :: [Text]
    -- ^ Additional imports needed for generated code
  }
  deriving (Eq, Show, Generic)
  deriving stock Lift

-- | Newtype generation specification
data NewtypeSpec = NewtypeSpec
  { newtypeConstructor :: Text
    -- ^ Constructor name (must be valid Haskell identifier)
  
  , newtypeModule :: Maybe Text
    -- ^ Module to generate newtype in
  
  , newtypeValidation :: Maybe Text
    -- ^ Validation function name
  
  , newtypeInstances :: [Text]
    -- ^ Additional typeclass instances to derive
  }
  deriving (Eq, Show, Generic)
  deriving stock Lift

-- | Schema annotations and metadata
data SchemaAnnotations = SchemaAnnotations
  { annotationTitle :: Maybe Text
    -- ^ Short description
  
  , annotationDescription :: Maybe Text
    -- ^ Detailed description (may be Markdown)
  
  , annotationDefault :: Maybe Value
    -- ^ Default value for this schema
  
  , annotationExamples :: [Value]
    -- ^ Example values
  
  , annotationDeprecated :: Maybe Bool
    -- ^ Whether this schema is deprecated (2019-09+)
  
  , annotationReadOnly :: Maybe Bool
    -- ^ Property is read-only
  
  , annotationWriteOnly :: Maybe Bool
    -- ^ Property is write-only (invariant: not both readOnly and writeOnly)
  
  , annotationComment :: Maybe Text
    -- ^ Comments for schema authors ($comment, draft-07+)

    -- === Custom Codegen Annotations ===
  , annotationCodegen :: Maybe CodegenAnnotations
    -- ^ Code generation hints (custom extension)
  }
  deriving (Eq, Show, Generic)
  deriving stock Lift

-- | Core schema structure: boolean shorthand or full object
data SchemaCore
  = BooleanSchema Bool
    -- ^ true = allow all, false = allow none
  | ObjectSchema SchemaObject
    -- ^ Full schema object with keywords
  deriving (Eq, Show, Generic)
  deriving stock Lift

-- | Object schema with all keyword groups
-- This is the actual record structure containing all schema keywords
data ObjectSchemaData = ObjectSchemaData
  { -- === Type Keywords ===
    objectSchemaDataType :: Maybe (OneOrMany SchemaType)
    -- ^ Type constraint (single type or union)
  
  , objectSchemaDataEnum :: Maybe (NonEmpty Value)
    -- ^ Enumeration of allowed values (at least one)
  
  , objectSchemaDataConst :: Maybe Value
    -- ^ Single allowed value (draft-06+)

    -- === Reference Keywords ===
  , objectSchemaDataRef :: Maybe Reference
    -- ^ Reference to another schema ($ref)
  
  , objectSchemaDataDynamicRef :: Maybe Reference
    -- ^ Dynamic reference (2020-12+)
  
  , objectSchemaDataAnchor :: Maybe Text
    -- ^ Named anchor for references ($anchor, 2019-09+)
  
  , objectSchemaDataDynamicAnchor :: Maybe Text
    -- ^ Dynamic anchor (2020-12+)
  
  , objectSchemaDataRecursiveRef :: Maybe Reference
    -- ^ Recursive reference (2019-09, replaced by $dynamicRef in 2020-12)

  , objectSchemaDataRecursiveAnchor :: Maybe Bool
    -- ^ Recursive anchor (2019-09, replaced by $dynamicAnchor in 2020-12)

    -- === Composition Keywords ===
  , objectSchemaDataAllOf :: Maybe (NonEmpty Schema)
    -- ^ Must satisfy all subschemas
  
  , objectSchemaDataAnyOf :: Maybe (NonEmpty Schema)
    -- ^ Must satisfy at least one subschema
  
  , objectSchemaDataOneOf :: Maybe (NonEmpty Schema)
    -- ^ Must satisfy exactly one subschema
  
  , objectSchemaDataNot :: Maybe Schema
    -- ^ Must NOT satisfy this subschema

    -- === Conditional Keywords (draft-07+) ===
  , objectSchemaDataIf :: Maybe Schema
    -- ^ Condition schema
  
  , objectSchemaDataThen :: Maybe Schema
    -- ^ Applied if 'if' validates successfully
  
  , objectSchemaDataElse :: Maybe Schema
    -- ^ Applied if 'if' validates unsuccessfully

    -- === Validation Substructure ===
  , objectSchemaDataValidation :: SchemaValidation
    -- ^ All validation keywords

    -- === Annotation/Metadata ===
  , objectSchemaDataAnnotations :: SchemaAnnotations
    -- ^ Annotations (title, description, examples, etc.)

    -- === Definition Storage ===
  , objectSchemaDataDefs :: Map Text Schema
    -- ^ Local schema definitions ($defs or definitions)
  }
  deriving (Eq, Show, Generic)
  deriving stock Lift

-- | Complete schema object: boolean shorthand or full object schema
-- Under the hood, this is `Either Bool ObjectSchemaData`
newtype SchemaObject = SchemaObject (Either Bool ObjectSchemaData)
  deriving (Eq, Show, Generic)
  deriving stock Lift

-- | Pattern synonym for boolean schema objects
pattern BooleanSchemaObject :: Bool -> SchemaObject
pattern BooleanSchemaObject b <- SchemaObject (Left b) where
  BooleanSchemaObject b = SchemaObject (Left b)

-- | Pattern synonym for object schema objects
pattern ObjectSchemaObject :: ObjectSchemaData -> SchemaObject
pattern ObjectSchemaObject obj <- SchemaObject (Right obj) where
  ObjectSchemaObject obj = SchemaObject (Right obj)

{-# COMPLETE BooleanSchemaObject, ObjectSchemaObject #-}

-- | Accessor functions for SchemaObject fields (backward compatibility)
-- These return Nothing for boolean schemas

schemaType :: SchemaObject -> Maybe (OneOrMany SchemaType)
schemaType (SchemaObject (Left _)) = Nothing
schemaType (SchemaObject (Right obj)) = objectSchemaDataType obj

schemaEnum :: SchemaObject -> Maybe (NonEmpty Value)
schemaEnum (SchemaObject (Left _)) = Nothing
schemaEnum (SchemaObject (Right obj)) = objectSchemaDataEnum obj

schemaConst :: SchemaObject -> Maybe Value
schemaConst (SchemaObject (Left _)) = Nothing
schemaConst (SchemaObject (Right obj)) = objectSchemaDataConst obj

schemaRef :: SchemaObject -> Maybe Reference
schemaRef (SchemaObject (Left _)) = Nothing
schemaRef (SchemaObject (Right obj)) = objectSchemaDataRef obj

schemaDynamicRef :: SchemaObject -> Maybe Reference
schemaDynamicRef (SchemaObject (Left _)) = Nothing
schemaDynamicRef (SchemaObject (Right obj)) = objectSchemaDataDynamicRef obj

schemaAnchor :: SchemaObject -> Maybe Text
schemaAnchor (SchemaObject (Left _)) = Nothing
schemaAnchor (SchemaObject (Right obj)) = objectSchemaDataAnchor obj

schemaDynamicAnchor :: SchemaObject -> Maybe Text
schemaDynamicAnchor (SchemaObject (Left _)) = Nothing
schemaDynamicAnchor (SchemaObject (Right obj)) = objectSchemaDataDynamicAnchor obj

schemaRecursiveRef :: SchemaObject -> Maybe Reference
schemaRecursiveRef (SchemaObject (Left _)) = Nothing
schemaRecursiveRef (SchemaObject (Right obj)) = objectSchemaDataRecursiveRef obj

schemaRecursiveAnchor :: SchemaObject -> Maybe Bool
schemaRecursiveAnchor (SchemaObject (Left _)) = Nothing
schemaRecursiveAnchor (SchemaObject (Right obj)) = objectSchemaDataRecursiveAnchor obj

schemaAllOf :: SchemaObject -> Maybe (NonEmpty Schema)
schemaAllOf (SchemaObject (Left _)) = Nothing
schemaAllOf (SchemaObject (Right obj)) = objectSchemaDataAllOf obj

schemaAnyOf :: SchemaObject -> Maybe (NonEmpty Schema)
schemaAnyOf (SchemaObject (Left _)) = Nothing
schemaAnyOf (SchemaObject (Right obj)) = objectSchemaDataAnyOf obj

schemaOneOf :: SchemaObject -> Maybe (NonEmpty Schema)
schemaOneOf (SchemaObject (Left _)) = Nothing
schemaOneOf (SchemaObject (Right obj)) = objectSchemaDataOneOf obj

schemaNot :: SchemaObject -> Maybe Schema
schemaNot (SchemaObject (Left _)) = Nothing
schemaNot (SchemaObject (Right obj)) = objectSchemaDataNot obj

schemaIf :: SchemaObject -> Maybe Schema
schemaIf (SchemaObject (Left _)) = Nothing
schemaIf (SchemaObject (Right obj)) = objectSchemaDataIf obj

schemaThen :: SchemaObject -> Maybe Schema
schemaThen (SchemaObject (Left _)) = Nothing
schemaThen (SchemaObject (Right obj)) = objectSchemaDataThen obj

schemaElse :: SchemaObject -> Maybe Schema
schemaElse (SchemaObject (Left _)) = Nothing
schemaElse (SchemaObject (Right obj)) = objectSchemaDataElse obj

schemaValidation :: SchemaObject -> SchemaValidation
schemaValidation (SchemaObject (Left _)) = SchemaValidation
  { validationMultipleOf = Nothing
  , validationMaximum = Nothing
  , validationExclusiveMaximum = Nothing
  , validationMinimum = Nothing
  , validationExclusiveMinimum = Nothing
  , validationMaxLength = Nothing
  , validationMinLength = Nothing
  , validationPattern = Nothing
  , validationFormat = Nothing
  , validationContentEncoding = Nothing
  , validationContentMediaType = Nothing
  , validationItems = Nothing
  , validationPrefixItems = Nothing
  , validationContains = Nothing
  , validationMaxItems = Nothing
  , validationMinItems = Nothing
  , validationUniqueItems = Nothing
  , validationMaxContains = Nothing
  , validationMinContains = Nothing
  , validationProperties = Nothing
  , validationPatternProperties = Nothing
  , validationAdditionalProperties = Nothing
  , validationUnevaluatedProperties = Nothing
  , validationUnevaluatedItems = Nothing
  , validationPropertyNames = Nothing
  , validationMaxProperties = Nothing
  , validationMinProperties = Nothing
  , validationRequired = Nothing
  , validationDependentRequired = Nothing
  , validationDependentSchemas = Nothing
  , validationDependencies = Nothing
  }
schemaValidation (SchemaObject (Right obj)) = objectSchemaDataValidation obj

schemaAnnotations :: SchemaObject -> SchemaAnnotations
schemaAnnotations (SchemaObject (Left _)) = SchemaAnnotations
  { annotationTitle = Nothing
  , annotationDescription = Nothing
  , annotationDefault = Nothing
  , annotationExamples = []
  , annotationDeprecated = Nothing
  , annotationReadOnly = Nothing
  , annotationWriteOnly = Nothing
  , annotationComment = Nothing
  , annotationCodegen = Nothing
  }
schemaAnnotations (SchemaObject (Right obj)) = objectSchemaDataAnnotations obj

schemaDefs :: SchemaObject -> Map Text Schema
schemaDefs (SchemaObject (Left _)) = Map.empty
schemaDefs (SchemaObject (Right obj)) = objectSchemaDataDefs obj

-- | Top-level JSON Schema document
data Schema = Schema
  { schemaVersion :: Maybe JsonSchemaVersion
    -- ^ Schema version from $schema keyword. Nothing means latest version.

  , schemaMetaschemaURI :: Maybe Text
    -- ^ Full $schema URI (e.g., "http://localhost:1234/custom-metaschema.json")
    -- Used to look up the metaschema in the registry to extract vocabulary restrictions

  , schemaId :: Maybe Text
    -- ^ Unique identifier for this schema ($id keyword)
    -- Invariant: Must be absolute URI if present

  , schemaCore :: SchemaCore
    -- ^ Core schema structure (boolean or object schema)

  , schemaVocabulary :: Maybe (Map Text Bool)
    -- ^ Vocabularies and whether they're required ($vocabulary keyword)
    -- Only valid for 2019-09+
    -- Invariant: All required vocabularies must be understood

  , schemaExtensions :: Map Text Value
    -- ^ Unknown keywords collected during parsing

  , schemaRawKeywords :: Map Text Value
    -- ^ All keywords in their raw Value form (for monadic compilation)
    -- This preserves the original keyword values for adjacent keyword access
    -- during compilation. Populated during parsing.
  }
  deriving (Eq, Show, Generic)
  deriving stock Lift

-- | A 'SchemaValidation' with every keyword cleared. Useful starting
-- point when constructing schemas in code (e.g. from
-- 'JsonSchema.Class.HasJsonSchema' instances).
defaultSchemaValidation :: SchemaValidation
defaultSchemaValidation = SchemaValidation
  { validationMultipleOf = Nothing
  , validationMaximum = Nothing
  , validationExclusiveMaximum = Nothing
  , validationMinimum = Nothing
  , validationExclusiveMinimum = Nothing
  , validationMaxLength = Nothing
  , validationMinLength = Nothing
  , validationPattern = Nothing
  , validationFormat = Nothing
  , validationContentEncoding = Nothing
  , validationContentMediaType = Nothing
  , validationItems = Nothing
  , validationPrefixItems = Nothing
  , validationContains = Nothing
  , validationMaxItems = Nothing
  , validationMinItems = Nothing
  , validationUniqueItems = Nothing
  , validationMaxContains = Nothing
  , validationMinContains = Nothing
  , validationUnevaluatedItems = Nothing
  , validationProperties = Nothing
  , validationPatternProperties = Nothing
  , validationAdditionalProperties = Nothing
  , validationUnevaluatedProperties = Nothing
  , validationPropertyNames = Nothing
  , validationMaxProperties = Nothing
  , validationMinProperties = Nothing
  , validationRequired = Nothing
  , validationDependentRequired = Nothing
  , validationDependentSchemas = Nothing
  , validationDependencies = Nothing
  }

-- | A 'SchemaAnnotations' with every annotation cleared.
defaultSchemaAnnotations :: SchemaAnnotations
defaultSchemaAnnotations = SchemaAnnotations
  { annotationTitle = Nothing
  , annotationDescription = Nothing
  , annotationDefault = Nothing
  , annotationExamples = []
  , annotationDeprecated = Nothing
  , annotationReadOnly = Nothing
  , annotationWriteOnly = Nothing
  , annotationComment = Nothing
  , annotationCodegen = Nothing
  }

-- | An 'ObjectSchemaData' with every keyword cleared. Mutate this with
-- record-update syntax when assembling schemas in code.
defaultObjectSchemaData :: ObjectSchemaData
defaultObjectSchemaData = ObjectSchemaData
  { objectSchemaDataType = Nothing
  , objectSchemaDataEnum = Nothing
  , objectSchemaDataConst = Nothing
  , objectSchemaDataRef = Nothing
  , objectSchemaDataDynamicRef = Nothing
  , objectSchemaDataAnchor = Nothing
  , objectSchemaDataDynamicAnchor = Nothing
  , objectSchemaDataRecursiveRef = Nothing
  , objectSchemaDataRecursiveAnchor = Nothing
  , objectSchemaDataAllOf = Nothing
  , objectSchemaDataAnyOf = Nothing
  , objectSchemaDataOneOf = Nothing
  , objectSchemaDataNot = Nothing
  , objectSchemaDataIf = Nothing
  , objectSchemaDataThen = Nothing
  , objectSchemaDataElse = Nothing
  , objectSchemaDataValidation = defaultSchemaValidation
  , objectSchemaDataAnnotations = defaultSchemaAnnotations
  , objectSchemaDataDefs = Map.empty
  }

-- | Build a top-level 'Schema' wrapping a boolean schema (@true@ /
-- @false@). 'True' validates everything; 'False' rejects everything.
booleanSchema :: Bool -> Schema
booleanSchema b = Schema
  { schemaVersion = Nothing
  , schemaMetaschemaURI = Nothing
  , schemaId = Nothing
  , schemaCore = BooleanSchema b
  , schemaVocabulary = Nothing
  , schemaExtensions = Map.empty
  , schemaRawKeywords = Map.empty
  }

-- | Build a top-level 'Schema' from an 'ObjectSchemaData' with no
-- @\$schema@, @\$id@ or vocabulary. Use record-update on the returned
-- 'Schema' to set those fields if needed.
objectSchema :: ObjectSchemaData -> Schema
objectSchema o = Schema
  { schemaVersion = Nothing
  , schemaMetaschemaURI = Nothing
  , schemaId = Nothing
  , schemaCore = ObjectSchema (SchemaObject (Right o))
  , schemaVocabulary = Nothing
  , schemaExtensions = Map.empty
  , schemaRawKeywords = Map.empty
  }

-- __Note on Manual Schema Construction__:
--
-- The @schemaRawKeywords@ field must be populated for the pluggable keyword system to work.
-- The parser automatically populates this field. If you need to construct schemas manually
-- for testing or other purposes, use @JsonSchema.Parser.parseSchema@ instead of
-- direct construction, or ensure @schemaRawKeywords@ is properly populated from your typed
-- fields.

-- | Re-export new validation result types from Validator.Result
type ValidationResult = VR.ValidationResult
type ValidationError = VR.ValidationError
type ValidationErrorTree = VR.ValidationErrorTree
type AnnotationCollection = VR.AnnotationCollection
type SomeAnnotation = VR.SomeAnnotation

-- | Legacy type for backward compatibility
-- Maps JSON Pointer -> keyword -> value (untyped annotations)
newtype ValidationAnnotations = ValidationAnnotations 
  { unAnnotations :: Map JsonPointer (Map Text Value) }
  deriving (Eq, Show, Generic)

instance Semigroup ValidationAnnotations where
  ValidationAnnotations a <> ValidationAnnotations b =
    ValidationAnnotations (Map.unionWith mergeAnnotationMaps a b)
    where
      mergeAnnotationMaps :: Map Text Value -> Map Text Value -> Map Text Value
      mergeAnnotationMaps = Map.unionWith mergeAnnotationValues
      mergeAnnotationValues :: Value -> Value -> Value
      mergeAnnotationValues (Aeson.Array a1) (Aeson.Array a2) = Aeson.Array (a1 <> a2)
      mergeAnnotationValues v1 _ = v1

instance Monoid ValidationAnnotations where
  mempty = ValidationAnnotations Map.empty

instance ToJSON ValidationAnnotations where
  toJSON (ValidationAnnotations m) =
    Aeson.Object $ KeyMap.fromList $ do
      (k, v) <- Map.toList m
      pure (Key.fromText $ renderPointer k, toJSON v)

instance FromJSON ValidationAnnotations where
  parseJSON = Aeson.withObject "ValidationAnnotations" $ \obj -> do
    pairs <- sequence
      [ case parsePointer (Key.toText k) of
          Right ptr -> do
            innerMap <- parseJSON v
            pure (ptr, innerMap)
          Left err -> fail $ T.unpack err
      | (k, v) <- KeyMap.toList obj
      ]
    pure $ ValidationAnnotations $ Map.fromList pairs

-- | Legacy type for backward compatibility
newtype ValidationErrors = ValidationErrors 
  { unErrors :: NonEmpty ValidationError }
  deriving (Eq, Show, Generic)
  deriving newtype (Semigroup)

-- | Pattern synonym for backward compatibility with ValidationSuccess constructor
pattern ValidationSuccess :: ValidationAnnotations -> ValidationResult
pattern ValidationSuccess anns <- (extractLegacyAnnotations -> Just anns)
  where
    ValidationSuccess anns = legacyToValidationResult anns

-- | Pattern synonym for backward compatibility with ValidationFailure constructor  
pattern ValidationFailure :: ValidationErrors -> ValidationResult
pattern ValidationFailure errs <- (extractLegacyErrors -> Just errs)
  where
    ValidationFailure errs = legacyErrorsToValidationResult errs

{-# COMPLETE ValidationSuccess, ValidationFailure #-}

-- | Extract legacy annotations from new ValidationResult (for pattern matching)
extractLegacyAnnotations :: ValidationResult -> Maybe ValidationAnnotations
extractLegacyAnnotations result
  | VR.isSuccess result =
      -- Convert AnnotationCollection back to ValidationAnnotations
      -- Since AnnotationCollection doesn't store keyword names, we reconstruct
      -- a compatible format where all annotations are stored under an empty keyword map
      Just $ annotationCollectionToLegacy (VR.resultAnnotations result)
  | otherwise = Nothing
  where
    annotationCollectionToLegacy :: VR.AnnotationCollection -> ValidationAnnotations
    annotationCollectionToLegacy (VR.AnnotationCollection m) =
      -- Convert back to legacy format by extracting stored keyword maps
      ValidationAnnotations $ Map.fromList
        [ (path, extractKeywordMap annotations)
        | (path, annotations) <- Map.toList m
        ]
    
    extractKeywordMap :: [VR.SomeAnnotation] -> Map Text Value
    extractKeywordMap [] = Map.empty
    extractKeywordMap (VR.SomeAnnotation val _ty : rest) =
      case cast val of
        Just (kwMap :: Map Text Value) -> kwMap <> extractKeywordMap rest
        Nothing -> extractKeywordMap rest

-- | Extract legacy errors from new ValidationResult (for pattern matching)
extractLegacyErrors :: ValidationResult -> Maybe ValidationErrors
extractLegacyErrors result
  | VR.isFailure result = 
      -- Convert error tree to flat list
      let flatErrors = flattenErrorTree (VR.resultErrors result)
      in case NE.nonEmpty flatErrors of
        Just ne -> Just (ValidationErrors ne)
        Nothing -> Nothing
  | otherwise = Nothing

-- | Flatten error tree to list
flattenErrorTree :: ValidationErrorTree -> [ValidationError]
flattenErrorTree (VR.ErrorLeaf err) = [err]
flattenErrorTree (VR.ErrorBranch _ children) = concatMap flattenErrorTree children

-- | Convert legacy annotations to new ValidationResult
legacyToValidationResult :: ValidationAnnotations -> ValidationResult
legacyToValidationResult (ValidationAnnotations annMap) =
  -- Convert legacy annotations to new AnnotationCollection
  -- Store the entire keyword map as a single annotation to preserve all data
  let annotations = VR.AnnotationCollection $ Map.fromList
        [ (path, [VR.SomeAnnotation kwMap (typeOf kwMap)])
        | (path, kwMap) <- Map.toList annMap
        ]
  in VR.validationSuccessWithAnnotations annotations

-- | Convert legacy errors to new ValidationResult
legacyErrorsToValidationResult :: ValidationErrors -> ValidationResult
legacyErrorsToValidationResult (ValidationErrors errs) =
  VR.validationFailureTree $ VR.ErrorBranch "root" (map VR.ErrorLeaf $ NE.toList errs)

-- | Type alias for successful validation (legacy)
-- This is kept for backward compatibility but is not actively used
type ValidationSuccess = ValidationAnnotations
{-# WARNING ValidationSuccess "This type alias is deprecated and may be removed in a future version" #-}

-- | Check if validation succeeded
isSuccess :: ValidationResult -> Bool
isSuccess = VR.isSuccess

-- | Check if validation failed
isFailure :: ValidationResult -> Bool
isFailure = VR.isFailure

-- | Create a simple validation error (legacy helper)
validationError :: Text -> ValidationError
validationError msg = VR.ValidationError
  { VR.errorMessage = msg
  , VR.errorSchemaPath = emptyPointer
  , VR.errorInstancePath = emptyPointer
  , VR.errorKeyword = "validation"
  }

-- | Create validation failure with keyword and message
validationFailure :: Text -> Text -> ValidationResult
validationFailure = VR.validationFailure

-- | Schema fingerprint for cycle detection
newtype SchemaFingerprint = SchemaFingerprint ByteString
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (Hashable)

-- | Split a URI into its document part and optional fragment (without the '#')
splitUriFragment :: Text -> (Text, Maybe Text)
splitUriFragment uriText =
  let (docPart, fragmentWithHash) = T.breakOn "#" uriText
  in if T.null fragmentWithHash
        then (uriText, Nothing)
        else (docPart, let fragment = T.drop 1 fragmentWithHash
                       in if T.null fragment then Nothing else Just fragment)

-- | Ensure a list of texts has no duplicates while preserving original order
uniqueTexts :: [Text] -> [Text]
uniqueTexts = go Set.empty
  where
    go _ [] = []
    go seen (x:xs)
      | Set.member x seen = go seen xs
      | otherwise = x : go (Set.insert x seen) xs

-- | Ensure a list of anchor pairs has no duplicates while preserving order
uniqueAnchors :: [(Text, Text)] -> [(Text, Text)]
uniqueAnchors = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
      | Set.member x seen = go seen xs
      | otherwise = x : go (Set.insert x seen) xs

-- | Resolve a URI against an optional base according to RFC 3986
resolveAgainstBaseURI :: Maybe Text -> Text -> Text
resolveAgainstBaseURI parentBase refText =
  case parseURIReference (T.unpack refText) of
    Nothing -> refText
    Just refUri
      | not (null (uriScheme refUri)) -> refText  -- Already absolute
      | otherwise ->
          case parentBase of
            Nothing -> refText
            Just baseText ->
              let (baseDoc, _) = splitUriFragment baseText
              in case parseURIReference (T.unpack baseDoc) of
                   Nothing -> refText
                   Just baseUri ->
                     T.pack $ uriToString id (refUri `relativeTo` baseUri) ""

-- | Registration metadata computed for a schema relative to a parent base URI
data SchemaRegistrationInfo = SchemaRegistrationInfo
  { sriBaseURI :: Maybe Text
  , sriSchemaKeys :: [Text]
  , sriAnchors :: [(Text, Text)]
  }

schemaRegistrationInfo :: Maybe Text -> Schema -> SchemaRegistrationInfo
schemaRegistrationInfo parentBase schema =
  case schemaId schema of
    Nothing ->
      mk parentBase [] []
    Just idText
      | T.isPrefixOf "#" idText ->
          let anchorName = T.drop 1 idText
              baseForAnchor = maybe "" id parentBase
              schemaKeys =
                case parentBase of
                  Just baseUri | not (T.null anchorName) -> [baseUri <> "#" <> anchorName]
                  _ -> []
              anchors =
                if T.null anchorName
                  then []
                  else [(baseForAnchor, anchorName)]
          in mk parentBase schemaKeys anchors
      | otherwise ->
          let resolved = resolveAgainstBaseURI parentBase idText
              (docUri, maybeFragment) = splitUriFragment resolved
              canonical =
                if T.null docUri
                  then resolved
                  else docUri
              baseUri =
                if T.null canonical
                  then Nothing
                  else Just canonical
              schemaKeys =
                if T.null canonical
                  then []
                  else [canonical]
              anchorsFromId =
                case maybeFragment of
                  Just fragmentName | not (T.null fragmentName) ->
                    [(canonical, fragmentName)]
                  _ -> []
          in mk baseUri schemaKeys anchorsFromId
  where
    mk base keys anchors = SchemaRegistrationInfo
      { sriBaseURI = base
      , sriSchemaKeys = uniqueTexts keys
      , sriAnchors = uniqueAnchors anchors
      }

-- | Effective base URI for a schema relative to a parent base URI
schemaEffectiveBase :: Maybe Text -> Schema -> Maybe Text
schemaEffectiveBase parentBase schema =
  sriBaseURI (schemaRegistrationInfo parentBase schema)

-- | Registry of schemas for reference resolution
data SchemaRegistry = SchemaRegistry
  { registrySchemas :: Map Text Schema
    -- ^ URI -> Schema mapping
  
  , registryAnchors :: Map (Text, Text) Schema
    -- ^ (Base URI, anchor name) -> Schema mapping
  
  , registryDynamicAnchors :: Map (Text, Text) Schema
    -- ^ Dynamic anchors (2020-12+)

  , registryRecursiveAnchors :: Map Text Schema
    -- ^ Recursive anchors (2019-09): schemas with $recursiveAnchor: true
  }
  deriving (Eq, Show, Generic)

instance Semigroup SchemaRegistry where
  r1 <> r2 = SchemaRegistry
    { registrySchemas = registrySchemas r1 <> registrySchemas r2
    , registryAnchors = registryAnchors r1 <> registryAnchors r2
    , registryDynamicAnchors = registryDynamicAnchors r1 <> registryDynamicAnchors r2
    , registryRecursiveAnchors = registryRecursiveAnchors r1 <> registryRecursiveAnchors r2
    }

instance Monoid SchemaRegistry where
  mempty = emptyRegistry

-- | Empty schema registry
emptyRegistry :: SchemaRegistry
emptyRegistry = SchemaRegistry
  { registrySchemas = Map.empty
  , registryAnchors = Map.empty
  , registryDynamicAnchors = Map.empty
  , registryRecursiveAnchors = Map.empty
  }

-- | Register a schema and all its sub-schemas in the registry
-- Walks the schema tree and registers:
-- - Schemas with $id (both absolute and fragment-only)
-- - Schemas with $anchor
-- - Schemas with $dynamicAnchor (2020-12+)
registerSchemaInRegistry :: Maybe Text -> Schema -> SchemaRegistry -> SchemaRegistry
registerSchemaInRegistry baseURI schema registry =
  let info = schemaRegistrationInfo baseURI schema
      currentBase = sriBaseURI info
      registryWithSchemas =
        foldr insertSchemaKey registry (sriSchemaKeys info)
      registryWithAnchors =
        foldr insertAnchorPair registryWithSchemas (sriAnchors info)
  in case schemaCore schema of
       BooleanSchema _ -> registryWithAnchors
       ObjectSchema obj ->
         let anchorBase =
               maybe "" id $
                 case currentBase of
                   Just uri -> Just uri
                   Nothing -> baseURI
             registryWithExplicitAnchors =
               let regWithAnchor =
                     maybe registryWithAnchors
                       (\anchorName -> insertAnchor anchorBase anchorName registryWithAnchors)
                       (schemaAnchor obj)
                   regWithDynamic =
                     maybe regWithAnchor
                       (\dynAnchor -> insertDynamicAnchor anchorBase dynAnchor regWithAnchor)
                       (schemaDynamicAnchor obj)
                   regWithRecursive =
                     maybe regWithDynamic
                       (\isRecursive -> if isRecursive
                                        then insertRecursiveAnchor anchorBase regWithDynamic
                                        else regWithDynamic)
                       (schemaRecursiveAnchor obj)
               in regWithRecursive
         in registerObjectSubSchemas currentBase obj registryWithExplicitAnchors
  where
    insertSchemaKey key reg =
      reg { registrySchemas = Map.insertWith preferSchema key schema (registrySchemas reg) }
      where
        preferSchema new existing
          | Map.null (schemaRawKeywords existing)
            , not (Map.null (schemaRawKeywords new)) = new
          | otherwise = existing

    insertAnchorPair (baseKey, anchorName) reg =
      reg { registryAnchors = Map.insert (baseKey, anchorName) schema (registryAnchors reg) }

    insertAnchor baseKey anchorName reg =
      reg { registryAnchors = Map.insert (baseKey, anchorName) schema (registryAnchors reg) }

    insertDynamicAnchor baseKey anchorName reg =
      reg { registryDynamicAnchors = Map.insert (baseKey, anchorName) schema (registryDynamicAnchors reg) }

    insertRecursiveAnchor baseKey reg =
      reg { registryRecursiveAnchors = Map.insert baseKey schema (registryRecursiveAnchors reg) }

    registerObjectSubSchemas :: Maybe Text -> SchemaObject -> SchemaRegistry -> SchemaRegistry
    registerObjectSubSchemas uri obj reg =
      let -- Register definitions
          reg1 = foldr (\defSchema r -> registerSchemaInRegistry uri defSchema r) 
                      reg (Map.elems $ schemaDefs obj)
          
          -- Register composition schemas
          reg2 = maybe reg1 (\schemas -> foldr (\s r -> registerSchemaInRegistry uri s r) reg1 (NE.toList schemas)) 
                      (schemaAllOf obj)
          reg3 = maybe reg2 (\schemas -> foldr (\s r -> registerSchemaInRegistry uri s r) reg2 (NE.toList schemas))
                      (schemaAnyOf obj)
          reg4 = maybe reg3 (\schemas -> foldr (\s r -> registerSchemaInRegistry uri s r) reg3 (NE.toList schemas))
                      (schemaOneOf obj)
          reg5 = maybe reg4 (\s -> registerSchemaInRegistry uri s reg4) (schemaNot obj)
          
          -- Register conditional schemas
          reg6 = maybe reg5 (\s -> registerSchemaInRegistry uri s reg5) (schemaIf obj)
          reg7 = maybe reg6 (\s -> registerSchemaInRegistry uri s reg6) (schemaThen obj)
          reg8 = maybe reg7 (\s -> registerSchemaInRegistry uri s reg7) (schemaElse obj)
          
          -- Register property schemas
          validation = schemaValidation obj
          reg9 = maybe reg8 (\props -> foldr (\s r -> registerSchemaInRegistry uri s r) reg8 (Map.elems props))
                      (validationProperties validation)
          reg10 = maybe reg9 (\patterns -> foldr (\s r -> registerSchemaInRegistry uri s r) reg9 (Map.elems patterns))
                       (validationPatternProperties validation)
          reg11 = maybe reg10 (\s -> registerSchemaInRegistry uri s reg10) 
                       (validationAdditionalProperties validation)
          reg12 = maybe reg11 (\s -> registerSchemaInRegistry uri s reg11)
                       (validationUnevaluatedProperties validation)
          
          -- Register array schemas
          reg13 = case validationItems validation of
            Just (ItemsSchema s) -> registerSchemaInRegistry uri s reg12
            Just (ItemsTuple schemas maybeAdditional) ->
              let r = foldr (\s acc -> registerSchemaInRegistry uri s acc) reg12 (NE.toList schemas)
              in maybe r (\s -> registerSchemaInRegistry uri s r) maybeAdditional
            Nothing -> reg12
          reg14 = maybe reg13 (\schemas -> foldr (\s r -> registerSchemaInRegistry uri s r) reg13 (NE.toList schemas))
                       (validationPrefixItems validation)
          reg15 = maybe reg14 (\s -> registerSchemaInRegistry uri s reg14) (validationContains validation)
          
          -- Register dependent schemas
          reg16 = maybe reg15 (\deps -> foldr (\s r -> registerSchemaInRegistry uri s r) reg15 (Map.elems deps))
                       (validationDependentSchemas validation)
      in reg16

-- | Build a registry with external references loaded
-- This function:
-- 1. Registers the root schema and all sub-schemas
-- 2. Finds all external $ref values
-- 3. Uses the ReferenceLoader to fetch external schemas
-- 4. Recursively registers loaded schemas
--
-- This should be called before validation to enable external $ref resolution
buildRegistryWithExternalRefs
  :: ReferenceLoader           -- ^ Loader for external schemas
  -> Schema                     -- ^ Root schema
  -> IO (Either Text SchemaRegistry)  -- ^ Built registry or error
buildRegistryWithExternalRefs loader rootSchema = do
  let initialRegistry = registerSchemaInRegistry Nothing rootSchema emptyRegistry
      rootInfo = schemaRegistrationInfo Nothing rootSchema
      initialRefs = uniqueTexts $ collectExternalReferenceDocs (sriBaseURI rootInfo) rootSchema
  loadExternal initialRegistry Set.empty initialRefs
  where
    loadExternal :: SchemaRegistry -> Set Text -> [Text] -> IO (Either Text SchemaRegistry)
    loadExternal registry _ [] = pure $ Right registry
    loadExternal registry loaded (uri:uris)
      | T.null uri = loadExternal registry loaded uris
      | Set.member uri loaded = loadExternal registry loaded uris
      | Map.member uri (registrySchemas registry) = loadExternal registry loaded uris
      | otherwise = do
          result <- loader uri
          case result of
            Left err -> pure $ Left err
            Right schema -> do
              let registry' = registerSchemaInRegistry (Just uri) schema registry
                  info = schemaRegistrationInfo (Just uri) schema
                  -- If the schema has no $id and wasn't registered with a key,
                  -- explicitly register it with the URI we used to load it
                  registry'' = if Map.member uri (registrySchemas registry')
                              then registry'
                              else registry' { registrySchemas = Map.insert uri schema (registrySchemas registry') }
                  newRefs = uniqueTexts $ collectExternalReferenceDocs (sriBaseURI info) schema
                  unseen = do
                    ref <- newRefs
                    guard $ not (Set.member ref loaded)
                    guard $ not (Map.member ref (registrySchemas registry''))
                    pure ref
              loadExternal registry'' (Set.insert uri loaded) (uris <> unseen)

    collectExternalReferenceDocs :: Maybe Text -> Schema -> [Text]
    collectExternalReferenceDocs parentBase schema =
      -- Include the $schema URI (metaschema) if present
      let metaschemaRefs = case schemaMetaschemaURI schema of
            Just uri | not (T.isPrefixOf "#" uri) -> [uri]
            _ -> []
      in case schemaCore schema of
        BooleanSchema _ -> metaschemaRefs
        ObjectSchema obj ->
          -- Compute the effective base URI for this schema, accounting for its $id
          let effectiveBase = schemaEffectiveBase parentBase schema
              directRefs = maybe [] (resolveRef effectiveBase) (schemaRef obj)
              dynamicRefs = maybe [] (resolveRef effectiveBase) (schemaDynamicRef obj)
              subRefs = collectFromObject effectiveBase schema obj
          in uniqueTexts (metaschemaRefs <> directRefs <> dynamicRefs <> subRefs)

    resolveRef :: Maybe Text -> Reference -> [Text]
    resolveRef base (Reference refText)
      | T.null refText = []
      | T.isPrefixOf "#" refText = []
      | otherwise =
          let resolved = resolveAgainstBaseURI base refText
              (docUri, _) = splitUriFragment resolved
              target =
                if T.null docUri
                  then resolved
                  else docUri
          in if T.null target then [] else [target]

    collectFromObject :: Maybe Text -> Schema -> SchemaObject -> [Text]
    collectFromObject base parentSchema obj =
      -- IMPORTANT: For $defs, each definition may have its own $id that changes the base URI.
      -- We must use the parent schema's effective base when collecting from $defs, because
      -- the parent might have an $id. But collectExternalReferenceDocs will compute each
      -- def's effective base internally. So we should pass `base` here, which is the
      -- effective base of the parent schema (computed by collectExternalReferenceDocs).
      let version = fromMaybe Draft202012 (schemaVersion parentSchema)
          rawKeywords = schemaRawKeywords parentSchema
          defRefs = concatMap (collectExternalReferenceDocs base) (Map.elems $ schemaDefs obj)
          allOfRefs = maybe [] (concatMap (collectExternalReferenceDocs base) . NE.toList) (schemaAllOf obj)
            <> collectRefsFromRawKeyword base parentSchema version rawKeywords "allOf"
          anyOfRefs = maybe [] (concatMap (collectExternalReferenceDocs base) . NE.toList) (schemaAnyOf obj)
            <> collectRefsFromRawKeyword base parentSchema version rawKeywords "anyOf"
          oneOfRefs = maybe [] (concatMap (collectExternalReferenceDocs base) . NE.toList) (schemaOneOf obj)
            <> collectRefsFromRawKeyword base parentSchema version rawKeywords "oneOf"
          notRefs = maybe [] (collectExternalReferenceDocs base) (schemaNot obj)
            <> collectRefsFromRawKeyword base parentSchema version rawKeywords "not"
          ifRefs = maybe [] (collectExternalReferenceDocs base) (schemaIf obj)
            <> collectRefsFromRawKeyword base parentSchema version rawKeywords "if"
          thenRefs = maybe [] (collectExternalReferenceDocs base) (schemaThen obj)
            <> collectRefsFromRawKeyword base parentSchema version rawKeywords "then"
          elseRefs = maybe [] (collectExternalReferenceDocs base) (schemaElse obj)
            <> collectRefsFromRawKeyword base parentSchema version rawKeywords "else"
          validation = schemaValidation obj
          propRefs = maybe [] (concatMap (collectExternalReferenceDocs base) . Map.elems) (validationProperties validation)
            <> collectRefsFromRawKeyword base parentSchema version rawKeywords "properties"
          patternRefs = maybe [] (concatMap (collectExternalReferenceDocs base) . Map.elems) (validationPatternProperties validation)
            <> collectRefsFromRawKeyword base parentSchema version rawKeywords "patternProperties"
          additionalRefs = maybe [] (collectExternalReferenceDocs base) (validationAdditionalProperties validation)
            <> collectRefsFromRawKeyword base parentSchema version rawKeywords "additionalProperties"
          unevaluatedRefs = maybe [] (collectExternalReferenceDocs base) (validationUnevaluatedProperties validation)
            <> collectRefsFromRawKeyword base parentSchema version rawKeywords "unevaluatedProperties"
          itemsRefs = case validationItems validation of
            Just (ItemsSchema s) -> collectExternalReferenceDocs base s
            Just (ItemsTuple schemas maybeAdditional) ->
              concatMap (collectExternalReferenceDocs base) (NE.toList schemas) <>
              maybe [] (collectExternalReferenceDocs base) maybeAdditional
            Nothing -> collectRefsFromRawKeyword base parentSchema version rawKeywords "items"
          prefixRefs = maybe [] (concatMap (collectExternalReferenceDocs base) . NE.toList) (validationPrefixItems validation)
            <> collectRefsFromRawKeyword base parentSchema version rawKeywords "prefixItems"
          containsRefs = maybe [] (collectExternalReferenceDocs base) (validationContains validation)
            <> collectRefsFromRawKeyword base parentSchema version rawKeywords "contains"
          dependentSchemaRefs = maybe [] (concatMap (collectExternalReferenceDocs base) . Map.elems) (validationDependentSchemas validation)
            <> collectRefsFromRawKeyword base parentSchema version rawKeywords "dependentSchemas"
      in concat
           [ defRefs, allOfRefs, anyOfRefs, oneOfRefs, notRefs
           , ifRefs, thenRefs, elseRefs, propRefs, patternRefs
           , additionalRefs, unevaluatedRefs, itemsRefs, prefixRefs
           , containsRefs, dependentSchemaRefs
           ]
      
    -- Helper to collect references from a raw keyword value when pre-parsed field is Nothing
    collectRefsFromRawKeyword :: Maybe Text -> Schema -> JsonSchemaVersion -> Map Text Value -> Text -> [Text]
    collectRefsFromRawKeyword base parentSchema version rawKeywords keywordName =
      case Map.lookup keywordName rawKeywords of
        Nothing -> []
        Just val -> collectRefsFromValue base parentSchema version val
      
    -- Helper to collect references from a JSON Value (extracting $ref/$dynamicRef directly)
    -- We extract references directly from JSON without full parsing to avoid circular dependencies
    collectRefsFromValue :: Maybe Text -> Schema -> JsonSchemaVersion -> Value -> [Text]
    collectRefsFromValue base parentSchema version val =
      case val of
        Aeson.Object objMap ->
          -- Extract $ref and $dynamicRef directly from the object
          let refRefs = case KeyMap.lookup "$ref" objMap of
                Just (Aeson.String refText) -> resolveRefFromText base refText
                _ -> []
              dynamicRefRefs = if version >= Draft202012
                               then case KeyMap.lookup "$dynamicRef" objMap of
                                 Just (Aeson.String refText) -> resolveRefFromText base refText
                                 _ -> []
                               else []
              -- Also check for references in nested subschemas
              -- For array keywords (allOf, anyOf, oneOf, prefixItems), extract from array elements
              arrayKeywords = ["allOf", "anyOf", "oneOf", "prefixItems"]
              arrayRefs = concatMap (\k -> case KeyMap.lookup (Key.fromText k) objMap of
                Just (Aeson.Array arr) -> concatMap (collectRefsFromValue base parentSchema version) (toList arr)
                _ -> []) arrayKeywords
              -- For single schema keywords
              singleSchemaKeywords = ["not", "if", "then", "else", "additionalProperties",
                                     "unevaluatedProperties", "items", "contains", "propertyNames"]
              singleRefs = concatMap (\k -> case KeyMap.lookup (Key.fromText k) objMap of
                Just v -> collectRefsFromValue base parentSchema version v
                _ -> []) singleSchemaKeywords
              -- For object keywords (properties, patternProperties, dependentSchemas), extract from values
              objectKeywords = ["properties", "patternProperties", "dependentSchemas"]
              objectRefs = concatMap (\k -> case KeyMap.lookup (Key.fromText k) objMap of
                Just (Aeson.Object nestedObj) -> concatMap (collectRefsFromValue base parentSchema version) (KeyMap.elems nestedObj)
                _ -> []) objectKeywords
              -- Also check $defs and definitions for references within definitions
              defsRefs = concatMap (\k -> case KeyMap.lookup (Key.fromText k) objMap of
                Just (Aeson.Object defsObj) -> concatMap (collectRefsFromValue base parentSchema version) (KeyMap.elems defsObj)
                _ -> []) ["$defs", "definitions"]
          in refRefs <> dynamicRefRefs <> arrayRefs <> singleRefs <> objectRefs <> defsRefs
        Aeson.Array arr ->
          -- Array of schemas - collect references from each
          concatMap (collectRefsFromValue base parentSchema version) (toList arr)
        _ -> []
      
    -- Helper to resolve a reference from text
    resolveRefFromText :: Maybe Text -> Text -> [Text]
    resolveRefFromText base refText
      | T.null refText = []
      | T.isPrefixOf "#" refText = []  -- Fragment only, not external
      | otherwise =
          let resolved = resolveAgainstBaseURI base refText
              (docUri, _) = splitUriFragment resolved
              target = if T.null docUri then resolved else docUri
          in if T.null target then [] else [target]

-- | Custom validator function type
type CustomValidator = Value -> Either ValidationError ()

-- | Reference loader function type
-- Given a URI, returns the schema at that URI (if available)
-- This allows pluggable loading strategies (HTTP, file system, cache, etc.)
type ReferenceLoader = Text -> IO (Either Text Schema)

-- | Format keyword behavior
--
-- Controls how the @format@ keyword is interpreted during validation.
--
-- __JSON Schema Specification Compliance__:
--
-- * __'FormatAssertion'__: Format validation failures cause schema validation to fail.
--   This is enabled via the @format-assertion@ vocabulary in JSON Schema 2019-09+.
--   Spec-compliant for implementations that support the format-assertion vocabulary.
--
-- * __'FormatAnnotation'__: Format validation failures are collected as annotations only.
--   This is the __default behavior__ in JSON Schema 2019-09 and 2020-12.
--   Spec-compliant as the default format behavior.
--
-- __Historical Context__:
--
-- * JSON Schema Draft-07 and earlier: The spec was ambiguous, and most implementations
--   treated format as assertions.
-- * JSON Schema 2019-09+: Format is explicitly annotation-only by default, with an
--   optional @format-assertion@ vocabulary to enable assertion behavior.
--
-- __Usage__:
--
-- This setting can be configured via:
--
-- 1. Dialect configuration ('dialectDefaultFormat' in 'Dialect')
-- 2. Global validation config ('validationFormatAssertion' in 'ValidationConfig')
-- 3. Per-schema via @$vocabulary@ declarations (when using dialect-aware parsing)
--
-- @since 0.1.0.0
data FormatBehavior
  = FormatAssertion
    -- ^ Format validation failures __cause schema validation to fail__.
    --
    -- Use this for strict validation where format violations are errors.
    -- This corresponds to enabling the @format-assertion@ vocabulary.
  | FormatAnnotation
    -- ^ Format validation failures are __collected as annotations only__.
    --
    -- Use this for permissive validation where format is informational.
    -- This is the default behavior in JSON Schema 2019-09 and 2020-12.
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

-- | Unknown keyword handling strategy
--
-- Controls how unrecognized keywords (not in registered vocabularies) are handled
-- during schema parsing.
--
-- __JSON Schema Specification Compliance__:
--
-- * __'IgnoreUnknown'__: Spec-compliant. Unknown keywords are silently ignored.
--
-- * __'CollectUnknown'__: Spec-compliant. Unknown keywords are collected in
--   'schemaExtensions' for introspection but don't affect validation.
--   This is the default behavior.
--
-- * __'WarnUnknown'__: Extension (non-standard). Emits warnings for unknown keywords
--   but continues parsing. Useful for development and debugging.
--   Does not violate spec compliance.
--
-- * __'ErrorOnUnknown'__: Extension (non-standard). Parsing fails when encountering
--   unknown keywords. Common in strict implementations for catching typos and
--   configuration errors. __Users should be aware this is stricter than the spec requires__.
--
-- __Important__: This setting is separate from @$vocabulary@ validation:
--
-- * Required vocabularies (via @$vocabulary@) __always__ cause failure if not understood
-- * Optional vocabularies are ignored per spec
-- * Unknown keywords are keywords not in __any__ registered vocabulary
--
-- __Usage__:
--
-- Configure via:
--
-- 1. Dialect configuration ('dialectUnknownKeywords' in 'Dialect')
-- 2. Parser-specific settings (when available)
--
-- __Recommendations__:
--
-- * __Production__: Use 'CollectUnknown' (default, spec-compliant)
-- * __Development__: Use 'WarnUnknown' to catch potential issues
-- * __Strict validation__: Use 'ErrorOnUnknown' to enforce clean schemas
-- * __Permissive parsing__: Use 'IgnoreUnknown' to skip unknown keywords entirely
--
-- @since 0.1.0.0
data UnknownKeywordMode
  = IgnoreUnknown
    -- ^ Silently ignore unknown keywords (not collected in extensions).
    --
    -- __Spec-compliant__: Unknown keywords have no effect on validation.
  | WarnUnknown
    -- ^ Emit warnings for unknown keywords but continue parsing.
    --
    -- __Extension (non-standard)__: Helpful for development.
    -- Warnings may be logged or collected separately.
  | ErrorOnUnknown
    -- ^ Fail parsing when unknown keywords are encountered.
    --
    -- __Extension (non-standard)__: Stricter than spec requires.
    -- Use to catch typos and enforce vocabulary usage.
  | CollectUnknown
    -- ^ Collect unknown keywords in 'schemaExtensions' map.
    --
    -- __Spec-compliant__ (default): Unknown keywords available for introspection.
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

-- | Configuration for validation behavior
--
-- This configuration controls various aspects of JSON Schema validation,
-- including format behavior, content validation, and custom extensions.
data ValidationConfig = ValidationConfig
  { validationVersion :: JsonSchemaVersion
    -- ^ Schema version to validate against
  
  , validationFormatAssertion :: Bool
    -- ^ Treat format as assertion (True) or annotation (False).
    --
    -- __Deprecated__: Use 'validationDialectFormatBehavior' for more precise control.
    -- This field is kept for backward compatibility.
    --
    -- When 'validationDialectFormatBehavior' is 'Nothing', this boolean is used.

  , validationDialectFormatBehavior :: Maybe FormatBehavior
    -- ^ Dialect-specific format behavior (overrides 'validationFormatAssertion').
    --
    -- * 'Just' 'FormatAssertion': Format validation failures cause schema failures
    -- * 'Just' 'FormatAnnotation': Format validation failures are annotations only
    -- * 'Nothing': Use 'validationFormatAssertion' for backward compatibility
    --
    -- This field is typically set when parsing with 'parseSchemaWithDialectRegistry'
    -- to respect the dialect's format behavior configuration.
    --
    -- @since 0.1.0.0

  , validationContentAssertion :: Bool
    -- ^ Treat contentEncoding/contentMediaType as assertion (True) or annotation (False)

  , validationShortCircuit :: Bool
    -- ^ Stop on first error (True) or collect all (False)
  
  , validationCollectAnnotations :: Bool
    -- ^ Collect annotations during validation
  
  , validationCustomValidators :: Map Text CustomValidator
    -- ^ Custom validators for format or other extensions
  
  , validationReferenceLoader :: Maybe ReferenceLoader
    -- ^ Optional loader for external references (Nothing = no external refs)
  }
  deriving (Generic)

-- | Stateful context threaded through validation
data ValidationContext = ValidationContext
  { contextConfig :: ValidationConfig
    -- ^ Validation configuration

  , contextSchemaRegistry :: SchemaRegistry
    -- ^ Registry of schemas for reference resolution

  , contextRootSchema :: Maybe Schema
    -- ^ Root schema for resolving local $ref (e.g., #/definitions/foo)

  , contextBaseURI :: Maybe Text
    -- ^ Current base URI for relative reference resolution

  , contextDynamicScope :: [Schema]
    -- ^ Stack for $dynamicRef resolution (2020-12+)

  , contextEvaluatedProperties :: Set Text
    -- ^ Properties evaluated so far (for unevaluatedProperties)

  , contextEvaluatedItems :: Set Int
    -- ^ Array indices evaluated so far (for unevaluatedItems)

  , contextVisitedSchemas :: Set SchemaFingerprint
    -- ^ Schemas visited (for cycle detection)

  , contextResolvingRefs :: Set Text
    -- ^ References currently being resolved (for detecting reference cycles)
    -- NOTE: This set is currently maintained but not used for cycle detection.
    -- We use contextRecursionDepth instead (see below).

  , contextRecursionDepth :: Int
    -- ^ Current recursion depth (for preventing infinite recursion in recursive schemas)
    --
    -- DESIGN NOTE - Recursive Reference Handling:
    -- We use a depth-based approach (limit: 100) rather than set-based cycle detection.
    -- This allows valid recursive schemas like: tree → node → tree → node → ...
    --
    -- Research into other JSON Schema validators showed different approaches:
    -- - Ajv (JavaScript): Compile-time resolution, pre-compiles refs into validation code
    -- - Python referencing: Dynamic scope traversal with structural termination conditions
    -- - gojsonschema (Go): Schema pool duplicate detection during parsing (prevention-oriented)
    -- - everit-org (Java): Reference caching/memoization with ReferenceKnot
    -- - Academic: StopList tracking (Context, Instance, Schema) triples
    --
    -- Our depth-based approach is simpler and more permissive than alternatives,
    -- allowing recursive data structures to validate naturally while preventing
    -- infinite recursion through a reasonable depth limit. This is appropriate
    -- for an interpreted validator and aligns with common recursive algorithm practices.
    --
    -- The limit of 100 is configurable via future enhancement to ValidationConfig.
  }
  deriving (Generic)

-- Placeholder instances to satisfy compilation
-- These will be implemented properly in later phases

instance ToJSON SchemaValidation where
  toJSON _ = object []  -- TODO: Implement in parser phase

instance FromJSON SchemaValidation where
  parseJSON _ = pure $ SchemaValidation  -- TODO: Implement in parser phase
    { validationMultipleOf = Nothing
    , validationMaximum = Nothing
    , validationExclusiveMaximum = Nothing
    , validationMinimum = Nothing
    , validationExclusiveMinimum = Nothing
    , validationMaxLength = Nothing
    , validationMinLength = Nothing
    , validationPattern = Nothing
    , validationFormat = Nothing
    , validationContentEncoding = Nothing
    , validationContentMediaType = Nothing
    , validationItems = Nothing
    , validationPrefixItems = Nothing
    , validationContains = Nothing
    , validationMaxItems = Nothing
    , validationMinItems = Nothing
    , validationUniqueItems = Nothing
    , validationMaxContains = Nothing
    , validationMinContains = Nothing
    , validationUnevaluatedItems = Nothing
    , validationProperties = Nothing
    , validationPatternProperties = Nothing
    , validationAdditionalProperties = Nothing
    , validationUnevaluatedProperties = Nothing
    , validationPropertyNames = Nothing
    , validationMaxProperties = Nothing
    , validationMinProperties = Nothing
    , validationRequired = Nothing
    , validationDependentRequired = Nothing
    , validationDependentSchemas = Nothing
    , validationDependencies = Nothing
    }

instance ToJSON SchemaAnnotations where
  toJSON _ = object []  -- TODO: Implement in parser phase

instance FromJSON SchemaAnnotations where
  parseJSON _ = pure $ SchemaAnnotations  -- TODO: Implement in parser phase
    { annotationTitle = Nothing
    , annotationDescription = Nothing
    , annotationDefault = Nothing
    , annotationExamples = []
    , annotationDeprecated = Nothing
    , annotationReadOnly = Nothing
    , annotationWriteOnly = Nothing
    , annotationComment = Nothing
    , annotationCodegen = Nothing
    }

instance ToJSON SchemaObject where
  toJSON _ = object []  -- TODO: Implement in renderer phase

instance FromJSON SchemaObject where
  parseJSON (Aeson.Bool b) = pure $ SchemaObject (Left b)  -- Boolean schema
  parseJSON _ = pure $ SchemaObject $ Right $ ObjectSchemaData  -- TODO: Implement in parser phase
    { objectSchemaDataType = Nothing
    , objectSchemaDataEnum = Nothing
    , objectSchemaDataConst = Nothing
    , objectSchemaDataRef = Nothing
    , objectSchemaDataDynamicRef = Nothing
    , objectSchemaDataAnchor = Nothing
    , objectSchemaDataDynamicAnchor = Nothing
    , objectSchemaDataRecursiveRef = Nothing
    , objectSchemaDataRecursiveAnchor = Nothing
    , objectSchemaDataAllOf = Nothing
    , objectSchemaDataAnyOf = Nothing
    , objectSchemaDataOneOf = Nothing
    , objectSchemaDataNot = Nothing
    , objectSchemaDataIf = Nothing
    , objectSchemaDataThen = Nothing
    , objectSchemaDataElse = Nothing
    , objectSchemaDataValidation = SchemaValidation
        { validationMultipleOf = Nothing
        , validationMaximum = Nothing
        , validationExclusiveMaximum = Nothing
        , validationMinimum = Nothing
        , validationExclusiveMinimum = Nothing
        , validationMaxLength = Nothing
        , validationMinLength = Nothing
        , validationPattern = Nothing
        , validationFormat = Nothing
        , validationContentEncoding = Nothing
        , validationContentMediaType = Nothing
        , validationItems = Nothing
        , validationPrefixItems = Nothing
        , validationContains = Nothing
        , validationMaxItems = Nothing
        , validationMinItems = Nothing
        , validationUniqueItems = Nothing
        , validationMaxContains = Nothing
        , validationMinContains = Nothing
        , validationProperties = Nothing
        , validationPatternProperties = Nothing
        , validationAdditionalProperties = Nothing
        , validationUnevaluatedProperties = Nothing
        , validationUnevaluatedItems = Nothing
        , validationPropertyNames = Nothing
        , validationMaxProperties = Nothing
        , validationMinProperties = Nothing
        , validationRequired = Nothing
        , validationDependentRequired = Nothing
        , validationDependentSchemas = Nothing
        , validationDependencies = Nothing
        }
    , objectSchemaDataAnnotations = SchemaAnnotations
        { annotationTitle = Nothing
        , annotationDescription = Nothing
        , annotationDefault = Nothing
        , annotationExamples = []
        , annotationDeprecated = Nothing
        , annotationReadOnly = Nothing
        , annotationWriteOnly = Nothing
        , annotationComment = Nothing
        , annotationCodegen = Nothing
        }
    , objectSchemaDataDefs = Map.empty
    }

instance ToJSON SchemaCore where
  toJSON (BooleanSchema b) = Aeson.Bool b
  toJSON (ObjectSchema obj) = toJSON obj

instance FromJSON SchemaCore where
  parseJSON (Aeson.Bool b) = pure $ BooleanSchema b
  parseJSON other = ObjectSchema <$> parseJSON other

instance ToJSON Schema where
  toJSON _ = object []  -- TODO: Implement in renderer phase

instance FromJSON Schema where
  parseJSON _ = pure $ Schema  -- TODO: Implement in parser phase
    { schemaVersion = Nothing
    , schemaMetaschemaURI = Nothing
    , schemaId = Nothing
    , schemaCore = BooleanSchema True
    , schemaVocabulary = Nothing
    , schemaExtensions = Map.empty
    , schemaRawKeywords = Map.empty
    }

