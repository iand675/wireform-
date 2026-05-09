-- | Wireform JSON Schema (JsonSchema)
--
-- A comprehensive, type-safe library for JSON Schema validation and code generation.
--
-- = Quick Start
--
-- > import JsonSchema
-- > import Data.Aeson
-- >
-- > -- Create a simple schema
-- > let schema = Schema
-- >       { schemaCore = ObjectSchema ...
-- >       , ...
-- >       }
-- >
-- > -- Validate some JSON
-- > case validateValue defaultValidationConfig schema someValue of
-- >   ValidationSuccess _ -> putStrLn "Valid!"
-- >   ValidationFailure errs -> print errs
--
-- = Core Modules
--
-- * "JsonSchema.Types" - Core type definitions
-- * "JsonSchema.Parser" - Parse schemas from JSON/YAML
-- * "JsonSchema.Validator" - Validate values against schemas
-- * "JsonSchema.Renderer" - Render schemas to JSON/YAML
-- * "JsonSchema.Vocabulary" - Extensible vocabulary system
module JsonSchema
  ( -- * Core Types
    Schema(..)
  , SchemaCore(..)
  , SchemaObject(..)
  , JsonSchemaVersion(..)
  , SchemaType(..)
  
    -- * Validation
  , validateValue
  , ValidationResult(..)
  , ValidationError(..)
  , ValidationConfig(..)
  , defaultValidationConfig
  , strictValidationConfig
  
    -- * Parsing
  , parseSchema
  , parseSchemaWithVersion
  , ParseError(..)
  
    -- * Rendering
  , renderSchema
  
    -- * Re-exports from Types
  , module JsonSchema.Types
  ) where

import JsonSchema.Types
import JsonSchema.Parser (parseSchema, parseSchemaWithVersion, ParseError(..))
import JsonSchema.Validator (validateValue, ValidationConfig(..), defaultValidationConfig, strictValidationConfig)
import JsonSchema.Renderer (renderSchema)

