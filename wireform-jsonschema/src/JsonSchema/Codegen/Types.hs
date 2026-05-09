{-# LANGUAGE GADTs #-}

-- | Types for multi-target code generation system
--
-- This module defines the types that enable custom keywords to provide
-- metadata that guides code generation for different targets (Aeson,
-- Servant, Yesod, Proto3, etc.).
module JsonSchema.Codegen.Types
  ( -- * Code Generation Targets
    CodegenTarget(..)
    -- * Code Generation Hints
  , CodegenHint(..)
  , StrategyHint(..)
  , FieldNamingConvention(..)
    -- * Codegen Handlers
  , CodegenHandler(..)
  , CodegenHandlerFunc
    -- * Validator References
  , ValidatorReference(..)
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Language.Haskell.TH (Type)

import JsonSchema.Types (Schema)

-- | Code generation target
--
-- Defines the different targets for which code can be generated.
-- Each target has independent handler registries.
data CodegenTarget
  = AesonTarget
    -- ^ Generate ToJSON/FromJSON instances
  | ServantTarget
    -- ^ Generate Servant API types
  | YesodTarget
    -- ^ Generate Yesod routes and handlers
  | Proto3Target
    -- ^ Generate Protocol Buffers definitions
  | CustomTarget Text
    -- ^ User-defined custom target
  deriving (Show, Eq, Ord)

-- | Strategy hint for type generation
data StrategyHint
  = NewtypeStrategy
    -- ^ Generate newtype wrapper
  | RecordStrategy
    -- ^ Generate record type
  | SumTypeStrategy
    -- ^ Generate sum type (enum/tagged union)
  deriving (Show, Eq)

-- | Field naming convention
data FieldNamingConvention
  = CamelCase
    -- ^ fieldName
  | SnakeCase
    -- ^ field_name
  | PascalCase
    -- ^ FieldName
  | KebabCase
    -- ^ field-name
  | CustomNaming (Text -> Text)
    -- ^ Custom transformation function

instance Show FieldNamingConvention where
  show CamelCase = "CamelCase"
  show SnakeCase = "SnakeCase"
  show PascalCase = "PascalCase"
  show KebabCase = "KebabCase"
  show (CustomNaming _) = "CustomNaming{<function>}"

-- | Reference to a validator function for embedding in generated code
--
-- Allows generated code to reference validation functions without
-- tight coupling to the validation machinery.
data ValidatorReference = ValidatorReference
  { validatorName :: Text
    -- ^ Name of the validator function
  , validatorModule :: Maybe Text
    -- ^ Module where validator is defined (if qualified import needed)
  , validatorType :: Maybe Type
    -- ^ TH Type representation (for type checking)
  }
  deriving (Show)

-- | Hints that guide code generation
--
-- Custom keywords can provide these hints to influence how code is generated.
-- Multiple hints can be composed, with conflict resolution via priority.
data CodegenHint
  = StrategyHint StrategyHint
    -- ^ Hint about which type generation strategy to use
  | FieldNamingHint FieldNamingConvention
    -- ^ Hint about field naming convention
  | ValidatorHint ValidatorReference
    -- ^ Reference to custom validator to embed
  | DeriveHint [Text]
    -- ^ Additional deriving clauses (e.g., ["Eq", "Ord", "Show"])
  | TargetSpecificHint CodegenTarget Value
    -- ^ Opaque hint for custom targets (target-specific interpretation)
  deriving (Show)

-- | Handler function for code generation
--
-- Given a keyword value and schema, produces codegen hints for a specific target.
-- Handlers are registered per (keyword, target) pair.
type CodegenHandlerFunc = Value -> Schema -> [CodegenHint]

-- | Registered codegen handler
data CodegenHandler = CodegenHandler
  { handlerKeyword :: Text
    -- ^ Keyword this handler applies to
  , handlerTarget :: CodegenTarget
    -- ^ Target this handler generates code for
  , handlerFunc :: CodegenHandlerFunc
    -- ^ The handler function
  }

instance Show CodegenHandler where
  show h = "CodegenHandler{keyword=" ++ show (handlerKeyword h) ++
           ", target=" ++ show (handlerTarget h) ++ "}"
