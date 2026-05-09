-- | Schema metadata extraction and introspection
module JsonSchema.Metadata
  ( extractMetadata
  , SchemaMetadata(..)
  ) where

import JsonSchema.Types
  ( Schema(..), SchemaCore(..), SchemaObject(..)
  , schemaAnnotations, SchemaAnnotations(..)
  )
import Data.Text (Text)

-- | Extracted metadata from a schema
data SchemaMetadata = SchemaMetadata
  { metadataTitle :: Maybe Text
  , metadataDescription :: Maybe Text
  }  
  deriving (Eq, Show)

-- | Extract metadata from a schema
extractMetadata :: Schema -> SchemaMetadata
extractMetadata schema = case schemaCore schema of
  BooleanSchema _ -> SchemaMetadata Nothing Nothing
  ObjectSchema obj -> SchemaMetadata
    { metadataTitle = annotationTitle (schemaAnnotations obj)
    , metadataDescription = annotationDescription (schemaAnnotations obj)
    }

