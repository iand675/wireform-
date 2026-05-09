{-# LANGUAGE TemplateHaskell #-}
-- | Embedded JSON Schema metaschemas
--
-- This module provides access to the standard JSON Schema metaschemas
-- embedded at compile time using file-embed. This ensures the library
-- is self-contained and doesn't require network access for standard schemas.
module JsonSchema.EmbeddedMetaschemas
  ( standardMetaschemaLoader
  , embeddedMetaschemas
  , lookupEmbeddedMetaschema
  , embeddedMetaschemaURIs  -- Debug export
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed (embedDir)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Aeson (Value, eitherDecode)
import System.FilePath ((</>), takeFileName, dropExtension)

import JsonSchema.Types (Schema, ReferenceLoader)
import JsonSchema.Parser (parseSchema)
import JsonSchema.EmbeddedMetaschemas.Raw
  ( embeddedMetaschemaValues
  , lookupRawMetaschema
  )

-- | Parse and cache all embedded metaschemas
-- Maps from JSON Schema URI to parsed Schema
embeddedMetaschemas :: Map Text Schema
embeddedMetaschemas = Map.mapMaybe parseMetaschema embeddedMetaschemaValues
  where
    parseMetaschema :: Value -> Maybe Schema
    parseMetaschema val = case parseSchema val of
      Left _err -> Nothing  -- Skip unparseable schemas
      Right schema -> Just schema

-- | Look up an embedded metaschema by URI
lookupEmbeddedMetaschema :: Text -> Maybe Schema
lookupEmbeddedMetaschema uri = Map.lookup normalizedURI embeddedMetaschemas
  where
    -- Normalize URI by removing fragment
    normalizedURI = T.takeWhile (/= '#') uri

-- | List all embedded metaschema URIs (for debugging)
embeddedMetaschemaURIs :: [Text]
embeddedMetaschemaURIs = Map.keys embeddedMetaschemas

-- | Standard metaschema loader that uses embedded schemas
-- This loader can be composed with other loaders using Alternative (<|>)
--
-- Example:
--   let loader = standardMetaschemaLoader <|> httpLoader <|> fileLoader
--
-- The loader will first try embedded schemas, then fall back to other loaders.
standardMetaschemaLoader :: ReferenceLoader
standardMetaschemaLoader uri =
  case lookupEmbeddedMetaschema uri of
    Just schema -> pure $ Right schema
    Nothing -> pure $ Left $ "Metaschema not found in embedded resources: " <> uri
