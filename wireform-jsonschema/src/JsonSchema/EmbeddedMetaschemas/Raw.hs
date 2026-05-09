{-# LANGUAGE TemplateHaskell #-}
-- | Raw embedded JSON Schema metaschemas (no parsing)
--
-- This module provides access to the raw JSON values of embedded metaschemas
-- without parsing them. This breaks the circular dependency between Parser
-- and EmbeddedMetaschemas.
module JsonSchema.EmbeddedMetaschemas.Raw
  ( embeddedMetaschemaValues
  , metaschemaURIForPath
  , lookupRawMetaschema
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed (embedDir)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Aeson (eitherDecode, Value)
import System.FilePath (takeFileName, dropExtension)

-- | All embedded metaschemas as raw ByteStrings
-- Maps from file path to content
embeddedFiles :: [(FilePath, ByteString)]
embeddedFiles = $(embedDir "data/metaschemas")

-- | Convert file path to JSON Schema URI
-- Path format: 2020-12/meta-core.json or 2019-09/schema.json
metaschemaURIForPath :: FilePath -> Text
metaschemaURIForPath path =
  let parts = splitPath path
      fileName = takeFileName path
      baseSchemaName = dropExtension fileName
  in case parts of
       -- Main schema files (2019-09 and later use https)
       [version, file] | version == "2020-12" && file == "schema.json" ->
         "https://json-schema.org/draft/2020-12/schema"
       [version, file] | version == "2019-09" && file == "schema.json" ->
         "https://json-schema.org/draft/2019-09/schema"
       -- Older drafts use http (not https) and different path format
       [version, file] | version == "draft-07" && file == "schema.json" ->
         "http://json-schema.org/draft-07/schema"
       [version, file] | version == "draft-06" && file == "schema.json" ->
         "http://json-schema.org/draft-06/schema"
       [version, file] | version == "draft-04" && file == "schema.json" ->
         "http://json-schema.org/draft-04/schema"
       -- Vocabulary metaschemas with "meta-" prefix (2019-09+)
       [version, _] | version == "2020-12" && "meta-" `isPrefix` baseSchemaName ->
         "https://json-schema.org/draft/2020-12/meta/" <> T.pack (drop 5 baseSchemaName)
       [version, _] | version == "2019-09" && "meta-" `isPrefix` baseSchemaName ->
         "https://json-schema.org/draft/2019-09/meta/" <> T.pack (drop 5 baseSchemaName)
       _ -> T.pack path
  where
    -- Split path by /
    splitPath :: FilePath -> [FilePath]
    splitPath = filter (not . null) . splitBy '/'

    -- Split by character
    splitBy :: Char -> String -> [String]
    splitBy _ [] = []
    splitBy c s =
      let (chunk, rest) = break (== c) s
      in chunk : case rest of
           [] -> []
           (_:xs) -> splitBy c xs

    -- Check if string starts with prefix
    isPrefix :: String -> String -> Bool
    isPrefix [] _ = True
    isPrefix _ [] = False
    isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

-- | Parse and cache all embedded metaschemas as raw JSON Values
-- Maps from JSON Schema URI to raw JSON Value
embeddedMetaschemaValues :: Map Text Value
embeddedMetaschemaValues = Map.fromList $ concatMap parseMetaschema embeddedFiles
  where
    parseMetaschema :: (FilePath, ByteString) -> [(Text, Value)]
    parseMetaschema (path, content) =
      case eitherDecode (BL.fromStrict content) of
        Left _err -> []  -- Skip invalid JSON
        Right val ->
          let uri = metaschemaURIForPath path
          in [(uri, val)]

-- | Look up a raw embedded metaschema by URI (returns raw JSON Value)
lookupRawMetaschema :: Text -> Maybe Value
lookupRawMetaschema uri = Map.lookup normalizedURI embeddedMetaschemaValues
  where
    -- Normalize URI by removing fragment
    normalizedURI = T.takeWhile (/= '#') uri

