{-# LANGUAGE OverloadedStrings #-}

-- | Reference loaders for external schema resolution
--
-- This module provides pluggable loaders for fetching external schemas.
-- Users can provide custom loaders for different protocols or caching strategies.
module JsonSchema.ReferenceLoader
  ( -- * Reference Loaders
    ReferenceLoader
  , noOpLoader
  , httpLoader
  , fileLoader
  , fileLoaderWithVersion
  , cachedLoader
  , compositeLoader
  
    -- * Scheme-based Routing
  , LoaderRegistry
  , emptyLoaderRegistry
  , registerScheme
  , registerDefaultLoader
  , makeLoader
  , mkSimpleLoaderRegistry
  
    -- * Helper Functions
  , resolveRelativeURI
  , isHttpURI
  , isFileURI
  , getUriScheme
  ) where

import JsonSchema.Types
import JsonSchema.Parser (parseSchema, parseSchemaWithVersion, parseSubschema, ParseError(parseErrorMessage))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import Network.HTTP.Client (Manager, httpLbs, parseRequest, responseBody, HttpException)
import Network.URI (parseURI, parseURIReference, uriToString, relativeTo)
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (newIORef, readIORef, modifyIORef')

-- | No-op loader that always fails (for pure validation without external refs)
noOpLoader :: ReferenceLoader
noOpLoader uri = pure $ Left $ "External references not supported: " <> uri

-- | HTTP-based loader using http-client
-- Fetches schemas from http:// and https:// URIs
httpLoader :: Manager -> ReferenceLoader
httpLoader manager uri
  | isHttpURI uri = do
      result <- try $ do
        request <- parseRequest (T.unpack uri)
        response <- httpLbs request manager
        let body = responseBody response
        case Aeson.eitherDecode body of
          Left err -> pure $ Left $ "Failed to parse schema from " <> uri <> ": " <> T.pack err
          Right value -> case parseSchema value of
            Left parseErr -> pure $ Left $ "Failed to parse schema: " <> parseErrorMessage parseErr
            Right schema -> pure $ Right schema
      case result of
        Left (exc :: HttpException) -> pure $ Left $ "HTTP error loading " <> uri <> ": " <> T.pack (show exc)
        Right res -> pure res
  | otherwise = pure $ Left $ "Not an HTTP URI: " <> uri

-- | File system loader (for file:// URIs and relative paths)
fileLoader :: ReferenceLoader
fileLoader uri
  | isFileURI uri = do
      let path = T.unpack $ T.drop 7 uri  -- Remove "file://"
      result <- try $ Aeson.eitherDecodeFileStrict path
      case result of
        Left (exc :: IOError) -> pure $ Left $ "File error loading " <> uri <> ": " <> T.pack (show exc)
        Right (Left err) -> pure $ Left $ "JSON parse error: " <> T.pack err
        Right (Right value) -> case parseSchema value of
          Left parseErr -> pure $ Left $ "Schema parse error: " <> parseErrorMessage parseErr
          Right schema -> pure $ Right schema
  | T.isPrefixOf "/" uri || T.isPrefixOf "./" uri || T.isPrefixOf "../" uri = do
      -- Relative or absolute file path
      result <- try $ Aeson.eitherDecodeFileStrict (T.unpack uri)
      case result of
        Left (exc :: IOError) -> pure $ Left $ "File error: " <> T.pack (show exc)
        Right (Left err) -> pure $ Left $ "JSON parse error: " <> T.pack err
        Right (Right value) -> case parseSchema value of
          Left parseErr -> pure $ Left $ "Schema parse error: " <> parseErrorMessage parseErr
          Right schema -> pure $ Right schema
  | not (T.any (== ':') uri) = do
      -- Plain path without scheme (treat as file path)
      result <- try $ Aeson.eitherDecodeFileStrict (T.unpack uri)
      case result of
        Left (exc :: IOError) -> pure $ Left $ "File error: " <> T.pack (show exc)
        Right (Left err) -> pure $ Left $ "JSON parse error: " <> T.pack err
        Right (Right value) -> case parseSchema value of
          Left parseErr -> pure $ Left $ "Schema parse error: " <> parseErrorMessage parseErr
          Right schema -> pure $ Right schema
  | otherwise = pure $ Left $ "Not a file URI: " <> uri

-- | Version-aware file system loader
-- Like fileLoader but parses schemas with a specific version if they don't have $schema
fileLoaderWithVersion :: JsonSchemaVersion -> Text -> IO (Either Text Schema)
fileLoaderWithVersion version uri
  | isFileURI uri = do
      let path = T.unpack $ T.drop 7 uri  -- Remove "file://"
      result <- try $ Aeson.eitherDecodeFileStrict path
      case result of
        Left (exc :: IOError) -> pure $ Left $ "File error loading " <> uri <> ": " <> T.pack (show exc)
        Right (Left err) -> pure $ Left $ "JSON parse error: " <> T.pack err
        Right (Right value) -> case parseSubschema version value of
          Left parseErr -> pure $ Left $ "Schema parse error: " <> parseErrorMessage parseErr
          Right schema -> pure $ Right schema
  | T.isPrefixOf "/" uri || T.isPrefixOf "./" uri || T.isPrefixOf "../" uri = do
      -- Relative or absolute file path
      result <- try $ Aeson.eitherDecodeFileStrict (T.unpack uri)
      case result of
        Left (exc :: IOError) -> pure $ Left $ "File error: " <> T.pack (show exc)
        Right (Left err) -> pure $ Left $ "JSON parse error: " <> T.pack err
        Right (Right value) -> case parseSubschema version value of
          Left parseErr -> pure $ Left $ "Schema parse error: " <> parseErrorMessage parseErr
          Right schema -> pure $ Right schema
  | not (T.any (== ':') uri) = do
      -- Plain path without scheme (treat as file path)
      result <- try $ Aeson.eitherDecodeFileStrict (T.unpack uri)
      case result of
        Left (exc :: IOError) -> pure $ Left $ "File error: " <> T.pack (show exc)
        Right (Left err) -> pure $ Left $ "JSON parse error: " <> T.pack err
        Right (Right value) -> case parseSubschema version value of
          Left parseErr -> pure $ Left $ "Schema parse error: " <> parseErrorMessage parseErr
          Right schema -> pure $ Right schema
  | otherwise = pure $ Left $ "Not a file URI: " <> uri

-- | Caching loader that wraps another loader
-- Caches successful schema loads to avoid repeated fetches
cachedLoader :: ReferenceLoader -> IO ReferenceLoader
cachedLoader baseLoader = do
  cacheRef <- newIORef Map.empty
  pure $ \uri -> do
    cache <- readIORef cacheRef
    case Map.lookup uri cache of
      Just schema -> pure $ Right schema
      Nothing -> do
        result <- baseLoader uri
        case result of
          Right schema -> do
            modifyIORef' cacheRef (Map.insert uri schema)
            pure $ Right schema
          Left err -> pure $ Left err

-- | Composite loader that tries multiple loaders in order
compositeLoader :: [ReferenceLoader] -> ReferenceLoader
compositeLoader loaders uri = tryLoaders loaders
  where
    tryLoaders [] = pure $ Left $ "No loader could resolve: " <> uri
    tryLoaders (loader:rest) = do
      result <- loader uri
      case result of
        Right schema -> pure $ Right schema
        Left _ -> tryLoaders rest

-- * Scheme-based Routing

-- | Registry for URI scheme-based loader routing
data LoaderRegistry = LoaderRegistry
  { registrySchemeLoaders :: Map.Map Text ReferenceLoader
    -- ^ Loaders registered for specific URI schemes (http, https, file, etc.)
  , registryDefaultLoader :: ReferenceLoader
    -- ^ Fallback loader when no scheme matches
  }

-- | Create an empty registry with a no-op default loader
emptyLoaderRegistry :: LoaderRegistry
emptyLoaderRegistry = LoaderRegistry
  { registrySchemeLoaders = Map.empty
  , registryDefaultLoader = noOpLoader
  }

-- | Register a loader for a specific URI scheme
-- Example: @registerScheme "http" (httpLoader manager) registry@
registerScheme :: Text -> ReferenceLoader -> LoaderRegistry -> LoaderRegistry
registerScheme scheme loader registry =
  registry { registrySchemeLoaders = Map.insert scheme loader (registrySchemeLoaders registry) }

-- | Set the default loader (used when no scheme matches)
registerDefaultLoader :: ReferenceLoader -> LoaderRegistry -> LoaderRegistry
registerDefaultLoader loader registry =
  registry { registryDefaultLoader = loader }

-- | Convert a LoaderRegistry into a ReferenceLoader
makeLoader :: LoaderRegistry -> ReferenceLoader
makeLoader registry uri =
  case getUriScheme uri of
    Just scheme ->
      case Map.lookup scheme (registrySchemeLoaders registry) of
        Just loader -> loader uri
        Nothing -> registryDefaultLoader registry uri
    Nothing ->
      -- No scheme, try default loader
      registryDefaultLoader registry uri

-- | Helper to create a simple registry with common schemes
-- Example: @mkSimpleLoaderRegistry (httpLoader manager) fileLoader@
mkSimpleLoaderRegistry :: ReferenceLoader -> ReferenceLoader -> LoaderRegistry
mkSimpleLoaderRegistry httpLdr fileLdr =
  emptyLoaderRegistry
    { registrySchemeLoaders = Map.fromList
        [ ("http", httpLdr)
        , ("https", httpLdr)
        , ("file", fileLdr)
        ]
    , registryDefaultLoader = fileLdr  -- Assume bare paths are files
    }

-- | Resolve a relative URI against a base URI
resolveRelativeURI :: Text -> Text -> Either Text Text
resolveRelativeURI base relative =
  case (parseURI (T.unpack base), parseURIReference (T.unpack relative)) of
    (Just baseURI, Just relURI) ->
      let resolved = relURI `relativeTo` baseURI
      in Right $ T.pack $ uriToString id resolved ""
    _ -> Left $ "Invalid URI: base=" <> base <> ", relative=" <> relative

-- | Check if a URI is HTTP/HTTPS
isHttpURI :: Text -> Bool
isHttpURI uri = T.isPrefixOf "http://" uri || T.isPrefixOf "https://" uri

-- | Check if a URI is a file:// URI
isFileURI :: Text -> Bool
isFileURI uri = T.isPrefixOf "file://" uri

-- | Extract the scheme from a URI (e.g., "http" from "http://example.com")
getUriScheme :: Text -> Maybe Text
getUriScheme uri =
  case T.breakOn "://" uri of
    (scheme, rest)
      | not (T.null rest) && T.all isSchemeChar scheme -> Just scheme
      | otherwise -> Nothing
  where
    isSchemeChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || 
                     (c >= '0' && c <= '9') || c == '+' || c == '-' || c == '.'

