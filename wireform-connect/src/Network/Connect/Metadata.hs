{-# LANGUAGE OverloadedStrings #-}

-- | Connect metadata: bridge grpc-spec 'Grpc.CustomMetadata' to\/from
-- wireform-http 'Headers', honoring Connect's rules (ASCII values verbatim;
-- @-bin@ keys carry base64; unary trailing-metadata is @trailer-@-prefixed
-- into the HTTP header block; streaming trailing-metadata rides the
-- EndStreamResponse JSON).
module Network.Connect.Metadata
  ( -- * Leading metadata (HTTP headers)
    leadingToHeaders,
    headersToLeading,

    -- * Unary trailing metadata (trailer- prefix)
    trailingToPrefixedHeaders,
    prefixedHeadersToTrailing,

    -- * Streaming trailing metadata (EndStreamResponse JSON)
    metadataToJSON,
    metadataFromJSON,

    -- * Classification
    isReservedHeader,
  ) where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AKey
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.Connect.Protocol (trailerPrefix)
import Network.GRPC.Spec qualified as Grpc
  ( CustomMetadata (..)
  , HeaderName (..)
  , customMetadataName
  , customMetadataValue
  , safeHeaderName
  )
import Network.GRPC.Spec.Serialization
  ( buildBinaryValue
  , parseBinaryValue
  )
import Network.HTTP.Types.Header (Header, HeaderName, Headers)

------------------------------------------------------------------------
-- Leading metadata
------------------------------------------------------------------------

-- | Render leading metadata (custom headers) as HTTP request headers.
leadingToHeaders :: [Grpc.CustomMetadata] -> Headers
leadingToHeaders = map cmToHeader

-- | Parse HTTP request headers back into leading metadata, skipping
-- reserved header names.
headersToLeading :: Headers -> [Grpc.CustomMetadata]
headersToLeading = mapMaybe headerToCmUnprefixed

------------------------------------------------------------------------
-- Unary trailing metadata (trailer- prefix in the same header block)
------------------------------------------------------------------------

-- | Render unary trailing metadata as @trailer-@-prefixed headers — the
-- unary shape, where trailers ride in the same header block.
trailingToPrefixedHeaders :: [Grpc.CustomMetadata] -> Headers
trailingToPrefixedHeaders = map cmToTrailerHeader

-- | Parse @trailer-@-prefixed headers back into trailing metadata.
prefixedHeadersToTrailing :: Headers -> [Grpc.CustomMetadata]
prefixedHeadersToTrailing = mapMaybe headerToCmTrailer

------------------------------------------------------------------------
-- Streaming trailing metadata (EndStreamResponse.metadata JSON)
------------------------------------------------------------------------

-- | Render trailing metadata as the Connect EndStreamResponse @metadata@
-- object: keys are header names (lower-case), values are arrays of the
-- wire-encoded strings (ASCII verbatim, @-bin@ values base64).
metadataToJSON :: [Grpc.CustomMetadata] -> Aeson.Value
metadataToJSON ms = Aeson.object (map toPair (Map.toList grouped))
  where
    grouped = Map.fromListWith (flip (<>)) (map kv ms)
    kv cm =
      ( decodeUtf8 (CI.foldedCase (ciName (Grpc.customMetadataName cm)))
      , [decodeUtf8 (encodeValue (Grpc.customMetadataName cm) (Grpc.customMetadataValue cm))]
      )
    toPair (n, vs) = AKey.fromText n .= vs

-- | Parse a Connect EndStreamResponse @metadata@ object back into
-- 'Grpc.CustomMetadata'. A @-bin@ suffix on the key marks a binary header
-- whose value is base64.
metadataFromJSON :: Aeson.Value -> Either String [Grpc.CustomMetadata]
metadataFromJSON (Aeson.Object o) =
  concat <$> traverse parseEntry (KeyMap.toList o)
  where
    parseEntry (k, v) = do
      let nmBs = encodeUtf8 (T.toLower (AKey.toText k))
      let isBin = "-bin" `BS.isSuffixOf` nmBs
      vals <- case v of
        Aeson.Array xs -> Right [t | Aeson.String t <- toList xs]
        Aeson.String t -> Right [t]
        _ -> Left "metadata value must be an array or string"
      traverse (parseVal nmBs isBin) vals
    parseVal nmBs isBin t =
      let valBs = encodeUtf8 t
          nm = if isBin then Grpc.BinaryHeader nmBs else Grpc.AsciiHeader nmBs
       in if isBin
            then case parseBinaryValue valBs :: Either String ByteString of
              Right decoded -> Right (Grpc.CustomMetadata nm decoded)
              Left e -> Left ("binary metadata value decode: " <> e)
            else Right (Grpc.CustomMetadata nm valBs)
metadataFromJSON _ = Left "metadata must be a JSON object"

------------------------------------------------------------------------
-- Header <-> CustomMetadata (single)
------------------------------------------------------------------------

cmToHeader :: Grpc.CustomMetadata -> Header
cmToHeader cm =
  ( ciName (Grpc.customMetadataName cm)
  , encodeValue (Grpc.customMetadataName cm) (Grpc.customMetadataValue cm)
  )

cmToTrailerHeader :: Grpc.CustomMetadata -> Header
cmToTrailerHeader cm =
  let (nm, val) = cmToHeader cm
   in (CI.map (trailerPrefix <>) nm, val)

-- The header name as a case-insensitive ByteString. grpc-spec's HeaderName
-- stores the raw lowercase name in its AsciiHeader/BinaryHeader payload
-- (binary names already carry the @-bin@ suffix).
ciName :: Grpc.HeaderName -> CI ByteString
ciName (Grpc.AsciiHeader nm) = CI.mk nm
ciName (Grpc.BinaryHeader nm) = CI.mk nm

-- | Encode a value per the header kind: ASCII verbatim, binary as unpadded
-- base64.
encodeValue :: Grpc.HeaderName -> ByteString -> ByteString
encodeValue (Grpc.AsciiHeader _) val = val
encodeValue (Grpc.BinaryHeader _) val = buildBinaryValue val

-- | Parse an unprefixed header into CustomMetadata (skipping reserved names).
headerToCmUnprefixed :: Header -> Maybe Grpc.CustomMetadata
headerToCmUnprefixed (nm, val)
  | isReservedHeaderCI nm = Nothing
  | otherwise = headerToCm (CI.foldedCase nm) val

-- | Parse a @trailer-@-prefixed header into CustomMetadata.
headerToCmTrailer :: Header -> Maybe Grpc.CustomMetadata
headerToCmTrailer (nm, val) =
  let raw = CI.foldedCase nm
   in if trailerPrefix `BS.isPrefixOf` raw
        then headerToCm (BS.drop (BS.length trailerPrefix) raw) val
        else Nothing

-- | Parse a raw (already-unprefixed, lower-cased) name + value into
-- CustomMetadata. Returns 'Nothing' if the name is invalid.
headerToCm :: ByteString -> ByteString -> Maybe Grpc.CustomMetadata
headerToCm rawName val =
  case Grpc.safeHeaderName rawName of
    Nothing -> Nothing
    Just nm -> case nm of
      Grpc.AsciiHeader _ -> Just (Grpc.CustomMetadata nm val)
      Grpc.BinaryHeader _ ->
        case parseBinaryValue val :: Either String ByteString of
          Right decoded -> Just (Grpc.CustomMetadata nm decoded)
          Left _ -> Nothing

------------------------------------------------------------------------
-- Reserved header classification
------------------------------------------------------------------------

-- | Is this header name reserved by Connect (and thus not custom metadata)?
isReservedHeader :: HeaderName -> Bool
isReservedHeader = isReservedHeaderCI

isReservedHeaderCI :: CI ByteString -> Bool
isReservedHeaderCI nm =
  let raw = CI.foldedCase nm
   in "connect-" `BS.isPrefixOf` raw
        || raw `elem` reservedExact
  where
    reservedExact :: [ByteString]
    reservedExact =
      [ "content-type"
      , "content-encoding"
      , "content-length"
      , "accept-encoding"
      , "trailer"
      , "transfer-encoding"
      , "connection"
      , "host"
      ]
