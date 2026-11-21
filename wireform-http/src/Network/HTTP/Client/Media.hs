{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Media types and content negotiation primitives.

The vocabulary here is small on purpose: a 'MediaType' is a parsed
@type\/subtype@ pair with parameters. 'HasMediaType' attaches a media
type to a phantom tag. 'Encode' and 'Decode' are open type classes
keyed on the tag: users define their own tags for protobuf, XML,
MessagePack, etc.

This module is library-style — no I\/O. The shipped tags (JSON,
plain text, form-urlencoded, octet-stream) live in
"Network.HTTP.Client.Media.JSON" and friends; that keeps aeson and
http-api-data out of the import graph until you actually want them.
-}
module Network.HTTP.Client.Media (
  -- * Media types
  MediaType (..),
  mediaTypeBytes,
  parseMediaType,
  parseMediaRange,
  renderMediaType,
  matches,
  matchesAny,
  stripParameters,
  Quality (..),
  maxQuality,
  minQuality,

  -- * Open type-class machinery
  HasMediaType (..),
  Encode (..),
  Decode (..),
  DecodeError (..),

  -- * Header helpers
  contentTypeOf,
  acceptHeaderValue,
) where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (toLower)
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Short qualified as ST
import Network.HTTP.ContentNegotiation qualified as Hermes
import Network.HTTP.Grammar.Rendering (renderSeparated)
import Network.HTTP.Headers.Mason qualified as M
import Network.HTTP.Headers.Parsing.Util (ParserT, Result (..), eof, isTokenChar, ows, runParser)
import Network.HTTP.Internal.Validation (equalShortBytes)
import Network.HTTP.Types.Header qualified as H
import Wireform.Parser.Driver (parseByteString)
import Wireform.Parser.Error (ParseError (..))


{- | A parsed @type\/subtype@ media type, plus any parameters that
came along on the @Content-Type@ header (e.g. @charset=utf-8@).

Comparison is case-insensitive on the type, subtype, and parameter
names; parameter /values/ are compared exactly.
-}
data MediaType = MediaType
  { mtType :: !ByteString
  , mtSubType :: !ByteString
  , mtParameters :: ![(ByteString, ByteString)]
  }
  deriving stock (Eq, Show)


{- | @\"type\/subtype\"@ literals build a 'MediaType' via the 'IsString'
instance. Parameters are parsed if present.
-}
instance IsString MediaType where
  fromString s = case parseMediaRange (BS8.pack s) of
    Right mt -> mt
    Left _ -> MediaType (BS8.pack (map toLower s)) "" []


mediaTypeBytes :: MediaType -> ByteString
mediaTypeBytes = renderMediaType


-- | Parse a concrete @Content-Type@ value, including any parameters.
-- Already lowercase type and subtype names may share the input buffer.
parseMediaType :: ByteString -> Either String MediaType
parseMediaType = parseMediaValue "Content-Type" (Hermes.mediaTypeWithRawNames fromWireMediaType)


-- | Parse one media range, including a possible wildcard and parameters.
parseMediaRange :: ByteString -> Either String MediaType
parseMediaRange = parseMediaValue "media range" (fromHermesRange <$> Hermes.mediaRangeParser)


parseMediaValue :: String -> ParserT st String MediaType -> ByteString -> Either String MediaType
parseMediaValue name parser bs0 =
  case parseByteString (ows *> parser <* ows <* eof) bs0 of
    Right media -> Right media
    Left (ParseErr _ e) -> Left e
    Left _ -> case runParser (ows *> parser) bs0 of
      OK _ leftover -> Left ("trailing input after " <> name <> ": " <> show leftover)
      Fail -> Left ("malformed " <> name <> ": " <> show bs0)
      Err e -> Left e
{-# INLINE parseMediaValue #-}


{- | Convert a hermes 'MediaRange' to our 'MediaType'. Hermes uses
'ShortText' for the type \/ subtype \/ parameter strings and
represents the wildcard as @""@; we collapse them back to
'ByteString' and normalise the wildcard back to @"*"@ to match
the rest of the Wire client.
-}
fromHermesRange :: Hermes.MediaRange -> MediaType
fromHermesRange r =
  let mt = Hermes.mediaType r
  in fromWireMediaType (wireName (Hermes.mediaBaseType mt)) (wireName (Hermes.mediaSubtype mt)) (Hermes.mediaParams r)
  where
    wireName name
      | ST.null name = "*"
      | otherwise = ST.toByteString name


fromWireMediaType :: ByteString -> ByteString -> [(ST.ShortText, ST.ShortText)] -> MediaType
fromWireMediaType base subtype parameters =
  MediaType (lowerToken base) (lowerToken subtype) (map lowerParameter parameters)
  where
    lowerParameter (k, v) = (lowerToken (ST.toByteString k), opaqueBytes v)

    -- The parser has already validated these names as ASCII tokens.
    lowerToken bytes
      | BS.any isUpperAscii bytes = BS.map lowerAscii bytes
      | otherwise = bytes
    isUpperAscii w = w >= 65 && w <= 90
    lowerAscii w
      | isUpperAscii w = w + 32
      | otherwise = w
    opaqueBytes value
      | T.isAscii (ST.toText value) = ST.toByteString value
      | otherwise = M.toStrictByteString (T.foldr (\c rest -> M.char8 c <> rest) mempty (ST.toText value))


renderMediaType :: MediaType -> ByteString
renderMediaType (MediaType base subtype []) = BS.concat [base, "/", subtype]
renderMediaType m = M.toStrictByteString (mediaTypeBuilder m)


mediaTypeBuilder :: MediaType -> M.Builder
mediaTypeBuilder m =
  M.byteString (mtType m) <> M.char7 '/' <> M.byteString (mtSubType m) <> foldMap renderParam (mtParameters m)
  where
    renderParam (key, value)
      | BS.null key = mempty
      | otherwise =
          M.byteString "; " <> M.byteString key <> M.char7 '=' <> renderValue value
    renderValue value
      | not (BS.null value) && BS8.all isTokenChar value = M.byteString value
      | BS.all plainQuotedByte value = M.char7 '"' <> M.byteString value <> M.char7 '"'
      | otherwise = Hermes.renderMediaParameterValue (ST.fromText (TE.decodeLatin1 value))
    plainQuotedByte byte = (byte == 9 || byte >= 32) && byte /= 34 && byte /= 92 && byte /= 127


-- | Drop any media-type parameters, leaving the bare @type\/subtype@.
stripParameters :: MediaType -> MediaType
stripParameters m = m {mtParameters = []}


{- | Match a media type against a target. Wildcards are honoured
according to RFC 9110: @*\/*@ matches anything, @text\/*@ matches
any @text\/...@, and @text\/plain@ matches exactly @text\/plain@.
Parameters on the candidate are ignored for matching.
-}
matches :: MediaType -> MediaType -> Bool
matches candidate target =
  let candTy = mtType candidate
      candSub = mtSubType candidate
      tgtTy = mtType target
      tgtSub = mtSubType target
      tyOk = equalShortBytes candTy tgtTy || tgtTy == "*"
      subOk = equalShortBytes candSub tgtSub || tgtSub == "*"
  in tyOk && subOk
{-# INLINE matches #-}


matchesAny :: MediaType -> [MediaType] -> Bool
matchesAny c = any (matches c)


-- | RFC 9110 @q=...@ quality value, clamped to @[0,1]@.
newtype Quality = Quality {unQuality :: Double}
  deriving stock (Eq, Ord, Show)


maxQuality :: Quality
maxQuality = Quality 1.0


minQuality :: Quality
minQuality = Quality 0.0


-- ---------------------------------------------------------------------------
-- Open typeclass machinery
-- ---------------------------------------------------------------------------

{- | Associate a content-type tag with its media type. Tags are
typically uninhabited phantom types; the class is a way to project
the type-level tag onto a runtime media type.

> mediaType @JSON  ==  "application/json"
-}
class HasMediaType tag where
  mediaType :: MediaType


class HasMediaType tag => Encode tag a where
  encode :: a -> ByteString


class HasMediaType tag => Decode tag a where
  decode :: ByteString -> Either DecodeError a


{- | A decode failure carrying the media type, the offending body
bytes (truncated for diagnostics by the caller if needed), and a
human-readable reason.
-}
data DecodeError = DecodeError
  { decodeMediaType :: !MediaType
  , decodeMessage :: !String
  }
  deriving stock (Show)


instance Exception DecodeError


-- ---------------------------------------------------------------------------
-- Header helpers
-- ---------------------------------------------------------------------------

{- | Extract a 'MediaType' from a header list. Returns
@application\/octet-stream@ if the field is absent, malformed, or repeated.
-}
contentTypeOf :: H.Headers -> MediaType
contentTypeOf hdrs = case H.lookupSingletonHeader H.hContentType hdrs of
  Right (Just v) -> either (const defaultMediaType) id (parseMediaType v)
  _ -> defaultMediaType
  where
    defaultMediaType = MediaType "application" "octet-stream" []


{- | Render media ranges and their qualities as an @Accept@ field value.
Qualities of at least one are implicit. Other values round to the nearest
thousandth, with negative values and NaN rendered as @q=0@.
-}
acceptHeaderValue :: [(MediaType, Quality)] -> ByteString
acceptHeaderValue [(m, Quality q)] | q >= 1 = renderMediaType m
acceptHeaderValue offers = M.toStrictByteString (renderSeparated (M.byteString ", ") renderOffer offers)
  where
    renderOffer (m, Quality q)
      | q >= 1 = mediaTypeBuilder m
      | otherwise = mediaTypeBuilder m <> M.byteString "; q=" <> Hermes.renderQualityValue q
