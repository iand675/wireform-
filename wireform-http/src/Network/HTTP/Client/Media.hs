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
import Data.List qualified as List
import Data.String (IsString (..))
import Data.Text.Encoding qualified as TE
import Data.Text.Short qualified as ST
import Network.HTTP.ContentNegotiation qualified as Hermes
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Network.HTTP.Types.Header qualified as H


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
  fromString s = case parseMediaType (BS8.pack s) of
    Right mt -> mt
    Left _ -> MediaType (BS8.pack (map toLower s)) "" []


mediaTypeBytes :: MediaType -> ByteString
mediaTypeBytes = renderMediaType


{- | Parse a @Content-Type@ value using hermes's
'Hermes.mediaRangeParser', then project into the wireform
'MediaType' shape. Leading and trailing HTTP whitespace is tolerated.
-}
parseMediaType :: ByteString -> Either String MediaType
parseMediaType bs0 =
  let bs = BS.dropWhile isOWS bs0
  in case runParser Hermes.mediaRangeParser bs of
       OK r leftover
         | BS.all isOWS leftover ->
             Right (fromHermesRange r)
         | otherwise ->
             Left ("trailing input after Content-Type: " <> show leftover)
       Fail -> Left ("malformed Content-Type: " <> show bs0)
       Err e -> Left e
  where
    isOWS b = b == 0x20 || b == 0x09


{- | Convert a hermes 'MediaRange' to our 'MediaType'. Hermes uses
'ShortText' for the type \/ subtype \/ parameter strings and
represents the wildcard as @""@; we collapse them back to
'ByteString' and normalise the wildcard back to @"*"@ to match
the rest of the Wire client.
-}
fromHermesRange :: Hermes.MediaRange -> MediaType
fromHermesRange r =
  let mt = Hermes.mediaType r
      stBytes = TE.encodeUtf8 . ST.toText
      normalise b
        | BS.null b = "*"
        | otherwise = BS8.map toLower b
  in MediaType
       { mtType = normalise (stBytes (Hermes.mediaBaseType mt))
       , mtSubType = normalise (stBytes (Hermes.mediaSubtype mt))
       , mtParameters =
           [ (BS8.map toLower (stBytes k), stBytes v)
           | (k, v) <- Hermes.mediaParams r
           ]
       }


renderMediaType :: MediaType -> ByteString
renderMediaType m =
  let core = mtType m <> "/" <> mtSubType m
      params =
        mconcat
          ["; " <> k <> "=" <> v | (k, v) <- mtParameters m, not (BS.null k)]
  in core <> params


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
      tyOk = candTy == tgtTy || tgtTy == "*"
      subOk = candSub == tgtSub || tgtSub == "*"
  in tyOk && subOk


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
@application\/octet-stream@ if the header is absent (RFC 9110
§ 8.3 default).
-}
contentTypeOf :: H.Headers -> MediaType
contentTypeOf hdrs = case H.lookupHeader H.hContentType hdrs of
  Nothing -> MediaType "application" "octet-stream" []
  Just v -> case parseMediaType v of
    Right mt -> mt
    Left _ -> MediaType "application" "octet-stream" []


{- | Render a list of @(media-type, quality)@ pairs as an @Accept@
header value. Quality 1.0 is omitted; other qualities are rendered
per RFC 9110 \u00a712.4.2 \u2014 @0\u201a1\u201a or @0.@ followed by 1-3
DIGIT, no exponent, trailing zeros dropped.
-}
acceptHeaderValue :: [(MediaType, Quality)] -> ByteString
acceptHeaderValue = BS.intercalate ", " . List.map one
  where
    one (m, Quality q)
      | q >= 1.0 = renderMediaType m
      | otherwise = renderMediaType m <> "; q=" <> renderQuality q


-- | Render a quality value (clamped to @[0,1]@) per RFC 9110 \u00a712.4.2.
renderQuality :: Double -> ByteString
renderQuality d
  | d >= 1.0 = "1"
  | d <= 0.0 = "0"
  | otherwise =
      let milli = round (d * 1000) :: Int -- 0..999
          padded = pad3 (BS8.pack (show milli))
          trimmed = BS.dropWhileEnd (== 0x30) padded
      in if BS.null trimmed
           then "0"
           else "0." <> trimmed
  where
    pad3 bs =
      let n = BS.length bs
      in if n >= 3 then bs else BS.replicate (3 - n) 0x30 <> bs
