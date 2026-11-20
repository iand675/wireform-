{-# LANGUAGE OverloadedStrings #-}

{- | HTTP header fields.

A condensed take on hermes' @Network.HTTP.Headers@: we keep the
'CI ByteString' name type (case-insensitive matching with byte-level
storage) but represent the field set as a flat association list
rather than hermes' 'HashMap'. The list shape is what
'Network.HTTP1.Types.Headers' and the HPACK header lists in
'Network.HTTP2' already use, so the conversion between the unified
API and either underlying implementation is a no-op.

The @h*@ constants in this module cover the IANA permanent header
field names; the list isn't exhaustive (hermes has every IANA
registration), just the ones the wireform HTTP stack needs to
inspect at the negotiation layer. Add more as they come up.
-}
module Network.HTTP.Types.Header (
  HeaderName,
  HeaderValue,
  Header,
  Headers,

  -- * Lookups
  lookupHeader,
  lookupSingletonHeader,
  lookupHeaders,
  hasHeader,
  insertHeader,
  addHeader,
  deleteHeader,

  -- * Validation (RFC 9110 \u00a75.1 \/ \u00a75.5)

  --
  -- These re-exports cover the validating constructors and the
  -- hop-by-hop \/ HTTP\/2 forbidden-header helpers. The fast-path
  -- 'insertHeader' \/ 'addHeader' do not validate; use
  -- 'mkHeaderName' \/ 'mkHeaderValue' (or
  -- 'insertHeaderChecked' \/ 'addHeaderChecked') to validate at
  -- the API boundary.
  HeaderError (..),
  mkHeaderName,
  mkHeaderValue,
  isValidHeaderName,
  isValidHeaderValue,
  insertHeaderChecked,
  addHeaderChecked,

  -- * Hop-by-hop and HTTP\/2 forbidden headers
  hopByHopHeaders,
  isHopByHop,
  stripHopByHop,
  http2ForbiddenHeaders,
  isHttp2Forbidden,
  validateHttp2Headers,

  -- * Common names
  hHost,
  hContentLength,
  hContentType,
  hContentEncoding,
  hContentLanguage,
  hContentLocation,
  hContentRange,
  hTransferEncoding,
  hConnection,
  hUpgrade,
  hHTTP2Settings,
  hServer,
  hUserAgent,
  hAccept,
  hAcceptEncoding,
  hAcceptLanguage,
  hAcceptCharset,
  hAcceptRanges,
  hRange,
  hExpect,
  hLocation,
  hAuthorization,
  hWWWAuthenticate,
  hProxyAuthenticate,
  hProxyAuthorization,
  hCookie,
  hSetCookie,
  hTE,
  hTrailer,
  hAllow,
  hDate,
  hLastModified,
  hExpires,
  hETag,
  hIfMatch,
  hIfNoneMatch,
  hIfModifiedSince,
  hIfUnmodifiedSince,
  hIfRange,
  hVary,
  hCacheControl,
  hAge,
  hRetryAfter,
  hLastEventID,
  hVia,
  hForwarded,
) where

import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI, mk)
import Network.HTTP.Internal.Validation (
  HeaderError (..),
  hopByHopHeaders,
  http2ForbiddenHeaders,
  isHopByHop,
  isHttp2Forbidden,
  isValidHeaderName,
  isValidHeaderValue,
  mkHeaderName,
  mkHeaderValue,
  stripHopByHop,
  validateHttp2Headers,
 )


-- | Header field name. Comparison and hashing are case-insensitive.
type HeaderName = CI ByteString


-- | Header field value, on-the-wire bytes.
type HeaderValue = ByteString


type Header = (HeaderName, HeaderValue)


type Headers = [Header]


{- | First-match lookup. Returns the field value if present, 'Nothing'
otherwise.
-}
lookupHeader :: HeaderName -> Headers -> Maybe HeaderValue
lookupHeader name = go
  where
    go [] = Nothing
    go ((k, v) : rest)
      | k == name = Just v
      | otherwise = go rest


-- | Look up a single field line. A second line is an error, even if its
-- value is identical to the first.
lookupSingletonHeader :: HeaderName -> Headers -> Either String (Maybe HeaderValue)
lookupSingletonHeader name = go
  where
    go [] = Right Nothing
    go ((k, v) : rest)
      | k /= name = go rest
      | hasHeader name rest = Left "Repeated header field lines"
      | otherwise = Right (Just v)
{-# INLINE lookupSingletonHeader #-}


{- | All values for a given header name, in original order. Useful for
multi-valued fields such as @Set-Cookie@ or @Via@.
-}
lookupHeaders :: HeaderName -> Headers -> [HeaderValue]
lookupHeaders name = foldr step []
  where
    step (k, v) acc
      | k == name = v : acc
      | otherwise = acc


hasHeader :: HeaderName -> Headers -> Bool
hasHeader name = any ((== name) . fst)


{- | Replace every existing entry of @name@ with the new value. If
@name@ wasn't present, append it.
-}
insertHeader :: HeaderName -> HeaderValue -> Headers -> Headers
insertHeader name value = go False
  where
    go seen [] = if seen then [] else [(name, value)]
    go seen ((k, v) : rest)
      | k == name =
          if seen
            then go True rest
            else (k, value) : go True rest
      | otherwise = (k, v) : go seen rest


{- | Append a new value without disturbing any existing entry of the
same name. Use this for multi-valued fields.
-}
addHeader :: HeaderName -> HeaderValue -> Headers -> Headers
addHeader name value hs = hs <> [(name, value)]


deleteHeader :: HeaderName -> Headers -> Headers
deleteHeader name = filter ((/= name) . fst)


{- | Like 'insertHeader' but validates the bytes against RFC 9110 grammar
before mutating the list. Returns @Left@ unchanged headers if the
name or value is invalid.
-}
insertHeaderChecked
  :: ByteString
  -> ByteString
  -> Headers
  -> Either HeaderError Headers
insertHeaderChecked rawName rawValue hdrs = do
  n <- mkHeaderName rawName
  v <- mkHeaderValue rawValue
  pure (insertHeader n v hdrs)


addHeaderChecked
  :: ByteString
  -> ByteString
  -> Headers
  -> Either HeaderError Headers
addHeaderChecked rawName rawValue hdrs = do
  n <- mkHeaderName rawName
  v <- mkHeaderValue rawValue
  pure (addHeader n v hdrs)


hHost
  , hContentLength
  , hContentType
  , hContentEncoding
  , hContentLanguage
  , hContentLocation
  , hContentRange
  , hTransferEncoding
  , hConnection
  , hUpgrade
  , hHTTP2Settings
  , hServer
  , hUserAgent
  , hAccept
  , hAcceptEncoding
  , hAcceptLanguage
  , hAcceptCharset
  , hAcceptRanges
  , hRange
  , hExpect
  , hLocation
  , hAuthorization
  , hWWWAuthenticate
  , hProxyAuthenticate
  , hProxyAuthorization
  , hCookie
  , hSetCookie
  , hTE
  , hTrailer
  , hAllow
  , hDate
  , hLastModified
  , hExpires
  , hETag
  , hIfMatch
  , hIfNoneMatch
  , hIfModifiedSince
  , hIfUnmodifiedSince
  , hIfRange
  , hVary
  , hCacheControl
  , hVia
  , hForwarded
  , hAge
  , hRetryAfter
  , hLastEventID
    :: HeaderName
hHost = mk "Host"
hContentLength = mk "Content-Length"
hContentType = mk "Content-Type"
hContentEncoding = mk "Content-Encoding"
hContentLanguage = mk "Content-Language"
hContentLocation = mk "Content-Location"
hContentRange = mk "Content-Range"
hTransferEncoding = mk "Transfer-Encoding"
hConnection = mk "Connection"
hUpgrade = mk "Upgrade"
hHTTP2Settings = mk "HTTP2-Settings"
hServer = mk "Server"
hUserAgent = mk "User-Agent"
hAccept = mk "Accept"
hAcceptEncoding = mk "Accept-Encoding"
hAcceptLanguage = mk "Accept-Language"
hAcceptCharset = mk "Accept-Charset"
hAcceptRanges = mk "Accept-Ranges"
hRange = mk "Range"
hExpect = mk "Expect"
hLocation = mk "Location"
hAuthorization = mk "Authorization"
hWWWAuthenticate = mk "WWW-Authenticate"
hProxyAuthenticate = mk "Proxy-Authenticate"
hProxyAuthorization = mk "Proxy-Authorization"
hCookie = mk "Cookie"
hSetCookie = mk "Set-Cookie"
hTE = mk "TE"
hTrailer = mk "Trailer"
hAllow = mk "Allow"
hDate = mk "Date"
hLastModified = mk "Last-Modified"
hExpires = mk "Expires"
hETag = mk "ETag"
hIfMatch = mk "If-Match"
hIfNoneMatch = mk "If-None-Match"
hIfModifiedSince = mk "If-Modified-Since"
hIfUnmodifiedSince = mk "If-Unmodified-Since"
hIfRange = mk "If-Range"
hVary = mk "Vary"
hCacheControl = mk "Cache-Control"
hAge = mk "Age"
hRetryAfter = mk "Retry-After"
hLastEventID = mk "Last-Event-ID"
hVia = mk "Via"
hForwarded = mk "Forwarded"
