{- | Wire-level HTTP header plumbing types.

These are the flat, untyped @(name, value)@ tuple aliases used by the
framing layers (@wireform-http2@'s HPACK header lists, HTTP\/1
header blocks). They are deliberately structurally identical to the
shapes the rest of the ecosystem uses (e.g. @http-types@'s @Header@ /
@RequestHeaders@ / @ResponseHeaders@), so values flow between layers
without conversion.

For the rich, typed header-field machinery see "Network.HTTP.Headers".
-}
module Network.HTTP.Header (
  HeaderName,
  Header,
  RequestHeaders,
  ResponseHeaders,
) where

import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)


-- | A case-insensitive header field name (byte-level storage).
type HeaderName = CI ByteString


-- | A single header field: a case-insensitive name and its raw value.
type Header = (HeaderName, ByteString)


-- | Request header fields.
type RequestHeaders = [Header]


-- | Response header fields.
type ResponseHeaders = [Header]
