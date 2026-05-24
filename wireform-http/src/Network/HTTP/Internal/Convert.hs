{- | Conversions between the unified wireform HTTP types and the
version-specific @wireform-http1@ \/ @wireform-http2@ types.

These conversions live in an Internal module because callers should
prefer the unified shapes; we expose them for advanced use where the
caller needs to drop into an underlying API (e.g. to use HTTP\/1's
sendfile path).
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Internal.Convert
  ( -- * HTTP\/1.x conversions
    toHttp1Request
  , fromHttp1Request
  , toHttp1Response
  , fromHttp1Response
  , toHttp1Headers
  , fromHttp1Headers
  , toHttp1Method
  , fromHttp1Method
  , toHttp1Status
  , fromHttp1Status
  , toHttp1Version
  , fromHttp1Version
  , toHttp1Body
  , fromHttp1Body
    -- * HTTP\/2 conversions
  , toHttp2Request
  , fromHttp2Request
  , toHttp2Response
  , fromHttp2Response
  , toHttp2Headers
  , fromHttp2Headers
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive.Unsafe (unsafeMk)

import Network.HTTP1.Internal.Ascii (asciiLower)

import qualified Network.HTTP1.Types as H1
import qualified Network.HTTP2.Server as H2S

import Network.HTTP.Message
import qualified Network.HTTP.Types.Body as U
import qualified Network.HTTP.Types.Header as U
import qualified Network.HTTP.Types.Method as U
import qualified Network.HTTP.Types.Status as U
import qualified Network.HTTP.Types.Version as U

------------------------------------------------------------------------
-- HTTP/1.x
------------------------------------------------------------------------

toHttp1Method :: U.Method -> H1.Method
toHttp1Method = H1.methodFromBytes . U.fromMethod

fromHttp1Method :: H1.Method -> U.Method
fromHttp1Method = U.methodFromBytes . H1.methodToBytes

toHttp1Status :: U.Status -> H1.Status
toHttp1Status (U.Status w) = H1.Status w

fromHttp1Status :: H1.Status -> U.Status
fromHttp1Status (H1.Status w) = U.Status w

toHttp1Version :: U.Version -> H1.Version
toHttp1Version v
  | v == U.HTTP1_0 = H1.HTTP_1_0
  | otherwise      = H1.HTTP_1_1

fromHttp1Version :: H1.Version -> U.Version
fromHttp1Version H1.HTTP_1_0 = U.HTTP1_0
fromHttp1Version H1.HTTP_1_1 = U.HTTP1_1

toHttp1Headers :: U.Headers -> H1.Headers
toHttp1Headers = map $ \(n, v) -> (CI.original n, v)

-- | Wrap HTTP/1 header names in case-insensitive wrappers.
-- Uses the CI constructor directly with a fast ASCII lowercase
-- to avoid the redundant allocation that CI.mk's foldCase would do
-- on an already-mostly-lowercase name.
fromHttp1Headers :: H1.Headers -> U.Headers
fromHttp1Headers = map $ \(n, v) -> (mkCIAscii n, v)

toHttp1Body :: U.Body -> H1.Body
toHttp1Body = \case
  U.BodyEmpty       -> H1.BodyEmpty
  U.BodyBytes bs    -> H1.BodyBytes bs
  U.BodyStream p    -> H1.BodyStream p

-- | HTTP\/1-only body variants ('BodyFile', 'BodyPreEncoded') are
-- collapsed back to their bytewise representation.  Use the http1
-- types directly if you want to preserve them.
fromHttp1Body :: H1.Body -> U.Body
fromHttp1Body = \case
  H1.BodyEmpty           -> U.BodyEmpty
  H1.BodyBytes bs        -> U.BodyBytes bs
  H1.BodyStream p        -> U.BodyStream p
  H1.BodyPreEncoded pe   -> U.BodyBytes (H1.peBytes pe)
  H1.BodyFile _          -> U.BodyStream (pure Nothing)
    -- Callers shouldn't see this on the wire from a server response,
    -- but we collapse to an empty stream just in case.

toHttp1Request :: Request -> H1.Request
toHttp1Request r = H1.Request
  { H1.requestMethod   = toHttp1Method (requestMethod r)
  , H1.requestTarget   = requestTarget r
  , H1.requestVersion  = toHttp1Version (requestVersion r)
  , H1.requestHeaders  = withHost (requestAuthority r) (toHttp1Headers (requestHeaders r))
  , H1.requestBody     = toHttp1Body (requestBody r)
  , H1.requestTrailers = toHttp1Headers <$> requestTrailers r
  }
  where
    withHost Nothing hs    = hs
    withHost (Just a) hs
      | any (\(n, _) -> BS.map toLower8 n == "host") hs = hs
      | otherwise = ("Host", a) : hs
    toLower8 c
      | c >= 0x41 && c <= 0x5A = c + 0x20
      | otherwise              = c

fromHttp1Request :: Scheme -> H1.Request -> Request
fromHttp1Request scheme r = Request
  { requestMethod    = fromHttp1Method (H1.requestMethod r)
  , requestTarget    = H1.requestTarget r
  , requestAuthority = U.lookupHeader U.hHost (fromHttp1Headers (H1.requestHeaders r))
  , requestScheme    = scheme
  , requestHeaders   = fromHttp1Headers (H1.requestHeaders r)
  , requestBody      = fromHttp1Body (H1.requestBody r)
  , requestVersion   = fromHttp1Version (H1.requestVersion r)
  , requestTrailers  = fromHttp1Headers <$> H1.requestTrailers r
  }

toHttp1Response :: Response -> H1.Response
toHttp1Response r = H1.Response
  { H1.responseStatus    = toHttp1Status (responseStatus r)
  , H1.responseVersion   = toHttp1Version (responseVersion r)
  , H1.responseHeaders   = toHttp1Headers (responseHeaders r)
  , H1.responseBody      = toHttp1Body (responseBody r)
  , H1.responseTrailers  = toHttp1Headers <$> responseTrailers r
  }

-- | HTTP\/1.x trailers come from the chunked body's terminator field
-- block.  'Network.HTTP1.Connection' already reads them and parks
-- them on an 'MVar'; we surface that as 'responseTrailers'.
--
-- Note: this is the /client-receive/ side. The 'Network.HTTP1.Types'
-- 'Response' record does not yet carry trailers (the H1 encoder
-- doesn't emit them), so server-emitted H1 trailers still need
-- wiring through the 'wireform-http1' encoder.
fromHttp1Response :: H1.Response -> Response
fromHttp1Response r = Response
  { responseStatus  = fromHttp1Status (H1.responseStatus r)
  , responseVersion = fromHttp1Version (H1.responseVersion r)
  , responseHeaders = fromHttp1Headers (H1.responseHeaders r)
  , responseBody    = fromHttp1Body (H1.responseBody r)
  , responseTrailers = fromHttp1Headers <$> H1.responseTrailers r
  , responseH2StreamId = 0
  , responseCancel = pure ()
  }

------------------------------------------------------------------------
-- HTTP/2
------------------------------------------------------------------------

-- HPACK requires lowercase header names; we take the folded case so we
-- never emit an uppercase byte on the wire.
toHttp2Headers :: U.Headers -> [(ByteString, ByteString)]
toHttp2Headers = map $ \(n, v) -> (CI.foldedCase n, v)

-- | HTTP/2 header names are already lowercase (HPACK mandates it).
-- unsafeMk avoids the redundant foldCase allocation.
fromHttp2Headers :: [(ByteString, ByteString)] -> U.Headers
fromHttp2Headers = map $ \(n, v) -> (unsafeMk n, v)

toHttp2Request :: Request -> H2S.Request
toHttp2Request r = H2S.Request
  { H2S.requestMethod    = U.fromMethod (requestMethod r)
  , H2S.requestPath      = requestTarget r
  , H2S.requestScheme    = case requestScheme r of
      SchemeHttp  -> "http"
      SchemeHttps -> "https"
  , H2S.requestAuthority = maybe "" id (requestAuthority r)
  , H2S.requestHeaders   = toHttp2Headers (requestHeaders r)
  , H2S.requestBody      = pure ""
  , H2S.requestStreamId  = 0
  , H2S.requestTrailers  = pure []
  }

-- | HTTP\/2 request → unified.  The request body is delivered as a
-- pull producer in the underlying API; we wrap it back into 'BodyStream'.
fromHttp2Request :: H2S.Request -> Request
fromHttp2Request r = Request
  { requestMethod    = U.methodFromBytes (H2S.requestMethod r)
  , requestTarget    = H2S.requestPath r
  , requestAuthority = case H2S.requestAuthority r of
      "" -> Nothing
      a  -> Just a
  , requestScheme    = case H2S.requestScheme r of
      "https" -> SchemeHttps
      _       -> SchemeHttp
  , requestHeaders   = fromHttp2Headers (H2S.requestHeaders r)
  , requestBody      = U.BodyStream $ do
      bs <- H2S.requestBody r
      if BS.null bs then pure Nothing else pure (Just bs)
  , requestVersion   = U.HTTP2
  , requestTrailers  = fromHttp2Headers <$> H2S.requestTrailers r
  }

-- | Materialise the unified 'Response' into the @http2@ shape.  Runs
-- the trailer-producing IO action because @H2S.Response.responseTrailers@
-- is a strict list.  Server handlers that don't emit trailers can
-- still build them via @pure []@ with no extra cost.
toHttp2Response :: Response -> IO H2S.Response
toHttp2Response r = do
  trs <- responseTrailers r
  pure H2S.Response
    { H2S.responseStatus   = fromIntegral (U.statusCode (responseStatus r))
    , H2S.responseHeaders  = toHttp2Headers (responseHeaders r)
    , H2S.responseBody     = case responseBody r of
        U.BodyEmpty     -> H2S.ResponseBodyEmpty
        U.BodyBytes bs  -> H2S.ResponseBodyBS bs
        U.BodyStream p  -> H2S.ResponseBodyStream p
    , H2S.responseTrailers = toHttp2Headers trs
    }

-- | Build a CI ByteString from an ASCII HTTP/1 header name.
-- Uses the SSE2-accelerated asciiLower (16 bytes/cycle) for the
-- fold, then unsafeMk to avoid the redundant BS.map toLower that
-- CI.mk would run. The result loses original wire casing — both
-- the original and foldedCase fields hold the lowered form. This
-- is correct for header comparison and matches HTTP/2 semantics;
-- HTTP/1 re-encoding via toHttp1Headers uses CI.original which
-- will emit the lowered name.
mkCIAscii :: ByteString -> CI.CI ByteString
mkCIAscii n = unsafeMk (asciiLower n)
{-# INLINE mkCIAscii #-}

fromHttp2Response :: H2S.Response -> Response
fromHttp2Response r = Response
  { responseStatus  = U.Status (fromIntegral (H2S.responseStatus r))
  , responseVersion = U.HTTP2
  , responseHeaders = fromHttp2Headers (H2S.responseHeaders r)
  , responseBody    = case H2S.responseBody r of
      H2S.ResponseBodyEmpty    -> U.BodyEmpty
      H2S.ResponseBodyBS bs    -> U.BodyBytes bs
      H2S.ResponseBodyStream p -> U.BodyStream p
  , responseTrailers = pure (fromHttp2Headers (H2S.responseTrailers r))
  , responseH2StreamId = 0
  , responseCancel = pure ()
  }

