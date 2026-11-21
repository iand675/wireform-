{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Conversions between the unified wireform HTTP types and the
version-specific @wireform-http1@ \/ @wireform-http2@ types.

These conversions live in an Internal module because callers should
prefer the unified shapes; we expose them for advanced use where the
caller needs to drop into an underlying API (e.g. to use HTTP\/1's
sendfile path).
-}
module Network.HTTP.Internal.Convert (
  prepareRequestBody,
  prepareRequestTrailers,
  bufferedRequestBody,
  streamedRequestBody,

  -- * HTTP\/1.x conversions
  toHttp1Request,
  fromHttp1Request,
  toHttp1Response,
  fromHttp1Response,
  toHttp1Headers,
  fromHttp1Headers,
  toHttp1Method,
  fromHttp1Method,
  toHttp1Status,
  fromHttp1Status,
  toHttp1Version,
  fromHttp1Version,
  toHttp1Body,
  fromHttp1Body,

  -- * HTTP\/2 conversions
  toHttp2Request,
  fromHttp2Request,
  toHttp2Response,
  fromHttp2Response,
  toHttp2Headers,
  fromHttp2Headers,
) where

import Control.Exception (throwIO)
import Data.Bifunctor qualified
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.CaseInsensitive qualified as CI
import Data.CaseInsensitive.Unsafe (unsafeMk)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe qualified
import Network.HTTP.Client.BodyStream (BodyStream, bodyStreamBytes, knownSize, pull)
import Network.HTTP.Client.Expect (hasExpectContinueHeader)
import Network.HTTP.Client.Protocol (ProtocolHints, protocolRequestTrailers)
import Network.HTTP.Client.Request qualified as WReq
import Network.HTTP.Headers qualified as Typed
import Network.HTTP.Headers.ETag (EntityTag (StrongETagBytes))
import Network.HTTP.Headers.IfRange qualified as IfRange
import Network.HTTP.Headers.Range qualified as Range
import Network.HTTP.Message
import Network.HTTP.Types.Body qualified as U
import Network.HTTP.Types.Header qualified as U
import Network.HTTP.Types.Method qualified as U
import Network.HTTP.Types.Status qualified as U
import Network.HTTP.Types.Version qualified as U
import Network.HTTP1.Types qualified as H1
import Network.HTTP2.Server qualified as H2S
import Wireform.Builder qualified as B


-- | Keep the popper deferred for Expect while preserving its framing.
prepareRequestBody :: (BodyStream -> IO U.Body) -> WReq.Request BodyStream -> IO (WReq.Request U.Body)
prepareRequestBody ordinary req
  | Just _ <- protocolRequestTrailers (WReq.protocolHints req) =
      pure req {WReq.headers = sizedFields, WReq.body = streamedRequestBody stream}
  | hasExpectContinueHeader fields =
      pure req
        { WReq.headers = sizedFields
        , WReq.body = if knownSize stream == Just 0 then U.BodyEmpty else streamedRequestBody stream
        }
  | otherwise = do
      lowBody <- ordinary stream
      pure req {WReq.headers = fields, WReq.body = lowBody}
  where
    fields = prepareConditionalRange (WReq.headers req)
    stream = WReq.body req
    sizedFields
      | Just size <- knownSize stream
      , size > 0
      , not (U.hasHeader U.hContentLength fields || U.hasHeader U.hTransferEncoding fields) =
          U.addHeader U.hContentLength (B.toStrictByteString (B.int64Dec size)) fields
      | otherwise = fields


-- | Attach the producer and choose the framing after protocol negotiation.
prepareRequestTrailers :: U.Version -> ProtocolHints -> Request -> IO Request
prepareRequestTrailers version hints req = case protocolRequestTrailers hints of
  Nothing -> pure req
  Just produce -> case version of
    U.HTTP1_1 -> pure req
      { requestHeaders = U.deleteHeader U.hContentLength chunkedFields
      , requestTrailers = produce
      }
    U.HTTP2 -> pure req {requestTrailers = produce}
    _ -> throwIO (WReq.RequestTrailersUnsupportedVersion version)
  where
    fields = requestHeaders req
    chunkedFields
      | U.hasHeader U.hTransferEncoding fields = fields
      | otherwise = U.addHeader U.hTransferEncoding "chunked" fields


-- Run after request modifiers and middleware, so Range and If-Range can
-- be attached in either order. Drop both when the pair cannot be sent.
prepareConditionalRange :: U.Headers -> U.Headers
prepareConditionalRange fields = case U.lookupSingletonHeader U.hIfRange fields of
  Right Nothing -> fields
  Right (Just validator)
    | validValidator validator
    , Right (Just range) <- U.lookupSingletonHeader U.hRange fields
    , Right _ <- Typed.parseFromHeaders @Range.Range Typed.defaultHeaderSettings (range :| []) -> fields
  _ -> filter (\(name, _) -> name /= U.hRange && name /= U.hIfRange) fields
  where
    validValidator wire =
      case Typed.parseFromHeaders @IfRange.IfRange Typed.defaultHeaderSettings (wire :| []) of
        Right (IfRange.IfRangeETag (StrongETagBytes _)) -> True
        Right (IfRange.IfRangeDate _) -> True
        _ -> False


bufferedRequestBody :: BodyStream -> IO U.Body
bufferedRequestBody stream = do
  bytes <- bodyStreamBytes stream
  pure (if BS.null bytes then U.BodyEmpty else U.BodyBytes bytes)


streamedRequestBody :: BodyStream -> U.Body
streamedRequestBody stream = U.BodyStream $ do
  chunk <- pull stream
  pure (if BS.null chunk then Nothing else Just chunk)


------------------------------------------------------------------------
-- HTTP/1.x
------------------------------------------------------------------------

toHttp1Method :: U.Method -> H1.Method
toHttp1Method = id


fromHttp1Method :: H1.Method -> U.Method
fromHttp1Method = id


toHttp1Status :: U.Status -> H1.Status
toHttp1Status = id


fromHttp1Status :: H1.Status -> U.Status
fromHttp1Status = id


toHttp1Version :: U.Version -> H1.Version
toHttp1Version = id


fromHttp1Version :: H1.Version -> U.Version
fromHttp1Version = id


toHttp1Headers :: U.Headers -> H1.Headers
toHttp1Headers = map $ Data.Bifunctor.first CI.original


{- | Wrap HTTP/1 header names in case-insensitive wrappers.
Header names are already lowercased in-place during parsing
(by parseOneHeader's SIMD lowercaseInPlace), so unsafeMk is
correct and avoids any fold allocation. The name ByteStrings
share the same ForeignPtr as the parsed header block.
-}
fromHttp1Headers :: H1.Headers -> U.Headers
fromHttp1Headers = map $ Data.Bifunctor.first unsafeMk


-- Trailer field names retain their received spelling in the HTTP/1 reader.
fromHttp1Trailers :: H1.Headers -> U.Headers
fromHttp1Trailers = map (Data.Bifunctor.first CI.mk)


toHttp1Body :: U.Body -> H1.Body
toHttp1Body = \case
  U.BodyEmpty -> H1.BodyEmpty
  U.BodyBytes bs -> H1.BodyBytes bs
  U.BodyStream p -> H1.BodyStream p


{- | HTTP\/1-only body variants ('BodyFile', 'BodyPreEncoded') are
collapsed back to their bytewise representation.  Use the http1
types directly if you want to preserve them.
-}
fromHttp1Body :: H1.Body -> U.Body
fromHttp1Body = \case
  H1.BodyEmpty -> U.BodyEmpty
  H1.BodyBytes bs -> U.BodyBytes bs
  H1.BodyStream p -> U.BodyStream p
  H1.BodyPreEncoded pe -> U.BodyBytes (H1.peBytes pe)
  H1.BodyFile _ -> U.BodyStream (pure Nothing)


-- Callers shouldn't see this on the wire from a server response,
-- but we collapse to an empty stream just in case.

toHttp1Request :: Request -> H1.Request
toHttp1Request r =
  H1.Request
    { H1.requestMethod = toHttp1Method (requestMethod r)
    , H1.requestTarget = requestTarget r
    , H1.requestVersion = toHttp1Version (requestVersion r)
    , H1.requestHeaders = requestFields
    , H1.requestBody = toHttp1Body (requestBody r)
    , H1.requestTrailers = toHttp1Headers <$> requestTrailers r
    }
  where
    requestFields
      | U.HTTP2 <- requestVersion r
      , Just authority <- requestAuthority r =
          toHttp1Headers (U.insertHeader U.hHost authority (requestHeaders r))
      | otherwise = withHost (requestAuthority r) (toHttp1Headers (requestHeaders r))
    withHost Nothing hs = hs
    withHost (Just a) hs
      | any (\(n, _) -> BS.map toLower8 n == "host") hs = hs
      | otherwise = ("Host", a) : hs
    toLower8 c
      | c >= 0x41 && c <= 0x5A = c + 0x20
      | otherwise = c


fromHttp1Request :: Scheme -> H1.Request -> Request
fromHttp1Request scheme r =
  Request
    { requestMethod = fromHttp1Method (H1.requestMethod r)
    , requestTarget = H1.requestTarget r
    , requestAuthority = U.lookupHeader U.hHost (fromHttp1Headers (H1.requestHeaders r))
    , requestScheme = scheme
    , requestHeaders = fromHttp1Headers (H1.requestHeaders r)
    , requestBody = fromHttp1Body (H1.requestBody r)
    , requestVersion = fromHttp1Version (H1.requestVersion r)
    , requestTrailers = fromHttp1Trailers <$> H1.requestTrailers r
    }


toHttp1Response :: Response -> H1.Response
toHttp1Response r =
  H1.Response
    { H1.responseStatus = toHttp1Status (responseStatus r)
    , H1.responseVersion = toHttp1Version (responseVersion r)
    , H1.responseHeaders = toHttp1Headers (responseHeaders r)
    , H1.responseBody = toHttp1Body (responseBody r)
    , H1.responseTrailers = toHttp1Headers <$> responseTrailers r
    }


{- | HTTP\/1.x trailers come from the chunked body's terminator field
block.  'Network.HTTP1.Connection' already reads them and parks
them on an 'MVar'; we surface that as 'responseTrailers'.

-}
fromHttp1Response :: H1.Response -> Response
fromHttp1Response r =
  Response
    { responseStatus = fromHttp1Status (H1.responseStatus r)
    , responseVersion = fromHttp1Version (H1.responseVersion r)
    , responseHeaders = fromHttp1Headers (H1.responseHeaders r)
    , responseBody = fromHttp1Body (H1.responseBody r)
    , responseTrailers = fromHttp1Trailers <$> H1.responseTrailers r
    , responseH2StreamId = 0
    , responseCancel = pure ()
    , responsePushPromises = pure []
    }


------------------------------------------------------------------------
-- HTTP/2
------------------------------------------------------------------------

-- HPACK requires lowercase header names; we take the folded case so we
-- never emit an uppercase byte on the wire.
toHttp2Headers :: U.Headers -> [(ByteString, ByteString)]
toHttp2Headers = map $ Data.Bifunctor.first CI.foldedCase


{- | HTTP/2 header names are already lowercase (HPACK mandates it).
unsafeMk avoids the redundant foldCase allocation.
-}
fromHttp2Headers :: [(ByteString, ByteString)] -> U.Headers
fromHttp2Headers = map $ Data.Bifunctor.first unsafeMk


toHttp2Request :: Request -> H2S.Request
toHttp2Request r =
  H2S.Request
    { H2S.requestMethod = U.fromMethod (requestMethod r)
    , H2S.requestPath = requestTarget r
    , H2S.requestScheme = case requestScheme r of
        SchemeHttp -> "http"
        SchemeHttps -> "https"
    , H2S.requestAuthority = Data.Maybe.fromMaybe "" (requestAuthority r)
    , H2S.requestHeaders = toHttp2Headers (requestHeaders r)
    , H2S.requestBody = pure ""
    , H2S.requestStreamId = 0
    , H2S.requestTrailers = toHttp2Headers <$> requestTrailers r
    }


{- | HTTP\/2 request → unified.  The request body is delivered as a
pull producer in the underlying API; we wrap it back into 'BodyStream'.
-}
fromHttp2Request :: H2S.Request -> Request
fromHttp2Request r =
  Request
    { requestMethod = U.methodFromBytes (H2S.requestMethod r)
    , requestTarget =
        if H2S.requestMethod r == "CONNECT"
          then H2S.requestAuthority r
          else H2S.requestPath r
    , requestAuthority = case H2S.requestAuthority r of
        "" -> Nothing
        a -> Just a
    , requestScheme = case H2S.requestScheme r of
        "https" -> SchemeHttps
        _ -> SchemeHttp
    , requestHeaders = fromHttp2Headers (H2S.requestHeaders r)
    , requestBody = U.BodyStream $ do
        bs <- H2S.requestBody r
        if BS.null bs then pure Nothing else pure (Just bs)
    , requestVersion = U.HTTP2
    , requestTrailers = fromHttp2Headers <$> H2S.requestTrailers r
    }


-- | Adapt the unified response, retaining the trailer action for the
-- response writer to evaluate after sending the body.
toHttp2Response :: Response -> H2S.Response
toHttp2Response r =
  H2S.Response
    { H2S.responseStatus = fromIntegral (U.statusCode (responseStatus r))
    , H2S.responseHeaders = toHttp2Headers (responseHeaders r)
    , H2S.responseBody = case responseBody r of
        U.BodyEmpty -> H2S.ResponseBodyEmptyWithTrailers produceTrailers
        U.BodyBytes bs -> H2S.ResponseBodyBSWithTrailers bs produceTrailers
        U.BodyStream produce -> H2S.ResponseBodyStreamWithTrailers produce produceTrailers
    , H2S.responseTrailers = []
    }
  where
    produceTrailers = toHttp2Headers <$> responseTrailers r


fromHttp2Response :: H2S.Response -> Response
fromHttp2Response r =
  Response
    { responseStatus = U.Status (fromIntegral (H2S.responseStatus r))
    , responseVersion = U.HTTP2
    , responseHeaders = fromHttp2Headers (H2S.responseHeaders r)
    , responseBody = case H2S.responseBody r of
        H2S.ResponseBodyEmpty -> U.BodyEmpty
        H2S.ResponseBodyBS bs -> U.BodyBytes bs
        H2S.ResponseBodyStream p -> U.BodyStream p
        H2S.ResponseBodyStreamWithTrailers p _ -> U.BodyStream p
        H2S.ResponseBodyEmptyWithTrailers _ -> U.BodyEmpty
        H2S.ResponseBodyBSWithTrailers bs _ -> U.BodyBytes bs
    , responseTrailers = fromHttp2Headers <$> case H2S.responseBody r of
        H2S.ResponseBodyStreamWithTrailers _ produce -> (H2S.responseTrailers r <>) <$> produce
        H2S.ResponseBodyEmptyWithTrailers produce -> (H2S.responseTrailers r <>) <$> produce
        H2S.ResponseBodyBSWithTrailers _ produce -> (H2S.responseTrailers r <>) <$> produce
        _ -> pure (H2S.responseTrailers r)
    , responseH2StreamId = 0
    , responseCancel = pure ()
    , responsePushPromises = pure []
    }
