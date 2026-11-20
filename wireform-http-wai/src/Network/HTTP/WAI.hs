{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

{- | Bidirectional adapter between WAI and wireform-http.

=== Testing existing WAI applications

@
import qualified Network.Wai as Wai
import Network.HTTP.WAI (waiToHandler, waiTransport)

myWaiApp :: Wai.'Wai.Application'
myWaiApp = ...

-- Option 1: use as a wireform-http Handler (server-level types)
handler :: 'Network.HTTP.Server.Handler'
handler = 'waiToHandler' myWaiApp

-- Option 2: use as a wireform-http Transport (client-level types)
-- for in-process testing with the full middleware stack
transport :: 'Network.HTTP.Client.Transport.Transport' IO
transport = 'waiTransport' myWaiApp
@

=== Serving wireform-http handlers through Warp

@
import qualified Network.Wai.Handler.Warp as Warp
import Network.HTTP.WAI (handlerToWai)

myHandler :: 'Network.HTTP.Server.Handler'
myHandler req = ...

main :: IO ()
main = Warp.run 8080 ('handlerToWai' myHandler)
@

=== Limitations

* WAI 'Network.Wai.responseRaw' (WebSocket upgrade) is not
  supported. The adapter converts the backup response; use a
  real TCP server for upgrade testing.
* 'fromWaiResponse' materialises the entire response body into
  memory. For large streaming responses, consider testing at the
  WAI level directly.
-}
module Network.HTTP.WAI (
  -- * Server-level adapters
  waiToHandler,
  handlerToWai,

  -- * Client-level adapter (in-process Transport)
  waiTransport,

  -- * Type conversions: wireform → WAI
  toWaiRequest,
  toWaiResponse,

  -- * Type conversions: WAI → wireform
  fromWaiRequest,
  fromWaiResponse,

  -- * Errors
  WaiAdapterError (..),
) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (byteString, toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.CaseInsensitive qualified as CI
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vault.Lazy qualified as Vault
import Data.Word (Word16)
import Network.HTTP.Client.BodyStream qualified as CBS
import Network.HTTP.Client.Protocol qualified as P
import Network.HTTP.Client.Request qualified as CR
import Network.HTTP.Client.Response qualified as CResp
import Network.HTTP.Client.Transport qualified as CT
import Network.HTTP.Client.URI qualified as URI
import Network.HTTP.Message qualified as U
import Network.Socket qualified as NS
import Network.Wai qualified as Wai
import Network.Wai.Internal (ResponseReceived (ResponseReceived))
import Network.Wai.Internal qualified as WaiI
import "http-types" Network.HTTP.Types qualified as WAIHttp
import "wireform-http" Network.HTTP.Types.Body qualified as U
import "wireform-http" Network.HTTP.Types.Header qualified as U
import "wireform-http" Network.HTTP.Types.Method qualified as U
import "wireform-http" Network.HTTP.Types.Status qualified as U
import "wireform-http" Network.HTTP.Types.Version qualified as U


------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

data WaiAdapterError
  = WaiAppDidNotRespond
  | WaiInvalidClientURI !String
  deriving stock (Show)


instance Exception WaiAdapterError


------------------------------------------------------------------------
-- Primitive type conversions
------------------------------------------------------------------------

toWaiMethod :: U.Method -> WAIHttp.Method
toWaiMethod = U.fromMethod


fromWaiMethod :: WAIHttp.Method -> U.Method
fromWaiMethod = U.methodFromBytes


toWaiStatus :: U.Status -> WAIHttp.Status
toWaiStatus s = WAIHttp.mkStatus (fromIntegral (U.statusCode s)) (U.statusReason s)


fromWaiStatus :: WAIHttp.Status -> U.Status
fromWaiStatus s = U.Status (fromIntegral (WAIHttp.statusCode s) :: Word16)


toWaiVersion :: U.Version -> WAIHttp.HttpVersion
toWaiVersion v =
  WAIHttp.HttpVersion
    (fromIntegral (U.versionMajor v))
    (fromIntegral (U.versionMinor v))


fromWaiVersion :: WAIHttp.HttpVersion -> U.Version
fromWaiVersion v =
  U.mkVersion
    (fromIntegral (WAIHttp.httpMajor v))
    (fromIntegral (WAIHttp.httpMinor v))


toWaiHeaders :: U.Headers -> WAIHttp.ResponseHeaders
toWaiHeaders = id


fromWaiHeaders :: [WAIHttp.Header] -> U.Headers
fromWaiHeaders = id


------------------------------------------------------------------------
-- Body conversions
------------------------------------------------------------------------

mkWaiBodyReader :: U.Body -> IO (IO ByteString)
mkWaiBodyReader = \case
  U.BodyEmpty -> pure (pure BS.empty)
  U.BodyBytes bs -> do
    ref <- newIORef (Just bs)
    pure $ atomicModifyIORef' ref $ \case
      Nothing -> (Nothing, BS.empty)
      Just b -> (Nothing, b)
  U.BodyStream producer -> pure $ do
    mb <- producer
    case mb of
      Nothing -> pure BS.empty
      Just b -> pure b


waiBodyToBody :: IO ByteString -> U.Body
waiBodyToBody reader = U.BodyStream $ do
  chunk <- reader
  if BS.null chunk
    then pure Nothing
    else pure (Just chunk)


------------------------------------------------------------------------
-- Request conversions (server-level, Network.HTTP.Message)
------------------------------------------------------------------------

splitTarget :: ByteString -> (ByteString, ByteString)
splitTarget = BS.break (== 0x3F)


decodePath :: ByteString -> [T.Text]
decodePath raw =
  let stripped = BS.dropWhile (== 0x2F) raw
  in if BS.null stripped
       then []
       else map TE.decodeUtf8 (BS.split 0x2F stripped)


{- | Ensure the header list contains a Host entry. If absent and
an authority is available, synthesise one.
-}
ensureHost :: Maybe ByteString -> U.Headers -> U.Headers
ensureHost mAuth hdrs
  | U.hasHeader U.hHost hdrs = hdrs
  | Just auth <- mAuth = (U.hHost, auth) : hdrs
  | otherwise = hdrs


{- | Convert a wireform unified 'U.Request' into a WAI 'Wai.Request'.

Fields WAI needs but wireform doesn't carry (vault, remoteHost) are
filled with sensible defaults.  A @Host@ header is synthesised from
'U.requestAuthority' if not already present in the header list.
-}
toWaiRequest :: U.Request -> IO Wai.Request
toWaiRequest r = do
  let body = U.requestBody r
  bodyReader <- mkWaiBodyReader body
  let (rawPath, rawQS) = splitTarget (U.requestTarget r)
      secure = case U.requestScheme r of
        U.SchemeHttps -> True
        U.SchemeHttp -> False
      bodyLen = case body of
        U.BodyEmpty -> Wai.KnownLength 0
        U.BodyBytes bs -> Wai.KnownLength (fromIntegral (BS.length bs))
        U.BodyStream _ -> Wai.ChunkedBody
      waiHdrs = toWaiHeaders (ensureHost (U.requestAuthority r) (U.requestHeaders r))
      hostHdr = U.lookupHeader U.hHost waiHdrs
  pure $
    Wai.setRequestBodyChunks bodyReader $
      Wai.mapRequestHeaders
        (const waiHdrs)
        Wai.defaultRequest
          { WaiI.requestMethod = toWaiMethod (U.requestMethod r)
          , WaiI.httpVersion = toWaiVersion (U.requestVersion r)
          , WaiI.rawPathInfo = rawPath
          , WaiI.rawQueryString = rawQS
          , WaiI.isSecure = secure
          , WaiI.remoteHost = NS.SockAddrInet 0 0
          , WaiI.pathInfo = decodePath rawPath
          , WaiI.queryString = WAIHttp.parseQuery rawQS
          , WaiI.vault = Vault.empty
          , WaiI.requestBodyLength = bodyLen
          , WaiI.requestHeaderHost = hostHdr
          , WaiI.requestHeaderRange = U.lookupHeader U.hRange waiHdrs
          , WaiI.requestHeaderReferer = lookupCI "Referer" waiHdrs
          , WaiI.requestHeaderUserAgent = U.lookupHeader U.hUserAgent waiHdrs
          }


-- | Convert a WAI 'Wai.Request' into a wireform unified 'U.Request'.
fromWaiRequest :: Wai.Request -> U.Request
fromWaiRequest r =
  U.Request
    { U.requestMethod = fromWaiMethod (Wai.requestMethod r)
    , U.requestTarget = Wai.rawPathInfo r <> Wai.rawQueryString r
    , U.requestAuthority = Wai.requestHeaderHost r
    , U.requestScheme = if Wai.isSecure r then U.SchemeHttps else U.SchemeHttp
    , U.requestHeaders = fromWaiHeaders (Wai.requestHeaders r)
    , U.requestBody = waiBodyToBody (Wai.getRequestBodyChunk r)
    , U.requestVersion = fromWaiVersion (Wai.httpVersion r)
    , U.requestTrailers = pure []
    }


------------------------------------------------------------------------
-- Response conversions (server-level, Network.HTTP.Message)
------------------------------------------------------------------------

-- | Convert a wireform unified 'U.Response' into a WAI 'Wai.Response'.
toWaiResponse :: U.Response -> Wai.Response
toWaiResponse r =
  let status = toWaiStatus (U.responseStatus r)
      hdrs = toWaiHeaders (U.responseHeaders r)
  in case U.responseBody r of
       U.BodyEmpty -> Wai.responseLBS status hdrs LBS.empty
       U.BodyBytes bs -> Wai.responseLBS status hdrs (LBS.fromStrict bs)
       U.BodyStream p -> Wai.responseStream status hdrs $ \write flush -> do
         let loop = do
               mb <- p
               case mb of
                 Nothing -> flush
                 Just chunk -> do
                   write (byteString chunk)
                   loop
         loop


{- | Convert a WAI 'Wai.Response' into a wireform unified 'U.Response'.

Uses 'Wai.responseToStream' to normalise the WAI response. The
entire body is materialised into memory as a strict 'ByteString'.
-}
fromWaiResponse :: Wai.Response -> IO U.Response
fromWaiResponse resp = do
  let (status, hdrs, withBody) = Wai.responseToStream resp
  body <- collectStreamingBody withBody
  pure
    U.Response
      { U.responseStatus = fromWaiStatus status
      , U.responseVersion = U.HTTP1_1
      , U.responseHeaders = stripFramingHeaders (fromWaiHeaders hdrs)
      , U.responseBody = if BS.null body then U.BodyEmpty else U.BodyBytes body
      , U.responseTrailers = pure []
      , U.responseH2StreamId = 0
      , U.responseCancel = pure ()
      , U.responsePushPromises = pure []
      }


{- | When materializing a streamed response to BodyBytes, the
original Transfer-Encoding header is no longer meaningful.
The wireform encoder will set Content-Length on encode.
-}
stripFramingHeaders :: U.Headers -> U.Headers
stripFramingHeaders = U.deleteHeader U.hTransferEncoding


{- | Collect a WAI StreamingBody into a strict ByteString.
Uses a difference-list style accumulator to avoid O(n²) appends.
-}
collectStreamingBody :: ((WaiI.StreamingBody -> IO ByteString) -> IO ByteString) -> IO ByteString
collectStreamingBody withBody = withBody $ \streamBody -> do
  accRef <- newIORef id
  streamBody
    ( \builder -> do
        let !bs = LBS.toStrict (toLazyByteString builder)
        atomicModifyIORef' accRef (\dl -> (dl . (bs :), ()))
    )
    (pure ())
  dl <- readIORef accRef
  pure $! BS.concat (dl [])


------------------------------------------------------------------------
-- Server-level adapters
------------------------------------------------------------------------

{- | Convert a WAI 'Wai.Application' into a wireform-http 'Handler'.

The resulting handler can be plugged into 'Network.HTTP.Server.ServerConfig'
or called directly in tests.
-}
waiToHandler :: Wai.Application -> (U.Request -> IO U.Response)
waiToHandler app req = do
  waiReq <- toWaiRequest req
  fromWaiResponseCPS (app waiReq)


{- | Convert a wireform-http 'Handler' into a WAI 'Wai.Application'.

Useful for serving wireform-http handlers through Warp or any other
WAI-compatible server.
-}
handlerToWai :: (U.Request -> IO U.Response) -> Wai.Application
handlerToWai handler waiReq respond = do
  let req = fromWaiRequest waiReq
  resp <- handler req
  respond (toWaiResponse resp)


------------------------------------------------------------------------
-- Client-level adapter: in-process Transport
------------------------------------------------------------------------

{- | Build a wireform-http 'CT.Transport' that dispatches requests to a
WAI 'Wai.Application' in-process, without any TCP connection.

This is the primary entry point for testing existing WAI apps with
wireform-http's client infrastructure (middleware stack, matchers,
assertions, VCR, etc.).

The client request URI is parsed to extract path, query, host, and
scheme; absolute URIs (after 'withBaseURL') are handled correctly.
-}
waiTransport :: Wai.Application -> CT.Transport IO
waiTransport app = CT.Transport $ \clientReq -> do
  waiReq <- clientRequestToWai clientReq
  fromWaiResponseToRaw (app waiReq)


------------------------------------------------------------------------
-- Client request → WAI request
------------------------------------------------------------------------

clientRequestToWai :: CR.Request CBS.BodyStream -> IO Wai.Request
clientRequestToWai req = do
  let waiHdrs = toWaiHeaders (CR.headers req)
  (rawPath, rawQS, hostVal, secure) <- case URI.renderRequestURI (CR.requestURI req) of
    Right uri ->
      pure
        ( let p = URI.uriPath uri in if BS.null p then "/" else p
        , URI.uriRawQuery uri
        , Just (URI.uriHost uri <> portSuffix uri)
        , URI.uriScheme uri == URI.SchemeHttps
        )
    Left _ ->
      -- Relative URI (no base URL set) — fall back to text rendering
      let txt = URI.requestURIToText (CR.requestURI req)
          bs = TE.encodeUtf8 txt
          (p, q) = splitTarget bs
      in pure (p, q, U.lookupHeader U.hHost (CR.headers req), False)
  bodyReader <- bodyStreamToWaiReader (CR.body req)
  let bodyLen = case CBS.knownSize (CR.body req) of
        Just n -> Wai.KnownLength (fromIntegral n)
        Nothing -> Wai.ChunkedBody
      allHdrs = case hostVal of
        Just h
          | not (U.hasHeader U.hHost (CR.headers req)) ->
              (U.hHost, h) : waiHdrs
        _ -> waiHdrs
  pure $
    Wai.setRequestBodyChunks bodyReader $
      Wai.mapRequestHeaders
        (const allHdrs)
        Wai.defaultRequest
          { WaiI.requestMethod = U.fromMethod (CR.method req)
          , WaiI.httpVersion = WAIHttp.http11
          , WaiI.rawPathInfo = rawPath
          , WaiI.rawQueryString = rawQS
          , WaiI.isSecure = secure
          , WaiI.remoteHost = NS.SockAddrInet 0 0
          , WaiI.pathInfo = decodePath rawPath
          , WaiI.queryString = WAIHttp.parseQuery rawQS
          , WaiI.vault = Vault.empty
          , WaiI.requestBodyLength = bodyLen
          , WaiI.requestHeaderHost = U.lookupHeader U.hHost allHdrs
          , WaiI.requestHeaderRange = U.lookupHeader U.hRange allHdrs
          , WaiI.requestHeaderReferer = lookupCI "Referer" allHdrs
          , WaiI.requestHeaderUserAgent = U.lookupHeader U.hUserAgent allHdrs
          }


portSuffix :: URI.URI -> ByteString
portSuffix uri =
  let p = URI.uriPort uri
      isDefault = case URI.uriScheme uri of
        URI.SchemeHttp -> p == 80
        URI.SchemeHttps -> p == 443
  in if isDefault then "" else ":" <> TE.encodeUtf8 (T.pack (show p))


bodyStreamToWaiReader :: CBS.BodyStream -> IO (IO ByteString)
bodyStreamToWaiReader stream = pure (CBS.pull stream)


------------------------------------------------------------------------
-- WAI response → wireform RawResponse (for Transport)
------------------------------------------------------------------------

fromWaiResponseToRaw :: ((Wai.Response -> IO ResponseReceived) -> IO ResponseReceived) -> IO CResp.RawResponse
fromWaiResponseToRaw withRespond = do
  resultRef <- newIORef Nothing
  _ <- withRespond $ \waiResp -> do
    let (status, hdrs, withBody) = Wai.responseToStream waiResp
    body <- collectStreamingBody withBody
    popper <- CBS.popperFromStrict body
    let raw =
          CResp.RawResponse
            { CResp.statusCode = fromWaiStatus status
            , CResp.headers = stripFramingHeaders (fromWaiHeaders hdrs)
            , CResp.bodyPopper = popper
            , CResp.protocolInfo = P.HTTP1_1
            }
    writeIORef resultRef (Just raw)
    pure ResponseReceived
  mr <- readIORef resultRef
  case mr of
    Just raw -> pure raw
    Nothing -> throwIO WaiAppDidNotRespond


------------------------------------------------------------------------
-- WAI response CPS helper (for waiToHandler)
------------------------------------------------------------------------

fromWaiResponseCPS :: ((Wai.Response -> IO ResponseReceived) -> IO ResponseReceived) -> IO U.Response
fromWaiResponseCPS withRespond = do
  resultRef <- newIORef Nothing
  _ <- withRespond $ \waiResp -> do
    r <- fromWaiResponse waiResp
    writeIORef resultRef (Just r)
    pure ResponseReceived
  mr <- readIORef resultRef
  case mr of
    Just r -> pure r
    Nothing -> throwIO WaiAppDidNotRespond


------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------

lookupCI :: ByteString -> U.Headers -> Maybe ByteString
lookupCI name = U.lookupHeader (CI.mk name)
