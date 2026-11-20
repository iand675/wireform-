{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Streaming-friendly base transport.

The default 'Network.HTTP.Client.Base.baseTransport' and
'Network.HTTP.Client.Pool.pooledTransport' materialise response
bodies before returning, because the low-level
'Network.HTTP.Connection.withConnection' bracket has to close
when 'sendOn' returns and the response popper lives inside that
bracket.

This module's 'streamedTransport' takes a different shape: for
each request it forks a /worker thread/ that owns the connection
through the response's trailer section, pipes the body through a bounded
'TBQueue', and returns a 'RawResponse' whose 'bodyPopper' reads
from that queue. The caller's popper is therefore live and
streaming — chunks land in memory as they come off the wire,
rather than the whole body being drained up front.

== Trade-offs vs the pool

* /No connection reuse./ Each request opens a fresh connection
  and closes it when the body and trailers have arrived. Pair with
  'Network.HTTP.Client.Pool.pooledTransport' when you want reuse
  on the request side and don't need streaming responses.
* /Back-pressure./ Read the body before waiting for trailers. If a
  caller abandons a response while the chunk queue is full, the worker
  cannot make more progress until the body is drained or cancelled.
* /Streaming-aware middleware./ Compression and tracing both work
  unchanged: compression sees the chunks as they arrive,
  'withTracing' keeps its span open until the popper hits EOF
  via the response popper.

== Future
This module is the substrate for a streaming-capable pool. Once
'wireform-http2' exposes non-bracketed connection open\/close, the
pool can adopt this same worker-per-checkout shape and we collapse
the two transports back into one.
-}
module Network.HTTP.Client.Streaming (
  streamedTransport,
  streamedTransportVia,
  streamedTransportWith,
  streamedTransportWithVia,
  StreamingError (..),
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, mask_, throwIO, try)
import Control.Monad (void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Network.HTTP.Client.BodyStream (BodyStream, Popper)
import Network.HTTP.Client.Protocol
import Network.HTTP.Client.Proxy (Proxy, ProxyConfig)
import Network.HTTP.Client.Proxy qualified as Pxy
import Network.HTTP.Client.Request qualified as WReq
import Network.HTTP.Client.Response
import Network.HTTP.Client.Transport
import Network.HTTP.Client.URI qualified as WURI
import Network.HTTP.Connection qualified as Conn
import Network.HTTP.Internal.Convert qualified as Conv
import Network.HTTP.Message qualified as Msg
import Network.HTTP.Types.Body qualified as LB
import Network.HTTP.Types.Header (Headers)
import Network.HTTP.Types.Version qualified as LV
import Network.HTTP.VersionRange qualified as VR


-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

data StreamingError
  = StreamingInvalidURI !String
  | StreamingHandshakeFailed !String
  deriving stock (Show)


instance Exception StreamingError


-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Streaming base transport with a default queue size.
streamedTransport :: VR.VersionRange -> Transport IO
streamedTransport = streamedTransportWith 32


{- | Proxy-aware variant of 'streamedTransport'. See
'streamedTransportWithVia' for the routing rules.
-}
streamedTransportVia :: VR.VersionRange -> ProxyConfig -> Transport IO
streamedTransportVia = streamedTransportWithVia 32


{- | Streaming base transport with explicit chunk-queue capacity. A
small queue applies natural back-pressure to the producer when
the consumer is slow; a large queue smooths over jittery
consumers at the cost of more buffering.
-}
streamedTransportWith :: Int -> VR.VersionRange -> Transport IO
streamedTransportWith qcap versionRange =
  streamedTransportWithVia qcap versionRange Pxy.noProxyConfig


{- | Proxy-aware variant of 'streamedTransportWith'. Routes HTTPS
targets through @CONNECT@ when an HTTPS proxy is configured;
HTTP targets dial the proxy directly (the absolute-form request
line is the responsibility of the
'Network.HTTP.Client.Proxy.withProxy' middleware).
-}
streamedTransportWithVia :: Int -> VR.VersionRange -> ProxyConfig -> Transport IO
streamedTransportWithVia qcap versionRange pcfg = Transport $ \req -> do
  cfg <- connectionConfigForRequest versionRange req
  uri_ <- case WURI.renderRequestURI (WReq.requestURI req) of
    Right u -> pure u
    Left err -> throwIO (StreamingInvalidURI err)
  let mProxy
        | Pxy.shouldBypass pcfg (WURI.uriHost uri_) = Nothing
        | otherwise = case WURI.uriScheme uri_ of
            WURI.SchemeHttp -> Pxy.proxyForHttp pcfg
            WURI.SchemeHttps -> Pxy.proxyForHttps pcfg
  prepared <- Conv.prepareRequestBody Conv.bufferedRequestBody req
  let lowReq = toLowLevelRequest uri_ prepared
      hints = WReq.protocolHints req

  chunks <- newTBQueueIO (fromIntegral qcap)
  rawVar <- newEmptyMVar
  completion <- newEmptyTMVarIO
  _wtid <- forkIO $ workerLoop cfg mProxy hints lowReq chunks rawVar completion
  result <- readMVar rawVar
  case result of
    Left e -> throwIO e
    Right raw -> pure raw


-- ---------------------------------------------------------------------------
-- Worker
-- ---------------------------------------------------------------------------

workerLoop
  :: Conn.ConnectionConfig
  -> Maybe Proxy
  -> ProtocolHints
  -> Msg.Request
  -> TBQueue ByteString
  -> MVar (Either SomeException RawResponse)
  -> TMVar (Either SomeException Headers)
  -> IO ()
workerLoop cfg mProxy hints lowReq chunks rawVar completion = do
  outcome <- try $ Conn.withConnectionVia cfg mProxy Nothing $ \conn -> do
    framed <- Conv.prepareRequestTrailers (Conn.negotiatedVersion conn) hints lowReq
    Conn.withResponseOnWithContinueTimeout (continueTimeoutMicros hints) conn framed $ \resp -> do
      let cancel = mask_ $ do
            running <- atomically $ do
              _ <- flushTBQueue chunks
              tryPutTMVar completion (Right [])
            when running (Msg.responseCancel resp)
          readTrailers = atomically (readTMVar completion >>= either throwSTM pure)
          raw =
            RawResponse
              { statusCode = Msg.responseStatus resp
              , headers = Msg.responseHeaders resp
              , bodyPopper = readChunk chunks completion
              , protocolInfo = withProtocolTrailers readTrailers $ case Msg.responseVersion resp of
                  LV.HTTP2 ->
                    HTTP2
                      Http2Info
                        { h2StreamId = Msg.responseH2StreamId resp
                        , h2PushPromises = pure []
                        , h2CancelStream = cancel
                        }
                  _ -> HTTP1_1
              }
      putMVar rawVar (Right raw)

      -- Completion lives outside the bounded queue, so it can be
      -- published even when the caller has chunks left to read.
      complete <- pumpBody (Msg.responseBody resp) writeChunk
      when complete $ do
        trailers <- Msg.responseTrailers resp
        atomically $ void (tryPutTMVar completion (Right trailers))
  case outcome of
    Right () -> pure ()
    Left (e :: SomeException) -> do
      void (tryPutMVar rawVar (Left e))
      atomically $ void (tryPutTMVar completion (Left e))
  where
    writeChunk bytes = atomically $ do
      running <- isEmptyTMVar completion
      when running (writeTBQueue chunks bytes)
      pure running


-- | Drain until EOF or until cancellation rejects the next chunk.
pumpBody :: LB.Body -> (ByteString -> IO Bool) -> IO Bool
pumpBody body writeChunk = case body of
  LB.BodyEmpty -> pure True
  LB.BodyBytes bs
    | BS.null bs -> pure True
    | otherwise -> writeChunk bs
  LB.BodyStream p -> drainStream p
  where
    drainStream p = do
      mChunk <- p
      case mChunk of
        Nothing -> pure True
        Just b
          | BS.null b -> drainStream p
          | otherwise -> do
              accepted <- writeChunk b
              if accepted then drainStream p else pure False


-- | Drain queued bytes before surfacing successful EOF or a wire error.
readChunk
  :: TBQueue ByteString
  -> TMVar (Either SomeException Headers)
  -> Popper
readChunk chunks completion =
  atomically $
    readTBQueue chunks
      `orElse` (readTMVar completion >>= either throwSTM (const (pure BS.empty)))


-- ---------------------------------------------------------------------------
-- Connection config + request lowering (shared with the pool, but
-- replicated here to keep the modules independent).
-- ---------------------------------------------------------------------------

connectionConfigForRequest
  :: VR.VersionRange
  -> WReq.Request BodyStream
  -> IO Conn.ConnectionConfig
connectionConfigForRequest versionRange req = do
  case WURI.renderRequestURI (WReq.requestURI req) of
    Left err -> throwIO (StreamingInvalidURI err)
    Right u -> do
      let scheme = WURI.uriScheme u
          host = BS8.unpack (WURI.uriHost u)
          tls = case scheme of
            WURI.SchemeHttps -> Just (Conn.defaultTlsConnectionConfig host)
            WURI.SchemeHttp -> Nothing
      pure
        Conn.ConnectionConfig
          { Conn.connectionHost = host
          , Conn.connectionPort = show (WURI.uriPort u)
          , Conn.connectionVersionRange = versionRange
          , Conn.connectionTls = tls
          }


toLowLevelRequest
  :: WURI.URI
  -> WReq.Request LB.Body
  -> Msg.Request
toLowLevelRequest uri_ req =
  let lowScheme = case WURI.uriScheme uri_ of
        WURI.SchemeHttps -> Msg.SchemeHttps
        WURI.SchemeHttp -> Msg.SchemeHttp
      target = WURI.uriPathAndQuery uri_
      hostBs = WURI.uriHost uri_
      authority =
        Just
          ( hostBs <> case (WURI.uriScheme uri_, WURI.uriPort uri_) of
              (WURI.SchemeHttp, 80) -> ""
              (WURI.SchemeHttps, 443) -> ""
              (_, p) -> ":" <> BS8.pack (show p)
          )
  in Msg.Request
       { Msg.requestMethod = WReq.method req
       , Msg.requestTarget = target
       , Msg.requestAuthority = authority
       , Msg.requestScheme = lowScheme
       , Msg.requestHeaders = WReq.headers req
       , Msg.requestBody = WReq.body req
       , Msg.requestVersion = VR.preferredVersion (Conn.connectionVersionRange placeholder)
       , Msg.requestTrailers = pure []
       }
  where
    placeholder =
      Conn.defaultConnectionConfig
        { Conn.connectionVersionRange = VR.preferHttp1
        }
