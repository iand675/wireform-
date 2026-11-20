{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Micro-benchmarks for the wireform HTTP unified layer.

Measures the overhead of the conversion layer and the unified
client/server on top of the HTTP/1 and HTTP/2 runtimes.
-}
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (finally)
import Criterion.Main
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.List qualified as List
import Network.HTTP
import Network.HTTP.Client.BodyStream (emptyStream, popperBytes, popperFromStrict)
import Network.HTTP.Client.Cache qualified as Cache
import Network.HTTP.Client.Compression qualified as Compression
import Network.HTTP.Client.Conditional qualified as Conditional
import Network.HTTP.Client.Cookies qualified as Cookies
import Network.HTTP.Client.Protocol qualified as Protocol
import Network.HTTP.Client.Range qualified as Range
import Network.HTTP.Client.Redirect qualified as Redirect
import Network.HTTP.Client.Request qualified as Request
import Network.HTTP.Client.Response qualified as Raw
import Network.HTTP.Client.SSE qualified as SSE
import Network.HTTP.Client.Transport qualified as Transport
import Network.HTTP.Client.URI qualified as URI
import Network.HTTP.Connection
import Network.HTTP.Server
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Status qualified as S
import Network.HTTP.Types.Version qualified as V
import Network.Socket qualified as NS


main :: IO ()
main = do
  cookieBenchmarks <- sequence
    [ receiveCookieBenchmark "session" "session=abc; Path=/; Secure"
    , receiveCookieBenchmark "Max-Age" "session=abc; Path=/; Secure; Max-Age=3600"
    , receiveCookieBenchmark "Expires" "session=abc; Path=/; Secure; Expires=Fri, 31 Dec 9999 00:00:00 GMT"
    ]
  defaultMain
    [ bgroup
        "HTTP/1.x"
        [ withSetup http1Only "hello-world GET" helloHandler $ \port ->
            bench "request/response" $ nfIO (doGet http1Only port V.HTTP1_1)
        , withSetup http1Only "64KiB POST echo" echoHandler $ \port ->
            bench "large body" $ nfIO (doPost http1Only port V.HTTP1_1 payload64k)
        , withSetup http1Only "streaming 64KiB" (streamHandler 16) $ \port ->
            bench "streaming response" $ nfIO (doGet http1Only port V.HTTP1_1)
        ]
    , bgroup
        "HTTP/2"
        [ withSetup http2Only "hello-world GET" helloHandler $ \port ->
            bench "request/response" $ nfIO (doGet http2Only port V.HTTP2)
        , withSetup http2Only "64KiB POST echo" echoHandler $ \port ->
            bench "large body" $ nfIO (doPost http2Only port V.HTTP2 payload64k)
        , withSetup http2Only "streaming 64KiB" (streamHandler 16) $ \port ->
            bench "streaming response" $ nfIO (doGet http2Only port V.HTTP2)
        ]
    , bgroup
        "SSE cursor"
        [ bench "data only" $ nf sseCursorScore (sseBatch "data: payload\n\n")
        , bench "ASCII IDs" $ nf sseCursorScore (sseBatch "id: event-42\ndata: payload\n\n")
        , bench "UTF-8 IDs" $ nf sseCursorScore (sseBatch "id: caf\xC3\xA9\ndata: payload\n\n")
        ]
    , bgroup
        "Multipart ranges"
        [ bench "two short parts" $ nf multipartScore (multipartFixture "0123456789")
        , bench "two 64 KiB parts" $ nf multipartScore (multipartFixture payload64k)
        , bench "two 64 KiB LF-filled parts" $ nf multipartScore (multipartFixture (BS.replicate (64 * 1024) 0x0A))
        ]
    , bgroup
        "Range request authoring"
        [ bench "replace range" $ nf (requestHeaderBytes . Range.withRange [Range.byteRange 0 511]) rangeRequest
        , bench "replace validator" $ nf (requestHeaderBytes . Conditional.ifRange strongValidator) rangeRequest
        ]
    , bgroup
        "Redirected ranges"
        [ bench "ordinary GET" $ nfIO (redirectScore ordinaryRedirectRequest)
        , bench "conditional range" $ nfIO (redirectScore (Conditional.ifRange strongValidator rangeRequest))
        ]
    , env (Compression.compressBytes @Compression.Gzip payload64k) $ \gzip ->
        bgroup
          "Decompression policy"
          [ bench "unencoded GET" $ nfIO (decompressionScore ordinaryRedirectRequest S.status200 [] "body")
          , bench "gzip GET" $ nfIO (decompressionScore ordinaryRedirectRequest S.status200 [(H.hContentEncoding, "gzip")] gzip)
          , bench "gzip partial" $ nfIO (decompressionScore rangeRequest S.status206 [(H.hContentEncoding, "gzip")] (BS.drop 5 gzip))
          ]
    , env (Compression.compressBytes @Compression.Gzip "cached representation") $ \gzip ->
        bgroup "Cache body metadata" [bench "revalidate decoded body" $ nfIO (revalidationScore gzip)]
    , bgroup "Cookie receipt" cookieBenchmarks
    ]


receiveCookieBenchmark :: String -> BS.ByteString -> IO Benchmark
receiveCookieBenchmark name field = do
  jar <- Cookies.newCookieJar
  let request = Request.request mGet (URI.staticURI "https://example.com/account") emptyStream
      response = Raw.RawResponse S.status200 [(H.hSetCookie, field)] (pure "") Protocol.HTTP1_1
      client = Cookies.withCookies jar (Transport.Transport (const (pure response)))
      receive = () <$ Transport.sendRaw client request
  receive
  pure (bench name (nfIO receive))


sseBatch :: BS.ByteString -> BS.ByteString
sseBatch = BS.concat . replicate 32


sseCursorScore :: BS.ByteString -> Int
sseCursorScore = List.foldl' score 0 . SSE.parseEventStreamEvents
  where
    score n event = n + BS.length (SSE.sseData event) + maybe 0 BS.length (SSE.sseEventId event)


multipartFixture :: BS.ByteString -> BS.ByteString
multipartFixture bytes = part 0 <> part size <> "\r\n--BENCHBOUNDARY--\r\n"
  where
    size = BS.length bytes
    part start =
      (if start == 0 then "" else "\r\n")
        <> "--BENCHBOUNDARY\r\nContent-Type: application/octet-stream\r\nContent-Range: "
        <> Range.renderContentRange
             (Range.ContentRange (fromIntegral start) (fromIntegral (start + size - 1)) (Just (fromIntegral (2 * size))))
        <> "\r\n\r\n" <> bytes


multipartScore :: BS.ByteString -> Int
multipartScore bytes = case Range.parseMultipartByteranges "BENCHBOUNDARY" bytes of
  Just parts -> List.foldl' (\n part -> n + BS.length (Range.mbBody part)) 0 parts
  Nothing -> error "Invalid multipart range benchmark fixture"


rangeRequest :: Request.Request ()
rangeRequest = (Request.get template)
  { Request.headers = [("X-Test", "fixture"), (H.hRange, "bytes=0-1023"), (H.hIfRange, "\"older\"")]
  }
  where
    template = either (error . show) id (URI.parseTemplate "https://example.com/download")


strongValidator :: Conditional.IfRange
strongValidator = Conditional.IfRangeETag (Conditional.strongETag "revision-42")


requestHeaderBytes :: Request.Request a -> Int
requestHeaderBytes = List.foldl' (\n (_, bytes) -> n + BS.length bytes) 0 . Request.headers


ordinaryRedirectRequest :: Request.Request ()
ordinaryRedirectRequest = rangeRequest
  { Request.headers = [(H.hAccept, "application/octet-stream"), ("X-Test", "fixture"), ("User-Agent", "benchmark")]
  }


redirectScore :: Request.Request () -> IO Int
redirectScore request = do
  let peer = Transport.Transport $ \req ->
        pure $ if URI.requestURIToText (Request.requestURI req) == "https://example.com/download"
          then Raw.RawResponse S.status307 [(H.hLocation, "/other")] (pure "") Protocol.HTTP1_1
          else Raw.RawResponse S.status200 (Request.headers req) (pure "") Protocol.HTTP1_1
      client = Redirect.withRedirects Redirect.defaultRedirectPolicy peer
  reply <- Transport.sendRaw client (fmap (const emptyStream) request)
  pure (fromIntegral (S.statusCode (Raw.statusCode reply)) + length (Raw.headers reply))


decompressionScore :: Request.Request () -> S.Status -> H.Headers -> BS.ByteString -> IO Int
decompressionScore request status fields bytes = do
  let peer = Transport.Transport $ \_ -> do
        popper <- popperFromStrict bytes
        pure (Raw.RawResponse status fields popper Protocol.HTTP1_1)
  reply <- Transport.sendRaw (Compression.withDecompression peer) (fmap (const emptyStream) request)
  payload <- popperBytes (Raw.bodyPopper reply)
  pure (BS.length payload + length (Raw.headers reply))


revalidationScore :: BS.ByteString -> IO Int
revalidationScore gzip = do
  cache <- Cache.newCache Cache.defaultCacheConfig
  let tag = (H.hETag, "\"gzip-revision\"")
      coding = (H.hContentEncoding, "gzip")
      peer = Transport.Transport $ \req ->
        if H.hasHeader H.hIfNoneMatch (Request.headers req)
          then pure (Raw.RawResponse S.status304 [(H.hCacheControl, "max-age=60"), tag, coding] (pure "") Protocol.HTTP1_1)
          else do
            body <- popperFromStrict gzip
            pure (Raw.RawResponse S.status200 [(H.hCacheControl, "max-age=0"), tag, coding] body Protocol.HTTP1_1)
      client = Cache.withCache cache (Compression.withDecompression peer)
      fetch = Transport.sendRaw client (fmap (const emptyStream) ordinaryRedirectRequest)
  original <- fetch
  _ <- popperBytes (Raw.bodyPopper original)
  refreshed <- fetch
  bytes <- popperBytes (Raw.bodyPopper refreshed)
  pure (BS.length bytes + length (Raw.headers refreshed))


payload64k :: BS.ByteString
payload64k = BS.replicate (64 * 1024) 0x61


helloHandler :: Handler
helloHandler _ =
  pure
    Response
      { responseStatus = S.status200
      , responseVersion = V.HTTP1_1
      , responseHeaders = []
      , responseBody = BodyBytes "hello"
      , responseTrailers = pure []
      , responseH2StreamId = 0
      , responseCancel = pure ()
      , responsePushPromises = pure []
      }


echoHandler :: Handler
echoHandler req = do
  body <- drainBody (requestBody req)
  pure
    Response
      { responseStatus = S.status200
      , responseVersion = V.HTTP1_1
      , responseHeaders = []
      , responseBody = BodyBytes body
      , responseTrailers = pure []
      , responseH2StreamId = 0
      , responseCancel = pure ()
      , responsePushPromises = pure []
      }


streamHandler :: Int -> Handler
streamHandler nChunks _ = do
  let chunk = BS.replicate 4096 0x78
  ref <- newIORef nChunks
  pure
    Response
      { responseStatus = S.status200
      , responseVersion = V.HTTP1_1
      , responseHeaders = []
      , responseBody = BodyStream $ do
          remaining <- readIORef ref
          if remaining <= 0
            then pure Nothing
            else do
              writeIORef ref (remaining - 1)
              pure (Just chunk)
      , responseTrailers = pure []
      , responseH2StreamId = 0
      , responseCancel = pure ()
      , responsePushPromises = pure []
      }


doGet :: VersionRange -> String -> V.Version -> IO BS.ByteString
doGet range port ver = do
  let cfg =
        defaultConnectionConfig
          { connectionHost = "127.0.0.1"
          , connectionPort = port
          , connectionVersionRange = range
          , connectionTls = Nothing
          }
  withConnection cfg $ \c -> do
    r <-
      sendOn
        c
        Request
          { requestMethod = methodFromBytes "GET"
          , requestTarget = "/"
          , requestAuthority = Just (BS8.pack ("127.0.0.1:" <> port))
          , requestScheme = SchemeHttp
          , requestHeaders = []
          , requestBody = BodyEmpty
          , requestVersion = ver
          , requestTrailers = pure []
          }
    drainBody (responseBody r)


doPost :: VersionRange -> String -> V.Version -> BS.ByteString -> IO BS.ByteString
doPost range port ver payload = do
  let cfg =
        defaultConnectionConfig
          { connectionHost = "127.0.0.1"
          , connectionPort = port
          , connectionVersionRange = range
          , connectionTls = Nothing
          }
  withConnection cfg $ \c -> do
    r <-
      sendOn
        c
        Request
          { requestMethod = methodFromBytes "POST"
          , requestTarget = "/"
          , requestAuthority = Just (BS8.pack ("127.0.0.1:" <> port))
          , requestScheme = SchemeHttp
          , requestHeaders = []
          , requestBody = BodyBytes payload
          , requestVersion = ver
          , requestTrailers = pure []
          }
    drainBody (responseBody r)


withSetup :: VersionRange -> String -> Handler -> (String -> Benchmark) -> Benchmark
withSetup range name handler mkBench =
  envWithCleanup (startServer range handler) stopServer $ \port ->
    bgroup name [mkBench port]


startServer :: VersionRange -> Handler -> IO String
startServer range handler = do
  readyVar <- newEmptyMVar
  tidVar <- newEmptyMVar
  let hints =
        NS.defaultHints
          { NS.addrFlags = [NS.AI_PASSIVE]
          , NS.addrSocketType = NS.Stream
          }
  addrs <- NS.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  case addrs of
    [] -> error "no addr"
    (addr : _) -> do
      listenSock <- NS.openSocket addr
      NS.setSocketOption listenSock NS.ReuseAddr 1
      NS.bind listenSock (NS.addrAddress addr)
      NS.listen listenSock 128
      bound <- NS.getSocketName listenSock
      let portStr = case bound of
            NS.SockAddrInet p _ -> show (fromIntegral p :: Int)
            _ -> "0"
          cfg =
            defaultServerConfig
              { serverHost = "127.0.0.1"
              , serverPort = portStr
              , serverVersionRange = range
              , serverHandler = handler
              }
      tid <- forkIO $ do
        putMVar readyVar ()
        runServerOnListener cfg listenSock `finally` NS.close listenSock
      putMVar tidVar tid
      takeMVar readyVar
      pure portStr


stopServer :: String -> IO ()
stopServer _ = pure ()


drainBody :: Body -> IO BS.ByteString
drainBody BodyEmpty = pure ""
drainBody (BodyBytes bs) = pure bs
drainBody (BodyStream p) = BS.concat <$> go
  where
    go = do
      mc <- p
      case mc of
        Nothing -> pure []
        Just c -> (c :) <$> go
