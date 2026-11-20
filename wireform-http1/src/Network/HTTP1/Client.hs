{- | HTTP\/1.x client.

Two primary modes:

* 'sendRequest' / 'sendRequestOn' — one request on a caller-supplied
  'Connection'. Pipelining is the caller's responsibility (issue
  N requests, then read N responses; ordered by the server per RFC
  9112 § 9.3.2).
* 'withClientConnection' — bracket pattern: open a TCP connection,
  run an action, close it.

For TLS build the 'Connection' via 'newConnectionFromTls' (passing a
handshaked 'Wireform.Network.TLS.OpenSSL.SslConn') and call
'sendRequestOn' yourself; the high-level helpers here are TCP-only.

A pooled variant lives in "Network.HTTP1.Client.Pool".
-}
module Network.HTTP1.Client (
  ClientConfig (..),
  defaultClientConfig,
  ClientConnection (..),
  openClientConnection,
  closeClientConnection,
  withClientConnection,

  -- * Per-request API
  sendRequest,
  sendRequestOn,
  sendRequestOnWithExpect,
  sendRequestOnWithContinueTimeout,

  -- * Connection introspection
  clientConnectionSocket,
  clientConnectionCanReuse,
) where

import Control.Exception (bracket, bracketOnError, catch, mask_, onException)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP1.Connection
import Network.HTTP1.Encode (requestBuilder)
import Network.HTTP1.Parser
import Network.HTTP1.StreamingReader qualified as SR
import Network.HTTP1.Types
import Network.Socket (Socket)
import Network.Socket qualified as NS
import System.Timeout (timeout)
import Wireform.Builder qualified as B
import Wireform.Transport.Send (sendBuilderDirect, sendByteString)


data ClientConfig = ClientConfig
  { clientHost :: !String
  , clientPort :: !String
  , clientMaxHeaderBytes :: !Int
  }


defaultClientConfig :: ClientConfig
defaultClientConfig =
  ClientConfig
    { clientHost = "127.0.0.1"
    , clientPort = "80"
    , clientMaxHeaderBytes = 32 * 1024
    }


{- | Opaque handle for a client-side HTTP\/1.x connection.  Construct
via 'openClientConnection' (TCP) or by wrapping a
'newConnectionFromTls' yourself (TLS).
-}
newtype ClientConnection = ClientConnection {unClientConnection :: Connection}


{- | The underlying socket if the connection has one (TCP and TLS both
do; the TLS variant deliberately doesn't expose it through the
sendfile fast path).
-}
clientConnectionSocket :: ClientConnection -> Maybe Socket
clientConnectionSocket (ClientConnection c) = connectionSocket c


-- | Whether the protocol permits reuse after the current response is drained.
clientConnectionCanReuse :: ClientConnection -> IO Bool
clientConnectionCanReuse (ClientConnection conn) = connectionCanReuse conn


openClientConnection :: ClientConfig -> IO ClientConnection
openClientConnection cfg = do
  let hints = NS.defaultHints {NS.addrSocketType = NS.Stream}
  addrs <- NS.getAddrInfo (Just hints) (Just (clientHost cfg)) (Just (clientPort cfg))
  case addrs of
    [] -> error "wireform-http1: no address found"
    (addr : _) -> bracketOnError (NS.openSocket addr) NS.close $ \sock -> do
      NS.connect sock (NS.addrAddress addr)
      NS.setSocketOption sock NS.NoDelay 1
      ClientConnection <$> newConnectionFromOwnedSocket sock


closeClientConnection :: ClientConnection -> IO ()
closeClientConnection (ClientConnection c) = closeConnection c


withClientConnection :: ClientConfig -> (ClientConnection -> IO a) -> IO a
withClientConnection cfg = bracket (openClientConnection cfg) closeClientConnection


{- | Send a single request on a freshly-opened connection and return
the response. The connection is closed afterwards.

The response body is fully read and materialised as a
'BodyBytes' before the connection is torn down — otherwise the
magic ring the body producer reads from would be freed by the
'closeClientConnection' that the inner bracket runs, and the
producer's subsequent reads would access unmapped memory.
Applications that need to stream large response bodies should
use 'sendRequestOn' with their own 'withClientConnection'
scope so the connection (and its ring) stays alive while the
body is being consumed.
-}
sendRequest :: ClientConfig -> Request -> IO (Either ParseError Response)
sendRequest cfg req = withClientConnection cfg $ \conn -> do
  r <- sendRequestOn conn req
  case r of
    Left e -> pure (Left e)
    Right resp -> materialize resp `catch` \(ProtocolException e) -> pure (Left e)
  where
    materialize resp = do
      bs <- collectBody (responseBody resp)
      trs <- responseTrailers resp
      pure $
        Right
          resp
            { responseBody = BodyBytes bs
            , responseTrailers = pure trs
            }


-- | Drain a 'Body' into a single contiguous 'ByteString'.
collectBody :: Body -> IO ByteString
collectBody = \case
  BodyEmpty -> pure BS.empty
  BodyBytes bs -> pure bs
  BodyPreEncoded _ -> pure BS.empty
  BodyFile _ -> pure BS.empty
  BodyStream producer -> loop []
    where
      loop acc = do
        mc <- producer
        case mc of
          Nothing -> pure (BS.concat (reverse acc))
          Just c -> loop (c : acc)


{- | Send a request on an existing connection (for keep-alive \/
pipelining). The returned response's body is a streaming producer;
you MUST consume it (or call 'drainBody') before sending another
request on the same connection — otherwise the next response will
be misframed. A close-delimited response ends the connection when
the body reaches EOF.
-}
sendRequestOn :: ClientConnection -> Request -> IO (Either ParseError Response)
sendRequestOn = sendRequestOnWithContinueTimeout 1_000_000


-- | Like 'sendRequestOn', with a per-request continue timeout in microseconds.
sendRequestOnWithContinueTimeout :: Int -> ClientConnection -> Request -> IO (Either ParseError Response)
sendRequestOnWithContinueTimeout timeoutUs client req
  | requestVersion req == HTTP_1_1
  , requestCarriesBody req
  , expectsContinue (requestHeaders req) =
      sendRequestOnWithExpect client req timeoutUs
  | otherwise = sendImmediately client req


sendImmediately :: ClientConnection -> Request -> IO (Either ParseError Response)
sendImmediately client@(ClientConnection conn) req = do
  recordRequestOptions conn req
  case requestBody req of
    BodyEmpty -> sendHead >> readHead
    BodyBytes bs
      | BS.null bs -> sendHead >> readHead
      | otherwise -> do
          withConnectionCork conn $ \corked -> do
            sendBuilderDirect corked (requestBuilder req)
            sendByteString corked bs
          readHead
    BodyStream producer -> case requestStreamFraming req of
      Left err -> pure (Left err)
      Right NoBody | requestVersion req == HTTP_1_0 -> do
        bytes <- bufferStream producer mempty
        sendImmediately client req {requestBody = BodyBytes bytes}
      Right framing -> do
        sendHead
        sendRequestStream conn req framing producer >>= either (pure . Left) (const readHead)
    BodyPreEncoded _ -> readHead
    BodyFile _ -> readHead
  where
    sendHead = connectionSendBuilderDirect conn (requestBuilder req)
    readHead = readFinal conn (requestMethod req)

    bufferStream producer !builder = producer >>= \case
      Nothing -> pure (B.toStrictByteString builder)
      Just chunk -> bufferStream producer (builder <> B.byteString chunk)


{- | Two-stage @Expect: 100-continue@ send (RFC 9110 §10.1.1).

Sends the request headers first, then waits up to @timeoutUs@
microseconds for @100 Continue@ or a final response.

* After @100 Continue@, the client sends the body and reads the final
  response. It skips other informational responses during the wait.
* An early final response returns without running the body producer
  and retires the connection from reuse.
* If @timeoutUs@ elapses, the body is
  sent unconditionally, as permitted by RFC 9110 §10.1.1.

'sendRequestOn' calls this automatically for HTTP/1.1 requests that
carry a body and a @100-continue@ expectation.
-}
sendRequestOnWithExpect
  :: ClientConnection
  -> Request
  -> Int
  -- ^ timeout in microseconds (e.g. 1_000_000 for 1 s)
  -> IO (Either ParseError Response)
sendRequestOnWithExpect client req _
  | requestVersion req /= HTTP_1_1 || not (requestCarriesBody req) = sendImmediately client req
sendRequestOnWithExpect client@(ClientConnection conn) req timeoutUs = case requestBody req of
  BodyStream producer -> case requestStreamFraming req of
    Left err -> pure (Left err)
    Right (ContentLength 0) -> sendImmediately client req
    Right framing -> handshake (sendRequestStream conn req framing producer)
  BodyBytes bytes -> handshake (Right <$> connectionSendBytes conn bytes)
  _ -> sendImmediately client req
  where
    handshake sendBody = do
      recordRequestOptions conn req
      connectionSendBuilderDirect conn (requestBuilder req)
      mInterim <- waitForContinue conn (requestMethod req) timeoutUs
      let sendBodyAndReadFinal =
            sendBody >>= either (pure . Left) (const (readFinal conn (requestMethod req)))
      case mInterim of
        Nothing -> sendBodyAndReadFinal
        Just (Left failure) -> pure (Left (headParseError failure))
        Just (Right (resp0, framing))
          | responseStatus resp0 == Status 100 -> sendBodyAndReadFinal
          | otherwise -> do
              retireConnection conn
              finishResponse conn resp0 framing


requestStreamFraming :: Request -> Either ParseError Framing
requestStreamFraming req =
  requestFraming (requestMethod req) (requestVersion req) (requestHeaders req)


sendRequestStream :: Connection -> Request -> Framing -> IO (Maybe ByteString) -> IO (Either ParseError ())
sendRequestStream conn req framing producer =
  writeBody `onException` retireConnection conn
  where
    writeBody = case framing of
      ContentLength size -> fixed size
      _ -> Right <$> writeChunkedBody conn producer (requestTrailers req)

    fixed remaining = producer >>= \case
      Nothing | remaining == 0 -> pure (Right ())
      Just chunk | let size = fromIntegral (BS.length chunk), size <= remaining -> do
        connectionSendBytes conn chunk
        fixed (remaining - size)
      _ -> do
        retireConnection conn
        pure (Left ParseInvalidLength)


requestCarriesBody :: Request -> Bool
requestCarriesBody req = case requestBody req of
  BodyBytes bytes -> not (BS.null bytes)
  BodyStream _ -> True
  _ -> False


waitForContinue :: Connection -> Method -> Int -> IO (Maybe (Either SR.ReadError (Response, Framing)))
waitForContinue _ _ timeoutUs | timeoutUs <= 0 = pure Nothing
waitForContinue conn method timeoutUs = do
  start <- getMonotonicTimeNSec
  observed <- newIORef Nothing
  -- A blocking receive is interruptible. Keep the completed head and
  -- cursor update together even if the timer fires as parsing finishes.
  let wait = do
        headResult <- readResponseHead conn method
        case headResult of
          Right (response, _)
            | let status = responseStatus response
            , isInterim status
            , status /= Status 100 -> do
                now <- getMonotonicTimeNSec
                when ((now - start) `quot` 1000 < fromIntegral timeoutUs) wait
          _ -> writeIORef observed (Just headResult)
  _ <- timeout timeoutUs (mask_ wait)
  readIORef observed


recordRequestOptions :: Connection -> Request -> IO ()
recordRequestOptions conn req =
  when (ConnClose `elem` connectionOptions (requestHeaders req)) (retireConnection conn)


-- | Read response heads, skipping interim responses.
readFinal :: Connection -> Method -> IO (Either ParseError Response)
readFinal conn method = do
  hE <- readResponseHead conn method
  case hE of
    Left failure -> pure (Left (headParseError failure))
    Right (resp0, framing)
      | isInterim (responseStatus resp0) -> readFinal conn method
      | otherwise -> finishResponse conn resp0 framing


-- Switching Protocols ends the HTTP exchange.
isInterim :: Status -> Bool
isInterim (Status code) = code >= 100 && code < 200 && code /= 101


headParseError :: SR.ReadError -> ParseError
headParseError = \case
  SR.ReadUnexpectedEof -> ParseUnexpectedEof
  SR.ReadMessageTooLong _ -> ParseMessageTooLong
  SR.ReadTransportError _ -> ParseUnexpectedEof
  SR.ReadParse failure -> failure


finishResponse :: Connection -> Response -> Framing -> IO (Either ParseError Response)
finishResponse conn response framing = do
  (body, trailers) <- readBodyAndTrailers conn framing
  pure (Right response {responseBody = body, responseTrailers = trailers})
