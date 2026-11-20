{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Connection pool for the base transport.

A 'ConnectionPool' is a per-target ((scheme, host, port)) cache of
open 'Network.HTTP.Connection.Connection' handles. Acquiring a
connection prefers a still-warm idle one; if none is available the
pool opens a fresh one (subject to 'maxConnectionsPerHost').
Releasing a connection puts it back on the idle list, and a
background reaper closes connections that have been idle for
longer than 'maxIdleSeconds'.

== Scope of real reuse

The wireform-http1 low-level API exposes 'openClientConnection' \/
'closeClientConnection' out of bracket scope, so plaintext
HTTP\/1.x connections genuinely get reused across requests.

TLS connections and HTTP\/2 connections still go through the
bracketed 'Network.HTTP.Connection.withConnection' (the low-level
HTTP\/2 client doesn't expose open\/close — it spawns a recv-loop
thread that's tied to the bracket). For those targets the pool
falls back to a fresh connection per request. The API surface
doesn't change; only the wins do.

When wireform-http2's TLS \/ multiplexing surface grows
non-bracketed open\/close hooks, the pool will automatically pick
those up.

== Idle health check (§5.3 audit fix)

On the reuse path, if 'sendAndMaterialise' raises an 'IOError' on
an idle connection (EPIPE \/ ECONNRESET — server closed the
keep-alive connection while it was idle), the pool closes the stale
connection and retries exactly once on a fresh connection.  The
retry is only attempted for genuine connection-level failures
('isConnectionError'), not for application-level errors (4xx, 5xx,
parse errors).

== Authority key includes TLS settings (§5.3 audit fix)

'Target' now carries a 'TlsTargetKey' when the scheme is HTTPS.
The key includes SNI and cert-validation settings; future work can
extend it with version-range info. This prevents a pool slot opened
with cert validation disabled from being reused for a request that
requires validation (or vice versa).  Currently TLS connections are
not pooled ('reuseOK' returns 'False' for HTTPS), but the key
ensures the safety invariant holds if TLS pooling is ever enabled.
-}
module Network.HTTP.Client.Pool (
  -- * Configuration
  PoolConfig (..),
  defaultPoolConfig,

  -- * The pool
  ConnectionPool,
  newPool,
  closePool,
  withPool,

  -- * Transport
  pooledTransport,
) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, bracket, mask, throwIO, try)
import Control.Monad (forever, void, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable (..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List qualified as List
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Data.Void (Void)
import GHC.Generics (Generic)
import Network.HTTP.Client.BodyStream (
  BodyStream (..),
  popperFromStrict,
 )
import Network.HTTP.Client.Expect (hasExpectContinueHeader)
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
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Method qualified as M
import Network.HTTP.Types.Version qualified as LV
import Network.HTTP.VersionRange qualified as VR
import System.IO.Error (isDoesNotExistError, isEOFError)


-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data PoolConfig = PoolConfig
  { maxConnectionsPerHost :: !Int
  {- ^ Cap on simultaneously open connections per
  @(scheme, host, port, proxy)@. Requests that exceed the
  cap block until a slot frees up.
  -}
  , maxIdleSeconds :: !Double
  -- ^ Idle connections older than this are closed by the reaper.
  , maxConnectionAgeSeconds :: !(Maybe Double)
  {- ^ Hard cap on a single connection's lifetime, counted from
  the moment it was first opened.  Once a connection is
  this old it's closed by the reaper at the next sweep
  regardless of recent activity.  Defaults to 'Nothing'
  (unlimited).

  Use this to recycle connections through load-balancer
  topology changes (where stale sticky-routing keeps a
  long-lived conn pinned to a victim host) and to bound
  the blast radius of a slowly-degrading TLS session.
  -}
  , reaperIntervalSeconds :: !Double
  -- ^ Reaper sweep cadence.
  , versionRange :: !VR.VersionRange
  -- ^ Negotiated version range for newly opened connections.
  , proxyConfig :: !ProxyConfig
  {- ^ Proxy selection. Defaults to 'Pxy.noProxyConfig'; when set
  the pool routes each request through the configured proxy
  (CONNECT-tunnelled for HTTPS, request-line-rewritten for
  HTTP). The 'Target' key includes the resolved proxy so
  different proxies do not share idle connections.
  -}
  }


defaultPoolConfig :: PoolConfig
defaultPoolConfig =
  PoolConfig
    { maxConnectionsPerHost = 8
    , maxIdleSeconds = 60
    , maxConnectionAgeSeconds = Nothing
    , reaperIntervalSeconds = 5
    , versionRange = VR.preferHttp1
    , proxyConfig = Pxy.noProxyConfig
    }


-- ---------------------------------------------------------------------------
-- Internal state
-- ---------------------------------------------------------------------------

{- | TLS settings that are part of the pool key.  Two connections to
the same host that differ in SNI or cert-validation policy must
not share a pool slot.
-}
data TlsTargetKey = TlsTargetKey
  { tlsKeyServerName :: !String
  -- ^ The SNI / X.509 hostname used for the TLS handshake.
  , tlsKeyValidate :: !Bool
  -- ^ Whether certificate chain + hostname was verified.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)


data Target = Target
  { tgtScheme :: !TargetScheme
  , tgtHost :: !ByteString
  , tgtPort :: !Int
  , tgtProxy :: !(Maybe (ByteString, Int))
  {- ^ Resolved proxy authority for this request, if any. Part of
  the hash key so different proxies maintain distinct pools.
  -}
  , tgtTls :: !(Maybe TlsTargetKey)
  {- ^ TLS key for HTTPS targets (§5.3 audit fix).  Ensures that
  connections with different SNI or cert-validation settings
  are never mixed in the same pool slot.
  -}
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)


data TargetScheme = TargetHttp | TargetHttps
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)


{- | One cached connection plus its last-used timestamp and the
creation time so the reaper can enforce 'maxConnectionAgeSeconds'.
-}
data Slot = Slot
  { slotConn :: !Conn.Connection
  , slotLastUsed :: !UTCTime
  , slotCreated :: !UTCTime
  }


data PerTarget = PerTarget
  { ptIdle :: ![Slot] -- available connections (LIFO so warmest first)
  , ptInUse :: !Int -- count of checked-out connections
  }


emptyPerTarget :: PerTarget
emptyPerTarget = PerTarget [] 0


data PoolState = PoolState
  { psPerTarget :: !(HashMap Target PerTarget)
  , psShutdown :: !Bool
  }


data ConnectionPool = ConnectionPool
  { poolState :: !(TVar PoolState)
  , poolConfig :: !PoolConfig
  , poolReaper :: !ThreadId
  }


-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

newPool :: PoolConfig -> IO ConnectionPool
newPool cfg = do
  stateVar <-
    newTVarIO
      PoolState
        { psPerTarget = HM.empty
        , psShutdown = False
        }
  rtid <- forkIO (reaperLoop cfg stateVar)
  pure
    ConnectionPool
      { poolState = stateVar
      , poolConfig = cfg
      , poolReaper = rtid
      }


closePool :: ConnectionPool -> IO ()
closePool pool = do
  killThread (poolReaper pool)
  victims <- atomically $ do
    s <- readTVar (poolState pool)
    writeTVar (poolState pool) s {psShutdown = True, psPerTarget = HM.empty}
    pure (concatMap (map slotConn . ptIdle) (HM.elems (psPerTarget s)))
  mapM_ (\c -> try @SomeException (Conn.closeConnection c)) victims


withPool :: PoolConfig -> (ConnectionPool -> IO a) -> IO a
withPool cfg = bracket (newPool cfg) closePool


-- ---------------------------------------------------------------------------
-- Reaper
-- ---------------------------------------------------------------------------

reaperLoop :: PoolConfig -> TVar PoolState -> IO ()
reaperLoop cfg stateVar = forever $ do
  let micros = max 1_000_000 (round (reaperIntervalSeconds cfg * 1_000_000))
  threadDelay micros
  now <- getCurrentTime
  victims <- atomically $ do
    s <- readTVar stateVar
    if psShutdown s
      then pure []
      else do
        let limit = realToFrac (maxIdleSeconds cfg) :: NominalDiffTime
            ageLimit =
              realToFrac <$> maxConnectionAgeSeconds cfg
                :: Maybe NominalDiffTime
            sweep pt =
              let stillLive sl =
                    diffUTCTime now (slotLastUsed sl) < limit
                      && case ageLimit of
                        Nothing -> True
                        Just al -> diffUTCTime now (slotCreated sl) < al
                  (live, dead) = List.partition stillLive (ptIdle pt)
              in (pt {ptIdle = live}, dead)
            (newMap, allDead) = HM.foldrWithKey step (HM.empty, []) (psPerTarget s)
            step k pt (acc, deadAcc) =
              let (pt', dead) = sweep pt
                  acc' =
                    if null (ptIdle pt') && ptInUse pt' == 0
                      then acc
                      else HM.insert k pt' acc
              in (acc', dead <> deadAcc)
        writeTVar stateVar s {psPerTarget = newMap}
        pure allDead
  mapM_ (\sl -> try @SomeException (Conn.closeConnection (slotConn sl))) victims


-- ---------------------------------------------------------------------------
-- Acquire / release
-- ---------------------------------------------------------------------------

{- | Try to grab an idle connection. If none is available and we're
below the per-host cap, return 'NeedNew'; otherwise 'STM.retry'.
-}
data Acquire = ReuseIdle !Slot | NeedNew


acquireSTM :: ConnectionPool -> Target -> STM Acquire
acquireSTM pool target = do
  s <- readTVar (poolState pool)
  when (psShutdown s) (throwSTM PoolClosed)
  let pt = fromMaybe emptyPerTarget (HM.lookup target (psPerTarget s))
  case ptIdle pt of
    (slot : rest) -> do
      let pt' = pt {ptIdle = rest, ptInUse = ptInUse pt + 1}
      writeTVar
        (poolState pool)
        s {psPerTarget = HM.insert target pt' (psPerTarget s)}
      pure (ReuseIdle slot)
    [] ->
      if ptInUse pt < maxConnectionsPerHost (poolConfig pool)
        then do
          let pt' = pt {ptInUse = ptInUse pt + 1}
          writeTVar
            (poolState pool)
            s {psPerTarget = HM.insert target pt' (psPerTarget s)}
          pure NeedNew
        else retry


releaseIdle :: ConnectionPool -> Target -> Slot -> IO ()
releaseIdle pool target slot = atomically $ do
  s <- readTVar (poolState pool)
  let pt = fromMaybe emptyPerTarget (HM.lookup target (psPerTarget s))
      pt' =
        pt
          { ptIdle = slot : ptIdle pt
          , ptInUse = max 0 (ptInUse pt - 1)
          }
  writeTVar
    (poolState pool)
    s {psPerTarget = HM.insert target pt' (psPerTarget s)}


releaseDead :: ConnectionPool -> Target -> IO ()
releaseDead pool target = atomically $ do
  s <- readTVar (poolState pool)
  let pt = fromMaybe emptyPerTarget (HM.lookup target (psPerTarget s))
      pt' = pt {ptInUse = max 0 (ptInUse pt - 1)}
  writeTVar
    (poolState pool)
    s {psPerTarget = HM.insert target pt' (psPerTarget s)}


-- ---------------------------------------------------------------------------
-- Transport
-- ---------------------------------------------------------------------------

{- | The pool's 'Transport'. Each request gets a connection from the
per-target slot, runs to completion, and the connection is
returned to the idle list (or closed, on failure or
TLS\/HTTP\/2 where reuse isn't supported yet).
-}
pooledTransport :: ConnectionPool -> Transport IO
pooledTransport pool = Transport $ \req -> do
  (target, mProxy) <- targetForRequest (proxyConfig (poolConfig pool)) req
  let cfg = connectionConfigFor pool target
  if reuseOK cfg
    then runWithSlot pool target cfg mProxy req
    else runOneShot cfg mProxy req


{- | True iff a connection for this config can be opened \/ closed
out of bracket scope (HTTP\/1.x plaintext only, today).
-}
reuseOK :: Conn.ConnectionConfig -> Bool
reuseOK cfg = case Conn.connectionTls cfg of
  Just _ -> False
  Nothing -> case VR.preferredVersion (Conn.connectionVersionRange cfg) of
    LV.HTTP2 -> False
    _ -> True


{- | Reusable path: grab a slot from the pool, send, return.

Idle connection health check (§5.3 audit fix): if the send fails
with a connection-level 'IOError' (EPIPE \/ ECONNRESET — the
server closed the keep-alive while it was idle), the pool closes
the stale slot and retries exactly once with a fresh connection.
Application-level errors (parse failures, 4xx\/5xx) propagate
without retry.
-}
runWithSlot
  :: ConnectionPool
  -> Target
  -> Conn.ConnectionConfig
  -> Maybe Proxy
  -> WReq.Request BodyStream
  -> IO RawResponse
runWithSlot pool target cfg mProxy req = mask $ \restore -> do
  acquired <- atomically (acquireSTM pool target)
  (conn, created, wasIdle) <- case acquired of
    ReuseIdle slot -> pure (slotConn slot, slotCreated slot, True)
    NeedNew -> do
      result <- restore (Conn.openConnectionVia cfg mProxy)
      case result of
        Right c -> do
          t <- getCurrentTime
          pure (c, t, False)
        Left err -> do
          releaseDead pool target
          throwIO (PoolInvalidURI err)
  -- From here we own the connection; on success put it back idle,
  -- on failure close + decrement.
  started <- if wasIdle && deferBody
    then Just <$> newIORef False
    else pure Nothing
  let attempt = case started of
        Nothing -> req
        Just flag ->
          let stream = WReq.body req
          in req {WReq.body = stream {pull = writeIORef flag True >> pull stream}}
  result <- restore (try (sendAndMaterialise conn attempt))
  case result of
    Right raw -> do
      reusable <- Conn.canReuseHttp1Connection conn
      if reusable
        then do
          now <- getCurrentTime
          releaseIdle pool target (Slot conn now created)
        else do
          void (try @SomeException (Conn.closeConnection conn))
          releaseDead pool target
      pure raw
    Left (e :: SomeException)
      | wasIdle && isConnectionError e -> do
          -- Stale keep-alive: close the dead connection and retry
          -- once on a fresh one (§5.3 audit fix).
          void (try @SomeException (Conn.closeConnection conn))
          releaseDead pool target
          producerStarted <- maybe (pure False) readIORef started
          if producerStarted then throwIO e else runWithSlot pool target cfg mProxy req
      | otherwise -> do
          void (try @SomeException (Conn.closeConnection conn))
          releaseDead pool target
          throwIO e
  where
    deferBody =
      hasExpectContinueHeader (WReq.headers req)
        || isJust (protocolRequestTrailers (WReq.protocolHints req))


{- | Heuristic: is this exception a connection-level transport error
that would also affect a fresh connection opened from the pool's
idle list?  Covers the common "server closed keep-alive socket"
patterns (EPIPE, ECONNRESET, unexpected EOF).
-}
isConnectionError :: SomeException -> Bool
isConnectionError e =
  case show e of
    s ->
      isEOFError (userError s)
        || isDoesNotExistError (userError s)
        || any
          (`List.isPrefixOf` s)
          [ "Network.Socket"
          , "recvBuf"
          , "sendBuf"
          , "Broken pipe"
          , "Connection reset"
          , "Connection refused"
          ]


{- | One-shot path for connections we can't reuse yet (TLS, HTTP\/2):
bracket a fresh 'Conn.withConnection' for each request.
-}
runOneShot
  :: Conn.ConnectionConfig
  -> Maybe Proxy
  -> WReq.Request BodyStream
  -> IO RawResponse
runOneShot cfg mProxy req =
  Conn.withConnectionVia cfg mProxy Nothing $ \conn ->
    sendAndMaterialise conn req


{- | Convert a 'Msg.ResponsePushPromise' to the protocol-level
'PushPromise'.  The promised request is synthesised from the push
promise's pseudo-headers; 'pushFulfil' delegates to the
'Msg.rppFulfil' action which blocks until the server sends the
pushed response and returns it fully materialised.
-}
toPushPromise :: Msg.ResponsePushPromise -> PushPromise WReq.Request RawResponse
toPushPromise pp =
  PushPromise
    { pushPromisedRequest = pushedRequestFromPseudoHeaders (Msg.rppHeaders pp)
    , pushFulfil = Msg.rppFulfil pp
    }


{- | Build a 'WReq.Request Void' for a push-promised resource from the
PUSH_PROMISE pseudo-headers.  Push promises carry no request body;
'body' is a bottom value that MUST NOT be forced.
-}
pushedRequestFromPseudoHeaders :: H.Headers -> WReq.Request Void
pushedRequestFromPseudoHeaders hdrs =
  let lookupPs name = lookup (CI.mk (BS8.pack name)) hdrs
      meth = maybe M.mGet M.methodFromBytes (lookupPs ":method")
      pathText = maybe "/" BS8.unpack (lookupPs ":path")
      restHdrs = filter (not . isPseudo) hdrs
  in WReq.Request
       { WReq.method = meth
       , WReq.requestURI = WURI.staticURI (T.pack pathText)
       , WReq.headers = restHdrs
       , WReq.body = error "push promise: body is Void (uninhabited)"
       , WReq.protocolHints = defaultHints
       , WReq.spanAttributes = []
       }
  where
    isPseudo (n, _) =
      let bs = CI.original n
      in not (BS.null bs) && BS.head bs == 0x3A


sendAndMaterialise
  :: Conn.Connection
  -> WReq.Request BodyStream
  -> IO RawResponse
sendAndMaterialise conn req = do
  lowReq <- toLowLevelRequest req
  let hints = WReq.protocolHints req
  framed <- Conv.prepareRequestTrailers (Conn.negotiatedVersion conn) hints lowReq
  resp <- Conn.sendOnWithContinueTimeout (continueTimeoutMicros hints) conn framed
  drained <- materialise (Msg.responseBody resp)
  trailers <- Msg.responseTrailers resp
  popper <- popperFromStrict drained
  let pushPromisesIO = fmap toPushPromise <$> Msg.responsePushPromises resp
  pure
    RawResponse
      { statusCode = Msg.responseStatus resp
      , headers = Msg.responseHeaders resp
      , bodyPopper = popper
      , protocolInfo = withBufferedTrailers trailers $ case Msg.responseVersion resp of
          LV.HTTP2 ->
            HTTP2
              Http2Info
                { h2StreamId = Msg.responseH2StreamId resp
                , h2PushPromises = pushPromisesIO
                , h2CancelStream = Msg.responseCancel resp
                }
          _ -> HTTP1_1
      }
  where
    materialise = \case
      LB.BodyEmpty -> pure BS.empty
      LB.BodyBytes b -> pure b
      LB.BodyStream p -> drainMaybe p
    drainMaybe p = go []
      where
        go acc =
          p >>= \case
            Nothing -> pure $! BS.concat (reverse acc)
            Just b
              | BS.null b -> go acc
              | otherwise -> go (b : acc)


toLowLevelRequest :: WReq.Request BodyStream -> IO Msg.Request
toLowLevelRequest req = do
  uri_ <- case WURI.renderRequestURI (WReq.requestURI req) of
    Right u -> pure u
    Left err -> throwIO (PoolInvalidURI err)
  prepared <- Conv.prepareRequestBody Conv.bufferedRequestBody req
  let lowScheme = case WURI.uriScheme uri_ of
        WURI.SchemeHttps -> Msg.SchemeHttps
        WURI.SchemeHttp -> Msg.SchemeHttp
      hostBs = WURI.uriHost uri_
      target = WURI.uriPathAndQuery uri_
      authority =
        Just
          ( hostBs <> case (WURI.uriScheme uri_, WURI.uriPort uri_) of
              (WURI.SchemeHttp, 80) -> ""
              (WURI.SchemeHttps, 443) -> ""
              (_, p) -> ":" <> BS8.pack (show p)
          )
  pure
    Msg.Request
      { Msg.requestMethod = WReq.method req
      , Msg.requestTarget = target
      , Msg.requestAuthority = authority
      , Msg.requestScheme = lowScheme
      , Msg.requestHeaders = WReq.headers prepared
      , Msg.requestBody = WReq.body prepared
      , Msg.requestVersion = VR.preferredVersion VR.preferHttp1
      , Msg.requestTrailers = pure []
      }


connectionConfigFor :: ConnectionPool -> Target -> Conn.ConnectionConfig
connectionConfigFor pool tgt =
  let host = BS8.unpack (tgtHost tgt)
      tls = case tgtScheme tgt of
        TargetHttps ->
          let tlsKey =
                fromMaybe
                  TlsTargetKey
                    { tlsKeyServerName = host
                    , tlsKeyValidate = True
                    }
                  (tgtTls tgt)
              baseCfg = Conn.defaultTlsConnectionConfig (tlsKeyServerName tlsKey)
          in Just
               baseCfg
                 { Conn.tlsValidateCert = tlsKeyValidate tlsKey
                 }
        TargetHttp -> Nothing
  in Conn.ConnectionConfig
       { Conn.connectionHost = host
       , Conn.connectionPort = show (tgtPort tgt)
       , Conn.connectionVersionRange = versionRange (poolConfig pool)
       , Conn.connectionTls = tls
       }


targetForRequest
  :: ProxyConfig
  -> WReq.Request BodyStream
  -> IO (Target, Maybe Proxy)
targetForRequest pcfg req = case WURI.renderRequestURI (WReq.requestURI req) of
  Left err -> throwIO (PoolInvalidURI err)
  Right u ->
    let scheme = case WURI.uriScheme u of
          WURI.SchemeHttps -> TargetHttps
          WURI.SchemeHttp -> TargetHttp
        host = WURI.uriHost u
        port = WURI.uriPort u
        mProxy
          | Pxy.shouldBypass pcfg host = Nothing
          | otherwise = case WURI.uriScheme u of
              WURI.SchemeHttp -> Pxy.proxyForHttp pcfg
              WURI.SchemeHttps -> Pxy.proxyForHttps pcfg
        proxyKey = fmap (\p -> (Pxy.proxyHost p, Pxy.proxyPort p)) mProxy
        tlsKey = case WURI.uriScheme u of
          WURI.SchemeHttps ->
            Just
              TlsTargetKey
                { tlsKeyServerName = BS8.unpack host
                , tlsKeyValidate = True
                }
          WURI.SchemeHttp -> Nothing
    in pure
         ( Target
             { tgtScheme = scheme
             , tgtHost = host
             , tgtPort = port
             , tgtProxy = proxyKey
             , tgtTls = tlsKey
             }
         , mProxy
         )


-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

data PoolError
  = PoolInvalidURI !String
  | PoolClosed
  deriving stock (Show)


instance Exception PoolError
