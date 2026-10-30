{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Client.Internal.Heartbeat
Description : Consumer group heartbeat background thread
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module implements the heartbeat background thread for consumer groups.

The heartbeat thread:
- Sends periodic Heartbeat requests to the group coordinator
- Detects session expiration or coordinator failures
- Triggers rebalance when needed
- Runs independently of the main consumer poll loop

Heartbeat interval should be less than session timeout (typically 1/3).
If heartbeats fail or stop, the consumer is removed from the group.
-}
module Kafka.Client.Internal.Heartbeat (
  -- * Heartbeat State
  HeartbeatState (..),
  createHeartbeatState,

  -- * Heartbeat Thread
  startHeartbeatThread,
  stopHeartbeatThread,

  -- * Heartbeat Operations
  sendHeartbeat,
  HeartbeatOutcome (..),
  applyHeartbeatOutcome,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (void, when)
import Data.IORef (
  IORef,
  atomicModifyIORef',
  newIORef,
  readIORef,
  writeIORef,
 )
import Data.Int (Int16, Int32)
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Client.Internal.Request qualified as Req
import Kafka.Network.Connection (BrokerAddress (..), Connection)
import Kafka.Network.Connection qualified as Conn
import Kafka.Protocol.ApiVersions qualified as AV
import Kafka.Telemetry.Log qualified as KLog
import "wireform-kafka-protocol" Kafka.Protocol.Generated.HeartbeatRequest qualified as HBReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.HeartbeatResponse qualified as HBResp
import "wireform-kafka-protocol" Kafka.Protocol.Primitives qualified as P
import "wireform-kafka-protocol" Kafka.Protocol.Wire.Codec qualified as WC


-- | State for the heartbeat thread
data HeartbeatState = HeartbeatState
  { hbGroupId :: !Text
  -- ^ Consumer group ID
  , hbMemberId :: !(IORef Text)
  {- ^ Current member ID (updated after join). Tier 3 of
  STM-replacement: single-writer (the subscribe / heartbeat
  loop) / multi-reader (poll, heartbeat) handoff slot;
  'IORef' is sufficient.
  -}
  , hbGenerationId :: !(IORef Int32)
  {- ^ Current generation ID (updated after join). Same Tier 3
  single-writer / multi-reader pattern.
  -}
  , hbCoordinatorAddr :: !(IORef (Maybe BrokerAddress))
  -- ^ Group coordinator address. Tier 3.
  , hbIntervalMs :: !Int
  -- ^ Heartbeat interval in milliseconds
  , hbConnManager :: !Conn.ConnectionManager
  {- ^ Connection manager (kept around for the lock map even
  though heartbeat owns its own dedicated socket below)
  -}
  , hbDedicatedConn :: !(IORef (Maybe (BrokerAddress, Conn.Connection)))
  {- ^ Heartbeat owns its own coordinator socket, separate
  from the cached connection the foreground subscribe /
  commit / poll path uses.  Sharing a single socket
  between threads breaks 'Network.Connection''s internal
  buffer model: the foreground side leaves byte residue
  inside the buffer (e.g. unconsumed tagged-fields
  trailers on OffsetFetch v8 responses) that the
  heartbeat thread then reads as its own response and
  misframes.  A dedicated socket isolates the two.
  -}
  , hbVersionCache :: !AV.ApiVersionCache
  -- ^ Version cache for API negotiation
  , hbClientId :: !Text
  -- ^ Client ID
  , hbCorrelationId :: !(IORef Int32)
  {- ^ Next correlation ID. SPSC counter (heartbeat thread is
  sole reader/writer); 'IORef' + 'atomicModifyIORef\'' is
  sufficient and avoids the per-tick STM commit overhead.
  -}
  , hbRunning :: !(TVar Bool)
  -- ^ Whether the heartbeat thread should keep running
  , hbNeedsRebalance :: !(TVar Bool)
  -- ^ Whether a rebalance is needed
  , hbLost :: !(TVar Bool)
  {- ^ Whether the next rejoin should be treated as a
  /lost/ assignment (KIP-415): the broker fenced us
  (UNKNOWN_MEMBER_ID / FENCED_INSTANCE_ID), so any state
  we held for the previously-assigned partitions is
  considered junk and the consumer's
  'RebalanceListener.rlOnLost' fires instead of
  'rlOnRevoked'. Cleared by 'subscribe' once the lost
  signal has been observed.
  -}
  }


-- | Create a new heartbeat state
createHeartbeatState
  :: Text
  -- ^ Group ID
  -> Int
  -- ^ Heartbeat interval (ms)
  -> Conn.ConnectionManager
  -- ^ Connection manager
  -> AV.ApiVersionCache
  -- ^ Version cache
  -> Text
  -- ^ Client ID
  -> IO HeartbeatState
createHeartbeatState groupId intervalMs connMgr versionCache clientId = do
  memberId <- newIORef ""
  genId <- newIORef (-1)
  coordAddr <- newIORef Nothing
  dedicatedConn <- newIORef Nothing
  corrId <- newIORef 0
  running <- newTVarIO True
  needsRebal <- newTVarIO False
  lost <- newTVarIO False

  return
    HeartbeatState
      { hbGroupId = groupId
      , hbMemberId = memberId
      , hbGenerationId = genId
      , hbCoordinatorAddr = coordAddr
      , hbIntervalMs = intervalMs
      , hbConnManager = connMgr
      , hbDedicatedConn = dedicatedConn
      , hbVersionCache = versionCache
      , hbClientId = clientId
      , hbCorrelationId = corrId
      , hbRunning = running
      , hbNeedsRebalance = needsRebal
      , hbLost = lost
      }


-- | Start the heartbeat background thread
startHeartbeatThread :: HeartbeatState -> IO (Async ())
startHeartbeatThread state = async $ heartbeatLoop state


{- | Stop the heartbeat thread gracefully and tear down its
dedicated coordinator socket (if any).
-}
stopHeartbeatThread :: HeartbeatState -> Async () -> IO ()
stopHeartbeatThread state thread = do
  atomically $ writeTVar (hbRunning state) False
  cancel thread
  cached <- atomicModifyIORef' (hbDedicatedConn state) $ \c -> (Nothing, c)
  case cached of
    Just (_, conn) ->
      void (try (Conn.disconnect conn) :: IO (Either SomeException ()))
    Nothing -> pure ()


{- | Open or reuse the heartbeat thread's dedicated coordinator
connection.  Reopens if the cached entry is for a different
broker (post-coordinator-move) or has been dropped because of
a previous transport error.
-}
ensureHeartbeatConn
  :: HeartbeatState
  -> BrokerAddress
  -> IO (Either String Conn.Connection)
ensureHeartbeatConn HeartbeatState {..} coordAddr = do
  cached <- readIORef hbDedicatedConn
  case cached of
    Just (cachedAddr, c) | cachedAddr == coordAddr -> pure (Right c)
    Just (_, oldConn) -> do
      _ <- try (Conn.disconnect oldConn) :: IO (Either SomeException ())
      openFresh
    Nothing -> openFresh
  where
    openFresh = do
      r <- Conn.connect coordAddr Conn.defaultConnectionConfig
      case r of
        Left err -> pure (Left err)
        Right c -> do
          writeIORef hbDedicatedConn (Just (coordAddr, c))
          pure (Right c)


-- | Main heartbeat loop
heartbeatLoop :: HeartbeatState -> IO ()
heartbeatLoop state@HeartbeatState {..} = do
  shouldRun <- readTVarIO hbRunning
  if not shouldRun
    then return ()
    else do
      -- Tier 3: hbMemberId / hbGenerationId / hbCoordinatorAddr
      -- moved to IORef. Three independent reads instead of one
      -- STM transaction; the only inconsistency that matters is
      -- "did the subscribe path land a fresh memberId between
      -- our reads?", and the worst case is that we fall through
      -- the @Just coordAddr | not (T.null memberId) && genId >= 0@
      -- guard once and try again on the next tick.
      memberId <- readIORef hbMemberId
      genId <- readIORef hbGenerationId
      coordAddrM <- readIORef hbCoordinatorAddr

      case coordAddrM of
        Just coordAddr | not (T.null memberId) && genId >= 0 -> do
          result <- try $ sendHeartbeat state coordAddr memberId genId
          case result of
            Left (e :: SomeException) ->
              KLog.logWarn KLog.kafkaInstrumentationLibrary "consumer group: heartbeat failed" $
                KLog.attrShow "exception.message" e : KLog.serverAttrs (Conn.brokerHost coordAddr) (fromIntegral (Conn.brokerPort coordAddr))
            Right (Left outcome) -> applyHeartbeatOutcome state outcome
            Right (Right needsRebalance) ->
              when needsRebalance $ atomically $ writeTVar hbNeedsRebalance True
        _ -> pure ()

      threadDelay (hbIntervalMs * 1000)
      heartbeatLoop state


{- | Classified rejoin signal returned by a single 'sendHeartbeat'
call. The heartbeat loop pattern-matches on this so it can decide
whether to wipe the cached memberId before the next JoinGroup
(KIP-389: the broker has dropped us, so we must come back with an
empty memberId).
-}
data HeartbeatOutcome
  = {- | Broker returned @UNKNOWN_MEMBER_ID (25)@. KIP-389: clear the
    cached memberId before rejoining.
    -}
    HeartbeatUnknownMember
  | {- | Broker returned @FENCED_INSTANCE_ID (82)@. KIP-345 static
    member that lost its slot to a newer instance. Clear memberId
    and rejoin so we get a fresh slot.
    -}
    HeartbeatFencedInstance
  | {- | Broker returned @ILLEGAL_GENERATION (22)@. The generation
    counter is stale; the memberId is still valid so don't clear
    it, just rejoin to pick up the new generation.
    -}
    HeartbeatIllegalGeneration
  | {- | Any other broker-level error. Treat as "needs rejoin" but
    keep the memberId so the rejoin can be a no-op.
    -}
    HeartbeatOtherError !Int16 !String
  | {- | The request itself failed (network / parse error). The
    memberId is still valid; we'll just retry on the next tick.
    -}
    HeartbeatTransport !String
  deriving (Eq, Show)


describe :: HeartbeatOutcome -> String
describe HeartbeatUnknownMember = "UNKNOWN_MEMBER_ID (KIP-389)"
describe HeartbeatFencedInstance = "FENCED_INSTANCE_ID (KIP-345)"
describe HeartbeatIllegalGeneration = "ILLEGAL_GENERATION"
describe (HeartbeatOtherError ec _) = "broker error " <> show ec
describe (HeartbeatTransport msg) = "transport: " <> msg


{- | Update the in-memory heartbeat state in response to a non-OK
'HeartbeatOutcome'. Always flips 'hbNeedsRebalance' so the next
'poll' re-runs JoinGroup; additionally clears the cached
@memberId@ for UNKNOWN_MEMBER_ID / FENCED_INSTANCE_ID so the
rejoin goes out with an empty memberId (KIP-389 / KIP-345).
Transport-level failures don't touch any state because the broker
still considers us a member.
-}
applyHeartbeatOutcome :: HeartbeatState -> HeartbeatOutcome -> IO ()
applyHeartbeatOutcome HeartbeatState {..} outcome = case outcome of
  HeartbeatTransport _ -> pure ()
  HeartbeatUnknownMember -> do
    atomically $ do
      writeTVar hbNeedsRebalance True
      writeTVar hbLost True
    writeIORef hbMemberId ""
  HeartbeatFencedInstance -> do
    atomically $ do
      writeTVar hbNeedsRebalance True
      writeTVar hbLost True
    writeIORef hbMemberId ""
  HeartbeatIllegalGeneration ->
    atomically $ writeTVar hbNeedsRebalance True
  HeartbeatOtherError _ _ ->
    atomically $ writeTVar hbNeedsRebalance True


{- | Send a single heartbeat to the coordinator. The 'Left' arm
carries a typed 'HeartbeatOutcome' so the heartbeat loop can
branch on UNKNOWN_MEMBER_ID / FENCED_INSTANCE_ID without parsing
string error messages.
-}
sendHeartbeat
  :: HeartbeatState
  -> BrokerAddress
  -- ^ Coordinator address
  -> Text
  -- ^ Member ID
  -> Int32
  -- ^ Generation ID
  -> IO (Either HeartbeatOutcome Bool)
sendHeartbeat hb@HeartbeatState {..} coordAddr memberId genId = do
  -- The heartbeat thread keeps its own dedicated coordinator
  -- socket (see 'hbDedicatedConn' for why).  Open it lazily on
  -- the first tick after the coordinator is known, and reopen
  -- it if the broker resets us.
  connResult <- ensureHeartbeatConn hb coordAddr
  case connResult of
    Left err ->
      return $
        Left $
          HeartbeatTransport $
            "Failed to open heartbeat conn: " ++ err
    Right conn -> do
      corrId <- atomicModifyIORef' hbCorrelationId $ \cid -> (cid + 1, cid)

      let apiKey = 12 -- Heartbeat API key
          clientMaxVersion = 4 -- Max version we support

      -- Query broker's supported version
      brokerVersionM <- atomically $ AV.queryApiVersion hbVersionCache coordAddr apiKey

      let apiVersion = case brokerVersionM of
            Nothing -> 0 -- Fall back to v0 if unknown
            Just range -> case AV.selectVersion clientMaxVersion range of
              Nothing -> 0 -- Fall back if incompatible
              Just v -> v

          request =
            HBReq.HeartbeatRequest
              { HBReq.heartbeatRequestGroupId = P.mkKafkaString hbGroupId
              , HBReq.heartbeatRequestGenerationId = genId
              , HBReq.heartbeatRequestMemberId = P.mkKafkaString memberId
              , HBReq.heartbeatRequestGroupInstanceId = P.KafkaString P.Null
              }

          requestBody = WC.runEncodeVer @HBReq.HeartbeatRequest apiVersion request
          clientIdKafka = P.mkKafkaString hbClientId

      result <-
        Req.sendRequestReceiveResponse
          conn
          apiKey
          apiVersion
          corrId
          clientIdKafka
          requestBody

      case result of
        Left err -> do
          -- Drop the cached dedicated socket on transport
          -- error; it'll be reopened on the next tick.
          writeIORef hbDedicatedConn Nothing
          return $
            Left $
              HeartbeatTransport $
                "Heartbeat request failed: " ++ err
        Right (_, responseBody) -> do
          case WC.runDecodeVer @HBResp.HeartbeatResponse apiVersion responseBody of
            Left err ->
              return $
                Left $
                  HeartbeatTransport $
                    "Failed to parse HeartbeatResponse: " ++ err
            Right response -> do
              let errorCode = HBResp.heartbeatResponseErrorCode response

              case errorCode of
                0 -> return $ Right False -- success
                27 -> return $ Right True -- REBALANCE_IN_PROGRESS
                25 -> return $ Left HeartbeatUnknownMember
                22 -> return $ Left HeartbeatIllegalGeneration
                82 -> return $ Left HeartbeatFencedInstance
                _ ->
                  return $
                    Left $
                      HeartbeatOtherError errorCode $
                        "Heartbeat error code: " ++ show errorCode
