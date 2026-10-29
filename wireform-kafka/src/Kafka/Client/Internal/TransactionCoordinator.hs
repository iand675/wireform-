{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module: Kafka.Client.Internal.TransactionCoordinator
Description: Transaction coordinator discovery and communication

Handles communication with the Kafka transaction coordinator for:
- Coordinator discovery (FindCoordinatorRequest)
- Producer ID initialization (InitProducerIdRequest)
- Transaction lifecycle (AddPartitionsToTxnRequest, EndTxnRequest)
- Offset commits (AddOffsetsToTxnRequest, TxnOffsetCommitRequest)
-}
module Kafka.Client.Internal.TransactionCoordinator (
  TransactionCoordinator (..),
  TransactionCoordinatorError (..),
  interpretCoordinatorError,
  findTransactionCoordinator,
  initProducerId,
  addPartitionsToTxn,
  endTransaction,
  addOffsetsToTxn,
  txnOffsetCommitWith,
  TxnGroupMember (..),

  -- * Pure request builders (exposed for testing)
  buildAddOffsetsToTxnRequest,
  buildTxnOffsetCommitRequest,
) where

import Control.Concurrent.STM (atomically)
import Control.Exception (Exception)
import Data.IORef (IORef, atomicModifyIORef')
import Data.Int (Int16, Int32, Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Kafka.Client.Consumer (TopicPartition (..))
import Kafka.Client.Internal.Request qualified as Req
import Kafka.Network.Connection (BrokerAddress (..))
import Kafka.Network.Connection qualified as Conn
import Kafka.Protocol.ApiVersions qualified as AV
-- Import generated protocol messages

import "wireform-kafka-protocol" Kafka.Protocol.Generated.AddOffsetsToTxnRequest qualified as AOTReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.AddOffsetsToTxnResponse qualified as AOTResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.AddPartitionsToTxnRequest qualified as APTReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.AddPartitionsToTxnResponse qualified as APTResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.EndTxnRequest qualified as ETReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.EndTxnResponse qualified as ETResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.FindCoordinatorRequest qualified as FCReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.FindCoordinatorResponse qualified as FCResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.InitProducerIdRequest qualified as IPReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.InitProducerIdResponse qualified as IPResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.TxnOffsetCommitRequest qualified as TOCReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.TxnOffsetCommitResponse qualified as TOCResp
import "wireform-kafka-protocol" Kafka.Protocol.Primitives qualified as P
import "wireform-kafka-protocol" Kafka.Protocol.Wire.Codec qualified as WC


-- | Transaction coordinator information
data TransactionCoordinator = TransactionCoordinator
  { tcNodeId :: !Int32
  , tcHost :: !Text
  , tcPort :: !Int32
  }
  deriving (Show, Eq)


-- | Transaction coordinator errors
data TransactionCoordinatorError
  = CoordinatorNotAvailable Text
  | CoordinatorLoadInProgress Text
  | NotCoordinator Text
  | InvalidProducerIdMapping Text
  | InvalidProducerEpoch Text
  | InvalidTxnState Text
  | InvalidPartitionsInTxn Text
  | TransactionCoordinatorFenced Text
  | ConcurrentTransactions Text
  | ProducerFenced Text
  | UnknownCoordinatorError Int16 Text
  deriving (Show, Eq)


instance Exception TransactionCoordinatorError


{- | Interpret Kafka error codes into TransactionCoordinatorError.

Mapping cross-checked against
@clients/src/main/java/org/apache/kafka/common/protocol/Errors.java@
(Kafka 3.7.0). The codes the wire actually uses for the
transactional path:

  * 14 COORDINATOR_LOAD_IN_PROGRESS
  * 15 COORDINATOR_NOT_AVAILABLE
  * 16 NOT_COORDINATOR
  * 47 INVALID_PRODUCER_EPOCH
  * 48 INVALID_TXN_STATE
  * 49 INVALID_PRODUCER_ID_MAPPING
  * 50 INVALID_TRANSACTION_TIMEOUT
  * 51 CONCURRENT_TRANSACTIONS
  * 52 TRANSACTION_COORDINATOR_FENCED
  * 53 TRANSACTIONAL_ID_AUTHORIZATION_FAILED (no dedicated
       constructor — surfaced through 'UnknownCoordinatorError'
       with the code so callers can react)
  * 59 UNKNOWN_PRODUCER_ID
  * 90 PRODUCER_FENCED

The previous mapping used codes from a much older protocol
spec (51 -> INVALID_PRODUCER_EPOCH, 96 -> CONCURRENT_TRANSACTIONS,
etc.) which no Kafka 0.11+ broker ever sends. That meant /every/
transactional error was opaque ('UnknownCoordinatorError') and
the retry-on-mid-transition logic in 'Transaction.initTransactions'
silently never fired. Fixing the mapping is what makes
@TRANSACTION_COORDINATOR_FENCED@ / @CONCURRENT_TRANSACTIONS@
actually take their retry / fence paths.
-}
interpretCoordinatorError :: Int16 -> TransactionCoordinatorError
interpretCoordinatorError code = case code of
  14 -> CoordinatorLoadInProgress "Coordinator load in progress"
  15 -> CoordinatorNotAvailable "Coordinator not available"
  16 -> NotCoordinator "Not coordinator for this resource"
  47 -> InvalidProducerEpoch "Invalid producer epoch"
  48 -> InvalidTxnState "Invalid transaction state"
  49 -> InvalidProducerIdMapping "Invalid producer ID mapping"
  50 -> InvalidPartitionsInTxn "Invalid transaction timeout"
  51 -> ConcurrentTransactions "Concurrent transactions"
  52 -> TransactionCoordinatorFenced "Transaction coordinator fenced"
  90 -> ProducerFenced "Producer fenced by another instance"
  _ -> UnknownCoordinatorError code $ "Unknown error code: " <> T.pack (show code)


{- | Find the transaction coordinator for a given transactional ID
Uses FindCoordinatorRequest (API key 10) with coordinator type TRANSACTION (1)
-}
findTransactionCoordinator
  :: Conn.ConnectionManager
  -> Conn.ConnectionConfig
  -- ^ TLS / SASL settings for coordinator connections
  -> AV.ApiVersionCache
  -> IORef Int32
  -- ^ Correlation ID source
  -> BrokerAddress
  -- ^ Bootstrap broker
  -> Text
  -- ^ Client ID
  -> Text
  -- ^ Transactional ID
  -> IO (Either TransactionCoordinatorError TransactionCoordinator)
findTransactionCoordinator connMgr connCfg versionCache corrIdVar bootstrapBroker clientId transactionalId = do
  -- Get connection to bootstrap broker
  connResult <- Conn.getOrCreateConnection connMgr bootstrapBroker connCfg

  case connResult of
    Left err ->
      return $
        Left $
          CoordinatorNotAvailable $
            "Failed to connect to bootstrap broker: " <> T.pack err
    Right conn -> do
      -- Get correlation ID
      corrId <- atomicModifyIORef' corrIdVar (\cid -> (cid + 1, cid))

      let apiKey = 10 -- FindCoordinator API key
      -- v6 = trunk; we handle both legacy v0-v3 and v4+
      -- (KIP-699 batched Coordinators[]) shapes below.
          clientMaxVersion = 6

      brokerVersionM <- atomically $ AV.queryApiVersion versionCache bootstrapBroker apiKey
      let apiVersion = case brokerVersionM of
            Nothing -> 1 -- Default to v1 (has keyType field)
            Just range -> case AV.selectVersion clientMaxVersion range of
              Nothing -> 1
              Just v -> v

          request
            | apiVersion >= 4 =
                FCReq.FindCoordinatorRequest
                  { FCReq.findCoordinatorRequestKey = P.KafkaString P.Null
                  , FCReq.findCoordinatorRequestKeyType = 1 -- TRANSACTION
                  , FCReq.findCoordinatorRequestCoordinatorKeys =
                      P.mkKafkaArray (V.singleton (P.mkKafkaString transactionalId))
                  }
            | otherwise =
                FCReq.FindCoordinatorRequest
                  { FCReq.findCoordinatorRequestKey = P.mkKafkaString transactionalId
                  , FCReq.findCoordinatorRequestKeyType = 1
                  , FCReq.findCoordinatorRequestCoordinatorKeys = P.mkKafkaArray V.empty
                  }

          requestBody = WC.runEncodeVer @FCReq.FindCoordinatorRequest apiVersion request
          clientIdKafka = P.mkKafkaString clientId

      result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody

      case result of
        Left err ->
          return $
            Left $
              CoordinatorNotAvailable $
                "FindCoordinator request failed: " <> T.pack err
        Right (_corrId, responseBody) -> do
          case WC.runDecodeVer @FCResp.FindCoordinatorResponse apiVersion responseBody of
            Left err ->
              return $
                Left $
                  CoordinatorNotAvailable $
                    "Failed to parse FindCoordinatorResponse: " <> T.pack err
            Right response -> do
              -- v0-v3 carries the result on top-level fields;
              -- v4+ moves it into the first entry of
              -- Coordinators[]. KIP-699 also sinks the per-result
              -- error code into the array entry, so we read it
              -- from there too on v4+.
              let (errorCode, nodeId, host, port)
                    | apiVersion >= 4 =
                        case P.unKafkaArray (FCResp.findCoordinatorResponseCoordinators response) of
                          P.NotNull v
                            | not (V.null v) ->
                                let !c = V.head v
                                    h = case P.unKafkaString (FCResp.coordinatorHost c) of
                                      P.NotNull t -> t
                                      P.Null -> T.empty
                                in ( FCResp.coordinatorErrorCode c
                                   , FCResp.coordinatorNodeId c
                                   , h
                                   , FCResp.coordinatorPort c
                                   )
                          _ -> (15, 0, T.empty, 0) -- 15 = COORDINATOR_NOT_AVAILABLE
                    | otherwise =
                        let h = case P.unKafkaString (FCResp.findCoordinatorResponseHost response) of
                              P.NotNull t -> t
                              P.Null -> T.empty
                        in ( FCResp.findCoordinatorResponseErrorCode response
                           , FCResp.findCoordinatorResponseNodeId response
                           , h
                           , FCResp.findCoordinatorResponsePort response
                           )

              if errorCode /= 0
                then return $ Left $ interpretCoordinatorError errorCode
                else
                  return $
                    Right $
                      TransactionCoordinator
                        { tcNodeId = nodeId
                        , tcHost = host
                        , tcPort = port
                        }


{- | Initialize a producer ID for transactional or idempotent producer
Uses InitProducerIdRequest (API key 22)
-}
initProducerId
  :: Conn.ConnectionManager
  -> Conn.ConnectionConfig
  -- ^ TLS / SASL settings for coordinator connections
  -> AV.ApiVersionCache
  -> IORef Int32
  -- ^ Correlation ID source
  -> Text
  -- ^ Client ID
  -> TransactionCoordinator
  -- ^ Transaction coordinator
  -> Maybe Text
  -- ^ Transactional ID (Nothing for idempotent-only)
  -> Int32
  -- ^ Transaction timeout ms
  -> Maybe Int64
  -- ^ Producer ID (for fencing)
  -> Maybe Int16
  -- ^ Producer epoch (for fencing)
  -> IO (Either TransactionCoordinatorError (Int64, Int16))
  -- ^ Returns (ProducerId, ProducerEpoch)
initProducerId connMgr connCfg versionCache corrIdVar clientId coordinator transactionalId timeoutMs maybeProducerId maybeEpoch = do
  let coordAddr = BrokerAddress (T.unpack $ tcHost coordinator) (fromIntegral $ tcPort coordinator)

  -- Get connection to coordinator
  connResult <- Conn.getOrCreateConnection connMgr coordAddr connCfg

  case connResult of
    Left err ->
      return $
        Left $
          CoordinatorNotAvailable $
            "Failed to connect to coordinator: " <> T.pack err
    Right conn -> do
      -- Get correlation ID
      corrId <- atomicModifyIORef' corrIdVar (\cid -> (cid + 1, cid))

      let apiKey = 22 -- InitProducerId API key
      -- v6 = trunk; we set Enable2Pc=False / KeepPreparedTxn=False
      -- on the request below, which is the existing-behaviour-
      -- compatible (KIP-939 opt-in is False).
          clientMaxVersion = 6

      -- Version negotiation
      brokerVersionM <- atomically $ AV.queryApiVersion versionCache coordAddr apiKey
      let apiVersion = case brokerVersionM of
            Nothing -> 0 -- Default to v0
            Just range -> case AV.selectVersion clientMaxVersion range of
              Nothing -> 0
              Just v -> v

      -- Build InitProducerIdRequest
      let request =
            IPReq.InitProducerIdRequest
              { IPReq.initProducerIdRequestTransactionalId =
                  case transactionalId of
                    Nothing -> P.KafkaString P.Null
                    Just txnId -> P.mkKafkaString txnId
              , IPReq.initProducerIdRequestTransactionTimeoutMs = timeoutMs
              , IPReq.initProducerIdRequestProducerId = maybe (-1) id maybeProducerId
              , IPReq.initProducerIdRequestProducerEpoch = maybe (-1) id maybeEpoch
              }

          requestBody = WC.runEncodeVer @IPReq.InitProducerIdRequest apiVersion request
          clientIdKafka = P.mkKafkaString clientId

      -- Send request and receive response
      result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody

      case result of
        Left err ->
          return $
            Left $
              CoordinatorNotAvailable $
                "InitProducerId request failed: " <> T.pack err
        Right (_corrId, responseBody) -> do
          -- Parse response
          case WC.runDecodeVer @IPResp.InitProducerIdResponse apiVersion responseBody of
            Left err ->
              return $
                Left $
                  CoordinatorNotAvailable $
                    "Failed to parse InitProducerIdResponse: " <> T.pack err
            Right response -> do
              let errorCode = IPResp.initProducerIdResponseErrorCode response

              if errorCode /= 0
                then return $ Left $ interpretCoordinatorError errorCode
                else do
                  let producerId = IPResp.initProducerIdResponseProducerId response
                      producerEpoch = IPResp.initProducerIdResponseProducerEpoch response

                  return $ Right (producerId, producerEpoch)


{- | Add partitions to the current transaction
Uses AddPartitionsToTxnRequest (API key 24)
-}
addPartitionsToTxn
  :: Conn.ConnectionManager
  -> Conn.ConnectionConfig
  -- ^ TLS / SASL settings for coordinator connections
  -> AV.ApiVersionCache
  -> IORef Int32
  -- ^ Correlation ID source
  -> Text
  -- ^ Client ID
  -> TransactionCoordinator
  -- ^ Transaction coordinator
  -> Text
  -- ^ Transactional ID
  -> Int64
  -- ^ Producer ID
  -> Int16
  -- ^ Producer epoch
  -> [TopicPartition]
  -- ^ Partitions to add
  -> IO (Either TransactionCoordinatorError ())
addPartitionsToTxn connMgr connCfg versionCache corrIdVar clientId coordinator transactionalId producerId epoch partitions = do
  let coordAddr = BrokerAddress (T.unpack $ tcHost coordinator) (fromIntegral $ tcPort coordinator)

  -- Group partitions by topic
  let byTopic =
        Map.fromListWith
          (++)
          [(tp.topic, [tp.partition]) | tp <- partitions]

      topics =
        V.fromList
          [ APTReq.AddPartitionsToTxnTopic
              { APTReq.addPartitionsToTxnTopicName = P.mkKafkaString topic
              , APTReq.addPartitionsToTxnTopicPartitions = P.mkKafkaArray (V.fromList partIds)
              }
          | (topic, partIds) <- Map.toList byTopic
          ]

  -- Get connection to coordinator
  connResult <- Conn.getOrCreateConnection connMgr coordAddr connCfg

  case connResult of
    Left err ->
      return $
        Left $
          CoordinatorNotAvailable $
            "Failed to connect to coordinator: " <> T.pack err
    Right conn -> do
      corrId <- atomicModifyIORef' corrIdVar (\cid -> (cid + 1, cid))

      let apiKey = 24
          -- Hold at v3. The schema's own header note —
          -- @Versions 3 and below will be exclusively used by
          -- clients and versions 4 and above will be used by
          -- brokers@ — makes v4 (KIP-704 batched cross-txn
          -- shape) inter-broker-only; clients should stay at v3
          -- regardless of how high the broker advertises.
          clientMaxVersion = 3

      brokerVersionM <- atomically $ AV.queryApiVersion versionCache coordAddr apiKey
      let apiVersion = case brokerVersionM of
            Nothing -> 0
            Just range -> case AV.selectVersion clientMaxVersion range of
              Nothing -> 0
              Just v -> v

      let request =
            APTReq.AddPartitionsToTxnRequest
              { APTReq.addPartitionsToTxnRequestTransactions = P.mkKafkaArray V.empty -- v4+ only
              , APTReq.addPartitionsToTxnRequestV3AndBelowTransactionalId = P.mkKafkaString transactionalId
              , APTReq.addPartitionsToTxnRequestV3AndBelowProducerId = producerId
              , APTReq.addPartitionsToTxnRequestV3AndBelowProducerEpoch = epoch
              , APTReq.addPartitionsToTxnRequestV3AndBelowTopics = P.mkKafkaArray topics
              }

          requestBody = WC.runEncodeVer @APTReq.AddPartitionsToTxnRequest apiVersion request
          clientIdKafka = P.mkKafkaString clientId

      result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody

      case result of
        Left err ->
          return $
            Left $
              CoordinatorNotAvailable $
                "AddPartitionsToTxn request failed: " <> T.pack err
        Right (_corrId, responseBody) -> do
          case WC.runDecodeVer @APTResp.AddPartitionsToTxnResponse apiVersion responseBody of
            Left err ->
              return $
                Left $
                  CoordinatorNotAvailable $
                    "Failed to parse AddPartitionsToTxnResponse: " <> T.pack err
            Right response -> do
              -- Check for top-level error (v4+) or topic-level errors (v0-3)
              -- For simplicity, we'll check the first topic's first partition error if using v0-3
              let resultTopics = case P.unKafkaArray (APTResp.addPartitionsToTxnResponseResultsByTopicV3AndBelow response) of
                    P.NotNull v -> v
                    P.Null -> V.empty

              if V.null resultTopics
                then return $ Right () -- No topics means success
                else do
                  let firstTopic = V.head resultTopics
                      resultPartitions = case P.unKafkaArray (APTResp.addPartitionsToTxnTopicResultResultsByPartition firstTopic) of
                        P.NotNull v -> v
                        P.Null -> V.empty

                  if V.null resultPartitions
                    then return $ Right ()
                    else do
                      let firstPart = V.head resultPartitions
                          errorCode = APTResp.addPartitionsToTxnPartitionResultPartitionErrorCode firstPart

                      if errorCode /= 0
                        then return $ Left $ interpretCoordinatorError errorCode
                        else return $ Right ()


{- | End a transaction (commit or abort)
Uses EndTxnRequest (API key 26)
-}
endTransaction
  :: Conn.ConnectionManager
  -> Conn.ConnectionConfig
  -- ^ TLS / SASL settings for coordinator connections
  -> AV.ApiVersionCache
  -> IORef Int32
  -- ^ Correlation ID source
  -> Text
  -- ^ Client ID
  -> TransactionCoordinator
  -- ^ Transaction coordinator
  -> Text
  -- ^ Transactional ID
  -> Int64
  -- ^ Producer ID
  -> Int16
  -- ^ Producer epoch
  -> Bool
  -- ^ True = commit, False = abort
  -> IO (Either TransactionCoordinatorError ())
endTransaction connMgr connCfg versionCache corrIdVar clientId coordinator transactionalId producerId epoch committed = do
  let coordAddr = BrokerAddress (T.unpack $ tcHost coordinator) (fromIntegral $ tcPort coordinator)

  -- Get connection to coordinator
  connResult <- Conn.getOrCreateConnection connMgr coordAddr connCfg

  case connResult of
    Left err ->
      return $
        Left $
          CoordinatorNotAvailable $
            "Failed to connect to coordinator: " <> T.pack err
    Right conn -> do
      corrId <- atomicModifyIORef' corrIdVar (\cid -> (cid + 1, cid))

      let apiKey = 26 -- EndTxn API key
      -- v5 = trunk; the request fields are unchanged from
      -- v3 (v4 added a new error code, v5 added KIP-890
      -- Part 2 epoch-bumping which is broker-side
      -- behaviour).
          clientMaxVersion = 5

      brokerVersionM <- atomically $ AV.queryApiVersion versionCache coordAddr apiKey
      let apiVersion = case brokerVersionM of
            Nothing -> 0
            Just range -> case AV.selectVersion clientMaxVersion range of
              Nothing -> 0
              Just v -> v

      let request =
            ETReq.EndTxnRequest
              { ETReq.endTxnRequestTransactionalId = P.mkKafkaString transactionalId
              , ETReq.endTxnRequestProducerId = producerId
              , ETReq.endTxnRequestProducerEpoch = epoch
              , ETReq.endTxnRequestCommitted = committed
              }

          requestBody = WC.runEncodeVer @ETReq.EndTxnRequest apiVersion request
          clientIdKafka = P.mkKafkaString clientId

      result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody

      case result of
        Left err ->
          return $
            Left $
              CoordinatorNotAvailable $
                "EndTxn request failed: " <> T.pack err
        Right (_corrId, responseBody) -> do
          case WC.runDecodeVer @ETResp.EndTxnResponse apiVersion responseBody of
            Left err ->
              return $
                Left $
                  CoordinatorNotAvailable $
                    "Failed to parse EndTxnResponse: " <> T.pack err
            Right response -> do
              let errorCode = ETResp.endTxnResponseErrorCode response

              if errorCode /= 0
                then return $ Left $ interpretCoordinatorError errorCode
                else return $ Right ()


{- | Add consumer group offsets to the transaction. Sends an
@AddOffsetsToTxnRequest@ (API key 25) to the transaction
coordinator, registering the group so a subsequent
'txnOffsetCommit' is allowed inside the same transaction
envelope.
-}
addOffsetsToTxn
  :: Conn.ConnectionManager
  -> Conn.ConnectionConfig
  -- ^ TLS / SASL settings for coordinator connections
  -> AV.ApiVersionCache
  -> IORef Int32
  -- ^ Correlation ID source
  -> Text
  -- ^ Client ID
  -> TransactionCoordinator
  -- ^ Transaction coordinator
  -> Text
  -- ^ Transactional ID
  -> Int64
  -- ^ Producer ID
  -> Int16
  -- ^ Producer epoch
  -> Text
  -- ^ Consumer group ID
  -> IO (Either TransactionCoordinatorError ())
addOffsetsToTxn connMgr connCfg versionCache corrIdVar clientId coordinator transactionalId producerId epoch groupId = do
  let coordAddr =
        BrokerAddress
          (T.unpack (tcHost coordinator))
          (fromIntegral (tcPort coordinator))
  connResult <-
    Conn.getOrCreateConnection
      connMgr
      coordAddr
      connCfg
  case connResult of
    Left err ->
      return $
        Left $
          CoordinatorNotAvailable $
            "Failed to connect to coordinator: " <> T.pack err
    Right conn -> do
      corrId <- atomicModifyIORef' corrIdVar (\cid -> (cid + 1, cid))
      let apiKey = 25 -- AddOffsetsToTxn API key
          clientMaxVersion = 4
      brokerVersionM <-
        atomically $
          AV.queryApiVersion versionCache coordAddr apiKey
      let apiVersion = case brokerVersionM of
            Nothing -> 0
            Just range -> case AV.selectVersion clientMaxVersion range of
              Nothing -> 0
              Just v -> v

          request = buildAddOffsetsToTxnRequest transactionalId producerId epoch groupId
          requestBody = WC.runEncodeVer @AOTReq.AddOffsetsToTxnRequest apiVersion request
          clientIdKafka = P.mkKafkaString clientId

      result <-
        Req.sendRequestReceiveResponse
          conn
          apiKey
          apiVersion
          corrId
          clientIdKafka
          requestBody
      case result of
        Left err ->
          return $
            Left $
              CoordinatorNotAvailable $
                "AddOffsetsToTxn request failed: " <> T.pack err
        Right (_, responseBody) ->
          case WC.runDecodeVer @AOTResp.AddOffsetsToTxnResponse apiVersion responseBody of
            Left err ->
              return $
                Left $
                  CoordinatorNotAvailable $
                    "Failed to parse AddOffsetsToTxnResponse: " <> T.pack err
            Right response -> do
              let errorCode = AOTResp.addOffsetsToTxnResponseErrorCode response
              if errorCode /= 0
                then return $ Left (interpretCoordinatorError errorCode)
                else return $ Right ()


{- | Send a TxnOffsetCommitRequest (API key 28). Mirrors what the
JVM client does inside @KafkaProducer.commitTransaction@ when
'sendOffsetsToTransaction' staged consumer-group offsets.
-}
txnOffsetCommitWith
  :: Conn.ConnectionManager
  -> Conn.ConnectionConfig
  -- ^ TLS / SASL settings for coordinator connections
  -> AV.ApiVersionCache
  -> IORef Int32
  -> Text -- client id
  -> BrokerAddress -- consumer group coordinator
  -> Text -- transactional id
  -> TxnGroupMember
  -> Int64 -- producer id
  -> Int16 -- producer epoch
  -> [(TopicPartition, Int64)]
  -> IO (Either TransactionCoordinatorError ())
txnOffsetCommitWith connMgr connCfg versionCache corrIdVar clientId groupCoordinator transactionalId member producerId epoch offsets = do
  connResult <-
    Conn.getOrCreateConnection
      connMgr
      groupCoordinator
      connCfg
  case connResult of
    Left err ->
      return $
        Left $
          CoordinatorNotAvailable $
            "Failed to connect to group coordinator: " <> T.pack err
    Right conn -> do
      corrId <- atomicModifyIORef' corrIdVar (\cid -> (cid + 1, cid))
      let apiKey = 28 -- TxnOffsetCommit API key
      -- v5 = highest version that still uses topic /names/.
      -- v6 (KIP-1319) drops the name field in favour of
      -- topic IDs only; we'd need to plumb the metadata-
      -- cache UUID lookup into this codepath before bumping
      -- past v5. v4-v5 are pure error-code additions
      -- (TRANSACTION_ABORTABLE) — no shape change.
          clientMaxVersion = 5
      brokerVersionM <-
        atomically $
          AV.queryApiVersion versionCache groupCoordinator apiKey
      let apiVersion = case brokerVersionM of
            Nothing -> 0
            Just range -> case AV.selectVersion clientMaxVersion range of
              Nothing -> 0
              Just v -> v

      let request = buildTxnOffsetCommitRequest transactionalId member producerId epoch offsets
          requestBody = WC.runEncodeVer @TOCReq.TxnOffsetCommitRequest apiVersion request
          clientIdKafka = P.mkKafkaString clientId
      result <-
        Req.sendRequestReceiveResponse
          conn
          apiKey
          apiVersion
          corrId
          clientIdKafka
          requestBody
      case result of
        Left err ->
          return $
            Left $
              CoordinatorNotAvailable $
                "TxnOffsetCommit request failed: " <> T.pack err
        Right (_, responseBody) ->
          case WC.runDecodeVer @TOCResp.TxnOffsetCommitResponse apiVersion responseBody of
            Left err ->
              return $
                Left $
                  CoordinatorNotAvailable $
                    "Failed to parse TxnOffsetCommitResponse: " <> T.pack err
            Right response -> do
              -- Walk per-partition errors; the first non-zero is
              -- surfaced.
              let topics = case P.unKafkaArray (TOCResp.txnOffsetCommitResponseTopics response) of
                    P.Null -> V.empty
                    P.NotNull v -> v
              let firstErr =
                    V.foldr
                      ( \t acc -> case acc of
                          Just _ -> acc
                          Nothing ->
                            let parts = case P.unKafkaArray (TOCResp.txnOffsetCommitResponseTopicPartitions t) of
                                  P.Null -> V.empty
                                  P.NotNull v -> v
                            in V.foldr
                                 ( \p acc' -> case acc' of
                                     Just _ -> acc'
                                     Nothing ->
                                       let ec = TOCResp.txnOffsetCommitResponsePartitionErrorCode p
                                       in if ec /= 0 then Just ec else Nothing
                                 )
                                 Nothing
                                 parts
                      )
                      Nothing
                      topics
              case firstErr of
                Nothing -> return (Right ())
                Just ec -> return (Left (interpretCoordinatorError ec))




----------------------------------------------------------------------
-- Pure request builders
--
-- The IO sites above inline request construction inside the
-- network call. These pure helpers expose the same logic so
-- tests can assert the exact wire shape without spinning up a
-- broker.
----------------------------------------------------------------------

{- | Build an 'AddOffsetsToTxnRequest' (KIP-105 / API key 25).
Identical to what 'addOffsetsToTxn' sends; lifted out so tests
can round-trip the request through the encoder/decoder.
-}
buildAddOffsetsToTxnRequest
  :: Text
  -- ^ transactional id
  -> Int64
  -- ^ producer id
  -> Int16
  -- ^ producer epoch
  -> Text
  -- ^ consumer group id
  -> AOTReq.AddOffsetsToTxnRequest
buildAddOffsetsToTxnRequest transactionalId producerId epoch groupId =
  AOTReq.AddOffsetsToTxnRequest
    { AOTReq.addOffsetsToTxnRequestTransactionalId = P.mkKafkaString transactionalId
    , AOTReq.addOffsetsToTxnRequestProducerId = producerId
    , AOTReq.addOffsetsToTxnRequestProducerEpoch = epoch
    , AOTReq.addOffsetsToTxnRequestGroupId = P.mkKafkaString groupId
    }


{- | Build a 'TxnOffsetCommitRequest' (KIP-447 / API key 28).
Groups input offsets by topic, attaches a placeholder
transactional-id derived from (epoch, producer-id), and emits
empty leader-epoch / metadata fields. Mirrors what
'txnOffsetCommitWith' constructs internally.
-}
{- | The consumer-group member a transactional offset commit is made
on behalf of (the JVM's @ConsumerGroupMetadata@).
-}
data TxnGroupMember = TxnGroupMember
  { tgmGroupId :: !Text
  , tgmGenerationId :: !Int32
  , tgmMemberId :: !Text
  , tgmGroupInstanceId :: !(Maybe Text)
  }
  deriving stock (Eq, Show)


buildTxnOffsetCommitRequest
  :: Text
  -- ^ transactional id
  -> TxnGroupMember
  -> Int64
  -- ^ producer id
  -> Int16
  -- ^ producer epoch
  -> [(TopicPartition, Int64)]
  -- ^ offsets to commit
  -> TOCReq.TxnOffsetCommitRequest
buildTxnOffsetCommitRequest transactionalId member producerId epoch offsets =
  let !byTopic =
        Map.fromListWith
          (++)
          [ (tp.topic, [(tp.partition, off)])
          | (tp, off) <- offsets
          ]
      !topicVec =
        V.fromList
          [ TOCReq.TxnOffsetCommitRequestTopic
              { TOCReq.txnOffsetCommitRequestTopicName = P.mkKafkaString topic
              , TOCReq.txnOffsetCommitRequestTopicPartitions =
                  P.mkKafkaArray $
                    V.fromList
                      [ TOCReq.TxnOffsetCommitRequestPartition
                          { TOCReq.txnOffsetCommitRequestPartitionPartitionIndex = pid
                          , TOCReq.txnOffsetCommitRequestPartitionCommittedOffset = off
                          , TOCReq.txnOffsetCommitRequestPartitionCommittedLeaderEpoch = -1
                          , TOCReq.txnOffsetCommitRequestPartitionCommittedMetadata = P.KafkaString P.Null
                          }
                      | (pid, off) <- parts
                      ]
              }
          | (topic, parts) <- Map.toList byTopic
          ]
  in TOCReq.TxnOffsetCommitRequest
       { TOCReq.txnOffsetCommitRequestTransactionalId = P.mkKafkaString transactionalId
       , TOCReq.txnOffsetCommitRequestGroupId = P.mkKafkaString (tgmGroupId member)
       , TOCReq.txnOffsetCommitRequestProducerId = producerId
       , TOCReq.txnOffsetCommitRequestProducerEpoch = epoch
       , -- KIP-447: the coordinator fences a commit from a member whose
         -- generation is stale, so a zombie that lost its partitions
         -- in a rebalance cannot commit offsets for them.
         TOCReq.txnOffsetCommitRequestGenerationId = tgmGenerationId member
       , TOCReq.txnOffsetCommitRequestMemberId = P.mkKafkaString (tgmMemberId member)
       , TOCReq.txnOffsetCommitRequestGroupInstanceId =
           maybe (P.KafkaString P.Null) P.mkKafkaString (tgmGroupInstanceId member)
       , TOCReq.txnOffsetCommitRequestTopics = P.mkKafkaArray topicVec
       }
