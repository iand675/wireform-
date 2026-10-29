{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Client.AdminClient
Description : Manage the cluster — topics, groups, configs, ACLs.
Copyright   : (c) 2025
License     : BSD-3-Clause

The /control plane/ counterpart to the producer and consumer.
'AdminClient' is what you reach for when you need to:

  * create or delete a topic,
  * change a topic's partition count or replication factor,
  * inspect or update broker / topic configs,
  * list / describe / reset / delete consumer groups,
  * trim a partition by deleting records below an offset
    ('deleteRecords'),
  * trigger preferred or unclean leader election.

Like the producer and consumer, an 'AdminClient' holds a
long-lived connection pool — open it once at startup and reuse
it across calls.

= Quick start

@
import qualified Kafka.Client.AdminClient as Admin

main :: IO ()
main =
  Admin.'withAdminClient' [\"localhost:9092\"] Admin.'defaultAdminClientConfig' $ \\adm -> do
    Right results <- Admin.'createTopics' adm
      [ Admin.NewTopic
          { newTopicName              = \"events\"
          , newTopicNumPartitions     = 3
          , newTopicReplicationFactor = 1
          , newTopicConfigs           = []
          }
      ]
    print results
@

Every admin operation returns 'IO (Either String x)'. The
@x@ payload is typically a per-resource result so you can tell
which topic in a batch was rejected and why.
-}
module Kafka.Client.AdminClient (
  -- * AdminClient Types
  AdminClient,
  AdminClientConfig (..),

  -- * AdminClient Lifecycle
  withAdminClient,
  createAdminClient,
  closeAdminClient,

  -- * Cluster info
  adminClusterId,

  -- * Topic Operations
  NewTopic (..),
  TopicDescription (..),
  PartitionInfo (..),
  createTopics,
  ensureTopic,
  deleteTopics,
  listTopics,
  listTopicsExcludeInternal,
  describeTopics,

  -- * Consumer Group Operations
  ConsumerGroupListing (..),
  ConsumerGroupDescription (..),
  MemberDescription (..),
  listConsumerGroups,
  describeConsumerGroups,
  deleteConsumerGroups,
  listConsumerGroupOffsets,
  alterConsumerGroupOffsets,

  -- * Configuration Operations
  ConfigResource (..),
  ConfigResourceType (..),
  ConfigEntry (..),
  ConfigResourceResult (..),
  describeConfigs,
  alterConfigs,

  -- * Incremental config alterations
  AlterConfigOp (..),
  AlterableConfigEntry (..),
  incrementalAlterConfigs,

  -- * DeleteRecords
  deleteRecords,
  DeleteRecordsResultEntry (..),

  -- * Leader election
  ElectionType (..),
  electLeaders,

  -- * Partition management (KIP-195)
  NewPartitions (..),
  createPartitions,

  -- * Cluster discovery (KIP-700)
  describeCluster,

  -- * Generic groups (KIP-848)
  GroupListing (..),
  listGroups,

  -- * ACL admin (KIP-50)
  createAcls,
  describeAcls,
  deleteAcls,
  AclCreationResult (..),
  AclDeletionResult (..),

  -- * Partition reassignment (KIP-455)
  PartitionReassignmentSpec (..),
  OngoingPartitionReassignment (..),
  alterPartitionReassignments,
  listPartitionReassignments,

  -- * Broker lifecycle (KIP-704)
  unregisterBroker,

  -- * Client-quota admin (KIP-546)
  ClientQuotaEntry (..),
  describeClientQuotas,
  alterClientQuotas,

  -- * Transaction admin (KIP-664)
  TransactionListing (..),
  TransactionDescription (..),
  TransactionTopicPartitions (..),
  listTransactions,
  describeTransactions,

  -- * SCRAM credential admin (KIP-554)
  ScramMechanism (..),
  ScramCredentialInfo (..),
  ScramCredentialUpsertion (..),
  ScramCredentialDeletion (..),
  describeUserScramCredentials,
  alterUserScramCredentials,

  -- * Producer-state admin (KIP-664)
  ProducerState (..),
  describeProducers,

  -- * Log directory admin (KIP-113 / KIP-405)
  LogDirDescription (..),
  TopicLogDirDescription (..),
  PartitionLogDirDescription (..),
  ReplicaLogDirAssignment (..),
  describeLogDirs,
  alterReplicaLogDirs,

  -- * Delegation tokens (KIP-48)
  DelegationToken (..),
  createDelegationToken,
  renewDelegationToken,
  expireDelegationToken,
  describeDelegationToken,

  -- * KRaft voter management (KIP-853)
  RaftVoterEndpoint (..),
  addRaftVoter,
  removeRaftVoter,

  -- * KRaft quorum description
  QuorumInfo (..),
  PartitionQuorumInfo (..),
  ReplicaState (..),
  describeMetadataQuorum,

  -- * Consumer group member removal (KIP-345)
  MemberToRemove (..),
  removeMembersFromConsumerGroup,

  -- * Feature flags (KIP-584)
  FeatureMetadata (..),
  SupportedFeatureRange (..),
  FinalizedFeatureLevel (..),
  describeFeatures,
  FeatureUpdate (..),
  FeatureUpgradeType (..),
  updateFeatures,

  -- * Producer fencing (KIP-360)
  fenceProducers,
  FencedProducer (..),

  -- * Manual transaction abort (KIP-664)
  AbortTransactionSpec (..),
  abortTransaction,

  -- * KIP-848 consumer group describe
  ConsumerGroupDescription2 (..),
  ConsumerGroupMember (..),
  describeConsumerGroups2,

  -- * Share group describe (KIP-932)
  ShareGroupDescription (..),
  ShareGroupMember (..),
  describeShareGroups,

  -- * Classic group describe (KIP-848 explicit-protocol shape)
  describeClassicGroups,

  -- * Client metrics resources (KIP-714)
  listClientMetricsResources,

  -- * KIP-714 client telemetry id
  adminClientInstanceId,

  -- * Configuration
  defaultAdminClientConfig,
  defaultAdminApiTimeoutMs,

  -- * Topic-create defaults
  TopicCreateDefaults (..),
  defaultTopicCreateDefaults,

  -- * Null-key compaction policy
  NullKeyCompactionPolicy (..),
  defaultNullKeyCompactionPolicy,

  -- * Metric names

  --

  {- | Canonical metric names emitted by 'Kafka.Telemetry.Metrics'
  for the admin-client operations. Useful when wiring custom
  exporters.
  -}
  adminListTopicsLatencyMs,
  adminCreateTopicsLatencyMs,
  adminDescribeGroupsLatencyMs,
  adminAlterConfigsLatencyMs,
  adminDeleteRecordsLatencyMs,

  -- * Internal helpers (exposed for testing)
  decodeResourceTypeCode,
  unpackResourceResult,
  unpackConfigEntry,
  withNegotiatedVersion,
  extractText,
  adminMetadataOf,
  adminConfigOf,
) where

import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, throwIO, try)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
-- Extra admin RPCs (folded in from the older
-- 'Kafka.Client.AdminClient.Extras' module to keep the admin
-- surface in one place). The protocol-level types live under
-- @Kafka.Protocol.Generated.*@; this module wraps them into
-- operation-shaped functions over the common value types in
-- @Kafka.Common.*@.
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Int
import Data.List qualified
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word16)
import GHC.Generics (Generic)
import Kafka.Client.Internal.Request qualified as Req
import Kafka.Client.Metadata qualified as Meta
import Kafka.Client.Telemetry qualified as Telemetry
import Kafka.Client.TopicId qualified as TopicIdImp
import Kafka.Common (Cluster (..), Node (..))
import Kafka.Common qualified as Common
import Kafka.Common.Acl qualified as Acl
import Kafka.Common.Quota qualified as Quota
import Kafka.Common.Resource qualified as Resource
import Kafka.Errors qualified as Errors
import Kafka.Network.Connection (Connection)
import Kafka.Network.Connection qualified as Conn
import Kafka.Protocol.ApiVersions qualified as AV
import Kafka.Protocol.VersionNegotiation qualified as VN
import "wireform-kafka-protocol" Kafka.Protocol.Generated.AddRaftVoterRequest qualified as ARVReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.AddRaftVoterResponse qualified as ARVResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.AlterClientQuotasRequest qualified as ACQReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.AlterClientQuotasResponse qualified as ACQResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.AlterConfigsRequest qualified as ACReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.AlterConfigsResponse qualified as ACResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.AlterPartitionReassignmentsRequest qualified as APRReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.AlterPartitionReassignmentsResponse qualified as APRResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.AlterReplicaLogDirsRequest qualified as ALDReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.AlterReplicaLogDirsResponse qualified as ALDResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.AlterUserScramCredentialsRequest qualified as ASCReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.AlterUserScramCredentialsResponse qualified as ASCResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ApiVersionsRequest qualified as AVReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ApiVersionsResponse qualified as AVResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ConsumerGroupDescribeRequest qualified as CGDReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ConsumerGroupDescribeResponse qualified as CGDResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.CreateAclsRequest qualified as CAReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.CreateAclsResponse qualified as CAResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.CreateDelegationTokenRequest qualified as CDTReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.CreateDelegationTokenResponse qualified as CDTResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.CreatePartitionsRequest qualified as CPReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.CreatePartitionsResponse qualified as CPResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.CreateTopicsRequest qualified as CTReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.CreateTopicsResponse qualified as CTResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DeleteAclsRequest qualified as DelAReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DeleteAclsResponse qualified as DelAResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DeleteGroupsRequest qualified as DelGReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DeleteGroupsResponse qualified as DelGResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DeleteRecordsRequest qualified as DRReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DeleteRecordsResponse qualified as DRResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DeleteTopicsRequest qualified as DTReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DeleteTopicsResponse qualified as DTResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeAclsRequest qualified as DAReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeAclsResponse qualified as DAResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeClientQuotasRequest qualified as DCQReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeClientQuotasResponse qualified as DCQResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeClusterRequest qualified as DSReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeClusterResponse qualified as DSResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeConfigsRequest qualified as DCReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeConfigsResponse qualified as DCResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeDelegationTokenRequest qualified as DDTReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeDelegationTokenResponse qualified as DDTResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeGroupsRequest qualified as DGReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeGroupsResponse qualified as DGResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeLogDirsRequest qualified as DLDReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeLogDirsResponse qualified as DLDResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeProducersRequest qualified as DPReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeProducersResponse qualified as DPResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeQuorumRequest qualified as DQReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeQuorumResponse qualified as DQResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeTransactionsRequest qualified as DTxReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeTransactionsResponse qualified as DTxResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeUserScramCredentialsRequest qualified as DSCReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeUserScramCredentialsResponse qualified as DSCResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ElectLeadersRequest qualified as ELReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ElectLeadersResponse qualified as ELResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ExpireDelegationTokenRequest qualified as EDTReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ExpireDelegationTokenResponse qualified as EDTResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.IncrementalAlterConfigsRequest qualified as IACReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.IncrementalAlterConfigsResponse qualified as IACResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.InitProducerIdRequest qualified as IPIReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.InitProducerIdResponse qualified as IPIResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.LeaveGroupRequest qualified as LGRReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.LeaveGroupResponse qualified as LGRResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ListClientMetricsResourcesRequest qualified as LCMRReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ListClientMetricsResourcesResponse qualified as LCMRResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ListGroupsRequest qualified as LGReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ListGroupsResponse qualified as LGResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ListPartitionReassignmentsRequest qualified as LPRReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ListPartitionReassignmentsResponse qualified as LPRResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ListTransactionsRequest qualified as LTReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ListTransactionsResponse qualified as LTResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.MetadataRequest qualified as MReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.MetadataResponse qualified as MResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.OffsetCommitRequest qualified as OCReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.OffsetCommitResponse qualified as OCResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.OffsetFetchRequest qualified as OFReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.OffsetFetchResponse qualified as OFResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.RemoveRaftVoterRequest qualified as RRVReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.RemoveRaftVoterResponse qualified as RRVResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.RenewDelegationTokenRequest qualified as RDTReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.RenewDelegationTokenResponse qualified as RDTResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ShareGroupDescribeRequest qualified as SGDReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ShareGroupDescribeResponse qualified as SGDResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.UnregisterBrokerRequest qualified as UBReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.UnregisterBrokerResponse qualified as UBResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.UpdateFeaturesRequest qualified as UFReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.UpdateFeaturesResponse qualified as UFResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.WriteTxnMarkersRequest qualified as WTMReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.WriteTxnMarkersResponse qualified as WTMResp
import "wireform-kafka-protocol" Kafka.Protocol.Primitives qualified as P
import "wireform-kafka-protocol" Kafka.Protocol.Wire.Codec qualified as WC


-- | AdminClient configuration
data AdminClientConfig = AdminClientConfig
  { adminClientId :: !Text
  -- ^ Client identifier (default: "kafka-native-admin")
  , adminRequestTimeoutMs :: !Int
  -- ^ Request timeout in milliseconds (default: 30000)
  }
  deriving (Eq, Show, Generic)


-- | Default admin client configuration
defaultAdminClientConfig :: AdminClientConfig
defaultAdminClientConfig =
  AdminClientConfig
    { adminClientId = "kafka-native-admin"
    , adminRequestTimeoutMs = 30000
    }


-- | AdminClient handle
data AdminClient = AdminClient
  { adminConnManager :: !Conn.ConnectionManager
  -- ^ Connection manager for broker connections
  , adminMetadata :: !Meta.MetadataCache
  -- ^ Metadata cache
  , adminVersionCache :: !AV.ApiVersionCache
  -- ^ API version cache
  , adminCorrelationId :: !(IORef Int32)
  {- ^ Next correlation ID. Pre-Tier-1 lived in STM; the admin
  client never composes the increment with anything else
  transactionally, so 'IORef' + 'atomicModifyIORef\'' is
  sufficient and skips the per-RPC STM commit.
  -}
  , adminConfig :: !AdminClientConfig
  -- ^ Configuration
  }


-- | Create a new admin client
createAdminClient
  :: MonadIO m
  => [Text]
  -- ^ Bootstrap broker addresses ("host:port")
  -> AdminClientConfig
  -> m (Either String AdminClient)
createAdminClient brokerAddrs config = liftIO $ do
  -- Parse broker addresses
  let parsedBrokers = map parseBrokerAddress brokerAddrs

  -- Validate all brokers parsed successfully
  case sequence parsedBrokers of
    Left err -> return $ Left $ "Failed to parse broker addresses: " ++ err
    Right brokers -> do
      -- Create connection manager
      connManager <- Conn.createConnectionManager

      -- Create metadata cache
      metadataCache <- Meta.createMetadataCache

      -- Create version cache
      versionCache <- AV.createVersionCache

      -- Fetch initial metadata from first bootstrap broker
      let firstBroker = head brokers
          connConfig = Conn.defaultConnectionConfig
      connResult <- Conn.getOrCreateConnection connManager firstBroker connConfig
      case connResult of
        Left err -> return $ Left $ "Failed to connect to bootstrap broker: " ++ err
        Right conn -> do
          -- Initialize correlation ID first so the ApiVersions
          -- handshake and the metadata refresh share the same
          -- correlation-id source.
          corrId <- newIORef 1
          let nextCid = atomicModifyIORef' corrId $ \cid -> (cid + 1, cid)

          -- Run the ApiVersions handshake against the bootstrap
          -- broker before any other RPC. We swallow failure: an
          -- older broker (< 0.10) doesn't recognise
          -- ApiVersions and will tear down the connection with
          -- a protocol error; in that case downstream calls
          -- fall back to their compiled-in defaults.
          _ <-
            VN.ensureVersionsNegotiated
              conn
              firstBroker
              versionCache
              nextCid

          fetchResult <- Meta.refreshMetadata conn metadataCache 0
          case fetchResult of
            Left err -> return $ Left $ "Failed to fetch initial metadata: " ++ err
            Right _ ->
              return $
                Right
                  AdminClient
                    { adminConnManager = connManager
                    , adminMetadata = metadataCache
                    , adminVersionCache = versionCache
                    , adminCorrelationId = corrId
                    , adminConfig = config
                    }


-- | Parse broker address in "host:port" format
parseBrokerAddress :: Text -> Either String Conn.BrokerAddress
parseBrokerAddress addr =
  case T.splitOn ":" addr of
    [host, portText] ->
      case reads (T.unpack portText) of
        [(port, "")] -> Right $ Conn.BrokerAddress (T.unpack host) port
        _ -> Left $ "Invalid port: " ++ T.unpack portText
    _ -> Left $ "Invalid broker address format (expected host:port): " ++ T.unpack addr


{- | Open an admin client, run an action with it, and close it on
exit. The recommended bracket — guarantees connections are torn
down even when the body throws, and raises an 'IOError' on
startup failure so callers can use 'Control.Exception.try' /
'catch' to decide whether to retry.

@
'withAdminClient' [\"localhost:9092\"] 'defaultAdminClientConfig' $ \\adm -> do
  Right results <- 'createTopics' adm [..]
  print results
@
-}
withAdminClient
  :: MonadUnliftIO m
  => [Text]
  -> AdminClientConfig
  -> (AdminClient -> m a)
  -> m a
withAdminClient brokers cfg body =
  withRunInIO $ \run ->
    bracket open closeAdminClient (run . body)
  where
    open :: IO AdminClient
    open = do
      r <- createAdminClient brokers cfg
      case r of
        Left err ->
          throwIO $
            Errors.connectError
              (T.pack ("wireform-kafka: createAdminClient failed: " <> err))
        Right c -> pure c
{-# INLINEABLE withAdminClient #-}
{-# SPECIALIZE withAdminClient :: [Text] -> AdminClientConfig -> (AdminClient -> IO a) -> IO a #-}


-- | Close the admin client and clean up resources
closeAdminClient :: MonadIO m => AdminClient -> m ()
closeAdminClient AdminClient {..} = liftIO $ do
  Conn.closeAllConnections adminConnManager


-- | Get next correlation ID
getNextCorrelationId :: AdminClient -> IO Int32
getNextCorrelationId AdminClient {..} =
  atomicModifyIORef' adminCorrelationId $ \cid -> (cid + 1, cid)


----------------------------------------------------------------------
-- Connection + version-negotiation glue
----------------------------------------------------------------------

{- | Obtain a connection to @addr@ from the admin client's
connection pool /and/ make sure @ApiVersions@ has been
negotiated for it. Idempotent: if the cache already has an
entry for @addr@, the negotiation step is a no-op.

All admin RPCs that subsequently consult
'pickAdminApiVersion' for this broker can rely on the cache
being populated with the broker's actual supported ranges
(rather than the empty fallback path).
-}
getNegotiatedConn :: AdminClient -> Conn.BrokerAddress -> IO (Either String Connection)
getNegotiatedConn client@AdminClient {..} addr = do
  connResult <- Conn.getOrCreateConnection adminConnManager addr Conn.defaultConnectionConfig
  case connResult of
    Left err -> pure (Left err)
    Right conn -> do
      -- Brokers older than 0.10 don't speak ApiVersions; we
      -- treat that as "no information, use the caller-supplied
      -- fallback version" rather than a hard failure.
      _ <-
        VN.ensureVersionsNegotiated
          conn
          addr
          adminVersionCache
          (getNextCorrelationId client)
      pure (Right conn)


{- | Pick the right API version for an outbound admin RPC.

Wraps 'VN.pickApiVersion' so a 'VersionMismatch' becomes a
string error the caller can return up the stack. Compared to
the boilerplate that used to live at every call site, this
has two important behaviour differences:

  1. It runs the negotiation handshake on demand if the
     cache is still empty (via 'getNegotiatedConn'), so the
     'fallback' path only fires when the broker doesn't
     speak @ApiVersions@ at all.
  2. It returns @Left@ for an actual mismatch instead of
     silently falling back to v0 — a v0 send to a broker
     that only knows v9+ closes the connection with
     @InvalidRequestException@.
-}
pickAdminApiVersion
  :: AdminClient
  -> Conn.BrokerAddress
  -> Int16
  -- ^ API key
  -> Int16
  -- ^ client min version
  -> Int16
  -- ^ client max version
  -> Int16
  -- ^ fallback when broker doesn't speak ApiVersions
  -> IO (Either String Int16)
pickAdminApiVersion AdminClient {..} addr apiKey clientMin clientMax fallback = do
  r <- VN.pickApiVersion adminVersionCache addr apiKey clientMin clientMax fallback
  pure $ case r of
    Right v -> Right v
    Left mm -> Left $ formatMismatch mm


formatMismatch :: VN.VersionMismatch -> String
formatMismatch (VN.VersionMismatch k cmin cmax bmin bmax) =
  "Broker does not support a compatible version of API "
    <> show k
    <> ": client supports ["
    <> show cmin
    <> ".."
    <> show cmax
    <> "], broker supports ["
    <> show bmin
    <> ".."
    <> show bmax
    <> "]"


{- | Top-level wrapper used by every admin RPC call site:
get a (negotiated) connection to @addr@, pick a version
the broker accepts, allocate a correlation id, then run the
caller-supplied action with all three. Collapses the
5-line connection / version / correlation-id boilerplate
the call sites used to repeat.

Both connection and version-selection failures collapse into
@Left@ before the caller's action runs; the action itself
only sees a successful @(conn, corrId, apiVersion)@ triple
and returns the usual @Either String result@.
| Read-only accessor for the admin client's metadata cache.
-}
adminMetadataOf :: AdminClient -> Meta.MetadataCache
adminMetadataOf c = adminMetadata c


-- | Read-only accessor for the admin client's 'AdminClientConfig'.
adminConfigOf :: AdminClient -> AdminClientConfig
adminConfigOf c = adminConfig c


withNegotiatedVersion
  :: AdminClient
  -> Conn.BrokerAddress
  -> Int16
  -- ^ API key
  -> Int16
  -- ^ client min version
  -> Int16
  -- ^ client max version
  -> Int16
  -- ^ fallback when broker doesn't speak ApiVersions
  -> (Connection -> Int32 -> Int16 -> IO (Either String a))
  -> IO (Either String a)
withNegotiatedVersion client addr apiKey clientMin clientMax fallback k = do
  connR <- getNegotiatedConn client addr
  case connR of
    Left e -> pure (Left ("Failed to connect to broker: " <> e))
    Right conn -> do
      verR <- pickAdminApiVersion client addr apiKey clientMin clientMax fallback
      case verR of
        Left e -> pure (Left e)
        Right apiVersion -> do
          corrId <- getNextCorrelationId client
          k conn corrId apiVersion


-- * Topic Operations


-- | Specification for creating a new topic
data NewTopic = NewTopic
  { ntName :: !Text
  -- ^ Topic name
  , ntNumPartitions :: !Int32
  -- ^ Number of partitions (must be > 0, or -1 for broker default)
  , ntReplicationFactor :: !Int16
  -- ^ Replication factor (must be > 0, or -1 for broker default)
  , ntConfigs :: ![(Text, Text)]
  -- ^ Topic configuration overrides
  }
  deriving (Eq, Show, Generic)


-- | Information about a topic partition
data PartitionInfo = PartitionInfo
  { piPartitionId :: !Int32
  -- ^ Partition ID
  , piLeader :: !Int32
  -- ^ Leader broker ID
  , piReplicas :: ![Int32]
  -- ^ Replica broker IDs
  , piIsr :: ![Int32]
  -- ^ In-sync replica broker IDs
  }
  deriving (Eq, Show, Generic)


-- | Description of a topic
data TopicDescription = TopicDescription
  { tdName :: !Text
  -- ^ Topic name
  , tdInternal :: !Bool
  -- ^ Whether this is an internal topic
  , tdPartitions :: ![PartitionInfo]
  -- ^ Partition information
  }
  deriving (Eq, Show, Generic)


{- | Create one or more topics
Returns a list of (topic name, result) pairs
-}
createTopics
  :: MonadIO m
  => AdminClient
  -> [NewTopic]
  -> m (Either String [(Text, Either String ())])
createTopics client@AdminClient {..} topics = liftIO $ do
  -- Get any broker connection (Kafka will redirect to controller if needed)
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker : _) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 19 -- CreateTopics
      withNegotiatedVersion client brokerAddr apiKey 0 7 0 $ \conn corrId apiVersion -> do
        let creatableTopics = V.fromList $ map buildCreatableTopic topics
            request =
              CTReq.CreateTopicsRequest
                { CTReq.createTopicsRequestTopics = P.mkKafkaArray creatableTopics
                , CTReq.createTopicsRequesttimeoutMs = fromIntegral (adminRequestTimeoutMs adminConfig)
                , CTReq.createTopicsRequestvalidateOnly = False
                }
            requestBody = WC.runEncodeVer @CTReq.CreateTopicsRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "CreateTopics request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @CTResp.CreateTopicsResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse CreateTopicsResponse: " ++ err
              Right response -> do
                let topicResults = case P.unKafkaArray (CTResp.createTopicsResponseTopics response) of
                      P.Null -> V.empty
                      P.NotNull vec -> vec
                    results = V.toList $ V.map processTopicResult topicResults
                return $ Right results
  where
    buildCreatableTopic :: NewTopic -> CTReq.CreatableTopic
    buildCreatableTopic NewTopic {..} =
      let configs =
            V.fromList $
              map
                ( \(k, v) ->
                    CTReq.CreatableTopicConfig
                      { CTReq.creatableTopicConfigName = P.mkKafkaString k
                      , CTReq.creatableTopicConfigValue = P.mkKafkaString v
                      }
                )
                ntConfigs
      in CTReq.CreatableTopic
           { CTReq.creatableTopicName = P.mkKafkaString ntName
           , CTReq.creatableTopicNumPartitions = ntNumPartitions
           , CTReq.creatableTopicReplicationFactor = ntReplicationFactor
           , CTReq.creatableTopicAssignments = P.mkKafkaArray V.empty
           , CTReq.creatableTopicConfigs = P.mkKafkaArray configs
           }

    processTopicResult :: CTResp.CreatableTopicResult -> (Text, Either String ())
    processTopicResult result =
      let topicName = extractText $ CTResp.creatableTopicResultName result
          errorCode = CTResp.creatableTopicResultErrorCode result
          errorMsg = extractText $ CTResp.creatableTopicResultErrorMessage result
      in if errorCode == 0
           then (topicName, Right ())
           else (topicName, Left $ "Error " ++ show errorCode ++ ": " ++ T.unpack errorMsg)


{- | Idempotent topic creation. Calls 'createTopics' for the
single supplied topic and treats the broker-side
@TOPIC_ALREADY_EXISTS@ (error code 36) as a success: the topic
ends up created either way.

Returns:

  * @Right ()@ when the topic is now present (newly created or
    already there).
  * @Left msg@ for every other broker error (authorisation,
    invalid name, request-level transport failure).

This is the right helper for service-startup code that wants
to assert "my topic exists with these settings" without caring
whether it created it or inherited it.
-}
ensureTopic :: MonadIO m => AdminClient -> NewTopic -> m (Either String ())
ensureTopic adm t = do
  r <- createTopics adm [t]
  case r of
    Left err -> pure (Left err)
    Right results -> case lookup (ntName t) results of
      Nothing -> pure (Right ())
      Just (Right ()) -> pure (Right ())
      Just (Left err)
        | "Error 36" `Data.List.isPrefixOf` err -> pure (Right ())
        | otherwise -> pure (Left err)


{- | Delete one or more topics
Returns a list of (topic name, result) pairs
-}
deleteTopics
  :: MonadIO m
  => AdminClient
  -> [Text]
  -> m (Either String [(Text, Either String ())])
deleteTopics client@AdminClient {..} topicNames = liftIO $ do
  -- Get any broker connection (Kafka will redirect to controller if needed)
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker : _) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 20 -- DeleteTopics
      withNegotiatedVersion client brokerAddr apiKey 0 6 0 $ \conn corrId apiVersion -> do
        let topicNamesVec = V.fromList $ map P.mkKafkaString topicNames
            topicStatesVec =
              V.fromList $
                map
                  ( \name ->
                      DTReq.DeleteTopicState
                        { DTReq.deleteTopicStateName = P.mkKafkaString name
                        , DTReq.deleteTopicStateTopicId = P.nullUuid
                        }
                  )
                  topicNames
            request =
              DTReq.DeleteTopicsRequest
                { DTReq.deleteTopicsRequestTopics = P.mkKafkaArray topicStatesVec
                , DTReq.deleteTopicsRequestTopicNames = P.mkKafkaArray topicNamesVec
                , DTReq.deleteTopicsRequestTimeoutMs = fromIntegral (adminRequestTimeoutMs adminConfig)
                }
            requestBody = WC.runEncodeVer @DTReq.DeleteTopicsRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "DeleteTopics request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @DTResp.DeleteTopicsResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse DeleteTopicsResponse: " ++ err
              Right response -> do
                let topicResults = case P.unKafkaArray (DTResp.deleteTopicsResponseResponses response) of
                      P.Null -> V.empty
                      P.NotNull vec -> vec
                    results =
                      V.toList $
                        V.map
                          ( \r ->
                              let name = extractText $ DTResp.deletableTopicResultName r
                                  code = DTResp.deletableTopicResultErrorCode r
                                  msg = extractText $ DTResp.deletableTopicResultErrorMessage r
                              in if code == 0
                                   then (name, Right ())
                                   else (name, Left $ "Error " ++ show code ++ ": " ++ T.unpack msg)
                          )
                          topicResults
                return $ Right results


-- | List all topics in the cluster
listTopics
  :: MonadIO m
  => AdminClient
  -> m (Either String [Text])
listTopics client@AdminClient {..} = liftIO $ do
  -- Use Metadata API to list topics
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker : _) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 3 -- Metadata
          -- Metadata is flexible from v9. With the response-header
          -- v1 trailer skipped correctly the flexible variants
          -- decode cleanly; we still cap at v12 because v13+ adds
          -- TopicId fields the high-level 'TopicDescription' API
          -- doesn't expose yet.
          -- Metadata: codegen handles up to v13. v9 went flexible
          -- (response-header v1 trailer); v12 added Uuid topic id;
          -- v13 made per-topic name nullable when topic id is set.
          -- Our request always supplies the name (TopicId = nullUuid),
          -- which the broker treats as a name-based lookup at every
          -- version up to and including 13.
      withNegotiatedVersion client brokerAddr apiKey 0 13 8 $ \conn corrId apiVersion -> do
        let request =
              MReq.MetadataRequest
                { MReq.metadataRequestTopics = P.mkKafkaArray V.empty
                , MReq.metadataRequestAllowAutoTopicCreation = False
                , MReq.metadataRequestIncludeClusterAuthorizedOperations = False
                , MReq.metadataRequestIncludeTopicAuthorizedOperations = False
                }
            requestBody = WC.runEncodeVer @MReq.MetadataRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "Metadata request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @MResp.MetadataResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse MetadataResponse: " ++ err
              Right response -> do
                let topics = case P.unKafkaArray (MResp.metadataResponseTopics response) of
                      P.Null -> V.empty
                      P.NotNull vec -> vec
                    topicNames = V.toList $ V.map (extractText . MResp.metadataResponseTopicName) topics
                return $ Right topicNames


-- | Describe one or more topics
describeTopics
  :: MonadIO m
  => AdminClient
  -> [Text]
  -> m (Either String [TopicDescription])
describeTopics client@AdminClient {..} topicNames = liftIO $ do
  -- Use Metadata API to describe topics
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker : _) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 3 -- Metadata
          -- v9+ is flexible; cap at v12 (see 'listTopics').
      withNegotiatedVersion client brokerAddr apiKey 0 13 8 $ \conn corrId apiVersion -> do
        let topicReqs =
              V.fromList $
                map
                  ( \name ->
                      MReq.MetadataRequestTopic
                        { MReq.metadataRequestTopicTopicId = P.nullUuid
                        , MReq.metadataRequestTopicName = P.mkKafkaString name
                        }
                  )
                  topicNames
            request =
              MReq.MetadataRequest
                { MReq.metadataRequestTopics = P.mkKafkaArray topicReqs
                , MReq.metadataRequestAllowAutoTopicCreation = False
                , MReq.metadataRequestIncludeClusterAuthorizedOperations = False
                , MReq.metadataRequestIncludeTopicAuthorizedOperations = False
                }
            requestBody = WC.runEncodeVer @MReq.MetadataRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "Metadata request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @MResp.MetadataResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse MetadataResponse: " ++ err
              Right response -> do
                let topics = case P.unKafkaArray (MResp.metadataResponseTopics response) of
                      P.Null -> V.empty
                      P.NotNull vec -> vec
                    descriptions = V.toList $ V.mapMaybe buildTopicDescription topics
                return $ Right descriptions
  where
    buildTopicDescription :: MResp.MetadataResponseTopic -> Maybe TopicDescription
    buildTopicDescription topic =
      let name = extractText $ MResp.metadataResponseTopicName topic
          isInternal = MResp.metadataResponseTopicIsInternal topic
          partitions = case P.unKafkaArray (MResp.metadataResponseTopicPartitions topic) of
            P.Null -> V.empty
            P.NotNull vec -> vec

          partInfos = V.toList $ V.map buildPartitionInfo partitions
      in Just $
           TopicDescription
             { tdName = name
             , tdInternal = isInternal
             , tdPartitions = partInfos
             }

    buildPartitionInfo :: MResp.MetadataResponsePartition -> PartitionInfo
    buildPartitionInfo part =
      PartitionInfo
        { piPartitionId = MResp.metadataResponsePartitionPartitionIndex part
        , piLeader = MResp.metadataResponsePartitionLeaderId part
        , piReplicas = case P.unKafkaArray (MResp.metadataResponsePartitionReplicaNodes part) of
            P.Null -> []
            P.NotNull vec -> V.toList vec
        , piIsr = case P.unKafkaArray (MResp.metadataResponsePartitionIsrNodes part) of
            P.Null -> []
            P.NotNull vec -> V.toList vec
        }


-- * Consumer Group Operations


-- | Basic information about a consumer group
data ConsumerGroupListing = ConsumerGroupListing
  { cglGroupId :: !Text
  -- ^ Group ID
  , cglIsSimpleGroup :: !Bool
  -- ^ Whether this is a simple consumer group
  }
  deriving (Eq, Show, Generic)


-- | Description of a consumer group member
data MemberDescription = MemberDescription
  { mdMemberId :: !Text
  -- ^ Member ID
  , mdClientId :: !Text
  -- ^ Client ID
  , mdHost :: !Text
  -- ^ Member host
  }
  deriving (Eq, Show, Generic)


-- | Description of a consumer group
data ConsumerGroupDescription = ConsumerGroupDescription
  { cgdGroupId :: !Text
  -- ^ Group ID
  , cgdState :: !Text
  -- ^ Group state (e.g., "Stable", "PreparingRebalance")
  , cgdMembers :: ![MemberDescription]
  -- ^ Group members
  }
  deriving (Eq, Show, Generic)


-- | List all consumer groups in the cluster
listConsumerGroups
  :: MonadIO m
  => AdminClient
  -> m (Either String [ConsumerGroupListing])
listConsumerGroups client@AdminClient {..} = liftIO $ do
  -- Get any broker connection
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker : _) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 16 -- ListGroups
          -- ListGroups: codegen handles up to v5 (KIP-848 added the
          -- typesFilter field, which we already supply as empty).
      withNegotiatedVersion client brokerAddr apiKey 0 5 0 $ \conn corrId apiVersion -> do
        let request =
              LGReq.ListGroupsRequest
                { LGReq.listGroupsRequestStatesFilter = P.mkKafkaArray V.empty
                , LGReq.listGroupsRequestTypesFilter = P.mkKafkaArray V.empty
                }
            requestBody = WC.runEncodeVer @LGReq.ListGroupsRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "ListGroups request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @LGResp.ListGroupsResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse ListGroupsResponse: " ++ err
              Right response -> do
                let groups = case P.unKafkaArray (LGResp.listGroupsResponseGroups response) of
                      P.Null -> V.empty
                      P.NotNull vec -> vec
                    listings =
                      V.toList $
                        V.map
                          ( \g ->
                              ConsumerGroupListing
                                { cglGroupId = extractText $ LGResp.listedGroupGroupId g
                                , cglIsSimpleGroup = LGResp.listedGroupGroupType g == P.mkKafkaString "consumer"
                                }
                          )
                          groups
                return $ Right listings


-- | Describe one or more consumer groups
describeConsumerGroups
  :: MonadIO m
  => AdminClient
  -> [Text]
  -> m (Either String [ConsumerGroupDescription])
describeConsumerGroups client@AdminClient {..} groupIds = liftIO $ do
  -- Get any broker connection
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker : _) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 15 -- DescribeGroups
          -- DescribeGroups: codegen handles up to v6 (KIP-848 added
          -- additional response fields the high-level
          -- 'ConsumerGroupDescription' surface ignores, which is
          -- fine — extra fields decode and we just don't expose them).
      withNegotiatedVersion client brokerAddr apiKey 0 6 0 $ \conn corrId apiVersion -> do
        let groupVec = V.fromList $ map P.mkKafkaString groupIds
            request =
              DGReq.DescribeGroupsRequest
                { DGReq.describeGroupsRequestGroups = P.mkKafkaArray groupVec
                , DGReq.describeGroupsRequestIncludeAuthorizedOperations = False
                }
            requestBody = WC.runEncodeVer @DGReq.DescribeGroupsRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "DescribeGroups request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @DGResp.DescribeGroupsResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse DescribeGroupsResponse: " ++ err
              Right response -> do
                let groups = case P.unKafkaArray (DGResp.describeGroupsResponseGroups response) of
                      P.Null -> V.empty
                      P.NotNull vec -> vec
                    descriptions = V.toList $ V.map buildGroupDescription groups
                return $ Right descriptions
  where
    buildGroupDescription :: DGResp.DescribedGroup -> ConsumerGroupDescription
    buildGroupDescription group =
      let members = case P.unKafkaArray (DGResp.describedGroupMembers group) of
            P.Null -> V.empty
            P.NotNull vec -> vec

          memberDescs =
            V.toList $
              V.map
                ( \m ->
                    MemberDescription
                      { mdMemberId = extractText $ DGResp.describedGroupMemberMemberId m
                      , mdClientId = extractText $ DGResp.describedGroupMemberClientId m
                      , mdHost = extractText $ DGResp.describedGroupMemberClientHost m
                      }
                )
                members
      in ConsumerGroupDescription
           { cgdGroupId = extractText $ DGResp.describedGroupGroupId group
           , cgdState = extractText $ DGResp.describedGroupGroupState group
           , cgdMembers = memberDescs
           }


{- | Delete one or more consumer groups
Returns a list of (group ID, result) pairs
-}
deleteConsumerGroups
  :: MonadIO m
  => AdminClient
  -> [Text]
  -> m (Either String [(Text, Either String ())])
deleteConsumerGroups client@AdminClient {..} groupIds = liftIO $ do
  -- Get any broker connection
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker : _) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 42 -- DeleteGroups
      withNegotiatedVersion client brokerAddr apiKey 0 2 0 $ \conn corrId apiVersion -> do
        let groupVec = V.fromList $ map P.mkKafkaString groupIds
            request =
              DelGReq.DeleteGroupsRequest
                { DelGReq.deleteGroupsRequestGroupsNames = P.mkKafkaArray groupVec
                }
            requestBody = WC.runEncodeVer @DelGReq.DeleteGroupsRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "DeleteGroups request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @DelGResp.DeleteGroupsResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse DeleteGroupsResponse: " ++ err
              Right response -> do
                let groupResults = case P.unKafkaArray (DelGResp.deleteGroupsResponseResults response) of
                      P.Null -> V.empty
                      P.NotNull vec -> vec
                    results =
                      V.toList $
                        V.map
                          ( \r ->
                              let gid = extractText $ DelGResp.deletableGroupResultGroupId r
                                  code = DelGResp.deletableGroupResultErrorCode r
                              in if code == 0
                                   then (gid, Right ())
                                   else (gid, Left $ "Error code: " ++ show code)
                          )
                          groupResults
                return $ Right results


-- * Configuration Operations


-- | Type of configuration resource
data ConfigResourceType
  = -- | Topic configuration
    ConfigResourceTopic
  | -- | Broker configuration
    ConfigResourceBroker
  | -- | Broker logger configuration
    ConfigResourceBrokerLogger
  deriving (Eq, Show, Generic)


-- | A configuration resource to describe
data ConfigResource = ConfigResource
  { crType :: !ConfigResourceType
  -- ^ Resource type
  , crName :: !Text
  -- ^ Resource name (topic name, broker ID, etc.)
  }
  deriving (Eq, Show, Generic)


-- | A configuration entry (key-value pair)
data ConfigEntry = ConfigEntry
  { ceName :: !Text
  -- ^ Configuration key
  , ceValue :: !(Maybe Text)
  -- ^ Configuration value (Nothing if not set)
  , ceReadOnly :: !Bool
  -- ^ Whether this config is read-only
  , ceIsDefault :: !Bool
  -- ^ Whether this is the default value
  , ceSensitive :: !Bool
  -- ^ Whether this config contains sensitive data
  }
  deriving (Eq, Show, Generic)


-- | Result of describing a configuration resource
data ConfigResourceResult = ConfigResourceResult
  { crrResource :: !ConfigResource
  -- ^ The resource that was described
  , crrEntries :: ![ConfigEntry]
  -- ^ Configuration entries
  , crrError :: !(Maybe Text)
  -- ^ Error message, if any
  }
  deriving (Eq, Show, Generic)


{- | Describe configurations for one or more resources. Mirrors
@AdminClient.describeConfigs@: issues a single DescribeConfigs
RPC against any broker (the broker forwards to the controller
for broker-config requests), and unpacks each per-resource
result into a 'ConfigResourceResult'. Errors at the resource
level are surfaced via 'crrError'; transport-level failures
collapse into the outer 'Left'.
-}
describeConfigs
  :: MonadIO m
  => AdminClient
  -> [ConfigResource]
  -> m (Either String [ConfigResourceResult])
describeConfigs client@AdminClient {..} resources = liftIO $ do
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker : _) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 32 -- DescribeConfigs
          -- DescribeConfigs has been v1+ since Kafka 0.11; the encoder
          -- doesn't handle v0. We accept v1..v4 — v4 is the flexible
          -- variant and is exercised by the live-broker integration
          -- suite now that 'parseResponseFrame' correctly skips the
          -- v1 response-header tagged-fields trailer for flexible
          -- responses.
      withNegotiatedVersion client brokerAddr apiKey 1 4 1 $ \conn corrId apiVersion -> do
        let resourcesV = V.fromList (map buildResource resources)
            request =
              DCReq.DescribeConfigsRequest
                { DCReq.describeConfigsRequestResources = P.mkKafkaArray resourcesV
                , DCReq.describeConfigsRequestIncludeSynonyms = False
                , DCReq.describeConfigsRequestIncludeDocumentation = False
                }
            requestBody = WC.runEncodeVer @DCReq.DescribeConfigsRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <-
          Req.sendRequestReceiveResponse
            conn
            apiKey
            apiVersion
            corrId
            clientIdKafka
            requestBody
        case result of
          Left err -> return $ Left $ "DescribeConfigs request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @DCResp.DescribeConfigsResponse apiVersion responseBody of
              Left err ->
                return $
                  Left $
                    "Failed to parse DescribeConfigsResponse: " ++ err
              Right response -> do
                let results = case P.unKafkaArray (DCResp.describeConfigsResponseResults response) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                return $ Right $ V.toList $ V.map processResourceResult results
  where
    encodeResourceType :: ConfigResourceType -> Int8
    encodeResourceType = \case
      ConfigResourceTopic -> 2
      ConfigResourceBroker -> 4
      ConfigResourceBrokerLogger -> 8

    buildResource :: ConfigResource -> DCReq.DescribeConfigsResource
    buildResource cr =
      DCReq.DescribeConfigsResource
        { DCReq.describeConfigsResourceResourceType = encodeResourceType (crType cr)
        , DCReq.describeConfigsResourceResourceName = P.mkKafkaString (crName cr)
        , -- Empty 'configurationKeys' means "every key"; matches
          -- the JVM client's Map<ConfigResource,Collection<String>>
          -- where an empty inner collection requests all configs.
          DCReq.describeConfigsResourceConfigurationKeys = P.mkKafkaArray V.empty
        }

    processResourceResult = unpackResourceResult


{- | Decode a wire 'ResourceType' code (per the DescribeConfigs
RPC) into the higher-level 'ConfigResourceType' enum. Unknown
codes fall through to 'ConfigResourceTopic' as a best-effort
default; the JVM client behaves the same way (it reads back
'ConfigResource.Type.UNKNOWN', but we don't model that here).
-}
decodeResourceTypeCode :: Int8 -> ConfigResourceType
decodeResourceTypeCode = \case
  2 -> ConfigResourceTopic
  4 -> ConfigResourceBroker
  8 -> ConfigResourceBrokerLogger
  _ -> ConfigResourceTopic


{- | Translate a single 'DescribeConfigsResult' (one per resource
in the response) into a 'ConfigResourceResult'. Surfaces any
per-resource error code via 'crrError'; preserves 'crrEntries'
in the order the broker returned them.
-}
unpackResourceResult :: DCResp.DescribeConfigsResult -> ConfigResourceResult
unpackResourceResult r =
  let !rt = decodeResourceTypeCode (DCResp.describeConfigsResultResourceType r)
      !rn = extractText (DCResp.describeConfigsResultResourceName r)
      !errCode = DCResp.describeConfigsResultErrorCode r
      !errMsgT = extractText (DCResp.describeConfigsResultErrorMessage r)
      !configsV = case P.unKafkaArray (DCResp.describeConfigsResultConfigs r) of
        P.Null -> V.empty
        P.NotNull v -> v
      !entries = V.toList (V.map unpackConfigEntry configsV)
  in ConfigResourceResult
       { crrResource = ConfigResource {crType = rt, crName = rn}
       , crrEntries = entries
       , crrError =
           if errCode == 0
             then Nothing
             else
               Just
                 ( if T.null errMsgT
                     then T.pack ("Error code " <> show errCode)
                     else errMsgT
                 )
       }


{- | Translate a single 'DescribeConfigsResourceResult' (one per
config key under a resource) into a 'ConfigEntry'. Notable
mappings:

  * a null 'KafkaString' value becomes 'Nothing' (caller can
    distinguish "unset" from "empty");
  * KIP-226 'ConfigSource': only @5@ (DEFAULT_CONFIG) sets
    'ceIsDefault' to True; everything else (topic / dynamic /
    static / per-broker) is treated as a non-default value.
-}
unpackConfigEntry :: DCResp.DescribeConfigsResourceResult -> ConfigEntry
unpackConfigEntry e =
  let !nm = extractText (DCResp.describeConfigsResourceResultName e)
      !rawValT = extractText (DCResp.describeConfigsResourceResultValue e)
      !val = case DCResp.describeConfigsResourceResultValue e of
        P.KafkaString P.Null -> Nothing
        _ -> Just rawValT
      !ro = DCResp.describeConfigsResourceResultReadOnly e
      !sen = DCResp.describeConfigsResourceResultIsSensitive e
      !srcCode = DCResp.describeConfigsResourceResultConfigSource e
      !isDef = srcCode == 5
  in ConfigEntry
       { ceName = nm
       , ceValue = val
       , ceReadOnly = ro
       , ceIsDefault = isDef
       , ceSensitive = sen
       }


-- * Cluster info


{- | Read the broker-supplied cluster id off the admin
client's metadata cache. Returns 'Nothing' until the first
successful refresh.
-}
adminClusterId :: MonadIO m => AdminClient -> m (Maybe Text)
adminClusterId AdminClient {..} =
  liftIO $
    atomically (Meta.getClusterId adminMetadata)


-- * List-topics filtering


{- | Like 'listTopics' but skips Kafka-internal topics (those with
the @isInternal@ flag set: @__consumer_offsets@,
@__transaction_state@, …). Mirrors the JVM client's
@ListTopicsOptions.listInternal(false)@.
-}
listTopicsExcludeInternal :: MonadIO m => AdminClient -> m (Either String [Text])
listTopicsExcludeInternal client@AdminClient {..} = liftIO $ do
  -- Re-issues the same MetadataRequest 'listTopics' does but
  -- filters with the per-topic 'isInternal' flag.
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker : _) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 3 -- Metadata
          -- v9+ is flexible; cap at v12 (see 'listTopics').
      withNegotiatedVersion client brokerAddr apiKey 0 13 8 $ \conn corrId apiVersion -> do
        let request =
              MReq.MetadataRequest
                { MReq.metadataRequestTopics = P.mkKafkaArray V.empty
                , MReq.metadataRequestAllowAutoTopicCreation = False
                , MReq.metadataRequestIncludeClusterAuthorizedOperations = False
                , MReq.metadataRequestIncludeTopicAuthorizedOperations = False
                }
            requestBody = WC.runEncodeVer @MReq.MetadataRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "Metadata request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @MResp.MetadataResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse MetadataResponse: " ++ err
              Right response -> do
                let topicsVec = case P.unKafkaArray (MResp.metadataResponseTopics response) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                    keep t = not (MResp.metadataResponseTopicIsInternal t)
                pure $
                  Right $
                    V.toList $
                      V.map (extractText . MResp.metadataResponseTopicName) $
                        V.filter keep topicsVec


-- * alterConfigs


{- | Replace the configuration for one or more resources.
/Note/: this is the legacy "AlterConfigs" call which replaces
/every/ key for a resource — keys you don't include are reset
to their defaults. Prefer 'incrementalAlterConfigs' for new
code.

Returns one entry per input resource: @Right ()@ on success,
or @Left errorMessage@ for resources the broker rejected.
-}
alterConfigs
  :: MonadIO m
  => AdminClient
  -> [(ConfigResource, [(Text, Text)])]
  -> m (Either String [(ConfigResource, Either String ())])
alterConfigs client@AdminClient {..} resources = liftIO $ do
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker : _) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 33 -- AlterConfigs
      withNegotiatedVersion client brokerAddr apiKey 0 2 0 $ \conn corrId apiVersion -> do
        let resourcesV = V.fromList (map buildResource resources)
            request =
              ACReq.AlterConfigsRequest
                { ACReq.alterConfigsRequestResources = P.mkKafkaArray resourcesV
                , ACReq.alterConfigsRequestValidateOnly = False
                }
            requestBody = WC.runEncodeVer @ACReq.AlterConfigsRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "AlterConfigs request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @ACResp.AlterConfigsResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse AlterConfigsResponse: " ++ err
              Right response -> do
                let respVec = case P.unKafkaArray (ACResp.alterConfigsResponseResponses response) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                    out = V.toList $ V.map unpackACResp respVec
                return $ Right out
  where
    buildResource :: (ConfigResource, [(Text, Text)]) -> ACReq.AlterConfigsResource
    buildResource (cr, kvs) =
      let configsV =
            V.fromList $
              map
                ( \(k, v) ->
                    ACReq.AlterableConfig
                      { ACReq.alterableConfigName = P.mkKafkaString k
                      , ACReq.alterableConfigValue = P.mkKafkaString v
                      }
                )
                kvs
      in ACReq.AlterConfigsResource
           { ACReq.alterConfigsResourceResourceType = encodeResourceType (crType cr)
           , ACReq.alterConfigsResourceResourceName = P.mkKafkaString (crName cr)
           , ACReq.alterConfigsResourceConfigs = P.mkKafkaArray configsV
           }

    unpackACResp
      :: ACResp.AlterConfigsResourceResponse
      -> (ConfigResource, Either String ())
    unpackACResp r =
      let !rt = decodeResourceTypeCode (ACResp.alterConfigsResourceResponseResourceType r)
          !rn = extractText (ACResp.alterConfigsResourceResponseResourceName r)
          !ec = ACResp.alterConfigsResourceResponseErrorCode r
          !emsg = extractText (ACResp.alterConfigsResourceResponseErrorMessage r)
      in ( ConfigResource {crType = rt, crName = rn}
         , if ec == 0
             then Right ()
             else Left ("Error " ++ show ec ++ ": " ++ T.unpack emsg)
         )


-- * incrementalAlterConfigs


-- | Operation type for an individual configuration key.
data AlterConfigOp
  = -- | Set the key to the supplied value.
    AlterConfigOpSet
  | -- | Delete the key.
    AlterConfigOpDelete
  | -- | Append to a list-valued key.
    AlterConfigOpAppend
  | -- | Subtract from a list-valued key.
    AlterConfigOpSubtract
  deriving stock (Eq, Show, Generic)


-- | Incremental config alteration entry.
data AlterableConfigEntry = AlterableConfigEntry
  { aceName :: !Text
  , aceOp :: !AlterConfigOp
  , aceValue :: !(Maybe Text)
  -- ^ 'Nothing' for 'AlterConfigOpDelete'; otherwise required.
  }
  deriving (Eq, Show, Generic)


{- | Incremental (non-replacing) config alterations.
Set / Delete / Append / Subtract per key.

Strongly preferred over 'alterConfigs' for new code.
-}
incrementalAlterConfigs
  :: MonadIO m
  => AdminClient
  -> [(ConfigResource, [AlterableConfigEntry])]
  -> m (Either String [(ConfigResource, Either String ())])
incrementalAlterConfigs client@AdminClient {..} resources = liftIO $ do
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker : _) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 44 -- IncrementalAlterConfigs
      withNegotiatedVersion client brokerAddr apiKey 0 1 0 $ \conn corrId apiVersion -> do
        let resourcesV = V.fromList (map buildResource resources)
            request =
              IACReq.IncrementalAlterConfigsRequest
                { IACReq.incrementalAlterConfigsRequestResources = P.mkKafkaArray resourcesV
                , IACReq.incrementalAlterConfigsRequestValidateOnly = False
                }
            requestBody = WC.runEncodeVer @IACReq.IncrementalAlterConfigsRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "IncrementalAlterConfigs request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @IACResp.IncrementalAlterConfigsResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse IncrementalAlterConfigsResponse: " ++ err
              Right response -> do
                let respVec = case P.unKafkaArray (IACResp.incrementalAlterConfigsResponseResponses response) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                    out = V.toList $ V.map unpackResp respVec
                return $ Right out
  where
    encodeOp :: AlterConfigOp -> Int8
    encodeOp = \case
      AlterConfigOpSet -> 0
      AlterConfigOpDelete -> 1
      AlterConfigOpAppend -> 2
      AlterConfigOpSubtract -> 3

    buildResource :: (ConfigResource, [AlterableConfigEntry]) -> IACReq.AlterConfigsResource
    buildResource (cr, ents) =
      let configsV = V.fromList $ map buildEntry ents
      in IACReq.AlterConfigsResource
           { IACReq.alterConfigsResourceResourceType = encodeResourceType (crType cr)
           , IACReq.alterConfigsResourceResourceName = P.mkKafkaString (crName cr)
           , IACReq.alterConfigsResourceConfigs = P.mkKafkaArray configsV
           }

    buildEntry :: AlterableConfigEntry -> IACReq.AlterableConfig
    buildEntry AlterableConfigEntry {..} =
      IACReq.AlterableConfig
        { IACReq.alterableConfigName = P.mkKafkaString aceName
        , IACReq.alterableConfigConfigOperation = encodeOp aceOp
        , IACReq.alterableConfigValue = case aceValue of
            Nothing -> P.KafkaString P.Null
            Just v -> P.mkKafkaString v
        }

    unpackResp
      :: IACResp.AlterConfigsResourceResponse
      -> (ConfigResource, Either String ())
    unpackResp r =
      let !rt = decodeResourceTypeCode (IACResp.alterConfigsResourceResponseResourceType r)
          !rn = extractText (IACResp.alterConfigsResourceResponseResourceName r)
          !ec = IACResp.alterConfigsResourceResponseErrorCode r
          !emsg = extractText (IACResp.alterConfigsResourceResponseErrorMessage r)
      in ( ConfigResource {crType = rt, crName = rn}
         , if ec == 0
             then Right ()
             else Left ("Error " ++ show ec ++ ": " ++ T.unpack emsg)
         )


{- | Encode a 'ConfigResourceType' to its wire code (used by both
'alterConfigs' and 'incrementalAlterConfigs').
-}
encodeResourceType :: ConfigResourceType -> Int8
encodeResourceType = \case
  ConfigResourceTopic -> 2
  ConfigResourceBroker -> 4
  ConfigResourceBrokerLogger -> 8


-- * deleteRecords (admin entry)


{- | One row of the result returned by 'deleteRecords': the new
low-watermark for a partition (records strictly below it are
gone).
-}
data DeleteRecordsResultEntry = DeleteRecordsResultEntry
  { dreTopic :: !Text
  , drePartition :: !Int32
  , dreLowWatermark :: !Int64
  , dreErrorCode :: !Int16
  }
  deriving (Eq, Show, Generic)


{- | Trim the partition log up to (but not including) the
supplied offset for each (topic, partition). Mirrors
@AdminClient.deleteRecords(Map\<TopicPartition, RecordsToDelete\>)@.
-}
deleteRecords
  :: MonadIO m
  => AdminClient
  -> [(Text, Int32, Int64)]
  -- ^ (topic, partition, offset)
  -> m (Either String [DeleteRecordsResultEntry])
deleteRecords _ [] = pure (Right [])
deleteRecords client@AdminClient {..} entries = liftIO $ do
  -- DeleteRecords is answered by each partition's leader only (any
  -- other broker replies NOT_LEADER_OR_FOLLOWER): one request per leader.
  eDesc <- describeTopics client (Map.keys (Map.fromList (map (\(t, _, _) -> (t, ())) entries)))
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case (eDesc, brokersM) of
    (Left e, _) -> pure (Left ("DeleteRecords: leader lookup failed: " <> e))
    (_, Nothing) -> pure (Left "No brokers available")
    (Right descs, Just brokers) -> do
      let leaders =
            Map.fromList
              (concatMap (\d -> map (\pinfo -> ((tdName d, piPartitionId pinfo), piLeader pinfo)) (tdPartitions d)) descs)
          addrOf = Map.fromList (map (\b -> (Meta.brokerMetaNodeId b, Meta.brokerMetaAddress b)) brokers)
          grouped =
            Map.fromListWith
              (flip (<>))
              (map (\e@(t, p, _) -> (Map.lookup (t, p) leaders >>= (`Map.lookup` addrOf), [e])) entries)
      results <- forM (Map.toList grouped) $ \(mAddr, es) -> case mAddr of
        Nothing ->
          pure (Left ("DeleteRecords: no known leader for " <> show (map (\(t, p, _) -> (t, p)) es)))
        Just addr -> deleteRecordsOn client addr es
      pure (concat <$> sequence results)


-- | One DeleteRecords request to @brokerAddr@ (the leader of every entry).
deleteRecordsOn :: AdminClient -> Conn.BrokerAddress -> [(Text, Int32, Int64)] -> IO (Either String [DeleteRecordsResultEntry])
deleteRecordsOn client@AdminClient {..} brokerAddr entries = do
      let apiKey = 21 -- DeleteRecords
          -- DeleteRecords: codegen handles up to v2 (v2 went flexible).
          -- Request shape unchanged — same (topic, partition, offset,
          -- timeout) tuples at every version.
      withNegotiatedVersion client brokerAddr apiKey 0 2 1 $ \conn corrId apiVersion -> do
        -- Group partitions under their topic so we send one
        -- DeleteRecordsTopic per topic.
        let byTopic =
              Map.fromListWith
                (++)
                [(topic, [(part, off)]) | (topic, part, off) <- entries]
            topicsV =
              V.fromList $
                map
                  ( \(topic, parts) ->
                      DRReq.DeleteRecordsTopic
                        { DRReq.deleteRecordsTopicName = P.mkKafkaString topic
                        , DRReq.deleteRecordsTopicPartitions =
                            P.mkKafkaArray $
                              V.fromList $
                                map
                                  ( \(p, o) ->
                                      DRReq.DeleteRecordsPartition
                                        { DRReq.deleteRecordsPartitionPartitionIndex = p
                                        , DRReq.deleteRecordsPartitionOffset = o
                                        }
                                  )
                                  parts
                        }
                  )
                  (Map.toList byTopic)
            request =
              DRReq.DeleteRecordsRequest
                { DRReq.deleteRecordsRequestTopics = P.mkKafkaArray topicsV
                , DRReq.deleteRecordsRequestTimeoutMs = fromIntegral (adminRequestTimeoutMs adminConfig)
                }
            requestBody = WC.runEncodeVer @DRReq.DeleteRecordsRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "DeleteRecords request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @DRResp.DeleteRecordsResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse DeleteRecordsResponse: " ++ err
              Right response -> do
                let topicsVec = case P.unKafkaArray (DRResp.deleteRecordsResponseTopics response) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                    out = concatMap unpackTopic (V.toList topicsVec)
                return $ Right out
  where
    unpackTopic :: DRResp.DeleteRecordsTopicResult -> [DeleteRecordsResultEntry]
    unpackTopic t =
      let topic = extractText (DRResp.deleteRecordsTopicResultName t)
          parts = case P.unKafkaArray (DRResp.deleteRecordsTopicResultPartitions t) of
            P.Null -> V.empty
            P.NotNull v -> v
      in V.toList $
           V.map
             ( \p ->
                 DeleteRecordsResultEntry
                   { dreTopic = topic
                   , drePartition = DRResp.deleteRecordsPartitionResultPartitionIndex p
                   , dreLowWatermark = DRResp.deleteRecordsPartitionResultLowWatermark p
                   , dreErrorCode = DRResp.deleteRecordsPartitionResultErrorCode p
                   }
             )
             parts


-- * ElectLeaders


{- | Election type. 'PreferredElection' falls back to the
first replica in the assignment ("preferred"); 'UncleanElection'
promotes any in-sync replica even if it would lose data.
-}
data ElectionType
  = -- | Wire code 0
    PreferredElection
  | -- | Wire code 1
    UncleanElection
  deriving stock (Eq, Show, Generic)


{- | Ask the controller to (re-)elect leaders for the
supplied (topic, partition) list. An empty list elects every
partition that currently needs election.

Returns a list of (topic, partition, error-code) triples — one
per partition the controller responded about. @0@ means OK.
-}
electLeaders
  :: AdminClient
  -> ElectionType
  -> [(Text, Int32)]
  -> IO (Either String [(Text, Int32, Int16)])
electLeaders client@AdminClient {..} etype tps = do
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker : _) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 43 -- ElectLeaders
      withNegotiatedVersion client brokerAddr apiKey 0 2 2 $ \conn corrId apiVersion -> do
        let byTopic =
              Map.fromListWith
                (++)
                [(topic, [part]) | (topic, part) <- tps]
            topicsV =
              V.fromList $
                map
                  ( \(topic, parts) ->
                      ELReq.TopicPartitions
                        { ELReq.topicPartitionsTopic = P.mkKafkaString topic
                        , ELReq.topicPartitionsPartitions = P.mkKafkaArray (V.fromList parts)
                        }
                  )
                  (Map.toList byTopic)
            request =
              ELReq.ElectLeadersRequest
                { ELReq.electLeadersRequestElectionType = case etype of
                    PreferredElection -> 0
                    UncleanElection -> 1
                , ELReq.electLeadersRequestTopicPartitions = P.mkKafkaArray topicsV
                , ELReq.electLeadersRequestTimeoutMs = fromIntegral (adminRequestTimeoutMs adminConfig)
                }
            requestBody = WC.runEncodeVer @ELReq.ElectLeadersRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "ElectLeaders request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @ELResp.ElectLeadersResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse ElectLeadersResponse: " ++ err
              Right response -> do
                let resV = case P.unKafkaArray (ELResp.electLeadersResponseReplicaElectionResults response) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                    out = concatMap unpackTopic (V.toList resV)
                return $ Right out
  where
    unpackTopic :: ELResp.ReplicaElectionResult -> [(Text, Int32, Int16)]
    unpackTopic t =
      let topic = extractText (ELResp.replicaElectionResultTopic t)
          parts = case P.unKafkaArray (ELResp.replicaElectionResultPartitionResult t) of
            P.Null -> V.empty
            P.NotNull v -> v
      in V.toList $
           V.map
             ( \p ->
                 ( topic
                 , ELResp.partitionResultPartitionId p
                 , ELResp.partitionResultErrorCode p
                 )
             )
             parts


-- * Consumer-group offset management


{- | List every committed offset for a consumer group.

Returns a 'HashMap' keyed by @(topic, partition)@ containing
the broker's committed offset. Partitions with no committed
offset are absent.
-}
listConsumerGroupOffsets
  :: AdminClient
  -> Text
  -- ^ Group ID
  -> IO (Either String (HashMap (Text, Int32) Int64))
listConsumerGroupOffsets client@AdminClient {..} groupId = do
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker : _) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 9 -- OffsetFetch
          -- OffsetFetch: codegen handles up to v10. v6 went
          -- flexible. v7 added 'requireStable' which we always
          -- pass as False (matches the JVM client default). v8
          -- introduced the per-group 'groups[]' batched lookup
          -- shape, but our request continues to use the legacy
          -- single-group shape (groups = [], topics = Null) which
          -- the broker still honours through v10. v9/v10 added
          -- response fields ('topicAuthorizedOperations',
          -- KIP-941 'errorCode' on the per-group result) we
          -- decode but don't expose at the high-level
          -- surface — extra fields just round-trip and are
          -- ignored.
      withNegotiatedVersion client brokerAddr apiKey 0 7 5 $ \conn corrId apiVersion -> do
        -- KIP-211 / KIP-465: a /null/ topics array (not an empty
        -- one) is the broker's "fetch every committed offset for
        -- this group" sentinel. 'mkKafkaArray V.empty' produces an
        -- empty-but-non-null array, which the broker interprets
        -- as "no topics, no offsets". Build the Null variant
        -- explicitly.
        let request =
              OFReq.OffsetFetchRequest
                { OFReq.offsetFetchRequestGroupId = P.mkKafkaString groupId
                , OFReq.offsetFetchRequestTopics = P.KafkaArray P.Null
                , OFReq.offsetFetchRequestGroups = P.mkKafkaArray V.empty
                , OFReq.offsetFetchRequestRequireStable = False
                }
            requestBody = WC.runEncodeVer @OFReq.OffsetFetchRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "OffsetFetch request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @OFResp.OffsetFetchResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse OffsetFetchResponse: " ++ err
              Right response -> do
                let topicsVec = case P.unKafkaArray (OFResp.offsetFetchResponseTopics response) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                    go !acc tr =
                      let topic = extractText (OFResp.offsetFetchResponseTopicName tr)
                          partsVec = case P.unKafkaArray (OFResp.offsetFetchResponseTopicPartitions tr) of
                            P.Null -> V.empty
                            P.NotNull v -> v
                      in V.foldl'
                           ( \m p ->
                               let pid = OFResp.offsetFetchResponsePartitionPartitionIndex p
                                   ec = OFResp.offsetFetchResponsePartitionErrorCode p
                                   off = OFResp.offsetFetchResponsePartitionCommittedOffset p
                               in if ec == 0 && off >= 0
                                    then HashMap.insert (topic, pid) off m
                                    else m
                           )
                           acc
                           partsVec
                return $ Right $! V.foldl' go HashMap.empty topicsVec


{- | Write committed offsets for a group from /outside/
the consumer (e.g. a tool resetting a group). The group must
not have an active member when called; the broker rejects the
write otherwise.

Returns one entry per (topic, partition) the broker responded
about: @Right ()@ on success or @Left errorCode@ otherwise.
-}
alterConsumerGroupOffsets
  :: AdminClient
  -> Text
  -- ^ Group ID
  -> [(Text, Int32, Int64)]
  -- ^ (topic, partition, offset)
  -> IO (Either String [((Text, Int32), Either Int16 ())])
alterConsumerGroupOffsets _ _ [] = pure (Right [])
alterConsumerGroupOffsets client@AdminClient {..} groupId entries = do
  brokersM <- atomically $ Meta.getAllBrokers adminMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available"
    Just [] -> return $ Left "No brokers available"
    Just (broker : _) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
          apiKey = 8 -- OffsetCommit
          -- OffsetCommit: codegen handles up to v10. v6+ went
          -- flexible. v7 added 'groupInstanceId' (we send Null for
          -- the external-commit path — there's no group member to
          -- impersonate). v8 added per-topic 'topicId' (nullUuid =
          -- name-based lookup, broker compatible). v9+ moved to the
          -- KIP-848 member-epoch shape but the broker accepts
          -- generationId=-1 + empty memberId through v10 (the
          -- KIP-503 external-commit sentinel).
      withNegotiatedVersion client brokerAddr apiKey 0 9 5 $ \conn corrId apiVersion -> do
        let byTopic =
              Map.fromListWith
                (++)
                [(topic, [(part, off)]) | (topic, part, off) <- entries]
            topicsV =
              V.fromList $
                map
                  ( \(topic, parts) ->
                      OCReq.OffsetCommitRequestTopic
                        { OCReq.offsetCommitRequestTopicName = P.mkKafkaString topic
                        , OCReq.offsetCommitRequestTopicPartitions =
                            P.mkKafkaArray $
                              V.fromList $
                                map
                                  ( \(p, o) ->
                                      OCReq.OffsetCommitRequestPartition
                                        { OCReq.offsetCommitRequestPartitionPartitionIndex = p
                                        , OCReq.offsetCommitRequestPartitionCommittedOffset = o
                                        , OCReq.offsetCommitRequestPartitionCommittedLeaderEpoch = -1
                                        , OCReq.offsetCommitRequestPartitionCommittedMetadata = P.KafkaString P.Null
                                        }
                                  )
                                  parts
                        }
                  )
                  (Map.toList byTopic)
            -- KIP-503 / external offset commit: a memberId of
            -- the empty string is the broker's "no live group
            -- member" sentinel. The field is marked non-nullable
            -- in the spec, so we MUST send an empty string
            -- rather than a null. groupInstanceId stays null
            -- (we're not impersonating a static member) and the
            -- generation id is -1.
            request =
              OCReq.OffsetCommitRequest
                { OCReq.offsetCommitRequestGroupId = P.mkKafkaString groupId
                , OCReq.offsetCommitRequestGenerationIdOrMemberEpoch = -1
                , OCReq.offsetCommitRequestMemberId = P.mkKafkaString ""
                , OCReq.offsetCommitRequestGroupInstanceId = P.KafkaString P.Null
                , OCReq.offsetCommitRequestRetentionTimeMs = -1
                , OCReq.offsetCommitRequestTopics = P.mkKafkaArray topicsV
                }
            requestBody = WC.runEncodeVer @OCReq.OffsetCommitRequest apiVersion request
            clientIdKafka = P.mkKafkaString (adminClientId adminConfig)
        result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
        case result of
          Left err -> return $ Left $ "OffsetCommit request failed: " ++ err
          Right (_, responseBody) ->
            case WC.runDecodeVer @OCResp.OffsetCommitResponse apiVersion responseBody of
              Left err -> return $ Left $ "Failed to parse OffsetCommitResponse: " ++ err
              Right response -> do
                let topicsVec = case P.unKafkaArray (OCResp.offsetCommitResponseTopics response) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                    out = concatMap unpackTopic (V.toList topicsVec)
                return $ Right out
  where
    unpackTopic
      :: OCResp.OffsetCommitResponseTopic
      -> [((Text, Int32), Either Int16 ())]
    unpackTopic t =
      let topic = extractText (OCResp.offsetCommitResponseTopicName t)
          parts = case P.unKafkaArray (OCResp.offsetCommitResponseTopicPartitions t) of
            P.Null -> V.empty
            P.NotNull v -> v
      in V.toList $
           V.map
             ( \p ->
                 let pid = OCResp.offsetCommitResponsePartitionPartitionIndex p
                     ec = OCResp.offsetCommitResponsePartitionErrorCode p
                 in ( (topic, pid)
                    , if ec == 0 then Right () else Left ec
                    )
             )
             parts


-- * Helper Functions


-- | Extract Text from a KafkaString
extractText :: P.KafkaString -> Text
extractText (P.KafkaString P.Null) = ""
extractText (P.KafkaString (P.NotNull t)) = t


----------------------------------------------------------------------
-- Additional ergonomics
--
-- Previously lived in @Kafka.Client.AdminExtras@.
----------------------------------------------------------------------

-- | Mirrors @AdminClient.DEFAULT_API_TIMEOUT_MS@ in the JVM client.
defaultAdminApiTimeoutMs :: Int
defaultAdminApiTimeoutMs = 60_000


-- | Defaults applied when creating a topic without explicit knobs.
data TopicCreateDefaults = TopicCreateDefaults
  { tcdReplicationFactor :: !Int
  , tcdNumPartitions :: !Int32
  , tcdConfigOverrides :: !(Map Text Text)
  {- ^ Topic-level config overrides applied to every newly
  created topic when the caller doesn't override them.
  -}
  }
  deriving stock (Eq, Show, Generic)


defaultTopicCreateDefaults :: TopicCreateDefaults
defaultTopicCreateDefaults =
  TopicCreateDefaults
    { tcdReplicationFactor = 1
    , tcdNumPartitions = 1
    , tcdConfigOverrides = Map.empty
    }


{- | What to do when a producer sends a 'Nothing' key to a
compacted topic. The default Kafka behaviour is to reject
with @INVALID_RECORD@; newer brokers can treat missing keys as
tombstones / pass-through.
-}
data NullKeyCompactionPolicy
  = NkcReject
  | NkcTombstone
  | NkcPassThrough
  deriving stock (Eq, Show, Generic)


defaultNullKeyCompactionPolicy :: NullKeyCompactionPolicy
defaultNullKeyCompactionPolicy = NkcReject


-- Canonical telemetry metric names for the admin-client operations.
adminListTopicsLatencyMs
  , adminCreateTopicsLatencyMs
  , adminDescribeGroupsLatencyMs
  , adminAlterConfigsLatencyMs
  , adminDeleteRecordsLatencyMs
    :: Text
adminListTopicsLatencyMs = "kafka.admin.list-topics.latency.ms"
adminCreateTopicsLatencyMs = "kafka.admin.create-topics.latency.ms"
adminDescribeGroupsLatencyMs = "kafka.admin.describe-groups.latency.ms"
adminAlterConfigsLatencyMs = "kafka.admin.alter-configs.latency.ms"
adminDeleteRecordsLatencyMs = "kafka.admin.delete-records.latency.ms"


----------------------------------------------------------------------
-- SPECIALIZE pragmas for the IO hot path
--
-- See "Kafka.Client.Producer" for the rationale.
----------------------------------------------------------------------

{-# INLINEABLE createAdminClient #-}
{-# SPECIALIZE createAdminClient :: [Text] -> AdminClientConfig -> IO (Either String AdminClient) #-}


{-# INLINEABLE closeAdminClient #-}
{-# SPECIALIZE closeAdminClient :: AdminClient -> IO () #-}


{-# INLINEABLE createTopics #-}
{-# SPECIALIZE createTopics :: AdminClient -> [NewTopic] -> IO (Either String [(Text, Either String ())]) #-}


{-# INLINEABLE ensureTopic #-}
{-# SPECIALIZE ensureTopic :: AdminClient -> NewTopic -> IO (Either String ()) #-}


{-# INLINEABLE deleteTopics #-}
{-# SPECIALIZE deleteTopics :: AdminClient -> [Text] -> IO (Either String [(Text, Either String ())]) #-}


{-# INLINEABLE listTopics #-}
{-# SPECIALIZE listTopics :: AdminClient -> IO (Either String [Text]) #-}


{-# INLINEABLE describeTopics #-}
{-# SPECIALIZE describeTopics :: AdminClient -> [Text] -> IO (Either String [TopicDescription]) #-}


{-# INLINEABLE listTopicsExcludeInternal #-}
{-# SPECIALIZE listTopicsExcludeInternal :: AdminClient -> IO (Either String [Text]) #-}


{-# INLINEABLE listConsumerGroups #-}
{-# SPECIALIZE listConsumerGroups :: AdminClient -> IO (Either String [ConsumerGroupListing]) #-}


{-# INLINEABLE describeConsumerGroups #-}
{-# SPECIALIZE describeConsumerGroups :: AdminClient -> [Text] -> IO (Either String [ConsumerGroupDescription]) #-}


{-# INLINEABLE deleteConsumerGroups #-}
{-# SPECIALIZE deleteConsumerGroups :: AdminClient -> [Text] -> IO (Either String [(Text, Either String ())]) #-}


{-# INLINEABLE describeConfigs #-}
{-# SPECIALIZE describeConfigs :: AdminClient -> [ConfigResource] -> IO (Either String [ConfigResourceResult]) #-}


{-# INLINEABLE alterConfigs #-}
{-# SPECIALIZE alterConfigs :: AdminClient -> [(ConfigResource, [(Text, Text)])] -> IO (Either String [(ConfigResource, Either String ())]) #-}


{-# INLINEABLE incrementalAlterConfigs #-}
{-# SPECIALIZE incrementalAlterConfigs :: AdminClient -> [(ConfigResource, [AlterableConfigEntry])] -> IO (Either String [(ConfigResource, Either String ())]) #-}


{-# INLINEABLE deleteRecords #-}
{-# SPECIALIZE deleteRecords :: AdminClient -> [(Text, Int32, Int64)] -> IO (Either String [DeleteRecordsResultEntry]) #-}


{-# INLINEABLE adminClusterId #-}
{-# SPECIALIZE adminClusterId :: AdminClient -> IO (Maybe Text) #-}


----------------------------------------------------------------------
-- Long-tail admin RPCs (folded in from the older
-- 'Kafka.Client.AdminClient.Extras' module). Coverage:
--   * createPartitions (KIP-195)
--   * describeCluster (KIP-700)
--   * listGroups (KIP-848 generic)
--   * createAcls / describeAcls / deleteAcls (KIP-50)
--   * alter/listPartitionReassignments (KIP-455)
--   * unregisterBroker (KIP-704)
--   * describe/alterClientQuotas (KIP-546)
--   * list/describeTransactions (KIP-664)
--   * describe/alterUserScramCredentials (KIP-554)
--   * describeProducers / log dirs / delegation tokens
--   * add/removeRaftVoter (KIP-853)
--   * describeMetadataQuorum
--   * removeMembersFromConsumerGroup (KIP-345)
----------------------------------------------------------------------

----------------------------------------------------------------------
-- createPartitions
----------------------------------------------------------------------

{- | A request to add partitions to an existing topic. Mirrors
@org.apache.kafka.clients.admin.NewPartitions@. The
@newAssignments@ field is optional; @Nothing@ asks the broker
to assign partitions itself.
-}
data NewPartitions = NewPartitions
  { npTopicName :: !Text
  , npTotalCount :: !Int32
  {- ^ The /new total/ partition count, not the delta — matches
  the JVM semantics of @NewPartitions.increaseTo@.
  -}
  , npNewAssignments :: !(Maybe [[Int32]])
  {- ^ For each new partition, the broker ids to host it on.
  @Nothing@ delegates the assignment to the broker.
  -}
  }
  deriving stock (Eq, Show)


{- | Increase the partition count of one or more topics. Mirrors
@Admin.createPartitions(Map<String, NewPartitions>)@.

Returns a list of @(topicName, result)@ pairs.
-}
createPartitions
  :: MonadIO m
  => AdminClient
  -> [NewPartitions]
  -> m (Either String [(Text, Either String ())])
createPartitions client partitions = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 37 0 3 0 $ \conn corrId apiVer -> do
        let req =
              CPReq.CreatePartitionsRequest
                { CPReq.createPartitionsRequestTopics =
                    P.mkKafkaArray (V.fromList (map buildTopic partitions))
                , CPReq.createPartitionsRequestTimeoutMs = 30000
                , CPReq.createPartitionsRequestValidateOnly = False
                }
            body = WC.runEncodeVer @CPReq.CreatePartitionsRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 37 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("CreatePartitions request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @CPResp.CreatePartitionsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse CreatePartitionsResponse: " <> e))
              Right resp -> do
                let rs = case P.unKafkaArray (CPResp.createPartitionsResponseResults resp) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                pure $ Right (V.toList (V.map handleTopic rs))
  where
    buildTopic NewPartitions {..} =
      let !assignments = case npNewAssignments of
            Nothing -> V.empty
            Just xss ->
              V.fromList
                [ CPReq.CreatePartitionsAssignment
                    { CPReq.createPartitionsAssignmentBrokerIds =
                        P.mkKafkaArray (V.fromList ids)
                    }
                | ids <- xss
                ]
      in CPReq.CreatePartitionsTopic
           { CPReq.createPartitionsTopicName = P.mkKafkaString npTopicName
           , CPReq.createPartitionsTopicCount = npTotalCount
           , CPReq.createPartitionsTopicAssignments = P.mkKafkaArray assignments
           }

    handleTopic r =
      let !name = extractText (CPResp.createPartitionsTopicResultName r)
          !code = CPResp.createPartitionsTopicResultErrorCode r
          !msg = extractText (CPResp.createPartitionsTopicResultErrorMessage r)
      in if code == 0
           then (name, Right ())
           else (name, Left ("Error " <> show code <> ": " <> T.unpack msg))


----------------------------------------------------------------------
-- describeCluster
----------------------------------------------------------------------

{- | Get information about the nodes in the cluster. Mirrors
@Admin.describeCluster()@. Returns a 'Common.Cluster' value:
the cluster id, the broker list, the controller (if known),
and the requesting principal's cluster-level authorized
operations.
-}
describeCluster :: MonadIO m => AdminClient -> m (Either String Cluster)
describeCluster client = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 60 0 2 0 $ \conn corrId apiVer -> do
        let req =
              DSReq.DescribeClusterRequest
                { DSReq.describeClusterRequestIncludeClusterAuthorizedOperations = False
                , DSReq.describeClusterRequestEndpointType = 1
                , DSReq.describeClusterRequestIncludeFencedBrokers = False
                }
            body = WC.runEncodeVer @DSReq.DescribeClusterRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 60 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("DescribeCluster request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @DSResp.DescribeClusterResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse DescribeClusterResponse: " <> e))
              Right resp ->
                if DSResp.describeClusterResponseErrorCode resp /= 0
                  then
                    pure $
                      Left $
                        "DescribeCluster: "
                          <> T.unpack
                            (extractText (DSResp.describeClusterResponseErrorMessage resp))
                  else do
                    let brokers = case P.unKafkaArray (DSResp.describeClusterResponseBrokers resp) of
                          P.Null -> V.empty
                          P.NotNull v -> v
                        !nodes = V.toList (V.map decodeBroker brokers)
                        !cid' = extractText (DSResp.describeClusterResponseClusterId resp)
                        !cidM = if T.null cid' then Nothing else Just cid'
                        !ctlId = DSResp.describeClusterResponseControllerId resp
                        !ctl =
                          lookup
                            (fromIntegral ctlId)
                            [(Common.nodeId n, n) | n <- nodes]
                    pure $
                      Right
                        Common.emptyCluster
                          { clusterId = cidM
                          , clusterNodes = nodes
                          , clusterController = ctl
                          }
  where
    decodeBroker b =
      Node
        { nodeId = fromIntegral (DSResp.describeClusterBrokerBrokerId b)
        , nodeHost = extractText (DSResp.describeClusterBrokerHost b)
        , nodePort = fromIntegral (DSResp.describeClusterBrokerPort b)
        , nodeRack =
            let r = extractText (DSResp.describeClusterBrokerRack b)
            in if T.null r then Nothing else Just r
        }


----------------------------------------------------------------------
-- listGroups (KIP-848 generic)
----------------------------------------------------------------------

{- | A row from 'listGroups'. The KIP-848 generic shape carries
the group type ('Common.GroupType') and current state
('Common.GroupState'), not just the id; for backwards
compatibility 'glType' / 'glState' are 'Nothing' against
pre-3.7 brokers.
-}
data GroupListing = GroupListing
  { glGroupId :: !Text
  , glProtocolType :: !Text
  , glState :: !(Maybe Common.GroupState)
  , glType :: !(Maybe Common.GroupType)
  }
  deriving stock (Eq, Show)


{- | List every group on the cluster. The two filters are
broker-side: empty lists mean "match everything".
-}
listGroups
  :: MonadIO m
  => AdminClient
  -> [Common.GroupState]
  -- ^ filter by state
  -> [Common.GroupType]
  -- ^ filter by group type (KIP-848)
  -> m (Either String [GroupListing])
listGroups client states types = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 16 0 5 0 $ \conn corrId apiVer -> do
        let !stateNames =
              V.fromList
                [P.mkKafkaString (groupStateText s) | s <- states]
            !typeNames =
              V.fromList
                [P.mkKafkaString (groupTypeText t) | t <- types]
            req =
              LGReq.ListGroupsRequest
                { LGReq.listGroupsRequestStatesFilter = P.mkKafkaArray stateNames
                , LGReq.listGroupsRequestTypesFilter = P.mkKafkaArray typeNames
                }
            body = WC.runEncodeVer @LGReq.ListGroupsRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 16 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("ListGroups request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @LGResp.ListGroupsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse ListGroupsResponse: " <> e))
              Right resp ->
                if LGResp.listGroupsResponseErrorCode resp /= 0
                  then
                    pure $
                      Left $
                        "ListGroups error code: "
                          <> show (LGResp.listGroupsResponseErrorCode resp)
                  else do
                    let gs = case P.unKafkaArray (LGResp.listGroupsResponseGroups resp) of
                          P.Null -> V.empty
                          P.NotNull v -> v
                    pure $ Right (V.toList (V.map decodeGroup gs))
  where
    decodeGroup g =
      GroupListing
        { glGroupId = extractText (LGResp.listedGroupGroupId g)
        , glProtocolType = extractText (LGResp.listedGroupProtocolType g)
        , glState =
            let st = extractText (LGResp.listedGroupGroupState g)
            in groupStateFromText st
        , glType =
            let ty = extractText (LGResp.listedGroupGroupType g)
            in groupTypeFromText ty
        }


----------------------------------------------------------------------
-- ACL admin
----------------------------------------------------------------------

-- | Per-binding result of 'createAcls'.
data AclCreationResult = AclCreationResult
  { acrBinding :: !Acl.AclBinding
  , acrError :: !(Maybe Text)
  }
  deriving stock (Eq, Show)


{- | Per-filter result of 'deleteAcls' — a count of bindings
deleted plus an optional error message.
-}
data AclDeletionResult = AclDeletionResult
  { adrDeletedCount :: !Int
  , adrError :: !(Maybe Text)
  }
  deriving stock (Eq, Show)


{- | Create one or more ACL bindings. Mirrors
@Admin.createAcls(Collection<AclBinding>)@.
-}
createAcls
  :: MonadIO m
  => AdminClient
  -> [Acl.AclBinding]
  -> m (Either String [AclCreationResult])
createAcls client bindings = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 30 1 3 1 $ \conn corrId apiVer -> do
        let !creations = V.fromList (map buildCreation bindings)
            req =
              CAReq.CreateAclsRequest
                { CAReq.createAclsRequestCreations = P.mkKafkaArray creations
                }
            body = WC.runEncodeVer @CAReq.CreateAclsRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 30 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("CreateAcls request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @CAResp.CreateAclsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse CreateAclsResponse: " <> e))
              Right resp -> do
                let rs = case P.unKafkaArray (CAResp.createAclsResponseResults resp) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                    results = V.zipWith handleResult (V.fromList bindings) rs
                pure (Right (V.toList results))
  where
    buildCreation (Acl.AclBinding pat entry) =
      CAReq.AclCreation
        { CAReq.aclCreationResourceType =
            resourceTypeCode (Resource.rpResourceType pat)
        , CAReq.aclCreationResourceName =
            P.mkKafkaString (Resource.rpName pat)
        , CAReq.aclCreationResourcePatternType =
            patternTypeCode (Resource.rpPatternType pat)
        , CAReq.aclCreationPrincipal =
            P.mkKafkaString (Acl.aceePrincipal entry)
        , CAReq.aclCreationHost =
            P.mkKafkaString (Acl.aceeHost entry)
        , CAReq.aclCreationOperation =
            aclOperationCode (Acl.aceeOperation entry)
        , CAReq.aclCreationPermissionType =
            aclPermissionCode (Acl.aceePermissionType entry)
        }
    handleResult b r =
      let !code = CAResp.aclCreationResultErrorCode r
          !msg = extractText (CAResp.aclCreationResultErrorMessage r)
      in AclCreationResult b $
           if code == 0
             then Nothing
             else Just (T.pack ("Error " <> show code <> ": ") <> msg)


{- | List the bindings matching a filter. Mirrors
@Admin.describeAcls(AclBindingFilter)@.
-}
describeAcls
  :: MonadIO m
  => AdminClient
  -> Acl.AclBindingFilter
  -> m (Either String [Acl.AclBinding])
describeAcls client (Acl.AclBindingFilter patFilter entryFilter) = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 29 1 3 1 $ \conn corrId apiVer -> do
        let req =
              DAReq.DescribeAclsRequest
                { DAReq.describeAclsRequestResourceTypeFilter =
                    resourceTypeCode (Resource.rpfResourceType patFilter)
                , DAReq.describeAclsRequestResourceNameFilter =
                    P.mkKafkaString
                      (maybe T.empty id (Resource.rpfName patFilter))
                , DAReq.describeAclsRequestPatternTypeFilter =
                    patternTypeCode (Resource.rpfPatternType patFilter)
                , DAReq.describeAclsRequestPrincipalFilter =
                    P.mkKafkaString
                      (maybe T.empty id (Acl.acefPrincipal entryFilter))
                , DAReq.describeAclsRequestHostFilter =
                    P.mkKafkaString
                      (maybe T.empty id (Acl.acefHost entryFilter))
                , DAReq.describeAclsRequestOperation =
                    aclOperationCode (Acl.acefOperation entryFilter)
                , DAReq.describeAclsRequestPermissionType =
                    aclPermissionCode (Acl.acefPermissionType entryFilter)
                }
            body = WC.runEncodeVer @DAReq.DescribeAclsRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 29 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("DescribeAcls request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @DAResp.DescribeAclsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse DescribeAclsResponse: " <> e))
              Right resp ->
                if DAResp.describeAclsResponseErrorCode resp /= 0
                  then
                    pure $
                      Left $
                        "DescribeAcls: "
                          <> T.unpack
                            (extractText (DAResp.describeAclsResponseErrorMessage resp))
                  else do
                    let resVec = case P.unKafkaArray (DAResp.describeAclsResponseResources resp) of
                          P.Null -> V.empty
                          P.NotNull v -> v
                    pure $ Right $ concatMap flattenResource (V.toList resVec)
  where
    flattenResource r =
      let !rt = resourceTypeFromCode (DAResp.describeAclsResourceResourceType r)
          !nm = extractText (DAResp.describeAclsResourceResourceName r)
          !pt = patternTypeFromCode (DAResp.describeAclsResourcePatternType r)
          !pat =
            Resource.ResourcePattern
              { Resource.rpResourceType = rt
              , Resource.rpName = nm
              , Resource.rpPatternType = pt
              }
          aces = case P.unKafkaArray (DAResp.describeAclsResourceAcls r) of
            P.Null -> V.empty
            P.NotNull v -> v
      in V.toList (V.map (\a -> Acl.AclBinding pat (flattenAce a)) aces)

    flattenAce a =
      Acl.AccessControlEntry
        { Acl.aceePrincipal = extractText (DAResp.aclDescriptionPrincipal a)
        , Acl.aceeHost = extractText (DAResp.aclDescriptionHost a)
        , Acl.aceeOperation = aclOperationFromCode (DAResp.aclDescriptionOperation a)
        , Acl.aceePermissionType = aclPermissionFromCode (DAResp.aclDescriptionPermissionType a)
        }


{- | Delete the bindings matching the supplied filters. Mirrors
@Admin.deleteAcls(Collection<AclBindingFilter>)@.
-}
deleteAcls
  :: MonadIO m
  => AdminClient
  -> [Acl.AclBindingFilter]
  -> m (Either String [AclDeletionResult])
deleteAcls client filters = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 31 1 3 1 $ \conn corrId apiVer -> do
        let !filterVec = V.fromList (map buildFilter filters)
            req =
              DelAReq.DeleteAclsRequest
                { DelAReq.deleteAclsRequestFilters = P.mkKafkaArray filterVec
                }
            body = WC.runEncodeVer @DelAReq.DeleteAclsRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 31 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("DeleteAcls request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @DelAResp.DeleteAclsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse DeleteAclsResponse: " <> e))
              Right resp -> do
                let rs = case P.unKafkaArray (DelAResp.deleteAclsResponseFilterResults resp) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                pure (Right (V.toList (V.map handleFilter rs)))
  where
    buildFilter (Acl.AclBindingFilter pat entry) =
      DelAReq.DeleteAclsFilter
        { DelAReq.deleteAclsFilterResourceTypeFilter =
            resourceTypeCode (Resource.rpfResourceType pat)
        , DelAReq.deleteAclsFilterResourceNameFilter =
            P.mkKafkaString (maybe T.empty id (Resource.rpfName pat))
        , DelAReq.deleteAclsFilterPatternTypeFilter =
            patternTypeCode (Resource.rpfPatternType pat)
        , DelAReq.deleteAclsFilterPrincipalFilter =
            P.mkKafkaString (maybe T.empty id (Acl.acefPrincipal entry))
        , DelAReq.deleteAclsFilterHostFilter =
            P.mkKafkaString (maybe T.empty id (Acl.acefHost entry))
        , DelAReq.deleteAclsFilterOperation =
            aclOperationCode (Acl.acefOperation entry)
        , DelAReq.deleteAclsFilterPermissionType =
            aclPermissionCode (Acl.acefPermissionType entry)
        }
    handleFilter f =
      let !code = DelAResp.deleteAclsFilterResultErrorCode f
          !msg = extractText (DelAResp.deleteAclsFilterResultErrorMessage f)
          matches = case P.unKafkaArray (DelAResp.deleteAclsFilterResultMatchingAcls f) of
            P.Null -> 0
            P.NotNull v -> V.length v
      in AclDeletionResult
           { adrDeletedCount = matches
           , adrError =
               if code == 0
                 then Nothing
                 else Just (T.pack ("Error " <> show code <> ": ") <> msg)
           }


----------------------------------------------------------------------
-- helpers
----------------------------------------------------------------------

pickBroker :: AdminClient -> IO (Either String Conn.BrokerAddress)
pickBroker client = do
  let meta = adminMetadataOf client
  mbs <- atomically (Meta.getAllBrokers meta)
  case mbs of
    Nothing -> pure (Left "No brokers available")
    Just [] -> pure (Left "No brokers available")
    Just (b : _) -> pure (Right (Meta.brokerMetaAddress b))


clientIdOf :: AdminClient -> P.KafkaString
clientIdOf client =
  P.mkKafkaString (adminClientId (adminConfigOf client))


----------------------------------------------------------------------
-- enum <-> wire-code conversions
----------------------------------------------------------------------

resourceTypeCode :: Resource.ResourceType -> Int8
resourceTypeCode = \case
  Resource.ResourceUnknown -> 0
  Resource.ResourceAny -> 1
  Resource.ResourceTopic -> 2
  Resource.ResourceGroup -> 3
  Resource.ResourceCluster -> 4
  Resource.ResourceTransactionalId -> 5
  Resource.ResourceDelegationToken -> 6
  Resource.ResourceUser -> 7


resourceTypeFromCode :: Int8 -> Resource.ResourceType
resourceTypeFromCode = \case
  1 -> Resource.ResourceAny
  2 -> Resource.ResourceTopic
  3 -> Resource.ResourceGroup
  4 -> Resource.ResourceCluster
  5 -> Resource.ResourceTransactionalId
  6 -> Resource.ResourceDelegationToken
  7 -> Resource.ResourceUser
  _ -> Resource.ResourceUnknown


patternTypeCode :: Resource.PatternType -> Int8
patternTypeCode = \case
  Resource.PatternUnknown -> 0
  Resource.PatternAny -> 1
  Resource.PatternMatch -> 2
  Resource.PatternLiteral -> 3
  Resource.PatternPrefixed -> 4


patternTypeFromCode :: Int8 -> Resource.PatternType
patternTypeFromCode = \case
  1 -> Resource.PatternAny
  2 -> Resource.PatternMatch
  3 -> Resource.PatternLiteral
  4 -> Resource.PatternPrefixed
  _ -> Resource.PatternUnknown


aclOperationCode :: Acl.AclOperation -> Int8
aclOperationCode = \case
  Acl.AclUnknownOp -> 0
  Acl.AclAnyOp -> 1
  Acl.AclAll -> 2
  Acl.AclRead -> 3
  Acl.AclWrite -> 4
  Acl.AclCreate -> 5
  Acl.AclDelete -> 6
  Acl.AclAlter -> 7
  Acl.AclDescribe -> 8
  Acl.AclClusterAction -> 9
  Acl.AclDescribeConfigs -> 10
  Acl.AclAlterConfigs -> 11
  Acl.AclIdempotentWrite -> 12
  Acl.AclCreateTokens -> 13
  Acl.AclDescribeTokens -> 14
  Acl.AclTwoPhaseCommit -> 15


aclOperationFromCode :: Int8 -> Acl.AclOperation
aclOperationFromCode = \case
  1 -> Acl.AclAnyOp
  2 -> Acl.AclAll
  3 -> Acl.AclRead
  4 -> Acl.AclWrite
  5 -> Acl.AclCreate
  6 -> Acl.AclDelete
  7 -> Acl.AclAlter
  8 -> Acl.AclDescribe
  9 -> Acl.AclClusterAction
  10 -> Acl.AclDescribeConfigs
  11 -> Acl.AclAlterConfigs
  12 -> Acl.AclIdempotentWrite
  13 -> Acl.AclCreateTokens
  14 -> Acl.AclDescribeTokens
  15 -> Acl.AclTwoPhaseCommit
  _ -> Acl.AclUnknownOp


aclPermissionCode :: Acl.AclPermissionType -> Int8
aclPermissionCode = \case
  Acl.AclUnknownPerm -> 0
  Acl.AclAnyPerm -> 1
  Acl.AclDeny -> 2
  Acl.AclAllow -> 3


aclPermissionFromCode :: Int8 -> Acl.AclPermissionType
aclPermissionFromCode = \case
  1 -> Acl.AclAnyPerm
  2 -> Acl.AclDeny
  3 -> Acl.AclAllow
  _ -> Acl.AclUnknownPerm


groupStateText :: Common.GroupState -> Text
groupStateText = \case
  Common.GroupUnknownState -> "UNKNOWN"
  Common.GroupAssigning -> "ASSIGNING"
  Common.GroupReconciling -> "RECONCILING"
  Common.GroupStable -> "STABLE"
  Common.GroupDead -> "DEAD"
  Common.GroupEmpty -> "EMPTY"


groupStateFromText :: Text -> Maybe Common.GroupState
groupStateFromText t = case T.toUpper t of
  "UNKNOWN" -> Just Common.GroupUnknownState
  "ASSIGNING" -> Just Common.GroupAssigning
  "RECONCILING" -> Just Common.GroupReconciling
  "STABLE" -> Just Common.GroupStable
  "DEAD" -> Just Common.GroupDead
  "EMPTY" -> Just Common.GroupEmpty
  _ -> Nothing


groupTypeText :: Common.GroupType -> Text
groupTypeText = \case
  Common.ClassicGroup -> "classic"
  Common.ConsumerGroup -> "consumer"
  Common.ShareGroup -> "share"


groupTypeFromText :: Text -> Maybe Common.GroupType
groupTypeFromText t = case T.toLower t of
  "classic" -> Just Common.ClassicGroup
  "consumer" -> Just Common.ConsumerGroup
  "share" -> Just Common.ShareGroup
  _ -> Nothing


{- | Unwrap a wire 'P.KafkaBytes' to a strict 'ByteString'. The
protocol type carries a 'NotNull' tag; we collapse the 'Null'
case to an empty 'ByteString' since the broker only emits
'NotNull' for the fields we read here.
-}
fromKB :: P.KafkaBytes -> ByteString
fromKB (P.KafkaBytes (P.NotNull bs)) = bs
fromKB (P.KafkaBytes P.Null) = mempty


----------------------------------------------------------------------
-- Partition reassignment (KIP-455)
----------------------------------------------------------------------

{- | A per-partition reassignment request. @prsTargetReplicas =
Nothing@ means "cancel any in-flight reassignment for this
partition".
-}
data PartitionReassignmentSpec = PartitionReassignmentSpec
  { prsTopic :: !Text
  , prsPartition :: !Int32
  , prsTargetReplicas :: !(Maybe [Int32])
  }
  deriving stock (Eq, Show)


{- | An in-flight partition reassignment from
'listPartitionReassignments'. Mirrors
@PartitionReassignment@ in the JVM SDK.
-}
data OngoingPartitionReassignment = OngoingPartitionReassignment
  { oprTopic :: !Text
  , oprPartition :: !Int32
  , oprCurrentReplicas :: ![Int32]
  , oprAddingReplicas :: ![Int32]
  , oprRemovingReplicas :: ![Int32]
  }
  deriving stock (Eq, Show)


{- | Alter (or cancel) partition reassignments. Mirrors
@Admin.alterPartitionReassignments(Map<TopicPartition, Optional<NewPartitionReassignment>>)@.
-}
alterPartitionReassignments
  :: MonadIO m
  => AdminClient
  -> [PartitionReassignmentSpec]
  -> m (Either String [(Text, Int32, Either String ())])
alterPartitionReassignments client specs = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 45 0 0 0 $ \conn corrId apiVer -> do
        let grouped = groupByTopic specs
            !topics = V.fromList (map buildTopic grouped)
            req =
              APRReq.AlterPartitionReassignmentsRequest
                { APRReq.alterPartitionReassignmentsRequestTimeoutMs = 30000
                , APRReq.alterPartitionReassignmentsRequestTopics = P.mkKafkaArray topics
                }
            body = WC.runEncodeVer @APRReq.AlterPartitionReassignmentsRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 45 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("AlterPartitionReassignments request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @APRResp.AlterPartitionReassignmentsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if APRResp.alterPartitionReassignmentsResponseErrorCode resp /= 0
                  then
                    pure $
                      Left $
                        "AlterPartitionReassignments: "
                          <> T.unpack
                            (extractText (APRResp.alterPartitionReassignmentsResponseErrorMessage resp))
                  else do
                    let topicRs = case P.unKafkaArray (APRResp.alterPartitionReassignmentsResponseResponses resp) of
                          P.Null -> V.empty
                          P.NotNull v -> v
                    pure $ Right $ concatMap flattenTopic (V.toList topicRs)
  where
    groupByTopic xs =
      let byT = foldr (\s acc -> Map.insertWith (++) (prsTopic s) [s] acc) Map.empty xs
      in Map.toList byT

    buildTopic (topic, rs) =
      APRReq.ReassignableTopic
        { APRReq.reassignableTopicName = P.mkKafkaString topic
        , APRReq.reassignableTopicPartitions =
            P.mkKafkaArray (V.fromList (map buildPartition rs))
        }
    buildPartition s =
      APRReq.ReassignablePartition
        { APRReq.reassignablePartitionPartitionIndex = prsPartition s
        , APRReq.reassignablePartitionReplicas =
            case prsTargetReplicas s of
              Just rs -> P.mkKafkaArray (V.fromList rs)
              Nothing -> P.mkKafkaArray V.empty
        }
    flattenTopic t =
      let !nm = extractText (APRResp.reassignableTopicResponseName t)
          ps = case P.unKafkaArray (APRResp.reassignableTopicResponsePartitions t) of
            P.Null -> V.empty
            P.NotNull v -> v
      in V.toList (V.map (decodePart nm) ps)
    decodePart nm p =
      let !pi_ = APRResp.reassignablePartitionResponsePartitionIndex p
          !code = APRResp.reassignablePartitionResponseErrorCode p
          !msg = extractText (APRResp.reassignablePartitionResponseErrorMessage p)
      in if code == 0
           then (nm, pi_, Right ())
           else (nm, pi_, Left ("Error " <> show code <> ": " <> T.unpack msg))


{- | List in-flight partition reassignments. Passing 'Nothing'
asks for /every/ reassignment in the cluster; @'Just' tps@ scopes
to specific partitions.
-}
listPartitionReassignments
  :: MonadIO m
  => AdminClient
  -> Maybe [(Text, [Int32])]
  -- ^ topic + partition selector
  -> m (Either String [OngoingPartitionReassignment])
listPartitionReassignments client mScope = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 46 0 0 0 $ \conn corrId apiVer -> do
        let !topicsArr = case mScope of
              Nothing -> P.mkKafkaArray V.empty
              Just sc ->
                P.mkKafkaArray $
                  V.fromList
                    [ LPRReq.ListPartitionReassignmentsTopics
                        { LPRReq.listPartitionReassignmentsTopicsName =
                            P.mkKafkaString t
                        , LPRReq.listPartitionReassignmentsTopicsPartitionIndexes =
                            P.mkKafkaArray (V.fromList ps)
                        }
                    | (t, ps) <- sc
                    ]
            req =
              LPRReq.ListPartitionReassignmentsRequest
                { LPRReq.listPartitionReassignmentsRequestTimeoutMs = 30000
                , LPRReq.listPartitionReassignmentsRequestTopics = topicsArr
                }
            body = WC.runEncodeVer @LPRReq.ListPartitionReassignmentsRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 46 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("ListPartitionReassignments request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @LPRResp.ListPartitionReassignmentsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if LPRResp.listPartitionReassignmentsResponseErrorCode resp /= 0
                  then
                    pure $
                      Left $
                        "ListPartitionReassignments: "
                          <> T.unpack
                            (extractText (LPRResp.listPartitionReassignmentsResponseErrorMessage resp))
                  else do
                    let topicRs = case P.unKafkaArray (LPRResp.listPartitionReassignmentsResponseTopics resp) of
                          P.Null -> V.empty
                          P.NotNull v -> v
                    pure $ Right $ concatMap flattenT (V.toList topicRs)
  where
    flattenT t =
      let !nm = extractText (LPRResp.ongoingTopicReassignmentName t)
          ps = case P.unKafkaArray (LPRResp.ongoingTopicReassignmentPartitions t) of
            P.Null -> V.empty
            P.NotNull v -> v
      in V.toList (V.map (decodeP nm) ps)
    decodeP nm p =
      OngoingPartitionReassignment
        { oprTopic = nm
        , oprPartition = LPRResp.ongoingPartitionReassignmentPartitionIndex p
        , oprCurrentReplicas = unArr (LPRResp.ongoingPartitionReassignmentReplicas p)
        , oprAddingReplicas = unArr (LPRResp.ongoingPartitionReassignmentAddingReplicas p)
        , oprRemovingReplicas = unArr (LPRResp.ongoingPartitionReassignmentRemovingReplicas p)
        }
    unArr arr = case P.unKafkaArray arr of
      P.Null -> []
      P.NotNull v -> V.toList v


----------------------------------------------------------------------
-- Broker lifecycle
----------------------------------------------------------------------

{- | Unregister a broker. Mirrors @Admin.unregisterBroker(int)@.
Returns @Right ()@ on broker-side success; the @Left@ payload
carries the error code + message.
-}
unregisterBroker
  :: MonadIO m
  => AdminClient
  -> Int32
  -- ^ broker id
  -> m (Either String ())
unregisterBroker client bid = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 64 0 0 0 $ \conn corrId apiVer -> do
        let req =
              UBReq.UnregisterBrokerRequest
                { UBReq.unregisterBrokerRequestBrokerId = bid
                }
            body = WC.runEncodeVer @UBReq.UnregisterBrokerRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 64 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("UnregisterBroker request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @UBResp.UnregisterBrokerResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if UBResp.unregisterBrokerResponseErrorCode resp == 0
                  then pure (Right ())
                  else
                    pure $
                      Left $
                        "UnregisterBroker: error "
                          <> show (UBResp.unregisterBrokerResponseErrorCode resp)
                          <> ": "
                          <> T.unpack (extractText (UBResp.unregisterBrokerResponseErrorMessage resp))


----------------------------------------------------------------------
-- Client-quota admin (KIP-546)
----------------------------------------------------------------------

{- | A described quota entry — an entity together with the
per-name quota values configured for it. Mirrors
@ClientQuotaEntry@ in the JVM SDK.
-}
data ClientQuotaEntry = ClientQuotaEntry
  { cqeEntity :: !Quota.ClientQuotaEntity
  , cqeValues :: !(Map Text Double)
  }
  deriving stock (Eq, Show)


-- | Describe quotas. Mirrors @Admin.describeClientQuotas@.
describeClientQuotas
  :: MonadIO m
  => AdminClient
  -> Quota.ClientQuotaFilter
  -> m (Either String [ClientQuotaEntry])
describeClientQuotas client (Quota.ClientQuotaFilter comps strict) = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 48 0 1 0 $ \conn corrId apiVer -> do
        let req =
              DCQReq.DescribeClientQuotasRequest
                { DCQReq.describeClientQuotasRequestComponents =
                    P.mkKafkaArray (V.fromList (map buildComp comps))
                , DCQReq.describeClientQuotasRequestStrict = strict
                }
            body = WC.runEncodeVer @DCQReq.DescribeClientQuotasRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 48 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("DescribeClientQuotas request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @DCQResp.DescribeClientQuotasResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if DCQResp.describeClientQuotasResponseErrorCode resp /= 0
                  then
                    pure $
                      Left $
                        "DescribeClientQuotas: "
                          <> T.unpack
                            (extractText (DCQResp.describeClientQuotasResponseErrorMessage resp))
                  else do
                    let entries = case P.unKafkaArray (DCQResp.describeClientQuotasResponseEntries resp) of
                          P.Null -> V.empty
                          P.NotNull v -> v
                    pure $ Right (V.toList (V.map decodeEntry entries))
  where
    buildComp c =
      let (mt, mv) = case Quota.cqfcMatchType c of
            Quota.MatchExact nm -> (0 :: Int8, nm)
            Quota.MatchDefault -> (1, T.empty)
            Quota.MatchAny -> (2, T.empty)
      in DCQReq.ComponentData
           { DCQReq.componentDataEntityType = P.mkKafkaString (Quota.cqfcEntityType c)
           , DCQReq.componentDataMatchType = mt
           , DCQReq.componentDataMatch = P.mkKafkaString mv
           }
    decodeEntry e =
      let !ents = case P.unKafkaArray (DCQResp.entryDataEntity e) of
            P.Null -> V.empty
            P.NotNull v -> v
          !vals = case P.unKafkaArray (DCQResp.entryDataValues e) of
            P.Null -> V.empty
            P.NotNull v -> v
          !entMap =
            Map.fromList
              [ ( extractText (DCQResp.entityDataEntityType ed)
                , let n = extractText (DCQResp.entityDataEntityName ed)
                  in if T.null n then Nothing else Just n
                )
              | ed <- V.toList ents
              ]
          !valMap =
            Map.fromList
              [ ( extractText (DCQResp.valueDataKey vd)
                , DCQResp.valueDataValue vd
                )
              | vd <- V.toList vals
              ]
      in ClientQuotaEntry
           { cqeEntity = Quota.ClientQuotaEntity entMap
           , cqeValues = valMap
           }


-- | Alter quotas. Mirrors @Admin.alterClientQuotas@.
alterClientQuotas
  :: MonadIO m
  => AdminClient
  -> [Quota.ClientQuotaAlteration]
  -> Bool
  -- ^ validateOnly
  -> m (Either String [(Quota.ClientQuotaEntity, Either String ())])
alterClientQuotas client alterations validate = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 49 0 1 0 $ \conn corrId apiVer -> do
        let req =
              ACQReq.AlterClientQuotasRequest
                { ACQReq.alterClientQuotasRequestEntries =
                    P.mkKafkaArray (V.fromList (map buildEntry alterations))
                , ACQReq.alterClientQuotasRequestValidateOnly = validate
                }
            body = WC.runEncodeVer @ACQReq.AlterClientQuotasRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 49 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("AlterClientQuotas request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @ACQResp.AlterClientQuotasResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp -> do
                let rs = case P.unKafkaArray (ACQResp.alterClientQuotasResponseEntries resp) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                pure $ Right (V.toList (V.map decodeEntry rs))
  where
    buildEntry (Quota.ClientQuotaAlteration (Quota.ClientQuotaEntity entMap) ops) =
      ACQReq.EntryData
        { ACQReq.entryDataEntity =
            P.mkKafkaArray $
              V.fromList
                [ ACQReq.EntityData
                    { ACQReq.entityDataEntityType = P.mkKafkaString k
                    , ACQReq.entityDataEntityName = P.mkKafkaString (maybe T.empty id v)
                    }
                | (k, v) <- Map.toList entMap
                ]
        , ACQReq.entryDataOps =
            P.mkKafkaArray $
              V.fromList
                [ ACQReq.OpData
                    { ACQReq.opDataKey = P.mkKafkaString (Quota.cqoKey op)
                    , ACQReq.opDataValue = maybe 0 id (Quota.cqoValue op)
                    , ACQReq.opDataRemove = case Quota.cqoValue op of
                        Nothing -> True
                        Just _ -> False
                    }
                | op <- ops
                ]
        }
    decodeEntry e =
      let !ents = case P.unKafkaArray (ACQResp.entryDataEntity e) of
            P.Null -> V.empty
            P.NotNull v -> v
          !entMap =
            Map.fromList
              [ ( extractText (ACQResp.entityDataEntityType ed)
                , let n = extractText (ACQResp.entityDataEntityName ed)
                  in if T.null n then Nothing else Just n
                )
              | ed <- V.toList ents
              ]
          !ent = Quota.ClientQuotaEntity entMap
          !code = ACQResp.entryDataErrorCode e
          !msg = extractText (ACQResp.entryDataErrorMessage e)
      in if code == 0
           then (ent, Right ())
           else (ent, Left ("Error " <> show code <> ": " <> T.unpack msg))


----------------------------------------------------------------------
-- Transaction admin (KIP-664)
----------------------------------------------------------------------

-- | A row from 'listTransactions'.
data TransactionListing = TransactionListing
  { tlTransactionalId :: !Text
  , tlProducerId :: !Int64
  , tlState :: !Text
  {- ^ One of: @\"Empty\"@, @\"Ongoing\"@, @\"PrepareCommit\"@,
  @\"PrepareAbort\"@, @\"CompleteCommit\"@,
  @\"CompleteAbort\"@, @\"Dead\"@, @\"PrepareEpochFence\"@,
  @\"Unknown\"@.
  -}
  }
  deriving stock (Eq, Show)


-- | The detailed state of a single transaction.
data TransactionDescription = TransactionDescription
  { tdTransactionalId :: !Text
  , tdProducerId :: !Int64
  , tdProducerEpoch :: !Int16
  , tdTimeoutMs :: !Int32
  , tdStartTimeMs :: !Int64
  , tdState :: !Text
  , tdTopicPartitions :: ![TransactionTopicPartitions]
  }
  deriving stock (Eq, Show)


-- | The partitions of a single topic enrolled in a transaction.
data TransactionTopicPartitions = TransactionTopicPartitions
  { ttpTopic :: !Text
  , ttpPartitions :: ![Int32]
  }
  deriving stock (Eq, Show)


{- | List active transactions on the cluster, optionally filtered
by transaction state and producer id. Mirrors
@Admin.listTransactions()@.
-}
listTransactions
  :: MonadIO m
  => AdminClient
  -> [Text]
  -- ^ state filters
  -> [Int64]
  -- ^ producer-id filters
  -> Maybe Int64
  -- ^ min duration in ms
  -> m (Either String [TransactionListing])
listTransactions client stateFilters pidFilters durMs = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 66 0 1 0 $ \conn corrId apiVer -> do
        let req =
              LTReq.ListTransactionsRequest
                { LTReq.listTransactionsRequestStateFilters =
                    P.mkKafkaArray (V.fromList (map P.mkKafkaString stateFilters))
                , LTReq.listTransactionsRequestProducerIdFilters =
                    P.mkKafkaArray (V.fromList pidFilters)
                , LTReq.listTransactionsRequestDurationFilter =
                    maybe (-1) id durMs
                }
            body = WC.runEncodeVer @LTReq.ListTransactionsRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 66 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("ListTransactions request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @LTResp.ListTransactionsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if LTResp.listTransactionsResponseErrorCode resp /= 0
                  then
                    pure $
                      Left $
                        "ListTransactions: error code "
                          <> show (LTResp.listTransactionsResponseErrorCode resp)
                  else do
                    let ts = case P.unKafkaArray (LTResp.listTransactionsResponseTransactionStates resp) of
                          P.Null -> V.empty
                          P.NotNull v -> v
                    pure $ Right (V.toList (V.map decode_ ts))
  where
    decode_ t =
      TransactionListing
        { tlTransactionalId = extractText (LTResp.transactionStateTransactionalId t)
        , tlProducerId = LTResp.transactionStateProducerId t
        , tlState = extractText (LTResp.transactionStateTransactionState t)
        }


{- | Describe the supplied transactions. Mirrors
@Admin.describeTransactions(Collection<String>)@.
-}
describeTransactions
  :: MonadIO m
  => AdminClient
  -> [Text]
  -- ^ transactional ids
  -> m (Either String [TransactionDescription])
describeTransactions client tids = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 65 0 0 0 $ \conn corrId apiVer -> do
        let req =
              DTxReq.DescribeTransactionsRequest
                { DTxReq.describeTransactionsRequestTransactionalIds =
                    P.mkKafkaArray (V.fromList (map P.mkKafkaString tids))
                }
            body = WC.runEncodeVer @DTxReq.DescribeTransactionsRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 65 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("DescribeTransactions request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @DTxResp.DescribeTransactionsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp -> do
                let ts = case P.unKafkaArray (DTxResp.describeTransactionsResponseTransactionStates resp) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                pure $ Right (V.toList (V.map decode_ ts))
  where
    decode_ t =
      let !topics = case P.unKafkaArray (DTxResp.transactionStateTopics t) of
            P.Null -> V.empty
            P.NotNull v -> v
      in TransactionDescription
           { tdTransactionalId = extractText (DTxResp.transactionStateTransactionalId t)
           , tdProducerId = DTxResp.transactionStateProducerId t
           , tdProducerEpoch = DTxResp.transactionStateProducerEpoch t
           , tdTimeoutMs = DTxResp.transactionStateTransactionTimeoutMs t
           , tdStartTimeMs = DTxResp.transactionStateTransactionStartTimeMs t
           , tdState = extractText (DTxResp.transactionStateTransactionState t)
           , tdTopicPartitions =
               V.toList (V.map decodeTopic topics)
           }
    decodeTopic tp =
      let !ps = case P.unKafkaArray (DTxResp.topicDataPartitions tp) of
            P.Null -> []
            P.NotNull v -> V.toList v
      in TransactionTopicPartitions
           { ttpTopic = extractText (DTxResp.topicDataTopic tp)
           , ttpPartitions = ps
           }


----------------------------------------------------------------------
-- SCRAM credential admin (KIP-554)
----------------------------------------------------------------------

{- | SCRAM mechanism identifier. Mirrors
@org.apache.kafka.clients.admin.ScramMechanism@. Codes
match the broker wire shape.
-}
data ScramMechanism
  = ScramSha256
  | ScramSha512
  | ScramUnknown
  deriving stock (Eq, Show)


scramMechanismCode :: ScramMechanism -> Int8
scramMechanismCode = \case
  ScramUnknown -> 0
  ScramSha256 -> 1
  ScramSha512 -> 2


scramMechanismFromCode :: Int8 -> ScramMechanism
scramMechanismFromCode = \case
  1 -> ScramSha256
  2 -> ScramSha512
  _ -> ScramUnknown


{- | Per-user credential metadata returned by
'describeUserScramCredentials'.
-}
data ScramCredentialInfo = ScramCredentialInfo
  { sciMechanism :: !ScramMechanism
  , sciIterations :: !Int32
  }
  deriving stock (Eq, Show)


{- | Add or update a SCRAM credential. The broker requires
caller-supplied salt + salted password (PBKDF2-applied).
-}
data ScramCredentialUpsertion = ScramCredentialUpsertion
  { scuUser :: !Text
  , scuMechanism :: !ScramMechanism
  , scuIterations :: !Int32
  , scuSalt :: !ByteString
  , scuSaltedPassword :: !ByteString
  }
  deriving stock (Eq, Show)


-- | Delete a SCRAM credential for a user under a specific mechanism.
data ScramCredentialDeletion = ScramCredentialDeletion
  { scdUser :: !Text
  , scdMechanism :: !ScramMechanism
  }
  deriving stock (Eq, Show)


{- | Describe SCRAM credentials for the supplied users. Passing
@[]@ asks for every user the requesting principal can see.
-}
describeUserScramCredentials
  :: MonadIO m
  => AdminClient
  -> [Text]
  -> m (Either String [(Text, Either String [ScramCredentialInfo])])
describeUserScramCredentials client users = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 50 0 0 0 $ \conn corrId apiVer -> do
        let req =
              DSCReq.DescribeUserScramCredentialsRequest
                { DSCReq.describeUserScramCredentialsRequestUsers =
                    P.mkKafkaArray $
                      V.fromList
                        [DSCReq.UserName {DSCReq.userNameName = P.mkKafkaString u} | u <- users]
                }
            body = WC.runEncodeVer @DSCReq.DescribeUserScramCredentialsRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 50 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("DescribeUserScramCredentials request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @DSCResp.DescribeUserScramCredentialsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if DSCResp.describeUserScramCredentialsResponseErrorCode resp /= 0
                  then
                    pure $
                      Left $
                        "DescribeUserScramCredentials: "
                          <> T.unpack
                            (extractText (DSCResp.describeUserScramCredentialsResponseErrorMessage resp))
                  else do
                    let rs = case P.unKafkaArray (DSCResp.describeUserScramCredentialsResponseResults resp) of
                          P.Null -> V.empty
                          P.NotNull v -> v
                    pure $ Right (V.toList (V.map decodeUser rs))
  where
    decodeUser r =
      let !nm = extractText (DSCResp.describeUserScramCredentialsResultUser r)
          !code = DSCResp.describeUserScramCredentialsResultErrorCode r
          !msg = extractText (DSCResp.describeUserScramCredentialsResultErrorMessage r)
      in if code == 0
           then
             let cs = case P.unKafkaArray (DSCResp.describeUserScramCredentialsResultCredentialInfos r) of
                   P.Null -> V.empty
                   P.NotNull v -> v
             in (nm, Right (V.toList (V.map decodeCI cs)))
           else (nm, Left ("Error " <> show code <> ": " <> T.unpack msg))
    decodeCI ci =
      ScramCredentialInfo
        { sciMechanism = scramMechanismFromCode (DSCResp.credentialInfoMechanism ci)
        , sciIterations = DSCResp.credentialInfoIterations ci
        }


{- | Add and/or remove SCRAM credentials. Mirrors
@Admin.alterUserScramCredentials(List<UserScramCredentialAlteration>)@.
-}
alterUserScramCredentials
  :: MonadIO m
  => AdminClient
  -> [ScramCredentialUpsertion]
  -> [ScramCredentialDeletion]
  -> m (Either String [(Text, Either String ())])
alterUserScramCredentials client upserts deletes = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 51 0 0 0 $ \conn corrId apiVer -> do
        let req =
              ASCReq.AlterUserScramCredentialsRequest
                { ASCReq.alterUserScramCredentialsRequestDeletions =
                    P.mkKafkaArray (V.fromList (map buildDel deletes))
                , ASCReq.alterUserScramCredentialsRequestUpsertions =
                    P.mkKafkaArray (V.fromList (map buildUps upserts))
                }
            body = WC.runEncodeVer @ASCReq.AlterUserScramCredentialsRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 51 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("AlterUserScramCredentials request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @ASCResp.AlterUserScramCredentialsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp -> do
                let rs = case P.unKafkaArray (ASCResp.alterUserScramCredentialsResponseResults resp) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                pure $ Right (V.toList (V.map decodeR rs))
  where
    buildDel d =
      ASCReq.ScramCredentialDeletion
        { ASCReq.scramCredentialDeletionName = P.mkKafkaString (scdUser d)
        , ASCReq.scramCredentialDeletionMechanism = scramMechanismCode (scdMechanism d)
        }
    buildUps u =
      ASCReq.ScramCredentialUpsertion
        { ASCReq.scramCredentialUpsertionName = P.mkKafkaString (scuUser u)
        , ASCReq.scramCredentialUpsertionMechanism = scramMechanismCode (scuMechanism u)
        , ASCReq.scramCredentialUpsertionIterations = scuIterations u
        , ASCReq.scramCredentialUpsertionSalt = P.mkKafkaBytes (scuSalt u)
        , ASCReq.scramCredentialUpsertionSaltedPassword = P.mkKafkaBytes (scuSaltedPassword u)
        }
    decodeR r =
      let !nm = extractText (ASCResp.alterUserScramCredentialsResultUser r)
          !code = ASCResp.alterUserScramCredentialsResultErrorCode r
          !msg = extractText (ASCResp.alterUserScramCredentialsResultErrorMessage r)
      in if code == 0
           then (nm, Right ())
           else (nm, Left ("Error " <> show code <> ": " <> T.unpack msg))


----------------------------------------------------------------------
-- Producer-state admin (KIP-664)
----------------------------------------------------------------------

{- | Active-producer snapshot for a partition. Mirrors
@ProducerState@ in the JVM SDK.
-}
data ProducerState = ProducerState
  { psProducerId :: !Int64
  , psProducerEpoch :: !Int32
  , psLastSequence :: !Int32
  , psLastTimestamp :: !Int64
  , psCoordinatorEpoch :: !Int32
  , psCurrentTxnStartOffset :: !Int64
  }
  deriving stock (Eq, Show)


-- | Describe the producer state for the supplied partitions.
describeProducers
  :: MonadIO m
  => AdminClient
  -> [(Text, [Int32])]
  -- ^ topic → partition list
  -> m (Either String [(Text, Int32, Either String [ProducerState])])
describeProducers client targets = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 61 0 0 0 $ \conn corrId apiVer -> do
        let !topicReqs =
              V.fromList
                [ DPReq.TopicRequest
                    { DPReq.topicRequestName = P.mkKafkaString t
                    , DPReq.topicRequestPartitionIndexes = P.mkKafkaArray (V.fromList ps)
                    }
                | (t, ps) <- targets
                ]
            req =
              DPReq.DescribeProducersRequest
                { DPReq.describeProducersRequestTopics = P.mkKafkaArray topicReqs
                }
            body = WC.runEncodeVer @DPReq.DescribeProducersRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 61 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("DescribeProducers request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @DPResp.DescribeProducersResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp -> do
                let topicRs = case P.unKafkaArray (DPResp.describeProducersResponseTopics resp) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                pure $ Right $ concatMap flattenT (V.toList topicRs)
  where
    flattenT t =
      let !nm = extractText (DPResp.topicResponseName t)
          ps = case P.unKafkaArray (DPResp.topicResponsePartitions t) of
            P.Null -> V.empty
            P.NotNull v -> v
      in V.toList (V.map (decodeP nm) ps)
    decodeP nm p =
      let !pi_ = DPResp.partitionResponsePartitionIndex p
          !code = DPResp.partitionResponseErrorCode p
          !msg = extractText (DPResp.partitionResponseErrorMessage p)
      in if code == 0
           then
             let ps_ = case P.unKafkaArray (DPResp.partitionResponseActiveProducers p) of
                   P.Null -> V.empty
                   P.NotNull v -> v
             in (nm, pi_, Right (V.toList (V.map decodeS ps_)))
           else (nm, pi_, Left ("Error " <> show code <> ": " <> T.unpack msg))
    decodeS s =
      ProducerState
        { psProducerId = DPResp.producerStateProducerId s
        , psProducerEpoch = DPResp.producerStateProducerEpoch s
        , psLastSequence = DPResp.producerStateLastSequence s
        , psLastTimestamp = DPResp.producerStateLastTimestamp s
        , psCoordinatorEpoch = DPResp.producerStateCoordinatorEpoch s
        , psCurrentTxnStartOffset = DPResp.producerStateCurrentTxnStartOffset s
        }


----------------------------------------------------------------------
-- Log directory admin
----------------------------------------------------------------------

-- | A single broker's report of a log directory.
data LogDirDescription = LogDirDescription
  { lddPath :: !Text
  , lddErrorCode :: !Int16
  , lddTotalBytes :: !Int64
  , lddUsableBytes :: !Int64
  , lddTopics :: ![TopicLogDirDescription]
  }
  deriving stock (Eq, Show)


data TopicLogDirDescription = TopicLogDirDescription
  { tlddName :: !Text
  , tlddPartitions :: ![PartitionLogDirDescription]
  }
  deriving stock (Eq, Show)


data PartitionLogDirDescription = PartitionLogDirDescription
  { pldPartition :: !Int32
  , pldPartitionSize :: !Int64
  , pldOffsetLag :: !Int64
  , pldIsFutureKey :: !Bool
  }
  deriving stock (Eq, Show)


{- | Describe the log directories on the supplied partitions.
Mirrors @Admin.describeLogDirs(Collection<Integer>)@ — the
JVM variant takes broker ids; this one piggy-backs on the
admin client's currently-connected broker and only reports
its log dirs.
-}
describeLogDirs
  :: MonadIO m
  => AdminClient
  -> [(Text, [Int32])]
  -- ^ topics × partitions to query
  -> m (Either String [LogDirDescription])
describeLogDirs client targets = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 35 0 4 0 $ \conn corrId apiVer -> do
        let !ts =
              V.fromList
                [ DLDReq.DescribableLogDirTopic
                    { DLDReq.describableLogDirTopicTopic = P.mkKafkaString t
                    , DLDReq.describableLogDirTopicPartitions = P.mkKafkaArray (V.fromList ps)
                    }
                | (t, ps) <- targets
                ]
            req =
              DLDReq.DescribeLogDirsRequest
                { DLDReq.describeLogDirsRequestTopics = P.mkKafkaArray ts
                }
            body = WC.runEncodeVer @DLDReq.DescribeLogDirsRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 35 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("DescribeLogDirs request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @DLDResp.DescribeLogDirsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp -> do
                let rs = case P.unKafkaArray (DLDResp.describeLogDirsResponseResults resp) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                pure $ Right (V.toList (V.map decodeR rs))
  where
    decodeR r =
      let !ts = case P.unKafkaArray (DLDResp.describeLogDirsResultTopics r) of
            P.Null -> V.empty
            P.NotNull v -> v
      in LogDirDescription
           { lddPath = extractText (DLDResp.describeLogDirsResultLogDir r)
           , lddErrorCode = DLDResp.describeLogDirsResultErrorCode r
           , lddTotalBytes = DLDResp.describeLogDirsResultTotalBytes r
           , lddUsableBytes = DLDResp.describeLogDirsResultUsableBytes r
           , lddTopics = V.toList (V.map decodeT ts)
           }
    decodeT t =
      let !ps = case P.unKafkaArray (DLDResp.describeLogDirsTopicPartitions t) of
            P.Null -> V.empty
            P.NotNull v -> v
      in TopicLogDirDescription
           { tlddName = extractText (DLDResp.describeLogDirsTopicName t)
           , tlddPartitions = V.toList (V.map decodeP ps)
           }
    decodeP p =
      PartitionLogDirDescription
        { pldPartition = DLDResp.describeLogDirsPartitionPartitionIndex p
        , pldPartitionSize = DLDResp.describeLogDirsPartitionPartitionSize p
        , pldOffsetLag = DLDResp.describeLogDirsPartitionOffsetLag p
        , pldIsFutureKey = DLDResp.describeLogDirsPartitionIsFutureKey p
        }


{- | Move replicas to specific log directories. Each entry says
"for these (topic, partition) pairs, put them on this path".
-}
data ReplicaLogDirAssignment = ReplicaLogDirAssignment
  { rldaPath :: !Text
  , rldaPartitions :: ![(Text, [Int32])]
  }
  deriving stock (Eq, Show)


{- | Reassign replicas to specific log directories. Mirrors
@Admin.alterReplicaLogDirs(Map<TopicPartitionReplica, String>)@
(we adopt the per-path shape because that's how the wire
carries the request — JVM users flip it client-side).
-}
alterReplicaLogDirs
  :: MonadIO m
  => AdminClient
  -> [ReplicaLogDirAssignment]
  -> m (Either String [(Text, Int32, Either String ())])
alterReplicaLogDirs client assignments = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 34 0 2 0 $ \conn corrId apiVer -> do
        let !dirs =
              V.fromList
                [ ALDReq.AlterReplicaLogDir
                    { ALDReq.alterReplicaLogDirPath = P.mkKafkaString (rldaPath a)
                    , ALDReq.alterReplicaLogDirTopics =
                        P.mkKafkaArray $
                          V.fromList
                            [ ALDReq.AlterReplicaLogDirTopic
                                { ALDReq.alterReplicaLogDirTopicName = P.mkKafkaString t
                                , ALDReq.alterReplicaLogDirTopicPartitions = P.mkKafkaArray (V.fromList ps)
                                }
                            | (t, ps) <- rldaPartitions a
                            ]
                    }
                | a <- assignments
                ]
            req =
              ALDReq.AlterReplicaLogDirsRequest
                { ALDReq.alterReplicaLogDirsRequestDirs = P.mkKafkaArray dirs
                }
            body = WC.runEncodeVer @ALDReq.AlterReplicaLogDirsRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 34 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("AlterReplicaLogDirs request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @ALDResp.AlterReplicaLogDirsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp -> do
                let rs = case P.unKafkaArray (ALDResp.alterReplicaLogDirsResponseResults resp) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                pure $ Right (concatMap flattenT (V.toList rs))
  where
    flattenT t =
      let !nm = extractText (ALDResp.alterReplicaLogDirTopicResultTopicName t)
          ps = case P.unKafkaArray (ALDResp.alterReplicaLogDirTopicResultPartitions t) of
            P.Null -> V.empty
            P.NotNull v -> v
      in V.toList (V.map (decodeP nm) ps)
    decodeP nm p =
      let !pi_ = ALDResp.alterReplicaLogDirPartitionResultPartitionIndex p
          !code = ALDResp.alterReplicaLogDirPartitionResultErrorCode p
      in if code == 0
           then (nm, pi_, Right ())
           else (nm, pi_, Left ("Error " <> show code))


----------------------------------------------------------------------
-- Delegation tokens (KIP-48)
----------------------------------------------------------------------

-- | A described delegation token.
data DelegationToken = DelegationToken
  { dtTokenId :: !Text
  , dtHmac :: !ByteString
  , dtOwner :: !(Text, Text) -- principal type, principal name
  , dtTokenRequester :: !(Text, Text)
  , dtIssueTimestamp :: !Int64
  , dtExpiryTimestamp :: !Int64
  , dtMaxTimestamp :: !Int64
  }
  deriving stock (Eq, Show)


{- | Create a delegation token. The optional renewers list
nominates additional principals allowed to renew/expire the
token; pass @[]@ to lock it down to the issuer.
-}
createDelegationToken
  :: MonadIO m
  => AdminClient
  -> Maybe (Text, Text)
  -- ^ override owner principal (Nothing = use the issuer)
  -> [(Text, Text)]
  -- ^ renewers
  -> Int64
  -- ^ max lifetime ms (negative = broker default)
  -> m (Either String DelegationToken)
createDelegationToken client mOwner renewers maxLifeMs = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 38 0 3 0 $ \conn corrId apiVer -> do
        let (ot, on) = case mOwner of
              Just (t, n) -> (t, n)
              Nothing -> ("", "")
            !rens =
              V.fromList
                [ CDTReq.CreatableRenewers
                    { CDTReq.creatableRenewersPrincipalType = P.mkKafkaString t
                    , CDTReq.creatableRenewersPrincipalName = P.mkKafkaString n
                    }
                | (t, n) <- renewers
                ]
            req =
              CDTReq.CreateDelegationTokenRequest
                { CDTReq.createDelegationTokenRequestOwnerPrincipalType = P.mkKafkaString ot
                , CDTReq.createDelegationTokenRequestOwnerPrincipalName = P.mkKafkaString on
                , CDTReq.createDelegationTokenRequestRenewers = P.mkKafkaArray rens
                , CDTReq.createDelegationTokenRequestMaxLifetimeMs = maxLifeMs
                }
            body = WC.runEncodeVer @CDTReq.CreateDelegationTokenRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 38 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("CreateDelegationToken request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @CDTResp.CreateDelegationTokenResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if CDTResp.createDelegationTokenResponseErrorCode resp /= 0
                  then
                    pure $
                      Left $
                        "CreateDelegationToken: error code "
                          <> show (CDTResp.createDelegationTokenResponseErrorCode resp)
                  else
                    pure $
                      Right
                        DelegationToken
                          { dtTokenId =
                              extractText (CDTResp.createDelegationTokenResponseTokenId resp)
                          , dtHmac =
                              fromKB (CDTResp.createDelegationTokenResponseHmac resp)
                          , dtOwner =
                              ( extractText (CDTResp.createDelegationTokenResponsePrincipalType resp)
                              , extractText (CDTResp.createDelegationTokenResponsePrincipalName resp)
                              )
                          , dtTokenRequester =
                              ( extractText (CDTResp.createDelegationTokenResponseTokenRequesterPrincipalType resp)
                              , extractText (CDTResp.createDelegationTokenResponseTokenRequesterPrincipalName resp)
                              )
                          , dtIssueTimestamp = CDTResp.createDelegationTokenResponseIssueTimestampMs resp
                          , dtExpiryTimestamp = CDTResp.createDelegationTokenResponseExpiryTimestampMs resp
                          , dtMaxTimestamp = CDTResp.createDelegationTokenResponseMaxTimestampMs resp
                          }


{- | Push the token's expiry deadline forward by @renewPeriodMs@.
Returns the new expiry timestamp on success.
-}
renewDelegationToken
  :: MonadIO m
  => AdminClient
  -> ByteString
  -- ^ HMAC of the token to renew
  -> Int64
  -- ^ renew period ms
  -> m (Either String Int64)
renewDelegationToken client hmac periodMs = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 39 0 2 0 $ \conn corrId apiVer -> do
        let req =
              RDTReq.RenewDelegationTokenRequest
                { RDTReq.renewDelegationTokenRequestHmac = P.mkKafkaBytes hmac
                , RDTReq.renewDelegationTokenRequestRenewPeriodMs = periodMs
                }
            body = WC.runEncodeVer @RDTReq.RenewDelegationTokenRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 39 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("RenewDelegationToken request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @RDTResp.RenewDelegationTokenResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if RDTResp.renewDelegationTokenResponseErrorCode resp == 0
                  then pure $ Right (RDTResp.renewDelegationTokenResponseExpiryTimestampMs resp)
                  else
                    pure $
                      Left $
                        "RenewDelegationToken: error code "
                          <> show (RDTResp.renewDelegationTokenResponseErrorCode resp)


{- | Set the token's expiry deadline to @now + expiryPeriodMs@.
Passing a negative period invalidates the token immediately.
Returns the new expiry timestamp.
-}
expireDelegationToken
  :: MonadIO m
  => AdminClient
  -> ByteString
  -> Int64
  -> m (Either String Int64)
expireDelegationToken client hmac periodMs = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 40 0 2 0 $ \conn corrId apiVer -> do
        let req =
              EDTReq.ExpireDelegationTokenRequest
                { EDTReq.expireDelegationTokenRequestHmac = P.mkKafkaBytes hmac
                , EDTReq.expireDelegationTokenRequestExpiryTimePeriodMs = periodMs
                }
            body = WC.runEncodeVer @EDTReq.ExpireDelegationTokenRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 40 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("ExpireDelegationToken request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @EDTResp.ExpireDelegationTokenResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if EDTResp.expireDelegationTokenResponseErrorCode resp == 0
                  then pure $ Right (EDTResp.expireDelegationTokenResponseExpiryTimestampMs resp)
                  else
                    pure $
                      Left $
                        "ExpireDelegationToken: error code "
                          <> show (EDTResp.expireDelegationTokenResponseErrorCode resp)


{- | Describe issued delegation tokens. Pass @[]@ to ask for
/every/ token the requesting principal can see.
-}
describeDelegationToken
  :: MonadIO m
  => AdminClient
  -> [(Text, Text)]
  -- ^ owner principals to filter on
  -> m (Either String [DelegationToken])
describeDelegationToken client owners = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 41 0 3 0 $ \conn corrId apiVer -> do
        let !os =
              V.fromList
                [ DDTReq.DescribeDelegationTokenOwner
                    { DDTReq.describeDelegationTokenOwnerPrincipalType = P.mkKafkaString t
                    , DDTReq.describeDelegationTokenOwnerPrincipalName = P.mkKafkaString n
                    }
                | (t, n) <- owners
                ]
            req =
              DDTReq.DescribeDelegationTokenRequest
                { DDTReq.describeDelegationTokenRequestOwners = P.mkKafkaArray os
                }
            body = WC.runEncodeVer @DDTReq.DescribeDelegationTokenRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 41 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("DescribeDelegationToken request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @DDTResp.DescribeDelegationTokenResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if DDTResp.describeDelegationTokenResponseErrorCode resp /= 0
                  then
                    pure $
                      Left $
                        "DescribeDelegationToken: error code "
                          <> show (DDTResp.describeDelegationTokenResponseErrorCode resp)
                  else do
                    let ts = case P.unKafkaArray (DDTResp.describeDelegationTokenResponseTokens resp) of
                          P.Null -> V.empty
                          P.NotNull v -> v
                    pure $ Right (V.toList (V.map decodeT ts))
  where
    decodeT t =
      DelegationToken
        { dtTokenId = extractText (DDTResp.describedDelegationTokenTokenId t)
        , dtHmac = fromKB (DDTResp.describedDelegationTokenHmac t)
        , dtOwner =
            ( extractText (DDTResp.describedDelegationTokenPrincipalType t)
            , extractText (DDTResp.describedDelegationTokenPrincipalName t)
            )
        , dtTokenRequester =
            ( extractText (DDTResp.describedDelegationTokenTokenRequesterPrincipalType t)
            , extractText (DDTResp.describedDelegationTokenTokenRequesterPrincipalName t)
            )
        , dtIssueTimestamp = DDTResp.describedDelegationTokenIssueTimestamp t
        , dtExpiryTimestamp = DDTResp.describedDelegationTokenExpiryTimestamp t
        , dtMaxTimestamp = DDTResp.describedDelegationTokenMaxTimestamp t
        }


----------------------------------------------------------------------
-- KRaft voter management (KIP-853)
----------------------------------------------------------------------

{- | A KRaft voter endpoint: a (listener-name, host, port)
triple. Mirrors @RaftVoterEndpoint@ in the JVM SDK.
-}
data RaftVoterEndpoint = RaftVoterEndpoint
  { rveListenerName :: !Text
  , rveHost :: !Text
  , rvePort :: !Word16
  }
  deriving stock (Eq, Show)


{- | Add a voter node to the KRaft metadata quorum. Mirrors
@Admin.addRaftVoter(int, Uuid, Set<RaftVoterEndpoint>)@.
-}
addRaftVoter
  :: MonadIO m
  => AdminClient
  -> Int32
  -- ^ voter id
  -> P.KafkaUuid
  -- ^ voter directory id
  -> [RaftVoterEndpoint]
  -- ^ endpoints
  -> m (Either String ())
addRaftVoter client vid vdid endpoints = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 80 0 0 0 $ \conn corrId apiVer -> do
        let !ls =
              V.fromList
                [ ARVReq.Listener
                    { ARVReq.listenerName = P.mkKafkaString (rveListenerName e)
                    , ARVReq.listenerHost = P.mkKafkaString (rveHost e)
                    , ARVReq.listenerPort = rvePort e
                    }
                | e <- endpoints
                ]
            req =
              ARVReq.AddRaftVoterRequest
                { ARVReq.addRaftVoterRequestClusterId =
                    P.mkKafkaString T.empty
                , ARVReq.addRaftVoterRequestTimeoutMs = 30000
                , ARVReq.addRaftVoterRequestVoterId = vid
                , ARVReq.addRaftVoterRequestVoterDirectoryId = vdid
                , ARVReq.addRaftVoterRequestListeners = P.mkKafkaArray ls
                }
            body = WC.runEncodeVer @ARVReq.AddRaftVoterRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 80 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("AddRaftVoter request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @ARVResp.AddRaftVoterResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if ARVResp.addRaftVoterResponseErrorCode resp == 0
                  then pure (Right ())
                  else
                    pure $
                      Left $
                        "AddRaftVoter: "
                          <> T.unpack (extractText (ARVResp.addRaftVoterResponseErrorMessage resp))


{- | Remove a voter node from the KRaft metadata quorum.
Mirrors @Admin.removeRaftVoter(int, Uuid)@.
-}
removeRaftVoter
  :: MonadIO m
  => AdminClient
  -> Int32
  -- ^ voter id
  -> P.KafkaUuid
  -- ^ voter directory id
  -> m (Either String ())
removeRaftVoter client vid vdid = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 81 0 0 0 $ \conn corrId apiVer -> do
        let req =
              RRVReq.RemoveRaftVoterRequest
                { RRVReq.removeRaftVoterRequestClusterId = P.mkKafkaString T.empty
                , RRVReq.removeRaftVoterRequestVoterId = vid
                , RRVReq.removeRaftVoterRequestVoterDirectoryId = vdid
                }
            body = WC.runEncodeVer @RRVReq.RemoveRaftVoterRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 81 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("RemoveRaftVoter request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @RRVResp.RemoveRaftVoterResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if RRVResp.removeRaftVoterResponseErrorCode resp == 0
                  then pure (Right ())
                  else
                    pure $
                      Left $
                        "RemoveRaftVoter: "
                          <> T.unpack (extractText (RRVResp.removeRaftVoterResponseErrorMessage resp))


----------------------------------------------------------------------
-- KRaft quorum description
----------------------------------------------------------------------

-- | A description of a KRaft replica's state.
data ReplicaState = ReplicaState
  { rsReplicaId :: !Int32
  , rsLogEndOffset :: !Int64
  , rsLastFetchTimestamp :: !Int64
  , rsLastCaughtUpTimestamp :: !Int64
  }
  deriving stock (Eq, Show)


-- | A description of a single quorum partition.
data PartitionQuorumInfo = PartitionQuorumInfo
  { pqiPartition :: !Int32
  , pqiLeaderId :: !Int32
  , pqiLeaderEpoch :: !Int32
  , pqiHighWatermark :: !Int64
  , pqiVoters :: ![ReplicaState]
  , pqiObservers :: ![ReplicaState]
  }
  deriving stock (Eq, Show)


-- | A snapshot of the KRaft metadata quorum.
data QuorumInfo = QuorumInfo
  { qiPartitions :: ![(Text, [PartitionQuorumInfo])]
  }
  deriving stock (Eq, Show)


{- | Describe the metadata quorum. Mirrors
@Admin.describeMetadataQuorum()@.
-}
describeMetadataQuorum
  :: MonadIO m
  => AdminClient
  -> m (Either String QuorumInfo)
describeMetadataQuorum client = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 55 0 2 0 $ \conn corrId apiVer -> do
        -- Empty topics array asks for every topic the broker
        -- knows about; the JVM sends an explicit selector by
        -- default but the empty-list shape is the cheapest.
        let req =
              DQReq.DescribeQuorumRequest
                { DQReq.describeQuorumRequestTopics = P.mkKafkaArray V.empty
                }
            body = WC.runEncodeVer @DQReq.DescribeQuorumRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 55 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("DescribeQuorum request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @DQResp.DescribeQuorumResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp -> do
                let ts = case P.unKafkaArray (DQResp.describeQuorumResponseTopics resp) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                pure $ Right (QuorumInfo (V.toList (V.map decodeT ts)))
  where
    decodeT t =
      let ps = case P.unKafkaArray (DQResp.topicDataPartitions t) of
            P.Null -> V.empty
            P.NotNull v -> v
      in (extractText (DQResp.topicDataTopicName t), V.toList (V.map decodeP ps))
    decodeP p =
      PartitionQuorumInfo
        { pqiPartition = DQResp.partitionDataPartitionIndex p
        , pqiLeaderId = DQResp.partitionDataLeaderId p
        , pqiLeaderEpoch = DQResp.partitionDataLeaderEpoch p
        , pqiHighWatermark = DQResp.partitionDataHighWatermark p
        , pqiVoters = decodeReps (DQResp.partitionDataCurrentVoters p)
        , pqiObservers = decodeReps (DQResp.partitionDataObservers p)
        }
    decodeReps arr = case P.unKafkaArray arr of
      P.Null -> []
      P.NotNull v -> V.toList (V.map decodeR v)
    decodeR r =
      ReplicaState
        { rsReplicaId = DQResp.replicaStateReplicaId r
        , rsLogEndOffset = DQResp.replicaStateLogEndOffset r
        , rsLastFetchTimestamp = DQResp.replicaStateLastFetchTimestamp r
        , rsLastCaughtUpTimestamp = DQResp.replicaStateLastCaughtUpTimestamp r
        }


----------------------------------------------------------------------
-- Consumer-group member removal (KIP-345)
----------------------------------------------------------------------

{- | A member to remove from a consumer group. The static
'mtrGroupInstanceId' is the KIP-345 stable identifier;
'mtrMemberId' is the dynamic id assigned by the broker on
join. At least one must be set.
-}
data MemberToRemove = MemberToRemove
  { mtrMemberId :: !(Maybe Text)
  , mtrGroupInstanceId :: !(Maybe Text)
  , mtrReason :: !(Maybe Text)
  }
  deriving stock (Eq, Show)


{- | Force members out of a consumer group. Mirrors
@Admin.removeMembersFromConsumerGroup(String, RemoveMembersFromConsumerGroupOptions)@.
Routes through the LeaveGroup RPC; returns a per-member
result.
-}
removeMembersFromConsumerGroup
  :: MonadIO m
  => AdminClient
  -> Text
  -- ^ group id
  -> [MemberToRemove]
  -> m (Either String [(Text, Either String ())])
removeMembersFromConsumerGroup client groupId members = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 13 3 5 3 $ \conn corrId apiVer -> do
        let !ms =
              V.fromList
                [ LGRReq.MemberIdentity
                    { LGRReq.memberIdentityMemberId =
                        P.mkKafkaString (maybe T.empty id (mtrMemberId m))
                    , LGRReq.memberIdentityGroupInstanceId =
                        P.mkKafkaString (maybe T.empty id (mtrGroupInstanceId m))
                    , LGRReq.memberIdentityReason =
                        P.mkKafkaString (maybe T.empty id (mtrReason m))
                    }
                | m <- members
                ]
            req =
              LGRReq.LeaveGroupRequest
                { LGRReq.leaveGroupRequestGroupId = P.mkKafkaString groupId
                , LGRReq.leaveGroupRequestMemberId = P.mkKafkaString T.empty
                , LGRReq.leaveGroupRequestMembers = P.mkKafkaArray ms
                }
            body = WC.runEncodeVer @LGRReq.LeaveGroupRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 13 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("LeaveGroup request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @LGRResp.LeaveGroupResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if LGRResp.leaveGroupResponseErrorCode resp /= 0
                  then
                    pure $
                      Left $
                        "LeaveGroup: error code "
                          <> show (LGRResp.leaveGroupResponseErrorCode resp)
                  else do
                    let ms_ = case P.unKafkaArray (LGRResp.leaveGroupResponseMembers resp) of
                          P.Null -> V.empty
                          P.NotNull v -> v
                    pure $ Right (V.toList (V.map decodeM ms_))
  where
    decodeM m =
      let !mid = extractText (LGRResp.memberResponseMemberId m)
          !code = LGRResp.memberResponseErrorCode m
      in if code == 0
           then (mid, Right ())
           else (mid, Left ("Error " <> show code))


----------------------------------------------------------------------
-- Feature flags (KIP-584)
----------------------------------------------------------------------

{- | Supported version range a broker reports for a feature.
Mirrors @SupportedVersionRange@ in the JVM SDK.
-}
data SupportedFeatureRange = SupportedFeatureRange
  { sfrMinVersion :: !Int16
  , sfrMaxVersion :: !Int16
  }
  deriving stock (Eq, Show, Generic)


{- | Cluster-wide finalized version level for a feature.
Mirrors @FinalizedVersionRange@.
-}
data FinalizedFeatureLevel = FinalizedFeatureLevel
  { ffvMinVersionLevel :: !Int16
  , ffvMaxVersionLevel :: !Int16
  }
  deriving stock (Eq, Show, Generic)


{- | Snapshot of the cluster's feature metadata. Mirrors
@FeatureMetadata@.
-}
data FeatureMetadata = FeatureMetadata
  { fmSupportedFeatures :: !(Map.Map Text SupportedFeatureRange)
  , fmFinalizedFeatures :: !(Map.Map Text FinalizedFeatureLevel)
  , fmFinalizedFeaturesEpoch :: !(Maybe Int64)
  }
  deriving stock (Eq, Show, Generic)


{- | Describe the cluster's supported + finalized feature flags.
Mirrors @Admin.describeFeatures()@.

The wire shape lives on the ApiVersionsResponse (v3+), which
the broker emits alongside the per-API version negotiation.
We send a fresh ApiVersions request so the caller doesn't pay
the cost of digging into the AdminClient's cached negotiation
state.
-}
describeFeatures
  :: MonadIO m
  => AdminClient
  -> m (Either String FeatureMetadata)
describeFeatures client = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 18 3 4 3 $ \conn corrId apiVer -> do
        let req =
              AVReq.ApiVersionsRequest
                { AVReq.apiVersionsRequestClientSoftwareName =
                    P.mkKafkaString "wireform-kafka"
                , AVReq.apiVersionsRequestClientSoftwareVersion =
                    P.mkKafkaString "0.1.0.0"
                }
            body = WC.runEncodeVer @AVReq.ApiVersionsRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 18 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("ApiVersions request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @AVResp.ApiVersionsResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse ApiVersionsResponse: " <> e))
              Right resp ->
                if AVResp.apiVersionsResponseErrorCode resp /= 0
                  then
                    pure $
                      Left $
                        "describeFeatures: ApiVersions error code "
                          <> show (AVResp.apiVersionsResponseErrorCode resp)
                  else do
                    let sup = case P.unKafkaArray (AVResp.apiVersionsResponseSupportedFeatures resp) of
                          P.Null -> V.empty
                          P.NotNull v -> v
                        fin = case P.unKafkaArray (AVResp.apiVersionsResponseFinalizedFeatures resp) of
                          P.Null -> V.empty
                          P.NotNull v -> v
                        !supMap =
                          Map.fromList
                            [ ( extractText (AVResp.supportedFeatureKeyName f)
                              , SupportedFeatureRange
                                  { sfrMinVersion = AVResp.supportedFeatureKeyMinVersion f
                                  , sfrMaxVersion = AVResp.supportedFeatureKeyMaxVersion f
                                  }
                              )
                            | f <- V.toList sup
                            ]
                        !finMap =
                          Map.fromList
                            [ ( extractText (AVResp.finalizedFeatureKeyName f)
                              , FinalizedFeatureLevel
                                  { ffvMinVersionLevel = AVResp.finalizedFeatureKeyMinVersionLevel f
                                  , ffvMaxVersionLevel = AVResp.finalizedFeatureKeyMaxVersionLevel f
                                  }
                              )
                            | f <- V.toList fin
                            ]
                        !epoch = AVResp.apiVersionsResponseFinalizedFeaturesEpoch resp
                    pure $
                      Right
                        FeatureMetadata
                          { fmSupportedFeatures = supMap
                          , fmFinalizedFeatures = finMap
                          , fmFinalizedFeaturesEpoch =
                              if epoch < 0 then Nothing else Just epoch
                          }


{- | Whether an 'updateFeatures' request is allowed to downgrade
the feature level. Mirrors @FeatureUpdate.UpgradeType@.
-}
data FeatureUpgradeType
  = -- | wire value 1
    UpgradeOnly
  | -- | wire value 2
    SafeDowngrade
  | -- | wire value 3
    UnsafeDowngrade
  deriving stock (Eq, Show, Generic)


upgradeTypeCode :: FeatureUpgradeType -> Int8
upgradeTypeCode = \case
  UpgradeOnly -> 1
  SafeDowngrade -> 2
  UnsafeDowngrade -> 3


{- | A single feature-version update request. Mirrors
@FeatureUpdate@.
-}
data FeatureUpdate = FeatureUpdate
  { fuFeature :: !Text
  , fuMaxVersionLevel :: !Int16
  {- ^ A value of @0@ asks the broker to delete the feature's
  finalized version level entirely.
  -}
  , fuUpgradeType :: !FeatureUpgradeType
  }
  deriving stock (Eq, Show, Generic)


{- | Apply the supplied 'FeatureUpdate's atomically. Mirrors
@Admin.updateFeatures(Map<String, FeatureUpdate>,
UpdateFeaturesOptions)@.

Returns a per-feature @(name, result)@ list.
-}
updateFeatures
  :: MonadIO m
  => AdminClient
  -> [FeatureUpdate]
  -> Bool
  -- ^ validateOnly
  -> m (Either String [(Text, Either String ())])
updateFeatures client updates validate = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 57 1 2 1 $ \conn corrId apiVer -> do
        let !ups = V.fromList (map buildUp updates)
            req =
              UFReq.UpdateFeaturesRequest
                { UFReq.updateFeaturesRequesttimeoutMs = 30000
                , UFReq.updateFeaturesRequestFeatureUpdates = P.mkKafkaArray ups
                , UFReq.updateFeaturesRequestValidateOnly = validate
                }
            body = WC.runEncodeVer @UFReq.UpdateFeaturesRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 57 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("UpdateFeatures request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @UFResp.UpdateFeaturesResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse UpdateFeaturesResponse: " <> e))
              Right resp ->
                if UFResp.updateFeaturesResponseErrorCode resp /= 0
                  then
                    pure $
                      Left $
                        "updateFeatures: top-level error: "
                          <> T.unpack
                            (extractText (UFResp.updateFeaturesResponseErrorMessage resp))
                  else do
                    let rs = case P.unKafkaArray (UFResp.updateFeaturesResponseResults resp) of
                          P.Null -> V.empty
                          P.NotNull v -> v
                    pure $ Right (V.toList (V.map decodeR rs))
  where
    buildUp u =
      UFReq.FeatureUpdateKey
        { UFReq.featureUpdateKeyFeature =
            P.mkKafkaString (fuFeature u)
        , UFReq.featureUpdateKeyMaxVersionLevel = fuMaxVersionLevel u
        , UFReq.featureUpdateKeyAllowDowngrade =
            case fuUpgradeType u of
              UpgradeOnly -> False
              SafeDowngrade -> True
              UnsafeDowngrade -> True
        , UFReq.featureUpdateKeyUpgradeType =
            upgradeTypeCode (fuUpgradeType u)
        }
    decodeR r =
      let !nm = extractText (UFResp.updatableFeatureResultFeature r)
          !code = UFResp.updatableFeatureResultErrorCode r
          !msg = extractText (UFResp.updatableFeatureResultErrorMessage r)
      in if code == 0
           then (nm, Right ())
           else (nm, Left ("Error " <> show code <> ": " <> T.unpack msg))


----------------------------------------------------------------------
-- Producer fencing (KIP-360)
----------------------------------------------------------------------

{- | A fenced producer. Mirrors the JVM result of
@Admin.fenceProducers(Collection<String>)@: per-id producer
id + epoch the broker now considers authoritative. The next
producer that opens the same @transactional.id@ inherits
these values, fencing off any zombie producer still holding
the old epoch.
-}
data FencedProducer = FencedProducer
  { fpTransactionalId :: !Text
  , fpProducerId :: !Int64
  , fpProducerEpoch :: !Int16
  }
  deriving stock (Eq, Show, Generic)


{- | Bump the producer epochs of the supplied transactional ids,
evicting any in-flight producer holding the old epoch.
Mirrors @Admin.fenceProducers(Collection<String>)@.

One InitProducerId RPC per id (the JVM client does the same;
a "fence" is just an InitProducerId with no producer id /
epoch supplied, which forces the broker to issue a fresh
epoch).
-}
fenceProducers
  :: MonadIO m
  => AdminClient
  -> [Text]
  -> m (Either String [(Text, Either String FencedProducer)])
fenceProducers client tids = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr -> do
      rs <- mapM (fenceOne addr) tids
      pure (Right rs)
  where
    fenceOne addr tid = do
      -- 'withNegotiatedVersion' itself returns @IO (Either String a)@;
      -- we want a (tid, Either String FencedProducer) tuple regardless
      -- of which layer fails, so we collapse the outer error path
      -- into the inner 'Either' here.
      r <- withNegotiatedVersion client addr 22 4 5 4 $ \conn corrId apiVer -> do
        let req =
              IPIReq.InitProducerIdRequest
                { IPIReq.initProducerIdRequestTransactionalId =
                    P.mkKafkaString tid
                , IPIReq.initProducerIdRequestTransactionTimeoutMs = 30000
                , IPIReq.initProducerIdRequestProducerId = -1
                , IPIReq.initProducerIdRequestProducerEpoch = -1
                }
            body = WC.runEncodeVer @IPIReq.InitProducerIdRequest apiVer req
            cid = clientIdOf client
        sr <- Req.sendRequestReceiveResponse conn 22 apiVer corrId cid body
        case sr of
          Left e -> pure (Right (Left ("InitProducerId request failed: " <> e)))
          Right (_, respBody) ->
            case WC.runDecodeVer @IPIResp.InitProducerIdResponse apiVer respBody of
              Left e -> pure (Right (Left ("Failed to parse response: " <> e)))
              Right resp ->
                if IPIResp.initProducerIdResponseErrorCode resp /= 0
                  then
                    pure
                      ( Right
                          ( Left $
                              "InitProducerId error code "
                                <> show (IPIResp.initProducerIdResponseErrorCode resp)
                          )
                      )
                  else
                    pure
                      ( Right
                          ( Right
                              FencedProducer
                                { fpTransactionalId = tid
                                , fpProducerId = IPIResp.initProducerIdResponseProducerId resp
                                , fpProducerEpoch = IPIResp.initProducerIdResponseProducerEpoch resp
                                }
                          )
                      )
      pure $ case r of
        Left e -> (tid, Left e)
        Right (Left e) -> (tid, Left e)
        Right (Right f) -> (tid, Right f)


----------------------------------------------------------------------
-- Manual transaction abort (KIP-664)
----------------------------------------------------------------------

{- | A transaction to abort. Mirrors @AbortTransactionSpec@:
the abort marker the broker is asked to write must carry the
producer id + epoch the on-disk txn data already references,
plus the coordinator epoch the txn coordinator stamped on
the txn. The 'atsTopicPartitions' set identifies which
(topic, partition) pairs carry the open txn.
-}
data AbortTransactionSpec = AbortTransactionSpec
  { atsProducerId :: !Int64
  , atsProducerEpoch :: !Int16
  , atsCoordinatorEpoch :: !Int32
  , atsTopicPartitions :: ![(Text, [Int32])]
  }
  deriving stock (Eq, Show, Generic)


{- | Manually abort a hanging transaction. Mirrors
@Admin.abortTransaction(AbortTransactionSpec)@. Routes through
the WriteTxnMarkers RPC with @TransactionResult = false@
(= abort).
-}
abortTransaction
  :: MonadIO m
  => AdminClient
  -> AbortTransactionSpec
  -> m (Either String ())
abortTransaction client spec = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 27 1 1 1 $ \conn corrId apiVer -> do
        let !topics =
              V.fromList
                [ WTMReq.WritableTxnMarkerTopic
                    { WTMReq.writableTxnMarkerTopicName = P.mkKafkaString t
                    , WTMReq.writableTxnMarkerTopicPartitionIndexes =
                        P.mkKafkaArray (V.fromList ps)
                    }
                | (t, ps) <- atsTopicPartitions spec
                ]
            marker =
              WTMReq.WritableTxnMarker
                { WTMReq.writableTxnMarkerProducerId = atsProducerId spec
                , WTMReq.writableTxnMarkerProducerEpoch = atsProducerEpoch spec
                , WTMReq.writableTxnMarkerTransactionResult = False
                , WTMReq.writableTxnMarkerTopics = P.mkKafkaArray topics
                , WTMReq.writableTxnMarkerCoordinatorEpoch = atsCoordinatorEpoch spec
                }
            req =
              WTMReq.WriteTxnMarkersRequest
                { WTMReq.writeTxnMarkersRequestMarkers =
                    P.mkKafkaArray (V.singleton marker)
                }
            body = WC.runEncodeVer @WTMReq.WriteTxnMarkersRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 27 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("WriteTxnMarkers request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @WTMResp.WriteTxnMarkersResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp -> do
                let ms = case P.unKafkaArray (WTMResp.writeTxnMarkersResponseMarkers resp) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                case V.toList ms of
                  [] -> pure (Right ())
                  (m : _) ->
                    let tops = case P.unKafkaArray (WTMResp.writableTxnMarkerResultTopics m) of
                          P.Null -> []
                          P.NotNull v -> V.toList v
                        firstErr =
                          listToMaybe
                            [ (extractText (WTMResp.writableTxnMarkerTopicResultName t), pi_, code)
                            | t <- tops
                            , p <- case P.unKafkaArray (WTMResp.writableTxnMarkerTopicResultPartitions t) of
                                P.Null -> []
                                P.NotNull v -> V.toList v
                            , let pi_ = WTMResp.writableTxnMarkerPartitionResultPartitionIndex p
                                  code = WTMResp.writableTxnMarkerPartitionResultErrorCode p
                            , code /= 0
                            ]
                    in case firstErr of
                         Nothing -> pure (Right ())
                         Just (t, pi_, code) ->
                           pure $
                             Left $
                               "abortTransaction: "
                                 <> T.unpack t
                                 <> "-"
                                 <> show pi_
                                 <> ": error code "
                                 <> show code
  where
    listToMaybe [] = Nothing
    listToMaybe (x : _) = Just x


----------------------------------------------------------------------
-- KIP-848 consumer-group describe
----------------------------------------------------------------------

-- | A single member of a KIP-848 consumer group.
data ConsumerGroupMember = ConsumerGroupMember
  { cgmMemberId :: !Text
  , cgmInstanceId :: !(Maybe Text)
  , cgmRackId :: !(Maybe Text)
  , cgmMemberEpoch :: !Int32
  , cgmClientId :: !Text
  , cgmClientHost :: !Text
  , cgmSubscribedTopicNames :: ![Text]
  , cgmSubscribedTopicRegex :: !(Maybe Text)
  , cgmAssignedPartitions :: ![(Text, [Int32])]
  , cgmTargetAssignment :: ![(Text, [Int32])]
  }
  deriving stock (Eq, Show, Generic)


{- | A KIP-848 (next-gen) consumer group description. Mirrors
the JVM @ConsumerGroupDescription@ for the new protocol.
-}
data ConsumerGroupDescription2 = ConsumerGroupDescription2
  { cgd2GroupId :: !Text
  , cgd2GroupState :: !Text
  , cgd2GroupEpoch :: !Int32
  , cgd2AssignmentEpoch :: !Int32
  , cgd2AssignorName :: !Text
  , cgd2Members :: ![ConsumerGroupMember]
  , cgd2AuthorizedOperations :: !Int32
  }
  deriving stock (Eq, Show, Generic)


{- | Describe the supplied consumer groups using the KIP-848
ConsumerGroupDescribe RPC (API key 69). Mirrors
@Admin.describeConsumerGroups@ on a 4.0 broker, which uses
this RPC by default.
-}
describeConsumerGroups2
  :: MonadIO m
  => AdminClient
  -> [Text]
  -- ^ group ids
  -> Bool
  -- ^ include authorised operations
  -> m (Either String [ConsumerGroupDescription2])
describeConsumerGroups2 client gids inclAuth = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 69 0 1 0 $ \conn corrId apiVer -> do
        let req =
              CGDReq.ConsumerGroupDescribeRequest
                { CGDReq.consumerGroupDescribeRequestGroupIds =
                    P.mkKafkaArray (V.fromList (map P.mkKafkaString gids))
                , CGDReq.consumerGroupDescribeRequestIncludeAuthorizedOperations =
                    inclAuth
                }
            body = WC.runEncodeVer @CGDReq.ConsumerGroupDescribeRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 69 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("ConsumerGroupDescribe request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @CGDResp.ConsumerGroupDescribeResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp -> do
                let gs = case P.unKafkaArray (CGDResp.consumerGroupDescribeResponseGroups resp) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                pure $ Right (V.toList (V.map decodeG gs))
  where
    decodeG g =
      let !ms = case P.unKafkaArray (CGDResp.describedGroupMembers g) of
            P.Null -> V.empty
            P.NotNull v -> v
      in ConsumerGroupDescription2
           { cgd2GroupId = extractText (CGDResp.describedGroupGroupId g)
           , cgd2GroupState = extractText (CGDResp.describedGroupGroupState g)
           , cgd2GroupEpoch = CGDResp.describedGroupGroupEpoch g
           , cgd2AssignmentEpoch = CGDResp.describedGroupAssignmentEpoch g
           , cgd2AssignorName = extractText (CGDResp.describedGroupAssignorName g)
           , cgd2AuthorizedOperations = CGDResp.describedGroupAuthorizedOperations g
           , cgd2Members = V.toList (V.map decodeM ms)
           }
    decodeM m =
      ConsumerGroupMember
        { cgmMemberId = extractText (CGDResp.memberMemberId m)
        , cgmInstanceId = maybeText (extractText (CGDResp.memberInstanceId m))
        , cgmRackId = maybeText (extractText (CGDResp.memberRackId m))
        , cgmMemberEpoch = CGDResp.memberMemberEpoch m
        , cgmClientId = extractText (CGDResp.memberClientId m)
        , cgmClientHost = extractText (CGDResp.memberClientHost m)
        , cgmSubscribedTopicNames =
            let arr = case P.unKafkaArray (CGDResp.memberSubscribedTopicNames m) of
                  P.Null -> V.empty
                  P.NotNull v -> v
            in V.toList (V.map extractText arr)
        , cgmSubscribedTopicRegex =
            maybeText (extractText (CGDResp.memberSubscribedTopicRegex m))
        , cgmAssignedPartitions =
            decodeAssignment (CGDResp.memberAssignment m)
        , cgmTargetAssignment =
            decodeAssignment (CGDResp.memberTargetAssignment m)
        }
    decodeAssignment a =
      let tps = case P.unKafkaArray (CGDResp.assignmentTopicPartitions a) of
            P.Null -> V.empty
            P.NotNull v -> v
      in V.toList (V.map decodeTP tps)
    decodeTP tp =
      let pa = case P.unKafkaArray (CGDResp.topicPartitionsPartitions tp) of
            P.Null -> []
            P.NotNull v -> V.toList v
      in (extractText (CGDResp.topicPartitionsTopicName tp), pa)
    maybeText t = if T.null t then Nothing else Just t


{- | Describe consumer groups using the /classic/ DescribeGroups
RPC (API key 15). Mirrors @Admin.describeClassicGroups@ on
the JVM 4.0 SDK — same wire shape as 3.x @describeConsumerGroups@,
kept addressable so callers can target classic groups
explicitly when running on a mixed-mode cluster.
-}
describeClassicGroups
  :: MonadIO m
  => AdminClient
  -> [Text]
  -> m (Either String [ConsumerGroupDescription])
describeClassicGroups = describeConsumerGroups


----------------------------------------------------------------------
-- Share group describe (KIP-932)
----------------------------------------------------------------------

-- | A single member of a share group.
data ShareGroupMember = ShareGroupMember
  { sgmMemberId :: !Text
  , sgmRackId :: !(Maybe Text)
  , sgmMemberEpoch :: !Int32
  , sgmClientId :: !Text
  , sgmClientHost :: !Text
  , sgmSubscribedTopicNames :: ![Text]
  , sgmAssignedPartitions :: ![(Text, [Int32])]
  }
  deriving stock (Eq, Show, Generic)


-- | A share group description (KIP-932).
data ShareGroupDescription = ShareGroupDescription
  { sgdGroupId :: !Text
  , sgdGroupState :: !Text
  , sgdGroupEpoch :: !Int32
  , sgdAssignmentEpoch :: !Int32
  , sgdAssignorName :: !Text
  , sgdMembers :: ![ShareGroupMember]
  , sgdAuthorizedOperations :: !Int32
  }
  deriving stock (Eq, Show, Generic)


{- | Describe the supplied share groups. Mirrors
@Admin.describeShareGroups(Collection<String>)@.
-}
describeShareGroups
  :: MonadIO m
  => AdminClient
  -> [Text]
  -> Bool
  -> m (Either String [ShareGroupDescription])
describeShareGroups client gids inclAuth = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 77 0 0 0 $ \conn corrId apiVer -> do
        let req =
              SGDReq.ShareGroupDescribeRequest
                { SGDReq.shareGroupDescribeRequestGroupIds =
                    P.mkKafkaArray (V.fromList (map P.mkKafkaString gids))
                , SGDReq.shareGroupDescribeRequestIncludeAuthorizedOperations =
                    inclAuth
                }
            body = WC.runEncodeVer @SGDReq.ShareGroupDescribeRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 77 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("ShareGroupDescribe request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @SGDResp.ShareGroupDescribeResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp -> do
                let gs = case P.unKafkaArray (SGDResp.shareGroupDescribeResponseGroups resp) of
                      P.Null -> V.empty
                      P.NotNull v -> v
                pure $ Right (V.toList (V.map decodeG gs))
  where
    decodeG g =
      let !ms = case P.unKafkaArray (SGDResp.describedGroupMembers g) of
            P.Null -> V.empty
            P.NotNull v -> v
      in ShareGroupDescription
           { sgdGroupId = extractText (SGDResp.describedGroupGroupId g)
           , sgdGroupState = extractText (SGDResp.describedGroupGroupState g)
           , sgdGroupEpoch = SGDResp.describedGroupGroupEpoch g
           , sgdAssignmentEpoch = SGDResp.describedGroupAssignmentEpoch g
           , sgdAssignorName = extractText (SGDResp.describedGroupAssignorName g)
           , sgdAuthorizedOperations = SGDResp.describedGroupAuthorizedOperations g
           , sgdMembers = V.toList (V.map decodeM ms)
           }
    decodeM m =
      ShareGroupMember
        { sgmMemberId = extractText (SGDResp.memberMemberId m)
        , sgmRackId = maybeText (extractText (SGDResp.memberRackId m))
        , sgmMemberEpoch = SGDResp.memberMemberEpoch m
        , sgmClientId = extractText (SGDResp.memberClientId m)
        , sgmClientHost = extractText (SGDResp.memberClientHost m)
        , sgmSubscribedTopicNames =
            let arr = case P.unKafkaArray (SGDResp.memberSubscribedTopicNames m) of
                  P.Null -> V.empty
                  P.NotNull v -> v
            in V.toList (V.map extractText arr)
        , sgmAssignedPartitions =
            decodeAssignment (SGDResp.memberAssignment m)
        }
    decodeAssignment a =
      let tps = case P.unKafkaArray (SGDResp.assignmentTopicPartitions a) of
            P.Null -> V.empty
            P.NotNull v -> v
      in V.toList (V.map decodeTP tps)
    decodeTP tp =
      let pa = case P.unKafkaArray (SGDResp.topicPartitionsPartitions tp) of
            P.Null -> []
            P.NotNull v -> V.toList v
      in (extractText (SGDResp.topicPartitionsTopicName tp), pa)
    maybeText t = if T.null t then Nothing else Just t


----------------------------------------------------------------------
-- Client metrics resources (KIP-714)
----------------------------------------------------------------------

{- | List the client-metrics resource names configured on the
cluster. Mirrors
@Admin.listClientMetricsResources()@. Each returned name is a
configurable resource the broker uses to gate metric
collection (see KIP-714).
-}
listClientMetricsResources
  :: MonadIO m
  => AdminClient
  -> m (Either String [Text])
listClientMetricsResources client = liftIO $ do
  brokerR <- pickBroker client
  case brokerR of
    Left e -> pure (Left e)
    Right addr ->
      withNegotiatedVersion client addr 74 0 0 0 $ \conn corrId apiVer -> do
        let req = LCMRReq.ListClientMetricsResourcesRequest
            body = WC.runEncodeVer @LCMRReq.ListClientMetricsResourcesRequest apiVer req
            cid = clientIdOf client
        r <- Req.sendRequestReceiveResponse conn 74 apiVer corrId cid body
        case r of
          Left e -> pure (Left ("ListClientMetricsResources request failed: " <> e))
          Right (_, respBody) ->
            case WC.runDecodeVer @LCMRResp.ListClientMetricsResourcesResponse apiVer respBody of
              Left e -> pure (Left ("Failed to parse response: " <> e))
              Right resp ->
                if LCMRResp.listClientMetricsResourcesResponseErrorCode resp /= 0
                  then
                    pure $
                      Left $
                        "listClientMetricsResources: error code "
                          <> show (LCMRResp.listClientMetricsResourcesResponseErrorCode resp)
                  else do
                    let rs = case P.unKafkaArray (LCMRResp.listClientMetricsResourcesResponseClientMetricsResources resp) of
                          P.Null -> V.empty
                          P.NotNull v -> v
                    pure $
                      Right
                        [extractText (LCMRResp.clientMetricsResourceName r) | r <- V.toList rs]


----------------------------------------------------------------------
-- KIP-714 client telemetry id
----------------------------------------------------------------------

{- | Returns the admin client's client-instance id. Mirrors
@AdminClient.clientInstanceId(Duration)@. Deterministic
locally-derived UUID (same encoding the consumer + producer
use); broker-side telemetry assignment lands here when the
@GetTelemetrySubscriptions@ pipeline does.
-}
adminClientInstanceId :: MonadIO m => AdminClient -> m TopicIdImp.TopicId
adminClientInstanceId client =
  liftIO $
    pure
      ( TopicIdImp.TopicId
          (Telemetry.clientInstanceIdFromText (adminClientId (adminConfigOf client)))
      )
