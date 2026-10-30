{-# LANGUAGE OverloadedStrings #-}

{- | Smoke tests for the JVM-parity shims added in the SDK_PARITY
audit pass: the JVM-equivalence surface in 'Kafka.Client.Consumer'
(ConsumerRecords, OffsetAndMetadata, ConsumerGroupMetadata,
OffsetCommitCallback, SubscriptionPattern) and
'Kafka.Streams.Processor.Mock' (the mock processor context for
unit-testing user processors).
-}
module Streams.SdkParitySpec (tests) where

import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HashMap
-- The JVM-equivalence shims (ConsumerRecords, OffsetAndMetadata,
-- ConsumerGroupMetadata, OffsetCommitCallback, SubscriptionPattern,
-- the consumer-overload tail) live in 'Kafka.Client.Consumer'
-- itself now; the 'SDK' alias is kept for the tests so the
-- "JVM name" call-sites read clearly.

import Data.IORef
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Kafka.Client.AdminClient qualified as Adm
import Kafka.Client.Consumer qualified as C
import Kafka.Client.Consumer qualified as SDK
import Kafka.Client.Telemetry qualified as Tel
import Kafka.Common qualified as Common
import Kafka.Common.Acl qualified as Acl
import Kafka.Common.Quota qualified as Quota
import Kafka.Common.Resource qualified as Resource
import Kafka.Serde qualified as Serde
import Kafka.Streams.Config qualified
import Kafka.Streams.Errors qualified as Errors
import Kafka.Streams.Imperative qualified
import Kafka.Streams.Processor qualified as P
import Kafka.Streams.Processor.Assignment qualified as TA
import Kafka.Streams.Processor.Mock qualified as M
import Kafka.Streams.Runtime qualified
import Kafka.Streams.Time (
  StreamTime (..),
  Timestamp (..),
  initialStreamTime,
  runTimestampExtractor,
  usePartitionTimeOnInvalidTimestamp,
 )
import Kafka.Streams.Time qualified
import Kafka.Streams.Types (Record (..), mkRecord)
import Kafka.Streams.Window qualified as W
import Test.Syd


tests :: Spec
tests =
  describe "SDK parity shims (audit pass)" $
    sequence_
      [ describe "Kafka.Client.Consumer JVM-equivalence shims" $
          sequence_
            [ consumerRecords_groupings
            , offsetAndMetadata_builder
            , subscriptionPattern_match
            , offsetCommitCallback_compiles
            ]
      , describe "Kafka.Streams.Processor.Mock" $
          sequence_
            [ mock_captures_forwards
            , mock_captures_punctuators
            , mock_commit_request_flag
            ]
      , describe "Kafka.Common (v2 audit additions)" $
          sequence_
            [ common_node_endpoint_smoke
            , common_acl_wildcard
            , common_quota_helpers
            , timeextractor_use_partition_time
            ]
      , describe "Kafka.Client.AdminClient long-tail RPCs (v3 audit additions)" $
          sequence_
            [ admin_extras_value_smoke
            ]
      , describe "v3+: leftover gaps from v2 honest-list" $
          sequence_
            [ unlimited_windows_smoke
            , list_serde_roundtrip
            , task_assignor_default
            , per_error_streams_exceptions
            ]
      , describe "v4: protocol-codegen-dependent admin RPCs" $
          sequence_
            [ feature_metadata_value_smoke
            , feature_update_value_smoke
            , abort_transaction_spec_value_smoke
            , consumer_group_describe2_value_smoke
            , share_group_describe_value_smoke
            , fenced_producer_value_smoke
            ]
      , describe "v4: KIP-924 + KIP-714 runtime wiring" $
          sequence_
            [ task_assignor_plugin_configurable
            , task_assignor_plugin_invocable
            , telemetry_id_is_16_bytes
            , telemetry_id_is_deterministic
            , telemetry_id_distinguishes_inputs
            , telemetry_id_truncates_long_inputs
            , telemetry_id_pads_short_inputs
            , streams_runtime_telemetry_id
            ]
      ]


----------------------------------------------------------------------
-- Kafka.Client.Consumer JVM-equivalence shims
----------------------------------------------------------------------

mkRec :: T.Text -> Int -> Int -> C.ConsumerRecord
mkRec t p o =
  C.ConsumerRecord
    { C.topic = t
    , C.partition = fromIntegral p
    , C.offset = fromIntegral o
    , C.timestamp = 0
    , C.key = Nothing
    , C.value = Just ""
    , C.headers = []
    }


consumerRecords_groupings :: Spec
consumerRecords_groupings =
  it "ConsumerRecords: partition/topic/next-offset projections" $ do
    let rs =
          SDK.ConsumerRecords
            [ mkRec "events" 0 10
            , mkRec "events" 0 11
            , mkRec "events" 1 20
            , mkRec "audit" 0 30
            ]
    SDK.consumerRecordsCount rs `shouldBe` 4
    SDK.consumerRecordsPartitions rs
      `shouldBe` Set.fromList
        [ C.TopicPartition "events" 0
        , C.TopicPartition "events" 1
        , C.TopicPartition "audit" 0
        ]
    map C.offset (SDK.recordsByPartition (C.TopicPartition "events" 0) rs)
      `shouldBe` [10, 11]
    Map.keys (SDK.recordsByTopic rs)
      `shouldBe` ["audit", "events"]
    SDK.consumerRecordsNextOffsets rs
      `shouldBe` HashMap.fromList
        [ (C.TopicPartition "events" 0, 12)
        , (C.TopicPartition "events" 1, 21)
        , (C.TopicPartition "audit" 0, 31)
        ]


offsetAndMetadata_builder :: Spec
offsetAndMetadata_builder =
  it "OffsetAndMetadata: builder applies metadata + leader epoch" $ do
    let oam0 = SDK.offsetAndMetadata 42
        oam = SDK.withLeaderEpoch 7 (SDK.withMetadata "ckpt" oam0)
    SDK.oamOffset oam `shouldBe` 42
    SDK.oamMetadata oam `shouldBe` "ckpt"
    SDK.oamLeaderEpoch oam `shouldBe` Just 7


subscriptionPattern_match :: Spec
subscriptionPattern_match =
  it "SubscriptionPattern: matches by regex" $ do
    case SDK.subscriptionPattern "events\\.[a-z]+" of
      Left e -> error ("regex compile failed: " <> e)
      Right sp -> do
        (SDK.matchesSubscriptionPattern sp "events.user") `shouldBe` True
        (not (SDK.matchesSubscriptionPattern sp "events.UPPER")) `shouldBe` True


offsetCommitCallback_compiles :: Spec
offsetCommitCallback_compiles =
  it "OffsetCommitCallback type unifies with user callbacks" $ do
    let cb :: SDK.OffsetCommitCallback
        cb _ _ = pure ()
    cb Map.empty Nothing -- shape-only check; success is the unit return
    SDK.noopOffsetCommitCallback Map.empty Nothing


----------------------------------------------------------------------
-- MockProcessorContext
----------------------------------------------------------------------

mock_captures_forwards :: Spec
mock_captures_forwards =
  it "MockProcessorContext: forwardRecord lands in capturedForwards" $ do
    mock <- M.newMockProcessorContext "test-app" (P.TaskId 0 0)
    let ctx = M.mockContext mock
    P.forwardRecord ctx (mkRecord (Just "k1") ("hello" :: T.Text) (Timestamp 0))
    P.forwardRecord ctx (mkRecord (Just "k2") ("world" :: T.Text) (Timestamp 1))
    fs <- M.capturedForwards mock
    length fs `shouldBe` 2


mock_captures_punctuators :: Spec
mock_captures_punctuators =
  it "MockProcessorContext: schedule registers a punctuator" $ do
    mock <- M.newMockProcessorContext "test-app" (P.TaskId 0 0)
    let ctx = M.mockContext mock
    let pun = P.Punctuator (\_ -> pure ())
    _ <- P.schedule ctx 1000 P.WallClockTimePunctuation pun
    ps <- M.capturedPunctuators mock
    length ps `shouldBe` 1
    map M.cpType ps `shouldBe` [P.WallClockTimePunctuation]


mock_commit_request_flag :: Spec
mock_commit_request_flag =
  it "MockProcessorContext: requestCommit toggles the commit flag" $ do
    mock <- M.newMockProcessorContext "test-app" (P.TaskId 0 0)
    M.commitRequested mock >>= (`shouldBe` False)
    P.requestCommit (M.mockContext mock)
    M.commitRequested mock >>= (`shouldBe` True)
    M.readCommitRequested mock >>= (`shouldBe` True)
    M.commitRequested mock >>= (`shouldBe` False)


----------------------------------------------------------------------
-- Kafka.Common
----------------------------------------------------------------------

common_node_endpoint_smoke :: Spec
common_node_endpoint_smoke =
  it "Kafka.Common: Node/Endpoint/Cluster value types compose" $ do
    let !node = Common.Node 1 "broker-1" 9092 (Just "rack-a")
        !ep = Common.Endpoint "PLAINTEXT" "broker-1" 9092 "PLAINTEXT"
        !cl =
          Common.emptyCluster
            { Common.clusterId = Just "cid"
            , Common.clusterNodes = [node]
            , Common.clusterController = Just node
            }
    Common.nodeId node `shouldBe` 1
    Common.endpointPort ep `shouldBe` 9092
    Common.clusterId cl `shouldBe` Just "cid"
    Common.clusterController cl `shouldBe` Just node


common_acl_wildcard :: Spec
common_acl_wildcard =
  it "Kafka.Common.Acl: wildcard filter matches everything by construction" $ do
    let f = Acl.anyAclBindingFilter
    Acl.acefOperation (Acl.aclbfEntryFilter f) `shouldBe` Acl.AclAnyOp
    Acl.acefPermissionType (Acl.aclbfEntryFilter f) `shouldBe` Acl.AclAnyPerm
    Resource.rpfResourceType (Acl.aclbfPatternFilter f)
      `shouldBe` Resource.ResourceAny


common_quota_helpers :: Spec
common_quota_helpers =
  it "Kafka.Common.Quota: ClientQuotaEntity + filter helpers" $ do
    let e =
          Quota.clientQuotaEntity
            [ ("user", Just "alice")
            , ("client-id", Nothing)
            ]
    Map.size (Quota.cqeEntries e) `shouldBe` 2
    let c = Quota.exactMatch "user" "alice"
    Quota.cqfcMatchType c `shouldBe` Quota.MatchExact "alice"
    let d = Quota.defaultEntity "user"
    Quota.cqfcMatchType d `shouldBe` Quota.MatchDefault


timeextractor_use_partition_time :: Spec
timeextractor_use_partition_time =
  it
    "Kafka.Streams.Time.usePartitionTimeOnInvalidTimestamp: fall back to stream time on -1"
    $ do
      let ex :: TE T.Text T.Text
          ex = usePartitionTimeOnInvalidTimestamp
      -- Valid embedded timestamp ⇒ returned unchanged.
      r1 <-
        runTimestampExtractor
          ex
          (Just "k")
          ("x" :: T.Text)
          (Timestamp 100)
          initialStreamTime
      r1 `shouldBe` Timestamp 100
      -- Sentinel -1 + known stream time ⇒ stream time.
      r2 <-
        runTimestampExtractor
          ex
          (Just "k")
          ("x" :: T.Text)
          (Timestamp (-1))
          (StreamTime (Timestamp 42))
      r2 `shouldBe` Timestamp 42


-- Local type-alias so the @ScopedTypeVariables@-flavoured signature
-- in 'timeextractor_use_partition_time' reads cleanly.
type TE k v = Kafka.Streams.Time.TimestampExtractor k v


----------------------------------------------------------------------
-- AdminClient long-tail RPCs
----------------------------------------------------------------------

-- Smoke test: the public types of the new admin operations are
-- reachable and constructable. The operations themselves talk
-- to a real broker and are covered by the integration suite
-- under .github/workflows/wireform-kafka-integration.yml.
admin_extras_value_smoke :: Spec
admin_extras_value_smoke =
  it "Kafka.Client.AdminClient long-tail: public value types compose" $ do
    let np =
          Adm.NewPartitions
            { Adm.npTopicName = "events"
            , Adm.npTotalCount = 6
            , Adm.npNewAssignments = Just [[0, 1, 2], [1, 2, 0], [2, 0, 1]]
            }
    Adm.npTotalCount np `shouldBe` 6
    let gl =
          Adm.GroupListing
            { Adm.glGroupId = "g1"
            , Adm.glProtocolType = "consumer"
            , Adm.glState = Just Common.GroupStable
            , Adm.glType = Just Common.ConsumerGroup
            }
    Adm.glState gl `shouldBe` Just Common.GroupStable
    let acrOk =
          Adm.AclCreationResult
            { Adm.acrBinding =
                Acl.AclBinding
                  (Resource.ResourcePattern Resource.ResourceTopic "t" Resource.PatternLiteral)
                  (Acl.AccessControlEntry "User:alice" "*" Acl.AclRead Acl.AclAllow)
            , Adm.acrError = Nothing
            }
    Adm.acrError acrOk `shouldBe` Nothing
    let adr = Adm.AclDeletionResult {Adm.adrDeletedCount = 3, Adm.adrError = Nothing}
    Adm.adrDeletedCount adr `shouldBe` 3
    -- v3b: partition reassignment + transaction admin value types.
    let prs = Adm.PartitionReassignmentSpec "in" 0 (Just [1, 2, 3])
    Adm.prsTopic prs `shouldBe` "in"
    let opr = Adm.OngoingPartitionReassignment "in" 0 [1, 2, 3] [4] []
    Adm.oprAddingReplicas opr `shouldBe` [4]
    let tl =
          Adm.TransactionListing
            { Adm.tlTransactionalId = "txn-1"
            , Adm.tlProducerId = 100
            , Adm.tlState = "Ongoing"
            }
    Adm.tlState tl `shouldBe` "Ongoing"
    let td =
          Adm.TransactionDescription
            { Adm.tdTransactionalId = "txn-1"
            , Adm.tdProducerId = 100
            , Adm.tdProducerEpoch = 7
            , Adm.tdTimeoutMs = 60_000
            , Adm.tdStartTimeMs = 0
            , Adm.tdState = "Ongoing"
            , Adm.tdTopicPartitions =
                [Adm.TransactionTopicPartitions "out" [0, 1, 2]]
            }
    map Adm.ttpTopic (Adm.tdTopicPartitions td) `shouldBe` ["out"]
    let cqe =
          Adm.ClientQuotaEntry
            { Adm.cqeEntity =
                Quota.clientQuotaEntity [("user", Just "alice")]
            , Adm.cqeValues = Map.fromList [("producer_byte_rate", 1024)]
            }
    Adm.cqeValues cqe `shouldBe` Map.fromList [("producer_byte_rate", 1024)]
    -- v3c: SCRAM credentials + producer / log-dir / delegation tokens.
    let sci = Adm.ScramCredentialInfo Adm.ScramSha256 4096
    Adm.sciIterations sci `shouldBe` 4096
    let scu = Adm.ScramCredentialUpsertion "alice" Adm.ScramSha512 8192 "salt" "hashed"
    Adm.scuMechanism scu `shouldBe` Adm.ScramSha512
    let ps = Adm.ProducerState 5001 1 42 0 0 (-1)
    Adm.psProducerId ps `shouldBe` 5001
    let ldd = Adm.LogDirDescription "/var/log/kafka-data" 0 (1024 * 1024) 512000 []
    Adm.lddPath ldd `shouldBe` "/var/log/kafka-data"
    let dt =
          Adm.DelegationToken
            { Adm.dtTokenId = "tid"
            , Adm.dtHmac = "secret"
            , Adm.dtOwner = ("User", "alice")
            , Adm.dtTokenRequester = ("User", "alice")
            , Adm.dtIssueTimestamp = 0
            , Adm.dtExpiryTimestamp = 60000
            , Adm.dtMaxTimestamp = 86400000
            }
    Adm.dtTokenId dt `shouldBe` "tid"


----------------------------------------------------------------------
-- v3+: leftover gaps from v2 honest-list
----------------------------------------------------------------------

unlimited_windows_smoke :: Spec
unlimited_windows_smoke =
  it "unlimitedWindows assigns a single never-ending window per record" $ do
    let ws = W.unlimitedWindows
        ws_assigned = W.windowsAssign ws (Timestamp 1_000_000_000)
    length ws_assigned `shouldBe` 1


list_serde_roundtrip :: Spec
list_serde_roundtrip =
  it "listSerde encodes + decodes a [Text]" $ do
    let s = Serde.listSerde Serde.textSerde
        xs = ["alpha", "bravo", "charlie"] :: [T.Text]
        bs = Serde.serialize s xs
    Serde.deserialize s bs `shouldBe` Right xs
    -- empty list edge case
    Serde.deserialize s (Serde.serialize s ([] :: [T.Text])) `shouldBe` Right []


task_assignor_default :: Spec
task_assignor_default =
  it "defaultTaskAssignor hands every task to the first client" $ do
    let pidA = TA.ProcessId (UUID.fromWords 0 0 0 1)
        pidB = TA.ProcessId (UUID.fromWords 0 0 0 2)
        states =
          Map.fromList
            [
              ( pidA
              , TA.KafkaStreamsState pidA 1 Map.empty Set.empty Set.empty Nothing Map.empty
              )
            ,
              ( pidB
              , TA.KafkaStreamsState pidB 1 Map.empty Set.empty Set.empty Nothing Map.empty
              )
            ]
        tasks = Map.empty
        app = TA.ApplicationState tasks states TA.defaultAssignmentConfigs
    out <- TA.taAssign TA.defaultTaskAssignor app
    Map.keys (TA.taAssignments out) `shouldBe` [pidA]


per_error_streams_exceptions :: Spec
per_error_streams_exceptions =
  it "streams errors: per-Java-class discriminated constructors" $ do
    -- Catchable by their specific type now; smoke test the
    -- record / newtype shape.
    let _ = Errors.BrokerNotFoundException "no broker"
        _ = Errors.MissingSourceTopicException "events" "deleted"
        _ = Errors.TaskAssignmentException "boom"
        _ = Errors.TaskCorruptedException "0_0" "wal corrupt"
        _ = Errors.UnknownStateStoreException "counts"
        _ = Errors.LockException "/tmp/state" "held by other"
        _ = Errors.InvalidConfigurationException "bad knob"
        _ = Errors.InvalidPartitionsException "negative count"
    pure () :: IO ()


----------------------------------------------------------------------
-- v4: protocol-codegen-dependent admin RPCs
----------------------------------------------------------------------

-- The wrapper functions themselves can't be invoked without a
-- live broker, but the public value types they expose must
-- compose so user code can build requests / pattern-match
-- responses offline.

feature_metadata_value_smoke :: Spec
feature_metadata_value_smoke =
  it "FeatureMetadata + ranges compose" $ do
    let !fm =
          Adm.FeatureMetadata
            { Adm.fmSupportedFeatures =
                Map.singleton
                  "metadata.version"
                  (Adm.SupportedFeatureRange 0 20)
            , Adm.fmFinalizedFeatures =
                Map.singleton
                  "metadata.version"
                  (Adm.FinalizedFeatureLevel 14 14)
            , Adm.fmFinalizedFeaturesEpoch = Just 42
            }
    Map.size (Adm.fmSupportedFeatures fm) `shouldBe` 1
    Map.size (Adm.fmFinalizedFeatures fm) `shouldBe` 1
    Adm.fmFinalizedFeaturesEpoch fm `shouldBe` Just 42


feature_update_value_smoke :: Spec
feature_update_value_smoke =
  it "FeatureUpdate / UpgradeType compose" $ do
    let _ = Adm.FeatureUpdate "metadata.version" 16 Adm.UpgradeOnly
        _ = Adm.FeatureUpdate "metadata.version" 14 Adm.SafeDowngrade
        _ = Adm.FeatureUpdate "metadata.version" 14 Adm.UnsafeDowngrade
    pure () :: IO ()


abort_transaction_spec_value_smoke :: Spec
abort_transaction_spec_value_smoke =
  it "AbortTransactionSpec composes with multiple partitions" $ do
    let !spec =
          Adm.AbortTransactionSpec
            { Adm.atsProducerId = 1000
            , Adm.atsProducerEpoch = 7
            , Adm.atsCoordinatorEpoch = 3
            , Adm.atsTopicPartitions = [("txn-topic", [0, 1, 2])]
            }
    Adm.atsProducerId spec `shouldBe` 1000
    length (Adm.atsTopicPartitions spec) `shouldBe` 1


consumer_group_describe2_value_smoke :: Spec
consumer_group_describe2_value_smoke =
  it "KIP-848 ConsumerGroupDescription2 compose" $ do
    let !m =
          Adm.ConsumerGroupMember
            { Adm.cgmMemberId = "m-1"
            , Adm.cgmInstanceId = Nothing
            , Adm.cgmRackId = Just "rack-a"
            , Adm.cgmMemberEpoch = 5
            , Adm.cgmClientId = "c"
            , Adm.cgmClientHost = "h"
            , Adm.cgmSubscribedTopicNames = ["t-1"]
            , Adm.cgmSubscribedTopicRegex = Nothing
            , Adm.cgmAssignedPartitions = [("t-1", [0])]
            , Adm.cgmTargetAssignment = [("t-1", [0])]
            }
        !d =
          Adm.ConsumerGroupDescription2
            { Adm.cgd2GroupId = "g-1"
            , Adm.cgd2GroupState = "STABLE"
            , Adm.cgd2GroupEpoch = 5
            , Adm.cgd2AssignmentEpoch = 5
            , Adm.cgd2AssignorName = "uniform"
            , Adm.cgd2Members = [m]
            , Adm.cgd2AuthorizedOperations = 0
            }
    length (Adm.cgd2Members d) `shouldBe` 1


share_group_describe_value_smoke :: Spec
share_group_describe_value_smoke =
  it "KIP-932 ShareGroupDescription compose" $ do
    let !m =
          Adm.ShareGroupMember
            { Adm.sgmMemberId = "m-1"
            , Adm.sgmRackId = Nothing
            , Adm.sgmMemberEpoch = 1
            , Adm.sgmClientId = "c"
            , Adm.sgmClientHost = "h"
            , Adm.sgmSubscribedTopicNames = ["share-topic"]
            , Adm.sgmAssignedPartitions = [("share-topic", [0, 1])]
            }
        !d =
          Adm.ShareGroupDescription
            { Adm.sgdGroupId = "share-g-1"
            , Adm.sgdGroupState = "STABLE"
            , Adm.sgdGroupEpoch = 1
            , Adm.sgdAssignmentEpoch = 1
            , Adm.sgdAssignorName = "simple"
            , Adm.sgdMembers = [m]
            , Adm.sgdAuthorizedOperations = 0
            }
    length (Adm.sgdMembers d) `shouldBe` 1


fenced_producer_value_smoke :: Spec
fenced_producer_value_smoke =
  it "FencedProducer composes" $ do
    let !fp =
          Adm.FencedProducer
            { Adm.fpTransactionalId = "txn-id"
            , Adm.fpProducerId = 123
            , Adm.fpProducerEpoch = 4
            }
    Adm.fpTransactionalId fp `shouldBe` "txn-id"
    Adm.fpProducerEpoch fp `shouldBe` 4


----------------------------------------------------------------------
-- v4: KIP-924 TaskAssignor plug-in
----------------------------------------------------------------------

-- We can't spin up a full 'KafkaStreams' in a unit test, so this
-- pair exercises the wiring at the config layer: the plug-in
-- carries through 'StreamsConfig' unchanged, and a directly-
-- invoked plug-in produces the expected 'TaskAssignment' shape.

task_assignor_plugin_configurable :: Spec
task_assignor_plugin_configurable =
  it "TaskAssignor plug-in carries through StreamsConfig" $ do
    let !cfg =
          Kafka.Streams.Config.defaultStreamsConfig
            { Kafka.Streams.Config.taskAssignor =
                Just TA.defaultTaskAssignor
            }
    case Kafka.Streams.Config.taskAssignor cfg of
      Nothing -> (False) `shouldBe` True
      Just _ -> pure ()


task_assignor_plugin_invocable :: Spec
task_assignor_plugin_invocable =
  it "TaskAssignor plug-in produces a TaskAssignment" $ do
    let pid = TA.ProcessId UUID.nil
        emptyApp =
          TA.ApplicationState
            { TA.asAllTasks = Map.empty
            , TA.asKafkaStreamsStates =
                Map.singleton
                  pid
                  TA.KafkaStreamsState
                    { TA.kssProcessId = pid
                    , TA.kssNumProcessingThreads = 1
                    , TA.kssClientTags = Map.empty
                    , TA.kssPreviousActiveTasks = Set.empty
                    , TA.kssPreviousStandbyTasks = Set.empty
                    , TA.kssRackId = Nothing
                    , TA.kssTaskLags = Map.empty
                    }
            , TA.asAssignmentConfigs = TA.defaultAssignmentConfigs
            }
    out <- TA.taAssign TA.defaultTaskAssignor emptyApp
    -- The default assignor hands every task to the first client.
    -- Even with no tasks, the single registered ProcessId still
    -- gets an (empty) KafkaStreamsAssignment entry — this
    -- confirms the assignor surface is end-to-end invocable
    -- (the group leader consumes the same shape).
    Map.keysSet (TA.taAssignments out) `shouldBe` Set.singleton pid


----------------------------------------------------------------------
-- v4: KIP-714 deterministic client-instance-id derivation
----------------------------------------------------------------------

telemetry_id_is_16_bytes :: Spec
telemetry_id_is_16_bytes =
  it "clientInstanceIdFromText returns exactly 16 bytes" $ do
    BS.length (Tel.clientInstanceIdFromText "") `shouldBe` 16
    BS.length (Tel.clientInstanceIdFromText "short") `shouldBe` 16
    BS.length
      ( Tel.clientInstanceIdFromText
          "a-very-long-application-id-bigger-than-sixteen-bytes"
      )
      `shouldBe` 16


telemetry_id_is_deterministic :: Spec
telemetry_id_is_deterministic =
  it "clientInstanceIdFromText is deterministic" $ do
    let a = Tel.clientInstanceIdFromText "wireform-streams-app"
        b = Tel.clientInstanceIdFromText "wireform-streams-app"
    a `shouldBe` b


telemetry_id_distinguishes_inputs :: Spec
telemetry_id_distinguishes_inputs =
  it "clientInstanceIdFromText distinguishes distinct seeds" $
    ( Tel.clientInstanceIdFromText "alpha"
        /= Tel.clientInstanceIdFromText "beta"
    )
      `shouldBe` True


telemetry_id_pads_short_inputs :: Spec
telemetry_id_pads_short_inputs =
  it "clientInstanceIdFromText pads short seeds with \\0" $ do
    let bs = Tel.clientInstanceIdFromText "abc"
    BS.take 3 bs `shouldBe` "abc"
    BS.drop 3 bs `shouldBe` BS.replicate 13 0


telemetry_id_truncates_long_inputs :: Spec
telemetry_id_truncates_long_inputs =
  it "clientInstanceIdFromText truncates seeds longer than 16 bytes" $ do
    let !seed = "abcdefghijklmnopqrstuvwxyz" -- 26 bytes
        bs = Tel.clientInstanceIdFromText seed
    bs `shouldBe` "abcdefghijklmnop"


----------------------------------------------------------------------
-- v4: KafkaStreams runtime hooks (clientInstanceId + assignor)
----------------------------------------------------------------------

-- A trivial source→sink topology suitable for instantiating
-- 'KafkaStreams' without a live broker. Mirrors what the
-- other state-listener tests in this suite use.
mkTinyKafkaStreams
  :: Kafka.Streams.Config.StreamsConfig -> IO Kafka.Streams.Runtime.KafkaStreams
mkTinyKafkaStreams cfg = do
  b <- Kafka.Streams.Imperative.newStreamsBuilder
  s <-
    Kafka.Streams.Imperative.streamFromTopic
      b
      (Kafka.Streams.Imperative.topicName "in")
      (Kafka.Streams.Imperative.consumed Kafka.Streams.Imperative.textSerde Kafka.Streams.Imperative.textSerde)
  Kafka.Streams.Imperative.toTopic
    (Kafka.Streams.Imperative.topicName "out")
    (Kafka.Streams.Imperative.produced Kafka.Streams.Imperative.textSerde Kafka.Streams.Imperative.textSerde)
    s
  topo <- Kafka.Streams.Imperative.buildTopology b
  case Kafka.Streams.Imperative.validateTopology topo of
    Left err -> error (show err)
    Right v -> Kafka.Streams.Runtime.newKafkaStreams cfg v


streams_runtime_telemetry_id :: Spec
streams_runtime_telemetry_id =
  it "kafkaStreamsClientInstanceId derives from application.id" $ do
    ks <-
      mkTinyKafkaStreams
        Kafka.Streams.Config.defaultStreamsConfig
          { Kafka.Streams.Config.applicationId = "ti-app"
          , Kafka.Streams.Config.bootstrapServers = ["mock:0"]
          }
    bs <- Kafka.Streams.Runtime.kafkaStreamsClientInstanceId ks
    BS.length bs `shouldBe` 16
    bs `shouldBe` Tel.clientInstanceIdFromText "ti-app"


