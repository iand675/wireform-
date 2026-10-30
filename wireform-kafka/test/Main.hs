{- |
Module      : Main
Description : Test suite entry point for kafka-native
Copyright   : (c) 2025
License     : BSD-3-Clause

Main test suite for the kafka-native library.

Tests are organized into groups:

* Protocol tests - Round-trip serialization tests for primitives
* Compression tests - Compression/decompression verification
* Property tests - Hedgehog property-based tests

To run tests:

> stack test
-}
module Main (main) where

import Client.AdminClientConfigSpec qualified
import Client.AdminClientSpec qualified
import Client.AdminTimeoutsSpec qualified
import Client.BatchAccumulatorSpec qualified
import Client.BatchSplittingSpec qualified
import Client.ConfigParitySpec qualified
import Client.ConfigValidationSpec qualified
import Client.ConsumerConfigSpec qualified
import Client.ConsumerGroupV2Spec qualified
import Client.ConsumerSnapshotsSpec qualified
import Client.EnvSpec qualified
import Client.FetchSelectionSpec qualified
import Client.FilterSpec qualified
import Client.FutureSpec qualified
import Client.GroupSpec qualified
import Client.HeartbeatRejoinSpec qualified
import Client.InterceptorSpec qualified
import Client.MetadataCacheControlSpec qualified
import Client.MetadataLeaderUpdateSpec qualified
import Client.MetadataSpec qualified
import Client.MetricsRegistrySpec qualified
import Client.MockBrokerAdminSpec qualified
import Client.MockBrokerAdvancedSpec qualified
import Client.MockBrokerCoopSpec qualified
import Client.MockBrokerExtSpec qualified
import Client.MockBrokerFailureModesSpec qualified
import Client.MockBrokerIdempotentSpec qualified
import Client.MockBrokerNetSpec qualified
import Client.MockBrokerProtoSpec qualified
import Client.MockBrokerSpec qualified
import Client.MockBrokerStoreSpec qualified
import Client.MockShareConsumerSpec qualified
import Client.Murmur2Spec qualified
import Client.PartitionerSpec qualified
import Client.PipelineSpec qualified
import Client.ProducerConsumerLifecycleSpec qualified
import Client.ProducerRetrySpec qualified
import Client.TelemetryLogSpec qualified
import Client.ProducerTimeoutSpec qualified
import Client.ProducerTransactionWiringSpec qualified
import Client.RackAwareSpec qualified
import Client.RebalanceListenerSpec qualified
import Client.RecordMetadataSpec qualified
import Client.ResponseFrameSpec qualified
import Client.RetryClassifierSpec qualified
import Client.SerdeContextSpec qualified
import Client.ShareConsumerHelpersSpec qualified
import Client.ShareConsumerSpec qualified
import Client.StatsJsonSpec qualified
import Client.SubscribeSpec qualified
import Client.TelemetryPushRuntimeSpec qualified
import Client.TelemetryPushSpec qualified
import Client.TopicIdSpec qualified
import Client.TransactionCoordinatorSpec qualified
import Client.TransactionHelpersSpec qualified
import Client.TransactionSpec qualified
import Codegen.WireGeneratorSpec qualified
import Compression.RingSpec qualified
import Hedgehog.Internal.Config (resolveSkip)
import Network.AuthSpec qualified
import Network.BootstrapSpec qualified
import Network.ConnectionHelpersSpec qualified
import Network.ConnectionLivenessSpec qualified
import Network.ConnectionRetrySpec qualified
import Network.FrameParserSpec qualified
import Network.OAuthOidcSpec qualified
import Network.ReauthDriverSpec qualified
import Network.SaslReauthSpec qualified
import Network.TlsHandshakeSpec qualified
import Network.TlsOffloadSpec qualified
import Network.TransportSpec qualified
import Protocol.ApiVersionsSpec qualified
import Protocol.CRC32CSpec qualified
import Protocol.CompressionSpec qualified
import Protocol.Generated.ComprehensiveSpec qualified
import Protocol.Generated.KnownGoodSpec qualified
import Protocol.Generated.SimpleRoundTripSpec qualified
import Protocol.PrimitivesSpec qualified
import Protocol.RecordBatchAttributesSpec qualified
import Protocol.RecordBatchSpec qualified
import Protocol.RecordBatchWireSpec qualified
import Protocol.RoundTripSpec qualified
import Protocol.SliceVectorSpec qualified
import Protocol.StreamingSinkSpec qualified
import Protocol.VersionHandlingSpec qualified
import Protocol.VersionNegotiationSpec qualified
import Protocol.WireCodecParitySpec qualified
import Protocol.WireSpec qualified
import Serde.ProtoBufSpec qualified
import Test.Syd


main :: IO ()
main = do
  -- Force Hedgehog's "HEDGEHOG_SKIP" CAF before sydtest starts its workers.
  -- Every property forces it first; when many capabilities hit it at once,
  -- GHC 9.8.4's RTS can lose the wakeup of a thread blocked on it, leaving
  -- one property hung until the per-test timeout.
  _ <- resolveSkip Nothing
  knownGoodTests <- Protocol.Generated.KnownGoodSpec.tests
  comprehensiveTests <- Protocol.Generated.ComprehensiveSpec.tests
  sydTest $ tests knownGoodTests comprehensiveTests


tests :: Spec -> Spec -> Spec
tests knownGoodTests comprehensiveTests =
  describe
    "kafka-native"
    $ sequence_
      [ protocolTests
      , generatedTests knownGoodTests comprehensiveTests
      , versionTests
      , compressionTests
      , clientTests
      , networkTests
      , serdeTests
      ]


serdeTests :: Spec
serdeTests =
  describe
    "Serde"
    $ sequence_
      [ Serde.ProtoBufSpec.tests
      ]


protocolTests :: Spec
protocolTests =
  describe
    "Protocol"
    $ sequence_
      [ Protocol.CRC32CSpec.spec
      , Protocol.PrimitivesSpec.tests
      , Protocol.RoundTripSpec.tests
      , Protocol.RecordBatchSpec.tests
      , Protocol.RecordBatchAttributesSpec.tests
      , Protocol.WireSpec.tests
      , Protocol.RecordBatchWireSpec.tests
      , Protocol.SliceVectorSpec.tests
      , Protocol.VersionNegotiationSpec.tests
      ]


generatedTests :: Spec -> Spec -> Spec
generatedTests knownGoodTests comprehensiveTests =
  describe
    "Generated Messages"
    $ sequence_
      [ Protocol.Generated.SimpleRoundTripSpec.tests
      , knownGoodTests
      , comprehensiveTests
      ]


versionTests :: Spec
versionTests = Protocol.VersionHandlingSpec.tests


compressionTests :: Spec
compressionTests =
  describe
    "Compression"
    $ sequence_
      [ Protocol.CompressionSpec.compressionTests
      , Protocol.StreamingSinkSpec.tests
      ]


clientTests :: Spec
clientTests =
  describe
    "Client"
    $ sequence_
      [ Protocol.ApiVersionsSpec.tests
      , Client.BatchAccumulatorSpec.tests
      , Client.ConsumerConfigSpec.consumerConfigSpec
      , Client.MetadataSpec.tests
      , Client.PartitionerSpec.partitionerSpec
      , Client.ProducerTimeoutSpec.tests
      , Client.ProducerConsumerLifecycleSpec.lifecycleSpec
      , Client.TransactionSpec.transactionSpec
      , Client.TransactionCoordinatorSpec.transactionCoordinatorSpec
      , Client.ProducerTransactionWiringSpec.tests
      , Client.InterceptorSpec.tests
      , Client.MetadataLeaderUpdateSpec.tests
      , Client.MetricsRegistrySpec.tests
      , Client.Murmur2Spec.tests
      , Client.StatsJsonSpec.tests
      , Client.ConsumerGroupV2Spec.tests
      , Client.ShareConsumerSpec.tests
      , Client.TelemetryPushSpec.tests
      , Client.TelemetryPushRuntimeSpec.tests
      , Client.RecordMetadataSpec.tests
      , Client.FilterSpec.tests
      , Client.FutureSpec.tests
      , Client.TopicIdSpec.tests
      , Client.RebalanceListenerSpec.tests
      , Client.AdminTimeoutsSpec.tests
      , Client.RetryClassifierSpec.tests
      , Client.BatchSplittingSpec.tests
      , Client.ResponseFrameSpec.tests
      , Client.ConsumerSnapshotsSpec.tests
      , Client.MetadataCacheControlSpec.tests
      , Client.RackAwareSpec.tests
      , Client.TransactionHelpersSpec.tests
      , Network.ConnectionHelpersSpec.tests
      , Client.AdminClientConfigSpec.tests
      , Client.SerdeContextSpec.tests
      , Client.ShareConsumerHelpersSpec.tests
      , Client.MockShareConsumerSpec.tests
      , Client.AdminClientSpec.tests
      , Client.GroupSpec.groupSpec
      , Client.MockBrokerSpec.tests
      , Client.MockBrokerFailureModesSpec.tests
      , Client.MockBrokerAdvancedSpec.tests
      , Client.MockBrokerExtSpec.tests
      , Client.MockBrokerIdempotentSpec.tests
      , Client.MockBrokerAdminSpec.tests
      , Client.MockBrokerCoopSpec.tests
      , Client.MockBrokerNetSpec.tests
      , Client.MockBrokerStoreSpec.tests
      , Client.MockBrokerProtoSpec.tests
      , Client.ConfigParitySpec.tests
      , Client.ConfigValidationSpec.tests
      , Client.EnvSpec.tests
      , Client.FetchSelectionSpec.tests
      , Client.HeartbeatRejoinSpec.tests
      , Codegen.WireGeneratorSpec.tests
      , Protocol.WireCodecParitySpec.tests
      , Client.ProducerRetrySpec.tests
      , Client.TelemetryLogSpec.tests
      , Client.PipelineSpec.tests
      , Client.SubscribeSpec.tests
      , Network.ConnectionLivenessSpec.tests
      ]


networkTests :: Spec
networkTests =
  describe
    "Network"
    $ sequence_
      [ Network.ConnectionRetrySpec.tests
      , Network.AuthSpec.authSpec
      , Network.TlsHandshakeSpec.tests
      , Network.TransportSpec.tests
      , Network.TlsOffloadSpec.tests
      , Network.SaslReauthSpec.tests
      , Network.OAuthOidcSpec.tests
      , Network.BootstrapSpec.tests
      , Network.FrameParserSpec.tests
      , Compression.RingSpec.ringCompressionTests
      , Network.ReauthDriverSpec.tests
      ]
