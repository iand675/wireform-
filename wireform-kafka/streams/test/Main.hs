module Main (main) where

import Hedgehog.Internal.Config (resolveSkip)
import Streams.AggregationSpec qualified
import Streams.AssignorSpec qualified
import Streams.AsyncIOSpec qualified
import Streams.BackfillSpec qualified
import Streams.BoundedSuppressSpec qualified
import Streams.CacheSpec qualified
import Streams.CogroupSpec qualified
import Streams.DSLFacadeSpec qualified
import Streams.DSLSpec qualified
import Streams.DriverSpec qualified
import Streams.DynamicThreadsSpec qualified
import Streams.EOSRuntimeSpec qualified
import Streams.EOSSpec qualified
import Streams.EndToEndChainSpec qualified
import Streams.ExceptionHandlerSpec qualified
import Streams.ExtensionsSpec qualified
import Streams.ForeignKeyJoinDSLSpec qualified
import Streams.HealthSpec qualified
import Streams.IdiomaticDSLSpec qualified
import Streams.IdleHeartbeatSpec qualified
import Streams.InteractiveQueriesSpec qualified
import Streams.JoinSpec qualified
import Streams.KGroupedTableSpec qualified
import Streams.LagSpec qualified
import Streams.MetricsSpec qualified
import Streams.MockAdvancedSpec qualified
import Streams.MockClusterSpec qualified
import Streams.MockDriverModesSpec qualified
import Streams.MockFailureModesSpec qualified
import Streams.MoreParitySpec qualified
import Streams.MoreParityThreeSpec qualified
import Streams.MorePartiyTwoSpec qualified
import Streams.MultiInstanceHarnessSpec qualified
import Streams.MultiInstanceRebalanceSpec qualified
import Streams.MultiTaskSpec qualified
import Streams.NamedSpec qualified
import Streams.ObservabilityOTelSpec qualified
import Streams.ParityBatchSpec qualified
import Streams.PersistentStoreSpec qualified
import Streams.ProbingRebalanceRuntimeSpec qualified
import Streams.ProbingRebalanceSpec qualified
import Streams.ProcessorAndStoreExtrasSpec qualified
import Streams.Properties.AtLeastOnceRedeliverySpec qualified
import Streams.Properties.CDCSourceSpec qualified
import Streams.Properties.ChangelogReplaySpec qualified
import Streams.Properties.EOSChaosSpec qualified
import Streams.Properties.KVStoreSMSpec qualified
import Streams.Properties.KeyGroupAssignorSpec qualified
import Streams.Properties.KeyGroupDispatchSpec qualified
import Streams.Properties.ObservabilityTopologySpec qualified
import Streams.Properties.OperatorWatermarkSpec qualified
import Streams.Properties.OptimizerEqSpec qualified
import Streams.Properties.OrphanTopicsSpec qualified
import Streams.Properties.RebalanceBridgeSpec qualified
import Streams.Properties.RemoteKVStoreSpec qualified
import Streams.Properties.SchemaVersionedSpec qualified
import Streams.Properties.SnapshotSpec qualified
import Streams.Properties.StoreRefSpec qualified
import Streams.Properties.TTLSpec qualified
import Streams.Properties.TieredStoreSpec qualified
import Streams.Properties.TwoPhaseSinkSpec qualified
import Streams.Properties.WatermarkCoordSpec qualified
import Streams.Properties.WatermarkSpec qualified
import Streams.Properties.WatermarkWiringSpec qualified
import Streams.Properties.WindowMathSpec qualified
import Streams.Properties.WorkerPoolConcurrentSpec qualified
import Streams.Properties.WorkerPoolSMSpec qualified
import Streams.PunctuatorSpec qualified
import Streams.QueryAndDiscoverySpec qualified
import Streams.QuerySpec qualified
import Streams.RackAwareAssignorSpec qualified
import Streams.RemoteIQSpec qualified
import Streams.ReplaySpec qualified
import Streams.RevocationGraceSpec qualified
import Streams.RotatingFileSinkSpec qualified
import Streams.RuntimeDriverSpec qualified
import Streams.SchemaRegistryCompatSpec qualified
import Streams.SchemaRegistryFormatsSpec qualified
import Streams.SchemaRegistryHttpSpec qualified
import Streams.SchemaRegistrySerdeSpec qualified
import Streams.SdkParitySpec qualified
import Streams.SerdeSpec qualified
import Streams.StableNamesSpec qualified
import Streams.StandbyDriverSpec qualified
import Streams.StandbySpec qualified
import Streams.StandbyTaskSpec qualified
import Streams.StateListenerSpec qualified
import Streams.StateStoreSpec qualified
import Streams.StoresExtraSpec qualified
import Streams.StreamConvSpec qualified
import Streams.StreamsConfigSurfaceSpec qualified
import Streams.SuppressSpec qualified
import Streams.TestUtilsSpec qualified
import Streams.TimeSpec qualified
import Streams.TimestampedSpec qualified
import Streams.TopologyDescriptionSpec qualified
import Streams.TopologyFreeArrowSpec qualified
import Streams.TopologyFreeSpec qualified
import Streams.TopologyOptimizationSpec qualified
import Streams.TopologySpec qualified
import Streams.TopologyStatsSpec qualified
import Streams.TransactionalStoreSpec qualified
import Streams.VersionedSpec qualified
import Streams.WindowSpec qualified
import Streams.WindowedSuppressIntegrationSpec qualified
import Streams.WorkerPoolSpec qualified
import Test.Syd


main :: IO ()
main = do
  -- Force Hedgehog's "HEDGEHOG_SKIP" CAF before sydtest starts its workers.
  -- Every property forces it first; when many capabilities hit it at once,
  -- GHC 9.8.4's RTS can lose the wakeup of a thread blocked on it, leaving
  -- one property hung until the per-test timeout.
  _ <- resolveSkip Nothing
  sydTest $
    describe "kafka-streams" $
      sequence_
        [ Streams.SerdeSpec.tests
        , Streams.TimeSpec.tests
        , Streams.StateStoreSpec.tests
        , Streams.WindowSpec.tests
        , Streams.TopologySpec.tests
        , Streams.DriverSpec.tests
        , Streams.DSLSpec.tests
        , Streams.PunctuatorSpec.tests
        , Streams.PersistentStoreSpec.tests
        , Streams.JoinSpec.tests
        , Streams.AggregationSpec.tests
        , Streams.InteractiveQueriesSpec.tests
        , Streams.CacheSpec.tests
        , Streams.EOSSpec.tests
        , Streams.MultiTaskSpec.tests
        , Streams.StandbySpec.tests
        , Streams.AssignorSpec.tests
        , Streams.StreamConvSpec.tests
        , Streams.CogroupSpec.tests
        , Streams.SuppressSpec.tests
        , Streams.TimestampedSpec.tests
        , Streams.VersionedSpec.tests
        , Streams.TopologyDescriptionSpec.tests
        , Streams.MetricsSpec.tests
        , Streams.NamedSpec.tests
        , Streams.WorkerPoolSpec.tests
        , Streams.EOSRuntimeSpec.tests
        , Streams.ExtensionsSpec.tests
        , Streams.TestUtilsSpec.tests
        , Streams.StateListenerSpec.tests
        , Streams.QuerySpec.tests
        , Streams.ParityBatchSpec.tests
        , Streams.MoreParitySpec.tests
        , Streams.EndToEndChainSpec.tests
        , Streams.MorePartiyTwoSpec.tests
        , Streams.MoreParityThreeSpec.tests
        , Streams.MockClusterSpec.tests
        , Streams.MockFailureModesSpec.tests
        , Streams.MockAdvancedSpec.tests
        , Streams.MockDriverModesSpec.tests
        , Streams.ProbingRebalanceSpec.tests
        , Streams.RevocationGraceSpec.tests
        , Streams.StableNamesSpec.tests
        , Streams.TopologyOptimizationSpec.tests
        , Streams.SchemaRegistrySerdeSpec.tests
        , Streams.TransactionalStoreSpec.tests
        , Streams.StreamsConfigSurfaceSpec.tests
        , Streams.ForeignKeyJoinDSLSpec.tests
        , Streams.RuntimeDriverSpec.tests
        , Streams.ExceptionHandlerSpec.tests
        , Streams.KGroupedTableSpec.tests
        , Streams.StoresExtraSpec.tests
        , Streams.QueryAndDiscoverySpec.tests
        , Streams.ProcessorAndStoreExtrasSpec.tests
        , Streams.DynamicThreadsSpec.tests
        , Streams.BoundedSuppressSpec.tests
        , Streams.WindowedSuppressIntegrationSpec.tests
        , Streams.IdleHeartbeatSpec.tests
        , Streams.RackAwareAssignorSpec.tests
        , Streams.ProbingRebalanceRuntimeSpec.tests
        , Streams.StandbyTaskSpec.tests
        , Streams.StandbyDriverSpec.tests
        , Streams.RemoteIQSpec.tests
        , Streams.IdiomaticDSLSpec.tests
        , Streams.DSLFacadeSpec.tests
        , Streams.MultiInstanceRebalanceSpec.tests
        , Streams.RotatingFileSinkSpec.tests
        , Streams.SchemaRegistryCompatSpec.tests
        , Streams.SdkParitySpec.tests
        , Streams.MultiInstanceHarnessSpec.tests
        , Streams.SchemaRegistryHttpSpec.tests
        , Streams.SchemaRegistryFormatsSpec.tests
        , Streams.TopologyFreeSpec.tests
        , Streams.TopologyFreeArrowSpec.tests
        , Streams.AsyncIOSpec.tests
        , Streams.LagSpec.tests
        , Streams.HealthSpec.tests
        , Streams.TopologyStatsSpec.tests
        , Streams.ObservabilityOTelSpec.tests
        , Streams.ReplaySpec.tests
        , Streams.BackfillSpec.tests
        , describe "Properties" $
            sequence_
              [ Streams.Properties.KVStoreSMSpec.tests
              , Streams.Properties.OptimizerEqSpec.tests
              , Streams.Properties.WindowMathSpec.tests
              , Streams.Properties.EOSChaosSpec.tests
              , Streams.Properties.WorkerPoolSMSpec.tests
              , Streams.Properties.ObservabilityTopologySpec.tests
              , Streams.Properties.OrphanTopicsSpec.tests
              , Streams.Properties.ChangelogReplaySpec.tests
              , Streams.Properties.WatermarkSpec.tests
              , Streams.Properties.WorkerPoolConcurrentSpec.tests
              , Streams.Properties.AtLeastOnceRedeliverySpec.tests
              , Streams.Properties.TwoPhaseSinkSpec.tests
              , Streams.Properties.WatermarkCoordSpec.tests
              , Streams.Properties.TTLSpec.tests
              , Streams.Properties.SchemaVersionedSpec.tests
              , Streams.Properties.CDCSourceSpec.tests
              , Streams.Properties.KeyGroupAssignorSpec.tests
              , Streams.Properties.TieredStoreSpec.tests
              , Streams.Properties.RemoteKVStoreSpec.tests
              , Streams.Properties.StoreRefSpec.tests
              , Streams.Properties.KeyGroupDispatchSpec.tests
              , Streams.Properties.WatermarkWiringSpec.tests
              , Streams.Properties.SnapshotSpec.tests
              , Streams.Properties.OperatorWatermarkSpec.tests
              , Streams.Properties.RebalanceBridgeSpec.tests
              ]
        ]
