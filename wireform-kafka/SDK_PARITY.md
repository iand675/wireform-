# SDK parity — Apache Kafka 4.0 ↔ `wireform-kafka` / `wireform-kafka-streams`

This document is a class-by-class, method-by-method audit of the
public surface of Apache Kafka 4.0's Java SDK against the
Haskell `wireform-kafka` + `wireform-kafka-streams` libraries.
Use it as a lookup table: find the Java symbol, then check the
Haskell equivalent or the gap note. Historical audit sections are
kept because they explain why some rows are more detailed than
others, but the current status is what matters.

Convention:

- ✅ = direct equivalent exists.
- ⚠️ = partial equivalent (different shape / smaller surface).
- ❌ = no equivalent (yet); listed as a gap.

The audit is against the **Java 4.0.2 Javadoc** (`kafka.apache.org/40/javadoc`).

Companion files:

- [`CONFIG_PARITY.md`](CONFIG_PARITY.md) — `librdkafka` config-knob parity.
- [`streams/README.md`](streams/README.md) — operator-level parity for the streams DSL.

## How to read this file

- The package tables near the top are the current lookup surface.
- The drill-down sections below are intentionally more detailed:
  they walk overloads, options/result families, and subpackages that
  are easy to overstate in a headline parity table.
- The gap lists are allowed to be blunt. A ❌ row is better than a
  vague ✅ when a user is checking whether a Java call has a Haskell
  home.

---

## `org.apache.kafka.clients.producer`

| Java | Status | Haskell |
| ---- | ------ | ------- |
| `KafkaProducer` | ✅ | `Kafka.Client.Producer.Producer` + `createProducer` / `withProducer` |
| `MockProducer` | ✅ | `Kafka.Client.Mock.Producer.MockProducer` + streams `Kafka.Streams.Mock.Producer` |
| `Producer` (interface) | ✅ | The concrete `Producer` value is the only API — there is no separate interface, by design (the Haskell handle is already opaque so no abstraction-leak risk). |
| `ProducerConfig` | ✅ | `Kafka.Client.Producer.ProducerConfig` (every relevant knob in `CONFIG_PARITY.md`) |
| `ProducerRecord` | ✅ | `Kafka.Client.Producer.ProducerRecord` |
| `RecordMetadata` | ✅ | `Kafka.Client.Producer.RecordMetadata` |
| `Callback` | ✅ | `Kafka.Client.Producer.EnhancedCallback` + `producerOnAcknowledgement` field (richer than Java's single-method `Callback`) |
| `Partitioner` | ✅ | `Kafka.Client.Producer.Partitioner` |
| `ProducerInterceptor` | ✅ | `producerInterceptor` field on `ProducerConfig` |
| `BufferExhaustedException` | ⚠️ | Folded into `Kafka.Errors.KafkaErrorKind` (`RecordTooLargeError` / `TimeoutError`); no dedicated constructor. |
| `RoundRobinPartitioner` | ✅ | `roundRobinPartitioner` |
| `KafkaProducer.send` | ✅ | `sendMessage` / `sendMessageAsync` / `sendBatch` (+ a `_` family of low-allocation variants) |
| `flush` | ✅ | `flushProducer` |
| `initTransactions` | ✅ | `Kafka.Client.Transaction.initTransactions` |
| `beginTransaction` | ✅ | `beginTransaction` |
| `commitTransaction` | ✅ | `commitTransaction` |
| `abortTransaction` | ✅ | `abortTransaction` |
| `sendOffsetsToTransaction` | ✅ | `commitOffsetsInTransaction` |
| `partitionsFor` | ⚠️ | Use `Kafka.Client.AdminClient.describeTopics` |
| `metrics` | ✅ | `Kafka.Telemetry.Metrics` registry |
| `clientInstanceId` | ❌ | Telemetry instance-id getter |
| `registerMetricForSubscription` / `unregisterMetricFromSubscription` | ❌ | KIP-714 application-metric registration |

---

## `org.apache.kafka.clients.consumer`

| Java | Status | Haskell |
| ---- | ------ | ------- |
| `KafkaConsumer` | ✅ | `Kafka.Client.Consumer.Consumer` |
| `MockConsumer` | ✅ | `Kafka.Client.Mock.Consumer.MockConsumer` + streams `Kafka.Streams.Mock.Consumer` |
| `Consumer` (interface) | ✅ | Concrete `Consumer` only, same shape rationale as Producer |
| `ConsumerConfig` | ✅ | `ConsumerConfig` |
| `ConsumerRecord` | ✅ | `ConsumerRecord` |
| `ConsumerRecords` | ✅ | `Kafka.Client.Consumer.ConsumerRecords` (added in this audit) — `recordsByTopic` / `recordsByPartition` / `nextOffsets` |
| `ConsumerGroupMetadata` | ✅ | `Kafka.Client.Consumer.ConsumerGroupMetadata` (added in this audit) + `Kafka.Client.Consumer.groupMetadata` getter |
| `OffsetAndMetadata` | ✅ | `Kafka.Client.Consumer.OffsetAndMetadata` (added in this audit) |
| `OffsetAndTimestamp` | ✅ | `Kafka.Client.Consumer.OffsetAndTimestamp` |
| `OffsetCommitCallback` | ✅ | `Kafka.Client.Consumer.OffsetCommitCallback` (added) — a `Map TopicPartition OffsetAndMetadata -> Maybe SomeException -> IO ()` callback hook |
| `OffsetResetStrategy` | ✅ | `OffsetResetStrategy` |
| `ConsumerInterceptor` | ✅ | `consumerInterceptor` |
| `ConsumerRebalanceListener` | ✅ | `Kafka.Client.RebalanceListener.RebalanceListener` |
| `ConsumerPartitionAssignor` (interface) | ⚠️ | Strategy enum `AssignmentStrategy` (`Range` / `RoundRobin` / `Sticky`); pluggable assignor not user-extensible at the high level (the runtime uses `Kafka.Streams.Runtime.Assignor` internally). |
| `RangeAssignor` / `RoundRobinAssignor` / `StickyAssignor` | ✅ | `AssignmentStrategy` constructors |
| `CooperativeStickyAssignor` | ⚠️ | The cooperative protocol is the default rebalance path under `Kafka.Streams.Runtime.RevocationGrace`; no dedicated assignor class. |
| `AcknowledgementCommitCallback` | ✅ | `Kafka.Client.ShareConsumer.acknowledgeShareRecord` family already takes the callback closure inline; the JVM-style record is in `ShareConsumer` |
| `CommitFailedException` | ⚠️ | `Kafka.Errors.KafkaErrorKind` covers it; not a distinct constructor yet |
| `InvalidOffsetException` / `NoOffsetForPartitionException` / `OffsetOutOfRangeException` | ⚠️ | `Kafka.Errors.OffsetOutOfRangeError` covers OffsetOutOfRangeException; the other two fold into `KafkaException` with a string kind. |
| `RetriableCommitFailedException` / `LogTruncationException` | ⚠️ | Folded into `KafkaException` |
| `KafkaConsumer.subscribe(Collection)` | ✅ | `subscribe` |
| `subscribe(Pattern)` / `SubscriptionPattern` | ✅ | `Kafka.Client.Consumer.subscribeRegex` (added) + `SubscriptionPattern` |
| `assign` | ✅ | `assign` |
| `unsubscribe` | ✅ | `unsubscribe` |
| `poll(Duration)` | ✅ | `poll` |
| `commitSync` / `commitAsync` | ✅ | `commitSync` / `commitAsync` |
| `seek` / `seekToBeginning` / `seekToEnd` | ✅ | same names |
| `position` / `committed` / `committedAll` | ✅ | same names |
| `beginningOffsets` / `endOffsets` / `offsetsForTimes` | ✅ | same names |
| `pause` / `resume` / `paused` / `assignment` / `subscription` | ✅ | same names |
| `wakeup` | ❌ | Cancellation via async + STM/`MVar` patterns in user code; no dedicated `wakeup` |
| `enforceRebalance` | ✅ | `requestRejoin` |
| `groupMetadata` | ✅ | `Kafka.Client.Consumer.groupMetadata` (added) |
| `currentLag` | ❌ | Use streams `Kafka.Streams.Runtime.LagInfo` |
| `metrics` | ✅ | `Kafka.Telemetry.Metrics` registry |
| `clientInstanceId` | ❌ | (same as Producer) |
| `KafkaShareConsumer` / `MockShareConsumer` | ✅ | `Kafka.Client.ShareConsumer.ShareConsumer` + `MockShareConsumer` (added) |

---

## `org.apache.kafka.clients.admin`

The Java Admin interface has ~70 methods. The Haskell `Kafka.Client.AdminClient` covers the *operationally critical* subset; the rest are either trivial extensions of the protocol layer (every Admin RPC has a generated `Kafka.Protocol.Generated.*Request` / `*Response` pair) or are documented gaps.

### Core covered

| Java | Haskell |
| ---- | ------- |
| `Admin.create(...)` / `AdminClient.create(...)` | `Kafka.Client.AdminClient.createAdminClient` |
| `close()` / `close(Duration)` | `closeAdminClient` |
| `createTopics(Collection)` / `(Collection, Options)` | `createTopics` / `ensureTopic` |
| `deleteTopics(Collection)` | `deleteTopics` |
| `listTopics()` / `(Options)` | `listTopics` / `listTopicsExcludeInternal` |
| `describeTopics(Collection)` / `(TopicCollection, ...)` | `describeTopics` |
| `describeCluster()` | ⚠️ folded into `Kafka.Client.Metadata.describeCluster` (lower-level) |
| `describeConfigs(Collection)` | `describeConfigs` |
| `alterConfigs(Map)` / `incrementalAlterConfigs(Map)` | `alterConfigs` / `incrementalAlterConfigs` |
| `listConsumerGroups()` | `listConsumerGroups` |
| `describeConsumerGroups(Collection)` | `describeConsumerGroups` |
| `deleteConsumerGroups(Collection)` | `deleteConsumerGroups` |
| `listConsumerGroupOffsets(...)` | `listConsumerGroupOffsets` |
| `alterConsumerGroupOffsets(...)` | `alterConsumerGroupOffsets` |
| `deleteRecords(Map)` | `deleteRecords` |
| `electLeaders(ElectionType, Set)` | `electLeaders` |
| Per-call timeouts / routing | `Kafka.Client.AdminTimeouts` (pure planner; not yet attached to each call site as a JVM `*Options` record) |

### Gaps (kept honest)

`createPartitions`, `createAcls` / `describeAcls` / `deleteAcls`, `createDelegationToken` / `renewDelegationToken` / `expireDelegationToken` / `describeDelegationToken`, `describeLogDirs` / `alterReplicaLogDirs` / `describeReplicaLogDirs`, `alterPartitionReassignments` / `listPartitionReassignments`, `describeClientQuotas` / `alterClientQuotas`, `describeUserScramCredentials` / `alterUserScramCredentials`, `addRaftVoter` / `removeRaftVoter` / `describeMetadataQuorum`, `unregisterBroker`, `describeFeatures` / `updateFeatures`, `describeProducers` / `fenceProducers` / `abortTransaction` (admin variant) / `describeTransactions` / `listTransactions`, `describeClassicGroups` / `describeShareGroups`, `removeMembersFromConsumerGroup`, `listClientMetricsResources`, `listGroups()` (generic) — these are all available at the **protocol** layer (`Kafka.Protocol.Generated.*`) but **not** yet wrapped in `Kafka.Client.AdminClient` with a typed `*Result`-style record. The `kafka-codegen` exe emits each request/response pair; the audit-driven follow-up is to thread them through a typed admin façade.

Every Java `*Options` and `*Result` record is, by extension, **MISSING** as an in-tree Haskell type (only the operations we wrap above have the analogous Haskell record). When a missing operation is added, the `*Options` / `*Result` types follow naturally.

---

## `org.apache.kafka.common`

| Java | Status | Haskell |
| ---- | ------ | ------- |
| `TopicPartition` | ✅ | `Kafka.Client.Consumer.TopicPartition` (and the distinct streams flavour `Kafka.Streams.Types.TopicPartition`) |
| `TopicPartitionInfo` | ✅ | `Kafka.Client.AdminClient.PartitionInfo` |
| `TopicPartitionReplica` | ❌ | Used by `describeReplicaLogDirs` (which is also a gap). |
| `Node` | ⚠️ | `Kafka.Network.Bootstrap.BootstrapBroker` carries the same info at the lower level |
| `Cluster` | ❌ | Per-call metadata is exposed through admin describe; the cluster snapshot type is internal |
| `ClusterResource` / `ClusterResourceListener` | ❌ | No pluggable listener for cluster-resource-id changes (rarely used in practice) |
| `MetricName` / `KafkaMetric` | ⚠️ | `Kafka.Telemetry.Metrics` exposes a `MetricsRegistry` with metric-name strings; not the full Java metric object model |
| `Metric` | ⚠️ | (same) |
| `ConsumerGroupState` | ⚠️ | `Kafka.Client.AdminClient.ConsumerGroupDescription.cgdState` carries a `Text`; no enum |
| `GroupType` / `GroupState` | ❌ | KIP-848 generic group-type enums |
| `MemberAssignment` | ⚠️ | `Kafka.Client.Mock.Cluster.RebalanceDelta` captures the same shape internally |
| `ElectionType` | ✅ | `Kafka.Client.AdminClient.ElectionType` |
| `Endpoint` | ❌ | Used by `RaftVoterEndpoint` (also a gap) |
| `KafkaFuture` | ✅ | `Kafka.Client.Future` (added in this audit) — thin `Either`-shaped future newtype matching the Java callback surface |
| `Uuid` | ✅ | `Kafka.Client.TopicId.TopicId` (UUID-shaped) |
| `TopicIdPartition` | ⚠️ | Re-derivable from `TopicPartition + TopicId`; no dedicated alias |
| `TopicCollection` | ⚠️ | List-of-`Text` everywhere; no envelope type |
| `Reconfigurable` / `Configurable` | ❌ | Java pluggable-config interface; not idiomatic in Haskell |

---

## `org.apache.kafka.common.config`

| Java | Status | Haskell |
| ---- | ------ | ------- |
| `AbstractConfig` / `ConfigDef` / `ConfigException` | ⚠️ | `Kafka.Client.ConfigValidation.ConfigError` + per-config `validate*Config` |
| `ConfigResource` | ✅ | `Kafka.Client.AdminClient.ConfigResource` |
| `TopicConfig` | ❌ | Available as broker-side `Topic.*` constants in the JVM; on the Haskell side use the `Text` keys directly with `incrementalAlterConfigs` |
| `SecurityConfig` | ❌ | (same) |

---

## `org.apache.kafka.common.errors`

The Java SDK exposes ~50 named exception classes; the Haskell side
folds them into a single `Kafka.Errors.KafkaException` carrying a
sum `KafkaErrorKind`. The kinds the Haskell side names explicitly:

`ConnectError`, `AuthenticationError`, `AuthorizationError`,
`ConfigurationError`, `TimeoutError`, `NetworkError`,
`InvalidTopicError`, `TopicAlreadyExistsError`,
`UnknownTopicOrPartitionError`, `RecordTooLargeError`,
`SerializationError`, `ProducerFencedError`,
`TransactionAbortedError`, `OffsetOutOfRangeError`,
`NotInTransactionError`, `UnsupportedVersionError`,
`DeliveryFailedError`, `UnknownError`.

Java exceptions not explicitly named:
`OutOfOrderSequenceException`, `LeaderNotAvailableException`,
`NotLeaderOrFollowerException`, `NotControllerException`,
`PolicyViolationException`, `SecurityDisabledException`,
`ThrottlingQuotaExceededException`, `UnsupportedSaslMechanismException`,
`StaleBrokerEpochException`, `KafkaStorageException`,
`SaslAuthenticationException`, `SslAuthenticationException`,
`CorruptRecordException`, `BrokerNotAvailableException`,
`DisconnectException`, `WakeupException`, `InterruptException`,
`InvalidRequestException`, `InvalidProducerEpochException`, …
— all reachable from the same `KafkaException` value via the
underlying broker error code that's exposed in
`KafkaErrorKind`'s `Text` cause, but **MISSING** as discriminated
constructors.

---

## `org.apache.kafka.common.header`

| Java | Status | Haskell |
| ---- | ------ | ------- |
| `Header` / `Headers` | ✅ | `Kafka.Headers.Headers` (full lookup / append / replace / delete API) |
| `Headers.Internals` | ❌ | Internal-only Java type, no useful Haskell analogue |

---

## `org.apache.kafka.common.serialization`

| Java | Status | Haskell |
| ---- | ------ | ------- |
| `Serde<T>` | ✅ | `Kafka.Serde.Serde` (also `Kafka.Streams.Serde`) |
| `Serializer<T>` / `Deserializer<T>` | ⚠️ | Combined into `Serde { serialize, deserialize }` |
| `Serdes` factory | ✅ | Built-ins listed in `Kafka.Serde` |
| `StringSerializer/Deserializer` | ✅ | `textSerde` / `utf8Serde` |
| `Integer*` / `Long*` / `Short*` / `Float*` / `Double*` | ✅ | `int16Serde` / `int32Serde` / `int64Serde` / `word*Serde` / `floatSerde` / `doubleSerde` |
| `ByteArraySerializer` / `Deserializer` | ✅ | `byteArraySerde` |
| `ByteBufferSerializer` / `Deserializer` | ❌ | `Data.ByteString` is the analogue; no dedicated wrapper |
| `BytesSerializer` / `Deserializer` | ⚠️ | `byteStringSerde` |
| `UUIDSerializer` / `Deserializer` | ✅ | `uuidSerde` |
| `VoidSerializer` / `Deserializer` | ✅ | `voidSerde` |
| `ListSerializer` / `Deserializer` | ❌ | Combine `prefixedSerde` + `lengthPrefixedSerde` manually |

---

## `org.apache.kafka.streams` (top-level)

| Java | Status | Haskell |
| ---- | ------ | ------- |
| `KafkaStreams` | ✅ | `Kafka.Streams.Runtime.KafkaStreams` |
| `KafkaStreams.State` | ✅ | `Kafka.Streams.Runtime.StreamsStatus` |
| `KafkaStreams.StateListener` | ✅ | `Kafka.Streams.Runtime.StateListener` + `setStateListener` |
| `KafkaStreams.CloseOptions` | ✅ | `Kafka.Streams.Runtime.CloseOptions` |
| `start` / `close` / `cleanUp` | ✅ | `startKafkaStreams` / `closeKafkaStreams` / `cleanUp` |
| `pause` / `resume` / `isPaused` | ✅ | `pauseKafkaStreams` / `resumeKafkaStreams` / `isPausedKafkaStreams` |
| `state` / `setStateListener` / `setUncaughtExceptionHandler` | ✅ | same names |
| `setGlobalStateRestoreListener` / `setStandbyUpdateListener` | ✅ | `setGlobalRestoreListener` / `setStandbyListener` |
| `addStreamThread` / `removeStreamThread` | ✅ | same names |
| `metadataForAllStreamsClients` / `streamsMetadataForStore` | ✅ | same names |
| `queryMetadataForKey` | ✅ | `Kafka.Streams.Discovery.makeKeyQueryMetadata` (lower-level) |
| `store(StoreQueryParameters)` | ✅ | `queryEngineStore` + `Kafka.Streams.InteractiveQueries` |
| `metrics` | ✅ | `Kafka.Streams.Metrics` |
| `allLocalStorePartitionLags` | ✅ | `publishLag` + `LagInfo` |
| `clientInstanceIds` | ❌ | KIP-714 telemetry id getter |
| `metadataForLocalThreads` | ✅ | `metadataForLocalThreads` |
| `KafkaClientSupplier` | ⚠️ | Replaced by `startKafkaStreamsWith :: KafkaStreams -> StreamDriver -> IO ()` driver-injection seam in `Kafka.Streams.Runtime.NativeDriver` |
| `ClientInstanceIds` | ❌ |  |
| `KeyValue` | ⚠️ | Use `Kafka.Streams.Types.Record` (carries timestamp + headers in addition to key/value) |
| `KeyQueryMetadata` | ✅ | `Kafka.Streams.Discovery.KeyQueryMetadata` |
| `LagInfo` | ✅ | `Kafka.Streams.Runtime.LagInfo` |
| `StoreQueryParameters` | ✅ | `Kafka.Streams.InteractiveQueries.StoreQueryParameters` |
| `StreamsBuilder` | ✅ | `Kafka.Streams.StreamsBuilder.StreamsBuilder` |
| `StreamsConfig` | ✅ | `Kafka.Streams.Config.StreamsConfig` |
| `StreamsMetadata` | ✅ | `Kafka.Streams.Discovery.StreamsMetadata` |
| `StreamsMetrics` | ⚠️ | `Kafka.Streams.Metrics.MetricsRegistry` (compatible shape, not Java's exact interface) |
| `TaskMetadata` / `ThreadMetadata` | ⚠️ | `Kafka.Streams.Runtime.LocalThreadMetadata` (only the thread-local shape) |
| `TestInputTopic` / `TestOutputTopic` | ✅ | exposed by `Kafka.Streams.Driver` |
| `TopologyTestDriver` | ✅ | `Kafka.Streams.Driver.TopologyTestDriver` |
| `Topology` / `TopologyDescription` / `TopologyConfig` | ✅ | matching names (TopologyConfig is split across `StreamsConfig` + `Topology.Optimization`) |
| `AutoOffsetReset` | ✅ | `Kafka.Streams.Consumed.AutoOffsetReset` |

---

## `org.apache.kafka.streams.kstream`

| Java | Status | Haskell |
| ---- | ------ | ------- |
| `KStream` | ✅ | `Kafka.Streams.KStream.KStream` |
| `KStream.filter` / `filterNot` | ✅ | `filterStream` / `filterNotStream` (+ `*Named`) |
| `KStream.map` | ✅ | `mapKeyValue` (+ `mapKeyValueM` / `*Named`) |
| `KStream.mapValues` | ✅ | `mapValues` (+ `mapValuesM` / `*Named`) |
| `KStream.flatMap` / `flatMapValues` | ✅ | `flatMapKeyValue` / `flatMapValues` |
| `KStream.foreach` | ✅ | `foreachStream` (+ async variant) |
| `KStream.peek` | ✅ | `peekStream` (+ `*Named`) |
| `KStream.selectKey` | ✅ | `selectKey` (+ `*Named`) |
| `KStream.merge` | ✅ | `mergeStreams` / `mergeStreamsN` |
| `KStream.split` / `Branched` / `BranchedKStream` | ✅ | `splitStream` + `Branched` / `branchedFrom` / `withFunction` / `withConsumer` |
| `KStream.branch` (legacy) | ✅ | `branchStream` |
| `KStream.groupBy` / `groupByKey` | ✅ | `groupByStream` / `groupByKey` |
| `KStream.join` / `leftJoin` / `outerJoin` (windowed, stream-stream) | ✅ | `joinKStreamKStream` / `leftJoinKStreamKStream` / `outerJoinKStreamKStream` |
| `KStream.join` / `leftJoin` (KTable) | ✅ | `joinKStreamKTable` / `leftJoinKStreamKTable` |
| `KStream.join` / `leftJoin` (GlobalKTable) | ✅ | `joinKStreamGlobalKTable` / `leftJoinKStreamGlobalKTable` |
| `KStream.through` | ✅ | `throughTopic` |
| `KStream.to(topic)` / `to(TopicNameExtractor)` | ✅ | `toTopic` / `toExtracted` |
| `KStream.repartition` / `repartition(Repartitioned)` | ✅ | `repartition` / `repartitionWith` |
| `KStream.toTable` | ✅ | `toTable` |
| `KStream.print(Printed)` | ✅ | `printStream` / `Kafka.Streams.Printed.printKStream` |
| `KStream.process(ProcessorSupplier)` | ✅ | `processStream` |
| `KStream.processValues(FixedKeyProcessorSupplier)` | ✅ | `processValuesStream` |
| `KStream.values` | ✅ | `valuesStream` |
| `KTable` | ✅ | `Kafka.Streams.KTable.KTable` |
| `KTable.filter` / `filterNot` | ✅ | `filterTable` / `filterNotTable` |
| `KTable.mapValues` | ✅ | `mapValuesTable` |
| `KTable.join` / `leftJoin` / `outerJoin` (KTable-KTable) | ✅ | `joinKTableKTable` etc. |
| `KTable.join` / `leftJoin` (foreign-key) | ✅ | `foreignKeyJoinKTable` / `leftForeignKeyJoinKTable` |
| `KTable.groupBy` | ✅ | `groupTableBy` |
| `KTable.suppress` | ✅ | `suppressKStream` / `suppressWindowed` (+ time-limit variant) |
| `KTable.toStream` / `toStream(KeyValueMapper)` | ✅ | `toKStreamFromKTable` |
| `KTable.queryableStoreName` | ✅ | `ktableStore` |
| `KGroupedStream` / `KGroupedTable` | ✅ | `KGroupedStream` / `KGroupedTable` |
| `KGroupedStream.count` / `reduce` / `aggregate` / `windowedBy` | ✅ | `countStream` / `reduceStream` / `aggregateStream` / `windowedByTime` / `windowedBySession` |
| `KGroupedTable.count` / `reduce` / `aggregate` | ✅ | `countKGroupedTable` / `reduceKGroupedTable` / `aggregateKGroupedTable` |
| `Cogrouped*` family | ✅ | `Kafka.Streams.Cogroup` |
| `TimeWindowedKStream` / `SessionWindowedKStream` | ✅ | `Kafka.Streams.TimeWindowedKStream` / `SessionWindowedKStream` |
| `GlobalKTable` | ✅ | `Kafka.Streams.GlobalKTable` |
| `Aggregator` / `Initializer` / `Reducer` / `Merger` | ✅ | Higher-order parameters on the aggregation operators |
| `Branched` / `BranchedKStream` | ✅ | `Branched` |
| `Consumed` / `Produced` / `Joined` / `Grouped` / `Materialized` / `Named` / `Repartitioned` / `StreamJoined` / `TableJoined` | ✅ | matching modules (`Kafka.Streams.{Consumed,Produced,Joined,Grouped,Materialized,Named,Repartitioned}`) |
| `JoinWindows` / `SessionWindows` / `SlidingWindows` / `TimeWindows` / `Windows` | ✅ | `JoinWindows` / `SessionWindows` / `slidingWindows` / `tumblingWindows` / `hoppingWindows` (in `Kafka.Streams.Window`) |
| `UnlimitedWindows` | ❌ | Use grace = `forever` |
| `EmitStrategy` | ✅ | `Kafka.Streams.TimeWindowedKStream.EmitStrategy` (`emitOnWindowUpdate` / `emitOnWindowClose`) |
| `Suppressed` / `Suppressed.BufferConfig` / `Suppressed.EagerBufferConfig` / `Suppressed.StrictBufferConfig` | ✅ | `Kafka.Streams.Suppress.{Suppressed,BufferConfig,untilTimeLimit,untilWindowCloses,unboundedBufferConfig,maxBytesBufferConfig,maxRecordsBufferConfig,shutDownWhenFull,emitEarlyWhenFull}` |
| `Printed` | ✅ | `Kafka.Streams.Printed` |
| `Windowed` | ✅ | `Kafka.Streams.Serde.Windowed.WindowedKey` |
| `ValueJoiner` / `ValueJoinerWithKey` | ✅ | Higher-order parameters on joins |
| `ValueMapper` / `ValueMapperWithKey` | ✅ | Higher-order parameters |
| `ValueTransformerWithKey` / `*Supplier` (deprecated since 4.0) | ⚠️ | `transformValuesStream` covers the supported path; the deprecated ValueTransformer ADT is intentionally not ported |
| `ForeachAction` | ✅ | Higher-order parameter on `foreachStream` |
| `KeyValueMapper` | ✅ | Higher-order parameter on `selectKey` / `groupBy` |
| `Predicate` | ✅ | Higher-order parameter on `filterStream` / `filterNotStream` |
| `GlobalKTable.queryableStoreName` | ⚠️ | (the GlobalKTable handle carries the store name directly) |

---

## `org.apache.kafka.streams.processor.api`

| Java | Status | Haskell |
| ---- | ------ | ------- |
| `Processor<KIn,VIn,KOut,VOut>` | ✅ | `Kafka.Streams.Processor.Processor` |
| `ProcessorContext<KOut,VOut>` | ✅ | `Kafka.Streams.Processor.ProcessorContext` |
| `ProcessorSupplier<KIn,VIn,KOut,VOut>` | ✅ | `ProcessorSupplier` |
| `ContextualProcessor` | ⚠️ | Build via `Processor { procInit = \ctx -> writeIORef …, … }`; no class hierarchy |
| `FixedKeyProcessor` / `FixedKeyProcessorContext` / `FixedKeyProcessorSupplier` / `FixedKeyRecord` | ✅ | `Kafka.Streams.Processor.{FixedKeyProcessor,FixedKeyRecord}` |
| `ContextualFixedKeyProcessor` | ⚠️ | (same pattern) |
| `Record` (streams variant) | ✅ | `Kafka.Streams.Types.Record` |
| `Record.headers` / `withHeaders` / etc. | ✅ | `Kafka.Streams.Types.Headers` |
| `ProcessorWrapper` (KIP-1112) | ❌ | The wrapper-pattern is not exposed; usual workaround is to wrap the supplier directly |
| `StateStoreContext` | ⚠️ | Folded into `ProcessorContext` |
| `RecordMetadata` (streams) | ✅ | `Kafka.Streams.Types.RecordMetadata` |
| `To` (forward destination spec) | ⚠️ | `forwardTo :: ProcessorContext -> NodeName -> Record -> IO ()` (no separate `To` envelope) |
| `MockProcessorContext` | ✅ | `Kafka.Streams.Processor.MockProcessorContext` (added in this audit) — captures forwarded records and scheduled punctuators |

---

## `org.apache.kafka.streams.state`

| Java | Status | Haskell |
| ---- | ------ | ------- |
| `KeyValueStore` / `ReadOnlyKeyValueStore` | ✅ | `Kafka.Streams.State.Store.KeyValueStore` / `Kafka.Streams.InteractiveQueries.ReadOnlyKeyValueStore` |
| `WindowStore` / `ReadOnlyWindowStore` | ✅ | `WindowStore` / `ReadOnlyWindowStore` |
| `SessionStore` / `ReadOnlySessionStore` | ✅ | `SessionStore` / `ReadOnlySessionStore` |
| `TimestampedKeyValueStore` / `TimestampedWindowStore` | ✅ | `Kafka.Streams.State.KeyValue.Timestamped` / `Window.Timestamped` |
| `VersionedKeyValueStore` / `VersionedRecord` / `VersionedRecordIterator` | ✅ | `Kafka.Streams.State.KeyValue.Versioned` |
| `ValueAndTimestamp` | ✅ | `Kafka.Streams.State.KeyValue.Timestamped.ValueAndTimestamp` |
| `HostInfo` | ✅ | `Kafka.Streams.Discovery.HostInfo` |
| `KeyValueIterator` / `WindowStoreIterator` | ✅ | `Kafka.Streams.State.Store.KeyValueIterator` / `WindowStoreIterator` |
| `KeyValueBytesStoreSupplier` / `WindowBytesStoreSupplier` / `SessionBytesStoreSupplier` | ⚠️ | `StoreBuilderKV` / `StoreBuilderW` / `StoreBuilderS` (typed-key/value, not bytes-only) |
| `QueryableStoreType` | ⚠️ | The typed `Query` GADT replaces this |
| `QueryableStoreTypes` | ⚠️ | (same) |
| `RocksDBConfigSetter` | ⚠️ | `Kafka.Streams.State.KeyValue.RocksDB.RocksDBConfig` (under `+rocksdb`) |
| `StoreBuilder` / `StoreSupplier` | ✅ | `StoreBuilderKV` / `StoreBuilderW` / `StoreBuilderS` |
| `Stores` | ✅ | `Kafka.Streams.Stores` |
| `StateSerdes` | ⚠️ | Implicit via `Materialized.matKeySerde` / `matValueSerde`; no dedicated wrapper |
| `DslStoreSuppliers` / `BuiltInDslStoreSuppliers` / `DslKeyValueParams` / `DslSessionParams` / `DslWindowParams` | ✅ | `Kafka.Streams.DslStoreSuppliers` |

---

## `org.apache.kafka.streams.errors`

| Java | Status | Haskell |
| ---- | ------ | ------- |
| `StreamsException` | ✅ | `Kafka.Streams.Errors.StreamsException` |
| `DefaultProductionExceptionHandler` | ✅ | `Kafka.Streams.Errors.logAndContinueProduction` (and `logAndShutdown*`) |
| `DefaultProcessingExceptionHandler` | ✅ | `logAndContinueProcessing` |
| `StreamsUncaughtExceptionHandler` | ✅ | `Kafka.Streams.Errors.StreamsUncaughtExceptionHandler` |
| `DeserializationExceptionHandler` | ✅ | `Kafka.Streams.Errors.DeserializationHandler` |
| `ProductionExceptionHandler` | ✅ | `Kafka.Streams.Errors.ProductionHandler` |
| `ProcessingExceptionHandler` | ✅ | `Kafka.Streams.Errors.ProcessingExceptionHandler` |
| `TaskMigratedException` | ✅ | `Kafka.Streams.Errors.TaskMigratedException` |
| `TopologyException` | ✅ | `Kafka.Streams.Errors.TopologyException` |
| `InvalidStateStoreException` | ✅ | `InvalidStateStoreException` |
| `BrokerNotFoundException` / `MissingSourceTopicException` / `TaskAssignmentException` / `TaskCorruptedException` / `UnknownStateStoreException` / `LockException` | ❌ | Folded into a generic `StreamsException` with a textual reason |

---

## `org.apache.kafka.streams.query`

| Java | Status | Haskell |
| ---- | ------ | ------- |
| `Query<T>` | ✅ | `Kafka.Streams.Query.Query` GADT |
| `QueryResult<T>` | ✅ | `QueryResult` |
| `QueryConfig` | ❌ | No `executionInfo` flag; `execute` is unconditional |
| `KeyQuery` / `RangeQuery` / `AllQuery` | ✅ | `Query` constructors |
| `WindowKeyQuery` / `WindowRangeQuery` | ✅ | `executeWindowKeyQuery` / `executeWindowRangeQuery` |
| `VersionedKeyQuery` / `MultiVersionedKeyQuery` | ✅ | `executeVersionedKeyQuery` / `executeMultiVersionedKeyQuery` |
| `TimestampedKeyQuery` / `TimestampedRangeQuery` | ⚠️ | Folded into `KeyQuery` / `RangeQuery` against a timestamped store |
| `Position` / `PositionBound` | ✅ | `Position` / `PositionBound` |
| `StateQueryRequest` / `StateQueryResult` | ✅ | matching names |

---

## Summary of named gaps as of this audit

The audit surfaced several named gaps that are now closed:

- **`ConsumerRecords`** wrapper type → added as `Kafka.Client.Consumer.ConsumerRecords`.
- **`ConsumerGroupMetadata`** + `groupMetadata()` → added.
- **`OffsetAndMetadata`** → added.
- **`OffsetCommitCallback`** → added.
- **`SubscriptionPattern`** + regex subscribe → added (`Kafka.Client.Consumer.SubscriptionPattern` + `subscribeRegex`).
- **`KafkaFuture`** → added as `Kafka.Client.Future` (thin `Either`-shaped future newtype).
- **`MockProcessorContext`** for testing user processors → added as `Kafka.Streams.Processor.MockProcessorContext`.
- **`MockShareConsumer`** → added.

What's left in the gap list (in order of likely user impact):

1. The big Java `Admin.*` operations that aren't yet wrapped above the protocol layer — `createAcls` / `describeAcls` / `deleteAcls`, `createPartitions`, `alterPartitionReassignments` / `listPartitionReassignments`, `describeLogDirs` / `alterReplicaLogDirs`, `describeClientQuotas` / `alterClientQuotas`, `*DelegationToken*`, `*UserScramCredentials`, `addRaftVoter` / `removeRaftVoter` / `describeMetadataQuorum`, `unregisterBroker`, `describeFeatures` / `updateFeatures`, `describeProducers` / `fenceProducers` / `*Transactions`, `describeClassicGroups` / `describeShareGroups`, `removeMembersFromConsumerGroup`, `listClientMetricsResources`, `listGroups()` (generic).
2. KIP-714 `clientInstanceId` getters on Producer / Consumer / KafkaStreams.
3. Discriminated constructors for every Java `*Exception` in `org.apache.kafka.common.errors` / `org.apache.kafka.streams.errors`. (Today they all fold into one `KafkaException` value with a textual reason.)
4. `ConsumerPartitionAssignor` as a pluggable user-supplied interface (currently the enum + the streams runtime's internal `Kafka.Streams.Runtime.Assignor` cover the in-tree cases).
5. `Cluster` / `ClusterResource` / `Node` / `TopicCollection` / `Endpoint` / `Reconfigurable` / `Configurable` as public types.
6. `ByteBufferSerializer` / `Deserializer`, `ListSerializer` / `Deserializer` as named built-ins.
7. `UnlimitedWindows`.
8. `QueryConfig` (executionInfo flag).

These remain tracked in this file. They are
not blocking for the streams runtime + DSL parity (which is at
100% for the kstream operator surface) or for the producer /
consumer / transaction hot paths (which are at full parity).

---

## Detailed drill-down on classes the headline table compresses

The headline tables above are intentionally compact. This section
walks the overloads and subpackages where a single ✅ would hide too
much detail.

### `Producer` *interface* (separate from `KafkaProducer`)

| Java method | Status | Haskell |
| ----------- | ------ | ------- |
| `initTransactions()` | ✅ | `Kafka.Client.Transaction.initTransactions` |
| `beginTransaction()` | ✅ | `beginTransaction` |
| `sendOffsetsToTransaction(...)` | ✅ | `commitOffsetsInTransaction` |
| `commitTransaction()` / `abortTransaction()` | ✅ | `commitTransaction` / `abortTransaction` |
| `registerMetricForSubscription(KafkaMetric)` / `unregisterMetricFromSubscription(KafkaMetric)` | ❌ | KIP-714 metric subscription |
| `send(ProducerRecord)` / `send(ProducerRecord, Callback)` | ✅ | `sendMessage` / `sendMessageAsync` |
| `flush()` | ✅ | `flushProducer` |
| `partitionsFor(topic)` | ❌ | Use `Kafka.Client.AdminClient.describeTopics` |
| `metrics()` (returning `Map<MetricName, ? extends Metric>`) | ⚠️ | `Kafka.Telemetry.Metrics` returns a flat-key registry, not the typed `MetricName` map |
| `clientInstanceId(Duration)` | ❌ | KIP-714 telemetry id getter |
| `close()` / `close(Duration)` | ✅ | `closeProducer` / `closeProducerWithTimeout` |

### `Consumer` *interface* (separate from `KafkaConsumer`)

Every overload is listed here because the Java method family is wider than the compact table suggests:

| Java method | Status | Haskell |
| ----------- | ------ | ------- |
| `assignment()` / `subscription()` | ✅ | `assignment` / *no `subscription` getter — use the ConsumerConfig* (⚠️) |
| `subscribe(Collection<String>)` | ✅ | `subscribe` |
| `subscribe(Collection<String>, ConsumerRebalanceListener)` | ⚠️ | Compose `subscribe` + `setRebalanceListener` — no single-shot overload |
| `subscribe(Pattern)` / `subscribe(Pattern, ConsumerRebalanceListener)` | ❌ | `java.util.regex.Pattern` overload not exposed; the Haskell side has the `SubscriptionPattern` regex type but no `subscribe(pattern)` overload yet |
| `subscribe(SubscriptionPattern)` / `subscribe(SubscriptionPattern, ConsumerRebalanceListener)` | ❌ | (same) |
| `assign(Collection<TopicPartition>)` | ✅ | `assign` |
| `unsubscribe()` | ✅ | `unsubscribe` |
| `poll(Duration)` | ✅ | `poll` |
| `commitSync()` | ✅ | `commitSync` |
| `commitSync(Duration)` | ❌ | timeout overload |
| `commitSync(Map<TopicPartition, OffsetAndMetadata>)` | ❌ | typed-offsets overload |
| `commitSync(Map<TopicPartition, OffsetAndMetadata>, Duration)` | ❌ | both above |
| `commitAsync()` | ✅ | `commitAsync` |
| `commitAsync(OffsetCommitCallback)` | ⚠️ | The `OffsetCommitCallback` type exists in `Kafka.Client.Consumer`; the overload that *takes* it isn't wired to `commitAsync` yet |
| `commitAsync(Map<TopicPartition, OffsetAndMetadata>, OffsetCommitCallback)` | ⚠️ | (same) |
| `seek(TopicPartition, long)` / `seek(TopicPartition, OffsetAndMetadata)` | ⚠️ | first overload ✅; second (with metadata) ❌ |
| `seekToBeginning` / `seekToEnd` | ✅ | same names |
| `position(TopicPartition)` | ✅ | `position` |
| `position(TopicPartition, Duration)` | ❌ | timeout overload |
| `committed(Set<TopicPartition>)` | ✅ | `committed` / `committedAll` |
| `committed(Set<TopicPartition>, Duration)` | ❌ | timeout overload |
| `clientInstanceId(Duration)` | ❌ | KIP-714 telemetry id getter |
| `metrics()` | ⚠️ | (same as Producer) |
| `partitionsFor(topic)` / `partitionsFor(topic, Duration)` | ❌ | use `AdminClient.describeTopics` |
| `listTopics()` / `listTopics(Duration)` | ❌ | use `AdminClient.listTopics` |
| `paused()` | ✅ | `paused` |
| `pause(Collection)` / `resume(Collection)` | ✅ | `pause` / `resume` |
| `offsetsForTimes(Map)` / `offsetsForTimes(Map, Duration)` | ⚠️ | `offsetsForTimes` ✅ ; timeout overload ❌ |
| `beginningOffsets(Collection)` / `beginningOffsets(Collection, Duration)` | ⚠️ | first ✅, timeout ❌ |
| `endOffsets(Collection)` / `endOffsets(Collection, Duration)` | ⚠️ | first ✅, timeout ❌ |
| `currentLag(TopicPartition)` | ❌ | KIP-666 lag getter (streams has `Kafka.Streams.Runtime.LagInfo`) |
| `groupMetadata()` | ✅ | `Kafka.Client.Consumer.groupMetadata` |
| `enforceRebalance()` / `enforceRebalance(String reason)` | ⚠️ | `requestRejoin` is the first; the `reason` overload isn't carried |
| `close()` / `close(Duration)` | ✅ | `closeConsumer` / `closeConsumerWithTimeout` |
| `wakeup()` | ❌ | Use async + STM/MVar cancellation patterns |
| `registerMetricForSubscription` / `unregisterMetricFromSubscription` | ❌ | (same KIP-714 gap) |

### `KafkaConsumer` (concrete; extras beyond the interface)

| Java method | Status | Haskell |
| ----------- | ------ | ------- |
| `groupMetadata()` | ✅ | `groupMetadata` |
| `assignmentLost()` | ❌ | KIP-848 lost-partitions accessor |
| Static `createDeadLetterTopic` shortcuts | n/a | Not in the upstream Java SDK; handled by Streams `Kafka.Streams.Errors` handlers |

### `KStream` overload drill-down

The overload-level detail matters here:

| Java overload | Status | Haskell |
| ------------- | ------ | ------- |
| `filter(Predicate)` / `filter(Predicate, Named)` | ✅ | `filterStream` / `filterStreamNamed` |
| `filterNot(Predicate)` / `filterNot(Predicate, Named)` | ✅ | `filterNotStream` (Named variant ⚠️ — same processor name handling) |
| `map(KeyValueMapper)` / `map(KeyValueMapper, Named)` | ✅ | `mapKeyValue` / `mapKeyValueNamed` |
| `mapValues(ValueMapper)` / `mapValues(ValueMapper, Named)` / `mapValues(ValueMapperWithKey)` / `mapValues(ValueMapperWithKey, Named)` | ⚠️ | Two overloads collapsed: the `WithKey` variant goes through `mapKeyValue` (where the function takes the key); the `Named` variants on `*WithKey` use the same processor-name knob |
| `flatMap(KeyValueMapper)` / `flatMap(..., Named)` | ✅ | `flatMapKeyValue` |
| `flatMapValues(ValueMapper)` / `flatMapValues(ValueMapper, Named)` / `flatMapValues(ValueMapperWithKey)` / `flatMapValues(..., Named)` | ⚠️ | Single Haskell signature; `Named` variants composed from `flatMapValues` + `*Named` |
| `foreach(ForeachAction)` / `foreach(ForeachAction, Named)` | ✅ | `foreachStream` |
| `peek(ForeachAction)` / `peek(ForeachAction, Named)` | ✅ | `peekStream` / `peekStreamNamed` |
| `groupBy(KeyValueMapper)` / `groupBy(KeyValueMapper, Grouped)` | ✅ | `groupByStream` |
| `groupByKey()` / `groupByKey(Grouped)` | ✅ | `groupByKey` |
| `merge(KStream)` / `merge(KStream, Named)` | ✅ | `mergeStreams` |
| `to(String)` / `to(String, Produced)` / `to(TopicNameExtractor)` / `to(TopicNameExtractor, Produced)` | ✅ | `toTopic` / `toExtracted` |
| `repartition()` / `repartition(Repartitioned)` | ✅ | `repartition` / `repartitionWith` |
| `selectKey(KeyValueMapper)` / `selectKey(KeyValueMapper, Named)` | ✅ | `selectKey` / `selectKeyNamed` |
| `split()` / `split(Named)` | ⚠️ | `splitStream` is the unified entry; `Named` is folded into per-branch `Branched` records |
| `print(Printed)` | ✅ | `Kafka.Streams.Printed.printKStream` |
| `process(ProcessorSupplier, String...)` | ✅ | `processStream` |
| `processValues(FixedKeyProcessorSupplier, String...)` | ✅ | `processValuesStream` |
| `join` / `leftJoin` / `outerJoin` (stream-stream, 4 overloads each: default-serde / `StreamJoined` / windowed / windowed+`StreamJoined`) | ⚠️ | Default-serde + `StreamJoined` paths ✅; the four overloads collapse to one Haskell signature (`Joined` covers the optional serdes) |
| `join` / `leftJoin` (KTable, 2 overloads: default / `Joined`) | ✅ | `joinKStreamKTable` / `leftJoinKStreamKTable` (the `Joined` knob is on `Kafka.Streams.Joined`) |
| `join` / `leftJoin` (GlobalKTable, 4 overloads: default-key-mapper / `Named` / `ValueJoinerWithKey` / `Named` + `ValueJoinerWithKey`) | ⚠️ | `joinKStreamGlobalKTable` / `leftJoinKStreamGlobalKTable` — the `WithKey` joiner variant requires composing through `mapKeyValue` |
| `toTable()` / `toTable(Named)` / `toTable(Materialized)` / `toTable(Named, Materialized)` | ⚠️ | `toTable` takes a `Materialized` always; the `Named` variants compose `toTable` + a named pass-through |

### `KTable` overload drill-down

Same shape: every Java method has 2–4 overloads (default / `Materialized` / `Named` / `Named + Materialized`). Haskell collapses these to a single combinator with the optional `Named` going through `Kafka.Streams.Named.namedOr`. The functional reach is identical; the per-overload tally is ⚠️ for everything other than the headline single-arg form.

Notable concrete gaps in `KTable`:

| Java method | Status | Haskell |
| ----------- | ------ | ------- |
| `queryableStoreName()` | ✅ | `ktableStore` (a `StoreName`, not a `String`) |
| `suppress(Suppressed)` | ✅ | `suppressKStream` etc. (returns `KStream`, not `KTable`; documented in `streams/README.md`) |
| `toStream(KeyValueMapper)` (rekeying variant) | ⚠️ | `toKStreamFromKTable` followed by `selectKey`; not a single call |
| `toStream(Named)` | ⚠️ | Same — compose with `Named` |
| `join` / `leftJoin` (foreign-key, with `Named` overload) | ⚠️ | `foreignKeyJoinKTable` covers the default; the `Named` form folds through `TableJoined` |

### `Stores` factory

Java factory methods (audited against the [Stores Javadoc](https://kafka.apache.org/40/javadoc/org/apache/kafka/streams/state/Stores.html)):

| Java | Status | Haskell |
| ---- | ------ | ------- |
| `persistentKeyValueStore(name)` | ✅ | `Kafka.Streams.Stores.persistentKeyValueStore` |
| `persistentTimestampedKeyValueStore(name)` | ⚠️ | use `timestampedKeyValueStore` + persistent backing supplier |
| `persistentVersionedKeyValueStore(name, historyRetention)` | ⚠️ | `versionedKeyValueStore` (in-memory only; no `persistent` variant yet) |
| `persistentVersionedKeyValueStore(name, historyRetention, segmentInterval)` | ❌ | The two-arg overload isn't exposed |
| `inMemoryKeyValueStore(name)` | ✅ | `inMemoryKeyValueStore` |
| `lruMap(name, maxCacheSize)` | ✅ | `lruMap` |
| `persistentWindowStore(name, retentionPeriod, windowSize, retainDuplicates)` | ❌ | The persistent window backend isn't exposed in the public `Stores` re-exports |
| `persistentTimestampedWindowStore(...)` | ❌ | (same) |
| `inMemoryWindowStore(name, retentionPeriod, windowSize, retainDuplicates)` | ⚠️ | `inMemoryWindowStore` exists but takes `(name, size, retention)` — argument order + the `retainDuplicates` knob differ |
| `persistentSessionStore(name, retentionPeriod)` | ❌ | Persistent session backend not exposed |
| `inMemorySessionStore(name, retentionPeriod)` | ✅ | `inMemorySessionStore` |
| `keyValueStoreBuilder(supplier, keySerde, valueSerde)` | ✅ | `StoreBuilderKV` plumbing in `Kafka.Streams.State.Store` |
| `timestampedKeyValueStoreBuilder(...)` | ⚠️ | Same plumbing; no dedicated `timestampedKeyValueStoreBuilder` name |
| `versionedKeyValueStoreBuilder(...)` | ⚠️ | (same) |
| `windowStoreBuilder(...)` | ✅ | `StoreBuilderW` plumbing |
| `timestampedWindowStoreBuilder(...)` | ⚠️ | (same) |
| `sessionStoreBuilder(...)` | ✅ | `StoreBuilderS` plumbing |

### `org.apache.kafka.streams.processor` (the non-`api` package)

| Java | Status | Haskell |
| ---- | ------ | ------- |
| `BatchingStateRestoreCallback` | ❌ | (deprecated in favour of `StateRestoreListener`) |
| `Cancellable` | ✅ | `Kafka.Streams.Processor.Cancellable` |
| `CommitCallback` | ❌ | Store-side commit hook |
| `ConnectedStoreProvider` | ⚠️ | Encoded via the `StoreBuilder` returned from `ProcessorSupplier` |
| `ExtractRecordMetadataTimestamp` | ✅ | `Kafka.Streams.Time.extractRecordMetadataTimestamp` (added in this audit pass) |
| `FailOnInvalidTimestamp` | ✅ | `Kafka.Streams.Time.failOnNoTimestampExtractor` |
| `LogAndSkipOnInvalidTimestamp` | ✅ | `Kafka.Streams.Time.logAndSkipOnNoTimestamp` |
| `UsePartitionTimeOnInvalidTimestamp` | ✅ | `Kafka.Streams.Time.usePartitionTimeOnInvalidTimestamp` (added in this audit pass) |
| `WallclockTimestampExtractor` | ✅ | `Kafka.Streams.Time.wallClockTimestampExtractor` |
| `MockProcessorContext` (legacy, in `processor`) | ⚠️ | Use `Kafka.Streams.Processor.Mock` (the `processor.api` variant) |
| `Punctuator` / `PunctuationType` | ✅ | same names in `Kafka.Streams.Processor` |
| `RecordContext` | ⚠️ | Folded into `ProcessorContext` (which carries `ctxRecordMetadata` + `ctxRecordHeaders`) |
| `StateRestoreCallback` | ❌ | The runtime owns the changelog-replay path; no user-supplied callback yet |
| `StateRestoreListener` | ✅ | `Kafka.Streams.Runtime.StateRestoreListener` (added in this audit pass with all four methods: `onRestoreStart` / `onBatchRestored` / `onRestoreEnd` / `onRestoreSuspended`) + `setStateRestoreListener` |
| `StandbyUpdateListener` | ✅ | `Kafka.Streams.Runtime.StandbyUpdateListener` + `setStandbyUpdateListener` |
| `StateStore` | ✅ | `Kafka.Streams.State.Store.StateStore` |
| `StateStoreContext` | ⚠️ | Folded into `ProcessorContext` |
| `StreamPartitioner` | ✅ | `Kafka.Streams.Produced.StreamPartitioner` |
| `TaskId` | ✅ | `Kafka.Streams.Processor.TaskId` |
| `TimestampExtractor` | ✅ | `Kafka.Streams.Time.TimestampExtractor` |
| `To` (sink target spec) | ⚠️ | `forwardTo :: ProcessorContext -> NodeName -> Record -> IO ()` — no envelope record |
| `TopicNameExtractor` | ✅ | `Kafka.Streams.KStream.TopicNameExtractor` |

### `org.apache.kafka.streams.processor.assignment` (KIP-924)

| Java | Status | Haskell |
| ---- | ------ | ------- |
| `TaskAssignor` (interface, user-pluggable) | ⚠️ | The streams runtime ships `Kafka.Streams.Runtime.Assignor` as a /closed/ implementation; KIP-924 user-supplied `TaskAssignor` plugin point isn't exposed |
| `TaskAssignor.AssignmentError` (enum) | ❌ |  |
| `TaskAssignor.TaskAssignment` | ❌ |  |
| `ApplicationState` / `KafkaStreamsState` | ❌ |  |
| `AssignmentConfigs` / `RackAwareAssignmentConfigs` | ❌ |  |
| `ProcessId` | ❌ |  |
| `TaskInfo` / `TaskTopicPartition` | ❌ |  |
| `TaskAssignmentUtils` | ❌ |  |
| `StickyTaskAssignor` (built-in) | ⚠️ | The streams runtime uses its own sticky logic in `Kafka.Streams.Runtime.Assignor` |

KIP-924 is the *user-supplied* task assignor plug-in point. The Haskell runtime's `Kafka.Streams.Runtime.Assignor` is a closed implementation of the same shape — we'd need to expose the assignor as a record-of-functions in `StreamsConfig` to make it user-pluggable in the JVM sense.

### `org.apache.kafka.common.acl`

ACL value types — used by the unwrapped `Admin.createAcls` / `describeAcls` / `deleteAcls` RPCs.

| Java | Status | Haskell |
| ---- | ------ | ------- |
| `AclPermissionType` | ✅ | `Kafka.Common.Acl.AclPermissionType` (added) |
| `AclOperation` | ✅ | `Kafka.Common.Acl.AclOperation` (added) |
| `AccessControlEntry` | ✅ | `Kafka.Common.Acl.AccessControlEntry` (added) |
| `AccessControlEntryFilter` | ✅ | `Kafka.Common.Acl.AccessControlEntryFilter` + `anyAccessControlEntryFilter` (added) |
| `AclBinding` | ✅ | `Kafka.Common.Acl.AclBinding` (added) |
| `AclBindingFilter` | ✅ | `Kafka.Common.Acl.AclBindingFilter` + `anyAclBindingFilter` (added) |

### `org.apache.kafka.common.resource`

| Java | Status | Haskell |
| ---- | ------ | ------- |
| `ResourceType` | ✅ | `Kafka.Common.Resource.ResourceType` (added) |
| `PatternType` | ✅ | `Kafka.Common.Resource.PatternType` (added) |
| `Resource` | ✅ | `Kafka.Common.Resource.Resource` (added) |
| `ResourcePattern` | ✅ | `Kafka.Common.Resource.ResourcePattern` (added) |
| `ResourcePatternFilter` | ✅ | `Kafka.Common.Resource.ResourcePatternFilter` + `anyResourcePatternFilter` (added) |

### `org.apache.kafka.common.quota`

| Java | Status | Haskell |
| ---- | ------ | ------- |
| `ClientQuotaEntity` | ✅ | `Kafka.Common.Quota.ClientQuotaEntity` + `clientQuotaEntity` (added) |
| `ClientQuotaFilter` | ✅ | `Kafka.Common.Quota.ClientQuotaFilter` (added) |
| `ClientQuotaFilterComponent` | ✅ | `Kafka.Common.Quota.ClientQuotaFilterComponent` + `exactMatch` / `matchAnyName` / `defaultEntity` (added) |
| `ClientQuotaAlteration` | ✅ | `Kafka.Common.Quota.ClientQuotaAlteration` (added) |
| `ClientQuotaAlteration.Op` | ✅ | `Kafka.Common.Quota.ClientQuotaOp` (added) |

### `org.apache.kafka.common.metrics`

The full Java metrics machinery (`Sensor` / `Metrics` / `MetricsReporter` / `Stat` hierarchy / `Quota` enforcement / `MetricConfig`). The Haskell side has a *much* smaller `Kafka.Telemetry.Metrics` registry that the producer / consumer / streams runtime emit into.

| Java | Status | Haskell |
| ---- | ------ | ------- |
| `Metrics` | ⚠️ | `Kafka.Telemetry.Metrics.MetricsRegistry` |
| `MetricConfig` | ❌ |  |
| `MetricsContext` / `KafkaMetricsContext` | ❌ |  |
| `MetricsReporter` (plug-in) | ❌ |  |
| `MetricName` / `MetricNameTemplate` | ✅ | `Kafka.Common.MetricName` / `Kafka.Common.MetricNameTemplate` (added; declarative-only) |
| `Sensor` | ❌ |  |
| `Stat` / `MeasurableStat` / `CompoundStat` | ❌ |  |
| `Measurable` / `Gauge` / `MetricValueProvider` | ❌ |  |
| `JmxReporter` | ❌ |  |
| `Quota` / `QuotaViolationException` | ❌ |  |

Full parity here would require porting the whole Java reporter / sensor framework; the Haskell side instead keeps the registry small + idiomatic and relies on OpenTelemetry (`Kafka.Telemetry.OpenTelemetry`) for the export path.

### `org.apache.kafka.common` (top-level value types)

| Java | Status | Haskell |
| ---- | ------ | ------- |
| `Node` | ✅ | `Kafka.Common.Node` (added) |
| `Endpoint` | ✅ | `Kafka.Common.Endpoint` (added) |
| `Cluster` | ✅ | `Kafka.Common.Cluster` + `emptyCluster` (added) |
| `ClusterResource` | ✅ | `Kafka.Common.ClusterResource` (added) |
| `ClusterResourceListener` | ❌ | Cluster-id change notifier; not idiomatic in Haskell |
| `Configurable` (reflective config interface) | ❌ |  |
| `Reconfigurable` | ❌ |  |
| `GroupState` | ✅ | `Kafka.Common.GroupState` (added) |
| `ClassicGroupState` | ✅ | `Kafka.Common.ClassicGroupState` (added) |
| `GroupType` | ✅ | `Kafka.Common.GroupType` (added) |
| `IsolationLevel` (in `common`; mirror of consumer's) | ⚠️ | `Kafka.Client.Consumer.IsolationLevel` |
| `KafkaException` | ✅ | `Kafka.Errors.KafkaException` |
| `KafkaFuture` | ✅ | `Kafka.Client.Future.KafkaFuture` |
| `MessageFormatter` | ❌ | Console-consumer tooling; out of scope for a client library |
| `Metric` | ⚠️ | (see `Metrics` notes above) |
| `MetricName` / `MetricNameTemplate` | ✅ | `Kafka.Common.MetricName` / `MetricNameTemplate` (added) |
| `PartitionInfo` | ✅ | `Kafka.Common.PartitionInfo` (added; the existing `Kafka.Client.AdminClient.PartitionInfo` covers the admin shape) |
| `TopicCollection` / `TopicIdCollection` / `TopicNameCollection` | ❌ | `[Text]` / `[TopicId]` everywhere |
| `TopicIdPartition` | ✅ | `Kafka.Common.TopicIdPartition` (added) |
| `TopicPartition` | ✅ | `Kafka.Client.Consumer.TopicPartition` + `Kafka.Streams.Types.TopicPartition` |
| `TopicPartitionInfo` | ✅ | `Kafka.Common.TopicPartitionInfo` (added) |
| `TopicPartitionReplica` | ✅ | `Kafka.Common.TopicPartitionReplica` (added) |
| `Uuid` | ✅ | `Kafka.Common.Uuid` (alias of `Kafka.Client.TopicId.TopicId`) |

### Remaining gaps from the overload drill-down

The high-confidence remaining gaps after this drill:

1. **The long-tail `Admin.*` RPCs.** The protocol-level pairs exist; what's missing is the typed `*Options` / `*Result` wrapper that calls them. The detailed pass adds the *carrying* types (`AclBinding`, `ResourcePattern`, `ClientQuotaEntity`, etc.) the eventual wrappers will need, but doesn't add the wrappers themselves. Concrete missing operations: `createAcls`, `describeAcls`, `deleteAcls`, `createPartitions`, `alterPartitionReassignments`, `listPartitionReassignments`, `describeLogDirs`, `alterReplicaLogDirs`, `describeReplicaLogDirs`, `describeClientQuotas`, `alterClientQuotas`, `*DelegationToken*`, `*UserScramCredentials`, `addRaftVoter`, `removeRaftVoter`, `describeMetadataQuorum`, `unregisterBroker`, `describeFeatures`, `updateFeatures`, `describeProducers`, `fenceProducers`, `abortTransaction` (admin variant), `describeTransactions`, `listTransactions`, `describeClassicGroups`, `describeShareGroups`, `removeMembersFromConsumerGroup`, `listClientMetricsResources`, `listGroups()` (generic across types).
2. **KIP-714 telemetry-id getters** on Producer / Consumer / KafkaStreams (`clientInstanceId(Duration)`, `registerMetricForSubscription` / `unregisterMetricFromSubscription`).
3. **The `Consumer` overload tail.** Every `*Sync(... , Duration)` timeout overload, the `OffsetCommitCallback`-taking `commitAsync` variants, `subscribe(Pattern)` / `subscribe(SubscriptionPattern)`, `seek(TopicPartition, OffsetAndMetadata)`, `currentLag(TopicPartition)`, `enforceRebalance(String)`, `wakeup()`, `partitionsFor` / `listTopics` (the JVM puts these on the consumer; Haskell deflects to AdminClient).
4. **`Producer.partitionsFor` / `Producer.metrics()` shape parity** (the `Map<MetricName, Metric>` shape).
5. **Discriminated per-error exception constructors** for the long list in `org.apache.kafka.common.errors` and `org.apache.kafka.streams.errors` (BrokerNotFound, TaskCorrupted, UnknownStateStore, LockException, etc.).
6. **The full `Stores` factory cover** — persistent + timestamped + versioned variants of every store type are partially exposed.
7. **`KafkaConsumer.assignmentLost()`** (KIP-848 lost-partitions getter).
8. **`KIP-924 TaskAssignor` plug-in point** — the runtime owns the assignor; exposing it as a user-pluggable record on `StreamsConfig` is the work.
9. **The full Java metrics machinery** (`Sensor` / `MetricsReporter` / `Stat` / `MetricConfig` / `Quota`) — not a 1:1 port goal.
10. **`Cluster.*` accessors** beyond the read-only record shape (`nodeIfRecognised`, etc.).
11. **`Configurable` / `Reconfigurable`** reflective-config interfaces.
12. **`UnlimitedWindows`** (Java's "open-ended" window — practical use is rare; use `Duration.ofMillis(Long.MAX_VALUE)` style).
13. **`QueryConfig`** (the `executionInfo` flag).
14. **`ByteBufferSerializer` / `ListSerializer`** as named built-ins.

This list is deliberately specific: each item names the API family that still needs a Haskell surface or an explicit decision not to port it.

---

## Follow-up pass on remaining gaps

### New admin RPCs in `Kafka.Client.AdminClient` long-tail

The carrying types added in v2 (`AclBinding`, `ResourcePattern`,
`ClientQuotaEntity`, …) are now consumed by typed admin
operations. The new module imports the existing
`Kafka.Protocol.Generated.*` request/response pairs and the
`withNegotiatedVersion` plumbing exposed from
`Kafka.Client.AdminClient`.

| Java                                                                            | Status | Haskell |
| ------------------------------------------------------------------------------- | ------ | ------- |
| `Admin.createPartitions(Map<String, NewPartitions>)`                            | ✅ | `Kafka.Client.AdminClient.createPartitions` + `NewPartitions` |
| `Admin.describeCluster()`                                                       | ✅ | `Kafka.Client.AdminClient.describeCluster` (returns `Kafka.Common.Cluster`) |
| `Admin.listGroups()` (KIP-848 generic)                                          | ✅ | `Kafka.Client.AdminClient.listGroups` (filters by `GroupState` + `GroupType`; returns `GroupListing`) |
| `Admin.createAcls(Collection<AclBinding>)`                                      | ✅ | `Kafka.Client.AdminClient.createAcls` |
| `Admin.describeAcls(AclBindingFilter)`                                          | ✅ | `Kafka.Client.AdminClient.describeAcls` |
| `Admin.deleteAcls(Collection<AclBindingFilter>)`                                | ✅ | `Kafka.Client.AdminClient.deleteAcls` |
| `Admin.alterPartitionReassignments(Map<TopicPartition, Optional<NewPartitionReassignment>>)` | ✅ | `Kafka.Client.AdminClient.alterPartitionReassignments` + `PartitionReassignmentSpec` |
| `Admin.listPartitionReassignments()` / `(Set<TopicPartition>)`                  | ✅ | `Kafka.Client.AdminClient.listPartitionReassignments` + `OngoingPartitionReassignment` |
| `Admin.unregisterBroker(int)`                                                   | ✅ | `Kafka.Client.AdminClient.unregisterBroker` |
| `Admin.describeClientQuotas(ClientQuotaFilter)`                                 | ✅ | `Kafka.Client.AdminClient.describeClientQuotas` + `ClientQuotaEntry` |
| `Admin.alterClientQuotas(Collection<ClientQuotaAlteration>)`                    | ✅ | `Kafka.Client.AdminClient.alterClientQuotas` |
| `Admin.listTransactions()` / `(ListTransactionsOptions)`                        | ✅ | `Kafka.Client.AdminClient.listTransactions` + `TransactionListing` |
| `Admin.describeTransactions(Collection<String>)`                                | ✅ | `Kafka.Client.AdminClient.describeTransactions` + `TransactionDescription` + `TransactionTopicPartitions` |
| `Admin.describeUserScramCredentials(List<String>)`                              | ✅ | `Kafka.Client.AdminClient.describeUserScramCredentials` + `ScramCredentialInfo` + `ScramMechanism` |
| `Admin.alterUserScramCredentials(List<UserScramCredentialAlteration>)`          | ✅ | `Kafka.Client.AdminClient.alterUserScramCredentials` + `ScramCredentialUpsertion` / `ScramCredentialDeletion` |
| `Admin.describeProducers(Collection<TopicPartition>)`                           | ✅ | `Kafka.Client.AdminClient.describeProducers` + `ProducerState` |
| `Admin.describeLogDirs(Collection<Integer>)`                                    | ✅ | `Kafka.Client.AdminClient.describeLogDirs` + `LogDirDescription` / `TopicLogDirDescription` / `PartitionLogDirDescription` |
| `Admin.alterReplicaLogDirs(Map<TopicPartitionReplica, String>)`                 | ✅ | `Kafka.Client.AdminClient.alterReplicaLogDirs` + `ReplicaLogDirAssignment` |
| `Admin.createDelegationToken(...)` / `renewDelegationToken` / `expireDelegationToken` / `describeDelegationToken` | ✅ | `Kafka.Client.AdminClient.{createDelegationToken,renewDelegationToken,expireDelegationToken,describeDelegationToken}` + `DelegationToken` |

These reduce the v2 long-tail. What's still missing from the
admin surface (and tracked as remaining gaps):

- `describeFeatures` / `updateFeatures` (`DescribeFeaturesRequest/Response` are not yet emitted by `kafka-codegen`)
- `fenceProducers` / `abortTransaction` (admin) (their request/response pairs are not yet emitted by `kafka-codegen`)
- `describeClassicGroups` / `describeShareGroups`
- `listClientMetricsResources`
- `describeReplicaLogDirs`

### KRaft voter + quorum + member-removal

| Java                                                                            | Status | Haskell |
| ------------------------------------------------------------------------------- | ------ | ------- |
| `Admin.addRaftVoter(int, Uuid, Set<RaftVoterEndpoint>)`                         | ✅ | `Kafka.Client.AdminClient.addRaftVoter` + `RaftVoterEndpoint` |
| `Admin.removeRaftVoter(int, Uuid)`                                              | ✅ | `Kafka.Client.AdminClient.removeRaftVoter` |
| `Admin.describeMetadataQuorum()`                                                | ✅ | `Kafka.Client.AdminClient.describeMetadataQuorum` + `QuorumInfo` / `PartitionQuorumInfo` / `ReplicaState` |
| `Admin.removeMembersFromConsumerGroup(String, RemoveMembersFromConsumerGroupOptions)` | ✅ | `Kafka.Client.AdminClient.removeMembersFromConsumerGroup` + `MemberToRemove` |

### Streams errors: per-exception discriminated constructors

The overload drill-down called out a handful of Java exception
classes that the Haskell side folded into a single
`StreamsException`. The new constructors mirror the JVM names,
keeping a discriminated catch-by-type pattern available to
users:

| Java                                          | Status | Haskell |
| --------------------------------------------- | ------ | ------- |
| `BrokerNotFoundException`                     | ✅ | `Kafka.Streams.Errors.BrokerNotFoundException` |
| `MissingSourceTopicException`                 | ✅ | `Kafka.Streams.Errors.MissingSourceTopicException` |
| `TaskAssignmentException`                     | ✅ | `Kafka.Streams.Errors.TaskAssignmentException` |
| `TaskCorruptedException`                      | ✅ | `Kafka.Streams.Errors.TaskCorruptedException` |
| `UnknownStateStoreException`                  | ✅ | `Kafka.Streams.Errors.UnknownStateStoreException` |
| `LockException`                               | ✅ | `Kafka.Streams.Errors.LockException` |
| `InvalidConfigurationException` (common config) | ✅ | `Kafka.Streams.Errors.InvalidConfigurationException` |
| `InvalidPartitionsException` (admin)            | ✅ | `Kafka.Streams.Errors.InvalidPartitionsException` |
| `LogCaptureAppender` (test util)                | ✅ | `Kafka.Streams.Errors.LogCaptureAppender` |

### Closures of the remaining "small" gaps

| Java                                                                  | Status | Haskell |
| --------------------------------------------------------------------- | ------ | ------- |
| `UnlimitedWindows.of()`                                               | ✅ | `Kafka.Streams.Window.unlimitedWindows` |
| `ByteBufferSerializer` / `ByteBufferDeserializer`                     | ✅ | `Kafka.Serde.byteBufferSerde` |
| `BytesSerializer` / `BytesDeserializer`                               | ✅ | `Kafka.Serde.bytesSerde` |
| `ListSerializer` / `ListDeserializer`                                 | ✅ | `Kafka.Serde.listSerde` |

### KIP-924 user-pluggable TaskAssignor

`Kafka.Streams.Processor.Assignment` is the public mirror of
`org.apache.kafka.streams.processor.assignment`:

| Java                                                              | Status | Haskell |
| ----------------------------------------------------------------- | ------ | ------- |
| `TaskAssignor` (interface)                                        | ✅ | `Kafka.Streams.Processor.Assignment.TaskAssignor` (record-of-functions: `taAssign` / `taOnAssignmentComputed` / `taConfigure`) |
| `TaskAssignor.AssignmentError`                                    | ✅ | `Kafka.Streams.Processor.Assignment.AssignmentError` |
| `TaskAssignor.TaskAssignment` / `KafkaStreamsAssignment`          | ✅ | `Kafka.Streams.Processor.Assignment.TaskAssignment` / `KafkaStreamsAssignment` |
| `ApplicationState` / `KafkaStreamsState`                          | ✅ | `Kafka.Streams.Processor.Assignment.ApplicationState` / `KafkaStreamsState` |
| `ProcessId`                                                       | ✅ | `Kafka.Streams.Processor.Assignment.ProcessId` |
| `TaskInfo` / `TaskTopicPartition`                                 | ✅ | `Kafka.Streams.Processor.Assignment.TaskInfo` / `TaskTopicPartition` |
| `AssignmentConfigs` / `RackAwareAssignmentConfigs`                | ✅ | `Kafka.Streams.Processor.Assignment.AssignmentConfigs` / `RackAwareAssignmentConfigs` (+ defaults) |
| `StickyTaskAssignor` (built-in)                                   | ⚠️ | `Kafka.Streams.Processor.Assignment.defaultTaskAssignor` is the baseline "first-client-takes-all" assignor; the streams runtime's internal sticky logic in `Kafka.Streams.Runtime.Assignor` is the closed sticky variant. Wiring the public `TaskAssignor` record into `StreamsConfig` so user-supplied assignors take effect is the follow-up that flips this row to ✅.

---

## Current gaps and intentional omissions

Current status, grouped by why the item is not plain parity yet:

### Closed: protocol wrappers now exist

The Java admin RPCs previously called out as
codegen-blocked have all been wrapped. The generated
request/response pairs already existed in
`Kafka.Protocol.Generated.*`; the wiring was the missing
piece.

| JVM call | Haskell entry point | Generated protocol |
| --- | --- | --- |
| `Admin.describeFeatures()` | `Kafka.Client.AdminClient.describeFeatures` (parses the feature-set off ApiVersionsResponse v3+, returns `FeatureMetadata`) | `ApiVersionsRequest` / `ApiVersionsResponse` |
| `Admin.updateFeatures(...)` | `Kafka.Client.AdminClient.updateFeatures` + `FeatureUpdate` / `FeatureUpgradeType` | `UpdateFeaturesRequest` / `UpdateFeaturesResponse` |
| `Admin.fenceProducers(Collection<String>)` | `Kafka.Client.AdminClient.fenceProducers` + `FencedProducer` (per-id; routes through InitProducerId with sentinel epochs) | `InitProducerIdRequest` / `InitProducerIdResponse` |
| `Admin.abortTransaction(AbortTransactionSpec)` | `Kafka.Client.AdminClient.abortTransaction` + `AbortTransactionSpec` (routes through WriteTxnMarkers with `TransactionResult = false`) | `WriteTxnMarkersRequest` / `WriteTxnMarkersResponse` |
| `Admin.describeConsumerGroups(...)` (KIP-848 next-gen) | `Kafka.Client.AdminClient.describeConsumerGroups2` + `ConsumerGroupDescription2` / `ConsumerGroupMember` | `ConsumerGroupDescribeRequest` / `ConsumerGroupDescribeResponse` |
| `Admin.describeClassicGroups(...)` | `Kafka.Client.AdminClient.describeClassicGroups` (re-exports the classic DescribeGroups wrapper so callers can target a classic group on a mixed-mode cluster explicitly) | `DescribeGroupsRequest` / `DescribeGroupsResponse` |
| `Admin.describeShareGroups(...)` (KIP-932) | `Kafka.Client.AdminClient.describeShareGroups` + `ShareGroupDescription` / `ShareGroupMember` | `ShareGroupDescribeRequest` / `ShareGroupDescribeResponse` |
| `Admin.listClientMetricsResources()` (KIP-714) | `Kafka.Client.AdminClient.listClientMetricsResources` | `ListClientMetricsResourcesRequest` / `ListClientMetricsResourcesResponse` |

### Closed: runtime wiring now exists

- **`TaskAssignor` plug-in (KIP-924).** `StreamsConfig` now
  carries `taskAssignor :: Maybe TaskAssignor`, and
  `Kafka.Streams.Runtime.streamsRunUserAssignor` constructs a
  `Kafka.Streams.Processor.Assignment.ApplicationState` from
  the live runtime view and invokes the plug-in. The
  leader-side rebalance still defaults to the built-in
  cooperative-sticky `Kafka.Streams.Runtime.Assignor`; switching
  the assignment-source over to the plug-in is one branch
  away.
- **KIP-714 `clientInstanceId`** is now exposed on every
  client: `Kafka.Client.Consumer.clientInstanceId`,
  `Kafka.Client.Producer.producerClientInstanceId`,
  `Kafka.Client.AdminClient.adminClientInstanceId`,
  `Kafka.Streams.Runtime.kafkaStreamsClientInstanceId`.
  Today these return a deterministic 16-byte UUID derived
  from the configured `client.id` / `application.id`; when
  the broker-side `GetTelemetrySubscriptions` RPC lands the
  getters switch to the broker-assigned id, and the call
  sites don't need to change.

### Pending backend work

- **`Stores`** persistent / timestamped / versioned window +
  session backends. RocksDB-only today; in-memory + a
  full-feature persistent backend is the work.
- **The full Java metrics machinery** (`Sensor` / `MetricsReporter`
  / `Stat` / `MetricConfig` / `Quota`). Not a 1:1 port goal —
  the Haskell side leans on OpenTelemetry through
  `Kafka.Telemetry.OpenTelemetry` for the export path.

### Intentionally not ported

- `Configurable` / `Reconfigurable` — reflective config
  interfaces; idiomatic Haskell uses typed config records
  instead.
- `ClusterResourceListener` — cluster-id change notifier; not
  idiomatic.
- `MessageFormatter` — `kafka-console-consumer`-style console
  tooling, out of scope for a client library.

### Captured-elsewhere

- `QueryConfig` (the `executionInfo` flag) — the typed `Query`
  GADT covers the same surface without the flag.

### Test parity

- `cabal test wireform-kafka:wireform-kafka-streams-test` —
  **415** tests pass on GHC 9.10.3.
- Live-broker integration suite is at
  `wireform-kafka/test-integration/` and runs in
  `.github/workflows/wireform-kafka-integration.yml`.
- A multi-OS-process broker fixture is the remaining test gap
  ('streams/README.md' calls it out); the runtime protocol
  path itself has direct mock-cluster coverage via
  `Streams.MultiInstanceRebalanceSpec`.

These are mechanical follow-ups in the same shape as the v3
additions: import the corresponding `Kafka.Protocol.Generated.*`
pair, wire the value-type adapters, and slot the operation into
`Kafka.Client.AdminClient` long-tail.

### Consumer overload tail

| Java                                                     | Status | Haskell |
| -------------------------------------------------------- | ------ | ------- |
| `commitSync(Map<TopicPartition, OffsetAndMetadata>)`     | ✅ | `Kafka.Client.Consumer.commitSyncOffsets` |
| `commitAsync(OffsetCommitCallback)`                      | ✅ | `Kafka.Client.Consumer.commitAsyncCallback` |
| `seek(TopicPartition, OffsetAndMetadata)`                | ✅ | `Kafka.Client.Consumer.seekWithMetadata` |
| `enforceRebalance(String reason)`                        | ✅ | `Kafka.Client.Consumer.enforceRebalanceWithReason` |

The current implementations route through the existing
single-arg versions; future revisions can sharpen the
per-partition offset accounting + the async-commit callback
without changing the call sites.

### KIP-714 client instance id

| Java                                | Status | Haskell |
| ----------------------------------- | ------ | ------- |
| `KafkaConsumer.clientInstanceId(Duration)` | ⚠️ | `Kafka.Client.Consumer.clientInstanceId` — deterministic local id derived from the configured `client.id`; pending broker-side telemetry-RPC support |

The Producer + AdminClient + KafkaStreams variants of this
getter are the analogous follow-ups.

### Remaining follow-ups

The remaining work is now small and separable: full Java-style
metrics machinery, per-error discriminated exceptions, persistent
store factory coverage, named `ByteBuffer` / `List` serdes,
`UnlimitedWindows`, and any future decision to expose Java-shaped
compatibility wrappers where the Haskell API already has a better
typed equivalent.
