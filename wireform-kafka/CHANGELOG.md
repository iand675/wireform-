# Changelog for `wireform-kafka`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

- **Kafka Streams runtime, rebuilt on a `StreamDriver` seam**
  (`Kafka.Streams.Runtime.{Driver,NativeDriver,MockBroker}`): one task
  per subtopology and partition with co-partitioned routing,
  timestamp-ordered processing and back-pressure; changelogged stores
  restored before processing (checkpoints for persistent stores);
  repartition topics and global tables; exactly-once v2 (one
  transaction per commit, abort recovery from the changelogs, state
  wipe after an unclean stop); KTable change semantics (deletes and
  retractions reach every derived table, re-grouped aggregations, the
  KIP-213 foreign-key join); `MockBroker`, an in-memory cluster that
  runs the real runtime end to end, with fault injection.
- **Streams group assignor** (`Kafka.Streams.Runtime.GroupAssignor`,
  protocol `streams`): lag-aware, sticky, thread-proportional dealing of
  whole tasks; `num.standby.replicas` standby tasks kept current from the
  changelogs and promoted warm; KIP-441 warmups and probing rebalances;
  KIP-535 discovery (`applicationServer`, `streamsMetadata`,
  `queryMetadataForKey`, stale IQ reads from standbys). On the client:
  `Kafka.Client.Assignor` custom assignors with assignment user data,
  `Consumer.setAssignmentStrategy` / `setAssignmentHook`.
- **Suppress buffers are time-ordered**: `suppressWindowed`,
  `suppressWindowedShed` and `suppressUntilTimeLimit` scanned their
  whole buffer on every record and stream-time tick; they now keep a
  (time, key) index (rebuilt from the store after a restore) and evict
  only what is due. As on the JVM, a record is buffered before the due
  entries go, a window is emitted once stream time reaches its end plus
  grace, and an emitted record carries its last update's timestamp. The
  buffer cap counts the restored entries too. Windowed suppression ran
  with an error-thunk key serde for its changelog; it now has
  `windowedSerde`.
- **Windowed aggregations**: the aggregators forward each update keyed
  by its window themselves (the stream conversion rescanned the key's
  windows on every update and kept an ever-growing map of what it had
  sent); a record still updates its open windows when some of its
  hopping windows have closed (it was all or nothing); a windowed
  cogroup uses its windows (it bucketed into tumbling windows) and
  forwards every side's updates (only the first side's reached the
  stream); every window update is forwarded, as on the JVM, not only
  changed values. Session aggregates drop records past the grace
  period, and their stream is keyed by `SessionKey` with `Nothing`
  for each session merged into a larger one (`sessionKeySerde`).
- **`SINGLE_STORE_SELF_JOIN`** (KIP-862): an inner window join of a
  stream with itself runs on one store and processor when
  `optSingleStoreSelfJoin` is set, with output identical to the
  two-store join. `topologyOptimization` (`topology.optimization`, a
  level or a comma-separated list) is applied by the instance. A node
  now forwards to its children in the order they were added (it was
  the reverse), as the JVM does.
- **Stream-stream joins follow the JVM**: the right side matches the
  mirror of the left side's window (asymmetric `before` / `after` were
  applied the same way on both sides); records with the same key and
  timestamp are all kept (they overwrote each other); a joined record
  carries the later timestamp; stores keep records for the grace
  period too; both sides share one stream time; left and outer joins
  hold unmatched records until their window closes and emit them only
  then (KIP-633) instead of emitting `(v, Nothing)` at once. The
  `Joined` serdes and name are used for the join stores.
- **Record caches** (`statestoreCacheMaxBytes`, JVM
  `statestore.cache.max.bytes`, default 10 MiB split over the active
  tasks): the DSL's table stores buffer writes per key, and the store,
  its changelog and the operators downstream get the latest value of
  each key once per commit (earlier when a task's share fills up), as
  one change with the store's previous value. Reads and interactive
  queries see the buffered writes. `withCachingDisabled` (and
  `statestoreCacheMaxBytes = 0`) forwards every update at once, as
  before; `withCachingEnabledKV` caches a Processor API store.
  Replaces the free-standing `cachingKeyValueStore`. Windowed
  aggregations' stores are cached per window the same way
  (`withCachingEnabledW` for a Processor API window store).
- **Pattern sources at runtime**: `streamFromPattern` /
  `sourcePattern` topics are resolved against the cluster when the
  instance starts (whole-name match, internal topics left out) and again
  every `metadataMaxAgeMs` (`metadata.max.age.ms`); a change re-plans the
  instance in place (commit, clean close, new plan, rejoin) with its
  state carried over. `StreamDriver` gains `sdListTopics`.
- **Commit participants** (`Kafka.Streams.Runtime.Commit`): two-phase
  sinks (`twoPhaseParticipant`) prepare with every commit, are recorded
  in `<app>-twophase-commits` inside the transaction, commit after it,
  and are settled at the next start after a crash.
- **Streams metrics from the running instance** (`streamsMetrics`):
  per-task unboxed counters plus commits, aborts, recoveries,
  rebalances, restored and standby records; `localTaskLags`.

- **`wireform-kafka-protocol` sublibrary.** Codegen-emitted
  `Kafka.Protocol.Generated.*` wire records (and
  `Kafka.Protocol.{Primitives,Message,Wire.*}`) live in a dedicated
  public sublibrary; the default `wireform-kafka` library
  re-exports them. Regeneration runs
  `scripts/sync-kafka-protocol-cabal-modules.sh`.
- **`Kafka.Client.AdminClient` — KIP-584 feature flags.**
  `describeFeatures` parses the supported + finalized feature
  set off ApiVersionsResponse v3+ and returns a typed
  `FeatureMetadata`. `updateFeatures` (with
  `FeatureUpdate` + `FeatureUpgradeType`) wraps
  UpdateFeaturesRequest (API key 57) for declarative
  cluster-wide feature-level changes.
- **`Kafka.Client.AdminClient` — KIP-360 producer fencing.**
  `fenceProducers` bumps the producer epoch of the supplied
  transactional ids via InitProducerIdRequest, returning the
  authoritative (producer-id, epoch) tuple per id. Equivalent
  to `Admin.fenceProducers(Collection<String>)`.
- **`Kafka.Client.AdminClient` — KIP-664 manual transaction
  abort.** `abortTransaction` + `AbortTransactionSpec` route
  through WriteTxnMarkers to abort a hanging transaction by
  (producer-id, producer-epoch, coordinator-epoch) directly
  on the affected partitions.
- **`Kafka.Client.AdminClient` — KIP-848 group describe.**
  `describeConsumerGroups2` wraps the next-gen
  ConsumerGroupDescribe RPC (API key 69) and returns
  `ConsumerGroupDescription2` / `ConsumerGroupMember`
  including the group/assignment epoch, assignor name, and
  per-member (subscribed-topics, current-assignment,
  target-assignment) tuples.
- **`Kafka.Client.AdminClient` — KIP-932 share groups.**
  `describeShareGroups` + `ShareGroupDescription` /
  `ShareGroupMember` wrap ShareGroupDescribe (API key 77).
- **`Kafka.Client.AdminClient.describeClassicGroups`.** Same
  shape as the existing `describeConsumerGroups`; surface
  named to match the JVM 4.0 SDK's "explicit classic group"
  call site.
- **`Kafka.Client.AdminClient.listClientMetricsResources`**
  (KIP-714) — lists the named client-metrics configurable
  resources the broker exposes.
- **KIP-714 `clientInstanceId` on every client.**
  `Kafka.Client.Producer.producerClientInstanceId`,
  `Kafka.Client.AdminClient.adminClientInstanceId`,
  `Kafka.Streams.Runtime.kafkaStreamsClientInstanceId`
  return a deterministic 16-byte UUID derived from the
  configured `client.id` / `application.id`. Switches to the
  broker-assigned id when the `GetTelemetrySubscriptions`
  pipeline lands. (Consumer already had this in v3e.)
- **KIP-924 `TaskAssignor`.** `StreamsConfig.taskAssignor`
  replaces the streams group assignor's dealing at every
  rebalance; the result is validated (`AssignmentError`, incl.
  `MissingActiveTask`) and an invalid one falls back to the
  built-in assignment. `KafkaStreamsState` carries per-task lags.

### Changed

- **Kafka Streams.** `groupTableBy` returns a re-grouped table with
  subtract/add semantics; table operators forward `Change`s;
  aggregations return `KTable`s. Orphan-topic detection derives the
  expected topics from the planner. The ops demos run the real runtime
  on `MockBroker`.
- **Removed (Kafka Streams).** `Runtime.EOS` (`EOSCoordinator`,
  `runCommitCycle`, `withTwoPhaseSinks`), the `Kafka.Streams.Mock.*`
  simulated runtime and `MultiInstanceMockHarness`, `Runtime.{Task,
  StandbyTask,StandbyDriver,ProbingRebalance}`, `reportWarmupLag`,
  `streamsRunUserAssignor`, `setLagListener` / `publishLag`,
  `groupByKTable`, and the config knobs the runtime never read
  (dispatch mode, cache size, task timeout, assignor class, DSL store,
  rack awareness, window size, upgrade.from).
- **Module consolidation.** Folded the JVM-equivalence SDK
  shims and admin long-tail RPCs back into their primary
  modules so the public surface has one obvious home per
  concept:
  - `Kafka.Client.ConsumerSdk` → `Kafka.Client.Consumer`.
    `ConsumerRecords`, `OffsetAndMetadata`,
    `ConsumerGroupMetadata`, `OffsetCommitCallback`,
    `SubscriptionPattern`, `clientInstanceId`, and the
    overload tail (`commitSyncOffsets`,
    `commitAsyncCallback`, `seekWithMetadata`,
    `enforceRebalanceWithReason`) all live in
    `Kafka.Client.Consumer` directly.
  - `Kafka.Client.AdminClient.Extras` →
    `Kafka.Client.AdminClient`. `createPartitions`,
    `describeCluster`, `listGroups`, `createAcls` /
    `describeAcls` / `deleteAcls`,
    `alter/listPartitionReassignments`,
    `unregisterBroker`, `describe/alterClientQuotas`,
    `list/describeTransactions`,
    `describe/alterUserScramCredentials`,
    `describeProducers`, `describeLogDirs`,
    `alterReplicaLogDirs`, delegation-token operations,
    `add/removeRaftVoter`, `describeMetadataQuorum`, and
    `removeMembersFromConsumerGroup` all live in
    `Kafka.Client.AdminClient` directly.
  - `Kafka.Streams.Sink.RotatingFile` →
    `Kafka.Streams.Printed`. `RotatingHandle`,
    `openRotatingHandle`, `closeRotatingHandle`,
    `writeLine`, `rotatingPrintStream`,
    `rotatingPrintToHandle` now sit alongside the
    JVM-parity `Printed.toRotatingFile` builder.

  Old module names no longer exist; downstream call sites
  should swap the import path while keeping the symbol
  names unchanged.

### Added

- **`Kafka.Streams.DSL`** — Haskell-native builder-implicit
  façade over the existing `KStream` / `KTable` API. Carries
  the `StreamsBuilder` in a hand-rolled reader monad so users
  don't thread it through every source / sink call. Ships
  short, unsuffixed names (`map`, `filter`, `flatMap`, `peek`,
  `selectKey`, `merge`, `branch`, `join`, `leftJoin`,
  `outerJoin`, `count`, `reduce`, `aggregate`, …) plus a `|>`
  pipe operator. The `Pipe` class lets the same operator work
  on the head of a chain (pure `KStream`) and on the tail
  (`Streams (KStream …)`). The original imperative API
  (`streamFromTopic` / `filterStream` / `mapValues` / …) is
  unchanged.
- **`Kafka.Streams.Printed`** — `Printed.toFile` with
  log rotation. `openRotatingHandle` / `writeLine` /
  `closeRotatingHandle` manage the active file lifecycle;
  rotation triggers on either size (`rfMaxBytes`) or age
  (`rfMaxAge`). Rolled files take a UTC-suffixed archive name
  (`stream.20260516T110942Z.log`) so the active path stays
  stable. The terminal `KStream` sinks `rotatingPrintStream` /
  `rotatingPrintToHandle` close the parity gap with the JVM
  `KStream.print(Printed.toFile(...))`.
- **`Kafka.Streams.Printed`** — direct port of the JVM
  `org.apache.kafka.streams.kstream.Printed<K,V>` builder.
  `toSysOut` / `toErr` / `toHandle` / `toFile` /
  `toRotatingFile` constructors; `withLabel` /
  `withKeyValueMapper` modifiers; `printKStream` applies the
  built `Printed` to a `KStream`. Bracket-form
  `withPrintedFile` / `withPrintedRotatingFile` for
  deterministic shutdown.
- **Schema Registry compatibility-mode probing.**
  `Kafka.Streams.Serde.SchemaRegistry.SchemaRegistryClient`
  gained two new methods: `srCompatibilityMode` (read the
  per-subject policy — `NONE` / `BACKWARD` / `FORWARD` /
  `FULL` / `*_TRANSITIVE`) and `srTestCompatibility` (ask
  whether a candidate schema would pass the policy). The
  HTTP-backed client wires `GET /config/{subject}` and
  `POST /compatibility/subjects/{subject}/versions/latest`.
  The new `registrySerdeChecked` wrapper probes the policy
  once at construction time and fails fast with
  `IncompatibleSchema` before a producer publishes.
- **`Kafka.Streams.Pipeline` expansion.** The existing
  `Pipeline a b ≃ a -> IO b` newtype now ships `Arrow`,
  `ArrowChoice`, `Functor`, and `Applicative` instances so the
  full Kleisli vocabulary (`first` / `second` / `***` / `&&&`,
  `left` / `right` / `+++` / `|||`, `arr`, `fmap`) works on
  Pipeline values. Many new smart constructors:
  `pfilterNot`, `pvalues`, `pmerge`, `pmergeAll`, `pbranch`,
  `psink`, `psinkWith`, `pthrough`, `ptoTable`, `ptoStream`,
  `prepartition`, and a `liftPure` alias for `arr`.

### Fixed

- **GHC 9.10 build errors** picked up after the CI matrix bump:
  - `Kafka.Client.Group`: untangle the `Control.Exception` /
    `Control.Monad.IO.Unlift` import-list shadowing.
  - `Kafka.Client.Group`: switch the `closeTimeoutMs` accessor
    to `OverloadedRecordDot` so `DuplicateRecordFields`
    resolves it correctly.
  - `Kafka.Client.Consumer`: `closeConsumerWithTimeout` /
    `closeConsumerWithoutLeavingGroup` are declared
    `MonadIO m =>` but the implementation is `IO`; thread
    `liftIO`.
  - `Kafka.Client.Producer`: drop duplicate `INLINABLE` pragmas
    that collide with the earlier `INLINE` pragmas on the same
    binding.
  - `Kafka.Telemetry.OpenTelemetry`: use
    `Attributes.emptyAttributes` for `libraryAttributes`;
    `mempty` is no longer accepted by the current `Attributes`
    type.

- **`Kafka.Errors`** — the exception hierarchy every public Kafka
  operation throws on failure. `KafkaException` carries a
  structured `KafkaErrorKind` (`ConnectError`,
  `AuthenticationError`, `TimeoutError`, `RecordTooLargeError !Int
  !Int`, `ProducerFencedError`, `ConfigurationError ![Text]`,
  `OffsetOutOfRangeError !Text !Int32 !Int64`, …) plus an optional
  underlying `SomeException` cause. Includes `isRetriable` /
  `isFatal` classifiers (KIP-487 shape) and an `orThrow :: IO
  (Either String a) -> IO a` bridge for migrating internal
  helpers. Mirrors `org.apache.kafka.common.errors.*` on the JVM.
- **`Kafka.Headers`** — typed wrapper around `Vector (Text,
  ByteString)`. Headers are a list on the wire but at the
  application level you almost always want random-access lookup
  and structured construction. The new `Headers` newtype preserves
  insertion order, supports O(n) lookup / O(1) append / O(n)
  replace, and has a `Semigroup`/`Monoid` instance.
- **`Kafka.Serde`** — `Serde a` plus the standard built-ins
  (`textSerde`, `int16Serde`, `int32Serde`, `int64Serde`,
  `word16Serde`, `word32Serde`, `word64Serde`, `doubleSerde`,
  `floatSerde`, `voidSerde`, `uuidSerde`, `jsonSerde`, …). Moved
  here from `Kafka.Streams.Serde`, which is now a thin re-export
  so existing `import Kafka.Streams.Serde` call sites keep
  working. Numeric serdes use the GHC `byteSwap16` /
  `byteSwap32` / `byteSwap64` primops (one `MOV` + one `BSWAP`
  on x86-64; one `STR` + one `REV` on ARM64) — the same pattern
  `Kafka.Protocol.Wire` already uses for the protocol layer.
  Replaces the hand-rolled four-shift-OR sequence the previous
  encoder / decoder used.
- **`Kafka.Serde.Proto`** — `protoSerde :: (MessageEncode a,
  MessageDecode a) => Serde a` for any `wireform-proto`
  message. Hard dependency on the `wireform-proto` package;
  encodes via `Proto.Encode.encodeMessage`, decodes via
  `Proto.Decode.decodeMessage`. Threads the structured
  `Proto.Wire.Decode.DecodeError` through `show` to fit the
  `Serde`'s `String` error channel.
- **`Kafka.Serde.Avro`** — `avroSerde :: (ToAvro a, FromAvro a)
  => AvroType -> Serde a` for any `wireform-avro`-typed
  payload, plus an `avroValueSerde :: AvroType -> Serde
  Avro.Value.Value` for callers that work in the dynamic
  representation. Hard dependency on the `wireform-avro`
  package. Schema is passed at call-site, so the same `Serde`
  shape composes cleanly with the Confluent
  `Kafka.Streams.Serde.SchemaRegistry` envelope wrapper.
- **`Kafka.Topic`** — typed topic reference. `Topic k v` bundles
  a name + key serde + value serde. Smart constructors `topic`,
  `topicAny`, `bytesTopic`, `textTopic`.
- **`Kafka.Client.Producer.publish` / `publish_`** — typed sends
  that take a `Topic k v` and apply the topic's serdes
  automatically. Saves the typical `encodeUtf8` boilerplate at
  every send site.
- **`Kafka.Client.Producer.producerHealthy`** and
  **`Kafka.Client.Consumer.consumerHealthy`** — cheap in-process
  health probes suitable for a Kubernetes `livenessProbe`.
  `producerHealthy` returns `False` if the sender thread died;
  `consumerHealthy` returns `False` if the group's heartbeat
  thread died. Neither call contacts the broker.
- **`Kafka.Client.AdminClient.ensureTopic`** — idempotent topic
  creation. Calls `createTopics` for the supplied topic and
  treats the broker-side `TOPIC_ALREADY_EXISTS` (error code 36)
  as success.
- Runnable client examples under **`examples/Kafka/Client/Examples/`**:
  `Produce`, `ProduceTyped`, `Consume`, `Group`, `Transaction`.
  All wired to a `wireform-kafka-client-examples` cabal
  executable: `cabal run wireform-kafka-client-examples produce`.
- **`ConsumerRecord`, `ProducerRecord`, `RecordMetadata`, and
  `TopicPartition` fields are renamed to bare names**: `topic`,
  `partition`, `offset`, `timestamp`, `key`, `value`, `headers`.
  The previous Hungarian-prefixed selectors (`crKey`,
  `recordValue`, `metadataTopic`, `tpTopic`, …) no longer exist;
  callers read fields with `OverloadedRecordDot` syntax
  (`rec.key`, `md.offset`, `tp.topic`, …). `DuplicateRecordFields`
  is enabled in the cabal common defaults so the four records can
  share field names without ambiguity.
  
  Migration: every `crKey rec` becomes `rec.key`, every
  `Producer.recordTopic = "events"` in a record literal becomes
  `topic = "events"`, etc. The dot syntax disambiguates the
  record type at the use site, so no other annotations are
  needed.

### Changed

- **Every public function in the client surface is now polymorphic
  over `MonadIO m`** (or `MonadUnliftIO m` for the `with*` brackets
  and the `runConsumer` family), with the body wrapped in `liftIO $`
  so the implementation is unchanged. The IO call path is unaffected
  thanks to `INLINABLE` + `SPECIALIZE` pragmas on every entry point;
  application code that runs in a custom monad stack (ReaderT IO,
  RIO, MonadAppM, …) can now call `Kafka.sendMessage`,
  `Kafka.poll`, `Kafka.commitSync`, `Kafka.runConsumer`, … without
  sprinkling `liftIO` at every call site. Adds `unliftio-core`
  (>= 0.2 && < 0.3) — just the `MonadUnliftIO` typeclass, no
  transitive dep churn.

  Covers `Kafka.Client.Producer`, `Kafka.Client.Consumer`,
  `Kafka.Client.AdminClient`, `Kafka.Client.Transaction`,
  `Kafka.Client.Group`, `Kafka.Client.ShareConsumer`. The internal
  helpers (`getNextCorrelationId`, `runHandler`, `decodeAllBatches`,
  …) stay in `IO` because callers don't see them and the
  dictionary-passing overhead would hurt the hot path.

- **`defaultProducerConfig` now matches the Java 3.x producer
  defaults out of the box: idempotent producer is **ON**, acks
  are **all** (`producerDelivery = ExactlyOnce`), and
  `max.in.flight.requests.per.connection` is 5. Strongest
  single-producer delivery guarantees (no duplicates, no
  reordering) are the default; callers who specifically need
  lower latency can downgrade to `AtLeastOnce` / `AtMostOnce`
  explicitly. **Behavioural caveat:** creating a producer now
  triggers an additional `InitProducerId` round-trip with the
  coordinator.
- **`withProducer` / `withConsumer` / `withAdminClient` /
  `withGroupConsumer`** throw `Kafka.Errors.KafkaException` on
  setup failure instead of the previous generic `IOError`. The
  structured error kind lets callers pattern-match on
  `ConnectError` / `ConfigurationError` / etc. instead of
  grepping error strings.
- **`Kafka.Client.Producer.sendMessageAsync`** now returns `IO
  (MVar (Either String RecordMetadata))` instead of `IO (Either
  String ())`. Callers wait on the broker result by taking the
  `MVar` whenever they're ready:

  ```haskell
  handle <- sendMessageAsync p "events" Nothing "hello"
  ... do other work ...
  md <- takeMVar handle
  ```

  The producer's interceptor + onAcknowledgement hooks still fire
  on the sender thread.
- **`Kafka.Client.Producer` fire-and-forget rename**: the `Drop`
  family now uses the trailing-underscore convention idiomatic
  with `Control.Monad.forM_`:
  `sendMessageDrop` → `sendMessage_`,
  `sendMessageDropUnsafe` → `sendMessageUnsafe_`,
  `sendMessageDropFastest` → `sendMessageFastest_`,
  `sendMessagesDrop` → `sendMessages_`.
- **`Kafka.Client.Producer.withProducer`** and `withProducer'` —
  `Control.Exception.bracket` wrappers that open a producer, run a
  body, and flush + close on exit even when the body throws.
  Setup failures surface as `KafkaException` so they participate
  in the usual bracket / catch / restart idioms instead of being
  a returned `Either`.
- **`Kafka.Client.Consumer.withConsumer`** and `withConsumer'` — the
  consumer analogue. Optionally subscribes to a topic list as part
  of the bracket; on exit calls `closeConsumerWithTimeout` (or a
  user-supplied shutdown). Setup failures surface as
  `KafkaException`.
- New **`CONCEPTS.md`** — a five-minute, plain-language Kafka
  primer covering topics, partitions, brokers, producers,
  consumers, consumer groups, offsets, transactions, and streams,
  each mapped to the type / function in this library.
- The `Kafka` umbrella module now re-exports `Kafka.Client.Group`
  so `runConsumer` / `runBatchedConsumer` / `withGroupConsumer` /
  `defaultGroupConfig` are reachable as `Kafka.*` without a
  separate import.

### Removed

- The five `Kafka.Client.*Extras` modules are gone. Every utility
  they exposed is consolidated into the module it naturally
  belongs in. No functional change; just a cleaner module
  structure.

  | from | to |
  |---|---|
  | `Kafka.Client.AdminExtras.defaultAdminApiTimeoutMs` | `Kafka.Client.AdminClient` |
  | `Kafka.Client.AdminExtras.TopicCreateDefaults` (+ `defaultTopicCreateDefaults`) | `Kafka.Client.AdminClient` |
  | `Kafka.Client.AdminExtras.NullKeyCompactionPolicy` (+ `defaultNullKeyCompactionPolicy`) | `Kafka.Client.AdminClient` |
  | `Kafka.Client.AdminExtras.admin*LatencyMs` constants | `Kafka.Client.AdminClient` |
  | `Kafka.Client.AdminExtras.HostResolver` | `Kafka.Network.Connection` |
  | `Kafka.Client.AdminExtras.PerPartitionFetchKnob` | `Kafka.Client.Consumer` |
  | `Kafka.Client.AdminExtras.SslEngineFactory` | dropped (was a degenerate `newtype` wrapping `IO ()` with no callers) |
  | `Kafka.Client.ConsumerExtras` (all of it) | `Kafka.Client.Consumer` |
  | `Kafka.Client.ConnectionExtras` (all of it) | `Kafka.Network.Connection` |
  | `Kafka.Client.ProducerExtras.EnhancedCallback` (+ `noopEnhancedCallback` / `dispatchEnhanced`) | `Kafka.Client.Producer` |
  | `Kafka.Client.ProducerExtras.transactionalIdOptional` | `Kafka.Client.Transaction` |
  | `Kafka.Client.ProducerExtras.TxnErrorRecovery` (+ `classifyTxnError`) | `Kafka.Client.Transaction` |
  | `Kafka.Client.ProducerExtras.TxnDeadline` (+ `effectiveTxnDeadlineMs`) | `Kafka.Client.Transaction` |
  | `Kafka.Client.ShareGroupExtras` (all of it) | `Kafka.Client.ShareConsumer` |

  The five unit-test specs that exercised them are renamed to
  reflect what they're actually testing:
  `Client.AdminExtrasSpec` → `Client.AdminClientConfigSpec`,
  `Client.ConsumerExtrasSpec` → `Client.ConsumerSnapshotsSpec`,
  `Client.ConnectionExtrasSpec` → `Network.ConnectionHelpersSpec`,
  `Client.ProducerExtrasSpec` → `Client.TransactionHelpersSpec`,
  `Client.ShareGroupExtrasSpec` → `Client.ShareConsumerHelpersSpec`.

- `Kafka.Client.Simple` is gone. It was a single-broker,
  single-record helper used as a low-level reference for the
  protocol; everything it did is already covered by the
  higher-level modules:
  - `Simple.createSimpleClient` / `closeSimpleClient` —
    use `Kafka.Network.Connection.connect` /
    `Kafka.Network.Connection.disconnect` (or
    `Conn.withConnection`).
  - `Simple.getMetadata` — use `Kafka.Client.AdminClient.listTopics`
    or `Kafka.Client.AdminClient.describeTopics` (and the
    `MetadataCache` is automatically populated by
    `createProducer` / `createConsumer`).
  - `Simple.produceSimple` — use `Kafka.Client.Producer.sendMessage`.
  - `Simple.fetchSimple` — use `Kafka.Client.Consumer.assign` +
    `Kafka.Client.Consumer.poll`.
  The integration suite has been migrated; only one orphan unit
  test in the old `test/Integration/BasicSpec.hs` referenced it,
  and that file wasn't wired into any test target — it has been
  removed too.
- `Kafka.Telemetry.TraceContext` is gone. It was a hand-rolled
  W3C Trace Context codec that existed because the previous
  `Kafka.Telemetry.OpenTelemetry` module didn't actually use the
  OpenTelemetry SDK; that's been replaced by real
  `hs-opentelemetry-api` usage (see *Changed* below), so the
  hand-rolled codec is now redundant.
- The earlier seven dead span / metric stubs on
  `Kafka.Telemetry.OpenTelemetry` (`createProducerSpan`,
  `createConsumerSpan`, `createTransactionSpan`,
  `recordMessageSent`, `recordMessageReceived`,
  `recordRequestDuration`, `recordBatchSize`) are gone with this
  rewrite.

### Changed

- `Kafka.Telemetry.OpenTelemetry` is rewritten on top of
  [`hs-opentelemetry-api`](https://hackage.haskell.org/package/hs-opentelemetry-api).
  `Tracer`s are real `OTel.Tracer`s, spans are real `OTel.Span`s,
  and the propagator that ships with the configured
  `TracerProvider` (W3C Trace Context by default in the SDK) is
  the one we delegate to for header inject / extract. New
  exports: `kafkaTracer`, `kafkaInstrumentationLibrary`,
  `inProducerSpan`, `inConsumerSpan`, `inTransactionSpan`,
  `producerSpanArguments`, `consumerSpanArguments`,
  `transactionSpanArguments`, `injectIntoProducerHeaders`,
  `extractFromConsumerHeaders`, `tracingProducerInterceptor`.

### Changed

- The 20 `Kafka.Streams.DSL.*` modules are renamed to
  `Kafka.Streams.*` — `Kafka.Streams.DSL.KStream` →
  `Kafka.Streams.KStream`, etc. The `DSL` segment was a
  category, not a domain object, and sat between
  `Kafka.Streams` and the actual user-facing types
  (`KStream`, `KTable`, `StreamsBuilder`, `Consumed`,
  `Produced`, …). Imports of the umbrella `Kafka.Streams`
  module are unchanged; direct imports of
  `Kafka.Streams.DSL.X` must be rewritten to
  `Kafka.Streams.X`.
- `Kafka.Client.Group.GroupConfig` and `GroupConsumer` field
  selectors lose the `gc` prefix — `gcBootstrapBrokers` →
  `bootstrapBrokers`, `gcGroupId` → `groupId`, `gcTopics` →
  `topics`, and so on through every field. This is a breaking
  change for code that constructs `GroupConfig` by record-update
  syntax; the fix is a straight find-and-replace.
- `Kafka.Client.Producer` and `Kafka.Client.Consumer` module
  docstrings have been rewritten as a friendly orientation:
  what's in the module, the recommended starting point, how to
  pick between near-duplicate variants, where the config knobs
  live. Their export lists are reorganised into labelled sections
  so the everyday calls float above the more advanced surface.
- `Kafka.Client.Simple`'s docstring now opens with an explicit
  "this is not the module beginners should reach for, despite the
  name" callout — that module is a single-broker single-record
  reference for conformance / protocol tooling, not a
  high-throughput client.
- README is rewritten as a beginner-aimed on-ramp: what the package
  does in two sentences, a "pick the layer you need" table, and
  concrete recipes for produce / consume (high level) / consume
  (custom poll loop) / streams.
- TUTORIAL.md is re-ordered to start from `withProducer` +
  `runConsumer` against a live broker and progressively reveal the
  mock cluster, transactions, streams, state stores, multi-instance
  routing, schema registry, and observability.

- **TLS offload support** for the broker connection layer
  (`Kafka.Network.TlsOffload`, new field
  `Kafka.Network.Connection.connTlsOffload`). When set, the
  client skips its own `crypton-connection` TLS handshake and
  routes every broker socket through a sidecar / Unix-domain
  socket / kTLS-style endpoint, regardless of the value of
  `connUseTls`. Four deployment shapes are supported out of the
  box:
  - `transparentTlsOffload` (kTLS / NLB / TPROXY — connect to
    the broker's advertised address, cipher work happens
    out-of-band);
  - `staticTlsOffload` (every broker connection routes to one
    fixed TCP or Unix-domain endpoint — the standard Envoy /
    `kafka-proxy` shape);
  - `perBrokerTlsOffload` (per-broker endpoint map — the
    standard stunnel-with-port-per-broker shape used against
    MSK);
  - `customTlsOffload` (arbitrary `OffloadBrokerKey -> IO
    (Maybe TlsOffloadEndpoint)` resolver — for service-mesh /
    control-plane-discovered sidecars).
  The connection pool is still keyed by the logical broker
  address so per-broker SASL state and request pipelining are
  preserved when several brokers fan in to the same sidecar
  socket. New test suite `Network.TlsOffloadSpec` covers each
  mode end-to-end with in-process TCP and UDS sidecars.
- `Kafka.Network.Connection.connectOffload`: public,
  retry-aware single-broker form of the offload path for
  callers that don't need a `ConnectionManager`.

### Changed

- Vendored Apache Kafka **4.0.0** protocol JSON schemas
  (`data/kafka-protocol-schemas/`) and regenerated every
  `Kafka.Protocol.Generated.*` module. Removes 4.1+-only message
  types (`AlterShareGroupOffsets*`, `DeleteShareGroupOffsets*`,
  `DescribeShareGroupOffsets*`, `StreamsGroupHeartbeat*`,
  `StreamsGroupDescribe*`) and renames `ListConfigResources*` back
  to `ListClientMetricsResources*` (its 4.0.0 spelling — same
  apiKey 74).
- `Kafka.Protocol.Codegen.Types.FieldSpec` parses `tag` from either
  a JSON number (post-4.0 trunk style) or a numeric string (the
  4.0.0 release ships `"tag": "0"`), so the codegen is tolerant of
  either upstream encoding.
- High-level client call sites that hand-built request records lost
  the post-4.0 fields they no longer need to populate
  (`apiVersionsRequestClusterId`/`NodeId` (KIP-1242),
  `topicProduceDataTopicId`/`offsetCommitRequestTopicTopicId`/
  `offsetFetchRequestTopicsTopicId`/`txnOffsetCommitRequestTopicTopicId`
  (KIP-516 / KIP-848 v6/v10 topic-id additions),
  `fetchPartitionHighWatermark` (KIP-405),
  `initProducerIdRequestEnable2Pc`/`KeepPreparedTxn` (KIP-939),
  `txnOffsetCommitRequestGenerationIdOrMemberEpoch` rename).
- Integration `docker-compose.yml` and the bench / tutorial docs
  bumped from `apache/kafka:3.7.0` / `kafka_2.13-3.9.2` references
  to `apache/kafka:4.0.0` / `kafka_2.13-4.0.0`.
