# `wireform-kafka` tutorial

A guided walkthrough from the client basics to a working producer,
consumer, transaction, and Streams topology. The first client
examples use a broker at `localhost:9092`; the mock-broker and
Streams sections show how to work without Docker when you want fast
local tests.

> **Where this fits.** The [README](./README.md) is the
> catalogue of features; [`CONCEPTS.md`](./CONCEPTS.md) is the
> plain-language Kafka primer;
> [`streams/README.md`](./streams/README.md) is the Streams DSL
> reference; this file is the "first 30 minutes".

## 1. Send a record (`withProducer`)

The smallest possible Kafka writer:

```haskell
import qualified Kafka

main :: IO ()
main =
  Kafka.withProducer ["localhost:9092"] Kafka.defaultProducerConfig $ \p -> do
    md <- Kafka.sendMessage p "events" Nothing "hello"
    print md
```

`withProducer` is a `Control.Exception.bracket`: it builds the
`Producer`, runs your body, and on the way out flushes anything
buffered and closes the connection — even if you throw.

`sendMessage` returns `IO (Either String RecordMetadata)`. The
`Right` carries the assigned partition and offset; the `Left`
is a typed error message ready to log. For fire-and-forget,
use `sendMessage_`. For non-blocking with a result you read
later, use `sendMessageAsync` (returns an `MVar` you take when
ready).

## 2. Receive records (`runConsumer`)

The smallest possible Kafka reader. Joins a consumer group,
calls your handler once per record, commits offsets, and
leaves the group on exit:

```haskell
import qualified Kafka
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main =
  Kafka.runConsumer
    Kafka.defaultGroupConfig
      { Kafka.bootstrapBrokers = ["localhost:9092"]
      , Kafka.groupId          = "tutorial"
      , Kafka.topics           = ["events"]
      }
    $ \rec ->
        BS.putStrLn rec.value
```

For higher throughput, use `runBatchedConsumer` — same shape,
but the handler receives a `Vector ConsumerRecord` per call
and a single commit covers the whole batch.

### Error handling

If your handler throws, `runConsumer` consults `onError`:

  * `LogAndRaise` (default) — log to `stderr` and re-raise.
  * `SkipRecord` — log and keep going.
  * `StopLoop` — log and exit cleanly.
  * `CustomError pred` — your own predicate.

### Commit modes

`commitMode` on `GroupConfig`:

  * `CommitSync` (default) — commit after each successful
    handler call. Smallest possible duplicate window on a
    crash.
  * `CommitAsync` — fire-and-forget commit.
  * `CommitManual` — you call `commitSync` / `commitAsync`
    yourself.

## 3. Custom poll loop (`withConsumer`)

If `runConsumer`'s "one handler per record" shape doesn't fit
your control flow, drop down to `Kafka.Client.Consumer` and
own the loop:

```haskell
import qualified Kafka.Client.Consumer as Consumer
import Control.Monad (forever)

main :: IO ()
main =
  Consumer.withConsumer
    ["localhost:9092"] "tutorial"
    Consumer.defaultConsumerConfig
    ["events"]
    $ \c -> forever $ do
        r <- Consumer.poll c 1000
        case r of
          Left err   -> putStrLn ("poll failed: " <> err)
          Right recs -> do
            mapM_ print recs
            _ <- Consumer.commitSync c
            pure ()
```

`withConsumer` joins the group and subscribes for you; on the
way out it commits, sends `LeaveGroup`, and closes connections.

## 4. The in-process mock broker

The full client expects a real Kafka cluster. For unit tests
and learning, use the in-process mock broker — it speaks the
same Producer / Consumer API but lives entirely inside your
process:

```haskell
import Kafka.Client.Mock.Cluster
import Kafka.Client.Mock.Producer
import Kafka.Client.Mock.Consumer
import Kafka.Client.Mock.Fault
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  cluster <- newMockCluster 1            -- one broker
  createTopic cluster "events" 3         -- one topic, three partitions

  faults <- noFaults
  producer <- newMockProducer cluster faults Nothing
  _ <- sendMock producer "events" 0
         (Just (BS.pack "key")) (BS.pack "hello") 0
  putStrLn "produced"

  consumer <- newMockConsumer cluster faults
                              (GroupId "tutorial") ReadUncommitted 100
  subscribeMC consumer ["events"]
  PollResult records _ <- pollMC consumer
  print (length records)
```

The mock cluster is the workhorse for unit tests in this package:
same client shape, no broker process, and deterministic fault
injection when a test needs it.

## 5. Transactions

Transactions group multiple sends across multiple partitions
into one atomic write. Combined with
`commitOffsetsInTransaction`, they give end-to-end "exactly
once".

The lifecycle is split between `Kafka.Client.Transaction`
(state machine + coordinator wire) and `Kafka.Client.Producer`
(binding to a producer):

```haskell
import qualified Kafka
import qualified Kafka.Client.Transaction as T
import qualified Kafka.Network.Connection as Conn
import qualified Kafka.Protocol.ApiVersions as AV

main :: IO ()
main = do
  let txId = "tutorial-txn-1"
  Kafka.withProducer ["localhost:9092"]
    Kafka.defaultProducerConfig
      { Kafka.producerTransactional = Just txId
      , Kafka.producerIdempotent    = True
      }
    $ \p -> do
        connMgr <- Conn.createConnectionManager
        vCache  <- AV.createVersionCache
        txn <- T.createTransaction
                 (T.TransactionalId txId) connMgr vCache "tutorial-client"
                 (Conn.BrokerAddress "localhost" 9092) 60_000
        Right () <- T.initTransactions txn
        Kafka.bindTransaction p txn

        Right () <- T.beginTransaction txn
        _ <- Kafka.sendMessage p "events" Nothing "in-txn"
        Right () <- T.commitTransaction txn
        pure ()
```

Compared with a plain producer, the bound transaction controls when
sends are allowed and stamps every outgoing batch with the producer
id, epoch, sequence, and transactional bit. `withProducer` also
cleans up correctly: closing an open transactional producer aborts
before shutdown.

## 6. A first Streams topology

The Streams DSL builds a topology of stream operators and
runs it against a real broker (or the in-process test
driver). Combinators mirror the Java DSL one for one.

```haskell
import qualified Kafka.Streams                       as S
import qualified Kafka.Streams.StreamsBuilder    as SB
import qualified Kafka.Streams.KStream           as KS
import qualified Kafka.Streams.Serde                 as Serde

main :: IO ()
main = do
  let topology =
        SB.runStreamsBuilder $ do
          input <- SB.streamFromTopic "events"
                     Serde.bytesSerde Serde.bytesSerde
          KS.foreachStream
            input
            (\k v -> putStrLn ("got " <> show k <> "=" <> show v))
  print topology
```

The DSL is documented end-to-end in
[`streams/README.md`](./streams/README.md); see
`wireform-kafka/streams/examples` for runnable demos of every
operator family.

### Side effects: blocking vs async

`KStream.foreachStream` is the blocking terminal effect — the
worker thread waits for the callback to return. For metrics
emissions / logging where ordering doesn't matter, use
`foreachStreamAsync`:

```haskell
KS.foreachStreamAsync
  (\r -> Metrics.emit ("processed-" <> recordValue r))
  someStream
```

Each callback forks via `Control.Concurrent.Async` so a slow
sink can't back-pressure the worker.

## 7. State stores and exactly-once transactional writes

`Kafka.Streams.State.Transactional` wraps any
`KeyValueStore` so that puts and deletes are buffered until
the producer transaction commits:

```haskell
import qualified Kafka.Streams.State.KeyValue.InMemory as Mem
import qualified Kafka.Streams.State.Transactional     as TX
import qualified Kafka.Streams.State.Store             as Store

main :: IO ()
main = do
  underlying <- Mem.inMemoryKeyValueStore (Store.storeName "totals")
  txStore    <- TX.newTransactionalStore underlying
  let store = TX.txnStore txStore
  Store.kvsPut store "k" "v"
  Just "v" <- Store.kvsGet store "k"        -- read-your-writes
  Nothing  <- Store.kvsGet underlying "k"   -- nothing applied yet
  TX.txnCommit txStore                      -- commit drains
  Just "v" <- Store.kvsGet underlying "k"
  pure ()
```

Wire it into the engine's commit cycle via
`Kafka.Streams.Runtime.EOS.withTransactionalStores`. The
runtime runs the producer commit FIRST; on success the store
commits drain in declaration order. An abort runs the
producer abort + the store aborts so the buffered writes are
discarded and the store stays consistent with the broker-side
log.

## 8. Multi-instance Streams

A multi-instance Streams app needs three things to line up: partition
movement hooks, standby tasks for fast failover, and a query layer
that routes each key to the instance that owns it. The runtime exposes
those pieces directly; the full operational treatment lives in
`streams/README.md` and the website operations docs.

## 9. Schema Registry serdes

`Kafka.Streams.Serde.SchemaRegistry` exposes the Confluent wire
envelope (`magicByte + schemaId + payload`) plus a pluggable
`SchemaRegistryClient` interface. Use `inMemoryRegistry` in
tests and your own HTTP client in production:

```haskell
import qualified Kafka.Streams.Serde.SchemaRegistry as SR

main :: IO ()
main = do
  client <- SR.inMemoryRegistry
  Right sid <- SR.srRegister client (SR.SchemaSubject "events-value")
                              (SR.SchemaPayload "{\"type\":\"string\"}")
  let bs = SR.encodeEnvelope sid "hello"
  case SR.decodeEnvelope bs of
    Right (sid', payload) -> do
      print sid'         -- SchemaId 1
      print payload      -- "hello"
    Left err -> error err
```

## 10. Observability

`Kafka.Telemetry.StatsJson` mirrors the
[librdkafka stats JSON shape](https://github.com/confluentinc/librdkafka/blob/master/STATISTICS.md)
so dashboards written against the C client port over without
rewrites:

```haskell
import qualified Kafka.Telemetry.StatsJson as Stats
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  let snap = (Stats.defaultSnapshot "wfkafka" "client-1" Stats.StatsProducer)
        { Stats.ssMsgCount = 100
        , Stats.ssTopics = Map.singleton "events"
            (Stats.TopicStats "events" 100 5 1024 0 0)
        }
  print (Stats.renderStats snap)
```

OTel spans / metrics flow through
`Kafka.Telemetry.OpenTelemetry`; the JSON snapshot above is
the librdkafka-shaped mirror of the same counters.

## 11. Where to look next

- [`CONFIG_PARITY.md`](./CONFIG_PARITY.md) for the librdkafka-style
  config mapping.
- [`INTEGRATION_TESTING.md`](./INTEGRATION_TESTING.md) for the live
  broker test workflow.
- [`src/Kafka/Client/Mock/README.md`](./src/Kafka/Client/Mock/README.md)
  for mock broker and fault-injection primitives.
- [`streams/README.md`](./streams/README.md) for the Streams DSL and
  runtime reference.
- [`streams/examples/README.md`](./streams/examples/README.md) for the
  runnable Streams examples.
