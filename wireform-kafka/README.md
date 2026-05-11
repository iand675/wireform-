# kafka-native

A comprehensive Kafka client library for Haskell with full protocol support and modern features.

## Features

- **Full Protocol Support**: Comprehensive implementation of the Kafka wire protocol supporting all API versions
- **Code Generation**: Protocol messages generated from official Kafka definitions for accuracy and completeness
- **Security**: TLS/SSL encryption and SASL authentication (PLAIN, SCRAM-SHA-256, SCRAM-SHA-512)
- **Compression**: Multiple compression codecs (Gzip, LZ4, Zstd)
- **High Performance**: Request pipelining for maximum throughput
- **Producer API**: High-level producer with batching, partitioning, and delivery guarantees
- **Consumer API**: Full consumer group support with automatic rebalancing
- **Transactions**: Exactly-once semantics with transactional producer/consumer
- **Observability**: OpenTelemetry instrumentation following semantic conventions
- **Type Safety**: Leverages Haskell's type system to prevent errors at compile time

## Installation

### Using Stack

```bash
git clone https://github.com/yourusername/kafka-native.git
cd kafka-native
stack build
```

### Using Cabal

```bash
git clone https://github.com/yourusername/kafka-native.git
cd kafka-native
cabal build
```

## Quick Start

### Producer Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Kafka.Client.Producer

main :: IO ()
main = do
  result <- createProducer ["localhost:9092"] defaultProducerConfig
  case result of
    Left err -> putStrLn $ "Failed to create producer: " ++ err
    Right producer -> do
      -- Send a simple message
      sendResult <- sendMessage producer "my-topic" Nothing "Hello, Kafka!"
      case sendResult of
        Left err -> putStrLn $ "Send failed: " ++ err
        Right metadata -> do
          putStrLn $ "Message sent to partition " ++ show (metadataPartition metadata)
          putStrLn $ "  at offset " ++ show (metadataOffset metadata)
      
      closeProducer producer
```

### Consumer Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Kafka.Client.Consumer
import Control.Monad (forever)

main :: IO ()
main = do
  result <- createConsumer ["localhost:9092"] "my-group" defaultConsumerConfig
  case result of
    Left err -> putStrLn $ "Failed to create consumer: " ++ err
    Right consumer -> do
      -- Subscribe to topics
      subscribe consumer ["my-topic"]
      
      -- Poll for messages
      forever $ do
        records <- poll consumer 1000
        case records of
          Right recs -> do
            mapM_ processRecord recs
            commitSync consumer
          Left err -> putStrLn $ "Poll error: " ++ err

processRecord :: ConsumerRecord -> IO ()
processRecord record = do
  putStrLn $ "Received: " ++ show (crValue record)
  putStrLn $ "  from partition " ++ show (crPartition record)
  putStrLn $ "  at offset " ++ show (crOffset record)
```

### Transactional Producer Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Kafka.Client.Producer
import Kafka.Client.Transaction

main :: IO ()
main = do
  result <- createProducer ["localhost:9092"] config { producerTransactional = Just "my-txn-id" }
  case result of
    Left err -> putStrLn $ "Failed: " ++ err
    Right producer -> do
      txnResult <- initTransactions producer "my-txn-id"
      case txnResult of
        Left err -> putStrLn $ "Transaction init failed: " ++ err
        Right txn -> do
          result <- withTransaction txn $ \t -> do
            sendInTransaction t "output-topic" Nothing "Message 1"
            sendInTransaction t "output-topic" Nothing "Message 2"
            -- Transaction commits automatically if no exception
          case result of
            Left err -> putStrLn $ "Transaction failed: " ++ err
            Right _ -> putStrLn "Transaction committed successfully"
```

## Architecture

The library is organized into several layers:

### Protocol Layer (`Kafka.Protocol.*`)

- **Primitives**: Base types (VarInt, strings, arrays, tagged fields)
- **Encoding**: Version-aware serialization/deserialization
- **Generated**: Auto-generated message types from Kafka protocol definitions
- **ApiVersions**: API key definitions and version negotiation

### Network Layer (`Kafka.Network.*`)

- **Connection**: TCP/TLS connection management and pooling
- **Auth**: SASL authentication framework
  - **Plain**: SASL/PLAIN username/password authentication
  - **Scram**: SCRAM-SHA-256 and SCRAM-SHA-512 challenge-response

### Compression Layer (`Kafka.Compression.*`)

- **Gzip**: RFC 1952 gzip compression
- **Lz4**: LZ4 frame format compression
- **Zstd**: Zstandard compression (recommended)
- Snappy support pending library evaluation

### Client Layer (`Kafka.Client.*`)

- **Producer**: High-level producer API with batching and partitioning
- **Consumer**: Consumer group API with automatic rebalancing
- **Transaction**: Transactional producer for exactly-once semantics
- **Pipeline**: Request pipelining for high throughput

### Telemetry Layer (`Kafka.Telemetry.*`)

- **OpenTelemetry**: Distributed tracing and metrics following Kafka semantic conventions

## Code Generation

The library uses code generation to ensure protocol accuracy:

```bash
# Generate protocol message types
stack exec kafka-codegen kafka/clients/src/main/resources/common/message src/Kafka/Protocol/Generated
```

This reads Kafka's official JSON protocol definitions and generates:
- Data types with strict fields
- Serial instances for each API version
- Comprehensive Haddock documentation

## Configuration

### Producer Configuration

```haskell
import Kafka.Client.Producer
import Kafka.Compression.Types

myProducerConfig :: ProducerConfig
myProducerConfig = defaultProducerConfig
  { producerClientId = "my-app-producer"
  , producerCompression = Zstd              -- Use Zstd compression
  , producerBatchSize = 32768               -- 32KB batch size
  , producerLingerMs = 10                   -- Wait 10ms for batching
  , producerDelivery = AtLeastOnce          -- At-least-once delivery
  , producerIdempotent = True               -- Enable idempotence
  }
```

### Consumer Configuration

```haskell
import Kafka.Client.Consumer

myConsumerConfig :: ConsumerConfig
myConsumerConfig = defaultConsumerConfig
  { consumerClientId = "my-app-consumer"
  , consumerGroupId = "my-consumer-group"
  , consumerAutoCommit = False              -- Manual commit control
  , consumerMaxPollRecords = 1000           -- Fetch up to 1000 records
  , consumerAssignmentStrategy = StickyAssignment  -- Minimize rebalance
  , consumerAutoOffsetReset = Earliest      -- Start from earliest on new group
  }
```

### Connection Configuration with TLS

```haskell
import Kafka.Network.Connection
import qualified Network.TLS as TLS

myConnectionConfig :: ConnectionConfig
myConnectionConfig = defaultConnectionConfig
  { connUseTls = True
  , connTlsSettings = Just $ defaultTlsSettings "broker.example.com"
  , connTimeout = 30
  }
```

### Linux io_uring backend (optional, opt-in)

When built with `-fio-uring`, the client exposes a
`Kafka.Network.IoUring` module that replaces the standard
`recv` / `send` syscall pair with a single `io_uring_enter`
per round-trip on Linux 5.1+. The shim uses raw syscalls
(no `liburing` dependency) and mmaps the SQ/CQ rings
directly; on non-Linux hosts a stub is compiled instead so
the FFI symbols stay linkable.

```haskell
import qualified Kafka.Network.IoUring as IoUring
import qualified Kafka.Network.Transport as Transport
import qualified Network.Socket as Socket

-- Runtime probe: hardened kernels and containers with
-- 'kernel.io_uring_disabled=2' will return False.
ok <- IoUring.isIoUringSupported
if ok
  then IoUring.withIoUringRing 8 $ \ring -> do
    -- 'sock' is a connected TCP / UDS socket — e.g., the
    -- one a TLS-offload sidecar listener handed us.
    transport <- IoUring.mkIoUringTransport ring sock "iour:broker-1"
    -- 'transport' is a drop-in 'Transport' implementation;
    -- read/write through it goes via io_uring.
    pure transport
  else
    -- Kernel says no — fall back to a regular transport.
    ...
```

Build and test with:

```bash
cabal build -fio-uring wireform-kafka
cabal test  -fio-uring wireform-kafka:wireform-kafka-test
```

## Testing

The library includes comprehensive tests using Hedgehog for property-based testing:

```bash
# Run all tests
stack test

# Run specific test suite
stack test kafka-native:test:kafka-native-test
```

Tests cover:
- Protocol primitive round-trip serialization
- Compression/decompression verification
- Generated message serialization (TODO: after code generation)

## Development Status

### Completed

- ✅ Protocol primitives (VarInt, strings, arrays, tagged fields)
- ✅ Encoding/decoding framework with version support
- ✅ Code generator for protocol messages
- ✅ TCP/TLS connection management
- ✅ SASL authentication (PLAIN, SCRAM)
- ✅ Compression codecs (Gzip, LZ4, Zstd)
- ✅ Request pipelining framework
- ✅ Producer API structure
- ✅ Consumer API structure
- ✅ Transaction API structure
- ✅ OpenTelemetry instrumentation hooks
- ✅ Hedgehog property-based test framework

### In Progress / TODO

The codebase contains extensive TODOs marking implementation points. Key areas:

1. **Protocol Message Generation**: Run code generator to create all ~70 message types
2. **Serialization Logic**: Implement version-aware encode/decode for generated messages
3. **Network Implementation**: Complete connection pooling and request/response handling
4. **Producer Logic**: Implement batching, metadata caching, and sender thread
5. **Consumer Logic**: Implement group coordination, partition assignment, and rebalancing
6. **Transaction Logic**: Implement coordinator interaction and transaction state machine
7. **SCRAM Details**: Complete PBKDF2 key derivation and proof calculation
8. **Pipeline Threads**: Implement background threads for send/receive/timeout
9. **Integration Tests**: Add end-to-end tests with Docker-based Kafka cluster

Each TODO includes detailed comments about what needs to be implemented and how.

## Contributing

Contributions are welcome! Please see the TODOs in the source code for areas that need implementation.

Key areas where contributions would be valuable:
- Completing the protocol message serialization logic
- Implementing the background threads for pipelining
- Adding integration tests
- Performance optimization
- Documentation improvements

## License

BSD-3-Clause

## References

- [Apache Kafka Protocol Guide](https://kafka.apache.org/protocol.html)
- [OpenTelemetry Semantic Conventions for Messaging](https://opentelemetry.io/docs/specs/semconv/messaging/kafka/)
- [SASL SCRAM RFC 5802](https://tools.ietf.org/html/rfc5802)
- [SASL PLAIN RFC 4616](https://tools.ietf.org/html/rfc4616)

## Acknowledgments

This implementation draws inspiration from:
- [kafka-protocol-rs](https://github.com/tychedelia/kafka-protocol-rs) - Rust Kafka protocol implementation
- Franz (minimal Kafka client by @Diggsey)
- Official Kafka Java client

The code generation approach is directly inspired by kafka-protocol-rs's methodology of generating code from Kafka's official protocol definitions.
