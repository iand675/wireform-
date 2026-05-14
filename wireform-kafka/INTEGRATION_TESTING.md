# Integration Testing Guide

This document describes how to run integration tests for the wireform-kafka library.

## Prerequisites

The integration tests require a running Kafka cluster. The Nix flake provides helper scripts to start a local Kafka instance for testing.

### Using Nix (Recommended)

If you have Nix with flakes enabled, the development environment includes everything you need:

```bash
# Enter the Nix development shell
nix develop

# Or, if you use direnv, just cd into the project:
# direnv allow
cd /path/to/wireform/wireform-kafka
```

## Starting Kafka

### Option 1: Using the Nix helper script (easiest)

```bash
# Start Kafka in KRaft mode
start-kafka

# This will:
# - Start Kafka (no Zookeeper needed!)
# - Listen on localhost:9092
# - Wait for Kafka to be ready
# - Print process ID and log location
```

### Option 2: Manual startup

If not using Nix, you'll need to manually install and start Kafka:

```bash
# Download Kafka 4.0+ from https://kafka.apache.org/downloads
# Extract and run in KRaft mode:
cd kafka_2.13-4.x.x

# Generate a cluster ID
KAFKA_CLUSTER_ID="$(bin/kafka-storage.sh random-uuid)"

# Format log directories
bin/kafka-storage.sh format -t $KAFKA_CLUSTER_ID -c config/kraft/server.properties

# Start Kafka
bin/kafka-server-start.sh config/kraft/server.properties &
```

## Running Integration Tests

### Using the helper script

```bash
# This checks if Kafka is running and runs the integration tests
run-integration-tests
```

### Manual execution

```bash
# Build and run the integration test suite
cabal test wireform-kafka:wireform-kafka-integration

# Run with verbose output
cabal test wireform-kafka:wireform-kafka-integration --test-arguments='--verbose'

# Run only specific tests
cabal test wireform-kafka:wireform-kafka-integration --test-arguments='--pattern "Connection"'
```

## Managing Test Topics

### Create a test topic

```bash
# Create a topic with default settings (3 partitions, replication factor 1)
create-test-topic my-test-topic

# Create a topic with custom settings
create-test-topic my-topic 5 1  # 5 partitions, replication factor 1
```

### List topics

```bash
list-topics
```

### Using kafka CLI tools directly

The Nix environment includes the full Apache Kafka distribution:

```bash
# Describe a topic
kafka-topics.sh --bootstrap-server localhost:9092 --describe --topic my-topic

# Delete a topic
kafka-topics.sh --bootstrap-server localhost:9092 --delete --topic my-topic

# Produce test messages
kafka-console-producer.sh --bootstrap-server localhost:9092 --topic my-topic

# Consume messages
kafka-console-consumer.sh --bootstrap-server localhost:9092 --topic my-topic --from-beginning
```

## Stopping Kafka

```bash
# Stop both Kafka and Zookeeper, and clean up data directories
stop-kafka
```

## Troubleshooting

### Kafka fails to start

Check the logs:
```bash
tail -f /tmp/kafka-kraft.log
```

Common issues:
- Port 9092 or 9093 already in use - stop existing Kafka processes
- Insufficient disk space in /tmp
- Previous unclean shutdown - run `stop-kafka` to clean up

### Tests fail with connection errors

1. Verify Kafka is running:
   ```bash
   # Should show connection success
   nc -zv 127.0.0.1 9092
   ```

2. Check broker logs for errors:
   ```bash
   tail -f /tmp/kafka.log
   ```

3. Ensure the broker is advertising the correct address:
   ```bash
   # The advertised.listeners should be localhost:9092
   grep advertised.listeners /tmp/kafka-logs/meta.properties
   ```

### Tests are slow

Integration tests involve real network I/O and Kafka operations, so they're naturally slower than unit tests. However, if they're unusually slow:

- Check if Kafka logs show errors or warnings
- Verify no other processes are overwhelming the system
- Consider running unit tests separately: `cabal test wireform-kafka:wireform-kafka-test`

## Test Organization

Integration tests are in `test/Integration/`:

- `BasicSpec.hs` - Connection, metadata, basic produce/consume tests
- Future: `ProducerSpec.hs` - Producer-specific integration tests
- Future: `ConsumerSpec.hs` - Consumer-specific integration tests
- Future: `TransactionSpec.hs` - Transaction coordinator tests

## Continuous Integration

For CI environments, consider:

1. Using Docker to run Kafka in KRaft mode:
   ```bash
   docker run -d -p 9092:9092 apache/kafka:latest
   ```

2. Using Testcontainers (requires Docker and supports KRaft mode)

3. Setting appropriate timeouts for CI environments

## Development Workflow

Typical development workflow:

```bash
# 1. Enter development environment
nix develop

# 2. Start Kafka
start-kafka

# 3. Run tests during development
run-integration-tests

# Or run in watch mode with ghcid:
ghcid --command "stack ghci wireform-kafka:wireform-kafka-integration" \
      --test "main"

# 4. When done, stop Kafka
stop-kafka
```

## Writing New Integration Tests

When adding new integration tests:

1. Add test modules to `test/Integration/`
2. Import and register them in `test/IntegrationSpec.hs`
3. Ensure tests clean up after themselves (delete test topics, etc.)
4. Use unique topic names to avoid conflicts between tests
5. Add appropriate timeout handling for network operations
6. Document any special setup requirements

Example test template:

```haskell
module Integration.MyNewSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "My New Feature"
  [ testCase "Does something useful" testMyFeature
  ]

testMyFeature :: Assertion
testMyFeature = do
  -- 1. Setup (create topics, prepare data)
  -- 2. Execute operation
  -- 3. Verify results
  -- 4. Cleanup (delete topics, close connections)
  return ()
```
