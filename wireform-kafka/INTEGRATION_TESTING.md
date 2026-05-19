# Integration Testing Guide

Use this when you want the live-broker suite, not the in-process mock tests. The suite expects Kafka in KRaft mode and exercises the client against real sockets, metadata, transactions, and topic management.

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
# Stop Kafka and clean up data directories
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

## Test organization

Integration tests live under `test/Integration/` and are registered
from `test/IntegrationSpec.hs`. Prefer adding a focused spec next to
the existing live-broker coverage rather than building a second test
harness.

## Continuous integration

`.github/workflows/wireform-kafka-integration.yml` is the reference
shape: start an `apache/kafka` KRaft broker with Docker Compose, wait
for readiness, then run the integration suites with
`WIREFORM_KAFKA_BROKER` set. Keep local scripts aligned with that
workflow so failures reproduce cleanly.

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

## Writing new integration tests

When adding live-broker coverage:

1. Add the spec under `test/Integration/`.
2. Register it in `test/IntegrationSpec.hs`.
3. Use unique topic names, preferably with the test name in the prefix.
4. Clean up topics and close clients in bracketed setup/teardown.
5. Keep network timeouts explicit; live Kafka failures should fail with
   a useful message, not hang the suite.

Use the existing specs as templates; they already carry the right
setup and cleanup shape.

