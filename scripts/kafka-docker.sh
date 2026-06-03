#!/usr/bin/env bash
# Start/stop the wireform-kafka integration-test broker (KRaft, localhost:9092).
# Mirrors the readiness gates in .github/workflows/wireform-kafka-integration.yml.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPOSE_FILE="${ROOT}/wireform-kafka/test-integration/docker-compose.yml"

docker_cmd() {
  if docker info >/dev/null 2>&1; then
    docker "$@"
  else
    sudo docker "$@"
  fi
}

COMPOSE=(docker_cmd compose -f "${COMPOSE_FILE}")
DC=("${COMPOSE[@]}" exec -T kafka)
KAFKA_TOPICS=/opt/kafka/bin/kafka-topics.sh
KAFKA_TXN=/opt/kafka/bin/kafka-transactions.sh
BOOTSTRAP=localhost:9092

usage() {
  cat <<'EOF'
Usage: scripts/kafka-docker.sh <command>

Commands:
  start       docker compose up -d, then wait until the broker is test-ready
  stop        docker compose down -v
  status      show container health and whether :9092 answers
  wait-ready  block until CI-style readiness (no compose up)

After start:
  export WIREFORM_KAFKA_BROKER=localhost:9092
  cabal test wireform-kafka:wireform-kafka-integration \
             wireform-kafka:wireform-kafka-streams-integration
EOF
}

wait_stage1() {
  echo "==> Stage 1: broker accepts kafka-topics --list"
  for i in $(seq 1 30); do
    if "${DC[@]}" "${KAFKA_TOPICS}" --bootstrap-server "${BOOTSTRAP}" --list >/dev/null 2>&1; then
      echo "    ready (${i})"
      return 0
    fi
    sleep 2
  done
  echo "Stage 1 timed out" >&2
  return 1
}

wait_stage2() {
  echo "==> Stage 2: transaction coordinator"
  for i in $(seq 1 30); do
    if "${DC[@]}" "${KAFKA_TXN}" --bootstrap-server "${BOOTSTRAP}" list >/dev/null 2>&1; then
      echo "    ready (${i})"
      return 0
    fi
    sleep 2
  done
  echo "Stage 2 timed out" >&2
  return 1
}

wait_stage3() {
  echo "==> Stage 3: partition leadership propagation"
  local probe=wireform-kafka-readiness-probe
  "${DC[@]}" "${KAFKA_TOPICS}" --bootstrap-server "${BOOTSTRAP}" \
    --create --if-not-exists --topic "${probe}" \
    --partitions 1 --replication-factor 1 >/dev/null
  for i in $(seq 1 30); do
    leader=$("${DC[@]}" "${KAFKA_TOPICS}" --bootstrap-server "${BOOTSTRAP}" \
      --describe --topic "${probe}" 2>/dev/null \
      | awk '/Leader: [0-9]+/ {for (j=1; j<=NF; j++) if ($j == "Leader:") print $(j+1)}' \
      | head -n 1)
    if [ -n "${leader}" ] && [ "${leader}" != "-1" ] && [ "${leader}" != "none" ]; then
      echo "    partition leader = ${leader}; ready (${i})"
      "${DC[@]}" "${KAFKA_TOPICS}" --bootstrap-server "${BOOTSTRAP}" \
        --delete --topic "${probe}" >/dev/null 2>&1 || true
      return 0
    fi
    sleep 2
  done
  echo "Stage 3 timed out" >&2
  return 1
}

precreate_test_topics() {
  echo "==> Pre-creating integration test topics"
  local topics=(
    wireform-kafka-txn-source
    wireform-kafka-txn-sink
    kafka-native-integration-test
    wireform-bench-cmp
    payments.transactions
    payments.risk-features
    payments.bookkeeping-entries
  )
  for topic in "${topics[@]}"; do
    "${DC[@]}" "${KAFKA_TOPICS}" --bootstrap-server "${BOOTSTRAP}" \
      --create --if-not-exists --topic "${topic}" \
      --partitions 1 --replication-factor 1 >/dev/null
    # Wait for a real leader (auto-create alone can race the Haskell client).
    for _ in $(seq 1 30); do
      leader=$("${DC[@]}" "${KAFKA_TOPICS}" --bootstrap-server "${BOOTSTRAP}" \
        --describe --topic "${topic}" 2>/dev/null \
        | awk '/Leader: [0-9]+/ {for (j=1; j<=NF; j++) if ($j == "Leader:") print $(j+1)}' \
        | head -n 1)
      if [ -n "${leader}" ] && [ "${leader}" != "-1" ] && [ "${leader}" != "none" ]; then
        break
      fi
      sleep 1
    done
  done
}

wait_ready() {
  wait_stage1
  wait_stage2
  wait_stage3
  precreate_test_topics
  echo "Kafka is ready at ${BOOTSTRAP}"
}

cmd_start() {
  "${COMPOSE[@]}" up -d
  # Healthcheck in compose is a coarse gate; CI runs the stages below.
  wait_ready
}

cmd_stop() {
  "${COMPOSE[@]}" down -v
}

cmd_status() {
  "${COMPOSE[@]}" ps
  if command -v ss >/dev/null 2>&1; then
    ss -ltn | rg ':9092\b' || true
  fi
  if docker inspect wireform-kafka-it --format '{{.State.Health.Status}}' 2>/dev/null; then
    :
  fi
}

main() {
  case "${1:-}" in
    start) cmd_start ;;
    stop) cmd_stop ;;
    status) cmd_status ;;
    wait-ready) wait_ready ;;
    -h|--help|help|"") usage ;;
    *) echo "Unknown command: $1" >&2; usage >&2; exit 1 ;;
  esac
}

main "$@"
