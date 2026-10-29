# Interop pipeline: gRPC cross-language, Kafka live-broker, WebSocket Autobahn.
#
# These tests require external services (Docker containers, reference
# gRPC servers, Autobahn fuzzer) on the nix queue with docker=true.
{ lib
, changedFiles ? []
}:

let
  ci       = import ../dsl.nix { inherit lib; };
  render   = import ../render.nix { inherit lib; };
  packages = import ../packages.nix { inherit lib; };
  changes  = import ../changes.nix { inherit lib packages; };

  affected = changes.affectedPackages changedFiles;
  defaultGHC = "ghc910";

  inherit (ci) nixAgents;
  dockerAgents = nixAgents // { docker = "true"; };

  keyOf = name: builtins.replaceStrings [ "-" ] [ "_" ] name;

  grpcAffected =
    builtins.hasAttr "wireform-grpc" affected
    || builtins.hasAttr "grpc-spec" affected
    || builtins.hasAttr "wireform-proto" affected
    || builtins.hasAttr "wireform-http2" affected;

  kafkaAffected =
    builtins.hasAttr "wireform-kafka" affected
    || builtins.hasAttr "wireform-kafka-protocol" affected;

  websocketAffected =
    builtins.hasAttr "wireform-websocket" affected
    || builtins.hasAttr "wireform-http" affected
    || builtins.hasAttr "wireform-core" affected;

  # ------------------------------------------------------------------
  # gRPC interop
  # ------------------------------------------------------------------
  grpcSelfTest = ci.command {
    label = ":satellite_antenna: Self-test (wireform-to-wireform)";
    key = "grpc-self";
    command = [
      "nix develop .#${defaultGHC} --command cabal run wireform-grpc-interop -- --self-test"
    ];
    agents = dockerAgents;
    timeout = 20;
  };

  grpcCrossLanguage = lang: ci.command {
    label = ":satellite_antenna: Cross-language (${lang})";
    key = "grpc-${lang}";
    depends_on = "grpc-self";
    command = [
      "wireform-grpc/scripts/cross-language-interop.sh ${lang}"
    ];
    agents = dockerAgents;
    timeout = 20;
    soft_fail = true;
  };

  grpcGroup = ci.group ":satellite_antenna: gRPC Interop" {
    key = "grpc-interop";
    steps =
      [ grpcSelfTest ]
      ++ map grpcCrossLanguage [ "python" "cxx" "go" ];
  };

  # ------------------------------------------------------------------
  # Kafka integration
  # ------------------------------------------------------------------
  kafkaVersions = [ "3.7.0" "4.0.0" ];

  kafkaStep = version: ci.command {
    label = ":kafka: Kafka ${version} integration";
    key = "kafka-${builtins.replaceStrings ["."] ["_"] version}";
    command = [
      "docker compose -f wireform-kafka/test-integration/docker-compose.yml up -d"
      "WIREFORM_KAFKA_BROKER=localhost:9092 nix develop .#${defaultGHC} --command cabal test wireform-kafka-integration"
      "docker compose -f wireform-kafka/test-integration/docker-compose.yml down"
    ];
    # The compose file selects the broker image via KAFKA_IMAGE_TAG
    # (default 4.0.0); set it per matrix leg so 3.7.0 actually runs 3.7.0.
    env = { KAFKA_IMAGE_TAG = version; };
    agents = dockerAgents;
    timeout = 30;
    retry = {
      automatic = [
        { exit_status = -1; limit = 2; }
        { exit_status = 1;  limit = 1; }
      ];
    };
  };

  kafkaGroup = ci.group ":kafka: Kafka Integration" {
    key = "kafka-interop";
    steps = map kafkaStep kafkaVersions;
  };

  # ------------------------------------------------------------------
  # WebSocket Autobahn
  # ------------------------------------------------------------------
  autobahn = ci.command {
    label = ":satellite: Autobahn|Testsuite";
    key = "websocket-autobahn";
    # Use the canonical runner: it builds the right exe
    # (wireform-websocket-autobahn-echo), starts it, mounts the spec
    # (test-conformance/config) and report dir into the container, and
    # summarises the JSON report (python3 — provided by the dev shell).
    command = [
      "nix develop .#${defaultGHC} --command wireform-websocket/scripts/run-autobahn.sh"
    ];
    agents = dockerAgents;
    timeout = 30;
    artifact_paths = [ "wireform-websocket/test-conformance/reports/**/*" ];
  };

  websocketGroup = ci.group ":satellite: WebSocket Conformance" {
    key = "websocket-interop";
    steps = [ autobahn ];
  };

  # ------------------------------------------------------------------
  # Assemble
  # ------------------------------------------------------------------
  steps =
    (if grpcAffected then [ grpcGroup ] else [])
    ++ (if kafkaAffected then [ kafkaGroup ] else [])
    ++ (if websocketAffected then [ websocketGroup ] else []);

  pipeline = ci.pipeline {
    env = {
      NIX_CONFIG = "experimental-features = nix-command flakes";
    };
    agents = nixAgents;
    steps =
      if steps == [] then [
        (ci.command {
          label = ":white_check_mark: No interop tests needed";
          command = "echo 'No interop-affecting packages changed'";
          agents = nixAgents;
          timeout = 1;
        })
      ] else steps;
  };

in render.renderPipeline pipeline
