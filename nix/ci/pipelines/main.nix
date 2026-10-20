# Primary CI pipeline: Build -> Test -> Lint + Codegen + Bench + Docs
#
# Invoked with:
#   nix eval .#ci.main --json \
#     --arg changedFiles '["wireform-core/src/Foo.hs"]' \
#     --arg isMain true
{ lib
, changedFiles ? []
, isMain ? false
}:

let
  ci       = import ../dsl.nix { inherit lib; };
  render   = import ../render.nix { inherit lib; };
  packages = import ../packages.nix { inherit lib; };
  changes  = import ../changes.nix { inherit lib packages; };

  affected = changes.affectedPackages changedFiles;
  hasAffected = affected != {};

  defaultGHC = "ghc98";

  inherit (ci) nixAgents;

  # Slug-safe key from package name: wireform-core -> wireform_core
  keyOf = name: builtins.replaceStrings [ "-" ] [ "_" ] name;

  # ------------------------------------------------------------------
  # Build group: nix build every affected package x GHC (flake exposes
  # <pkg>-<shell> outputs; Buildkite interpolates {{matrix.ghc}}). Each
  # package carries its own supported-GHC list (packages.nix), so the
  # matrix only fans out to compilers that can actually build it.
  # ------------------------------------------------------------------
  buildSteps = ci.forPackagesSorted affected (pkg:
    ci.command {
      label = "${pkg.emoji} ${pkg.name} ({{matrix.ghc}})";
      key = "build-${keyOf pkg.name}";
      command = "nix build .#packages.x86_64-linux.${pkg.name}-{{matrix.ghc}} --print-build-logs";
      agents = nixAgents;
      timeout = 45;
      priority = if pkg.tier == "core" then 5 else 0;
      matrix = {
        setup = { ghc = pkg.ghcVersions; };
      };
    }
  );

  buildGroup = ci.group ":nix: Build" {
    key = "build";
    steps = buildSteps;
  };

  # ------------------------------------------------------------------
  # Test group: cabal test for affected packages with test suites
  # ------------------------------------------------------------------
  # Sandbox-hostile suites (packages.nix `sandboxHostileTest`) can't run as
  # an in-sandbox `nix build .#checks.<pkg>`; the interop pipeline covers
  # them. Everything else becomes a pure `nix build` of its check
  # derivation — the suite runs inside the build, so no `nix develop`.
  testable = lib.filterAttrs (_: p: !(p.sandboxHostileTest or false))
    (ci.withTests affected);

  testSteps = ci.forPackagesSorted testable (pkg:
    ci.command {
      label = "${pkg.emoji} ${pkg.name} tests";
      key = "test-${keyOf pkg.name}";
      command = [
        "nix build .#checks.x86_64-linux.${pkg.name} --print-build-logs"
        ''buildkite-agent annotate --style success --context "test-${keyOf pkg.name}" --append "| ${pkg.emoji} ${pkg.name} | :white_check_mark: passed |"''
      ];
      agents = nixAgents;
      depends_on = "build-${keyOf pkg.name}";
      timeout = 30;
    }
  );

  # Proto conformance runs as part of `nix build .#checks…wireform-proto`
  # (the protobuf-conformance-test suite), so no dedicated step is needed.
  conformanceSteps = [];

  testGroup = ci.group ":test_tube: Test" {
    key = "test";
    depends_on = "build";
    steps = testSteps ++ conformanceSteps;
  };

  # ------------------------------------------------------------------
  # Lint group: HLint + Fourmolu as `nix build` checks (whole-tree
  # runCommand derivations defined in flake.nix; cacheable by src hash).
  # ------------------------------------------------------------------
  lintGroup = ci.group ":mag: Lint" {
    key = "lint";
    depends_on = "build";
    steps = [
      (ci.command {
        label = ":mag: HLint";
        key = "hlint";
        command = "nix build .#checks.x86_64-linux.hlint --print-build-logs";
        agents = nixAgents;
        soft_fail = true;
        timeout = 15;
      })
      (ci.command {
        label = ":art: Fourmolu";
        key = "fourmolu";
        command = "nix build .#checks.x86_64-linux.fourmolu --print-build-logs";
        agents = nixAgents;
        soft_fail = true;
        timeout = 10;
      })
    ];
  };

  # ------------------------------------------------------------------
  # Codegen roundtrip group
  # ------------------------------------------------------------------
  codegenSteps = lib.optionals (builtins.hasAttr "wireform-proto" affected) [
    (ci.command {
      label = ":label: regen-wkt verify";
      key = "codegen-wkt";
      # Build the proto package (ships the regen-wkt exe in bin), run it,
      # and assert the committed well-known-types tree is unchanged.
      command = [
        "nix build .#packages.x86_64-linux.wireform-proto --print-build-logs"
        "result/bin/regen-wkt"
        "git diff --exit-code wireform-proto/src/Proto/Google/Protobuf/"
      ];
      agents = nixAgents;
      depends_on = "build-wireform_proto";
      timeout = 15;
    })
  ] ++ lib.optionals (builtins.hasAttr "wireform-kafka" affected) [
    (ci.command {
      label = ":kafka: regen-kafka-protocol verify";
      key = "codegen-kafka";
      command = [
        "scripts/regen-kafka-protocol.sh /tmp/kafka-schemas"
        "git diff --exit-code wireform-kafka/src/Kafka/Protocol/Generated/"
      ];
      agents = nixAgents;
      depends_on = "build-wireform_kafka";
      timeout = 15;
    })
  ];

  codegenGroup = ci.group ":memo: Codegen Roundtrip" {
    key = "codegen";
    depends_on = "build";
    steps = codegenSteps;
  };

  # ------------------------------------------------------------------
  # Benchmark group (main branch only)
  # ------------------------------------------------------------------
  benchable = ci.withBenchmarks affected;

  # Compile-only: `nix build .#…<pkg>-bench` builds the benchmark
  # components (warming the cache) without running them — measurement in a
  # shared CI sandbox is meaningless. No concurrency/artifact handling is
  # needed since nothing is measured here.
  benchSteps = ci.forPackagesSorted benchable (pkg:
    ci.command {
      label = ":stopwatch: ${pkg.name} bench (compile)";
      key = "bench-${keyOf pkg.name}";
      command = "nix build .#packages.x86_64-linux.${pkg.name}-bench --print-build-logs";
      agents = nixAgents;
      depends_on = "test";
      timeout = 45;
    }
  );

  benchGroup = ci.group ":bar_chart: Benchmarks" {
    key = "bench";
    depends_on = "test";
    if_ = "build.branch == 'main'";
    steps = benchSteps;
  };

  # ------------------------------------------------------------------
  # Docs group (main branch only)
  # ------------------------------------------------------------------
  docsGroup = ci.group ":rocket: Docs" {
    key = "docs";
    depends_on = "test";
    if_ = "build.branch == 'main'";
    steps = [
      (ci.command {
        label = ":book: Haddock";
        key = "haddock";
        command = "nix build .#packages.x86_64-linux.haddock --print-build-logs";
        agents = nixAgents;
        timeout = 30;
      })
    ];
  };

  # ------------------------------------------------------------------
  # C kernel harness (c-harness/): sanitizer tests, corpus replay,
  # deterministic fuzz smoke, static analysis, MSan and valgrind. Runs
  # when any C source, the harness, or global build config changes.
  # ------------------------------------------------------------------
  touchesC = builtins.any
    (f: lib.hasPrefix "c-harness/" f
        || lib.hasSuffix ".c" f
        || lib.hasSuffix ".h" f
        || changes.isGlobalChange f)
    changedFiles;

  cHarnessGroup = ci.group ":shield: C harness" {
    key = "c-harness";
    steps = [
      (ci.command {
        label = ":shield: C kernels: sanitizers, fuzz smoke, analyzer";
        key = "c-harness-ci";
        command = "nix build .#checks.x86_64-linux.c-harness --print-build-logs";
        agents = nixAgents;
        timeout = 45;
      })
    ];
  };

  # Long fuzz campaigns: scheduled builds only. Crashes are uploaded as
  # artifacts; reproduce one with the fuzz binary from `make fuzz-build`.
  fuzzCampaignGroup = ci.group ":bug: C fuzz campaign" {
    key = "c-fuzz";
    if_ = "build.source == 'schedule'";
    steps = [
      (ci.command {
        label = ":bug: libFuzzer campaign (5 min per target)";
        key = "c-fuzz-campaign";
        command = "nix develop .#c-harness -c make -C c-harness -k fuzz FUZZ_TIME=300 FUZZ_FORK=4";
        agents = nixAgents;
        timeout = 600;
        artifact_paths = [ "c-harness/build/crashes/**/*" ];
      })
    ];
  };

  # ------------------------------------------------------------------
  # Annotation summary step
  # ------------------------------------------------------------------
  summaryStep = ci.command {
    label = ":clipboard: Summary";
    key = "summary";
    depends_on = "test";
    allow_dependency_failure = true;
    command = ''
      echo "### Build Summary" | buildkite-agent annotate --style info --context summary
    '';
    agents = nixAgents;
    timeout = 5;
  };

  # ------------------------------------------------------------------
  # Assemble pipeline
  # ------------------------------------------------------------------
  allSteps =
    (if hasAffected then [ buildGroup ] else [])
    ++ (if testSteps != [] || conformanceSteps != [] then [ testGroup ] else [])
    ++ (if hasAffected then [ lintGroup ] else [])
    ++ (if touchesC then [ cHarnessGroup ] else [])
    ++ [ fuzzCampaignGroup ]
    ++ (if codegenSteps != [] then [ codegenGroup ] else [])
    ++ (if benchSteps != [] then [ benchGroup ] else [])
    ++ [ docsGroup ]
    ++ [ summaryStep ];

  pipeline = ci.pipeline {
    env = {
      NIX_CONFIG = "experimental-features = nix-command flakes";
    };
    agents = nixAgents;
    steps = allSteps;
  };

in render.renderPipeline pipeline
