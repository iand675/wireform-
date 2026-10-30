{ lib }:

let
  # Strip null-valued and empty-list keys from an attrset so the
  # rendered JSON never contains fields Buildkite would reject.
  compact = attrs:
    lib.filterAttrs (_: v:
      v != null && v != [] && v != {}
    ) attrs;

  defaultRetry = {
    automatic = [
      { exit_status = -1;  limit = 2; }   # agent lost
      { exit_status = 255; limit = 2; }   # forced shutdown
    ];
  };

  nixAgents = { queue = "nix"; };

  # ------------------------------------------------------------------
  # Step constructors
  # ------------------------------------------------------------------

  mkCommand =
    { label
    , command
    , key             ? null
    , agents          ? nixAgents
    , env             ? {}
    , depends_on      ? null
    , soft_fail       ? false
    , retry           ? defaultRetry
    , artifact_paths  ? []
    , timeout         ? 30
    , parallelism     ? null
    , priority        ? null
    , skip            ? false
    , matrix          ? null
    , plugins         ? []
    , if_             ? null
    , allow_dependency_failure ? false
    , concurrency       ? null
    , concurrency_group ? null
    }:
    compact {
      _type = "command";
      inherit label key agents env depends_on soft_fail retry
              artifact_paths parallelism priority skip matrix plugins
              allow_dependency_failure concurrency concurrency_group;
      timeout_in_minutes = timeout;
      "if" = if_;
      commands = if builtins.isList command then command else [ command ];
    };

  mkGroup = label:
    { key         ? null
    , steps
    , depends_on  ? null
    , if_         ? null
    , skip        ? false
    , notify      ? []
    , allow_dependency_failure ? false
    }:
    compact {
      _type = "group";
      group = label;
      inherit key steps depends_on skip notify allow_dependency_failure;
      "if" = if_;
    };

  mkWait =
    { continue_on_failure ? false
    , key                 ? null
    , if_                 ? null
    }:
    compact {
      _type = "wait";
      wait = null;      # Buildkite uses `wait: ~`
      inherit key continue_on_failure;
      "if" = if_;
    };

  mkBlock = label:
    { prompt       ? null
    , fields       ? []
    , key          ? null
    , blocked_state ? null
    , if_          ? null
    , depends_on   ? null
    }:
    compact {
      _type = "block";
      block = label;
      inherit prompt fields key blocked_state depends_on;
      "if" = if_;
    };

  mkTrigger = pipeline:
    { label    ? ":pipeline: Trigger ${pipeline}"
    , key      ? null
    , async    ? false
    , build    ? {}
    , agents   ? nixAgents
    , soft_fail ? false
    , if_      ? null
    , depends_on ? null
    }:
    compact {
      _type = "trigger";
      trigger = pipeline;
      inherit label key async build agents soft_fail depends_on;
      "if" = if_;
    };

  # ------------------------------------------------------------------
  # Nix-aware helpers
  # ------------------------------------------------------------------

  nixBuild =
    { flakeRef
    , label    ? ":nix: Build ${flakeRef}"
    , key      ? null
    , agents   ? nixAgents
    , timeout  ? 45
    , priority ? null
    , depends_on ? null
    , artifact_paths ? []
    }:
    mkCommand {
      inherit label key agents timeout priority depends_on artifact_paths;
      command = "nix build .#${flakeRef} --print-build-logs";
    };

  nixCheck =
    { flakeRef
    , label    ? ":test_tube: Check ${flakeRef}"
    , key      ? null
    , agents   ? nixAgents
    , timeout  ? 45
    , depends_on ? null
    , soft_fail ? false
    , artifact_paths ? []
    }:
    mkCommand {
      inherit label key agents timeout depends_on soft_fail artifact_paths;
      command = "nix build .#checks.${flakeRef} --print-build-logs";
    };

  # ------------------------------------------------------------------
  # Combinators
  # ------------------------------------------------------------------

  forPackages = pkgs: fn:
    map fn (builtins.attrValues pkgs);

  forPackagesSorted = pkgs: fn:
    let
      sorted = builtins.sort (a: b: a.name < b.name) (builtins.attrValues pkgs);
    in map fn sorted;

  # Generate steps across a GHC version matrix. Each element of
  # `ghcVersions` is a string like "ghc910"; `fn` receives
  # { ghc, ghcVersion } and returns a step.
  withGhcMatrix = ghcVersions: fn:
    builtins.concatMap (ghc: let
      step = fn { inherit ghc; ghcVersion = ghc; };
    in if builtins.isList step then step else [ step ]
    ) ghcVersions;

  # Filter a package set by tier
  byTier = tier: pkgs:
    lib.filterAttrs (_: p: p.tier == tier) pkgs;

  # Convenience: only packages that have test suites
  withTests = pkgs:
    lib.filterAttrs (_: p: p.hasTests or false) pkgs;

  withBenchmarks = pkgs:
    lib.filterAttrs (_: p: p.hasBenchmarks or false) pkgs;

  # ------------------------------------------------------------------
  # Pipeline top-level
  # ------------------------------------------------------------------

  mkPipeline =
    { env     ? {}
    , agents  ? nixAgents
    , steps
    , notify  ? []
    }:
    compact {
      _type = "pipeline";
      inherit env agents steps notify;
    };

  # Shorthand barrier (no options)
  barrier = mkWait {};

in {
  command   = mkCommand;
  group     = mkGroup;
  wait      = mkWait;
  block     = mkBlock;
  trigger   = mkTrigger;
  nixBuild  = nixBuild;
  nixCheck  = nixCheck;
  pipeline  = mkPipeline;

  inherit barrier;
  inherit forPackages forPackagesSorted withGhcMatrix;
  inherit byTier withTests withBenchmarks;
  inherit defaultRetry compact nixAgents;
}
