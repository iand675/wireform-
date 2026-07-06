{
  description = "wireform — high-performance multi-format serialization toolkit for Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        lib = pkgs.lib;
        hlib = pkgs.haskell.lib.compose;

        # The CI package registry: the same source of truth the Buildkite
        # pipeline uses to decide which `<pkg>-<ghc>` outputs to build and
        # which GHC versions each package supports (`ghcVersions`, narrowed
        # by per-package overrides). Reused below for the `rollup` output so
        # it stays the exact equivalent of the Buildkite Build group.
        ciPackages = import ./nix/ci/packages.nix { inherit lib; };

        # GHC build matrix — add/remove entries to test against
        # different compilers. Shell names become `nix develop .#ghcXY`.
        # Covers the released GHC 9.6–9.14 line (odd minors are
        # unreleased dev snapshots and are intentionally omitted). GHC 9.4
        # is excluded: wireform-core's parser uses the delimited-continuation
        # primop PromptTag#, which only exists from GHC 9.6 onward.
        ghcMatrix = {
          ghc96  = "ghc96";
          ghc98  = "ghc98";
          ghc910 = "ghc910";
          ghc912 = "ghc912";
          ghc914 = "ghc914";
        };

        defaultGHC = "ghc98";

        systemDeps = [
          pkgs.zlib
          pkgs.snappy
          pkgs.zstd
          pkgs.lz4
          pkgs.brotli
          pkgs.openssl
        ];

        sharedTools = [
          pkgs.cabal-install
          pkgs.pkg-config
          pkgs.ghciwatch
          pkgs.haskellPackages.fourmolu
          pkgs.hlint
          pkgs.prek
          pkgs.llvmPackages.llvm
          # protoc is required at configure time by
          # proto-lens-protobuf-types, which the wireform-grpc test
          # suites pull in. It also unblocks anyone running the
          # protobuf conformance / interop scripts manually.
          pkgs.protobuf
          # python3 drives the helper scripts in scripts/ and
          # wireform-websocket/scripts/autobahn-summary.py (the Autobahn
          # conformance summariser the interop pipeline shells out to).
          pkgs.python3
          # TLA+ tools (tlc, tlasany, pcal) for the model-checked
          # protocol arguments — currently the Lattice invalidation
          # pipeline model in wireform-lattice/tla/ (run ./check.sh).
          pkgs.tlaplus
        ];

        # ------------------------------------------------------------
        # Per-format package set
        # ------------------------------------------------------------
        # Every per-format package gets its own callCabal2nix entry;
        # the umbrella `wireform` then depends on the lot. Without
        # this list, cabal2nix would try to resolve names like
        # `wireform-proto` against Hackage and fail.
        #
        # Adding a new per-format package means: drop a line into
        # `wireformPackages` *and* an entry under `cabal.project`
        # at the workspace root.
        wireformPackages = {
          wireform-core              = ./wireform-core;
          wireform-derive            = ./wireform-derive;
          wireform-columnar-core     = ./wireform-columnar-core;
          wireform-columnar          = ./wireform-columnar;
          wireform-proto             = ./wireform-proto;
          wireform-avro              = ./wireform-avro;
          wireform-thrift            = ./wireform-thrift;
          wireform-cbor              = ./wireform-cbor;
          wireform-msgpack           = ./wireform-msgpack;
          wireform-bson              = ./wireform-bson;
          wireform-ion               = ./wireform-ion;
          wireform-capnproto         = ./wireform-capnproto;
          wireform-flatbuffers       = ./wireform-flatbuffers;
          wireform-bond              = ./wireform-bond;
          wireform-asn1              = ./wireform-asn1;
          wireform-xml               = ./wireform-xml;
          wireform-html              = ./wireform-html;
          wireform-parquet           = ./wireform-parquet;
          wireform-orc               = ./wireform-orc;
          wireform-arrow             = ./wireform-arrow;
          wireform-iceberg           = ./wireform-iceberg;
          wireform-edn               = ./wireform-edn;
          wireform-bencode           = ./wireform-bencode;
          wireform-toml              = ./wireform-toml;
          wireform-csv               = ./wireform-csv;
          wireform-ndjson            = ./wireform-ndjson;
          wireform-fory              = ./wireform-fory;
          wireform-network           = ./wireform-network;
          wireform-attoparsec        = ./wireform-attoparsec;
          wireform-binary            = ./wireform-binary;
          wireform-cereal            = ./wireform-cereal;
          wireform-http1             = ./wireform-http1;
          wireform-http2             = ./wireform-http2;
          wireform-http              = ./wireform-http;
          wireform-http-wai          = ./wireform-http-wai;
          wireform-kafka-protocol    = ./wireform-kafka-protocol;
          wireform-kafka             = ./wireform-kafka;
          wireform-stats             = ./wireform-stats;
          wireform-lance             = ./wireform-lance;
          wireform-yaml              = ./wireform-yaml;
          wireform-delta             = ./wireform-delta;
          wireform-hudi              = ./wireform-hudi;
          wireform-grpc              = ./wireform-grpc;
          # Previously only present transitively / via a one-off
          # callCabal2nix; listed here so each is exposed as a
          # per-GHC package output the CI matrix can build.
          hermes                     = ./hermes;
          grpc-spec                  = ./grpc-spec;
          wireform-websocket         = ./wireform-websocket;
          wireform-cel               = ./wireform-cel;
          wireform-protovalidate     = ./wireform-protovalidate;
          wireform-hw-kafka-client   = ./wireform-hw-kafka-client;
          wireform-lattice           = ./wireform-lattice;
        };

        # Cabal flags to enable on specific packages.
        #
        # NOTE: the optional codec flags (+snappy/+zstd/+lz4) are
        # deliberately NOT enabled for the `nix build` outputs. Under
        # cabal2nix, `extra-libraries: zstd` and the Haskell
        # `build-depends: zstd` both resolve to `haskellPackages.zstd`,
        # and the overlay binds that name to the C library (pkgs.zstd)
        # so `extra-libraries` link. Enabling the flag would then ask
        # for the Haskell `zstd` package under the same (now-shadowed)
        # name and fail with "missing or private dependencies: zstd".
        # The codec paths are exercised by the `cabal test` step, which
        # solves against Hackage rather than the pinned nix package set.
        packageFlags = {
        };

        applyFlags = name: drv:
          lib.foldl' (acc: flag: hlib.enableCabalFlag flag acc) drv
            (packageFlags.${name} or []);

        # Build every per-format package via callCabal2nix and
        # apply the right Cabal flags. Benchmarks and tests are
        # both off at the Nix level: tests belong in `cabal test`,
        # not in the Nix sandbox (ring-buffer / OS-specific tests
        # misbehave under sandboxing, and dependency-cycle
        # packages like wireform-columnar-core would deadlock the
        # build graph). Use `cabal test` inside the dev shell instead.
        # wireform-kafka-protocol's library reads its sources from a
        # sibling tree (`hs-source-dirs: ../wireform-kafka/src`). A bare
        # callCabal2nix copies only the package directory, so the sibling
        # path is absent in the sandbox ("can't find source for
        # Kafka/Protocol/Generated/..."). Stitch both directories into a
        # single source root and point cabal2nix at the subpath.
        kafkaProtocolSrc = pkgs.runCommand "wireform-kafka-protocol-src" {} ''
          mkdir -p $out/wireform-kafka-protocol $out/wireform-kafka
          cp -R ${./wireform-kafka-protocol}/. $out/wireform-kafka-protocol/
          cp -R ${./wireform-kafka/src} $out/wireform-kafka/src
        '';

        # `snappy` and `zlib` are ambiguous names: wireform-kafka uses them
        # as C libraries (`pkgconfig-depends: snappy, zlib`) while other
        # packages use the Haskell bindings (`build-depends: snappy`/`zlib`).
        # We keep the global names bound to the Haskell packages (so those
        # resolve) and feed kafka the system libraries through pkgconfig
        # here. `liblz4`/`libzstd`/`lz4` are C-only names mapped in the
        # overlay below; `zstd` is Haskell-only.
        cLibPkgconfigDeps = {
          wireform-kafka = [ pkgs.snappy pkgs.zlib ];
        };

        # GHC versions on which we strip cabal version bounds with
        # `doJailbreak`. Prefer fixing bounds at the source (bump the cap in
        # the offending cabal file) so the constraint stays meaningful; this
        # list is the escape hatch for cases that can't be fixed that way.
        # Currently empty: the template-haskell <2.24 caps that blocked GHC
        # 9.14 were bumped to <2.25 in the cabal files instead.
        jailbreakGhcs = [ ];

        mkHaskellOverlay = ghcName: self: super:
          let
            maybeJailbreak =
              if builtins.elem ghcName jailbreakGhcs
              then hlib.doJailbreak
              else lib.id;
            # Build a Haskell package straight from its Hackage release,
            # hashing the UNPACKED tree (fetchzip) so the hash is stable
            # across mirror://hackage gzip variance. Used for deps that are
            # newer than what the pinned nixpkgs provides.
            callHackageZip = name: ver: sha256:
              hlib.doJailbreak (self.callCabal2nix name (pkgs.fetchzip {
                url = "https://hackage.haskell.org/package/${name}-${ver}/${name}-${ver}.tar.gz";
                inherit sha256;
              }) {});
            mkRaw = name: src:
              if name == "wireform-kafka-protocol"
              then self.callCabal2nixWithOptions name kafkaProtocolSrc
                     "--subpath wireform-kafka-protocol" {}
              else self.callCabal2nix name src {};
            mkPkg = name: src:
              applyFlags name
                (maybeJailbreak
                  (hlib.overrideCabal (drv: {
                    doBenchmark = false;
                    doCheck    = false;
                    # Force haddock on so every package emits a `doc`
                    # output. nixpkgs defaults `doHaddock` to false for
                    # multi-public-library cabal packages (wireform-kafka
                    # ships the `wireform-kafka-streams` and
                    # `wireform-kafka-codegen` sub-libraries), which is why
                    # it otherwise produced no docs and broke the haddock
                    # aggregate. The others already default to true, so this
                    # only flips kafka.
                    doHaddock = true;
                    libraryPkgconfigDepends =
                      (drv.libraryPkgconfigDepends or [])
                      ++ (cLibPkgconfigDeps.${name} or []);
                  })
                    (mkRaw name src)));
            perFormatAttrs = lib.mapAttrs mkPkg wireformPackages;
            wireformAttr = applyFlags "wireform"
              (maybeJailbreak
                (hlib.overrideCabal (drv: { doBenchmark = false; })
                  (self.callCabal2nix "wireform" ./. {})));
            # crc32c-0.2.2 in nixpkgs lists only x86 Darwin as supported,
            # but Google's crc32c C library has ARMv8 CRC hardware support.
            # Lift the false platform restriction so grpc-spec (and
            # wireform-grpc) resolve on aarch64-darwin.
            crc32cUnrestricted = super.crc32c.overrideAttrs (old: {
              meta = old.meta // { platforms = lib.platforms.all; };
            });
          in
            perFormatAttrs // {
              wireform = wireformAttr;
              crc32c   = crc32cUnrestricted;
              # wireform-http / wireform-kafka need hs-opentelemetry-api 1.0,
              # which nixpkgs hasn't packaged (it ships 0.3.1.0). The 1.0
              # release split the package, so pull the whole trio from
              # Hackage: api -> api-types + semantic-conventions (>=1.40);
              # nixpkgs has only semantic-conventions 0.1 and no api-types.
              hs-opentelemetry-api =
                callHackageZip "hs-opentelemetry-api" "1.0.0.0"
                  "sha256-COhj9Ms1eu1Gt9wTC21oQ37k6vJ9mxlJvYpHtvXff6A=";
              hs-opentelemetry-api-types =
                callHackageZip "hs-opentelemetry-api-types" "1.0.0.0"
                  "sha256-9ByP41wlV45TMCqbyyVpwejQDi5fsG0+j8bMk8ORLw8=";
              hs-opentelemetry-semantic-conventions =
                callHackageZip "hs-opentelemetry-semantic-conventions" "1.40.0.0"
                  "sha256-7cIC9dTrd5bJjAsiEyyupi1xSZyc17FpjbACnm0p5ik=";
              # tasty-hspec in nixpkgs caps base <4.22, which excludes GHC
              # 9.14 (base 4.22). It is pulled in as a transitive test
              # dependency of some upstream package; jailbreak so it builds.
              tasty-hspec = hlib.doJailbreak super.tasty-hspec;
              # symbolize (a hermes dependency) has a currently-busted test
              # suite; skip it so the library still builds.
              symbolize = hlib.dontCheck super.symbolize;
              # http-api-data 0.6.3 (a wireform-http dependency) caps
              # base <4.22 / containers <0.8, which excludes GHC 9.14
              # (base 4.22, containers 0.8). Jailbreak so it configures;
              # harmless on the older GHCs where the bounds already hold.
              http-api-data = hlib.doJailbreak super.http-api-data;
              # uri-templater 1.0.0.1 (transitive via http-api-data) ships a
              # doctest suite that fails on GHC 9.12 with an ambiguous-type
              # `print it` (a doctest/GHCi defaulting regression, not our
              # code). Skip the dependency's test suite.
              uri-templater = hlib.dontCheck super.uri-templater;
              # C-library names cabal2nix resolves against the Haskell
              # package set. These are unambiguous C libs (pkgconfig /
              # extra-libraries), with no Haskell package of the same name:
              #   liblz4/libzstd  -> wireform-kafka `pkgconfig-depends`
              #   lz4             -> wireform-columnar-core `extra-libraries`
              #   openssl         -> `pkgconfig-depends`
              # `zstd` and `snappy` are deliberately left as Haskell
              # bindings (build-depends users); kafka's C `snappy` is
              # supplied via `cLibPkgconfigDeps` above.
              liblz4   = pkgs.lz4;
              libzstd  = pkgs.zstd;
              lz4      = pkgs.lz4;
              openssl  = pkgs.openssl;
              # blake3's SIMD cabal flags default on and compile x86
              # intrinsics C on every arch (broken on aarch64); force the
              # portable path, mirroring the cabal.project flags block.
              # nixpkgs also marks the package x86-only (same false
              # restriction as crc32c above); with the portable path forced
              # it builds fine on aarch64-darwin, so lift meta.platforms.
              # Its test suite prints every case OK yet exits FAIL there,
              # so skip it (same treatment as symbolize above).
              blake3 = lib.pipe super.blake3 [
                hlib.dontCheck
                (hlib.disableCabalFlag "avx512")
                (hlib.disableCabalFlag "avx2")
                (hlib.disableCabalFlag "sse41")
                (hlib.disableCabalFlag "sse2")
                (drv: drv.overrideAttrs (old: {
                  meta = old.meta // { platforms = lib.platforms.all; };
                }))
              ];
            };

        mkDevShell = ghcAttr:
          let
            hp = (pkgs.haskell.packages.${ghcAttr}).override {
              overrides = mkHaskellOverlay ghcAttr;
            };
            # `cabal.project` forces `wireform-arrow: +zstd +lz4`, so any
            # `cabal` invocation in the shell (e.g. `cabal run
            # wireform-grpc-interop`, `cabal test`) needs the `lz4-hs` and
            # `zstd` Haskell packages in the shell's package db. The plain
            # `nix build` outputs keep the codec flags off (see the
            # packageFlags NOTE), so re-derive arrow *for the shell only*
            # with the flags on — callCabal2nixWithOptions regenerates the
            # dependency set so lz4-hs / zstd are actually pulled into the
            # shellFor closure rather than just toggling a configure flag.
            # The dev shell must be self-contained: every `cabal test` /
            # `cabal bench` / `cabal run <suite>` run in CI (the interop
            # kafka / grpc jobs and main.nix's test / bench / conformance /
            # haddock steps) resolves its build plan purely against the
            # shell's GHC package db. The plain `nix build` outputs keep
            # checks and benchmarks off so the build matrix stays lean, but
            # `shellFor` only pulls a package's *test* / *benchmark*
            # dependencies into the db when doCheck / doBenchmark are on.
            # Turn them on for the shell so sydtest, criterion, hedgehog, …
            # land in the db; otherwise cabal falls back to a (frequently
            # absent on fresh agents) Hackage index and tries to build the
            # test deps from source — the [Cabal-7043]/[Cabal-7070] "solver
            # did not find a plan that included the test suites" failures.
            withTestDeps = drv: hlib.doBenchmark (hlib.doCheck drv);
            arrowWithCodecs =
              withTestDeps
                (hp.callCabal2nixWithOptions "wireform-arrow" ./wireform-arrow
                  "--flag=lz4 --flag=zstd" {});
            # Every package the workspace ships, so a single
            # `nix develop` shell can build any of them via
            # `cabal build <pkg>`.
            workspaceDrvs =
              lib.attrValues
                (lib.mapAttrs
                  (n: drv:
                    if n == "wireform-arrow" then arrowWithCodecs
                    else withTestDeps drv)
                  (lib.getAttrs (lib.attrNames wireformPackages) hp));
          in
          hp.shellFor {
            packages = _: workspaceDrvs;

            nativeBuildInputs = [
              hp.haskell-language-server
            ] ++ sharedTools ++ cdnTools;

            buildInputs = systemDeps;

            shellHook = ''
              echo "wireform dev shell — $(ghc --version)"
              echo "  packages: ${
                toString (lib.attrNames wireformPackages)} wireform"
            '';
          };

        devShells = builtins.mapAttrs (_: mkDevShell) ghcMatrix;

        # CDN harness tools (wireform-lattice/cdn), part of the dev shell:
        # node for the conformance checker + the Cloudflare Worker harness
        # (wrangler runs via npx), curl for the run scripts. Varnish itself
        # runs as a container (official varnish:8.0 image, which bundles
        # varnish-modules incl. vmod_xkey) via podman — see
        # wireform-lattice/cdn/README.md. The native nixpkgs varnish is
        # deliberately not used: 8.0.2 needs several Darwin build patches
        # (false ac_cv pre-seeds, pre-2.4.7 libtool), and its successor —
        # Varnish rebranded to Vinyl Cache at v9; nixpkgs ships it as
        # `vinyl-cache` 9.0.1 — is marked broken on Darwin and has no
        # vinyl-compatible modules set for xkey.
        cdnTools = [
          pkgs.nodejs
          pkgs.curl
        ];

        packagesForGhc = ghcAttr:
          let
            hp = (pkgs.haskell.packages.${ghcAttr}).override {
              overrides = mkHaskellOverlay ghcAttr;
            };
            perFormat = lib.getAttrs (lib.attrNames wireformPackages) hp;
          in perFormat // {
            wireform = hp.wireform;
          };

        # ------------------------------------------------------------
        # Synthetic CI outputs (tests / lint / benchmarks / docs)
        # ------------------------------------------------------------
        # The Buildkite pipeline runs its non-build steps (test, lint,
        # bench, haddock) on defaultGHC. We express each as a `nix build`
        # target so the whole pipeline is `nix build`-only (no
        # `nix develop`) and a single `nix build .#rollup` warms the cache
        # for every step. The overlaid defaultGHC package set, reused below.
        hpDefault = (pkgs.haskell.packages.${defaultGHC}).override {
          overrides = mkHaskellOverlay defaultGHC;
        };

        # Test suites that cannot run sealed in the nix sandbox (network /
        # broker / OS-specific / dependency-cycle). Derived from the CI
        # registry (`sandboxHostileTest = true`) so the flake and the
        # Buildkite pipeline agree on which suites skip the in-sandbox
        # `checks.<pkg>` set. Grow the registry flag as sandboxed
        # `nix build .#checks.<pkg>` failures surface.
        sandboxHostileTests = map (p: p.name)
          (lib.filter (p: p.sandboxHostileTest or false)
            (lib.attrValues ciPackages));

        # Registry-driven lists (same source of truth the pipeline uses).
        testablePackages = lib.filter
          (p: (p.hasTests or false)
              && !(builtins.elem p.name sandboxHostileTests))
          (lib.attrValues ciPackages);
        benchPackages = lib.filter
          (p: p.hasBenchmarks or false)
          (lib.attrValues ciPackages);

        # `checks.<pkg>` — run the suite in-sandbox during the build.
        sandboxChecks = lib.listToAttrs (map
          (p: { name = p.name; value = hlib.doCheck hpDefault.${p.name}; })
          testablePackages);

        # `packages.<pkg>-bench` — compile-only: `doBenchmark` builds the
        # benchmark components (pulling criterion etc.) but the haskell
        # builder never runs them, so this just warms the compile cache.
        benchPkgsAttrs = lib.listToAttrs (map
          (p: { name = "${p.name}-bench"; value = hlib.doBenchmark hpDefault.${p.name}; })
          benchPackages);

        # `packages.haddock` — aggregate of every package's `doc` output.
        # These are the same derivations as the lib outputs (nixpkgs builds
        # haddock as a second output of the library derivation), so this
        # adds no build work beyond the lib matrix; it just gives the docs
        # step a single `nix build` target.
        haddockAgg = pkgs.linkFarm "wireform-haddock" (map
          (name: { inherit name; path = hpDefault.${name}.doc; })
          (lib.attrNames wireformPackages));

        # Lint as `nix build`: runCommand derivations over the source tree.
        lintSrc = ./.;
        lintPaths = lib.concatMapStringsSep " " (p: p.path)
          (lib.attrValues ciPackages);
        hlintCheck = pkgs.runCommand "wireform-hlint"
          { nativeBuildInputs = [ pkgs.hlint ]; } ''
          cd ${lintSrc}
          hlint ${lintPaths}
          touch "$out"
        '';
        fourmoluCheck = pkgs.runCommand "wireform-fourmolu"
          { nativeBuildInputs = [ pkgs.haskellPackages.fourmolu pkgs.bash ]; } ''
          cd ${lintSrc}
          bash scripts/fourmolu-no-cpp.sh check ${lintPaths}
          touch "$out"
        '';

        checksAttrs = sandboxChecks // {
          hlint = hlintCheck;
          fourmolu = fourmoluCheck;
        };
      in
      {
        devShells = devShells // {
          default = devShells.${defaultGHC};
        };

        # Every per-format package is also exposed as a build
        # output for downstream Nix consumers. The umbrella
        # `wireform` is the default.
        #
        # Unsuffixed names use defaultGHC (ghc98). CI matrix jobs
        # use <pkg>-<shell> (e.g. wireform-core-ghc96).
        packages =
          let
            defaultPkgs = packagesForGhc defaultGHC;
            matrixPkgs = lib.foldl' (acc: shellName:
              let
                ghcAttr = ghcMatrix.${shellName};
                pkgs_ = packagesForGhc ghcAttr;
                suffixed = lib.mapAttrs'
                  (name: drv: {
                    name = "${name}-${shellName}";
                    value = drv;
                  })
                  pkgs_;
              in acc // suffixed
            ) {} (lib.attrNames ghcMatrix);
            # A single aggregate output that is the exact equivalent of the
            # Buildkite Build group (minus the Docker steps): for every
            # package in the CI registry, build `<pkg>-<ghc>` for each GHC in
            # that package's supported list (`ghcVersions`, already narrowed
            # by the per-package overrides in nix/ci/packages.nix). Building
            # `.#rollup` therefore forces precisely the same `nix build`
            # invocations Buildkite fans out, no more (no GHCs a package
            # can't build on) and no less. `linkFarm` keeps each component
            # addressable under the result symlink without re-deriving.
            rollupComponents = lib.concatMap
              (pkg: map
                (ghc: {
                  name = "${pkg.name}-${ghc}";
                  path = matrixPkgs."${pkg.name}-${ghc}";
                })
                pkg.ghcVersions)
              (lib.attrValues ciPackages);
            # The rollup also pulls in every synthetic CI output so a single
            # `nix build .#rollup` warms the cache for the *entire* pipeline:
            # the build matrix (above), the in-sandbox test checks, the lint
            # derivations, the compile-only benchmark builds, and the
            # aggregate haddock. Each stays individually addressable under
            # the result symlink.
            #
            # For cache warming, run `nix build .#rollup --keep-going`: some
            # components are gates that can legitimately fail (a failing test
            # check, or a soft lint), and --keep-going still builds and
            # pushes every other store path instead of aborting on the first
            # failure.
            ciComponents =
              (lib.mapAttrsToList (n: drv: { name = "check-${n}"; path = drv; })
                checksAttrs)
              ++ (lib.mapAttrsToList (n: drv: { name = n; path = drv; })
                benchPkgsAttrs)
              ++ [ { name = "haddock"; path = haddockAgg; } ];
            rollup = pkgs.linkFarm "wireform-rollup"
              (rollupComponents ++ ciComponents);
          in defaultPkgs // matrixPkgs // benchPkgsAttrs // {
            default = defaultPkgs.wireform;
            haddock = haddockAgg;
            inherit rollup;
          };

        # Each test suite (and the lint passes) as a `nix build .#checks.*`
        # target, so the Buildkite Test / Lint groups are pure `nix build`
        # steps instead of `nix develop … cabal test`.
        checks = checksAttrs;

        # Buildkite pipeline generators.
        # Preview locally:
        #   nix eval .#ci.main --json --arg changedFiles '["wireform-core/src/Foo.hs"]' | jq .
        #   nix eval .#ci.interop --json --arg changedFiles '["wireform-grpc/src/Foo.hs"]' | jq .
        ci = {
          main = args: import ./nix/ci/pipelines/main.nix ({ inherit lib; } // args);
          interop = args: import ./nix/ci/pipelines/interop.nix ({ inherit lib; } // args);
          packages = import ./nix/ci/packages.nix { inherit lib; };
          dsl = import ./nix/ci/dsl.nix { inherit lib; };
          changes = import ./nix/ci/changes.nix {
            inherit lib;
            packages = import ./nix/ci/packages.nix { inherit lib; };
          };
        };
      }
    );
}
