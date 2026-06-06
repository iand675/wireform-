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

        # GHC build matrix — add/remove entries to test against
        # different compilers. Shell names become `nix develop .#ghcXY`.
        ghcMatrix = {
          ghc96  = "ghc96";
          ghc98  = "ghc98";
          ghc910 = "ghc910";
          ghc912 = "ghc912";
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
          wireform-edi               = ./wireform-edi;
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
        };

        # Cabal flags to enable on specific packages. Mirrors the
        # `package <name>` blocks in `cabal.project`.
        packageFlags = {
          wireform = [ "snappy" "zstd" ];
          wireform-arrow = [ "zstd" "lz4" ];
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
        haskellOverlay = self: super:
          let
            mkPkg = name: src:
              applyFlags name
                (hlib.overrideCabal (drv: {
                  doBenchmark = false;
                  doCheck    = false;
                })
                  (self.callCabal2nix name src {}));
            perFormatAttrs = lib.mapAttrs mkPkg wireformPackages;
            wireformAttr = applyFlags "wireform"
              (hlib.overrideCabal (drv: { doBenchmark = false; })
                (self.callCabal2nix "wireform" ./. {}));
            hermesAttr = hlib.overrideCabal (drv: { doBenchmark = false; })
              (self.callCabal2nix "hermes" ./hermes {});
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
              hermes   = hermesAttr;
              crc32c   = crc32cUnrestricted;
              # Map pkg-config names (cabal `pkgconfig-depends`) and
              # bare C library names (cabal `extra-libraries`) to
              # system packages so cabal2nix-generated derivations
              # can find them.
              liblz4   = pkgs.lz4;
              lz4      = pkgs.lz4;
              snappy   = pkgs.snappy;
              libzstd  = pkgs.zstd;
              zstd     = pkgs.zstd;
              openssl  = pkgs.openssl;
            };

        mkDevShell = ghcAttr:
          let
            hp = (pkgs.haskell.packages.${ghcAttr}).override {
              overrides = haskellOverlay;
            };
            # Every package the workspace ships, so a single
            # `nix develop` shell can build any of them via
            # `cabal build <pkg>`.
            workspaceDrvs =
              lib.attrValues
                (lib.getAttrs (lib.attrNames wireformPackages) hp);
          in
          hp.shellFor {
            packages = _: workspaceDrvs;

            nativeBuildInputs = [
              hp.haskell-language-server
            ] ++ sharedTools;

            buildInputs = systemDeps;

            shellHook = ''
              echo "wireform dev shell — $(ghc --version)"
              echo "  packages: ${
                toString (lib.attrNames wireformPackages)} wireform"
            '';
          };

        devShells = builtins.mapAttrs (_: mkDevShell) ghcMatrix;
      in
      {
        devShells = devShells // {
          default = devShells.${defaultGHC};
        };

        # Every per-format package is also exposed as a build
        # output for downstream Nix consumers. The umbrella
        # `wireform` is the default.
        packages =
          let hp = (pkgs.haskell.packages.${defaultGHC}).override {
                overrides = haskellOverlay;
              };
              perFormat = lib.getAttrs (lib.attrNames wireformPackages) hp;
          in perFormat // {
            wireform = hp.wireform;
            default  = hp.wireform;
          };
      }
    );
}
