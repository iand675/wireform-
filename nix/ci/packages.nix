# Package registry for the wireform monorepo.
#
# Metadata (emoji, tier, codegen) is hand-maintained.
# Dependencies, test-suite presence, and benchmark presence are
# extracted automatically from each package's .cabal file at
# eval time via builtins.readFile — no IFD, no build during eval.
#
# To add a package: drop an entry into `metadata` with path,
# emoji, and tier.  deps / hasTests / hasBenchmarks are derived.
{ lib, src ? ../.. }:

let
  knownNames = builtins.attrNames metadata;

  # The full released GHC line the flake exposes (`<pkg>-<ghc>` outputs).
  # Keep in sync with `ghcMatrix` in flake.nix. (GHC 9.4 is excluded: the
  # core parser needs the PromptTag# primop, which is GHC 9.6+.)
  allGhcVersions = [ "ghc96" "ghc98" "ghc910" "ghc912" "ghc914" ];

  # Per-package GHC support overrides. A package absent from this map
  # builds across the full matrix. Entries here narrow the matrix for
  # packages with a known upstream incompatibility on some compiler so
  # CI does not emit jobs that cannot pass.
  #
  #   * text >= 2.1 first ships with GHC 9.8, so packages pinned to it
  #     cannot build on 9.4 / 9.6.
  #   * the proto-lens / ghc-source-gen toolchain (pulled in by the gRPC
  #     stack) does not compile on GHC 9.10+ in the pinned nixpkgs.
  ghcOverrides = {
    wireform-html = [ "ghc98" "ghc910" "ghc912" "ghc914" ];
    grpc-spec     = [ "ghc96" "ghc98" ];
    wireform-grpc = [ "ghc96" "ghc98" ];
  };

  # Word-boundary match: "wireform-core" must NOT match inside
  # "wireform-core-test" (the trailing hyphen is [a-zA-Z0-9-]).
  lineHasDep = depName: line:
    let padded = " ${line} ";
    in builtins.match ".*[^a-zA-Z0-9-]${depName}[^a-zA-Z0-9-].*" padded != null;

  # Classify cabal file lines by stanza context.
  # Only lines inside `library` or `common` stanzas (plus their
  # continuation lines) are used for dependency extraction.
  # test-suite / benchmark / executable stanzas are skipped so
  # test-only deps don't inflate the reverse-dep graph.
  libraryScope = allLines:
    let
      stanzaKeywords = [
        "library" "test-suite" "benchmark" "executable"
        "common" "flag" "source-repository" "custom-setup"
      ];

      isStanza = line: builtins.any (kw:
        builtins.match "${kw}([ \t].*)?" line != null
      ) stanzaKeywords;

      isKeepStanza = line:
        builtins.match "library([ \t].*)?" line != null
        || builtins.match "common .*" line != null;

      isIndented = line:
        line == "" || builtins.match "[ \t].*" line != null;

      result = builtins.foldl' (acc: line:
        if !(isIndented line) && isStanza line then
          { skip = !(isKeepStanza line); lines = acc.lines; }
        else if !(isIndented line) then
          # Top-level field (preamble) — skip for dep purposes
          { skip = true; lines = acc.lines; }
        else
          { inherit (acc) skip;
            lines = if acc.skip then acc.lines else acc.lines ++ [ line ];
          }
      ) { skip = true; lines = []; } allLines;
    in result.lines;

  # From a list of library-scope lines, extract only lines that
  # fall inside `build-depends:` blocks (the field plus its
  # indented continuations). Stops at the next field keyword.
  buildDependsLines = lines:
    let
      result = builtins.foldl' (acc: line:
        let
          isBD    = builtins.match "[ \t]*build-depends:.*" line != null;
          isField = builtins.match "[ \t]*[a-z][a-zA-Z-]*:.*" line != null;
        in {
          inBD  = if isBD then true
                  else if isField then false
                  else acc.inBD;
          lines = if isBD || acc.inBD
                  then acc.lines ++ [ line ]
                  else acc.lines;
        }
      ) { inBD = false; lines = []; } lines;
    in result.lines;

  readCabalInfo = name: meta:
    let
      cabalPath = src + "/${meta.path}/${name}.cabal";
      content = builtins.readFile cabalPath;
      allLines = lib.splitString "\n" content;

      # Library/common scope → build-depends fields only, comments stripped
      libLines = builtins.filter (l:
        l != "" && builtins.match "[ \t]*--.*" l == null
      ) (libraryScope allLines);

      depLines = buildDependsLines libLines;

      deps = builtins.filter (dep:
        dep != name && builtins.any (lineHasDep dep) depLines
      ) knownNames;

      # Test / benchmark detection uses ALL lines
      hasTests = builtins.any (l:
        builtins.match "test-suite .*" l != null
      ) allLines;

      hasBenchmarks = builtins.any (l:
        builtins.match "benchmark .*" l != null
      ) allLines;
    in { inherit deps hasTests hasBenchmarks; };

  # ------------------------------------------------------------------
  # Hand-maintained metadata.
  # Only fields that can't be derived from cabal files live here.
  # ------------------------------------------------------------------
  metadata = {
    # -- core --------------------------------------------------------
    wireform-core          = { path = "wireform-core";          emoji = ":gear:";             tier = "core"; };
    wireform-derive        = { path = "wireform-derive";        emoji = ":magic_wand:";       tier = "core"; };
    # sandboxHostileTest: doCheck deadlocks the nix build graph
    # (dependency-cycle), so its suite can't run as an in-sandbox
    # `nix build .#checks.<pkg>`; it stays a `nix develop`/interop concern.
    wireform-columnar-core = { path = "wireform-columnar-core"; emoji = ":bar_chart:";        tier = "core"; sandboxHostileTest = true; };
    wireform-kafka-protocol= { path = "wireform-kafka-protocol";emoji = ":kafka:";            tier = "core"; };

    # -- format ------------------------------------------------------
    wireform-proto         = { path = "wireform-proto";         emoji = ":label:";         tier = "format"; codegen = true; };
    wireform-avro          = { path = "wireform-avro";          emoji = ":books:";             tier = "format"; };
    wireform-thrift        = { path = "wireform-thrift";        emoji = ":ledger:";           tier = "format"; };
    wireform-cbor          = { path = "wireform-cbor";          emoji = ":bookmark_tabs:";             tier = "format"; };
    wireform-msgpack       = { path = "wireform-msgpack";       emoji = ":package:";          tier = "format"; };
    wireform-bson          = { path = "wireform-bson";          emoji = ":page_with_curl:";             tier = "format"; };
    wireform-ion           = { path = "wireform-ion";           emoji = ":dna:";              tier = "format"; };
    wireform-capnproto     = { path = "wireform-capnproto";     emoji = ":gift:";        tier = "format"; };
    wireform-flatbuffers   = { path = "wireform-flatbuffers";   emoji = ":bento:";      tier = "format"; };
    wireform-bond          = { path = "wireform-bond";          emoji = ":link:";             tier = "format"; };
    wireform-asn1          = { path = "wireform-asn1";          emoji = ":lock:";             tier = "format"; };
    wireform-xml           = { path = "wireform-xml";           emoji = ":newspaper:";              tier = "format"; };
    wireform-html          = { path = "wireform-html";          emoji = ":html:";             tier = "format"; };
    wireform-edn           = { path = "wireform-edn";           emoji = ":paperclip:";             tier = "format"; };
    wireform-bencode       = { path = "wireform-bencode";       emoji = ":open_file_folder:";          tier = "format"; };
    wireform-toml          = { path = "wireform-toml";          emoji = ":wrench:";             tier = "format"; };
    wireform-yaml          = { path = "wireform-yaml";          emoji = ":yaml:";             tier = "format"; };
    wireform-csv           = { path = "wireform-csv";           emoji = ":page_facing_up:";              tier = "format"; };
    wireform-ndjson        = { path = "wireform-ndjson";        emoji = ":json:";             tier = "format"; };
    wireform-fory          = { path = "wireform-fory";          emoji = ":truck:";             tier = "format"; };

    # -- columnar ----------------------------------------------------
    wireform-arrow         = { path = "wireform-arrow";         emoji = ":card_index_dividers:";            tier = "columnar"; };
    wireform-parquet       = { path = "wireform-parquet";       emoji = ":file_cabinet:";          tier = "columnar"; };
    wireform-orc           = { path = "wireform-orc";           emoji = ":card_file_box:";              tier = "columnar"; };
    wireform-columnar      = { path = "wireform-columnar";      emoji = ":abacus:";         tier = "columnar"; };
    wireform-iceberg       = { path = "wireform-iceberg";       emoji = ":ice_cube:";          tier = "columnar"; };
    wireform-delta         = { path = "wireform-delta";         emoji = ":ocean:";            tier = "columnar"; };
    wireform-lance         = { path = "wireform-lance";         emoji = ":floppy_disk:";            tier = "columnar"; };
    wireform-hudi          = { path = "wireform-hudi";          emoji = ":ship:";             tier = "columnar"; };

    # -- network -----------------------------------------------------
    wireform-network       = { path = "wireform-network";       emoji = ":electric_plug:";    tier = "network"; };
    hermes                 = { path = "hermes";                 emoji = ":envelope:";         tier = "network"; };
    wireform-http1         = { path = "wireform-http1";         emoji = ":signal_strength:";             tier = "network"; };
    wireform-http2         = { path = "wireform-http2";         emoji = ":signal_strength:";             tier = "network"; };
    wireform-http          = { path = "wireform-http";          emoji = ":globe_with_meridians:"; tier = "network"; };
    wireform-http-wai      = { path = "wireform-http-wai";      emoji = ":bridge_at_night:";  tier = "network"; };
    wireform-websocket     = { path = "wireform-websocket";     emoji = ":satellite:";        tier = "network"; };
    grpc-spec              = { path = "grpc-spec";              emoji = ":satellite_antenna:";             tier = "network"; };
    wireform-grpc          = { path = "wireform-grpc";          emoji = ":satellite_antenna:";             tier = "network"; };
    wireform-kafka         = { path = "wireform-kafka";         emoji = ":kafka:";            tier = "network"; };
    wireform-hw-kafka-client = { path = "wireform-hw-kafka-client"; emoji = ":kafka:";        tier = "network"; };
    wireform-lattice       = { path = "wireform-lattice";       emoji = ":spider_web:";       tier = "network"; };

    # -- tool --------------------------------------------------------
    wireform-attoparsec    = { path = "wireform-attoparsec";    emoji = ":bridge_at_night:";  tier = "tool"; };
    wireform-binary        = { path = "wireform-binary";        emoji = ":bridge_at_night:";  tier = "tool"; };
    wireform-cereal        = { path = "wireform-cereal";        emoji = ":bridge_at_night:";  tier = "tool"; };
    wireform-stats         = { path = "wireform-stats";         emoji = ":bar_chart:";        tier = "tool"; };
    wireform-cel           = { path = "wireform-cel";           emoji = ":scroll:";           tier = "tool"; };
    wireform-protovalidate = { path = "wireform-protovalidate"; emoji = ":shield:";           tier = "tool"; };
    wireform-dst           = { path = "wireform-dst";           emoji = ":game_die:";         tier = "tool"; };
    wireform-dst-net       = { path = "wireform-dst-net";       emoji = ":game_die:";         tier = "tool"; };
    wireform-state-machine = { path = "wireform-state-machine"; emoji = ":traffic_light:";    tier = "tool"; };
  };

  # Merge hand-maintained metadata with auto-extracted cabal info,
  # plus the resolved per-package GHC support list.
  packages = builtins.mapAttrs (name: meta:
    { inherit name;
      ghcVersions = ghcOverrides.${name} or allGhcVersions;
    } // meta // (readCabalInfo name meta)
  ) metadata;

in packages
