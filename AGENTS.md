# Cursor Cloud agent instructions

See [agents.md](agents.md) for full wireform development guidelines (codegen, performance, module layout).

## Cursor Cloud specific instructions

### Toolchain on the VM

GHC and Cabal come from [ghcup](https://www.haskell.org/ghcup/) (`~/.ghcup/env`). After a fresh VM, if `ghc` is missing from `PATH`, run `source "$HOME/.ghcup/env"` (or open a login shell that sources `~/.bashrc`).

Ubuntu packages required before the first `cabal build all` (match `.github/workflows/ci.yml` plus OpenSSL for `wireform-network`):

```bash
sudo apt-get install -y build-essential libgmp-dev libffi-dev libncurses-dev \
  zlib1g-dev libnuma-dev xz-utils pkg-config protobuf-compiler \
  libsnappy-dev liblz4-dev libzstd-dev libbrotli-dev librdkafka-dev libssl-dev
```

HLint for local lint: `sudo apt-get install -y hlint` (distro build; CI uses a newer pin via `haskell-actions/setup`).

### Build and test (default workflow)

From repo root:

```bash
cabal update
cabal configure --enable-tests --enable-benchmarks
cabal build all -j2 --ghc-options="-j2"   # first build ~25–40 min on 2-core VMs
cabal test wireform-test --test-show-details=streaming
```

Subsequent builds reuse `~/.cabal/store` and are much faster.

**Hello-world executables** (no external services):

- `cabal run example-derive` — one ADT encoded to proto, CBOR, MsgPack, JSON
- `cabal run example-msgpack` — schema-less roundtrip
- `cabal run payments-pipeline -- demo` — Kafka Streams topology in `TopologyTestDriver` (no broker)

### Optional services

| Service | Start | Notes |
|---------|--------|--------|
| Kafka (integration tests) | `docker compose -f wireform-kafka/test-integration/docker-compose.yml up -d` | Then `WIREFORM_KAFKA_BROKER=localhost:9092` for relevant `cabal test` targets |
| Docs site | `cd website && npm install && npm run dev` | http://localhost:4321/wireform-/ |
| Nix dev shell | `nix develop` or `nix develop .#ghc98` | Alternative to ghcup; provides fourmolu, prek, native libs |

### Gotchas

- **`loadProto` splices need `DataKinds`:** Any Cabal stanza that calls `loadProto` must enable `DataKinds` (wired into `wireform.cabal` / `wireform-proto.cabal` defaults). Open-enum wire values use a synthetic `Type''Unrecognized` constructor, not `Type'Unknown`.
- **First build is slow** — use `-j2` on small cloud VMs; see [agents.md — Toolchain](agents.md#toolchain).
- **Heavy optional flags** (`+python-interop`, `+dataframe-bridge`, etc.) are off by default; see the Cabal flags table in [agents.md](agents.md#cabal-flags-worth-knowing).
