---
title: wireform-network
description: "Magic-ring socket / TLS transports that feed the wireform parser surface directly. The shared receive path under wireform-kafka, wireform-http1, and wireform-http2."
sidebar:
  order: 4
---

`wireform-network` is the receive-side infrastructure shared by every
networked wireform package. It owns the **magic-ring transport**: a
double-mapped pinned buffer that the kernel writes recv data into and that
the wireform parser reads decoded values out of, with no intermediate
`ByteString` allocation and no per-call malloc. Built on top of it:

* `withRecvTransport` / `withRecvBufTransport` / `newRecvBufTransport`:
  the magic-ring `Wireform.Transport` constructors used by
  `wireform-kafka`, `wireform-http1`, and `wireform-http2` as the
  exclusive read path.
* `Wireform.Network.TLS.OpenSSL`: direct OpenSSL FFI that decrypts
  TLS plaintext into a caller-supplied pointer (i.e. the magic ring's
  backing memory), so the entire socket → TLS → parser pipeline runs
  copy-free.

## Why a magic ring

The classic Haskell socket recv path allocates a fresh pinned
`ByteString` per `recv()` call, hands it to the parser, and lets the
GC free it later. Parsers that need more bytes than fit in one
`recv()` then concatenate chunks, another allocation and copy. For a
parser that processes hundreds of thousands of small frames per
second (Kafka pipelining, HTTP/2 streams, HTTP/1.1 keep-alive) those
allocations dominate.

A **magic ring** sidesteps the whole thing. It is a power-of-two-sized
region of pinned memory mapped twice into adjacent virtual addresses
(Linux: `memfd_create` + two `mmap MAP_FIXED` calls; macOS and
Windows have equivalents). Any read of up to `N` bytes starting
anywhere in the first `[base, base + N)` page is contiguous in
virtual memory, because the MMU silently picks the second mapping
when the cursor crosses the wrap point. The parser never sees a
wrap-around boundary; the recv path never allocates a `ByteString`.

```
virtual address:   |XXXXXXXXXXXXXXXX|XXXXXXXXXXXXXXXX|
                   ^                ^                ^
                   base            +N (wrap)        +2N
physical memory:   shared FD page-mapped twice
```

The parser's `Wireform.Parser` (in `wireform-core`) is built around
this contract: `takeBs n` returns a zero-copy slice of the ring's
backing memory, valid until the transport's tail is advanced past it.

## Transport constructors

The transport surface is symmetric: a `ReceiveTransport` carries
parser-side reads, a `SendTransport` carries encoder-side writes,
and `DuplexTransport` pairs them on one underlying byte stream.

### Receive side

| Function | When to use |
|----------|-------------|
| `withReceiveTransport :: TransportConfig -> Socket -> (ReceiveTransport -> IO a) -> IO a` | TCP socket recv, bracket-scoped. The straightforward path. |
| `withReceiveBufTransport :: TransportConfig -> ReceiveFn -> (ReceiveTransport -> IO a) -> IO a` | Wrap any `Ptr Word8 -> Int -> IO Int` recv callback (TLS, in-memory test pipe, mock socket). Bracket-scoped. |
| `newReceiveBufTransport :: TransportConfig -> ReceiveFn -> IO ReceiveTransport` | Same as above but lifetime-managed by the caller; `receiveClose` unmaps the ring. |

### Send side (symmetric)

| Function | When to use |
|----------|-------------|
| `withSendTransport :: TransportConfig -> Socket -> (SendTransport -> IO a) -> IO a` | TCP socket send, bracket-scoped. The dual of `withReceiveTransport`. |
| `withSendBufTransport :: TransportConfig -> SendFn -> IO () -> (SendTransport -> IO a) -> IO a` | Wrap any `Ptr Word8 -> Int -> IO Int` send callback (TLS, in-memory test sink). The `IO ()` is the shutdown-write action. |
| `newSendBufTransport :: TransportConfig -> SendFn -> IO () -> IO SendTransport` | Lifetime-managed variant. |

Encoders interact with the send ring via the reservation API
exposed from `Wireform.Transport.Send`:

```haskell
reserveSend         :: SendTransport -> Int -> IO (Ptr Word8, Word64)
withSendReservation :: SendTransport -> Int -> (Ptr Word8 -> Int -> IO Int) -> IO Int
sendByteString      :: SendTransport -> ByteString -> IO ()
sendByteStringMany  :: SendTransport -> [ByteString] -> IO ()
sendBuilder         :: SendTransport -> Builder -> IO ()
```

### Duplex (paired on one wire)

| Function | When to use |
|----------|-------------|
| `withDuplexTransport :: TransportConfig -> Socket -> (DuplexTransport -> IO a) -> IO a` | The shape downstream `Connection` objects in `wireform-http1` / `wireform-http2` / `wireform-kafka` build on. |
| `newDuplexTransport :: TransportConfig -> Socket -> IO DuplexTransport` | Lifetime-managed variant. |
| `newDuplexPipe :: TransportConfig -> IO (DuplexTransport, DuplexTransport)` | In-memory paired duplex for tests; replaces the per-package `mkPipeTransport` variants. |

The accompanying `TransportConfig` selects the ring size (default
1 MiB; `Pipeline` callers in `wireform-kafka` configure 16 MiB to fit
typical Fetch responses) and an IO-manager wait policy.

`chunkedReceiveFn :: [ByteString] -> IO ReceiveFn` is a test fixture
that delivers a fixed chunk list one at a time then signals EOF.
The streaming-parser test suites in every downstream package use it
to drive the magic ring without a real socket pair.

## TLS-on-ring via OpenSSL

`Wireform.Network.TLS.OpenSSL` is the architecturally clean TLS path:
plaintext bytes flow from `libssl` straight into the magic ring's
backing memory with zero intermediate `ByteString` allocations.

```
libssl  (cbits/wireform_openssl.c)
   │  SSL_read_ex(ssl, dst, dst_len, &n)  ← writes plaintext into dst
   ▼
tlsRecvFn :: SslConn -> RecvFn            (Ptr Word8 -> Int -> IO Int)
   ▼
newRecvBufTransport / withRecvBufTransport (magic ring)
   ▼
Wireform.Transport → StreamingReader / FrameParser
```

The surface mirrors the bits OpenSSL exposes: `newClientCtx` /
`newServerCtx` for `SSL_CTX` construction with PEM cert + key load,
`setAlpnClient` / `setAlpnServer` for ALPN negotiation,
`newClient` / `newServer` to drive `SSL_connect` / `SSL_accept` with
`WANT_READ` / `WANT_WRITE` parked on the GHC IO manager,
`tlsRecvFn` for the magic-ring direct read path,
`tlsSend` for the symmetric write side, plus
`getAlpn` / `setClientHostnameVerify` for the usual ergonomics.

`withTlsRecvTransport :: TransportConfig -> SslConn -> (Transport -> IO a) -> IO a`
glues it all together: hands you a magic-ring `Transport` that the
streaming-parser readers in any wireform package can drive.

OpenSSL is the only TLS implementation in the repo: `wireform-kafka`,
`wireform-http1`, `wireform-http2` (both the new stack and the
vendored grapesy engine under `Network.HTTP2.Engine.*`), and
`wireform-grpc` all go through `Wireform.Network.TLS.OpenSSL`.
The pure-Haskell `tls` package + the `crypton-x509-*` family are
no longer dependencies anywhere.  The contrast versus the previous
arrangement:

| Concern | `tls` bridge | OpenSSL direct |
|---------|--------------|----------------|
| Plaintext into ring (no memcpy) | ✗ (one copy per record) | ✓ |
| Crypto implementation | `tls` (pure Haskell) | `libssl` (system) |
| Per-record allocation | 1 `ByteString` | 0 |
| External system dep | none | `libssl` |
| Auditability | pure Haskell | C |

See [`docs/tls-on-ring.md`](https://github.com/iand675/wireform-/blob/main/docs/tls-on-ring.md)
in the repo for the detailed design notes.

## Benchmarks: faster than the classic recv path

Three head-to-head benchmarks compared the classic recv-buffer +
parser path against the magic-ring + streaming-reader path on the
same workload, with the magic ring amortised outside the per-iteration
loop (rings are connection-scoped in production; a per-iteration
`mmap` would dwarf the parser cost we're trying to measure). All
numbers are per-iteration with criterion `--time-limit 2` on a
single x86_64 core:

### HTTP/1

| Workload | Classic `RecvBuffer + parseRequest` | Magic-ring `StreamingReader.readRequestHead` | Speedup |
|----------|---|---|---|
| Small request, whole chunk | 339 ns | 245 ns | **−28 %** |
| Big request (~1 KiB), whole chunk | 972 ns | 828 ns | **−15 %** |
| Big request, 64-byte recv chunks | 1.89 µs | 1.32 µs | **−30 %** |
| Big request, 4-byte recv chunks | 15.5 µs | 7.59 µs | **−51 %** |

The 4-byte-chunk case is where the gap is biggest: every wireform
parser pass through the same SIMD CRLFCRLF scanner the classic
parser uses, but the magic ring's double-mapping means we never
compact the recv buffer and the scanner picks up where the previous
round left off (`scanFrom` argument plumbed through
`findCRLFCRLF`). The classic recv buffer compacts on every refill
and the SIMD scan restarts from offset zero each time.

### HTTP/2

| Workload | Classic `RecvBuffer + decodeFrameHeader/Payload` | Magic-ring `Frame.StreamingReader.readFrameFrom` | Speedup |
|----------|---|---|---|
| 100 small DATA frames (11 byte body) | 1.98 µs | 2.16 µs | +9 % |
| 1000 small DATA frames | 22.4 µs | 22.1 µs | **−2 %** |
| 100 big DATA frames (1 KiB body) | 5.47 µs | 4.18 µs | **−24 %** |

For HTTP/2 the per-frame cost is dominated by the (already cheap)
9-byte header decode + payload slice; the magic ring path wins on
medium and large frames and is at parity on very small ones. The
small +9% on 100 small frames is criterion's per-batch overhead
divided across a small constant cost; the 1000-frame number (where
the per-frame cost is unambiguous) is a 1.5 % win.

### Kafka

| Workload | Classic `connectionGetExact + runGet` | Magic-ring `kafkaFrameParser` | Speedup |
|----------|---|---|---|
| 100 small frames (64 B body) | 15.0 µs | 5.59 µs | **−63 %** |
| 1000 small frames | 150 µs | 58.1 µs | **−61 %** |
| 100 big frames (4 KiB body) | 37.4 µs | 13.6 µs | **−64 %** |

Kafka shows the biggest win because the classic path
(`connectionGetExact` + `Data.Binary.Get.runGet`) allocates two
fresh `ByteString`s per frame (one for the length prefix, one for
the body) and walks the body's first 4 bytes through `runGet` to
extract the correlation id. The wireform pipeline parses the
length + correlation id with `anyInt32be` twice and returns a
zero-copy `takeBs` slice for the body. That's about 2.5-2.8×
faster end-to-end.

### Reproducing the numbers

The benchmarks themselves were removed once the migration completed
(the magic-ring path is the only recv path now in
`wireform-kafka`, `wireform-http1`, and `wireform-http2`). To
reproduce the comparison you'd reintroduce the classic path locally;
the head-to-head sources lived at
`wireform-{http1,http2,kafka}/bench/RecvVsTransport.hs` before the
removal commit.

## Magic-ring sizing

The ring's size sets a hard cap on the largest single `takeBs n`
the parser can ask for: requesting more than the ring holds
deadlocks the wait loop. Defaults reflect the worst-case in each
package:

| Package | Default ring size | Tuning knob | Rationale |
|---------|---|---|---|
| `wireform-network` (raw socket) | 1 MiB | `TransportConfig.ringSizeHint` | Generic; large enough for typical message frames. |
| `wireform-http1` `Connection` | 256 KiB | `newConnectionFromTransportWithRingSize` | h2o's 32 KiB header-block cap + several chunked-TE body chunks + room. |
| `wireform-http2` `Connection` | 1 MiB | (constant) | Well over the practical 16 KiB `SETTINGS_MAX_FRAME_SIZE`. |
| `wireform-kafka` `Pipeline` | 16 MiB | `PipelineConfig.pipelineRingSize` | Sized for typical Fetch responses; tune up to `fetch.max.bytes` for big workloads. |

Magic-ring virtual address space is cheap on Linux: only the pages
the recv path actually touches are paged in, so over-provisioning
has near-zero physical cost. A 16 MiB ring across 1000 idle Kafka
connections is 16 GiB of vmem but ~0 RSS.

## Stand-alone use

You don't need any of the HTTP / Kafka packages to use the magic
ring. The minimal idiom is:

```haskell
import Wireform.Network          (withRecvTransport, defaultTransportConfig)
import Wireform.Parser           (anyWord32be, takeBs)
import Wireform.Parser.Driver    (runParserLoop, LoopControl (..))

drainFrames :: Socket -> IO ()
drainFrames sock =
  withReceiveTransport defaultTransportConfig sock $ \t ->
    runParserLoop t lengthPrefixedFrame $ \body -> do
      handle body
      pure Continue
  where
    lengthPrefixedFrame = do
      len <- anyWord32be
      takeBs (fromIntegral len)
```

The handler receives `body` as a zero-copy slice of the ring; if
you need to retain it past the next loop iteration call
`Data.ByteString.copy` first.
