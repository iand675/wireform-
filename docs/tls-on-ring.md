# TLS on the magic ring — architectural notes

## TL;DR

There are two TLS paths in wireform now:

* **`tls`-package bridge** (the legacy path).  Used by
  `Network.HTTP1.TLS.tlsTransport` and `Network.HTTP2.TLS.tlsTransport`.
  `tls`'s `recvData` returns plaintext as a fresh `ByteString` per
  TLS record; the per-package buffered-recv bridges memcpy it into
  the magic ring.  One extra copy + one fresh `ByteString` per
  record on top of the in-place AES-GCM `tls` already does
  internally.  No longer used anywhere in the repo — OpenSSL is
  the only TLS implementation now (including in the vendored
  grapesy engine + `wireform-grpc`).

* **Direct OpenSSL bridge** in
  `Wireform.Network.TLS.OpenSSL`.  Calls `libssl`'s `SSL_read_ex`
  directly via FFI; the destination buffer is the magic ring's
  backing memory, so plaintext is written into the ring with
  *zero* extra allocations or copies on the recv path.  This is
  the "TLS-on-ring" architectural shape we wanted; it works because
  OpenSSL's API exposes a Ptr-based recv whereas the Haskell `tls`
  package does not.

Both paths exist side-by-side; callers pick based on tradeoffs:

| Concern                       | `tls` bridge                | OpenSSL direct                                |
| ----------------------------- | --------------------------- | --------------------------------------------- |
| Decrypt-into-ring (no copy)   | ✗ (memcpy per record)       | ✓                                             |
| Crypto implementation         | `tls` (pure Haskell)        | `libssl` (system)                             |
| Cert verification             | full chain via crypton-x509 | system trust store via `SSL_CTX_set_default_verify_paths` |
| ALPN                          | ✓                           | ✓                                             |
| TLS 1.3                       | ✓                           | ✓                                             |
| External system dependency    | `crypton`                   | `libssl`                                      |
| Per-record allocation         | 1 `ByteString`              | 0                                             |

## The legacy bridge path

For HTTP/1, HTTP/2, and Kafka, TLS connections can plumb into the
magic-ring transport through the `tls`-package bridge:

```
TLS.Context
   │  recvData :: IO ByteString
   ▼
bufferedRecvTransport   (small leftover IORef-held ByteString)
   │  tRecvBuf :: Ptr Word8 -> Int -> IO Int
   ▼
withRecvBufTransport / newRecvBufTransport   (magic ring)
   │  Wireform.Transport
   ▼
Network.HTTP{1,2}.StreamingReader / Kafka.Network.FrameParser
```

The bridge `bufferedRecvTransport` lives in `Network.HTTP{1,2}.Transport`
and is used by `Network.HTTP1.TLS.tlsTransport` and
`Network.HTTP2.TLS.tlsTransport`.  Every `recvData` call yields a fresh
heap `ByteString` containing one TLS record's plaintext; the bridge
memcpys from that `ByteString` into the ring on each `tRecvBuf` call.

That's **one extra memcpy + one fresh `ByteString` per TLS record** on
top of the in-place decryption the `tls` package does internally.  At
~16 KiB per record and ~5 GB/s memcpy speed this is in the
sub-microsecond regime per record — measurable on benchmarks that
saturate plaintext throughput, but invisible on workloads where the
AES-GCM crypto or the application handler dominates.

## Why we don't decrypt directly into the ring today

The `tls` package's public API (`Network.TLS`,
`Network.TLS.Backend`) only exposes:

* `recvData :: Context -> IO ByteString` — high-level, returns decrypted
  plaintext as a fresh `ByteString`.
* `Backend { backendRecv :: Int -> IO ByteString,  backendSend :: ByteString -> IO () }` —
  the **encrypted** byte stream the TLS state machine reads from and
  writes to.

There is no `recvDataInto :: Context -> Ptr Word8 -> Int -> IO Int`
that would write plaintext bytes directly into a caller-supplied
buffer.  The decrypted bytes always come back as a `ByteString`
allocated by `tls` internals.

To actually decrypt straight into the magic ring we would need one of:

1. **Patch `tls` upstream** to add a Ptr-based recv.  The change is
   small in spirit (re-route the decrypt output into a caller-supplied
   buffer) but `tls`'s internal record-layer code is single-buffer:
   it currently writes into a fresh `ByteString` chunk because the
   decoded record size isn't known until after the AEAD tag is
   verified.  A correct implementation would either decrypt into a
   small staging buffer + memcpy to the caller's Ptr (saving one
   allocation, not the memcpy) or grow a caller-supplied resizable
   buffer (which complicates the API).  Probably accepted but a
   multi-week effort including upstream review.

2. **Reimplement the TLS record layer** on top of `crypton`'s
   AES-GCM / ChaCha20-Poly1305 primitives, delegating only the
   handshake to `tls`.  Roughly:
   * After handshake, extract the cipher state from `TLS.Context`
     via `Network.TLS.Internal` (if exposed; not entirely public
     today).
   * Read the 5-byte record header off the socket directly into the
     ring's "header buffer".
   * Read the ciphertext + tag straight into the ring at the
     plaintext offset.
   * Call AES-GCM-decrypt-in-place to overwrite the ciphertext with
     plaintext.
   * The parser sees plaintext at the same ring offset.

   This is the architecturally clean shape — no extra allocations,
   no memcpys, plaintext lives on the ring.  Estimated 1-2 weeks
   of dedicated work plus a sustained maintenance commitment
   (record-layer code talks to crypto primitives; bugs are
   security-sensitive).

3. **Use `kTLS`** — Linux's in-kernel TLS offload.  The application
   does the handshake; the kernel decrypts records and presents
   plaintext to userspace via the normal socket recv path.  The
   magic ring's existing `recvBuf` path then writes plaintext
   straight into the ring with zero extra work.  This is the
   smallest-code-change option but adds a kernel-version dependency
   (kTLS shipped in 4.13+ but the application-handshake APIs
   evolved through 5.x).

## The direct-OpenSSL path

`Wireform.Network.TLS.OpenSSL` is the ring-direct shape.  Layout:

```
libssl  (via cbits/wireform_openssl.c)
   │  SSL_read_ex(ssl, dst, dst_len, &n)
   ▼
tlsRecvFn :: SslConn -> RecvFn   (Ptr Word8 -> Int -> IO Int)
   │
   ▼
withRecvBufTransport / newRecvBufTransport   (magic ring)
   │  Wireform.Transport
   ▼
Network.HTTP{1,2}.StreamingReader / Kafka.Network.FrameParser
```

OpenSSL handles handshake (`SSL_connect` / `SSL_accept`),
cert verification (against the system trust store or with
`SslVerifyNone` for self-signed / test setups), SNI
(`SSL_set_tlsext_host_name`), ALPN (`SSL_CTX_set_alpn_protos` +
`SSL_get0_alpn_selected`), and TLS 1.3.  The decrypt-into-Ptr
read path is `SSL_read_ex` directly — no intermediate buffer on
the Haskell side, no `ByteString` allocation per record.

WANT_READ / WANT_WRITE are surfaced as `WF_SSL_WANT_RETRY` and
the Haskell layer parks on the GHC IO manager
(`threadWaitRead` / `threadWaitWrite`) before retrying, so the
thread doesn't busy-spin on partial reads.

The HTTP/1, HTTP/2, and Kafka connection layers can use this in
place of the `tls`-package bridge by constructing the magic-ring
transport via `Wireform.Network.TLS.OpenSSL.newTlsRecvTransport`
or `withTlsRecvTransport` instead of going through
`bufferedRecvTransport`.

## When to use which

* **Prefer OpenSSL direct** when you can rely on a system `libssl`
  being present, want to skip the per-record `ByteString`
  allocation, or need TLS 1.3 features (session resumption,
  0-RTT) that the Haskell `tls` package implements less aggressively.
* **Prefer the `tls` bridge** when you can't add a C dependency
  (statically-linked / sandbox-restricted deploys), need the
  exact certificate-validation semantics `crypton-x509` provides,
  or want a pure-Haskell crypto path for auditability.

Both are first-class — neither is going away.

## Historical: why the tls package alone wasn't enough

The first attempt at TLS-on-ring kept going through the Haskell
`tls` package and tried to find a Ptr-based recv inside its public
surface.  There isn't one — `recvData` and `Backend.backendRecv`
both return `ByteString`.  Three options to fix it from the `tls`
side, in roughly increasing order of effort, all of which we
rejected in favour of the OpenSSL direct path:

1. **Patch `tls` upstream** to add a Ptr-based recv.  Small in
   spirit; the internal record layer has to grow a caller-supplied
   buffer parameter.  Probably accepted but multi-week including
   upstream review.

2. **Reimplement the TLS record layer** on top of `crypton`'s
   AES-GCM / ChaCha20-Poly1305 primitives, delegating only the
   handshake to `tls`.  Architecturally cleanest but ~1–2 weeks
   of dedicated work plus a sustained maintenance commitment
   (record-layer code talks to crypto primitives; bugs are
   security-sensitive).

3. **Linux kTLS** — kernel decrypts records and presents
   plaintext to userspace via the normal socket recv path.  Our
   existing `recvBuf` path then writes plaintext straight into
   the ring with zero extra work.  Smallest code change but adds
   a kernel-version dependency and an ABI dance with `tls` /
   OpenSSL to surrender the cipher state after handshake.

OpenSSL exposes the right API up front (`SSL_read_ex`).  Pulling
in `libssl` is a smaller engineering bet than any of the above
and gets us the ring-direct shape today.

## Compression contrast

For comparison, **compression** is fully ring-direct via
`Kafka.Compression.Ring`:

  * **Source** is a raw `Ptr Word8 + Int` — typically a slice of
    the magic ring's backing memory.  No input-side `ByteString`
    allocation.
  * **Destination** is either:
      - a freshly-allocated `BSI.mallocByteString` sized exactly to
        the codec-reported output length (snappy + zstd, via the
        new direct C FFI in `cbits/wireform_decompress.c`); or
      - a **caller-supplied magic ring** (`decompressIntoRing`) that
        the C decompressor writes plaintext into in-place.  For
        gzip + lz4 — whose frame headers don't reliably encode the
        plaintext size — this lets the caller size the destination
        ring once at connection / batch scope and reuse it across
        decompressions with no per-call allocation.

The reason compression-on-ring is tractable where TLS-on-ring is
not: snappy / zstd / lz4 / zlib all expose `void*` / `Ptr` C APIs
that take a destination buffer; the `tls` package does not.  To get
TLS to the same shape we'd need either upstream surface changes,
a new record-layer implementation on top of crypton, or Linux kTLS
— see the three options above.

## Send-side pipeline

The send path is the mirror image of recv.  Encoders write frame
data into a double-mapped send ring; a consumer loop drains the
ring to the wire via `sendmsg` or `SSL_write_ex`.

```
Builder / sendByteString / hand-written encoder
   │  writes into send ring at [head, head+n)
   ▼
SendTransport  (magic ring, double-mapped)
   │  sendPublishHead(newHead)
   ▼
inline send loop  (sendmsg / SSL_write_ex)
   │  advances tail as bytes drain
   ▼
wire
```

`sendBuilderDirect` runs the builder directly into the ring via
the `RingSink` data sink — no intermediate `ByteString` allocation.
`withSendCork` defers publishing so headers + body coalesce into
a single `sendmsg` / io_uring SQE.

For TLS, the send consumer calls `SSL_write_ex` via the same
OpenSSL FFI used on the recv side.  The data flows from the
send ring (plaintext) through `libssl` (encryption) to the socket.
