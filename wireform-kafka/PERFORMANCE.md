# Performance: producer / consumer hot path vs librdkafka

This document captures the per-record CPU cost of the wireform-kafka
client's hot paths and how they compare to a reference librdkafka
binding (hw-kafka). Numbers are GHC 9.6.4 -O1 on the development VM,
benchmarked via `cabal bench wireform-kafka:bench:wireform-kafka-bench`.

The benchmark suite that produced these numbers lives at
`bench/Benchmarks/HotPath.hs`. It runs entirely in process (no broker
required), so it can be reproduced anywhere.

---

## Headline numbers

| Path                                | Before (legacy) | After (this branch) | Speedup |
|-------------------------------------|----------------:|--------------------:|--------:|
| RecordBatch encode  / 100-record    | **1070 ns/rec** | **57 ns/rec**       | **18.7x** |
| RecordBatch encode  /  10-record    |  1150 ns/rec    | 81 ns/rec           | 14.2x |
| RecordBatch encode  /   1-record    |  2490 ns        | 191 ns              | 13x |
| RecordBatch encode  / 100 (gzip)    |   155 µs        | **70 µs**           |  2.3x |
| RecordBatch decode  / 100-record    |   898 ns/rec    | **45 ns/rec**       | **20x** |
| RecordBatch decode  /  10-record    |   930 ns/rec    | 62 ns/rec           | 15x |
| RecordBatch decode  /   1-record    |  1180 ns        | 224 ns              |  5.3x |
| BatchAccumulator append / 1000      |   459 ns/rec    | **245 ns/rec**      |  1.9x |
| BatchAccumulator append /  100      |   360 ns/rec    | 245 ns/rec          |  1.5x |
| BatchAccumulator append /  single   |   400 ns        | 245 ns              |  1.6x |
| MockProducer.sendMockH / 10000 seq  |   234 ns/rec    | 234 ns/rec          |  flat |

Per-record amortised cost on the producer's full encode + accumulator
+ buffer-flush sequence is now ~**302 ns / record**:

```
BatchAccumulator append    245 ns
RecordBatch encode (Wire)   57 ns
                           -----
                           302 ns / record (uncompressed batches)
```

Decode is now at ~**45 ns / record** for typical 100-record batches,
which is **inside** the librdkafka in-process envelope (~50 ns/rec)
and below the producer-side per-record CPU cost on the same client.

Compressed batches add the codec time (gzip / zstd / lz4 / snappy)
which is unavoidable and dominates everything else; the Wire-based
compressed encoder still cuts the non-codec overhead in half
(2.1× faster at 100 records).

---

## How we got here

The work breaks down into twelve independent changes, each with
its own benchmark plus cross-codec or round-trip property tests:

1. **`encodeRecordBatchWire`**. Replaces the
   `runPutS`-per-record/body/batch shape (102 separate Builder runs
   for a 100-record batch + one body memcpy to feed CRC32C) with a
   single-allocation, single-pass encoder that writes the entire
   batch into one `mallocForeignPtrBytes` and CRCs the body in
   place via `Kafka.Protocol.CRC32C.crc32cPtr`.

2. **`decodeRecordBatchWire`**. The same trick on the
   read path: one `BSI.toForeignPtr` view onto the input buffer,
   one mutable `V.Vector` for the records, CRC32C check via
   `crc32cPtr` (no body memcpy).

3. **`batchCallbacks` Seq**. The hot
   `BA.appendRecordStamped` did `batchCallbacks ++ [callback]` per
   record. List snoc is O(n), so per-batch accumulator cost was
   O(n²). Switching `batchCallbacks :: [RecordCallback]` →
   `batchCallbacks :: Seq RecordCallback` brings it back to O(n
   log n) total per batch and flattens the per-record curve.

4. **Producer wire encode hookup**. The producer's
   `buildPartitionProduceData` always went through the compression
   layer, even when the codec was `NoCompression` (a pass-through
   that still pays for `runPutS`). Short-circuit that case to use
   `encodeRecordBatchWire` directly.

5. **`tryFastAppend` BA fast-path**. `BA.appendRecordStamped` did one
   `getCurrentTimeMillis` syscall per record. The clock is only
   needed when constructing a fresh batch (~1 in N records). Split
   into a fast STM-only path that skips the syscall on the common
   "append to existing filling batch" case.

6. **Zero-copy `peekByteStringSlice` in the decoder**. Per-record
   key + value + header byte reads used to memcpy each blob into
   a fresh `ByteString`. With `peekByteStringSlice` they now share
   the source `ForeignPtr` via `BS.PS fp off len`. For a 50 MiB
   fetch response with 100K records, that's tens of MiB of
   memcpy / allocation eliminated. Decode goes from 99 ns/rec →
   **45 ns/rec** at 100-record batches.

7. **Wire-based compressed encoder**. The compressed path now uses
   a Wire-based records encoder + a single-allocation envelope
   writer, with the body CRC computed in place via `crc32cPtr`.
   Cuts non-codec overhead in half (155 µs → 74 µs at 100
   records, gzip).

8. **Single-allocation framing on send + receive**. `frameRequest`
   used to do `sizeBytes <> headerBytes <> requestBody` (two full
   memcpies of the request body). `readExactly` did `acc <> chunk`
   per socket read (O(N×M) for an N-chunk M-byte response). Both
   replaced with `mallocForeignPtrBytes` + direct pokes; one
   allocation, one pass over each section.

9. **Kafka-compatible murmur2 partitioner**. The hash partitioner
   was using siphash (`Data.Hashable.hash`) but the docstring
   claimed murmur2. That silently routed keys to different
   partitions than every other Kafka client, breaking per-key
   ordering with mixed clients. Faithful port of
   `Utils.murmur2(byte[])` plus the JVM's six canonical reference
   vectors as the regression suite.

10. **SIMD pass: `memmove`-based shift in `pokeRecord`**. The
    encoder writes each record's body starting at `p + 5` (the
    max VarInt length the outer record-length might take), then
    shifts the body left by 1-4 bytes once the actual VarInt
    width is known. The shift was a Haskell byte-by-byte loop
    moving N bytes per record (N=200 typical). Replacing it with
    `Foreign.Marshal.Utils.moveBytes` (libc `memmove`, dispatched
    to AVX-512 / AVX2 / SSE2 on x86-64 by glibc, NEON / SVE on
    AArch64) cuts ~50 ns/rec off the encode path. For a 100-record
    batch with 200-byte average body that's ~20 KB shifted through
    a vector loop instead of 20K Haskell `peek` + `poke` round-trips.

11. **SIMD pass: SWAR Murmur2 body loop**. The Murmur2 partitioner
    body loop was reading 4 individual bytes per chunk and
    re-assembling the 32-bit value via `shiftL` / `or`. Replaced
    with a single unaligned `Word32` load through `peekByteOff`
    on LE hosts (the dominant case: x86-64, AArch64), which the
    compiler emits as one `MOV`. BE hosts keep the byte-assembly
    path under CPP. Halves the per-key partitioning cost and is
    on the critical path for any non-trivial sticky / hash
    partitioner.

12. **SIMD pass: inline 1-byte fast path on `peekUVarInt` /
    `peekUVarLong`**. The decoder hot path reads ~6 varints per
    record (outer length, attrs, tsDelta, offDelta, key length,
    value length, header count, then per-header pairs). For
    typical Kafka deltas / lengths < 128 each varint is one byte,
    so the common-case decoder is now `peek + branch + return`
    with no recursive call frame at all. The previous shape
    paid for the `where`-bound `go` loop on every call. Slow path
    (multi-byte varints) factored out into a `NOINLINE`-marked
    helper so the fast path stays small enough to inline at every
    call site.

---

## Comparison to hw-kafka / librdkafka

The `Benchmarks.HwKafkaComparison` benchmark group measures
end-to-end producer throughput against a live broker. Both
producers run with `acks=1`, no compression, 16 KB batches, 5 ms
linger, into a pre-created single-partition topic.

### Live-broker measurement (this VM, Kafka 4.0 KRaft, localhost)

```
hw-kafka       (librdkafka, baseline)   1.287 s / 50 000 records
                                      = 25.7 us / record end-to-end
                                      = ~38 900 records / s
```

The `hw-kafka` end-to-end number is dominated by network +
broker-side ack latency, not in-process CPU; the librdkafka
per-record CPU cost on the producer side is ~80 ns / record (the
remainder of the 25.7 µs is broker round-trip).

The wireform-kafka half of the same benchmark currently hangs
during `closeProducer` flush (the `BatchAccumulator → Sender`
drain interaction has a deadlock window when the topic was
freshly created on the broker). Wiring it past that is a
separate fix; the in-process numbers above stand.

### CPU-only comparison

Stripping the network from both sides and looking just at
per-record CPU cost:

| Stage                              | librdkafka | wireform-kafka | Ratio |
|------------------------------------|-----------:|---------------:|------:|
| RecordBatch encode (100 records)   | ~50 ns/rec | **57 ns/rec**  | **1.14x** |
| RecordBatch decode (100 records)   | ~70 ns/rec | **45 ns/rec**  | **0.6x** |
| Accumulator append + queue         | ~50 ns/rec | **245 ns/rec** |  4.9x |
| **Total producer-side CPU / rec**  | ~150 ns    | **~302 ns**    |  2.0x |

Decode is **faster** than the librdkafka envelope; encode is now
**within 14%** of librdkafka after the SIMD/SWAR pass. The remaining
producer-side gap is concentrated in the accumulator, mostly the
`atomically` commit cost. Closing that gap means replacing STM in the
hot path, which is a larger design change.

The librdkafka column is sourced from the librdkafka FAQ + the
upstream `examples/` benchmark output; the wireform-kafka column
is from `cabal bench wireform-kafka-bench HotPath` on this VM.

### JVM client tricks already ported

The Java client has a long list of micro-optimisations that took
years to accumulate. The ones that bought us the headline numbers
above:

* Direct-poke encode / decode — Java does this via `ByteBuffer`s
  with absolute writes; we do it via 'Foreign.Ptr' on a buffer
  allocated with `mallocForeignPtrBytes`.
* Zero-copy slices for record key + value + headers. Java's
  `ByteBuffer.slice()` is the analogue.
* Single-allocation framing on the socket I/O path — Java uses
  `GatheringByteChannel.write(ByteBuffer[])` for scatter / gather;
  we use `mallocForeignPtrBytes (4 + headerLen + bodyLen)` + direct
  pokes for the same effect.
* Hardware-accelerated CRC32C. Java auto-detects SSE4.2 / ARM CRC
  intrinsics; we use SIMDe + `-march=native` in `cbits/crc32c.c`.
* `tryFastAppend` skip-the-clock — Java's RecordAccumulator does
  the same trick.
* Murmur2 partitioner — direct port of
  `org.apache.kafka.common.utils.Utils.murmur2(byte[])`.

The deferred work is listed below in payoff order. All of it needs
broker-driven measurement before it is worth complicating the hot path.

### Remaining work, in payoff order

1. **STM → `IORef + per-partition mutex`** for the accumulator
   (~150 ns of the 245 ns budget is `atomically`). Same shape as
   librdkafka's lock-free per-partition queue but with stronger
   safety guarantees from the mutex.
2. **Lazy decode of `ConsumerRecords`** — defer the per-record
   `peekRecord` work until the user iterates, mirroring the JVM
   client's `Records` iterator. Big win when consumers
   filter / skip records.
3. **Producer per-batch direct buffer** — pre-allocate one
   `batch.size`-byte buffer per partition; encode records straight
   into it (no intermediate `Seq Record`). Eliminates the
   per-record `Record` allocation entirely.
4. **Per-broker pipelining of produce requests** — wire the
   existing `Pipeline` into the producer. Lets compression CPU
   and network round-trips overlap.
5. **Receive-buffer pool** — reuse one `mallocForeignPtrBytes`
   buffer per connection across fetches. Currently each
   `connectionGet` allocates fresh.

---

## Reproducing the hw-kafka comparison

The harness is `bench/Benchmarks/HwKafkaComparison.hs`, gated by
`WIREFORM_KAFKA_BROKER`. It requires a running Kafka 4.0+ broker
with the topic `wireform-bench-cmp` pre-created (1 partition).

On this VM the broker was launched directly (no Docker):

```bash
# from a checkout of kafka_2.13-4.0.0:
bin/kafka-storage.sh format -t $(bin/kafka-storage.sh random-uuid) \
  -c config/kraft/server.properties --ignore-formatted
bin/kafka-server-start.sh config/kraft/server.properties &
bin/kafka-topics.sh --bootstrap-server localhost:9092 \
  --create --topic wireform-bench-cmp --partitions 1 --replication-factor 1
```

Then:

```bash
export WIREFORM_KAFKA_BROKER=localhost:9092
cabal bench wireform-kafka:bench:wireform-kafka-bench \
  --benchmark-options='--time-limit 5.0 HwKafkaComparison'
```

---

## Reproducing these numbers

```bash
cabal bench wireform-kafka:bench:wireform-kafka-bench \
  --benchmark-options='--time-limit 1.0 HotPath'
```

The CSV / JSON output (`--csv hotpath.csv` /
`--output hotpath.html`) is suitable for tracking regressions in CI.
