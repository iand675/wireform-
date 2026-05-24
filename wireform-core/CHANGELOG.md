# Changelog for wireform-core

## Unreleased

* `Wireform.Builder.FastBuilder` -- new `RingSink` data sink variant.
  The builder writes directly into a `SendTransport`'s double-mapped
  ring buffer with no intermediate `ByteString` allocation or memcpy.
  On overflow, already-written bytes are published and the builder
  continues into freshly-available ring space.

* `Wireform.Transport.Send` -- new `sendBuilderDirect`: the default
  builder→ring path.  `sendBuilder` now delegates to it.  The old
  materialize-then-copy path is `sendBuilderViaByteString` (retained
  for benchmarks).

* `Wireform.Transport.Send` -- new `withSendCork`: defers
  `sendPublishHead` so multiple sends batch into one publish (one
  `sendmsg` / one io_uring SQE).  Demand-driven: only breaks cork
  when the ring is genuinely full.

* `Wireform.Ring` -- gave `MagicRing` a phantom type parameter `s`
  modelled after `Control.Monad.ST.ST`.  `withMagicRing` is now
  rank-2 (`Int -> (forall s. MagicRing s -> IO a) -> IO a`), which
  seals `s` inside the ring's scope.  New `RingSlice s` type (a
  pointer + length tagged with the ring's `s`) replaces ad-hoc
  `ByteString` slicing for callers that want type-system-enforced
  safety against dangling references after a refill.  `copyRingSlice
  :: RingSlice s -> IO ByteString` is the explicit escape hatch.

* `Wireform.Transport` -- replaced the typed `transportRing ::
  MagicRing` field with three raw fields (`transportRingBaseField`,
  `transportRingSizeField`, `transportRingMaskField`).  This keeps
  `Transport` un-parameterised so the new `s` does not have to
  cascade through every downstream `Transport`-using package.  The
  existing `transportRing :: Transport -> MagicRing s` getter
  remains for backwards compatibility but is polymorphic in `s`, i.e.
  un-scoped from a safety standpoint.  Transport constructors must
  populate the three raw fields explicitly.

* `Wireform.Parser` -- `takeBs` and `takeBsCopy` now drain any byte
  count, including counts larger than the magic ring's capacity.
  Reads that fit in the ring keep their existing fast path
  (zero-copy slice or single memcpy).  Reads that exceed the ring
  allocate a fresh pinned 'ByteString' and chunk-copy through the
  ring, calling 'modeCheckpoint' between chunks so the producer has
  room to keep refilling.  No deadlock, no `ParseRingOverflow` for
  these primitives — the caller gets the full byte string they
  asked for.  Primitives that cannot be drained safely (literal
  `byteString` matches, `isolate`, raw `ensureN#`) still surface
  `ParseRingOverflow` when their size exceeds the ring.

* `Wireform.Parser.Driver` / `Wireform.Parser.Error` -- detect the
  case where the streaming parser asks (via `ensureN#`, `byteString`,
  or `isolate`) for more bytes than the ring can ever hold and
  short-circuit with a new `ParseRingOverflow` error variant
  (position, requested bytes, ring size).  Without this guard the
  wait loop spins forever on a no-progress @MoreData@ from the
  transport because the producer cannot make room and the consumer
  is suspended waiting for it.

* `Wireform.Parser.Driver` -- fix a latent wrap bug in the
  StepSuspend / StepCheckpoint resume paths.  The driver used to
  compute the resumed eob as @base + (head .&. mask)@, which
  collapses onto cur whenever the producer has filled exactly one
  ring-worth of bytes since the suspension (because the masked
  offsets coincide).  The parser would then see zero bytes available
  even though `head - cur == ringSize`.  Replaced with
  @newCur + (head - pos)@, which correctly places eob in the second
  mapping when wrap happens.  Same fix applied to the initial eob
  in `runParserInternal`.

* `Wireform.Parser.Internal.ParserEnv` -- replaced the immutable
  `peStartPos` / `peInitCur` fields with mutable `peAnchorPos` /
  `peAnchorCur` cells, plus matching `writeAnchor` / `writeAnchor#`
  helpers.  Whenever the driver wraps the parser's cur back into the
  first mapping (StepCheckpoint or StepSuspend resume), it now also
  re-anchors the env so that `curToPos` keeps producing the correct
  absolute position even after the wrap.  `curToPos` is now
  State#-threaded (no behavioural change for ordinary callers — the
  existing call sites all already have a State# in scope).  Callers
  that constructed `ParserEnv` directly (driver, parseByteString,
  Stateful) now stack-allocate a 24-byte cells buffer (via
  `allocaBytes`) instead of `bracket (mallocBytes 8) free`.  Removes
  a C `malloc` + `free` round-trip per `runParser` call on the
  streaming path.

## 0.1.0.0 -- 2026

Initial release.

* `Wireform.FFI` -- C FFI surface for the per-format encoders /
  decoders:
    * Packed-varint pre-scan (`countPackedVarints`,
      `packedAllSingleByte`).
    * SWAR UTF-8 validation (`validateUtf8SWAR`).
    * Branchless 8-byte varint decode (`decodeVarintSWAR`).
    * Page-boundary relocation helper (`relocatePageBoundary`).
    * C-native scalar field encoders (`encodeLengthDelimitedC`,
      `encodeVarintFieldC`, `encodeBoolFieldC`).
    * SIMD NUL / byte / ASCII / JSON-escape / EDN-whitespace
      scanners (`findNul`, `findByte`, `isAscii`,
      `findJsonEscape`, `skipWhitespace`).
    * SIMD Iceberg partition-bounds comparison (`compareBounds`).
    * SIMD Arrow IPC buffer validation
      (`validateArrowBuffers`).
    * BE / LE endianness helpers (`readBE16H` … `writeLE64H`).
    * Text decoding via the `text` library's bundled simdutf
      (`decodeTextFast`).
* `Wireform.Encode.Direct` -- shared direct-write encode buffer
  primitive (`Buf`) for the per-format encoders.
* `Wireform.Hash` -- SIMD-accelerated hashing helpers.

C sources live in `cbits/`; the vendored `simde` headers ship in
`include/simde/`.
