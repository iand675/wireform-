# wireform-core/cbits/fast_decode.c: protobuf varint / tag / packed /
# encode primitives, SWAR UTF-8 validation, SIMD byte scanners (NUL,
# ASCII, JSON escape, EDN whitespace), Iceberg bounds and Arrow buffer
# validation. Imported by wireform-core/src/Wireform/FFI.hs.
MODULES += core-fast-decode
core-fast-decode_SRCS := $(ROOT)/wireform-core/cbits/fast_decode.c
core-fast-decode_CFLAGS := -isystem $(ROOT)/wireform-core/include/simde -I$(ROOT)/wireform-core/include
core-fast-decode_OPT := -O3
core-fast-decode_TESTS := test_varint test_scan
core-fast-decode_FUZZERS := fuzz_varint fuzz_packed fuzz_encode fuzz_utf8 fuzz_scan \
  fuzz_compare_bounds fuzz_arrow_buffers
# native: simde lowers SSE2 to the host ISA (NEON on arm64).
# portable: simde's plain-C fallbacks, the path non-SIMD targets take.
core-fast-decode_VARIANTS := native portable
core-fast-decode_VFLAGS_portable := -DSIMDE_NO_NATIVE
