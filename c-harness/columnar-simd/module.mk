# wireform-columnar-core/cbits/columnar_simd.c: Columnar.SIMD bit kernels,
# Arrow buffer validators, gathers and takes.
MODULES += columnar-simd
columnar-simd_SRCS := $(ROOT)/wireform-columnar-core/cbits/columnar_simd.c
columnar-simd_CFLAGS := -isystem $(ROOT)/wireform-columnar-core/include
# cabal: cc-options -march=native; ghc-options -O2 (passed on to the C compiler).
columnar-simd_ARCH := -march=native
columnar-simd_OPT := -O2
# The cabal stanza links lz4 for Columnar.LZ4 (Haskell bindings); this C file
# does not use it, so nothing is linked and MSan stays on.
columnar-simd_TESTS := test_bits test_validators
columnar-simd_FUZZERS := fuzz_bits fuzz_validators fuzz_view_refs fuzz_take
# native: simde lowers the SSE2 loads/stores in hs_columnar_memcpy_fast to the
# host ISA (NEON on arm64). portable: simde's plain-C fallbacks.
columnar-simd_VARIANTS := native portable
columnar-simd_VFLAGS_portable := -DSIMDE_NO_NATIVE
