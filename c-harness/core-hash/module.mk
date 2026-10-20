# wireform-core/cbits/wireform_hash_simd.c: Murmur3-32, Iceberg bucket,
# XXH64, Roaring container kernels (Wireform.Hash).
MODULES += core-hash
core-hash_SRCS := $(ROOT)/wireform-core/cbits/wireform_hash_simd.c
core-hash_CFLAGS := -isystem $(ROOT)/wireform-core/include/simde -I$(ROOT)/wireform-core/include
core-hash_OPT := -O3
core-hash_TESTS := test_hash test_roaring
core-hash_FUZZERS := fuzz_murmur3 fuzz_xxh64 fuzz_roaring_array fuzz_roaring_bitset
# native: simde lowers SSE2/SSE4.1 to the host ISA (NEON on arm64).
# portable: simde's plain-C fallbacks, the path non-SIMD targets take.
core-hash_VARIANTS := native portable
core-hash_VFLAGS_portable := -DSIMDE_NO_NATIVE
