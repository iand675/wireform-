# wireform-core/cbits/fast_scan.c: hs_find_byte, hs_ws_mask.
MODULES += core-fast-scan
core-fast-scan_SRCS := $(ROOT)/wireform-core/cbits/fast_scan.c
core-fast-scan_CFLAGS := -isystem $(ROOT)/wireform-core/include/simde -I$(ROOT)/wireform-core/include
core-fast-scan_OPT := -O3
core-fast-scan_TESTS := test_fast_scan
core-fast-scan_FUZZERS := fuzz_find_byte fuzz_ws_mask
# native: simde lowers SSE2 to the host ISA (NEON on arm64).
# portable: simde's plain-C fallbacks, the path non-SIMD targets take.
core-fast-scan_VARIANTS := native portable
core-fast-scan_VFLAGS_portable := -DSIMDE_NO_NATIVE
