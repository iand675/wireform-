# wireform-core/cbits/base64.c: hs_base64_encode, hs_base64_decode and the
# two length helpers (Wireform.Base64).
MODULES += core-base64
core-base64_SRCS := $(ROOT)/wireform-core/cbits/base64.c
core-base64_CFLAGS := -isystem $(ROOT)/wireform-core/include/simde -I$(ROOT)/wireform-core/include
core-base64_OPT := -O3
core-base64_TESTS := test_base64
core-base64_FUZZERS := fuzz_base64_encode fuzz_base64_decode
# native: simde lowers SSE2/SSSE3 to the host ISA (NEON on arm64; on x86-64
# the cabal build has no -mssse3, so PSHUFB goes through simde's SSE2 code).
# portable: simde's plain-C fallbacks.
# ssse3 (x86-64 only): the real PSHUFB path a -march=native build would take.
core-base64_VARIANTS := native portable
core-base64_VFLAGS_portable := -DSIMDE_NO_NATIVE
ifeq ($(UNAME_M),x86_64)
core-base64_VARIANTS += ssse3
core-base64_VFLAGS_ssse3 := -mssse3
endif
