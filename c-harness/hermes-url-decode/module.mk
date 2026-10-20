# hermes/cbits/url_decode.c: hermes_url_scan_special, hermes_url_decode.
MODULES += hermes-url-decode
hermes-url-decode_SRCS := $(ROOT)/hermes/cbits/url_decode.c
hermes-url-decode_OPT := -O3
hermes-url-decode_TESTS := test_url_decode
hermes-url-decode_FUZZERS := fuzz_url_decode fuzz_url_roundtrip
# native: the cabal build (-O3, no -march), i.e. the SSE2 scan on x86-64
# and NEON on arm64. avx2: the source's __AVX2__ branch, which the cabal
# build reaches when compiled with -mavx2; only on x86-64 hosts that can
# run it.
hermes-url-decode_VARIANTS := native
ifeq ($(UNAME_M),x86_64)
ifneq ($(shell grep -qw avx2 /proc/cpuinfo 2>/dev/null && echo yes),)
hermes-url-decode_VARIANTS += avx2
hermes-url-decode_VFLAGS_avx2 := -mavx2
endif
endif
