# wireform-columnar-core/cbits/byte_stream_split.c: bounded byte transposes.
MODULES += columnar-bss
columnar-bss_SRCS := $(ROOT)/wireform-columnar-core/cbits/byte_stream_split.c
columnar-bss_CFLAGS := -isystem $(ROOT)/wireform-columnar-core/include
columnar-bss_ARCH := -march=native
columnar-bss_OPT := -O2
columnar-bss_TESTS := test_transpose
columnar-bss_FUZZERS := fuzz_transpose
columnar-bss_VARIANTS := native portable
columnar-bss_VFLAGS_portable := -DSIMDE_NO_NATIVE
