MODULES += lzo
lzo_SRCS := $(ROOT)/wireform-columnar-core/cbits/lzo.c
lzo_ARCH := -march=native
lzo_OPT := -O2
lzo_TESTS := test_lzo
lzo_FUZZERS := fuzz_decode fuzz_compress
