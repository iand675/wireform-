# Generic packed-integer/FOR scaling and IEEE scatter kernels.
MODULES += packed-float
packed-float_SRCS := $(ROOT)/wireform-columnar-core/cbits/packed_float.c
packed-float_CFLAGS := -isystem $(ROOT)/wireform-columnar-core/include
packed-float_ARCH := -march=native
packed-float_OPT := -O2
packed-float_TESTS := test_alp
packed-float_FUZZERS := fuzz_alp
packed-float_VARIANTS := native portable
packed-float_VFLAGS_portable := -DSIMDE_NO_NATIVE
