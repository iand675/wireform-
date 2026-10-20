# wireform-kafka/cbits/{snappy_ffi,lz4_ffi,wireform_decompress}.c: the
# Kafka codec shims (decompression of untrusted broker bytes).
MODULES += kafka-compress
kafka-compress_SRCS := $(ROOT)/wireform-kafka/cbits/snappy_ffi.c \
  $(ROOT)/wireform-kafka/cbits/lz4_ffi.c \
  $(ROOT)/wireform-kafka/cbits/wireform_decompress.c
kafka-compress_CFLAGS := -I$(ROOT)/wireform-kafka/cbits \
  $(shell $(PKG_CONFIG) --cflags snappy liblz4 libzstd zlib)
kafka-compress_ARCH := -march=native
kafka-compress_OPT := -O3
kafka-compress_LIBS := $(shell $(PKG_CONFIG) --libs snappy liblz4 libzstd zlib)
kafka-compress_TESTS := test_compress
kafka-compress_FUZZERS := fuzz_snappy fuzz_lz4f fuzz_gzip fuzz_zstd
# native: the cabal build. zchunk: zlib is handed at most 5 bytes of
# input / output per inflate() call (WF_DECOMPRESS_ZCHUNK), driving the
# > 4 GiB splitting loop with ordinary buffers.
kafka-compress_VARIANTS := native zchunk
kafka-compress_VFLAGS_zchunk := -DWF_DECOMPRESS_ZCHUNK=5
# snappy / lz4 / zstd / zlib are not MSan-instrumented.
kafka-compress_MSAN := no
