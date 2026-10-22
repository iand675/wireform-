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
kafka-compress_TESTS := test_compress test_fail
kafka-compress_FUZZERS := fuzz_snappy fuzz_lz4f fuzz_gzip fuzz_zstd
# native: the cabal build. zchunk: zlib is handed at most 5 bytes of
# input / output per inflate() call (WF_DECOMPRESS_ZCHUNK), driving the
# > 4 GiB splitting loop with ordinary buffers. fail: failure injection
# (common/wf_fail.h) - the shims' allocation and library-constructor
# entry points are renamed to failpoint-aware fakes (failfns.h/.c) so
# test_fail can drive the out-of-memory error paths.
kafka-compress_VARIANTS := native zchunk fail
kafka-compress_VFLAGS_zchunk := -DWF_DECOMPRESS_ZCHUNK=5
kafka-compress_VFLAGS_fail := -DWF_FAIL_VARIANT=1 \
  -include $(COMMON)/wf_fail.h \
  -include $(HARNESS_DIR)/kafka-compress/failfns.h \
  -Dmalloc=wf_fail_malloc -Dcalloc=wf_fail_calloc \
  -D'inflateInit2_(a,b,c,d)=wf_fail_inflateInit2_(a,b,c,d)' \
  -D'LZ4F_createDecompressionContext(a,b)=wf_fail_LZ4F_createDecompressionContext(a,b)' \
  -D'LZ4F_compressFrame(a,b,c,d,e)=wf_fail_LZ4F_compressFrame(a,b,c,d,e)' \
  -D'LZ4F_decompress(a,b,c,d,e,f)=wf_fail_LZ4F_decompress(a,b,c,d,e,f)' \
  -D'snappy_compress(a,b,c,d)=wf_fail_snappy_compress(a,b,c,d)' \
  -D'ZSTD_getErrorName(a)=wf_fail_ZSTD_getErrorName(a)'
kafka-compress_EXTRA := $(HARNESS_DIR)/kafka-compress/failfns.c
# snappy / lz4 / zstd / zlib are not MSan-instrumented.
kafka-compress_MSAN := no
