# wireform-websocket/cbits/wf_pmd.c: RFC 7692 permessage-deflate over zlib.
MODULES += websocket-pmd
websocket-pmd_SRCS := $(ROOT)/wireform-websocket/cbits/wf_pmd.c
websocket-pmd_CFLAGS := -I$(ROOT)/wireform-websocket/cbits $(shell $(PKG_CONFIG) --cflags zlib)
websocket-pmd_OPT := -O2
websocket-pmd_LIBS := $(shell $(PKG_CONFIG) --libs zlib)
websocket-pmd_TESTS := test_pmd test_fail
websocket-pmd_FUZZERS := fuzz_pmd_inflate fuzz_pmd_roundtrip
# native: the cabal build. chunked: zlib is handed at most 14 bytes of
# input / output per call (WF_PMD_CHUNK), driving the > 4 GiB splitting
# loops that the native build only reaches with multi-GiB buffers.
# fail: failure injection (common/wf_fail.h) - the shim's allocation and
# zlib entry points are renamed to failpoint-aware fakes (failfns.h/.c)
# so test_fail can drive the out-of-memory, constructor-failure and
# error-mapping paths.
websocket-pmd_VARIANTS := native chunked fail
websocket-pmd_VFLAGS_chunked := -DWF_PMD_CHUNK=14
websocket-pmd_VFLAGS_fail := -DWF_FAIL_VARIANT=1 \
  -include $(COMMON)/wf_fail.h \
  -include $(HARNESS_DIR)/websocket-pmd/failfns.h \
  -Dmalloc=wf_fail_malloc -Dcalloc=wf_fail_calloc \
  -D'inflateInit2_(a,b,c,d)=wf_fail_inflateInit2_(a,b,c,d)' \
  -D'deflateInit2_(a,b,c,d,e,f,g,h)=wf_fail_deflateInit2_(a,b,c,d,e,f,g,h)' \
  -D'inflateGetDictionary(a,b,c)=wf_fail_inflateGetDictionary(a,b,c)' \
  -D'inflateReset(a)=wf_fail_inflateReset(a)' \
  -D'deflateReset(a)=wf_fail_deflateReset(a)' \
  -D'deflate(a,b)=wf_fail_deflate(a,b)'
websocket-pmd_EXTRA := $(HARNESS_DIR)/websocket-pmd/failfns.c
# zlib is not MSan-instrumented.
websocket-pmd_MSAN := no
