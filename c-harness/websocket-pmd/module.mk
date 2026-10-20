# wireform-websocket/cbits/wf_pmd.c: RFC 7692 permessage-deflate over zlib.
MODULES += websocket-pmd
websocket-pmd_SRCS := $(ROOT)/wireform-websocket/cbits/wf_pmd.c
websocket-pmd_CFLAGS := -I$(ROOT)/wireform-websocket/cbits $(shell $(PKG_CONFIG) --cflags zlib)
websocket-pmd_OPT := -O2
websocket-pmd_LIBS := $(shell $(PKG_CONFIG) --libs zlib)
websocket-pmd_TESTS := test_pmd
websocket-pmd_FUZZERS := fuzz_pmd_inflate fuzz_pmd_roundtrip
# native: the cabal build. chunked: zlib is handed at most 7 bytes of
# input / output per call (WF_PMD_CHUNK), driving the > 4 GiB splitting
# loops that the native build only reaches with multi-GiB buffers.
websocket-pmd_VARIANTS := native chunked
websocket-pmd_VFLAGS_chunked := -DWF_PMD_CHUNK=7
# zlib is not MSan-instrumented.
websocket-pmd_MSAN := no
