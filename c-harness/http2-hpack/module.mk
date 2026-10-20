# wireform-http2/cbits/hpack_huffman.c: HPACK Huffman encode, encode_len
# and decode (RFC 7541 Appendix B). Plain C, no SIMD, no cc-options in the
# cabal file, so one variant at the default optimisation level.
MODULES += http2-hpack
http2-hpack_SRCS := $(ROOT)/wireform-http2/cbits/hpack_huffman.c
http2-hpack_CFLAGS := -I$(ROOT)/wireform-http2/cbits
http2-hpack_TESTS := test_hpack_huffman test_hpack_table
http2-hpack_FUZZERS := fuzz_decode fuzz_encode
