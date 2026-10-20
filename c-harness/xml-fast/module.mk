# wireform-xml/cbits/fast_xml.c: SIMD XML scanners (hs_xml_find_*,
# hs_html_find_text_end, hs_xml_scan_structural).
MODULES += xml-fast
xml-fast_SRCS := $(ROOT)/wireform-xml/cbits/fast_xml.c
xml-fast_CFLAGS := -isystem $(ROOT)/wireform-xml/cbits/simde -I$(ROOT)/wireform-xml/cbits
xml-fast_ARCH := -march=native
xml-fast_OPT := -O3
xml-fast_TESTS := test_fast_xml
xml-fast_FUZZERS := fuzz_xml_scan fuzz_xml_structural
# native: simde lowers SSE2 to the host ISA (NEON on arm64, SSE/AVX on x86-64).
# portable: simde's plain-C fallbacks, the path non-SIMD targets take.
xml-fast_VARIANTS := native portable
xml-fast_VFLAGS_portable := -DSIMDE_NO_NATIVE
