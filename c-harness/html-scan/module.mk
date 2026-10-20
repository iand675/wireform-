# wireform-html/cbits/html_scan.c: the HTML tokenizer's NEON/SSE2 scanners
# (text, tag name, attribute values, tag skipping, ASCII checks) and the
# escape finders used by HTML.Encode.
MODULES += html-scan
html-scan_SRCS := $(ROOT)/wireform-html/cbits/html_scan.c
html-scan_CFLAGS := -isystem $(ROOT)/wireform-html/cbits/simde
html-scan_ARCH := -march=native
html-scan_OPT := -O3
html-scan_TESTS := test_html_scan
html-scan_FUZZERS := fuzz_text fuzz_attr fuzz_tag fuzz_escape fuzz_tokenize
# native: the hand-written NEON (arm64) or SSE2 (x86-64) loops.
# portable: WIREFORM_HTML_SCAN_NO_SIMD, the scalar loops every other
# target compiles to.
html-scan_VARIANTS := native portable
html-scan_VFLAGS_portable := -DWIREFORM_HTML_SCAN_NO_SIMD
