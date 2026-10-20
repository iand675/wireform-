# wireform-http1/cbits/http1_scan.c: CR / LF / token / field-value
# scanners, ASCII case folding and the chunk-size hex parser.
# The source has a hand-written SSE2 branch (x86 / x86-64 only) and a
# portable scalar path; arm64 runs the scalar path, x86-64 CI the SSE2 one
# with the same tests (the scalar loops also handle every tail there).
MODULES += http1-scan
http1-scan_SRCS := $(ROOT)/wireform-http1/cbits/http1_scan.c
http1-scan_CFLAGS := -I$(ROOT)/wireform-http1/cbits
http1-scan_OPT := -O3
http1-scan_TESTS := test_http1_scan
http1-scan_FUZZERS := fuzz_scan fuzz_case fuzz_parse_hex fuzz_head
