# wireform-http1/cbits/http1_send.c: send(MSG_MORE), send-all and the
# portable sendfile shim, exercised over socketpairs and temp files.
# native: the platform primitive (Linux sendfile, Darwin sendfile).
# pread:  -DHS_HTTP1_SENDFILE_PREAD forces the portable pread+send
#         fallback the other POSIX platforms build.
# The _WIN32 branch (TransmitFile) cannot be built here.
MODULES += http1-send
http1-send_SRCS := $(ROOT)/wireform-http1/cbits/http1_send.c
http1-send_CFLAGS := -I$(ROOT)/wireform-http1/cbits
http1-send_OPT := -O3
http1-send_LIBS := -lpthread
http1-send_TESTS := test_http1_send
http1-send_FUZZERS := fuzz_send
http1-send_VARIANTS := native pread
http1-send_VFLAGS_pread := -DHS_HTTP1_SENDFILE_PREAD
http1-send_TSAN := yes
