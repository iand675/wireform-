# wireform-http1/cbits/http1_send.c: send(MSG_MORE), send-all and the
# portable sendfile shim, exercised over socketpairs and temp files.
# native: the platform primitive (Linux sendfile, Darwin sendfile).
# pread:  -DHS_HTTP1_SENDFILE_PREAD forces the portable pread+send
#         fallback the other POSIX platforms build.
# fail:   failure injection (common/wf_fail.h) - send and pread are
#         renamed to failpoint-aware fakes (failfns.h/.c) so test_fail
#         can drive the peer-closed and interrupted-transfer arms a
#         live stream socket cannot produce (send() never returns 0
#         for a non-empty buffer). The pread fallback is built here
#         too, so its send loop sees the same fakes. The renames are
#         object-like so the system headers' prototypes are rewritten
#         in place to declare the fakes.
# The _WIN32 branch (TransmitFile) cannot be built here.
MODULES += http1-send
http1-send_SRCS := $(ROOT)/wireform-http1/cbits/http1_send.c
http1-send_CFLAGS := -I$(ROOT)/wireform-http1/cbits
http1-send_OPT := -O3
http1-send_LIBS := -lpthread
http1-send_TESTS := test_http1_send test_fail
http1-send_FUZZERS := fuzz_send
http1-send_VARIANTS := native pread fail
http1-send_VFLAGS_pread := -DHS_HTTP1_SENDFILE_PREAD
http1-send_VFLAGS_fail := -DHS_HTTP1_SENDFILE_PREAD \
  -DWF_FAIL_VARIANT=1 \
  -include $(COMMON)/wf_fail.h \
  -include $(HARNESS_DIR)/http1-send/failfns.h \
  -Dsend=wf_fail_send -Dpread=wf_fail_pread
http1-send_EXTRA := $(HARNESS_DIR)/http1-send/failfns.c
http1-send_TSAN := yes
