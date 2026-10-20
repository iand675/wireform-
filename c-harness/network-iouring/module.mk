# wireform-network/cbits/iouring_glue.c: the io_uring receive transport
# (Wireform.Network.Transport.IOUring, cabal flag `iouring`, Linux only).
MODULES += network-iouring
network-iouring_SRCS := $(ROOT)/wireform-network/cbits/iouring_glue.c
network-iouring_PLATFORMS := Linux
ifeq ($(UNAME_S),Linux)
network-iouring_CFLAGS := $(shell $(PKG_CONFIG) --cflags liburing)
network-iouring_LIBS := $(shell $(PKG_CONFIG) --libs liburing)
endif
network-iouring_TESTS := test_iouring
network-iouring_FUZZERS := fuzz_iouring_ops
# liburing is not MSan-instrumented, and the kernel fills the rings.
network-iouring_MSAN := no
