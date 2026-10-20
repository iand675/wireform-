# wireform-core magic ring: hs_page_size, hs_ring_create, hs_ring_destroy.
# The cabal file builds magic_ring_linux.c (memfd) on Linux and
# magic_ring_posix.c (shm_open) on Darwin/BSD; the harness does the same.
# magic_ring_windows.c is not buildable here.
MODULES += core-magic-ring
ifeq ($(UNAME_S),Linux)
core-magic-ring_SRCS := $(ROOT)/wireform-core/cbits/magic_ring_linux.c
else
core-magic-ring_SRCS := $(ROOT)/wireform-core/cbits/magic_ring_posix.c
endif
core-magic-ring_CFLAGS := -I$(ROOT)/wireform-core/include
core-magic-ring_OPT := -O3
core-magic-ring_TESTS := test_magic_ring
core-magic-ring_FUZZERS := fuzz_ring_ops
# native: the kernel as cabal builds it.
# faults: every syscall the kernel makes (shm_open / memfd_create,
# ftruncate, mmap) goes through fault_inject.h so each error path can be
# forced deterministically and checked for leaks and errno.
core-magic-ring_VARIANTS := native faults
core-magic-ring_VFLAGS_faults := -DWF_FI=1 -include $(HARNESS_DIR)/core-magic-ring/fault_inject.h
# Concurrent create/destroy/hs_page_size from many threads (GHC calls these
# from several capabilities at once).
core-magic-ring_TSAN := yes
