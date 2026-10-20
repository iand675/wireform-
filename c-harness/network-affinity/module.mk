# wireform-network/cbits/wireform_affinity.c: sched_setaffinity_single,
# imported by Wireform.Network.Transport.Profile (Linux only, like the
# cabal c-sources entry). HsFFI.h comes from a local stub here.
MODULES += network-affinity
network-affinity_SRCS := $(ROOT)/wireform-network/cbits/wireform_affinity.c
network-affinity_CFLAGS := -I$(HARNESS_DIR)/network-affinity
network-affinity_TESTS := test_affinity
network-affinity_PLATFORMS := Linux
