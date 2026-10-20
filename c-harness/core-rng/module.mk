# wireform-core/cbits/fast_rng.c: hs_xoshiro256pp_next (Wireform.FFI
# fastRandomWord64) and hs_xoshiro256pp_reseed.
MODULES += core-rng
core-rng_SRCS := $(ROOT)/wireform-core/cbits/fast_rng.c
core-rng_OPT := -O3
core-rng_TESTS := test_rng
core-rng_FUZZERS := fuzz_rng
# Per-thread state and a fork handler: run the tests under TSan too.
core-rng_TSAN := yes
ifeq ($(UNAME_S),Linux)
# dlsym(RTLD_NEXT) in entropy_fake.h; pthread_atfork in the kernel.
core-rng_LIBS := -ldl -pthread
endif
