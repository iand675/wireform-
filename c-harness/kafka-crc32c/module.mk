# wireform-kafka/cbits/crc32c.c: crc32c_init/append/finalize, crc32c.
# The cabal build passes -O3 -march=native so simde lowers
# simde_mm_crc32_u* to the hardware CRC32C instructions (SSE4.2 on
# x86-64, the ARMv8 CRC extension on arm64).
MODULES += kafka-crc32c
kafka-crc32c_SRCS := $(ROOT)/wireform-kafka/cbits/crc32c.c
kafka-crc32c_CFLAGS := -isystem $(ROOT)/wireform-kafka/cbits/simde -I$(ROOT)/wireform-kafka/cbits
kafka-crc32c_ARCH := -march=native
kafka-crc32c_OPT := -O3
kafka-crc32c_TESTS := test_crc32c
kafka-crc32c_FUZZERS := fuzz_crc32c
# native: hardware CRC32C via simde. portable: simde's table-free C
# fallback, the path a build without SSE4.2 / +crc takes.
kafka-crc32c_VARIANTS := native portable
kafka-crc32c_VFLAGS_portable := -DSIMDE_NO_NATIVE
