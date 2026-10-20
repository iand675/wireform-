# wireform-kafka/cbits/wireform_time.c: wireform_current_time_millis /
# wireform_current_time_micros (non-Windows only in the cabal file).
MODULES += kafka-time
kafka-time_SRCS := $(ROOT)/wireform-kafka/cbits/wireform_time.c
kafka-time_CFLAGS := -I$(ROOT)/wireform-kafka/cbits
kafka-time_ARCH := -march=native
kafka-time_OPT := -O3
kafka-time_TESTS := test_time
