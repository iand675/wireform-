/* Shared helpers for the kafka-compress fuzz targets. */
#ifndef KAFKA_COMPRESS_FUZZ_COMMON_H
#define KAFKA_COMPRESS_FUZZ_COMMON_H

#include "harness.h"
#include "api.h"

/* Reference decoders stop after this much output (bombs). */
enum { FUZZ_LIMIT = 1 << 24 };

/* Output capacity from a selector byte: small fixed values, or the
 * reference size give or take one (when known). */
static inline size_t fuzz_cap(uint8_t sel, int ref_ok, size_t ref_n)
{
    if (sel & 0x80) {
        size_t base = ref_ok == 1 ? ref_n : 4096;
        size_t d = sel & 3; /* 0, 1, 2, 3 -> -1, 0, +1, +2 */
        if (d == 0) return base ? base - 1 : 0;
        return base + d - 1;
    }
    return (size_t)(sel & 0x7f) * 37;
}

static inline size_t min_sz(size_t a, size_t b) { return a < b ? a : b; }

#endif
