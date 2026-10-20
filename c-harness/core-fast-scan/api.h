/* Kernel API as wireform-core/src/Wireform/FFI.hs imports it, plus the
 * scalar reference implementations the tests compare against. */
#ifndef CORE_FAST_SCAN_API_H
#define CORE_FAST_SCAN_API_H

#include <stdint.h>

int hs_find_byte(const uint8_t *buf, int offset, int len, uint8_t target_byte);
void hs_ws_mask(uint8_t *buf, int len, uint32_t mask);

static inline int ref_find_byte(const uint8_t *buf, int offset, int len, uint8_t t)
{
    for (int i = offset; i < len; i++) {
        if (buf[i] == t) return i;
    }
    return len;
}

static inline void ref_ws_mask(uint8_t *buf, int len, uint32_t mask)
{
    for (int i = 0; i < len; i++) {
        buf[i] ^= (uint8_t)(mask >> (8 * (3 - (i & 3))));
    }
}

#endif
