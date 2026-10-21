/* Kernel API as wireform-core/src/Wireform/FFI.hs imports it, plus the
 * scalar reference implementations the tests compare against.
 *
 * Widths (the same split as wireform-html's and wireform-http1's
 * scanners): positions into a buffer are ptrdiff_t, pure byte counts are
 * size_t. Neither narrows to CInt on the Haskell side, so a ByteString or
 * scan range over 2 GiB is neither truncated nor overflowed:
 *
 *   hs_find_byte(buf, offset, len, target)
 *     offset  start position                        (ptrdiff_t)
 *     len     the buffer's length, used as the
 *             exclusive end of the scan             (ptrdiff_t)
 *     result  position of the match, or len on a miss (ptrdiff_t)
 *     Requires 0 <= offset; offset >= len is a miss that reads nothing.
 *
 *   hs_ws_mask(buf, len, mask)
 *     len     number of bytes XORed in place          (size_t)
 *
 * Both kernels were int/int-typed, which truncated a >2 GiB length (see
 * find_byte_beyond_2gib in test_fast_scan.c). The signatures are pinned
 * below at compile time, because hs_ws_mask touches every byte it is
 * given and so cannot be exercised at >2 GiB in a test.
 */
#ifndef CORE_FAST_SCAN_API_H
#define CORE_FAST_SCAN_API_H

#include <stddef.h>
#include <stdint.h>

ptrdiff_t hs_find_byte(const uint8_t *buf, ptrdiff_t offset, ptrdiff_t len,
                       uint8_t target_byte);
void hs_ws_mask(uint8_t *buf, size_t len, uint32_t mask);

/* Exact signature, or the default arm (0) if any width or qualifier moved. */
#define WF_SIG(fn, type) _Generic(&(fn), type: 1, default: 0)

_Static_assert(
    WF_SIG(hs_find_byte,
           ptrdiff_t (*)(const uint8_t *, ptrdiff_t, ptrdiff_t, uint8_t)),
    "hs_find_byte must take/return ptrdiff_t positions and a uint8_t target");
_Static_assert(
    WF_SIG(hs_ws_mask, void (*)(uint8_t *, size_t, uint32_t)),
    "hs_ws_mask must take a size_t byte count");

static inline ptrdiff_t ref_find_byte(const uint8_t *buf, ptrdiff_t offset,
                                      ptrdiff_t len, uint8_t t)
{
    for (ptrdiff_t i = offset; i < len; i++) {
        if (buf[i] == t) return i;
    }
    return len;
}

static inline void ref_ws_mask(uint8_t *buf, size_t len, uint32_t mask)
{
    for (size_t i = 0; i < len; i++) {
        buf[i] ^= (uint8_t)(mask >> (8 * (3 - (i & 3))));
    }
}

#endif
