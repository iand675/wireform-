/* Kernel API of hermes/cbits/url_decode.c, plus a scalar reference
 * percent-decoder and encoder for differential and round-trip tests.
 *
 * Haskell imports (hermes/src/Network/HTTP/URL/Decode.hs), both
 * `foreign import ccall unsafe`:
 *
 *   hermes_url_scan_special :: Ptr Word8 -> Int -> Int -> IO Int
 *   hermes_url_decode       :: Ptr Word8 -> Int -> Ptr Word8 -> Int -> IO Int
 *
 * Haskell Int is 64-bit, matching size_t / ptrdiff_t. plus_is_space is
 * an Int on the Haskell side and an int here; the callers only pass 0 or
 * 1 (boolFlag, decodeWith's mode), and both x86-64 SysV and AAPCS64 pass
 * it in a register whose low 32 bits the C side reads.
 *
 * Caller contract:
 *
 *   hermes_url_scan_special(src, len, plus_is_space): reads src[0 .. len)
 *     and returns the index of the first '%' (or '+' when plus_is_space
 *     is nonzero), or len. firstSpecialOffset calls it on any ByteString,
 *     including the empty one, whose pointer may be NULL: (NULL, 0) must
 *     return 0 without touching memory.
 *
 *   hermes_url_decode(src, srclen, dst, plus_is_space): decodeWith calls
 *     it only for srclen > 0, on the suffix starting at the first special
 *     byte, with dst a fresh, distinct buffer of srclen bytes (the
 *     createUptoN buffer offset past the copied prefix). dst == src
 *     (in-place) is also supported. On success it writes exactly the
 *     returned number of bytes (always <= srclen) to dst; on error (-1
 *     truncated escape, -2 invalid hex digit, first error in input order)
 *     dst holds an unspecified partial result within dst[0 .. srclen).
 */
#ifndef HERMES_URL_DECODE_API_H
#define HERMES_URL_DECODE_API_H

#include <stddef.h>
#include <stdint.h>

size_t hermes_url_scan_special(const uint8_t *src, size_t len, int plus_is_space);
ptrdiff_t hermes_url_decode(const uint8_t *src, size_t srclen, uint8_t *dst, int plus_is_space);

static inline size_t ref_url_scan_special(const uint8_t *src, size_t len, int plus)
{
    for (size_t i = 0; i < len; i++) {
        if (src[i] == '%' || (plus && src[i] == '+')) return i;
    }
    return len;
}

static inline int ref_hex(uint8_t c)
{
    static const char digits[] = "0123456789abcdef";
    uint8_t l = (c >= 'A' && c <= 'Z') ? (uint8_t)(c + 32) : c;
    for (int k = 0; k < 16; k++) {
        if (digits[k] == l) return k;
    }
    return -1;
}

/* RFC 3986 section 2.1 percent-decoding, plus the form-urlencoded '+' to
 * space mapping when plus is nonzero. Byte-at-a-time, errors in input
 * order. dst must hold n bytes. */
static inline ptrdiff_t ref_url_decode(const uint8_t *src, size_t n, uint8_t *dst, int plus)
{
    size_t o = 0;
    size_t i = 0;
    while (i < n) {
        uint8_t c = src[i];
        if (c == '%') {
            if (i + 1 >= n) return -1;
            int hi = ref_hex(src[i + 1]);
            if (hi < 0) return -2;
            if (i + 2 >= n) return -1;
            int lo = ref_hex(src[i + 2]);
            if (lo < 0) return -2;
            dst[o++] = (uint8_t)(hi * 16 + lo);
            i += 3;
        } else if (plus && c == '+') {
            dst[o++] = ' ';
            i++;
        } else {
            dst[o++] = c;
            i++;
        }
    }
    return (ptrdiff_t)o;
}

/* Percent-encode src into dst (capacity 3 * n). Bit k of choose(k) picks
 * whether byte k is escaped even when it need not be, and its hex case;
 * '%' (and '+' in plus mode) are always escaped, and ' ' becomes '+' in
 * plus mode when not escaped. Returns the encoded length. */
static inline size_t ref_url_encode(const uint8_t *src, size_t n, uint8_t *dst, int plus,
                                    const uint8_t *choice, size_t nchoice)
{
    static const char up[] = "0123456789ABCDEF", lo[] = "0123456789abcdef";
    size_t o = 0;
    for (size_t i = 0; i < n; i++) {
        uint8_t ch = nchoice ? choice[i % nchoice] : 0;
        uint8_t c = src[i];
        int must = c == '%' || (plus && c == '+');
        if (must || (ch & 1)) {
            const char *d = (ch & 2) ? lo : up;
            dst[o++] = '%';
            dst[o++] = (uint8_t)d[c >> 4];
            dst[o++] = (uint8_t)d[c & 15];
        } else if (plus && c == ' ') {
            dst[o++] = '+';
        } else {
            dst[o++] = c;
        }
    }
    return o;
}

#endif
