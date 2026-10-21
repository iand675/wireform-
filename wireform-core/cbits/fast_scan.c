/*
 * Format-agnostic SIMD byte-scanning primitives.
 *
 * These used to live in wireform-xml/cbits/fast_xml.c under
 * XML-specific names because that's where they were first
 * introduced. CSV, NDJSON, and a few other decoders reached
 * into that file to reuse hs_xml_find_byte; to keep the
 * dependency graph honest the generic helper lives here and is
 * exported under a neutral name. The XML-specific scanners
 * (find_lt, find_attr_end, find_cdata_end, etc.) remain in
 * wireform-xml since they encode XML-grammar knowledge.
 *
 * Uses SSE2 via simde for portable SIMD. Fallback to scalar
 * loops on architectures without simde support.
 *
 * Widths (the same split as wireform-html and wireform-http1):
 * positions into a buffer are ptrdiff_t, so a ByteString or scan
 * range over 2 GiB is not truncated and a miss can be reported as
 * len (never -1 here); pure byte counts are size_t. Neither type
 * narrows to CInt on the Haskell side.
 */

#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <simde/x86/sse2.h>

/*
 * Scan buf[offset..len) for the first occurrence of target_byte.
 *
 * `offset` is the start position and `len` the buffer's length, which
 * doubles as the exclusive end of the scan, so both are indices into
 * `buf` and both must fit in ptrdiff_t (any real object does). Requires
 * 0 <= offset. Returns the absolute position of the match, or len if no
 * match was found (offset >= len is a miss that touches no memory).
 * Used by CSV delimiter scanning, NDJSON newline scanning, XML
 * structural scanning, etc.
 */
ptrdiff_t hs_find_byte(const uint8_t *buf, ptrdiff_t offset, ptrdiff_t len, uint8_t target_byte)
{
    ptrdiff_t i = offset;
    simde__m128i target = simde_mm_set1_epi8((char)target_byte);

    /* Scalar prologue until 16-byte aligned. */
    while (i < len && ((uintptr_t)(buf + i) & 15) != 0) {
        if (buf[i] == target_byte) return i;
        i++;
    }

    /* SIMD main loop: 16 bytes at a time. The bound is len - i (never
     * i + 16 <= len) so a len near PTRDIFF_MAX cannot wrap it. */
    for (; len - i >= 16; i += 16) {
        simde__m128i chunk = simde_mm_load_si128((const simde__m128i *)(buf + i));
        int mask = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, target));
        if (mask != 0) return i + __builtin_ctz(mask);
    }

    /* Scalar epilogue for the remaining <16 bytes. */
    for (; i < len; i++) {
        if (buf[i] == target_byte) return i;
    }

    return len;
}

/*
 * In-place XOR of a 4-byte repeating mask over @buf[0..len).
 * Used by wireform-websocket for RFC 6455 sec 5.3 frame masking
 * (both directions: the parser un-masks an inbound payload; the
 * builder masks an outbound client payload).
 *
 * `len` is a pure byte count (size_t): the kernel touches every
 * byte it is given, so it has no 2 GiB ceiling.
 *
 * The mask repeats in 4-byte network-order chunks; for the SSE2
 * inner loop we broadcast that 32-bit pattern into a 16-byte
 * vector once and XOR 16 bytes per iteration.  Scalar prologue +
 * epilogue handle any sub-16-byte head / tail.
 *
 * @mask is a uint32_t with the four mask bytes in NETWORK ORDER,
 * i.e. byte 0 of the mask in the high byte.  The same byte
 * ordering the wire format uses.
 */
void hs_ws_mask(uint8_t *buf, size_t len, uint32_t mask)
{
    /* Build the byte ordering the loop needs: mask[0] at offset 0,
     * mask[1] at offset 1, …  Easiest done by re-laying the bytes. */
    uint8_t mb[4];
    mb[0] = (uint8_t)(mask >> 24);
    mb[1] = (uint8_t)(mask >> 16);
    mb[2] = (uint8_t)(mask >>  8);
    mb[3] = (uint8_t) mask;

    size_t i = 0;

    /* Align the scalar prologue so the main loop can use 'movdqa'
     * once we hit a 16-byte boundary.  The mask byte at offset i
     * is mb[i & 3] because the mask repeats every 4 bytes. */
    while (i < len && ((uintptr_t)(buf + i) & 15) != 0) {
        buf[i] ^= mb[i & 3];
        i++;
    }

    /* SSE2 main loop: 16 bytes per iteration.  The 4-byte mask
     * tiled into a 16-byte vector once.  If i hit alignment at an
     * offset where i & 3 != 0, the tile rotates accordingly. */
    if (len - i >= 16) {
        unsigned phase = (unsigned)(i & 3);
        uint8_t tile[16];
        for (int k = 0; k < 16; k++) {
            tile[k] = mb[(phase + (unsigned)k) & 3];
        }
        simde__m128i v = simde_mm_loadu_si128((const simde__m128i *)tile);
        /* len - i (not i + 16 <= len): no wrap for a len near SIZE_MAX. */
        for (; len - i >= 16; i += 16) {
            simde__m128i chunk = simde_mm_load_si128((const simde__m128i *)(buf + i));
            chunk = simde_mm_xor_si128(chunk, v);
            simde_mm_store_si128((simde__m128i *)(buf + i), chunk);
        }
    }

    /* Scalar epilogue for the trailing <16 bytes. */
    for (; i < len; i++) {
        buf[i] ^= mb[i & 3];
    }
}
