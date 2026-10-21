/* Differential fuzz target for the bit kernels: bitmap_popcount,
 * unpack_bits_lsb, memcpy_fast, popcount_bits, copy_bits, and_bits and
 * gather_bits against the scalar references in api.h.
 *
 * Input: [mode][4 parameter bytes][split byte][payload...]. The payload is
 * split into one or two exact-size buffers (wf_dup), the parameters pick
 * bit offsets and lengths inside them (taken modulo what fits, so every
 * request stays within the FFI contract), and every output buffer is
 * exact-size too, so a read or write one byte past the covered range
 * faults. */
#include "harness.h"
#include "api.h"

static uint64_t take_u16(wf_input *in)
{
    return (uint64_t)wf_take_u8(in) | (uint64_t)wf_take_u8(in) << 8;
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size > (1u << 16)) return 0;
    wf_input in = { data, size };
    uint8_t mode = wf_take_u8(&in) % 7;
    uint64_t p1 = take_u16(&in), p2 = take_u16(&in);
    uint8_t split = wf_take_u8(&in);
    size_t len = in.n;
    uint8_t *buf = wf_dup(in.p, len);
    size_t bits = 8 * len;

    switch (mode) {
    case 0: {
        WF_FUZZ_ASSERT(hs_columnar_bitmap_popcount(buf, len) == ref_bitmap_popcount(buf, len));
        break;
    }
    case 1: {
        size_t n = (size_t)(p1 % (bits + 1));
        uint8_t *dst = wf_alloc(n), *want = malloc(n + 1);
        size_t nb = (n + 7) / 8;
        /* src is exactly the covering bytes */
        uint8_t *src = wf_dup(buf, nb);
        hs_columnar_unpack_bits_lsb(src, n, dst);
        ref_unpack_bits_lsb(src, n, want);
        WF_FUZZ_ASSERT(memcmp(dst, want, n) == 0);
        wf_free(src, nb);
        wf_free(dst, n);
        free(want);
        break;
    }
    case 2: {
        uint8_t *dst = wf_alloc(len);
        hs_columnar_memcpy_fast(buf, dst, len);
        WF_FUZZ_ASSERT(len == 0 || memcmp(dst, buf, len) == 0);
        wf_free(dst, len);
        break;
    }
    case 3: {
        size_t off = (size_t)(p1 % (bits + 1));
        size_t n = (size_t)(p2 % (bits - off + 1));
        /* the buffer is trimmed to the bytes that cover [off, off + n) */
        size_t nb = (off + n + 7) / 8;
        WF_FUZZ_ASSERT(hs_columnar_popcount_bits(buf, off, n) == ref_popcount_bits(buf, off, n));
        uint8_t *b = wf_dup(buf, nb);
        WF_FUZZ_ASSERT(hs_columnar_popcount_bits(b, off, n) == ref_popcount_bits(b, off, n));
        wf_free(b, nb);
        break;
    }
    case 4:
    case 5: {
        /* two operands: payload[0, cut) and payload[cut, len) */
        size_t cut = len ? (size_t)split * len / 255 : 0;
        size_t abits = 8 * cut, bbits = 8 * (len - cut);
        size_t aoff = (size_t)(p1 % (abits + 1)), boff = (size_t)(p2 % (bbits + 1));
        size_t n = abits - aoff < bbits - boff ? abits - aoff : bbits - boff;
        if (len) n = n ? (size_t)((p1 >> 3 ^ p2) % (n + 1)) : 0;
        size_t ab = (aoff + n + 7) / 8, bb = (boff + n + 7) / 8;
        uint8_t *a = wf_dup(buf, ab), *b = wf_dup(buf + cut, bb);
        if (mode == 4) {
            /* copy a's range into b's range; b bits outside it survive */
            uint8_t *want = malloc(bb + 1);
            memcpy(want, b, bb);
            ref_copy_bits(want, boff, a, aoff, n);
            hs_columnar_copy_bits(b, boff, a, aoff, n);
            WF_FUZZ_ASSERT(bb == 0 || memcmp(b, want, bb) == 0);
            free(want);
        } else {
            size_t db = (n + 7) / 8;
            uint8_t *d = wf_alloc(db), *want = malloc(db + 1);
            int64_t got = hs_columnar_and_bits(d, a, aoff, b, boff, n);
            WF_FUZZ_ASSERT(got == ref_and_bits(want, a, aoff, b, boff, n));
            WF_FUZZ_ASSERT(db == 0 || memcmp(d, want, db) == 0);
            wf_free(d, db);
            free(want);
        }
        wf_free(a, ab);
        wf_free(b, bb);
        break;
    }
    default: {
        /* source bits: the first `cut` bytes; indices: u16 pairs after it */
        size_t cut = len ? (size_t)split * len / 255 : 0;
        if (cut == 0) break;
        size_t soff = (size_t)(p1 % 64);
        size_t srcbits = 8 * cut - (soff < 8 * cut ? soff : 0);
        if (soff >= 8 * cut) soff = 0;
        size_t n = (len - cut) / 2;
        int64_t *idx = malloc((n ? n : 1) * 8);
        for (size_t k = 0; k < n; k++) {
            uint64_t v = (uint64_t)buf[cut + 2 * k] | (uint64_t)buf[cut + 2 * k + 1] << 8;
            idx[k] = (int64_t)(v % srcbits);
        }
        uint8_t *ix = wf_dup(idx, n * 8);
        size_t db = (n + 7) / 8;
        uint8_t *d = wf_alloc(db), *want = malloc(db + 1);
        uint8_t *src = wf_dup(buf, cut);
        int64_t got = hs_columnar_gather_bits(d, src, soff, (const int64_t *)(void *)ix, n);
        WF_FUZZ_ASSERT(got == ref_gather_bits(want, src, soff, idx, n));
        WF_FUZZ_ASSERT(db == 0 || memcmp(d, want, db) == 0);
        wf_free(src, cut);
        wf_free(d, db);
        wf_free(ix, n * 8);
        free(want);
        free(idx);
        break;
    }
    }
    wf_free(buf, len);
    return 0;
}
