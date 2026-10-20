/* Roaring BITSET container fuzz target: decode, membership and encode.
 *
 * Input layout: [hi u32 LE][mode byte][body...].
 *  mode & 3 == 0: body is the bitset's leading bytes, rest zero;
 *  mode & 3 == 1: body is the bitset's leading bytes, rest 0xFF;
 *  mode & 3 == 2: body tiles the whole 8192 bytes;
 *  mode & 3 == 3: body is a list of uint16 positions (any order,
 *                 duplicates) built with encode_bitset.
 * The 8192-byte payload is an exact-size copy; decode_bitset writes into
 * a destination of exactly popcount Int32 (it may not write further even
 * though the contract gives it 65536). contains is checked for every
 * value in a stride and for the positions named by the body. */
#include "harness.h"
#include "api.h"
#include "vecbuf.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size > 65536) return 0;
    wf_input in = { data, size };
    uint32_t hi = wf_take_u32(&in);
    uint8_t mode = wf_take_u8(&in);

    uint8_t img[8192];
    switch (mode & 3) {
    case 0:
    case 1:
        memset(img, (mode & 3) ? 0xff : 0x00, sizeof img);
        memcpy(img, in.p, in.n < sizeof img ? in.n : sizeof img);
        break;
    case 2:
        if (in.n == 0) memset(img, 0, sizeof img);
        else for (size_t i = 0; i < sizeof img; i++) img[i] = in.p[i % in.n];
        break;
    default: {
        size_t card = in.n / 2;
        uint16_t *lows = vec_alloc(2 * card);
        for (size_t i = 0; i < card; i++) {
            lows[i] = (uint16_t)(in.p[2 * i] | (in.p[2 * i + 1] << 8));
        }
        uint8_t *dst = wf_alloc(8192);
        memset(dst, 0x5a, 8192);
        hs_wf_roaring_encode_bitset(lows, (int32_t)card, dst);
        memset(img, 0, sizeof img);
        for (size_t i = 0; i < card; i++) img[lows[i] >> 3] |= (uint8_t)(1u << (lows[i] & 7));
        WF_FUZZ_ASSERT(memcmp(dst, img, sizeof img) == 0);
        wf_free(dst, 8192);
        vec_free(lows, 2 * card);
        break;
    }
    }

    uint8_t *src = wf_dup(img, sizeof img);
    int32_t *want = malloc(65536 * 4);
    int32_t n = ref_decode_bitset(src, hi, want);
    int32_t *dst = vec_alloc(4 * (size_t)n);
    WF_FUZZ_ASSERT(hs_wf_roaring_decode_bitset(src, hi, dst) == n);
    WF_FUZZ_ASSERT(n == 0 || memcmp(dst, want, 4 * (size_t)n) == 0);
    vec_free(dst, 4 * (size_t)n);
    free(want);

    uint32_t stride = 1 + (mode >> 2);
    for (uint32_t v = (uint32_t)(hi & 0xff) % stride; v < 65536; v += stride) {
        WF_FUZZ_ASSERT(hs_wf_roaring_contains(1, src, 0, (uint16_t)v)
                       == ref_bitset_member(src, (uint16_t)v));
    }
    for (size_t i = 0; i + 1 < in.n && i < 512; i += 2) {
        uint16_t v = (uint16_t)(in.p[i] | (in.p[i + 1] << 8));
        WF_FUZZ_ASSERT(hs_wf_roaring_contains(1, src, (int32_t)hi, v)
                       == ref_bitset_member(src, v));
    }
    wf_free(src, sizeof img);
    return 0;
}
