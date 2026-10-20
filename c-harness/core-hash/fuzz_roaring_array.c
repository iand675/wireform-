/* Roaring ARRAY container fuzz target: decode, membership and encode.
 *
 * Input layout: [hi u32 LE][kind byte][payload...]. The payload's whole
 * uint16 lows form an ARRAY container (cardinality = payload/2) in an
 * exact-size buffer, as the Haskell wrapper passes it after clamping.
 *
 *  - decode_array vs the reference, into an exact-size Int32 destination;
 *  - contains on the raw (usually unsorted, as a hostile blob would be)
 *    payload: 0 or 1, never a false positive; kind byte picks 0 or a
 *    non-1 alias of ARRAY;
 *  - contains on a sorted copy: exact, around the linear/binary switch;
 *  - encode_array round trip and encode_bitset / contains agreement. */
#include "harness.h"
#include "api.h"
#include "vecbuf.h"

static int cmp_u16(const void *a, const void *b)
{
    uint16_t x = *(const uint16_t *)a, y = *(const uint16_t *)b;
    return (x > y) - (x < y);
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size > (1u << 18)) return 0;
    wf_input in = { data, size };
    uint32_t hi = wf_take_u32(&in);
    uint8_t kb = wf_take_u8(&in);
    int32_t kind = kb & 1 ? 0 : (kb & 2 ? -1 : 2);
    size_t card = in.n / 2;
    size_t nb = 2 * card;

    uint8_t *src = wf_dup(in.p, nb);
    int32_t *dst = vec_alloc(4 * card);
    int32_t *want = malloc(4 * card + 4);
    WF_FUZZ_ASSERT(hs_wf_roaring_decode_array(src, (int32_t)card, hi, dst)
                   == (int32_t)(2 * card));
    ref_decode_array(src, (int32_t)card, hi, want);
    WF_FUZZ_ASSERT(card == 0 || memcmp(dst, want, 4 * card) == 0);

    /* Needles: the edges, every member's neighbourhood (bounded), and
     * bytes of the input. */
    uint16_t needles[64 + 3];
    size_t nn = 0;
    needles[nn++] = 0;
    needles[nn++] = 0xffff;
    needles[nn++] = (uint16_t)hi;
    for (size_t i = 0; i < card && nn < sizeof needles / sizeof needles[0]; i += 1 + card / 32) {
        needles[nn++] = (uint16_t)(ref_low(src, i) + (i & 1));
    }
    for (size_t i = 0; i < nn; i++) {
        int32_t got = hs_wf_roaring_contains(kind, src, (int32_t)card, needles[i]);
        WF_FUZZ_ASSERT(got == 0 || got == 1);
        if (got) WF_FUZZ_ASSERT(ref_array_member(src, (int32_t)card, needles[i]));
    }

    /* Sorted (duplicates kept): membership is exact. */
    uint16_t *lows = vec_alloc(nb);
    for (size_t i = 0; i < card; i++) lows[i] = ref_low(src, i);
    qsort(lows, card, sizeof lows[0], cmp_u16);
    uint8_t *sorted = wf_alloc(nb);
    WF_FUZZ_ASSERT(hs_wf_roaring_encode_array(lows, (int32_t)card, sorted)
                   == (int32_t)nb);
    for (size_t i = 0; i < card; i++) WF_FUZZ_ASSERT(ref_low(sorted, i) == lows[i]);
    for (size_t i = 0; i < nn; i++) {
        WF_FUZZ_ASSERT(hs_wf_roaring_contains(kind, sorted, (int32_t)card, needles[i])
                       == ref_array_member(sorted, (int32_t)card, needles[i]));
    }
    size_t step = 1 + card / 512;
    for (size_t i = 0; i < card; i += step) {
        for (int d = -1; d <= 1; d++) {
            uint16_t v = (uint16_t)(lows[i] + d);
            int32_t want_m = d == 0 ? 1 : ref_array_member(sorted, (int32_t)card, v);
            WF_FUZZ_ASSERT(hs_wf_roaring_contains(0, sorted, (int32_t)card, v) == want_m);
        }
    }

    /* The same set as a BITSET. */
    uint8_t *bits = wf_alloc(8192);
    hs_wf_roaring_encode_bitset(lows, (int32_t)card, bits);
    for (size_t i = 0; i < nn; i++) {
        WF_FUZZ_ASSERT(hs_wf_roaring_contains(1, bits, 0, needles[i])
                       == ref_array_member(sorted, (int32_t)card, needles[i]));
    }
    for (size_t i = 0; i < card; i += step) {
        WF_FUZZ_ASSERT(ref_bitset_member(bits, lows[i]));
    }

    wf_free(bits, 8192);
    wf_free(sorted, nb);
    vec_free(lows, nb);
    free(want);
    vec_free(dst, 4 * card);
    wf_free(src, nb);
    return 0;
}
