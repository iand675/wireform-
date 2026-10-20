/* Differential fuzz target: wireform_hpack_huffman_decode vs the
 * bit-by-bit RFC 7541 reference decoder, on arbitrary wire bytes.
 *
 * Input layout: [cap selector][huffman bytes...]. The selector picks the
 * output capacity: the one huffmanDecode passes (2n + 256), exactly the
 * decoded size, a few bytes short of it, or a small absolute value.
 * Checks: same verdict, same out_len (on failure, the symbols before the
 * error, clipped to the capacity), same bytes, nothing written past the
 * exact-size output buffer, and a successful decode re-encodes to exactly
 * the input (valid Huffman strings are canonical: padding is the
 * shortest all-ones tail). */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 1 || size > (1u << 16)) return 0;
    uint8_t sel = data[0];
    size_t n = size - 1;
    uint8_t *src = wf_dup(data + 1, n);

    uint8_t *ref = malloc(REF_DECODE_MAX(n));
    size_t emitted = 0;
    int ref_rc = ref_decode(src, n, ref, &emitted);

    size_t cap;
    switch (sel & 3) {
    case 0: cap = 2 * n + 256; break;
    case 1: cap = emitted; break;
    case 2: cap = emitted > (size_t)(sel >> 2) ? emitted - (sel >> 2) - 1 : 0; break;
    default: cap = sel >> 2; break;
    }

    uint8_t *dst = wf_alloc(cap);
    size_t out_len = (size_t)-1;
    int rc = wireform_hpack_huffman_decode(src, n, dst, cap, &out_len);

    int want_rc = (ref_rc == 0 && emitted <= cap) ? 0 : -1;
    size_t want_len = emitted < cap ? emitted : cap;
    WF_FUZZ_ASSERT(rc == want_rc);
    WF_FUZZ_ASSERT(out_len == want_len);
    WF_FUZZ_ASSERT(out_len == 0 || memcmp(dst, ref, out_len) == 0);

    if (rc == 0) {
        size_t elen = wireform_hpack_huffman_encode_len(dst, out_len);
        WF_FUZZ_ASSERT(elen == n);
        uint8_t *enc = wf_alloc(elen);
        WF_FUZZ_ASSERT(wireform_hpack_huffman_encode(dst, out_len, enc) == elen);
        WF_FUZZ_ASSERT(elen == 0 || memcmp(enc, src, elen) == 0);
        wf_free(enc, elen);
    }

    wf_free(dst, cap);
    free(ref);
    wf_free(src, n);
    return 0;
}
