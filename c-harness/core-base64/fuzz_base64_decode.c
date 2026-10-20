/* Differential fuzz target for hs_base64_decode.
 *
 * The whole input is untrusted base64 text (what decodeBase64 receives off
 * the wire). It is copied into an exact-size buffer and decoded into an
 * exact-size buffer of hs_base64_decoded_max_length(n) bytes prefilled with
 * a sentinel. The result and the decoded bytes must match the strict
 * reference decoder, nothing past the decoded length may be written, and a
 * successful decode must survive an encode / decode round trip. */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size > (1u << 20)) return 0;
    int n = (int)size;
    uint8_t *in = wf_dup(data, size);
    int ml = hs_base64_decoded_max_length(n);
    WF_FUZZ_ASSERT(ml == n / 4 * 3);
    uint8_t *out = wf_alloc((size_t)ml);
    memset(out, 0xA5, (size_t)ml);
    uint8_t *want = malloc((size_t)ml + 1);
    long wr = ref_b64_decode(in, n, want);
    int got = hs_base64_decode(in, n, out);
    WF_FUZZ_ASSERT(got == wr);
    if (got >= 0) {
        WF_FUZZ_ASSERT(got <= ml);
        WF_FUZZ_ASSERT(got == 0 || memcmp(out, want, (size_t)got) == 0);
        for (int k = got; k < ml; k++) WF_FUZZ_ASSERT(out[k] == 0xA5);
        /* Canonical form: re-encoding then decoding is the identity. */
        int el = hs_base64_encoded_length(got);
        WF_FUZZ_ASSERT(el == n);
        uint8_t *re = wf_alloc((size_t)el);
        WF_FUZZ_ASSERT(hs_base64_encode(out, got, re) == el);
        uint8_t *back = wf_alloc((size_t)ml);
        WF_FUZZ_ASSERT(hs_base64_decode(re, el, back) == got);
        WF_FUZZ_ASSERT(got == 0 || memcmp(back, out, (size_t)got) == 0);
        wf_free(back, (size_t)ml);
        wf_free(re, (size_t)el);
    }
    free(want);
    wf_free(out, (size_t)ml);
    wf_free(in, size);
    return 0;
}
