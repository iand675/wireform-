/* Differential fuzz target for hs_base64_encode, plus the round trip
 * through hs_base64_decode.
 *
 * The whole input is the plaintext. It is copied into an exact-size buffer;
 * the encoded text goes into an exact-size buffer of
 * hs_base64_encoded_length(n) bytes and must match the bit-serial reference
 * encoder; decoding it back (into exactly hs_base64_decoded_max_length
 * bytes) must reproduce the input. */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size > (1u << 20)) return 0;
    int n = (int)size;
    uint8_t *in = wf_dup(data, size);
    int el = hs_base64_encoded_length(n);
    WF_FUZZ_ASSERT(el == ref_b64_encoded_length(n));
    uint8_t *enc = wf_alloc((size_t)el);
    WF_FUZZ_ASSERT(hs_base64_encode(in, n, enc) == el);
    uint8_t *want = malloc((size_t)el + 1);
    WF_FUZZ_ASSERT(ref_b64_encode(in, size, want) == (size_t)el);
    WF_FUZZ_ASSERT(memcmp(enc, want, (size_t)el) == 0);
    int ml = hs_base64_decoded_max_length(el);
    uint8_t *dec = wf_alloc((size_t)ml);
    WF_FUZZ_ASSERT(hs_base64_decode(enc, el, dec) == n);
    WF_FUZZ_ASSERT(size == 0 || memcmp(dec, in, size) == 0);
    wf_free(dec, (size_t)ml);
    free(want);
    wf_free(enc, (size_t)el);
    wf_free(in, size);
    return 0;
}
