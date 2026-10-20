/* Round-trip fuzz target for wireform_hpack_huffman_encode and
 * wireform_hpack_huffman_encode_len.
 *
 * The whole input is the plaintext. Checks: encode_len matches the
 * reference and the Haskell 4n + 1 bound, encode writes exactly that many
 * bytes into an exact-size buffer and matches the bit-at-a-time reference
 * encoder, and decoding gives the plaintext back at exactly its length
 * while one byte less capacity fails without overrunning. */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size > (1u << 16)) return 0;
    uint8_t *src = wf_dup(data, size);

    size_t want = ref_encode_len(src, size);
    size_t elen = wireform_hpack_huffman_encode_len(src, size);
    WF_FUZZ_ASSERT(elen == want);
    WF_FUZZ_ASSERT(elen <= 4 * size + 1);

    uint8_t *ref = malloc(want ? want : 1);
    WF_FUZZ_ASSERT(ref_encode(src, size, ref) == want);

    uint8_t *enc = wf_alloc(elen);
    WF_FUZZ_ASSERT(wireform_hpack_huffman_encode(src, size, enc) == elen);
    WF_FUZZ_ASSERT(elen == 0 || memcmp(enc, ref, elen) == 0);

    uint8_t *dst = wf_alloc(size);
    size_t out_len = (size_t)-1;
    WF_FUZZ_ASSERT(wireform_hpack_huffman_decode(enc, elen, dst, size, &out_len) == 0);
    WF_FUZZ_ASSERT(out_len == size);
    WF_FUZZ_ASSERT(size == 0 || memcmp(dst, src, size) == 0);
    wf_free(dst, size);

    if (size > 0) {
        size_t cap = size - 1;
        uint8_t *shrt = wf_alloc(cap);
        out_len = (size_t)-1;
        WF_FUZZ_ASSERT(wireform_hpack_huffman_decode(enc, elen, shrt, cap, &out_len) == -1);
        WF_FUZZ_ASSERT(out_len == cap);
        WF_FUZZ_ASSERT(cap == 0 || memcmp(shrt, src, cap) == 0);
        wf_free(shrt, cap);
    }

    wf_free(enc, elen);
    free(ref);
    wf_free(src, size);
    return 0;
}
