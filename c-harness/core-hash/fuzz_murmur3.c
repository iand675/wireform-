/* Differential fuzz target: hs_wf_murmur3_32 and hs_wf_bucket_long vs the
 * SMHasher-style reference in api.h.
 *
 * Input layout: [buckets u32 LE][value: 8 bytes LE][data...]. The data is
 * hashed from an exact-size copy (wf_dup, so an overread hits the redzone
 * or a guard page). buckets is used raw, so 0 and negative counts (what
 * the Haskell wrapper produces for N >= 2^31) are exercised. */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size > (1u << 20)) return 0;
    wf_input in = { data, size };
    int32_t buckets = (int32_t)wf_take_u32(&in);
    uint64_t lo = wf_take_u32(&in), hi = wf_take_u32(&in);
    int64_t value = (int64_t)(lo | (hi << 32));

    uint8_t *buf = wf_dup(in.p, in.n);
    int32_t got = hs_wf_murmur3_32(buf, in.n);
    WF_FUZZ_ASSERT(got == ref_murmur3_32(buf, in.n, 0));
    /* The whole input, too: the header bytes are realistic data as well. */
    uint8_t *all = wf_dup(data, size);
    WF_FUZZ_ASSERT(hs_wf_murmur3_32(all, size)
                   == ref_murmur3_32(all, size, 0));
    wf_free(all, size);
    wf_free(buf, in.n);

    int32_t b = hs_wf_bucket_long(value, buckets);
    WF_FUZZ_ASSERT(b == ref_bucket_long(value, buckets));
    WF_FUZZ_ASSERT(buckets <= 0 ? b == 0 : (b >= 0 && b < buckets));
    return 0;
}
