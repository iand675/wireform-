/* Differential fuzz target: hs_wf_xxh64 vs the specification transcription
 * in api.h.
 *
 * Input layout: [seed: 8 bytes LE][data...]. The data is hashed from an
 * exact-size copy with the fuzzed seed and with seed 0 (what Parquet's
 * bloom filter uses); the whole input is hashed too, so the corpus's raw
 * documents are covered without the header. */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size > (1u << 20)) return 0;
    wf_input in = { data, size };
    uint64_t lo = wf_take_u32(&in), hi = wf_take_u32(&in);
    uint64_t seed = lo | (hi << 32);

    uint8_t *buf = wf_dup(in.p, in.n);
    WF_FUZZ_ASSERT(hs_wf_xxh64(buf, (int64_t)in.n, seed)
                   == ref_xxh64(buf, in.n, seed));
    WF_FUZZ_ASSERT(hs_wf_xxh64(buf, (int64_t)in.n, 0) == ref_xxh64(buf, in.n, 0));
    wf_free(buf, in.n);

    uint8_t *all = wf_dup(data, size);
    WF_FUZZ_ASSERT(hs_wf_xxh64(all, (int64_t)size, 0) == ref_xxh64(all, size, 0));
    wf_free(all, size);
    return 0;
}
