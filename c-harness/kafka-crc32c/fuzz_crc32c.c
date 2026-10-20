/* Differential fuzz target: crc32c / crc32c_append vs the bitwise
 * reference, one-shot and split at a fuzzer-chosen point.
 *
 * Input layout: [seed register: 4 bytes LE][split byte][skew byte][data...]
 * The data is copied into an exact-size buffer at a 0..15 byte skew so
 * every start alignment of the 8-byte stage is reached. */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 6 || size > (1u << 20)) return 0;
    wf_input in = { data, size };
    uint32_t seed = wf_take_u32(&in);
    uint8_t split = wf_take_u8(&in);
    size_t skew = wf_take_u8(&in) & 15;
    size_t n = in.n;

    uint8_t *region = wf_alloc(skew + n);
    memset(region, 0xA5, skew);
    uint8_t *b = region + skew;
    memcpy(b, in.p, n);

    WF_FUZZ_ASSERT(crc32c(b, n) == ref_crc32c(in.p, n));
    uint32_t want = ref_crc32c_append(seed, in.p, n);
    WF_FUZZ_ASSERT(crc32c_append(seed, b, n) == want);
    size_t cut = n ? (size_t)split * n / 255 : 0;
    uint32_t c = crc32c_append(seed, b, cut);
    c = crc32c_append(c, b + cut, n - cut);
    WF_FUZZ_ASSERT(c == want);

    wf_free(region, skew + n);
    return 0;
}
