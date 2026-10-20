/* Differential fuzz target: hs_ws_mask vs a scalar reference, on a
 * buffer whose start is shifted by up to 15 bytes so every alignment of
 * the SIMD prologue is reached.
 *
 * Input layout: [mask: 4 bytes LE][skew byte][payload...]. */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 5 || size > (1u << 20)) return 0;
    wf_input in = { data, size };
    uint32_t mask = wf_take_u32(&in);
    size_t skew = wf_take_u8(&in) & 15;
    size_t len = in.n;

    uint8_t *region = wf_alloc(skew + len);
    memset(region, 0xa5, skew);
    uint8_t *buf = region + skew;
    memcpy(buf, in.p, len);
    uint8_t *want = malloc(len ? len : 1);
    memcpy(want, in.p, len);

    hs_ws_mask(buf, (int)len, mask);
    ref_ws_mask(want, (int)len, mask);
    WF_FUZZ_ASSERT(len == 0 || memcmp(buf, want, len) == 0);
    for (size_t i = 0; i < skew; i++) WF_FUZZ_ASSERT(region[i] == 0xa5);

    free(want);
    wf_free(region, skew + len);
    return 0;
}
