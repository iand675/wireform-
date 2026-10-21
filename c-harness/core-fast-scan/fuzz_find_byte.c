/* Differential fuzz target: hs_find_byte vs a scalar reference.
 *
 * Input layout: [target byte][offset byte][haystack...]. The haystack is
 * copied into an exact-size buffer (data+2 may be shared with libFuzzer's
 * allocation, whose redzone starts after the full input), and the offset
 * is taken modulo len+2 so out-of-range offsets are exercised too. */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 2 || size > (1u << 20)) return 0;
    uint8_t target = data[0];
    ptrdiff_t len = (ptrdiff_t)(size - 2);
    ptrdiff_t off = data[1] % (len + 2);
    uint8_t *buf = wf_dup(data + 2, (size_t)len);
    ptrdiff_t got = hs_find_byte(buf, off, len, target);
    ptrdiff_t want = ref_find_byte(buf, off, len, target);
    WF_FUZZ_ASSERT(got == want);
    wf_free(buf, (size_t)len);
    return 0;
}
