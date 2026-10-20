/* Differential fuzz target: hs_proto_validate_utf8_fast against the
 * Unicode table 3-7 byte-range validator in api.h, on the whole input
 * and on every suffix starting in the first 16 bytes (shifts the input
 * across the SIMD/SWAR block boundaries). */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size > (1u << 18)) return 0;
    uint8_t *buf = wf_dup(data, size);
    for (size_t k = 0; k <= size && k < 16; k++) {
        int n = (int)(size - k);
        WF_FUZZ_ASSERT(hs_proto_validate_utf8_fast(buf + k, n) == ref_utf8_valid(buf + k, n));
    }
    wf_free(buf, size);
    return 0;
}
