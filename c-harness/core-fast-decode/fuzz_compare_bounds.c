/* Differential fuzz target: hs_proto_compare_bounds (Iceberg manifest
 * pruning) against a scalar reference.
 *
 * Input layout: [count byte][width byte][search: width bytes][bounds...].
 * width is 4 (the SIMD int32 path) when the width byte's top bit is set,
 * else width byte % 10. count is count byte % 72 (past the 32-bit mask),
 * clipped to the bounds actually present. Bounds and search are exact-
 * size copies, so reading past count * width faults. */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 2 || size > (1u << 16)) return 0;
    int width = data[1] & 0x80 ? 4 : data[1] % 10;
    size_t rest = size - 2;
    if (rest < (size_t)width) return 0;
    int count = data[0] % 72;
    size_t avail = rest - (size_t)width;
    if (width > 0 && (size_t)count * (size_t)width > avail) count = (int)(avail / (size_t)width);
    size_t bsz = (size_t)count * (size_t)width;
    uint8_t *search = wf_dup(data + 2, (size_t)width);
    uint8_t *bounds = wf_dup(data + 2 + width, bsz);
    int got = hs_proto_compare_bounds(bounds, count, width, search);
    WF_FUZZ_ASSERT(got == ref_compare_bounds(bounds, count, width, search));
    if (count < 32) WF_FUZZ_ASSERT(((uint32_t)got >> count) == 0);
    wf_free(bounds, bsz);
    wf_free(search, (size_t)width);
    return 0;
}
