/* Differential fuzz target: hs_proto_validate_arrow_buffers against a
 * 128-bit-exact scalar reference.
 *
 * Input layout: [body_length: 8 bytes LE][(offset, length) int64 LE
 * pairs...]; trailing bytes short of a pair are ignored. Pairs are
 * copied into an exact-size array. */
#include "harness.h"
#include "api.h"
#include "buffers.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 8 || size > (1u << 16)) return 0;
    int64_t body = (int64_t)ref_load_le(data, 8);
    int count = (int)((size - 8) / 16);
    size_t sz = (size_t)count * 16;
    a8buf ab = a8_alloc(sz); /* Storable Buffer vector: 8-aligned */
    int64_t *pairs = ab.p;
    for (int i = 0; i < 2 * count; i++) pairs[i] = (int64_t)ref_load_le(data + 8 + 8 * i, 8);
    WF_FUZZ_ASSERT(hs_proto_validate_arrow_buffers(pairs, count, body)
                   == ref_validate_arrow(pairs, count, body));
    a8_free(ab);
    return 0;
}
