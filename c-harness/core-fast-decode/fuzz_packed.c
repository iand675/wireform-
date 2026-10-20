/* Differential fuzz target for the packed-repeated-field kernels of
 * fast_decode.c: count_packed_varints, packed_all_single_byte,
 * decode_packed_varints and decode_packed_single_byte_varints.
 *
 * Input layout: [max_out byte][packed bytes...]. max_out 0xFF means
 * "len" (always enough room); otherwise it is the byte value, so short
 * output arrays are exercised. Output arrays are exact-size. */
#include "harness.h"
#include "api.h"
#include "buffers.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 1 || size > (1u << 16)) return 0;
    int len = (int)(size - 1);
    int max_out = data[0] == 0xFF ? len : data[0];
    uint8_t *buf = wf_dup(data + 1, (size_t)len);

    int count = hs_proto_count_packed_varints(buf, len);
    WF_FUZZ_ASSERT(count == ref_count_packed(buf, len));
    int single = hs_proto_packed_all_single_byte(buf, len);
    WF_FUZZ_ASSERT(single == ref_all_single_byte(buf, len));
    WF_FUZZ_ASSERT(single == (count == len));

    size_t osz = (size_t)max_out * sizeof(uint64_t);
    a8buf ob1 = a8_alloc(osz);
    uint64_t *o1 = ob1.p;
    uint64_t *o2 = malloc((size_t)(max_out ? max_out : 1) * sizeof(uint64_t));
    int c1 = hs_proto_decode_packed_varints(buf, len, o1, max_out);
    int c2 = ref_decode_packed(buf, len, o2, max_out);
    WF_FUZZ_ASSERT(c1 == c2);
    if (c1 > 0) {
        WF_FUZZ_ASSERT(memcmp(o1, o2, (size_t)c1 * sizeof(uint64_t)) == 0);
        /* a well-formed run has exactly one terminator per value */
        WF_FUZZ_ASSERT(c1 == count);
    }
    free(o2);
    a8_free(ob1);

    a8buf ob3 = a8_alloc((size_t)len * sizeof(uint64_t));
    uint64_t *o3 = ob3.p;
    WF_FUZZ_ASSERT(hs_proto_decode_packed_single_byte_varints(buf, len, o3) == len);
    for (int i = 0; i < len; i++) WF_FUZZ_ASSERT(o3[i] == buf[i]);
    a8_free(ob3);
    wf_free(buf, (size_t)len);
    return 0;
}
