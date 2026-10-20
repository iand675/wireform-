/* Differential fuzz target for the single-varint decoders of
 * fast_decode.c: decode_varint_fast, decode_tag_fast, skip_varint,
 * skip_field (every wire type), decode_varint_swar and
 * relocate_page_boundary, against the references in api.h.
 *
 * Input layout: [offset byte][message bytes...]. The message is copied
 * into an exact-size buffer; the offset is taken modulo len + 2 so
 * offsets at and past the end are exercised too. */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 1 || size > (1u << 16)) return 0;
    int len = (int)(size - 1);
    int off = data[0] % (len + 2);
    uint8_t *buf = wf_dup(data + 1, (size_t)len);

    uint64_t a = 0x1111, b = 0x1111;
    int na = hs_proto_decode_varint_fast(buf, len, off, &a);
    int nb = ref_decode_varint(buf, len, off, &b);
    WF_FUZZ_ASSERT(na == nb && a == b);
    WF_FUZZ_ASSERT(hs_proto_skip_varint(buf, len, off) == nb);

    int fnum = -7, wt = -7;
    int nt = hs_proto_decode_tag_fast(buf, len, off, &fnum, &wt);
    WF_FUZZ_ASSERT(nt == nb);
    if (nb) WF_FUZZ_ASSERT(wt == (int)(b & 7) && fnum == (int)(b >> 3));
    else WF_FUZZ_ASSERT(fnum == -7 && wt == -7);

    if (off <= len) {
        for (int w = -1; w <= 8; w++) {
            int got = hs_proto_skip_field(buf, len, off, w);
            WF_FUZZ_ASSERT(got == ref_skip_field(buf, len, off, w));
            WF_FUZZ_ASSERT(got == -1 || (got > off && got <= len));
        }
    }

    if (off <= len && len - off >= 8) {
        uint64_t s = 0x2222, t = 0x2222;
        int ns = hs_proto_decode_varint_swar(buf, off, &s);
        WF_FUZZ_ASSERT(ns == ref_decode_varint8(buf, off, &t) && s == t);
    }

    /* relocate: decision follows the end address; copy is exact. */
    uint8_t *out = wf_alloc((size_t)len + 7);
    int rc = hs_proto_relocate_page_boundary(buf, len, out, len + 7);
    uintptr_t e = (uintptr_t)(buf + len) & 4095;
    int near = len > 0 && (e == 0 || e > 4088);
    WF_FUZZ_ASSERT(rc == near);
    if (rc == 1) {
        WF_FUZZ_ASSERT(memcmp(out, buf, (size_t)len) == 0);
        for (int i = 0; i < 7; i++) WF_FUZZ_ASSERT(out[len + i] == 0);
        WF_FUZZ_ASSERT(hs_proto_relocate_page_boundary(buf, len, out, len + 6) == -1);
    }
    wf_free(out, (size_t)len + 7);
    wf_free(buf, (size_t)len);
    return 0;
}
