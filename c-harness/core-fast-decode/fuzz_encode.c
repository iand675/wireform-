/* Round-trip fuzz target for the encoders of fast_decode.c:
 * encode_varint, encode_varint_field, encode_bool_field and
 * encode_length_delimited, decoded back with decode_varint_fast,
 * decode_varint_swar, decode_tag_fast and skip_field.
 *
 * Input layout: [value: 8 bytes LE][tag][dst offset][shift][payload...].
 * value >> (shift % 64) spreads values over every varint length. Every
 * destination is exact-size, so a write past the encoded length faults. */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 11 || size > (1u << 16)) return 0;
    uint64_t v = ref_load_le(data, 8) >> (data[10] % 64);
    uint8_t tag = data[8];
    int off = data[9] % 8;
    int plen = (int)(size - 11);
    uint8_t *payload = wf_dup(data + 11, (size_t)plen);

    uint8_t want[10];
    int wn = ref_encode_varint(want, v);

    /* bare varint, padded to 8 bytes after it for the SWAR decoder */
    size_t sz = (size_t)off + (size_t)wn;
    uint8_t *o = wf_alloc(sz);
    WF_FUZZ_ASSERT(hs_proto_encode_varint(o, off, v) == wn);
    WF_FUZZ_ASSERT(memcmp(o + off, want, (size_t)wn) == 0);
    uint64_t d = 0;
    WF_FUZZ_ASSERT(hs_proto_decode_varint_fast(o, (int)sz, off, &d) == wn && d == v);
    wf_free(o, sz);

    size_t psz = (size_t)off + 10;
    o = wf_alloc(psz);
    memset(o, data[10] & 0x7F, psz);
    hs_proto_encode_varint(o, off, v);
    d = 0;
    int ns = hs_proto_decode_varint_swar(o, off, &d);
    WF_FUZZ_ASSERT(wn <= 8 ? (ns == wn && d == v) : ns == 0);
    wf_free(o, psz);

    /* varint field */
    sz = (size_t)off + 1 + (size_t)wn;
    o = wf_alloc(sz);
    WF_FUZZ_ASSERT(hs_proto_encode_varint_field(o, off, tag, v) == 1 + wn);
    WF_FUZZ_ASSERT(o[off] == tag && memcmp(o + off + 1, want, (size_t)wn) == 0);
    WF_FUZZ_ASSERT(hs_proto_skip_field(o, (int)sz, off + 1, 0) == (int)sz);
    wf_free(o, sz);

    /* bool field */
    sz = (size_t)off + 2;
    o = wf_alloc(sz);
    WF_FUZZ_ASSERT(hs_proto_encode_bool_field(o, off, tag, (int)(uint32_t)v) == 2);
    WF_FUZZ_ASSERT(o[off] == tag && o[off + 1] == ((uint32_t)v != 0));
    wf_free(o, sz);

    /* length-delimited field; empty payload passes NULL like Haskell */
    uint8_t hdr[10];
    int hn = ref_encode_varint(hdr, (uint64_t)plen);
    sz = (size_t)off + 1 + (size_t)hn + (size_t)plen;
    o = wf_alloc(sz);
    WF_FUZZ_ASSERT(hs_proto_encode_length_delimited(o, off, tag, plen ? payload : NULL, plen)
                   == 1 + hn + plen);
    WF_FUZZ_ASSERT(o[off] == tag);
    WF_FUZZ_ASSERT(memcmp(o + off + 1, hdr, (size_t)hn) == 0);
    if (plen) WF_FUZZ_ASSERT(memcmp(o + off + 1 + hn, payload, (size_t)plen) == 0);
    int fnum = 0, wt = 0;
    uint64_t tv = 0;
    int tn = ref_decode_varint(o, (int)sz, off, &tv);
    WF_FUZZ_ASSERT(hs_proto_decode_tag_fast(o, (int)sz, off, &fnum, &wt) == tn);
    if (tag < 0x80) WF_FUZZ_ASSERT(tn == 1 && fnum == tag >> 3 && wt == (tag & 7));
    WF_FUZZ_ASSERT(hs_proto_skip_field(o, (int)sz, off + 1, 2) == (int)sz);
    /* one byte short: the declared length no longer fits */
    WF_FUZZ_ASSERT(hs_proto_skip_field(o, (int)sz - 1, off + 1, 2) == -1);
    wf_free(o, sz);
    wf_free(payload, (size_t)plen);
    return 0;
}
