/* Varint, tag, skip, packed and encode tests for wireform-core
 * fast_decode.c, against the scalar references in api.h. */
#include "harness.h"
#include "api.h"
#include "buffers.h"

#include <limits.h>

/* Values on both sides of every 7-bit group boundary, plus the extremes. */
static uint64_t edge_value(int k)
{
    /* k in [0, 64*3): (2^g)-1, 2^g, 2^g+1 for g = 0..63 */
    int g = k / 3, d = k % 3;
    uint64_t p = g == 64 ? 0 : (1ULL << g);
    return d == 0 ? p - 1 : d == 1 ? p : p + 1;
}
#define N_EDGE (64 * 3)

/* Known vectors from the protobuf encoding guide. */
static void varint_vectors(void)
{
    static const struct {
        uint8_t b[10];
        int n;
        uint64_t v;
    } vec[] = {
        { { 0x00 }, 1, 0 },
        { { 0x01 }, 1, 1 },
        { { 0x7F }, 1, 127 },
        { { 0x96, 0x01 }, 2, 150 },
        { { 0xAC, 0x02 }, 2, 300 },
        { { 0x80, 0x80, 0x01 }, 3, 16384 },
        { { 0xFF, 0xFF, 0xFF, 0xFF, 0x0F }, 5, 0xFFFFFFFFULL },
        { { 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x7F }, 9, 0x7FFFFFFFFFFFFFFFULL },
        { { 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01 }, 10, UINT64_MAX },
        /* int32 -1 sign-extended to 10 bytes */
        { { 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01 }, 10, (uint64_t)-1 },
        /* non-minimal (padded) encodings are accepted */
        { { 0x80, 0x00 }, 2, 0 },
        { { 0x81, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x00 }, 10, 1 },
        /* 10th byte bits above 2^64 are dropped (lenient, like upb) */
        { { 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x7F }, 10, 1ULL << 63 },
    };
    for (size_t t = 0; t < sizeof vec / sizeof vec[0]; t++) {
        uint8_t *b = wf_dup(vec[t].b, (size_t)vec[t].n);
        uint64_t v = 0xdead;
        WF_CHECK_EQ(hs_proto_decode_varint_fast(b, vec[t].n, 0, &v), vec[t].n);
        WF_CHECK(v == vec[t].v);
        WF_CHECK_EQ(hs_proto_skip_varint(b, vec[t].n, 0), vec[t].n);
        /* truncated by one byte: unterminated */
        v = 0xdead;
        WF_CHECK_EQ(hs_proto_decode_varint_fast(b, vec[t].n - 1, 0, &v), 0);
        WF_CHECK(v == 0xdead);
        WF_CHECK_EQ(hs_proto_skip_varint(b, vec[t].n - 1, 0), 0);
        wf_free(b, (size_t)vec[t].n);
    }
}

/* Every prefix of every 1..11 byte continuation run, flush against a
 * guard page, at every start offset: decode_fast / skip / skip_field(0)
 * agree with the reference and never read past len. 11-byte varints are
 * rejected by all three. */
static void varint_truncation_sweep(void)
{
    enum { MAXN = 24 };
    /* gap > 0: the bytes after len are 0x01 terminators, so reading past
     * len would turn an unterminated varint into a decoded one */
    static const int gaps[] = { 0, 1, 5, 9 };
    wf_guarded g = wf_guard_tail(MAXN + 16);
    for (size_t gi = 0; gi < sizeof gaps / sizeof gaps[0]; gi++)
    for (int run = 0; run <= 11; run++) {          /* continuation bytes */
        for (int len = 0; len <= MAXN; len++) {
            uint8_t *b = g.p + (g.n - (size_t)len - (size_t)gaps[gi]);
            memset(b + len, 0x01, (size_t)gaps[gi]);
            for (int off = 0; off <= len; off++) {
                for (int i = 0; i < len; i++) {
                    int k = i - off;
                    b[i] = k >= 0 && k < run ? (uint8_t)(0x80 | (k * 13 & 0x7F))
                                             : (uint8_t)(0x11 + i);
                }
                uint64_t v1 = 1, v2 = 1;
                int n1 = hs_proto_decode_varint_fast(b, len, off, &v1);
                int n2 = ref_decode_varint(b, len, off, &v2);
                WF_CHECK_EQ(n1, n2);
                WF_CHECK(v1 == v2);
                WF_CHECK_EQ(hs_proto_skip_varint(b, len, off), n2);
                WF_CHECK_EQ(hs_proto_skip_field(b, len, off, 0), n2 ? off + n2 : -1);
                int fnum = -1, wt = -1;
                int n3 = hs_proto_decode_tag_fast(b, len, off, &fnum, &wt);
                WF_CHECK_EQ(n3, n2);
                if (n2) {
                    WF_CHECK_EQ(wt, (int)(v2 & 7));
                    WF_CHECK_EQ(fnum, (int)(v2 >> 3));
                } else {
                    WF_CHECK_EQ(fnum, -1);
                    WF_CHECK_EQ(wt, -1);
                }
            }
        }
    }
    wf_guard_free(g);
}

/* Regression: skip_varint accepted an 11-byte varint (10 continuation
 * bytes and a terminator) because its length check ran one byte late;
 * decode_varint_fast rejects the same input. */
static void skip_varint_rejects_11_bytes(void)
{
    uint8_t in[11];
    memset(in, 0x80, 10);
    in[10] = 0x00;
    uint8_t *b = wf_dup(in, sizeof in);
    uint64_t v;
    WF_CHECK_EQ(hs_proto_decode_varint_fast(b, 11, 0, &v), 0);
    WF_CHECK_EQ(hs_proto_skip_varint(b, 11, 0), 0);
    WF_CHECK_EQ(hs_proto_skip_field(b, 11, 0, 0), -1);
    wf_free(b, sizeof in);
}

/* Encode every edge value with every encoder, at every offset into an
 * exact-size output, then decode it back with every decoder. */
static void encode_roundtrip_edges(void)
{
    for (int k = 0; k <= N_EDGE; k++) {
        uint64_t v = k == N_EDGE ? UINT64_MAX : edge_value(k);
        uint8_t want[10];
        int wn = ref_encode_varint(want, v);
        WF_CHECK_EQ(wn, ref_varint_size(v));
        for (int off = 0; off < 3; off++) {
            size_t sz = (size_t)off + (size_t)wn;
            uint8_t *o = wf_alloc(sz);
            memset(o, 0xEE, sz);
            WF_CHECK_EQ(hs_proto_encode_varint(o, off, v), wn);
            WF_CHECK_MEM(o + off, want, (size_t)wn);
            for (int i = 0; i < off; i++) WF_CHECK_EQ(o[i], 0xEE);
            uint64_t d = 0;
            WF_CHECK_EQ(hs_proto_decode_varint_fast(o, (int)sz, off, &d), wn);
            WF_CHECK(d == v);
            WF_CHECK_EQ(hs_proto_skip_varint(o, (int)sz, off), wn);
            wf_free(o, sz);

            /* varint field: tag + varint */
            sz = (size_t)off + 1 + (size_t)wn;
            o = wf_alloc(sz);
            WF_CHECK_EQ(hs_proto_encode_varint_field(o, off, (uint8_t)(0x08 + k), v), 1 + wn);
            WF_CHECK_EQ(o[off], (uint8_t)(0x08 + k));
            WF_CHECK_MEM(o + off + 1, want, (size_t)wn);
            wf_free(o, sz);

            /* SWAR decode over a buffer padded to 8 readable bytes */
            size_t pad = (size_t)off + (wn > 8 ? (size_t)wn : 8);
            o = wf_alloc(pad);
            memset(o, 0, pad);
            memcpy(o + off, want, (size_t)wn);
            d = 0;
            int sn = hs_proto_decode_varint_swar(o, off, &d);
            if (wn <= 8) {
                WF_CHECK_EQ(sn, wn);
                WF_CHECK(d == v);
            } else {
                WF_CHECK_EQ(sn, 0);
            }
            wf_free(o, pad);
        }
    }
}

/* SWAR decode on 8-byte windows flush against a guard page, for every
 * pattern of sign bits (256) and assorted payload bits. */
static void swar_exhaustive_sign_patterns(void)
{
    wf_rng r = wf_rng_from_env();
    wf_guarded g = wf_guard_tail(8);
    for (int pat = 0; pat < 256; pat++) {
        for (int rep = 0; rep < 16; rep++) {
            for (int i = 0; i < 8; i++) {
                uint8_t payload = rep == 0 ? 0x00 : rep == 1 ? 0x7F : (uint8_t)(wf_rng_next(&r) & 0x7F);
                g.p[i] = (uint8_t)(payload | ((pat >> i) & 1 ? 0x80 : 0));
            }
            uint64_t a = 7, b = 7;
            int na = hs_proto_decode_varint_swar(g.p, 0, &a);
            int nb = ref_decode_varint8(g.p, 0, &b);
            WF_CHECK_EQ(na, nb);
            WF_CHECK(a == b);
        }
    }
    wf_guard_free(g);
}

static void swar_random(void)
{
    wf_rng r = wf_rng_from_env();
    for (long it = 0; it < wf_iters(50000); it++) {
        int len = 8 + (int)wf_rng_below(&r, 24);
        uint8_t *b = wf_alloc((size_t)len);
        for (int i = 0; i < len; i++) {
            b[i] = (uint8_t)wf_rng_next(&r);
            if (wf_rng_below(&r, 4) == 0) b[i] &= 0x7F;
        }
        int off = (int)wf_rng_below(&r, (uint64_t)len - 7);
        uint64_t a = 3, c = 3;
        WF_CHECK_EQ(hs_proto_decode_varint_swar(b, off, &a), ref_decode_varint8(b, off, &c));
        WF_CHECK(a == c);
        wf_free(b, (size_t)len);
    }
}

/* Random encode streams decoded as a packed run, through every packed
 * entry point; then the same stream with corruptions. */
static void packed_random(void)
{
    wf_rng r = wf_rng_from_env();
    for (long it = 0; it < wf_iters(20000); it++) {
        int nvals = (int)wf_rng_below(&r, 40);
        uint8_t tmp[40 * 10 + 1];
        uint64_t vals[40];
        int len = 0;
        int small = wf_rng_below(&r, 3) == 0;
        for (int i = 0; i < nvals; i++) {
            uint64_t v = wf_rng_next(&r) >> wf_rng_below(&r, 64);
            if (small) v &= 0x7F;
            vals[i] = v;
            len += ref_encode_varint(tmp + len, v);
        }
        int mut = (int)wf_rng_below(&r, 4);
        if (mut == 1 && len > 0) tmp[wf_rng_below(&r, (uint64_t)len)] ^= 0x80;
        if (mut == 2 && len > 0) len -= (int)wf_rng_below(&r, (uint64_t)len) + 1;
        uint8_t *b = wf_dup(tmp, (size_t)len);
        WF_CHECK_EQ(hs_proto_count_packed_varints(b, len), ref_count_packed(b, len));
        WF_CHECK_EQ(hs_proto_packed_all_single_byte(b, len), ref_all_single_byte(b, len));
        int max_out = mut == 3 ? (int)wf_rng_below(&r, (uint64_t)nvals + 1) : len;
        size_t osz = (size_t)(max_out > 0 ? max_out : 0) * sizeof(uint64_t);
        a8buf ob1 = a8_alloc(osz);
        uint64_t *o1 = ob1.p;
        uint64_t o2[40 * 10 + 1];
        int c1 = hs_proto_decode_packed_varints(b, len, o1, max_out);
        int c2 = ref_decode_packed(b, len, o2, max_out);
        WF_CHECK_EQ(c1, c2);
        if (c1 > 0) WF_CHECK_MEM(o1, o2, (size_t)c1 * sizeof(uint64_t));
        if (mut == 0) {
            WF_CHECK_EQ(c1, nvals);
            if (c1 > 0) WF_CHECK_MEM(o1, vals, (size_t)c1 * sizeof(uint64_t));
        }
        a8_free(ob1);
        a8buf ob3 = a8_alloc((size_t)len * sizeof(uint64_t));
        uint64_t *o3 = ob3.p;
        WF_CHECK_EQ(hs_proto_decode_packed_single_byte_varints(b, len, o3), len);
        for (int i = 0; i < len; i++) WF_CHECK(o3[i] == b[i]);
        a8_free(ob3);
        wf_free(b, (size_t)len);
    }
}

/* Packed scanners at every length 0..3 SIMD blocks and every start
 * alignment, flush against a guard page, with a single high-bit byte
 * at each position (and all-0x00 / all-0xFF / all-0x80 fills). */
static void packed_scan_boundaries(void)
{
    enum { MAXN = 70 };
    /* gap > 0: the bytes after len flip every high bit relative to the
     * fill, so reading past len changes the count / single-byte answer */
    static const int gaps[] = { 0, 1, 7, 15 };
    wf_guarded g = wf_guard_tail(MAXN + 16);
    static const uint8_t fills[] = { 0x00, 0x7F, 0x80, 0xFF, 0x41 };
    for (size_t gi = 0; gi < sizeof gaps / sizeof gaps[0]; gi++)
    for (size_t f = 0; f < sizeof fills; f++) {
        for (int len = 0; len <= MAXN; len++) {
            uint8_t *b = g.p + (g.n - (size_t)len - (size_t)gaps[gi]);
            memset(b + len, fills[f] ^ 0x80, (size_t)gaps[gi]);
            for (int pos = -1; pos < len; pos++) {
                memset(b, fills[f], (size_t)len);
                if (pos >= 0) b[pos] ^= 0x80;
                WF_CHECK_EQ(hs_proto_count_packed_varints(b, len), ref_count_packed(b, len));
                WF_CHECK_EQ(hs_proto_packed_all_single_byte(b, len), ref_all_single_byte(b, len));
                a8buf ob = a8_alloc((size_t)len * 8);
                uint64_t *o = ob.p;
                WF_CHECK_EQ(hs_proto_decode_packed_single_byte_varints(b, len, o), len);
                for (int i = 0; i < len; i++) WF_CHECK(o[i] == b[i]);
                a8_free(ob);
            }
        }
    }
    /* empty ByteString: NULL pointer, zero length */
    WF_CHECK_EQ(hs_proto_count_packed_varints(NULL, 0), 0);
    WF_CHECK_EQ(hs_proto_packed_all_single_byte(NULL, 0), 1);
    WF_CHECK_EQ(hs_proto_decode_packed_single_byte_varints(NULL, 0, NULL), 0);
    WF_CHECK_EQ(hs_proto_decode_packed_varints(NULL, 0, NULL, 0), 0);
    uint64_t v;
    WF_CHECK_EQ(hs_proto_decode_varint_fast(NULL, 0, 0, &v), 0);
    WF_CHECK_EQ(hs_proto_skip_varint(NULL, 0, 0), 0);
    wf_guard_free(g);
}

/* decode_packed_varints: max_out bounds the output exactly. */
static void packed_max_out(void)
{
    uint8_t in[] = { 1, 0x96, 0x01, 3, 0xFF, 0x7F };
    uint8_t *b = wf_dup(in, sizeof in);
    for (int m = 0; m <= 5; m++) {
        a8buf ob = a8_alloc((size_t)m * 8);
        uint64_t *o = ob.p;
        int c = hs_proto_decode_packed_varints(b, (int)sizeof in, o, m);
        WF_CHECK_EQ(c, m >= 4 ? 4 : -1);
        if (m >= 4) {
            WF_CHECK(o[0] == 1 && o[1] == 150 && o[2] == 3 && o[3] == 16383);
        }
        a8_free(ob);
    }
    /* trailing unterminated varint */
    WF_CHECK_EQ(hs_proto_decode_packed_varints(b, 5, (uint64_t[8]){ 0 }, 8), -1);
    wf_free(b, sizeof in);
}

/* encode_length_delimited: every length across the 1/2/3-byte varint
 * boundaries, exact-size destination at several offsets. */
static void length_delimited(void)
{
    static const int lens[] = { 0, 1, 2, 15, 16, 127, 128, 129, 300, 16383, 16384, 16385, 70000 };
    wf_rng r = wf_rng_from_env();
    for (size_t li = 0; li < sizeof lens / sizeof lens[0]; li++) {
        int len = lens[li];
        uint8_t *src = wf_alloc((size_t)len);
        wf_rng_fill(&r, src, (size_t)len);
        for (int off = 0; off < 4; off++) {
            uint8_t hdr[10];
            int hn = ref_encode_varint(hdr, (uint64_t)len);
            size_t sz = (size_t)off + 1 + (size_t)hn + (size_t)len;
            uint8_t *o = wf_alloc(sz);
            memset(o, 0xCC, (size_t)off);
            WF_CHECK_EQ(hs_proto_encode_length_delimited(o, off, 0x12, src, len), 1 + hn + len);
            for (int i = 0; i < off; i++) WF_CHECK_EQ(o[i], 0xCC);
            WF_CHECK_EQ(o[off], 0x12);
            WF_CHECK_MEM(o + off + 1, hdr, (size_t)hn);
            WF_CHECK_MEM(o + off + 1 + hn, src, (size_t)len);
            /* and it parses back as a wire-type-2 field */
            int fnum, wt;
            WF_CHECK_EQ(hs_proto_decode_tag_fast(o, (int)sz, off, &fnum, &wt), 1);
            WF_CHECK_EQ(fnum, 2);
            WF_CHECK_EQ(wt, 2);
            WF_CHECK_EQ(hs_proto_skip_field(o, (int)sz, off + 1, 2), (int)sz);
            wf_free(o, sz);
        }
        wf_free(src, (size_t)len);
    }
}

/* Regression: an empty ByteString / Text reaches encodeLengthDelimitedC
 * as (NULL, 0); memcpy(dst, NULL, 0) is undefined behaviour. */
static void length_delimited_null_empty(void)
{
    uint8_t *o = wf_alloc(2);
    WF_CHECK_EQ(hs_proto_encode_length_delimited(o, 0, 0x0A, NULL, 0), 2);
    WF_CHECK_EQ(o[0], 0x0A);
    WF_CHECK_EQ(o[1], 0x00);
    wf_free(o, 2);
}

static void bool_field(void)
{
    static const int vals[] = { 0, 1, 2, -1, INT_MIN, INT_MAX, 0x100 };
    for (size_t i = 0; i < sizeof vals / sizeof vals[0]; i++) {
        for (int off = 0; off < 3; off++) {
            uint8_t *o = wf_alloc((size_t)off + 2);
            WF_CHECK_EQ(hs_proto_encode_bool_field(o, off, 0x18, vals[i]), 2);
            WF_CHECK_EQ(o[off], 0x18);
            WF_CHECK_EQ(o[off + 1], vals[i] != 0);
            wf_free(o, (size_t)off + 2);
        }
    }
}

/* skip_field for every wire type at every truncation point. */
static void skip_field_all_types(void)
{
    wf_rng r = wf_rng_from_env();
    for (long it = 0; it < wf_iters(30000); it++) {
        uint8_t tmp[64];
        int len = (int)wf_rng_below(&r, sizeof tmp + 1);
        for (int i = 0; i < len; i++) {
            tmp[i] = (uint8_t)wf_rng_next(&r);
            if (wf_rng_below(&r, 3) == 0) tmp[i] &= 0x3F; /* short lengths */
        }
        uint8_t *b = wf_dup(tmp, (size_t)len);
        int off = (int)wf_rng_below(&r, (uint64_t)len + 1);
        for (int wt = -1; wt <= 8; wt++) {
            WF_CHECK_EQ(hs_proto_skip_field(b, len, off, wt), ref_skip_field(b, len, off, wt));
        }
        wf_free(b, (size_t)len);
    }
}

/* Regression: a wire-type-2 length was narrowed with (int)field_len, so a
 * declared length of 2^32-1 became -1 and skip_field returned an offset
 * *before* the field (a parser looping on it never advances), and
 * lengths near INT_MAX overflowed the int addition. */
static void skip_field_huge_lengths(void)
{
    static const uint64_t lens[] = {
        0xFFFFFFFFULL, 0xFFFFFFFEULL, 0x80000000ULL, 0x7FFFFFFFULL,
        0x100000000ULL, 0x100000002ULL, UINT64_MAX, 1ULL << 63, 5, 6,
    };
    for (size_t i = 0; i < sizeof lens / sizeof lens[0]; i++) {
        uint8_t tmp[16];
        int n = ref_encode_varint(tmp, lens[i]);
        memset(tmp + n, 0x41, 5);
        int len = n + 5;
        uint8_t *b = wf_dup(tmp, (size_t)len);
        int want = lens[i] <= 5 ? n + (int)lens[i] : -1;
        WF_CHECK_EQ(hs_proto_skip_field(b, len, 0, 2), want);
        WF_CHECK_EQ(ref_skip_field(b, len, 0, 2), want);
        wf_free(b, (size_t)len);
    }
}

static void fixed_and_endian(void)
{
    wf_rng r = wf_rng_from_env();
    for (long it = 0; it < wf_iters(5000); it++) {
        uint8_t src[8];
        wf_rng_fill(&r, src, 8);
        for (int w = 2; w <= 8; w *= 2) {
            for (int off = 0; off < 8; off++) {
                size_t sz = (size_t)off + (size_t)w;
                uint8_t *b = wf_alloc(sz);
                memcpy(b + off, src, (size_t)w);
                uint64_t le = ref_load_le(src, w), be = ref_load_be(src, w);
                if (w == 2) {
                    WF_CHECK(hs_proto_read_le16(b, off) == le);
                    WF_CHECK(hs_proto_read_be16(b, off) == be);
                } else if (w == 4) {
                    WF_CHECK(hs_proto_read_le32(b, off) == le);
                    WF_CHECK(hs_proto_read_be32(b, off) == be);
                    WF_CHECK(hs_proto_decode_fixed32(b, off) == le);
                } else {
                    WF_CHECK(hs_proto_read_le64(b, off) == le);
                    WF_CHECK(hs_proto_read_be64(b, off) == be);
                    WF_CHECK(hs_proto_decode_fixed64(b, off) == le);
                }
                /* writers: write the value back in each order */
                memset(b, 0, sz);
                if (w == 2) hs_proto_write_le16(b, off, (uint16_t)le);
                if (w == 4) hs_proto_write_le32(b, off, (uint32_t)le);
                if (w == 8) hs_proto_write_le64(b, off, le);
                WF_CHECK_MEM(b + off, src, (size_t)w);
                memset(b, 0, sz);
                if (w == 2) hs_proto_write_be16(b, off, (uint16_t)be);
                if (w == 4) hs_proto_write_be32(b, off, (uint32_t)be);
                if (w == 8) hs_proto_write_be64(b, off, be);
                WF_CHECK_MEM(b + off, src, (size_t)w);
                wf_free(b, sz);
            }
        }
    }
    uint8_t v[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
    WF_CHECK_EQ(hs_proto_read_be16(v, 0), 0x0102);
    WF_CHECK(hs_proto_read_be32(v, 1) == 0x02030405u);
    WF_CHECK(hs_proto_read_be64(v, 0) == 0x0102030405060708ULL);
    WF_CHECK(hs_proto_read_le64(v, 0) == 0x0807060504030201ULL);
    WF_CHECK(hs_proto_decode_fixed32(v, 4) == 0x08070605u);
}

/* relocate_page_boundary: the 0/1/-1 decision follows the address of
 * buf+len, the copy is exact and the 7 padding bytes are zero. */
static void relocate(void)
{
    WF_CHECK_EQ(hs_proto_relocate_page_boundary(NULL, 0, NULL, 7), 0);
    wf_guarded g = wf_guard_tail(9000);
    uint8_t *page_end = g.p + g.n; /* page aligned */
    for (int back = 0; back < 40; back++) {
        for (int len = 1; len < 40; len++) {
            const uint8_t *buf = page_end - back - len;
            uintptr_t e = (uintptr_t)(buf + len) & 4095;
            int near = e == 0 || e > 4088;
            int out_len = len + 7;
            uint8_t *o = wf_alloc((size_t)out_len);
            memset(o, 0xAA, (size_t)out_len);
            for (int i = 0; i < len; i++) ((uint8_t *)buf)[i] = (uint8_t)(i * 7 + back);
            int rc = hs_proto_relocate_page_boundary(buf, len, o, out_len);
            WF_CHECK_EQ(rc, near);
            if (near) {
                WF_CHECK_MEM(o, buf, (size_t)len);
                for (int i = 0; i < 7; i++) WF_CHECK_EQ(o[len + i], 0);
                /* too-small output buffers are refused */
                WF_CHECK_EQ(hs_proto_relocate_page_boundary(buf, len, o, out_len - 1), -1);
                WF_CHECK_EQ(hs_proto_relocate_page_boundary(buf, len, o, 0), -1);
                WF_CHECK_EQ(hs_proto_relocate_page_boundary(buf, len, o, INT_MIN), -1);
            } else {
                for (int i = 0; i < out_len; i++) WF_CHECK_EQ(o[i], 0xAA);
            }
            wf_free(o, (size_t)out_len);
        }
    }
    wf_guard_free(g);
}

int main(void)
{
    (void)wf_rng_from_env();
    WF_RUN(varint_vectors);
    WF_RUN(varint_truncation_sweep);
    WF_RUN(skip_varint_rejects_11_bytes);
    WF_RUN(encode_roundtrip_edges);
    WF_RUN(swar_exhaustive_sign_patterns);
    WF_RUN(swar_random);
    WF_RUN(packed_random);
    WF_RUN(packed_scan_boundaries);
    WF_RUN(packed_max_out);
    WF_RUN(length_delimited);
    WF_RUN(length_delimited_null_empty);
    WF_RUN(bool_field);
    WF_RUN(skip_field_all_types);
    WF_RUN(skip_field_huge_lengths);
    WF_RUN(fixed_and_endian);
    WF_RUN(relocate);
    return wf_finish("core-fast-decode/varint");
}
