/* Tests for wireform-http2/cbits/hpack_huffman.c: RFC 7541 Appendix C
 * vectors, every symbol and symbol pair, exhaustive short inputs against
 * the bit-by-bit reference decoder, the section 5.2 rejection rules,
 * output capacity bounds, and randomized round trips. */
#include "harness.h"
#include "api.h"

/* ------------------------------------------------------------------ */
/* Helpers                                                             */
/* ------------------------------------------------------------------ */

static size_t unhex(const char *h, uint8_t *out)
{
    size_t n = 0;
    while (h[0] && h[1]) {
        unsigned v;
        sscanf(h, "%2x", &v);
        out[n++] = (uint8_t)v;
        h += 2;
    }
    return n;
}

/* Encode through the kernel into an exact-size buffer and compare with
 * the reference encoder. Returns a wf_dup'd copy of the encoding (caller
 * frees with wf_free(p, *enc_len)). */
static uint8_t *check_encode(const uint8_t *s, size_t n, size_t *enc_len)
{
    uint8_t *src = wf_dup(s, n);
    size_t want = ref_encode_len(s, n);
    size_t got_len = wireform_hpack_huffman_encode_len(src, n);
    WF_CHECK_EQ(got_len, want);

    uint8_t *ref = malloc(want ? want : 1);
    WF_CHECK_EQ(ref_encode(s, n, ref), want);

    uint8_t *dst = wf_alloc(want);
    size_t w = wireform_hpack_huffman_encode(src, n, dst);
    WF_CHECK_EQ(w, want);
    if (w == want) WF_CHECK_MEM(dst, ref, want);

    uint8_t *copy = wf_dup(ref, want);
    wf_free(dst, want);
    free(ref);
    wf_free(src, n);
    *enc_len = want;
    return copy;
}

/* Decode src[0..n) with capacity cap through the kernel, both in exact-size
 * buffers, and compare every observable with the reference. Returns the
 * kernel's return code. */
static int check_decode_cap(const uint8_t *s, size_t n, size_t cap)
{
    uint8_t *ref = malloc(REF_DECODE_MAX(n));
    size_t emitted = 0;
    int ref_rc = ref_decode(s, n, ref, &emitted);

    uint8_t *src = wf_dup(s, n);
    uint8_t *dst = wf_alloc(cap);
    size_t out_len = (size_t)-1;
    int rc = wireform_hpack_huffman_decode(src, n, dst, cap, &out_len);

    int want_rc = (ref_rc == 0 && emitted <= cap) ? 0 : -1;
    size_t want_len = emitted < cap ? emitted : cap;
    WF_CHECK_EQ(rc, want_rc);
    WF_CHECK_EQ(out_len, want_len);
    if (out_len == want_len) WF_CHECK_MEM(dst, ref, out_len);
    if (rc != want_rc || out_len != want_len) wf_hexdump("input", s, n);

    wf_free(dst, cap);
    wf_free(src, n);
    free(ref);
    return rc;
}

/* Decode with the capacity huffmanDecode passes. */
static int check_decode(const uint8_t *s, size_t n)
{
    return check_decode_cap(s, n, 2 * n + 256);
}

/* Full round trip of a plaintext: encode matches the reference, decoding
 * returns it at the Haskell capacity and at the exact capacity, fails
 * cleanly one byte short, and padding mutations are rejected. */
static void check_roundtrip(const uint8_t *s, size_t n)
{
    size_t elen;
    uint8_t *enc = check_encode(s, n, &elen);

    WF_CHECK_EQ(check_decode(enc, elen), 0);
    WF_CHECK_EQ(check_decode_cap(enc, elen, n), 0);
    if (n > 0) WF_CHECK_EQ(check_decode_cap(enc, elen, n - 1), -1);

    {
        uint8_t *src = wf_dup(enc, elen);
        uint8_t *dst = wf_alloc(n);
        size_t out_len = (size_t)-1;
        WF_CHECK_EQ(wireform_hpack_huffman_decode(src, elen, dst, n, &out_len), 0);
        WF_CHECK_EQ(out_len, n);
        if (out_len == n) WF_CHECK_MEM(dst, s, n);
        wf_free(dst, n);
        wf_free(src, elen);
    }

    /* An extra 0xFF byte makes the padding 8+ bits of ones. */
    {
        uint8_t *m = malloc(elen + 1);
        if (elen) memcpy(m, enc, elen);
        m[elen] = 0xFF;
        WF_CHECK_EQ(check_decode(m, elen + 1), -1);
        free(m);
    }

    /* Clearing a padding bit leaves pending bits that are not a prefix
     * of EOS: an error, unless they now spell a whole short code (e.g.
     * 7 bits 1111011 is 'z'), in which case the string grows by that
     * symbol. Either way the kernel must agree with the reference. */
    size_t bits = 0;
    for (size_t i = 0; i < n; i++) bits += ref_len[s[i]];
    size_t pad = elen * 8 - bits;
    for (size_t p = 0; p < pad; p++) {
        uint8_t *m = wf_dup(enc, elen);
        m[elen - 1] &= (uint8_t)~(1u << p);
        uint8_t out[1024];
        size_t emitted = 0;
        if (ref_decode(m, elen, out, &emitted) == 0) WF_CHECK_EQ(emitted, n + 1);
        check_decode(m, elen);
        wf_free(m, elen);
    }

    wf_free(enc, elen);
}

/* ------------------------------------------------------------------ */
/* RFC vectors                                                         */
/* ------------------------------------------------------------------ */

/* Every Huffman-coded string literal in RFC 7541 C.4 and C.6. */
static const struct {
    const char *plain;
    const char *hex;
} rfc_vectors[] = {
    { "www.example.com", "f1e3c2e5f23a6ba0ab90f4ff" },
    { "no-cache", "a8eb10649cbf" },
    { "custom-key", "25a849e95ba97d7f" },
    { "custom-value", "25a849e95bb8e8b4bf" },
    { "302", "6402" },
    { "private", "aec3771a4b" },
    { "Mon, 21 Oct 2013 20:13:21 GMT",
      "d07abe941054d444a8200595040b8166e082a62d1bff" },
    { "https://www.example.com", "9d29ad171863c78f0b97c8e9ae82ae43d3" },
    { "307", "640eff" },
    { "Mon, 21 Oct 2013 20:13:22 GMT",
      "d07abe941054d444a8200595040b8166e084a62d1bff" },
    { "gzip", "9bd9ab" },
    { "foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1",
      "94e7821dd7f2e6c7b335dfdfcd5b3960d5af27087f3672c1ab270fb5291f9587"
      "316065c003ed4ee5b1063d5007" },
};

static void test_rfc_vectors(void)
{
    for (size_t v = 0; v < sizeof rfc_vectors / sizeof rfc_vectors[0]; v++) {
        const uint8_t *plain = (const uint8_t *)rfc_vectors[v].plain;
        size_t n = strlen(rfc_vectors[v].plain);
        uint8_t want[128];
        size_t wn = unhex(rfc_vectors[v].hex, want);

        size_t elen;
        uint8_t *enc = check_encode(plain, n, &elen);
        WF_CHECK_EQ(elen, wn);
        if (elen == wn) WF_CHECK_MEM(enc, want, wn);
        wf_free(enc, elen);

        uint8_t *src = wf_dup(want, wn);
        uint8_t *dst = wf_alloc(n);
        size_t out_len = 0;
        WF_CHECK_EQ(wireform_hpack_huffman_decode(src, wn, dst, n, &out_len), 0);
        WF_CHECK_EQ(out_len, n);
        if (out_len == n) WF_CHECK_MEM(dst, plain, n);
        wf_free(dst, n);
        wf_free(src, wn);

        check_roundtrip(plain, n);
    }
}

/* ------------------------------------------------------------------ */
/* Symbols                                                             */
/* ------------------------------------------------------------------ */

/* Each symbol alone and repeated 1..17 times, which puts its code at
 * every bit offset and gives every padding length 0..7. */
static void test_every_symbol(void)
{
    uint8_t buf[17];
    for (int b = 0; b < 256; b++) {
        for (size_t r = 1; r <= sizeof buf; r++) {
            memset(buf, b, r);
            check_roundtrip(buf, r);
        }
    }
    check_roundtrip(NULL, 0);
}

static void test_symbol_pairs(void)
{
    for (int a = 0; a < 256; a++) {
        for (int b = 0; b < 256; b++) {
            uint8_t s[2] = { (uint8_t)a, (uint8_t)b };
            size_t elen;
            uint8_t *enc = check_encode(s, 2, &elen);
            WF_CHECK_EQ(check_decode_cap(enc, elen, 2), 0);
            WF_CHECK_EQ(check_decode_cap(enc, elen, 1), -1);
            wf_free(enc, elen);
        }
    }
}

/* All 256 byte values in one string, both orders. */
static void test_all_bytes(void)
{
    uint8_t s[256];
    for (int i = 0; i < 256; i++) s[i] = (uint8_t)i;
    check_roundtrip(s, 256);
    for (int i = 0; i < 256; i++) s[i] = (uint8_t)(255 - i);
    check_roundtrip(s, 256);
}

/* ------------------------------------------------------------------ */
/* Exhaustive short inputs                                             */
/* ------------------------------------------------------------------ */

/* Every input of 0..3 bytes against the reference, at the Haskell
 * capacity; 0..2 bytes also at every capacity 0..4. */
static void test_exhaustive_short(void)
{
    check_decode(NULL, 0);
    for (int a = 0; a < 256; a++) {
        uint8_t s[1] = { (uint8_t)a };
        for (size_t cap = 0; cap <= 3; cap++) check_decode_cap(s, 1, cap);
        check_decode(s, 1);
    }
    for (int a = 0; a < 65536; a++) {
        uint8_t s[2] = { (uint8_t)(a >> 8), (uint8_t)a };
        for (size_t cap = 0; cap <= 4; cap++) check_decode_cap(s, 2, cap);
    }

    /* Three bytes: compare directly, without per-call allocation. */
    uint8_t ref[8], dst[8];
    for (uint32_t a = 0; a < (1u << 24); a++) {
        uint8_t s[3] = { (uint8_t)(a >> 16), (uint8_t)(a >> 8), (uint8_t)a };
        size_t emitted = 0, out_len = (size_t)-1;
        int ref_rc = ref_decode(s, 3, ref, &emitted);
        int rc = wireform_hpack_huffman_decode(s, 3, dst, sizeof dst, &out_len);
        if (rc != ref_rc || out_len != emitted || memcmp(dst, ref, emitted) != 0) {
            WF_CHECK_EQ(rc, ref_rc);
            WF_CHECK_EQ(out_len, emitted);
            wf_hexdump("input", s, 3);
        }
    }
    wf__checks += 1 << 24;
}

/* ------------------------------------------------------------------ */
/* RFC 7541 section 5.2 rejections                                     */
/* ------------------------------------------------------------------ */

static void test_rejections(void)
{
    /* EOS alone: 30 ones + 2 ones of padding. */
    {
        uint8_t s[] = { 0xFF, 0xFF, 0xFF, 0xFF };
        WF_CHECK_EQ(check_decode(s, sizeof s), -1);
    }
    /* EOS followed by valid-looking data: 30 ones then '0' (00000) and
     * padding. Must still fail, having emitted nothing. */
    {
        uint8_t s[] = { 0xFF, 0xFF, 0xFF, 0xFC, 0x1F };
        WF_CHECK_EQ(check_decode(s, sizeof s), -1);
    }
    /* 'a' (00011), then EOS, then padding: 5 + 30 + 5 = 40 bits. The
     * decoder must have emitted 'a' only. */
    {
        uint8_t s[] = { 0x1F, 0xFF, 0xFF, 0xFF, 0xFF };
        uint8_t dst[16];
        size_t out_len = 0;
        WF_CHECK_EQ(wireform_hpack_huffman_decode(s, sizeof s, dst, sizeof dst, &out_len), -1);
        WF_CHECK_EQ(out_len, 1);
        WF_CHECK_EQ(dst[0], 'a');
        WF_CHECK_EQ(check_decode(s, sizeof s), -1);
    }
    /* Padding of 8..29 ones after a symbol. */
    {
        uint8_t s[] = { 0x1F, 0xFF, 0xFF, 0xFF };
        for (size_t n = 2; n <= 4; n++) WF_CHECK_EQ(check_decode(s, n), -1);
    }
    /* Padding of 8+ ones with no symbol at all: 0xFF, 0xFFFF, 0xFFFFFF. */
    {
        uint8_t s[] = { 0xFF, 0xFF, 0xFF };
        for (size_t n = 1; n <= 3; n++) WF_CHECK_EQ(check_decode(s, n), -1);
    }
    /* Padding not all ones: 'a' followed by 000, 001, ..., 110. */
    for (int pad = 0; pad < 7; pad++) {
        uint8_t s[] = { (uint8_t)(0x18 | pad) };
        WF_CHECK_EQ(check_decode(s, 1), -1);
    }
    /* 'a' with full 111 padding is fine. */
    {
        uint8_t s[] = { 0x1F };
        WF_CHECK_EQ(check_decode(s, 1), 0);
    }
    /* All-zero runs: 00000 is '0', so 5k zero bytes decode to 8k '0's
     * and every other length leaves zero padding. */
    {
        uint8_t z[40] = { 0 };
        for (size_t n = 0; n <= sizeof z; n++) {
            int rc = check_decode(z, n);
            WF_CHECK_EQ(rc, n % 5 == 0 ? 0 : -1);
        }
    }
    /* All-0xFF of every length up to 40: only the empty string is valid. */
    {
        uint8_t f[40];
        memset(f, 0xFF, sizeof f);
        for (size_t n = 1; n <= sizeof f; n++) WF_CHECK_EQ(check_decode(f, n), -1);
    }
    /* Sign-bit bytes and mixed patterns, against the reference. */
    {
        uint8_t pat[] = { 0x80, 0x7F, 0xFE, 0x01, 0xAA, 0x55, 0xF0, 0x0F };
        uint8_t s[48];
        for (size_t p = 0; p < sizeof pat; p++) {
            memset(s, pat[p], sizeof s);
            for (size_t n = 0; n <= sizeof s; n++) check_decode(s, n);
        }
    }
}

/* ------------------------------------------------------------------ */
/* Output capacity                                                     */
/* ------------------------------------------------------------------ */

/* Densest inputs (5-bit codes only) decode to floor(8n/5) bytes, the
 * worst case the Haskell 2n+256 capacity has to cover; every capacity
 * below the decoded size must fail without writing past it. */
static void test_capacity(void)
{
    static const uint8_t five_bit[] = "012aceiost";
    wf_rng r = wf_rng_from_env();
    uint8_t plain[200];
    for (size_t n = 0; n <= sizeof plain; n++) {
        wf_rng_fill_from(&r, plain, n, five_bit, 10);
        size_t elen;
        uint8_t *enc = check_encode(plain, n, &elen);
        WF_CHECK(n <= elen * 8 / 5);
        WF_CHECK(elen * 8 / 5 <= 2 * elen + 256);
        for (size_t cap = 0; cap <= n + 2; cap++) {
            WF_CHECK_EQ(check_decode_cap(enc, elen, cap), cap >= n ? 0 : -1);
        }
        wf_free(enc, elen);
    }
    /* Exactly floor(8n/5): n = 5k bytes of zeros give 8k symbols. */
    for (size_t k = 1; k <= 20; k++) {
        uint8_t z[100] = { 0 };
        size_t n = 5 * k;
        WF_CHECK_EQ(check_decode_cap(z, n, n * 8 / 5), 0);
        WF_CHECK_EQ(check_decode_cap(z, n, n * 8 / 5 - 1), -1);
    }
}

/* ------------------------------------------------------------------ */
/* Guard-page sweeps                                                   */
/* ------------------------------------------------------------------ */

/* Inputs flush against a guard page at every length and start offset,
 * for the encoder, encode_len and the decoder. */
static void test_guard_sweep(void)
{
    wf_rng r = wf_rng_from_env();
    enum { MAX = 96 };
    uint8_t plain[MAX];
    wf_rng_fill(&r, plain, MAX);

    for (size_t n = 0; n <= MAX; n++) {
        size_t elen;
        uint8_t *enc = check_encode(plain, n, &elen);

        wf_guarded g = wf_guard_tail(elen);
        if (elen) memcpy(g.p, enc, elen);
        for (size_t k = 0; k <= elen; k++) {
            uint8_t *dst = wf_alloc(2 * elen + 256);
            size_t out_len = 0;
            wireform_hpack_huffman_decode(g.p + k, elen - k, dst, 2 * elen + 256, &out_len);
            WF_CHECK(out_len <= 2 * elen + 256);
            wf_free(dst, 2 * elen + 256);
        }
        wf_guard_free(g);

        wf_guarded h = wf_guard_head(elen);
        if (elen) memcpy(h.p, enc, elen);
        uint8_t *dst = wf_alloc(n);
        size_t out_len = 0;
        WF_CHECK_EQ(wireform_hpack_huffman_decode(h.p, elen, dst, n, &out_len), 0);
        WF_CHECK_EQ(out_len, n);
        if (out_len == n) WF_CHECK_MEM(dst, plain, n);
        wf_free(dst, n);
        wf_guard_free(h);

        wf_guarded s = wf_guard_tail(n);
        if (n) memcpy(s.p, plain, n);
        for (size_t k = 0; k <= n; k++) {
            size_t want = ref_encode_len(plain + k, n - k);
            WF_CHECK_EQ(wireform_hpack_huffman_encode_len(s.p + k, n - k), want);
            wf_guarded o = wf_guard_tail(want);
            WF_CHECK_EQ(wireform_hpack_huffman_encode(s.p + k, n - k, o.p), want);
            wf_guard_free(o);
        }
        wf_guard_free(s);

        wf_free(enc, elen);
    }
}

/* ------------------------------------------------------------------ */
/* Randomized                                                          */
/* ------------------------------------------------------------------ */

static void test_random_roundtrip(void)
{
    static const uint8_t header_chars[] =
        "abcdefghijklmnopqrstuvwxyz0123456789-_.:/;=, ";
    static const uint8_t long_codes[] = { 0, 1, 10, 13, 22, 127, 128, 200, 254, 255 };
    wf_rng r = wf_rng_from_env();
    long iters = wf_iters(3000);
    uint8_t plain[512];
    for (long it = 0; it < iters; it++) {
        size_t n = (size_t)wf_rng_below(&r, sizeof plain + 1);
        switch (wf_rng_below(&r, 3)) {
        case 0: wf_rng_fill(&r, plain, n); break;
        case 1: wf_rng_fill_from(&r, plain, n, header_chars, sizeof header_chars - 1); break;
        default: wf_rng_fill_from(&r, plain, n, long_codes, sizeof long_codes); break;
        }
        check_roundtrip(plain, n);
    }
}

/* Random bytes, biased towards long runs of ones (EOS and padding
 * territory), decoded against the reference at random capacities. */
static void test_random_decode(void)
{
    static const uint8_t ones_heavy[] = { 0xFF, 0xFF, 0xFF, 0xFE, 0xFC, 0xF8, 0x7F, 0x3F, 0x00, 0x80 };
    wf_rng r = wf_rng_from_env();
    long iters = wf_iters(20000);
    uint8_t s[96];
    for (long it = 0; it < iters; it++) {
        size_t n = (size_t)wf_rng_below(&r, sizeof s + 1);
        if (wf_rng_below(&r, 2)) wf_rng_fill(&r, s, n);
        else wf_rng_fill_from(&r, s, n, ones_heavy, sizeof ones_heavy);
        size_t cap = wf_rng_below(&r, 2) ? 2 * n + 256 : (size_t)wf_rng_below(&r, n * 2 + 2);
        check_decode_cap(s, n, cap);
    }
}

int main(void)
{
    WF_RUN(test_rfc_vectors);
    WF_RUN(test_every_symbol);
    WF_RUN(test_symbol_pairs);
    WF_RUN(test_all_bytes);
    WF_RUN(test_exhaustive_short);
    WF_RUN(test_rejections);
    WF_RUN(test_capacity);
    WF_RUN(test_guard_sweep);
    WF_RUN(test_random_roundtrip);
    WF_RUN(test_random_decode);
    return wf_finish("test_hpack_huffman");
}
