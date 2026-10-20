/* Unit, boundary, differential and round-trip tests for
 * wireform-core/cbits/base64.c. */
#include "harness.h"
#include "api.h"

#define MAXLEN 300

/* Two big tail-guarded scratch areas, mapped once. A slice ending at the
 * guard page (`flush_in` / `flush_out`) gives an exact-size buffer whose
 * next byte faults, without an mmap per check. */
#define SCRATCH (1u << 18)
static wf_guarded g_in, g_out;

static uint8_t *flush_in(const void *src, size_t n)
{
    if (!g_in.p) g_in = wf_guard_tail(SCRATCH);
    uint8_t *p = g_in.p + SCRATCH - n;
    if (n) memmove(p, src, n);
    return p;
}

static uint8_t *flush_out(size_t n)
{
    if (!g_out.p) g_out = wf_guard_tail(SCRATCH);
    return g_out.p + SCRATCH - n;
}

/* Encode `in` (n bytes) with the kernel into an output flush against a
 * guard page of exactly hs_base64_encoded_length(n) bytes and compare
 * with the reference. */
static void check_encode(const uint8_t *in, int n)
{
    int el = hs_base64_encoded_length(n);
    WF_CHECK_EQ(el, ref_b64_encoded_length(n));
    uint8_t *o = flush_out((size_t)el);
    memset(o, 0xEE, (size_t)el);
    uint8_t *want = malloc((size_t)el + 1);
    size_t wl = ref_b64_encode(in, (size_t)n, want);
    WF_CHECK_EQ(hs_base64_encode(in, n, o), el);
    WF_CHECK_EQ(wl, el);
    WF_CHECK_MEM(o, want, (size_t)el);
    free(want);
}

/* Decode `in` (n chars) with the kernel into an output flush against a
 * guard page of exactly hs_base64_decoded_max_length(n) bytes prefilled
 * with a sentinel, and compare result and bytes with the reference. Bytes
 * past the decoded length must still hold the sentinel. Returns the
 * kernel's result. */
static int check_decode(const uint8_t *in, int n)
{
    int ml = hs_base64_decoded_max_length(n);
    WF_CHECK_EQ(ml, n < 0 ? 0 : (n / 4) * 3);
    uint8_t *o = flush_out((size_t)ml);
    memset(o, 0xA5, (size_t)ml);
    uint8_t *want = malloc((size_t)ml + 1);
    long wr = ref_b64_decode(in, n, want);
    int got = hs_base64_decode(in, n, o);
    WF_CHECK_EQ(got, wr);
    if (got >= 0 && got == wr) {
        WF_CHECK(got <= ml);
        WF_CHECK_MEM(o, want, (size_t)got);
        for (int k = got; k < ml; k++) WF_CHECK_EQ(o[k], 0xA5);
    }
    free(want);
    return got;
}

/* check_decode on a copy flush against the input guard page. */
static int check_decode_flush(const uint8_t *text, int n)
{
    return check_decode(flush_in(text, (size_t)n), n);
}

/* RFC 4648 sec 10 test vectors plus a few well-known strings. */
static void rfc4648_vectors(void)
{
    static const struct { const char *plain, *enc; } v[] = {
        { "", "" },
        { "f", "Zg==" },
        { "fo", "Zm8=" },
        { "foo", "Zm9v" },
        { "foob", "Zm9vYg==" },
        { "fooba", "Zm9vYmE=" },
        { "foobar", "Zm9vYmFy" },
        /* RFC 6455 sec 1.3 handshake key and accept value. */
        { "the sample nonce", "dGhlIHNhbXBsZSBub25jZQ==" },
        { "Man is distinguished, not only by his reason, but by this singular passion",
          "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlz"
          "IHNpbmd1bGFyIHBhc3Npb24=" },
    };
    for (size_t t = 0; t < sizeof v / sizeof v[0]; t++) {
        size_t pn = strlen(v[t].plain), en = strlen(v[t].enc);
        uint8_t *p = wf_dup(v[t].plain, pn);
        uint8_t *e = wf_dup(v[t].enc, en);
        uint8_t *o = wf_alloc(en);
        WF_CHECK_EQ(hs_base64_encode(p, (int)pn, o), en);
        WF_CHECK_MEM(o, v[t].enc, en);
        wf_free(o, en);
        size_t ml = (size_t)hs_base64_decoded_max_length((int)en);
        uint8_t *d = wf_alloc(ml);
        WF_CHECK_EQ(hs_base64_decode(e, (int)en, d), pn);
        WF_CHECK_MEM(d, v[t].plain, pn);
        wf_free(d, ml);
        wf_free(p, pn);
        wf_free(e, en);
    }
    /* SHA-1 of the RFC 6455 key + GUID, and its accept header value. */
    static const uint8_t sha[20] = {
        0xb3, 0x7a, 0x4f, 0x2c, 0xc0, 0x62, 0x4f, 0x16, 0x90, 0xf6,
        0x46, 0x06, 0xcf, 0x38, 0x59, 0x45, 0xb2, 0xbe, 0xc4, 0xea };
    uint8_t out[28];
    WF_CHECK_EQ(hs_base64_encode(sha, 20, out), 28);
    WF_CHECK_MEM(out, "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=", 28);
    /* Every sextet value once: bytes 00 10 83 10 51 87 ... */
    uint8_t all[48];
    for (int s = 0; s < 64; s += 4) {
        uint32_t w = ((uint32_t)s << 18) | ((uint32_t)(s + 1) << 12)
                   | ((uint32_t)(s + 2) << 6) | (uint32_t)(s + 3);
        all[s / 4 * 3 + 0] = (uint8_t)(w >> 16);
        all[s / 4 * 3 + 1] = (uint8_t)(w >> 8);
        all[s / 4 * 3 + 2] = (uint8_t)w;
    }
    uint8_t eall[64];
    WF_CHECK_EQ(hs_base64_encode(all, 48, eall), 64);
    WF_CHECK_MEM(eall, ref_b64_alpha, 64);
    uint8_t dall[48];
    WF_CHECK_EQ(hs_base64_decode((const uint8_t *)ref_b64_alpha, 64, dall), 48);
    WF_CHECK_MEM(dall, all, 48);
}

static void length_helpers(void)
{
    for (int n = 0; n <= 100000; n++) {
        WF_CHECK_EQ(hs_base64_encoded_length(n), ref_b64_encoded_length(n));
        WF_CHECK_EQ(hs_base64_decoded_max_length(n), (n / 4) * 3);
    }
    static const int neg[] = { -1, -2, -3, -4, INT_MIN, INT_MIN + 1 };
    for (size_t k = 0; k < sizeof neg / sizeof neg[0]; k++) {
        WF_CHECK_EQ(hs_base64_encoded_length(neg[k]), 0);
        WF_CHECK_EQ(hs_base64_decoded_max_length(neg[k]), 0);
    }
    /* Top of the int range: no signed overflow, exact where representable. */
    for (int n = WF_B64_MAX_ENCODE_INPUT - 50; n <= WF_B64_MAX_ENCODE_INPUT; n++) {
        WF_CHECK_EQ(hs_base64_encoded_length(n), ref_b64_encoded_length(n));
        WF_CHECK(hs_base64_encoded_length(n) > 0);
    }
    WF_CHECK_EQ(hs_base64_encoded_length(WF_B64_MAX_ENCODE_INPUT), INT_MAX - 3);
    for (long long n = WF_B64_MAX_ENCODE_INPUT + 1LL; n <= INT_MAX; n += 997) {
        WF_CHECK_EQ(hs_base64_encoded_length((int)n), -1);
    }
    WF_CHECK_EQ(hs_base64_encoded_length(INT_MAX), -1);
    WF_CHECK_EQ(hs_base64_encoded_length(INT_MAX - 1), -1);
    WF_CHECK_EQ(hs_base64_encoded_length(WF_B64_MAX_ENCODE_INPUT + 1), -1);
    WF_CHECK_EQ(hs_base64_decoded_max_length(INT_MAX), (INT_MAX / 4) * 3);
    WF_CHECK_EQ(hs_base64_decoded_max_length(INT_MAX - 3), (INT_MAX / 4) * 3);
}

/* Regression: an input length whose encoding overflows int used to run the
 * encoder (signed overflow of the output index, then writes far past any
 * buffer the caller could size). It must refuse without touching memory. */
static void encode_rejects_oversized(void)
{
    wf_guarded in = wf_guard_tail(1), out = wf_guard_tail(1);
    in.p[0] = 'x';
    out.p[0] = 0x5A;
    WF_CHECK_EQ(hs_base64_encode(in.p, WF_B64_MAX_ENCODE_INPUT + 1, out.p), -1);
    WF_CHECK_EQ(hs_base64_encode(in.p, INT_MAX, out.p), -1);
    WF_CHECK_EQ(out.p[0], 0x5A);
    WF_CHECK_EQ(hs_base64_encode(in.p, -1, out.p), 0);
    WF_CHECK_EQ(hs_base64_encode(in.p, INT_MIN, out.p), 0);
    WF_CHECK_EQ(out.p[0], 0x5A);
    WF_CHECK_EQ(hs_base64_encode(NULL, 0, NULL), 0);
    wf_guard_free(in);
    wf_guard_free(out);
}

/* Every length 0..MAXLEN: input and output each flush against a guard
 * page; then every start alignment 0..15 of input and output with the
 * byte before the start guarded when the alignment is 0. */
static void encode_exhaustive(void)
{
    wf_rng r = wf_rng_from_env();
    wf_guarded src = wf_guard_tail(MAXLEN);
    wf_rng_fill(&r, src.p, MAXLEN);
    for (int n = 0; n <= MAXLEN; n++) {
        const uint8_t *in = src.p + (MAXLEN - n);
        check_encode(in, n);
        int el = hs_base64_encoded_length(n);
        uint8_t *want = malloc((size_t)el + 1);
        ref_b64_encode(in, (size_t)n, want);
        for (int a = 0; a < 16; a++) {
            wf_guarded hi = wf_guard_head((size_t)n + 16);
            wf_guarded ho = wf_guard_head((size_t)el + 16);
            memcpy(hi.p + a, in, (size_t)n);
            memset(ho.p, 0xEE, (size_t)el + 16);
            WF_CHECK_EQ(hs_base64_encode(hi.p + a, n, ho.p + a), el);
            WF_CHECK_MEM(ho.p + a, want, (size_t)el);
            for (int k = 0; k < a; k++) WF_CHECK_EQ(ho.p[k], 0xEE);
            for (int k = a + el; k < el + 16; k++) WF_CHECK_EQ(ho.p[k], 0xEE);
            wf_guard_free(hi);
            wf_guard_free(ho);
        }
        free(want);
    }
    wf_guard_free(src);
}

/* Round trip for every length 0..MAXLEN with encoded text and decoded
 * output each flush against a guard page, then every alignment. */
static void decode_roundtrip_exhaustive(void)
{
    wf_rng r = wf_rng_from_env();
    uint8_t plain[MAXLEN];
    wf_rng_fill(&r, plain, MAXLEN);
    for (int n = 0; n <= MAXLEN; n++) {
        int el = hs_base64_encoded_length(n);
        wf_guarded enc = wf_guard_tail((size_t)el);
        ref_b64_encode(plain, (size_t)n, enc.p);
        WF_CHECK_EQ(check_decode(enc.p, el), n);
        int ml = hs_base64_decoded_max_length(el);
        for (int a = 0; a < 16; a++) {
            wf_guarded hi = wf_guard_head((size_t)el + 16);
            wf_guarded ho = wf_guard_head((size_t)ml + 16);
            memcpy(hi.p + a, enc.p, (size_t)el);
            memset(ho.p, 0xEE, (size_t)ml + 16);
            WF_CHECK_EQ(hs_base64_decode(hi.p + a, el, ho.p + a), n);
            WF_CHECK_MEM(ho.p + a, plain, (size_t)n);
            for (int k = 0; k < a; k++) WF_CHECK_EQ(ho.p[k], 0xEE);
            for (int k = a + n; k < ml + 16; k++) WF_CHECK_EQ(ho.p[k], 0xEE);
            wf_guard_free(hi);
            wf_guard_free(ho);
        }
        wf_guard_free(enc);
    }
}

/* The end of input and output at every distance 0..63 from the guard
 * page, with the gap filled by bytes that would change the result if
 * read (random bytes after plaintext, valid base64 after text) and
 * checked for stray writes after the output. */
static void end_offset_sweep(void)
{
    wf_rng r = wf_rng_from_env();
    uint8_t plain[MAXLEN + 64], enc[(MAXLEN + 2) / 3 * 4 + 64];
    for (int n = 0; n <= MAXLEN; n++) {
        int el = hs_base64_encoded_length(n);
        int ml = hs_base64_decoded_max_length(el);
        for (int gap = 0; gap < 64; gap++) {
            wf_rng_fill(&r, plain, (size_t)n + (size_t)gap);
            uint8_t *in = flush_in(plain, (size_t)n + (size_t)gap);
            uint8_t *o = flush_out((size_t)el + (size_t)gap);
            memset(o, 0xEE, (size_t)el + (size_t)gap);
            size_t wl = ref_b64_encode(in, (size_t)n, enc);
            WF_CHECK_EQ(hs_base64_encode(in, n, o), el);
            WF_CHECK_EQ(wl, el);
            WF_CHECK_MEM(o, enc, (size_t)el);
            for (int k = el; k < el + gap; k++) WF_CHECK_EQ(o[k], 0xEE);

            memset(enc + el, 'A', (size_t)gap);
            uint8_t *t = flush_in(enc, (size_t)el + (size_t)gap);
            uint8_t *d = flush_out((size_t)ml + (size_t)gap);
            memset(d, 0xEE, (size_t)ml + (size_t)gap);
            WF_CHECK_EQ(hs_base64_decode(t, el, d), n);
            WF_CHECK_MEM(d, plain, (size_t)n);
            for (int k = n; k < ml + gap; k++) WF_CHECK_EQ(d[k], 0xEE);
        }
    }
}

/* Every byte value at every position of valid encodings whose lengths
 * cover the tail-only, scalar-middle and SIMD-window paths. */
static void decode_single_substitutions(void)
{
    wf_rng r = wf_rng_from_env();
    for (int n = 0; n <= 60; n++) {
        uint8_t plain[64], enc[84];
        wf_rng_fill(&r, plain, (size_t)n);
        int el = (int)ref_b64_encode(plain, (size_t)n, enc);
        for (int pos = 0; pos < el; pos++) {
            uint8_t keep = enc[pos];
            for (int v = 0; v < 256; v++) {
                enc[pos] = (uint8_t)v;
                check_decode_flush(enc, el);
            }
            enc[pos] = keep;
        }
    }
}

/* Two substitutions drawn from the bytes the decoder branches on. */
static void decode_pair_substitutions(void)
{
    static const uint8_t special[] = { '=', 'A', '/', '+', '-', '_', ' ', '\n',
                                       '\r', '\t', 0x00, 0x7F, 0x80, 0xFF, '.', 'z' };
    uint8_t enc[40];
    for (int el = 4; el <= 40; el += 4) {
        for (int i = 0; i < el; i++) enc[i] = (uint8_t)ref_b64_alpha[(i * 7) % 64];
        for (int p = 0; p < el; p++) {
            for (int q = p; q < el; q++) {
                for (size_t a = 0; a < sizeof special; a++) {
                    for (size_t b = 0; b < sizeof special; b++) {
                        uint8_t kp = enc[p], kq = enc[q];
                        enc[p] = special[a];
                        enc[q] = special[b];
                        check_decode_flush(enc, el);
                        enc[p] = kp;
                        enc[q] = kq;
                    }
                }
            }
        }
    }
}

static int decode_str(const char *s)
{
    size_t n = strlen(s);
    uint8_t *e = wf_dup(s, n);
    int got = check_decode(e, (int)n);
    wf_free(e, n);
    return got;
}

static void decode_padding_and_malformed(void)
{
    /* Padding forms. */
    WF_CHECK_EQ(decode_str("===="), -1);
    WF_CHECK_EQ(decode_str("A==="), -1);
    WF_CHECK_EQ(decode_str("AA=="), 1);
    WF_CHECK_EQ(decode_str("AAA="), 2);
    WF_CHECK_EQ(decode_str("AA=A"), -1);
    WF_CHECK_EQ(decode_str("A=AA"), -1);
    WF_CHECK_EQ(decode_str("=AAA"), -1);
    WF_CHECK_EQ(decode_str("AA==AAAA"), -1);
    WF_CHECK_EQ(decode_str("AAA=AAAA"), -1);
    WF_CHECK_EQ(decode_str("AAAA===="), -1);
    WF_CHECK_EQ(decode_str("AAAAAA=="), 4);
    WF_CHECK_EQ(decode_str("Zm9vYmFyZm9vYmFyZm9vYmFyZm9vY==="), -1);
    WF_CHECK_EQ(decode_str("Zm9vYmFyZm9vYmFyZm9vYmFyZm9vYg=="), 22);
    /* Non-canonical pad bits are accepted (documented in api.h). */
    WF_CHECK_EQ(decode_str("QR=="), 1);
    WF_CHECK_EQ(decode_str("QUJ="), 2);
    /* Truncation: not a multiple of four. */
    WF_CHECK_EQ(decode_str("Z"), -1);
    WF_CHECK_EQ(decode_str("Zm"), -1);
    WF_CHECK_EQ(decode_str("Zm9"), -1);
    WF_CHECK_EQ(decode_str("Zm9vY"), -1);
    WF_CHECK_EQ(decode_str("Zm9vYg="), -1);
    WF_CHECK_EQ(decode_str("Zm9vYg"), -1);
    /* Whitespace and line breaks are not tolerated anywhere. */
    WF_CHECK_EQ(decode_str("Zm9v\nYmFy"), -1);
    WF_CHECK_EQ(decode_str("Zm9vYmFy\r\n"), -1);
    WF_CHECK_EQ(decode_str(" Zm9vYmF"), -1);
    WF_CHECK_EQ(decode_str("Zm9vYmFyZm9vYmFyZm9v\tmFyZm9vYmFy"), -1);
    WF_CHECK_EQ(decode_str("Zm9vYmFyZm9vYmFyZm9vYmFyZm9vYmFy Zm9vYmFyZm9vYmF"), -1);
    /* URL-safe alphabet is not accepted. */
    WF_CHECK_EQ(decode_str("-_-_"), -1);
    WF_CHECK_EQ(decode_str("+/+/"), 3);
    /* Empty and negative lengths, without touching memory. */
    WF_CHECK_EQ(hs_base64_decode(NULL, 0, NULL), 0);
    WF_CHECK_EQ(hs_base64_decode(NULL, -4, NULL), -1);
    WF_CHECK_EQ(hs_base64_decode(NULL, INT_MIN, NULL), -1);
    WF_CHECK_EQ(hs_base64_decode(NULL, 5, NULL), -1);
    WF_CHECK_EQ(hs_base64_decode(NULL, INT_MAX, NULL), -1);
}

/* A high-bit byte anywhere in an otherwise valid text of several SIMD
 * windows is rejected, in every window position. */
static void decode_high_bit_every_position(void)
{
    enum { N = 132 };
    uint8_t enc[N];
    for (int i = 0; i < N; i++) enc[i] = (uint8_t)ref_b64_alpha[(i * 13 + 5) % 64];
    static const uint8_t hi[] = { 0x80, 0x81, 0xC3, 0xFE, 0xFF, 0xAB };
    for (int pos = 0; pos < N; pos++) {
        for (size_t h = 0; h < sizeof hi; h++) {
            uint8_t keep = enc[pos];
            enc[pos] = hi[h];
            WF_CHECK_EQ(check_decode_flush(enc, N), -1);
            enc[pos] = keep;
        }
    }
}

/* All-0x00, all-0xFF and 0x80-pattern inputs, both directions. */
static void adversarial_fill_patterns(void)
{
    static const uint8_t fills[] = { 0x00, 0xFF, 0x80, 0x7F, '=', 'A', '/' };
    for (size_t f = 0; f < sizeof fills; f++) {
        for (int n = 0; n <= 100; n++) {
            uint8_t *b = wf_alloc((size_t)n);
            memset(b, fills[f], (size_t)n);
            check_encode(b, n);
            check_decode(b, n);
            wf_free(b, (size_t)n);
        }
    }
}

/* Random differential: encode vs reference, decode(encode) round trip, and
 * decode of mutated text vs the reference decoder. */
static void random_differential(void)
{
    wf_rng r = wf_rng_from_env();
    long iters = wf_iters(4000);
    for (long it = 0; it < iters; it++) {
        int n = (int)wf_rng_below(&r, it % 16 == 0 ? 5000 : 400);
        uint8_t *plain = malloc((size_t)n + 1);
        wf_rng_fill(&r, plain, (size_t)n);
        uint8_t *p = wf_dup(plain, (size_t)n);
        check_encode(p, n);
        int el = hs_base64_encoded_length(n);
        uint8_t *enc = malloc((size_t)el + 1);
        hs_base64_encode(p, n, enc);
        uint8_t *e = wf_dup(enc, (size_t)el);
        WF_CHECK_EQ(check_decode(e, el), n);
        wf_free(e, (size_t)el);
        /* Mutate a few characters, sometimes truncate. */
        int muts = (int)wf_rng_below(&r, 4);
        for (int m = 0; m < muts && el > 0; m++) {
            static const uint8_t pool[] = { '=', '+', '/', '-', '_', ' ', '\n', 0, 0x80, 0xFF, 'Q' };
            size_t at = (size_t)wf_rng_below(&r, (uint64_t)el);
            enc[at] = wf_rng_below(&r, 2) ? pool[wf_rng_below(&r, sizeof pool)]
                                          : (uint8_t)wf_rng_next(&r);
        }
        int cut = wf_rng_below(&r, 8) == 0 ? (int)wf_rng_below(&r, (uint64_t)el + 1) : el;
        e = wf_dup(enc, (size_t)cut);
        check_decode(e, cut);
        wf_free(e, (size_t)cut);
        free(enc);
        wf_free(p, (size_t)n);
        free(plain);
    }
}

/* Decoding random alphabet text (any 4k-length string over the alphabet is
 * valid) and re-encoding gives back the text up to the pad bits. */
static void random_alphabet_text(void)
{
    wf_rng r = wf_rng_from_env();
    long iters = wf_iters(4000);
    for (long it = 0; it < iters; it++) {
        int q = (int)wf_rng_below(&r, 100);
        int n = q * 4;
        uint8_t *t = malloc((size_t)n + 1);
        wf_rng_fill_from(&r, t, (size_t)n, (const uint8_t *)ref_b64_alpha, 64);
        if (n && wf_rng_below(&r, 3) == 0) t[n - 1] = '=';
        if (n && t[n - 1] == '=' && wf_rng_below(&r, 2) == 0) t[n - 2] = '=';
        uint8_t *e = wf_dup(t, (size_t)n);
        int d = check_decode(e, n);
        WF_CHECK(d >= 0);
        if (d >= 0) {
            uint8_t *dec = malloc((size_t)d + 1);
            hs_base64_decode(e, n, dec);
            uint8_t *re = malloc((size_t)n + 1);
            WF_CHECK_EQ(hs_base64_encode(dec, d, re), n);
            /* Identical except the last sextet before padding (pad bits). */
            int pad = n == 0 ? 0 : (t[n - 1] == '=') + (n > 1 && t[n - 2] == '=');
            int cmp = n - pad - (pad ? 1 : 0);
            if (cmp > 0) WF_CHECK_MEM(re, t, (size_t)cmp);
            for (int k = n - pad; k < n; k++) WF_CHECK_EQ(re[k], '=');
            free(re);
            free(dec);
        }
        wf_free(e, (size_t)n);
        free(t);
    }
}

/* A long input crosses many SIMD iterations in both directions. */
static void large_roundtrip(void)
{
    wf_rng r = wf_rng_from_env();
    for (int n = (1 << 16) - 3; n <= (1 << 16) + 3; n++) {
        uint8_t *plain = malloc((size_t)n);
        wf_rng_fill(&r, plain, (size_t)n);
        uint8_t *p = wf_dup(plain, (size_t)n);
        check_encode(p, n);
        int el = hs_base64_encoded_length(n);
        uint8_t *enc = wf_alloc((size_t)el);
        WF_CHECK_EQ(hs_base64_encode(p, n, enc), el);
        WF_CHECK_EQ(check_decode(enc, el), n);
        wf_free(enc, (size_t)el);
        wf_free(p, (size_t)n);
        free(plain);
    }
}

int main(void)
{
    WF_RUN(rfc4648_vectors);
    WF_RUN(length_helpers);
    WF_RUN(encode_rejects_oversized);
    WF_RUN(encode_exhaustive);
    WF_RUN(decode_roundtrip_exhaustive);
    WF_RUN(end_offset_sweep);
    WF_RUN(decode_single_substitutions);
    WF_RUN(decode_pair_substitutions);
    WF_RUN(decode_padding_and_malformed);
    WF_RUN(decode_high_bit_every_position);
    WF_RUN(adversarial_fill_patterns);
    WF_RUN(random_differential);
    WF_RUN(random_alphabet_text);
    WF_RUN(large_roundtrip);
    return wf_finish("test_base64");
}
