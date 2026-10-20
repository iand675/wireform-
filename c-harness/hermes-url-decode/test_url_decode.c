/* Unit, boundary, differential and round-trip tests for
 * hermes/cbits/url_decode.c. */
#include "harness.h"
#include "api.h"

/* decodeWith from Network/HTTP/URL/Decode.hs, in C: scan, copy the clean
 * prefix, decode the rest into a fresh srclen-byte buffer. Returns the
 * decoded length or the negative error; *out gets the exact-size result. */
static ptrdiff_t haskell_decode(const uint8_t *src, size_t n, int mode, uint8_t **out)
{
    *out = NULL;
    if (n == 0) return 0;
    size_t first = hermes_url_scan_special(src, n, mode);
    if (first == n) return (ptrdiff_t)n;
    uint8_t *dst = wf_alloc(n);
    memcpy(dst, src, first);
    ptrdiff_t r = hermes_url_decode(src + first, n - first, dst + first, mode);
    if (r >= 0) {
        r += (ptrdiff_t)first;
        *out = wf_dup(dst, (size_t)r);
    }
    wf_free(dst, n);
    return r;
}

/* Checks every way of calling the kernel on (src, n) against the
 * reference: out of place into an n-byte buffer, out of place into a
 * buffer of exactly the decoded length (so writing one byte past the
 * result faults), in place, and through the Haskell wrapper's split. */
static void check_decode(const uint8_t *src, size_t n, int mode)
{
    int plus = mode != 0;
    uint8_t *s = wf_dup(src, n);
    uint8_t *want = malloc(n + 1);
    ptrdiff_t wn = ref_url_decode(s, n, want, plus);

    WF_CHECK_EQ(hermes_url_scan_special(s, n, mode), ref_url_scan_special(s, n, plus));

    uint8_t *d = wf_alloc(n);
    ptrdiff_t got = hermes_url_decode(s, n, d, mode);
    if (got != wn) wf_hexdump("src", s, n);
    WF_CHECK_EQ(got, wn);
    if (wn >= 0 && got == wn) WF_CHECK_MEM(d, want, (size_t)wn);
    wf_free(d, n);

    if (wn >= 0) {
        uint8_t *tight = wf_alloc((size_t)wn);
        WF_CHECK_EQ(hermes_url_decode(s, n, tight, mode), wn);
        WF_CHECK_MEM(tight, want, (size_t)wn);
        wf_free(tight, (size_t)wn);
    }

    uint8_t *inplace = wf_dup(src, n);
    got = hermes_url_decode(inplace, n, inplace, mode);
    WF_CHECK_EQ(got, wn);
    if (wn >= 0 && got == wn) WF_CHECK_MEM(inplace, want, (size_t)wn);
    wf_free(inplace, n);

    uint8_t *hs;
    got = haskell_decode(s, n, mode, &hs);
    WF_CHECK_EQ(got, wn);
    if (wn >= 0 && got == wn) {
        WF_CHECK_MEM(hs ? hs : s, want, (size_t)wn);
    }
    if (hs) wf_free(hs, (size_t)got);

    free(want);
    wf_free(s, n);
}

static void check_str(const char *str, int mode)
{
    check_decode((const uint8_t *)str, strlen(str), mode);
}

static void expect(const char *in, int mode, ptrdiff_t code, const char *out)
{
    size_t n = strlen(in);
    uint8_t *s = wf_dup(in, n);
    uint8_t *d = wf_alloc(n);
    ptrdiff_t r = hermes_url_decode(s, n, d, mode);
    if (r != code) fprintf(stderr, "    input \"%s\" mode %d\n", in, mode);
    WF_CHECK_EQ(r, code);
    if (code >= 0 && r == code) WF_CHECK_MEM(d, out, (size_t)code);
    wf_free(d, n);
    wf_free(s, n);
    check_str(in, mode);
}

/* RFC 3986 section 2.1 / 2.4 and HTML form-urlencoded vectors. */
static void decode_vectors(void)
{
    expect("", 0, 0, "");
    expect("abc", 0, 3, "abc");
    expect("%41%42%43", 0, 3, "ABC");
    expect("a%20b", 0, 3, "a b");
    expect("%e2%82%ac", 0, 3, "\xe2\x82\xac");
    expect("%E2%82%AC", 1, 3, "\xe2\x82\xac");
    expect("%aA%Aa%fF", 0, 3, "\xaa\xaa\xff");
    expect("%00%7f%80%ff", 0, 4, "\x00\x7f\x80\xff");
    expect("%25", 0, 1, "%");
    expect("%2525", 0, 3, "%25");
    expect("%2B", 1, 1, "+");
    expect("a+b", 0, 3, "a+b");
    expect("a+b", 1, 3, "a b");
    expect("+++", 1, 3, "   ");
    expect("name=J%C3%BCrgen+M%C3%BCller&x=1", 1, 24, "name=J\xc3\xbcrgen M\xc3\xbcller&x=1");
    expect("/over/there%3Fname%3Dferret%23nose", 0, 28, "/over/there?name=ferret#nose");
    /* Truncated escapes (-1). */
    expect("%", 0, -1, NULL);
    expect("%", 1, -1, NULL);
    expect("%4", 0, -1, NULL);
    expect("abc%", 0, -1, NULL);
    expect("abc%f", 1, -1, NULL);
    expect("%41%", 0, -1, NULL);
    expect("%41%4", 0, -1, NULL);
    /* Invalid hex digits (-2), including at the very end. */
    expect("%zz", 0, -2, NULL);
    expect("%z", 0, -2, NULL);
    expect("%4z", 0, -2, NULL);
    expect("%z4", 0, -2, NULL);
    expect("%G0", 0, -2, NULL);
    expect("%0G", 0, -2, NULL);
    expect("%%", 0, -2, NULL);
    expect("%%41", 0, -2, NULL);
    expect("%+1", 1, -2, NULL);
    expect("% 1", 0, -2, NULL);
    expect("abc%g", 0, -2, NULL);
    expect("%41%:", 0, -2, NULL);
    /* The first error wins. */
    expect("%zz%", 0, -2, NULL);
    expect("%4", 1, -1, NULL);
    expect("%/0", 0, -2, NULL);
    expect("%@0", 0, -2, NULL);
    expect("%`0", 0, -2, NULL);
}

/* Every two-byte escape body, so every byte value in each hex position
 * (the '/'..':', '@'..'G', '`'..'g' edges of the digit ranges), and
 * every escape at the end of the buffer with 0, 1 or 2 bytes after '%'. */
static void decode_all_escapes(void)
{
    for (int a = 0; a < 256; a++) {
        for (int b = 0; b < 256; b++) {
            uint8_t s[3] = { '%', (uint8_t)a, (uint8_t)b };
            check_decode(s, 3, 0);
            if (a == '+' || b == '+') check_decode(s, 3, 1);
        }
        uint8_t t[2] = { '%', (uint8_t)a };
        check_decode(t, 2, 0);
        check_decode(t, 2, 1);
        uint8_t u[5] = { 'x', 'y', '%', (uint8_t)a, '0' };
        check_decode(u, 5, 0);
    }
    check_decode((const uint8_t *)"%", 1, 0);
}

/* (NULL, 0) as firstSpecialOffset passes for BS.empty, and decode of an
 * empty range: return 0 without touching memory. */
static void empty_inputs(void)
{
    WF_CHECK_EQ(hermes_url_scan_special(NULL, 0, 0), 0);
    WF_CHECK_EQ(hermes_url_scan_special(NULL, 0, 1), 0);
    WF_CHECK_EQ(hermes_url_decode(NULL, 0, NULL, 0), 0);
    WF_CHECK_EQ(hermes_url_decode(NULL, 0, NULL, 1), 0);
    wf_guarded g = wf_guard_tail(0);
    WF_CHECK_EQ(hermes_url_scan_special(g.p, 0, 1), 0);
    WF_CHECK_EQ(hermes_url_decode(g.p, 0, g.p, 1), 0);
    wf_guard_free(g);
}

/* scan_special for every length across the 16/32-byte strides and the
 * scalar tail, every start alignment (skew from the guard page) and every
 * hit position, with '%' and '+' in both modes; plus_is_space is any
 * nonzero int. Bytes past the end hold '%' / '+' so an overread gives a
 * wrong answer even where it would not fault. */
static void scan_exhaustive(void)
{
    enum { MAXN = 100, MAXSKEW = 32 };
    static const uint8_t fillers[] = { 'a', 0x00, 0x80, 0xff, '*', '&' };
    static const int modes[] = { 0, 1, 2, -1 };
    wf_guarded g = wf_guard_tail(MAXN + MAXSKEW);
    for (int skew = 0; skew < MAXSKEW; skew++) {
        for (int len = 0; len <= MAXN; len++) {
            uint8_t *b = g.p + g.n - (size_t)skew - (size_t)len;
            for (int k = 0; k < skew; k++) b[len + k] = (k & 1) ? '+' : '%';
            for (size_t fi = 0; fi < sizeof fillers; fi++) {
                memset(b, fillers[fi], (size_t)len);
                for (size_t mi = 0; mi < 4; mi++) {
                    WF_CHECK_EQ(hermes_url_scan_special(b, (size_t)len, modes[mi]), (size_t)len);
                }
                for (int pos = 0; pos < len; pos++) {
                    for (int which = 0; which < 2; which++) {
                        b[pos] = which ? '+' : '%';
                        if (pos + 1 < len) b[len - 1] = '%'; /* a later hit must not win */
                        for (size_t mi = 0; mi < 4; mi++) {
                            int plus = modes[mi] != 0;
                            WF_CHECK_EQ(hermes_url_scan_special(b, (size_t)len, modes[mi]),
                                        ref_url_scan_special(b, (size_t)len, plus));
                        }
                        b[pos] = fillers[fi];
                        b[len - 1] = fillers[fi];
                    }
                }
            }
        }
    }
    wf_guard_free(g);
}

/* Decode boundary sweep: one escape (complete, truncated, or invalid) or
 * '+' at every position of buffers of every length, flush against a
 * guard page. */
static void decode_sweep(void)
{
    enum { MAXN = 80 };
    static const char *const pieces[] = { "%41", "%4", "%", "%zz", "%z", "%4z", "+", "%2B", "%25" };
    uint8_t buf[MAXN + 4];
    for (int len = 0; len <= MAXN; len++) {
        memset(buf, 'q', (size_t)len);
        check_decode(buf, (size_t)len, 0);
        for (size_t pi = 0; pi < sizeof pieces / sizeof pieces[0]; pi++) {
            size_t pl = strlen(pieces[pi]);
            for (int pos = 0; pos + (int)pl <= len; pos++) {
                memcpy(buf + pos, pieces[pi], pl);
                check_decode(buf, (size_t)len, 0);
                check_decode(buf, (size_t)len, 1);
                memset(buf + pos, 'q', pl);
            }
        }
        /* Dense: every byte an escape or a '+'. */
        for (int k = 0; k < len; k++) buf[k] = "%7e+"[k % 4];
        check_decode(buf, (size_t)len, 0);
        check_decode(buf, (size_t)len, 1);
    }
}

static void decode_random(void)
{
    wf_rng r = wf_rng_from_env();
    static const uint8_t alpha[] = { '%', '%', '+', '0', '9', 'a', 'f', 'A', 'F', 'g', 'G', 'z', ' ',
                                     '/', ':', '@', '`', 0x00, 0x80, 0xff };
    static const uint8_t hex[] = "0123456789abcdefABCDEF";
    for (long it = 0; it < wf_iters(20000); it++) {
        size_t n = (size_t)wf_rng_below(&r, 300);
        uint8_t *tmp = malloc(n + 1);
        int style = (int)wf_rng_below(&r, 3);
        for (size_t i = 0; i < n; i++) {
            if (style == 0) {
                tmp[i] = alpha[wf_rng_below(&r, sizeof alpha)];
            } else {
                /* Mostly valid escapes so decoding runs long before an
                 * error, if any. */
                uint64_t v = wf_rng_below(&r, 100);
                if (v < 10 && i + 2 < n) {
                    tmp[i++] = '%';
                    tmp[i++] = hex[wf_rng_below(&r, sizeof hex - 1)];
                    tmp[i] = hex[wf_rng_below(&r, sizeof hex - 1)];
                } else if (v < 15) {
                    tmp[i] = '+';
                } else if (v < 16 && style == 2) {
                    tmp[i] = '%';
                } else {
                    tmp[i] = (uint8_t)('a' + v % 26);
                }
            }
        }
        check_decode(tmp, n, (int)wf_rng_below(&r, 2));
        free(tmp);
    }
}

/* encode(bytes) |> decode == bytes, for arbitrary bytes, with random
 * choices of which bytes are escaped and in which hex case. */
static void roundtrip_random(void)
{
    wf_rng r = wf_rng_from_env();
    for (long it = 0; it < wf_iters(5000); it++) {
        size_t n = (size_t)wf_rng_below(&r, 200);
        uint8_t *raw = malloc(n + 1), *choice = malloc(n + 1), *enc = malloc(3 * n + 1);
        wf_rng_fill(&r, raw, n);
        wf_rng_fill(&r, choice, n);
        for (int plus = 0; plus < 2; plus++) {
            size_t en = ref_url_encode(raw, n, enc, plus, choice, n);
            uint8_t *s = wf_dup(enc, en);
            uint8_t *d = wf_alloc(n);
            WF_CHECK_EQ(hermes_url_decode(s, en, d, plus), (ptrdiff_t)n);
            WF_CHECK_MEM(d, raw, n);
            wf_free(d, n);
            WF_CHECK_EQ(hermes_url_decode(s, en, s, plus), (ptrdiff_t)n);
            WF_CHECK_MEM(s, raw, n);
            wf_free(s, en);
        }
        free(raw);
        free(choice);
        free(enc);
    }
}

/* Long clean runs: the scan must skip whole strides and find a hit in the
 * last stride or the scalar tail of a large buffer. */
static void long_runs(void)
{
    enum { N = 4096 + 37 };
    uint8_t *src = malloc(N);
    memset(src, 'k', N);
    for (int pos = N - 70; pos < N; pos++) {
        src[pos] = '%';
        uint8_t *b = wf_dup(src, N);
        WF_CHECK_EQ(hermes_url_scan_special(b, N, 0), (size_t)pos);
        WF_CHECK_EQ(hermes_url_scan_special(b + 1, N - 1, 1), (size_t)pos - 1);
        wf_free(b, N);
        src[pos] = 'k';
    }
    free(src);
}

int main(void)
{
    (void)wf_rng_from_env();
    WF_RUN(empty_inputs);
    WF_RUN(decode_vectors);
    WF_RUN(decode_all_escapes);
    WF_RUN(scan_exhaustive);
    WF_RUN(decode_sweep);
    WF_RUN(decode_random);
    WF_RUN(roundtrip_random);
    WF_RUN(long_runs);
    return wf_finish("hermes-url-decode");
}
