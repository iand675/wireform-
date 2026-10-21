/* UTF-8 validation, byte scanners, Iceberg bounds and Arrow buffer
 * validation tests for wireform-core fast_decode.c. */
#include "harness.h"
#include "api.h"
#include "buffers.h"

#include <limits.h>

/* ------------------------------------------------------------------ */
/* UTF-8                                                               */
/* ------------------------------------------------------------------ */

/* Validate s both at the start of an exact-size buffer and after an
 * ASCII prefix that pushes it past the SIMD and SWAR fast paths. */
static void utf8_expect(const uint8_t *s, int n, int want)
{
    static const int prefixes[] = { 0, 1, 7, 8, 15, 16, 17, 31, 32 };
    for (size_t p = 0; p < sizeof prefixes / sizeof prefixes[0]; p++) {
        int pre = prefixes[p];
        uint8_t *b = wf_alloc((size_t)(pre + n));
        memset(b, 'a', (size_t)pre);
        if (n) memcpy(b + pre, s, (size_t)n);
        WF_CHECK_EQ(hs_proto_validate_utf8_fast(b, pre + n), want);
        wf_free(b, (size_t)(pre + n));
    }
}

#define U(...) ((const uint8_t[]){ __VA_ARGS__ }), (int)sizeof((const uint8_t[]){ __VA_ARGS__ })

/* Markus Kuhn's UTF-8 decoder stress test and RFC 3629 section 10. */
static void utf8_vectors(void)
{
    utf8_expect(NULL, 0, 1);
    WF_CHECK_EQ(hs_proto_validate_utf8_fast(NULL, 0), 1);
    utf8_expect(U('h', 'i'), 1);
    utf8_expect(U(0x00), 1);
    utf8_expect(U(0x7F), 1);
    utf8_expect(U(0xC2, 0x80), 1);                /* U+0080 */
    utf8_expect(U(0xDF, 0xBF), 1);                /* U+07FF */
    utf8_expect(U(0xE0, 0xA0, 0x80), 1);          /* U+0800 */
    utf8_expect(U(0xED, 0x9F, 0xBF), 1);          /* U+D7FF */
    utf8_expect(U(0xEE, 0x80, 0x80), 1);          /* U+E000 */
    utf8_expect(U(0xEF, 0xBF, 0xBD), 1);          /* U+FFFD */
    utf8_expect(U(0xEF, 0xBF, 0xBF), 1);          /* U+FFFF (noncharacter, valid) */
    utf8_expect(U(0xF0, 0x90, 0x80, 0x80), 1);    /* U+10000 */
    utf8_expect(U(0xF4, 0x8F, 0xBF, 0xBF), 1);    /* U+10FFFF */
    utf8_expect(U(0x41, 0xE2, 0x89, 0xA2, 0xCE, 0x91, 0x2E), 1); /* RFC 3629 */
    utf8_expect(U(0xED, 0x95, 0x9C, 0xEA, 0xB5, 0xAD, 0xEC, 0x96, 0xB4), 1);
    utf8_expect(U(0xEF, 0xBB, 0xBF, 0xF0, 0xA3, 0x8E, 0xB4), 1);
    /* overlongs */
    utf8_expect(U(0xC0, 0x80), 0);
    utf8_expect(U(0xC0, 0xAF), 0);
    utf8_expect(U(0xC1, 0xBF), 0);
    utf8_expect(U(0xE0, 0x80, 0xAF), 0);
    utf8_expect(U(0xE0, 0x9F, 0xBF), 0);
    utf8_expect(U(0xF0, 0x80, 0x80, 0xAF), 0);
    utf8_expect(U(0xF0, 0x8F, 0xBF, 0xBF), 0);
    utf8_expect(U(0xF8, 0x88, 0x80, 0x80, 0x80), 0);
    utf8_expect(U(0xFC, 0x84, 0x80, 0x80, 0x80, 0x80), 0);
    /* surrogates, paired and unpaired */
    utf8_expect(U(0xED, 0xA0, 0x80), 0);
    utf8_expect(U(0xED, 0xBF, 0xBF), 0);
    utf8_expect(U(0xED, 0xA0, 0x80, 0xED, 0xB0, 0x80), 0);
    /* above U+10FFFF */
    utf8_expect(U(0xF4, 0x90, 0x80, 0x80), 0);
    utf8_expect(U(0xF5, 0x80, 0x80, 0x80), 0);
    utf8_expect(U(0xF7, 0xBF, 0xBF, 0xBF), 0);
    /* stray continuation, impossible bytes, truncation */
    utf8_expect(U(0x80), 0);
    utf8_expect(U(0xBF), 0);
    utf8_expect(U(0xFE), 0);
    utf8_expect(U(0xFF), 0);
    utf8_expect(U(0xC2), 0);
    utf8_expect(U(0xE2, 0x82), 0);
    utf8_expect(U(0xF0, 0x9F, 0x98), 0);
    utf8_expect(U(0xC2, 0x41), 0);
    utf8_expect(U(0xE2, 0x41, 0x82), 0);
    utf8_expect(U(0xF0, 0x9F, 0x98, 0x41), 0);
    utf8_expect(U(0xC2, 0xC2, 0x80), 0);
}

/* Every 1-, 2- and 3-byte sequence (16.8M) against the table reference,
 * at the very end of a guard-paged buffer so a missing length check on a
 * truncated lead byte faults. */
static void utf8_exhaustive_short(void)
{
    wf_guarded g = wf_guard_tail(3);
    for (uint32_t x = 0; x < (1u << 24); x++) {
        g.p[0] = (uint8_t)(x >> 16);
        g.p[1] = (uint8_t)(x >> 8);
        g.p[2] = (uint8_t)x;
        if (x < 256) {
            WF_CHECK_EQ(hs_proto_validate_utf8_fast(g.p + 2, 1), ref_utf8_valid(g.p + 2, 1));
        }
        if (x < 65536) {
            WF_CHECK_EQ(hs_proto_validate_utf8_fast(g.p + 1, 2), ref_utf8_valid(g.p + 1, 2));
        }
        int got = hs_proto_validate_utf8_fast(g.p, 3);
        int want = ref_utf8_valid(g.p, 3);
        if (got != want) WF_CHECK_EQ(got, want);
    }
    wf_guard_free(g);
}

/* Every 4-byte lead/second-byte combination with representative
 * third/fourth bytes, and every scalar value encoded. */
static void utf8_four_byte_and_scalars(void)
{
    static const uint8_t tails[] = { 0x00, 0x41, 0x7F, 0x80, 0x8F, 0x90, 0xBF, 0xC0, 0xF4, 0xFF };
    wf_guarded g = wf_guard_tail(4);
    for (int a = 0; a < 256; a++) {
        for (int b = 0; b < 256; b++) {
            for (size_t c = 0; c < sizeof tails; c++) {
                for (size_t d = 0; d < sizeof tails; d++) {
                    g.p[0] = (uint8_t)a;
                    g.p[1] = (uint8_t)b;
                    g.p[2] = tails[c];
                    g.p[3] = tails[d];
                    int got = hs_proto_validate_utf8_fast(g.p, 4);
                    int want = ref_utf8_valid(g.p, 4);
                    if (got != want) WF_CHECK_EQ(got, want);
                }
            }
        }
    }
    for (uint32_t cp = 0; cp < 0x200000; cp++) {
        uint8_t e[4];
        int n = ref_utf8_encode(cp, e);
        memcpy(g.p + 4 - n, e, (size_t)n);
        int want = cp <= 0x10FFFF && !(cp >= 0xD800 && cp <= 0xDFFF);
        int got = hs_proto_validate_utf8_fast(g.p + 4 - n, n);
        if (got != want) WF_CHECK_EQ(got, want);
    }
    wf_guard_free(g);
}

/* A multibyte character (or a bad byte) placed at every position of
 * every length up to a few SIMD blocks, at every start alignment flush
 * against a guard page: the ASCII fast path hands over correctly. */
static void utf8_boundary_sweep(void)
{
    enum { MAXN = 72 };
    static const struct { uint8_t b[4]; int n; int ok; } ins[] = {
        { { 0xC3, 0xA9 }, 2, 1 },
        { { 0xE2, 0x82, 0xAC }, 3, 1 },
        { { 0xF0, 0x9F, 0x98, 0x80 }, 4, 1 },
        { { 0x80 }, 1, 0 },
        { { 0xED, 0xA0, 0x80 }, 3, 0 },
        { { 0xF0, 0x9F, 0x98 }, 3, 0 }, /* truncated if at the end */
        { { 0xFF }, 1, 0 },
    };
    /* gap 0: end flush on the guard page; other gaps: the bytes after len
     * hold 0xFF, so a read past len would turn valid input invalid. */
    static const int gaps[] = { 0, 1, 7, 15 };
    wf_guarded g = wf_guard_tail(MAXN + 16);
    for (size_t gi = 0; gi < sizeof gaps / sizeof gaps[0]; gi++)
    for (size_t k = 0; k < sizeof ins / sizeof ins[0]; k++) {
        for (int len = 0; len <= MAXN; len++) {
            uint8_t *b = g.p + (g.n - (size_t)len - (size_t)gaps[gi]);
            memset(b + len, 0xFF, (size_t)gaps[gi]);
            memset(b, 'x', (size_t)len);
            WF_CHECK_EQ(hs_proto_validate_utf8_fast(b, len), 1);
            for (int pos = 0; pos + ins[k].n <= len; pos++) {
                memset(b, 'x', (size_t)len);
                memcpy(b + pos, ins[k].b, (size_t)ins[k].n);
                int want = ref_utf8_valid(b, len);
                WF_CHECK_EQ(hs_proto_validate_utf8_fast(b, len), want);
                if (k == 5) WF_CHECK_EQ(want, 0);
                else WF_CHECK_EQ(want, ins[k].ok);
            }
        }
    }
    wf_guard_free(g);
}

/* Random mixed strings: valid UTF-8 built from random scalars, then one
 * random byte flip in half of them. */
static void utf8_random(void)
{
    wf_rng r = wf_rng_from_env();
    for (long it = 0; it < wf_iters(30000); it++) {
        uint8_t tmp[256 + 4];
        int n = 0, target = (int)wf_rng_below(&r, 256);
        int ascii_bias = (int)wf_rng_below(&r, 4);
        while (n < target) {
            uint32_t cp;
            uint64_t sel = wf_rng_below(&r, 8);
            if (sel < (uint64_t)ascii_bias + 2) cp = (uint32_t)wf_rng_below(&r, 0x80);
            else if (sel < 6) cp = (uint32_t)wf_rng_below(&r, 0x10000);
            else cp = 0x10000 + (uint32_t)wf_rng_below(&r, 0x100000);
            if (cp >= 0xD800 && cp <= 0xDFFF) cp = 0xFFFD;
            n += ref_utf8_encode(cp, tmp + n);
        }
        if (wf_rng_below(&r, 2) && n > 0) {
            tmp[wf_rng_below(&r, (uint64_t)n)] = (uint8_t)wf_rng_next(&r);
        }
        if (wf_rng_below(&r, 4) == 0 && n > 0) n -= (int)wf_rng_below(&r, 4) % (n + 1);
        uint8_t *b = wf_dup(tmp, (size_t)n);
        WF_CHECK_EQ(hs_proto_validate_utf8_fast(b, n), ref_utf8_valid(b, n));
        wf_free(b, (size_t)n);
    }
}

/* ------------------------------------------------------------------ */
/* Byte scanners                                                       */
/* ------------------------------------------------------------------ */

/* Exhaustive (end gap, length, offset, special-byte position) sweep for
 * the four offset-taking scanners. With gap 0 the buffer ends flush on a
 * guard page; otherwise the gap after len holds the special byte, so a
 * read past len changes the answer. */
static void scanners_exhaustive(void)
{
    enum { MAXN = 52 };
    /* fill byte, special byte: the special is what each scanner stops
     * on; the fill is innocuous for all of them (or deliberately not). */
    static const uint8_t specials[] = { 0x00, 0x80, 0xFF, '"', '\\', 0x1F, 0x20, 0x7F,
                                        ' ', '\t', '\n', '\r', ',', ';', 'a' };
    static const uint8_t fills[] = { 'a', ' ', 0x80, ';' };
    static const int gaps[] = { 0, 3, 15 };
    wf_guarded g = wf_guard_tail(MAXN + 16);
    for (size_t gi = 0; gi < sizeof gaps / sizeof gaps[0]; gi++)
    for (size_t f = 0; f < sizeof fills; f++) {
        for (size_t s = 0; s < sizeof specials; s++) {
            for (int len = 0; len <= MAXN; len++) {
                uint8_t *b = g.p + (g.n - (size_t)len - (size_t)gaps[gi]);
                memset(b + len, specials[s], (size_t)gaps[gi]);
                for (int pos = -1; pos < len; pos++) {
                    memset(b, fills[f], (size_t)len);
                    if (pos >= 0) b[pos] = specials[s];
                    for (int off = 0; off <= len + 1; off++) {
                        WF_CHECK_EQ(hs_proto_find_nul(b, off, len), ref_find_nul(b, off, len));
                        WF_CHECK_EQ(hs_proto_skip_ws(b, off, len), ref_skip_ws(b, off, len));
                        if (off <= len) {
                            WF_CHECK_EQ(hs_proto_is_ascii(b, off, len - off),
                                        ref_is_ascii(b, off, len - off));
                            WF_CHECK_EQ(hs_proto_find_json_escape(b, off, len - off),
                                        ref_find_json_escape(b, off, len - off));
                        }
                    }
                }
            }
        }
    }
    wf_guard_free(g);
}

/* Each scanner stops on exactly its byte class: all 256 values, at a
 * position in the SIMD block and in the scalar tail. */
static void scanners_byte_classes(void)
{
    for (int v = 0; v < 256; v++) {
        for (int pos = 0; pos < 40; pos += 13) {
            uint8_t tmp[40];
            memset(tmp, 'a', sizeof tmp);
            tmp[pos] = (uint8_t)v;
            uint8_t *b = wf_dup(tmp, sizeof tmp);
            WF_CHECK_EQ(hs_proto_find_nul(b, 0, 40), v == 0 ? pos : -1);
            WF_CHECK_EQ(hs_proto_is_ascii(b, 0, 40), v < 0x80);
            WF_CHECK_EQ(hs_proto_find_json_escape(b, 0, 40),
                        v < 0x20 || v == '"' || v == '\\' ? pos : 40);
            memset(b, ' ', 40);
            b[pos] = (uint8_t)v;
            int ws = v == ' ' || v == '\t' || v == '\n' || v == '\r' || v == ',';
            WF_CHECK_EQ(hs_proto_skip_ws(b, 0, 40), ws || v == ';' ? 40 : pos);
            wf_free(b, sizeof tmp);
        }
    }
}

/* Regression: isAsciiBS / findJsonEscape on an empty ByteString pass
 * (NULL, 0, 0); the kernels formed NULL + 0, which is undefined in C and
 * reported by UBSan's pointer-overflow check. */
static void scanners_null_empty(void)
{
    WF_CHECK_EQ(hs_proto_is_ascii(NULL, 0, 0), 1);
    WF_CHECK_EQ(hs_proto_find_json_escape(NULL, 0, 0), 0);
    WF_CHECK_EQ(hs_proto_find_nul(NULL, 0, 0), -1);
    WF_CHECK_EQ(hs_proto_skip_ws(NULL, 0, 0), 0);
    WF_CHECK_EQ(hs_proto_validate_utf8_fast(NULL, 0), 1);
}

static void skip_ws_comments(void)
{
    static const struct { const char *s; int off, want; } t[] = {
        { "", 0, 0 },
        { "   x", 0, 3 },
        { ",,\t\r\n x", 0, 6 },
        { "; comment\nx", 0, 10 },
        { "; comment to the end", 0, 20 },
        { " ;a\n ;b\n\n  ,y", 0, 12 },
        { "                ;                \n                x", 0, 50 },
        { "x  ", 1, 3 },
        { "x  ", 5, 5 },
        { ";\n;\n;\n;\n;\n;\n;\n;\n;\n;\n;\n;\n;\n;\n;\n;\n;\n;\nq", 0, -1 }, /* -1: n - 1 */
    };
    for (size_t i = 0; i < sizeof t / sizeof t[0]; i++) {
        int n = (int)strlen(t[i].s);
        int want = t[i].want < 0 ? n - 1 : t[i].want;
        uint8_t *b = wf_dup(t[i].s, (size_t)n);
        WF_CHECK_EQ(hs_proto_skip_ws(b, t[i].off, n), want);
        WF_CHECK_EQ(ref_skip_ws(b, t[i].off, n), want);
        wf_free(b, (size_t)n);
    }
}

static void scanners_random(void)
{
    wf_rng r = wf_rng_from_env();
    static const uint8_t alpha[] = { 'a', ' ', '\t', '\n', '\r', ',', ';', 0, '"', '\\',
                                     0x1F, 0x20, 0x7F, 0x80, 0xFF };
    for (long it = 0; it < wf_iters(30000); it++) {
        int len = (int)wf_rng_below(&r, 200);
        uint8_t tmp[200];
        size_t alen = 1 + wf_rng_below(&r, sizeof alpha);
        if (wf_rng_below(&r, 2)) {
            /* whitespace-heavy runs so skip_ws goes through many blocks */
            for (int i = 0; i < len; i++) {
                tmp[i] = wf_rng_below(&r, 40) ? (uint8_t)" \t\n,"[wf_rng_below(&r, 4)]
                                               : alpha[wf_rng_below(&r, alen)];
            }
        } else {
            wf_rng_fill_from(&r, tmp, (size_t)len, alpha, alen);
        }
        uint8_t *b = wf_dup(tmp, (size_t)len);
        int off = (int)wf_rng_below(&r, (uint64_t)len + 2);
        WF_CHECK_EQ(hs_proto_find_nul(b, off, len), ref_find_nul(b, off, len));
        WF_CHECK_EQ(hs_proto_skip_ws(b, off, len), ref_skip_ws(b, off, len));
        if (off <= len) {
            WF_CHECK_EQ(hs_proto_is_ascii(b, off, len - off), ref_is_ascii(b, off, len - off));
            WF_CHECK_EQ(hs_proto_find_json_escape(b, off, len - off),
                        ref_find_json_escape(b, off, len - off));
        }
        wf_free(b, (size_t)len);
    }
}

/* Regression: the offset-taking scanners took (and returned) int, so a
 * ByteString or scan range over 2 GiB was truncated: the kernel worked on
 * a wrapped length and the position it returned was a bogus int. A sparse
 * mapping just over 2 GiB is reserved and only the page holding the
 * scanned window is touched, so the test costs one page of memory and
 * never reads the middle of the range. Skipped if the mapping fails. */
static void scanners_beyond_2gib(void)
{
    /* Sparse, so one page of memory; valgrind emulates it in minutes and
     * skips. The widths are also pinned at compile time in api.h. */
    if (wf_under_valgrind()) {
        printf("    scanners_beyond_2gib: skipped (valgrind)\n");
        return;
    }
    if (sizeof(ptrdiff_t) < 8 || sizeof(size_t) < 8) {
        printf("    scanners_beyond_2gib: skipped (32-bit position width)\n");
        return;
    }
    size_t pg = (size_t)sysconf(_SC_PAGESIZE);
    size_t total = ((size_t)1 << 31) + 4 * pg; /* > INT_MAX */
    uint8_t *big = mmap(NULL, total, PROT_READ | PROT_WRITE,
                        MAP_PRIVATE | MAP_ANON, -1, 0);
    if (big == MAP_FAILED) {
        printf("    scanners_beyond_2gib: skipped (no >2 GiB of address space)\n");
        return;
    }
    /* Window wholly past INT_MAX; len is the buffer length (> INT_MAX). */
    ptrdiff_t len = (ptrdiff_t)total;
    ptrdiff_t off = len - 128;
    memset(big + off, 'a', 128);

    /* find_nul: -1 on a miss, the position past INT_MAX on a hit. */
    WF_CHECK_EQ(hs_proto_find_nul(big, off, len), -1);
    WF_CHECK_EQ(ref_find_nul(big, off, len), -1);
    big[off + 40] = 0;
    WF_CHECK_EQ(hs_proto_find_nul(big, off, len), off + 40);
    WF_CHECK_EQ(ref_find_nul(big, off, len), off + 40);
    big[off + 40] = 'a';

    /* skip_ws: a whitespace run past INT_MAX, then the position of the
     * first other byte; and a fully skipped range reports len itself. */
    memset(big + off, ' ', 100);
    big[off + 100] = 'x';
    WF_CHECK_EQ(hs_proto_skip_ws(big, off, len), off + 100);
    WF_CHECK_EQ(ref_skip_ws(big, off, len), off + 100);
    memset(big + off, '\n', 128);
    WF_CHECK_EQ(hs_proto_skip_ws(big, off, len), len);
    WF_CHECK_EQ(ref_skip_ws(big, off, len), len);

    /* find_json_escape: len counts from offset; the miss is offset + len,
     * so both the escape position and the miss pass INT_MAX. */
    memset(big + off, 'a', 128);
    WF_CHECK_EQ(hs_proto_find_json_escape(big, off, 128), off + 128);
    WF_CHECK_EQ(ref_find_json_escape(big, off, 128), off + 128);
    big[off + 77] = '"';
    WF_CHECK_EQ(hs_proto_find_json_escape(big, off, 128), off + 77);
    big[off + 77] = 'a';
    big[off + 5] = 0x1f;
    WF_CHECK_EQ(hs_proto_find_json_escape(big, off, 128), off + 5);
    big[off + 5] = 'a';

    /* is_ascii: an all-ASCII window past INT_MAX, then a non-ASCII byte at
     * each end of it. */
    WF_CHECK_EQ(hs_proto_is_ascii(big, off, 128), 1);
    big[off + 127] = 0x80;
    WF_CHECK_EQ(hs_proto_is_ascii(big, off, 128), 0);
    big[off + 127] = 'a';
    big[off] = 0xff;
    WF_CHECK_EQ(hs_proto_is_ascii(big, off, 128), 0);

    munmap(big, total);
}

/* ------------------------------------------------------------------ */
/* Iceberg bounds                                                      */
/* ------------------------------------------------------------------ */

static void put_le32(uint8_t *p, int32_t v)
{
    uint32_t u = (uint32_t)v;
    p[0] = (uint8_t)u;
    p[1] = (uint8_t)(u >> 8);
    p[2] = (uint8_t)(u >> 16);
    p[3] = (uint8_t)(u >> 24);
}

/* Regression: with 32 or more int32 bounds the SIMD loop shifted a 4-bit
 * group by 28 (into the sign bit of int: signed overflow) and then by
 * >= 32 (shift past the width), and the scalar paths evaluated 1 << 31.
 * Bounds past the 32nd are now ignored, as the scalar path documents. */
static void bounds_many(void)
{
    for (int count = 0; count <= 70; count++) {
        for (int width = 0; width <= 9; width++) {
            size_t bsz = (size_t)count * (size_t)width;
            uint8_t *bounds = wf_alloc(bsz);
            uint8_t *search = wf_alloc((size_t)width);
            for (size_t i = 0; i < bsz; i++) bounds[i] = (uint8_t)(i * 37 + (size_t)width);
            for (int i = 0; i < width; i++) search[i] = (uint8_t)(0x80 + i);
            WF_CHECK_EQ(hs_proto_compare_bounds(bounds, count, width, search),
                        ref_compare_bounds(bounds, count, width, search));
            wf_free(bounds, bsz);
            wf_free(search, (size_t)width);
        }
    }
    /* all 32 bounds <= search: every bit set, as a C int that is -1 */
    uint8_t *b = wf_alloc(40 * 4), *s = wf_alloc(4);
    for (int i = 0; i < 40; i++) put_le32(b + 4 * i, INT32_MIN + i);
    put_le32(s, INT32_MAX);
    /* empty bounds/search ByteStrings (NULL) with width 0 */
    WF_CHECK_EQ(hs_proto_compare_bounds(NULL, 5, 0, NULL), 0x1F);
    WF_CHECK_EQ(hs_proto_compare_bounds(b, 40, 4, s), -1);
    WF_CHECK_EQ(hs_proto_compare_bounds(b, 32, 4, s), -1);
    WF_CHECK_EQ(hs_proto_compare_bounds(b, 31, 4, s), 0x7FFFFFFF);
    WF_CHECK_EQ(hs_proto_compare_bounds(b, 4, 4, s), 0xF);
    wf_free(b, 40 * 4);
    wf_free(s, 4);
}

static void bounds_random(void)
{
    wf_rng r = wf_rng_from_env();
    static const int32_t edges[] = { INT32_MIN, INT32_MIN + 1, -1, 0, 1, INT32_MAX - 1, INT32_MAX, 0x80, -0x80 };
    for (long it = 0; it < wf_iters(20000); it++) {
        int count = (int)wf_rng_below(&r, 40);
        int width = wf_rng_below(&r, 2) ? 4 : (int)wf_rng_below(&r, 9);
        size_t bsz = (size_t)count * (size_t)width;
        uint8_t *bounds = wf_alloc(bsz), *search = wf_alloc((size_t)width);
        if (width == 4) {
            int32_t sv = wf_rng_below(&r, 2) ? edges[wf_rng_below(&r, 9)] : (int32_t)wf_rng_next(&r);
            put_le32(search, sv);
            for (int i = 0; i < count; i++) {
                uint64_t m = wf_rng_below(&r, 3);
                int32_t v = m == 0 ? sv : m == 1 ? edges[wf_rng_below(&r, 9)] : (int32_t)wf_rng_next(&r);
                put_le32(bounds + 4 * i, v);
            }
        } else {
            wf_rng_fill(&r, bounds, bsz);
            wf_rng_fill(&r, search, (size_t)width);
            for (int i = 0; i < count; i++) {
                if (wf_rng_below(&r, 3) == 0) memcpy(bounds + (size_t)i * (size_t)width, search, (size_t)width);
            }
        }
        WF_CHECK_EQ(hs_proto_compare_bounds(bounds, count, width, search),
                    ref_compare_bounds(bounds, count, width, search));
        wf_free(bounds, bsz);
        wf_free(search, (size_t)width);
    }
}

/* ------------------------------------------------------------------ */
/* Arrow buffers                                                       */
/* ------------------------------------------------------------------ */

static int arrow(const int64_t *pairs, int count, int64_t body)
{
    size_t sz = (size_t)count * 16;
    /* validateArrowBuffers passes a Storable Buffer vector: 8-aligned */
    a8buf ab = a8_dup(pairs, sz);
    int64_t *p = ab.p;
    int got = hs_proto_validate_arrow_buffers(p, count, body);
    WF_CHECK_EQ(got, ref_validate_arrow(p, count, body));
    a8_free(ab);
    return got;
}

static void arrow_vectors(void)
{
    WF_CHECK_EQ(hs_proto_validate_arrow_buffers(NULL, 0, 0), 1);
    WF_CHECK_EQ(arrow((int64_t[]){ 0, 8 }, 1, 8), 1);
    WF_CHECK_EQ(arrow((int64_t[]){ 0, 9 }, 1, 8), 0);
    WF_CHECK_EQ(arrow((int64_t[]){ 0, 1, 8, 12 }, 2, 20), 1);
    WF_CHECK_EQ(arrow((int64_t[]){ 0, 0, 0, 0, 0, 8, 8, 0, 8, 8 }, 5, 16), 1);
    WF_CHECK_EQ(arrow((int64_t[]){ 0, 0, 0, 12, 16, 0, 16, 20 }, 4, 36), 1);
    WF_CHECK_EQ(arrow((int64_t[]){ 0, 8, 4, 8 }, 2, 16), 0);          /* overlap */
    WF_CHECK_EQ(arrow((int64_t[]){ 8, 8, 0, 8 }, 2, 16), 0);          /* out of order */
    WF_CHECK_EQ(arrow((int64_t[]){ 0, 8, 4, 0 }, 2, 16), 1);          /* empty inside */
    WF_CHECK_EQ(arrow((int64_t[]){ -1, 0 }, 1, 16), 0);
    WF_CHECK_EQ(arrow((int64_t[]){ 0, -1 }, 1, 16), 0);
    WF_CHECK_EQ(arrow((int64_t[]){ 0, 0, -8, 0 }, 2, 16), 0);
    WF_CHECK_EQ(arrow((int64_t[]){ 0, 0, 0, INT64_MIN }, 2, 16), 0);
    WF_CHECK_EQ(arrow((int64_t[]){ 0, 0, 0, 0, INT64_MIN, 0 }, 3, 16), 0);
    /* offset + length overflows int64 */
    WF_CHECK_EQ(arrow((int64_t[]){ INT64_MAX, INT64_MAX }, 1, INT64_MAX), 0);
    WF_CHECK_EQ(arrow((int64_t[]){ 0, 0, INT64_MAX, 1 }, 2, INT64_MAX), 0);
    WF_CHECK_EQ(arrow((int64_t[]){ 0, 0, 1, INT64_MAX }, 2, INT64_MAX), 0);
    WF_CHECK_EQ(arrow((int64_t[]){ 0, 0, INT64_MAX, 0 }, 2, INT64_MAX), 1);
    WF_CHECK_EQ(arrow((int64_t[]){ 0, INT64_MAX }, 1, INT64_MAX), 1);
    /* negative body length rejects every buffer */
    WF_CHECK_EQ(arrow((int64_t[]){ 0, 0 }, 1, -1), 0);
    WF_CHECK_EQ(arrow((int64_t[]){ 0, 0 }, 1, INT64_MIN), 0);
}

static void arrow_random(void)
{
    wf_rng r = wf_rng_from_env();
    static const int64_t edges[] = { 0, 1, -1, 7, 8, INT64_MAX, INT64_MIN, INT64_MAX - 1, 1LL << 62 };
    for (long it = 0; it < wf_iters(30000); it++) {
        int count = (int)wf_rng_below(&r, 12);
        int64_t p[24];
        int64_t pos = 0;
        int64_t body = (int64_t)wf_rng_below(&r, 300);
        for (int i = 0; i < count; i++) {
            int64_t gap = (int64_t)wf_rng_below(&r, 4) - (wf_rng_below(&r, 6) == 0);
            int64_t len = (int64_t)wf_rng_below(&r, 40) * (wf_rng_below(&r, 4) != 0);
            p[2 * i] = pos + gap;
            p[2 * i + 1] = len;
            pos += gap + len;
            if (wf_rng_below(&r, 25) == 0) p[2 * i] = edges[wf_rng_below(&r, 9)];
            if (wf_rng_below(&r, 25) == 0) p[2 * i + 1] = edges[wf_rng_below(&r, 9)];
        }
        if (wf_rng_below(&r, 2)) body = pos + (int64_t)wf_rng_below(&r, 3) - 1;
        if (wf_rng_below(&r, 30) == 0) body = edges[wf_rng_below(&r, 9)];
        arrow(p, count, body);
    }
}

int main(void)
{
    (void)wf_rng_from_env();
    WF_RUN(utf8_vectors);
    WF_RUN(utf8_exhaustive_short);
    WF_RUN(utf8_four_byte_and_scalars);
    WF_RUN(utf8_boundary_sweep);
    WF_RUN(utf8_random);
    WF_RUN(scanners_exhaustive);
    WF_RUN(scanners_byte_classes);
    WF_RUN(scanners_null_empty);
    WF_RUN(skip_ws_comments);
    WF_RUN(scanners_random);
    WF_RUN(scanners_beyond_2gib);
    WF_RUN(bounds_many);
    WF_RUN(bounds_random);
    WF_RUN(arrow_vectors);
    WF_RUN(arrow_random);
    return wf_finish("core-fast-decode/scan");
}
