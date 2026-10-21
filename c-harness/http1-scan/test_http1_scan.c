/* Unit, boundary and differential tests for wireform-http1 http1_scan.c. */
#include "harness.h"
#include "api.h"
#include "head_diff.h"

#include <sys/mman.h>

/* A byte that does not stop a scan of the given kind, and one that does. */
static const uint8_t filler_of[SCAN_KINDS] = { 'a', 'a', 'a', 'a' };
static const uint8_t stopper_of[SCAN_KINDS] = { '\r', '\n', ':', '\r' };

/* ------------------------------------------------------------------ */
/* Scanners                                                            */
/* ------------------------------------------------------------------ */

/* The kernels' byte classes are exactly the RFC ones, for every byte value
 * at every lane of a 16-byte block (and in the scalar tail). */
static void scan_byte_classes(void)
{
    enum { N = 48 };
    wf_guarded g = wf_guard_tail(N);
    for (int kind = 0; kind < SCAN_KINDS; kind++) {
        for (int v = 0; v < 256; v++) {
            int stops = scan_stops(kind, (uint8_t)v);
            for (int pos = 0; pos < N; pos++) {
                memset(g.p, filler_of[kind], N);
                g.p[pos] = (uint8_t)v;
                WF_CHECK_EQ(scan_kernel(kind, g.p, 0, N), stops ? pos : N);
            }
        }
    }
    wf_guard_free(g);
}

/* RFC 9110 5.6.2 spelled out, and the field-value set from 5.5. */
static void scan_rfc_sets(void)
{
    static const char tchars[] =
        "!#$%&'*+-.^_`|~0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    int n = (int)strlen(tchars);
    uint8_t *b = wf_dup(tchars, (size_t)n);
    WF_CHECK_EQ(hs_http1_find_non_token(b, 0, n), n);
    wf_free(b, (size_t)n);
    int count = 0;
    for (int v = 0; v < 256; v++) count += ref_is_tchar((uint8_t)v);
    WF_CHECK_EQ(count, n);

    static const char separators[] = "\"(),/:;<=>?@[\\]{} \t";
    for (const char *s = separators; *s; s++) {
        uint8_t one = (uint8_t)*s;
        WF_CHECK_EQ(hs_http1_find_non_token(&one, 0, 1), 0);
    }
    /* obs-text is a legal field-value byte but never a token byte.
     * Regression: the scalar is_fv_ok table was shifted by one entry from
     * 0x7F on, so DEL was accepted and 0x80 rejected in field values on
     * every non-x86 build (and in the scalar tail on x86). */
    uint8_t hi[2] = { 0x80, 0xff };
    WF_CHECK_EQ(hs_http1_find_non_fieldvalue(hi, 0, 2), 2);
    WF_CHECK_EQ(hs_http1_find_non_token(hi, 0, 2), 0);
    /* NUL, DEL and every C0 control except HTAB end a value. */
    for (int v = 0; v < 0x20; v++) {
        uint8_t c = (uint8_t)v;
        WF_CHECK_EQ(hs_http1_find_non_fieldvalue(&c, 0, 1), v == 0x09 ? 1 : 0);
    }
    uint8_t del = 0x7f;
    WF_CHECK_EQ(hs_http1_find_non_fieldvalue(&del, 0, 1), 0);
}

/* Every (len, offset, stop position) in a window covering zero, one and
 * several 16-byte blocks plus the scalar tail, buffer flush against a
 * guard page. Two stop bytes per buffer so "first match wins" inside one
 * block is covered as well. */
static void scan_exhaustive(void)
{
    enum { MAXN = 80 };
    wf_guarded g = wf_guard_tail(MAXN);
    for (int kind = 0; kind < SCAN_KINDS; kind++) {
        uint8_t fill = filler_of[kind], stop = stopper_of[kind];
        for (int len = 0; len <= MAXN; len++) {
            uint8_t *b = g.p + (g.n - (size_t)len);
            memset(b, fill, (size_t)len);
            for (int off = 0; off <= len + 17; off++) {
                WF_CHECK_EQ(scan_kernel(kind, b, off, len), len);
            }
            for (int pos = 0; pos < len; pos++) {
                b[pos] = stop;
                for (int off = 0; off <= len; off++) {
                    int want = off <= pos ? pos : len;
                    WF_CHECK_EQ(scan_kernel(kind, b, off, len), want);
                }
                int second = pos + 1 + (pos % 5);
                if (second < len) {
                    b[second] = stop;
                    for (int off = 0; off <= len; off++) {
                        WF_CHECK_EQ(scan_kernel(kind, b, off, len), scan_ref(kind, b, off, len));
                    }
                    b[second] = fill;
                }
                b[pos] = fill;
            }
        }
    }
    wf_guard_free(g);
}

/* The buffer end at every distance 0..63 before the guard page, the gap
 * holding stop bytes: a read past len changes the answer (ASan, where the
 * gap is addressable) or faults (gap 0, guard page). */
static void scan_gaps(void)
{
    enum { MAXN = 80, GAP = 64 };
    wf_guarded g = wf_guard_tail(MAXN + GAP);
    for (int kind = 0; kind < SCAN_KINDS; kind++) {
        for (int gap = 0; gap < GAP; gap++) {
            for (int len = 0; len <= MAXN; len++) {
                uint8_t *b = g.p + g.n - (size_t)len - (size_t)gap;
                memset(b, filler_of[kind], (size_t)len);
                memset(b + len, stopper_of[kind], (size_t)gap);
                for (int off = 0; off <= len; off++) {
                    WF_CHECK_EQ(scan_kernel(kind, b, off, len), len);
                }
                if (len) {
                    b[len - 1] = stopper_of[kind];
                    WF_CHECK_EQ(scan_kernel(kind, b, 0, len), len - 1);
                }
            }
        }
    }
    wf_guard_free(g);
}

/* Start alignment: sub-slices (p + k, n - k) of one tail-guarded buffer. */
static void scan_alignments(void)
{
    enum { N = 70 };
    wf_rng r = wf_rng_from_env();
    static const uint8_t alpha[] = { 'a', 'Z', '0', '-', ' ', '\t', '\r', '\n', ':', 0, 0x7f, 0x80, 0xff, ';' };
    wf_guarded g = wf_guard_tail(N);
    for (int rep = 0; rep < 200; rep++) {
        wf_rng_fill_from(&r, g.p, N, alpha, sizeof alpha);
        for (int k = 0; k < 32; k++) {
            const uint8_t *b = g.p + k;
            int len = N - k;
            for (int kind = 0; kind < SCAN_KINDS; kind++) {
                for (int off = 0; off <= len; off += 1 + (off & 3)) {
                    WF_CHECK_EQ(scan_kernel(kind, b, off, len), scan_ref(kind, b, off, len));
                }
            }
        }
    }
    wf_guard_free(g);
}

/* Nothing before buf + offset is read: buffer flush with a head guard. */
static void scan_head_guard(void)
{
    enum { N = 64 };
    wf_guarded g = wf_guard_head(N);
    for (int kind = 0; kind < SCAN_KINDS; kind++) {
        memset(g.p, filler_of[kind], N);
        for (int len = 0; len <= N; len++) {
            WF_CHECK_EQ(scan_kernel(kind, g.p, 0, len), len);
        }
        g.p[0] = stopper_of[kind];
        WF_CHECK_EQ(scan_kernel(kind, g.p, 0, N), 0);
    }
    wf_guard_free(g);
}

static void scan_random(void)
{
    wf_rng r = wf_rng_from_env();
    static const uint8_t alpha[] = { 'a', 'Z', '0', '-', '!', ' ', '\t', '\r', '\n', ':', 0, 0x7f, 0x80, 0xff, 0x1f, '"' };
    long iters = wf_iters(20000);
    uint8_t *tmp = malloc(1200);
    for (long it = 0; it < iters; it++) {
        int len = (int)wf_rng_below(&r, 1200);
        int kind = (int)wf_rng_below(&r, SCAN_KINDS);
        /* Mostly passing bytes so scans run long, sometimes pure noise. */
        if (wf_rng_next(&r) & 1) {
            memset(tmp, filler_of[kind], (size_t)len);
            int stops = (int)wf_rng_below(&r, 4);
            for (int s = 0; s < stops && len; s++) {
                tmp[wf_rng_below(&r, (uint64_t)len)] = alpha[wf_rng_below(&r, sizeof alpha)];
            }
        } else {
            wf_rng_fill(&r, tmp, (size_t)len);
        }
        uint8_t *b = wf_dup(tmp, (size_t)len);
        int off = (int)wf_rng_below(&r, (uint64_t)len + 20);
        WF_CHECK_EQ(scan_kernel(kind, b, off, len), scan_ref(kind, b, off, len));
        wf_free(b, (size_t)len);
    }
    free(tmp);
}

/* Regression: the block loops tested `i + 16 <= len`, which overflows int
 * (UB; at -O3 the wrapped sum passes the test and the next block is read
 * past `len`) once len > INT_MAX - 16. Every len up to INT_MAX is in the
 * contract. Only the last few bytes below `len` are mapped: the buffer
 * pointer is placed INT_MAX - W bytes before a small readable window, so
 * any access outside [offset, len) faults. */
static void scan_len_near_int_max(void)
{
    enum { W = 40 };
    wf_guarded g = wf_guard_tail(W);
    uintptr_t window = (uintptr_t)g.p;
    if (window < (uintptr_t)INT_MAX) { /* cannot place the base; skip */
        wf_guard_free(g);
        return;
    }
    const uint8_t *base = (const uint8_t *)(window - ((uintptr_t)INT_MAX - W));
    for (int kind = 0; kind < SCAN_KINDS; kind++) {
        memset(g.p, filler_of[kind], W);
        for (int back = 0; back <= W; back++) {
            int off = INT_MAX - back;
            WF_CHECK_EQ(scan_kernel(kind, base, off, INT_MAX), INT_MAX);
        }
        g.p[W - 1] = stopper_of[kind];
        for (int back = 1; back <= W; back++) {
            WF_CHECK_EQ(scan_kernel(kind, base, INT_MAX - back, INT_MAX), INT_MAX - 1);
        }
    }
    /* to_lower / ieq start at index 0, so the same overflow there needs a
     * 2 GiB object; their loops share the scanners' bound expression. */
    wf_guard_free(g);
}

/* Offsets and lengths above 2^31 used to be truncated to int on the Haskell
 * side, and the block bound overflowed int at the same point. They are
 * ptrdiff_t now. LEN is 3 GiB (> INT_MAX) but only the last few bytes are
 * mapped: the base pointer is placed LEN - W bytes before a small
 * guard-page buffer, and every scan starts within the last W bytes, so any
 * access outside [offset, LEN) faults and a narrowed length would read the
 * wrong window. No multi-GiB memory is mapped or touched. */
static void scan_len_beyond_2gib(void)
{
    enum { W = 40 };
    const ptrdiff_t LEN = (ptrdiff_t)3 << 30;
    if (sizeof(ptrdiff_t) < 8) {
        printf("scan_len_beyond_2gib: skipped (32-bit ptrdiff_t)\n");
        return;
    }
    wf_guarded g = wf_guard_tail(W);
    uintptr_t window = (uintptr_t)g.p;
    if (window < (uintptr_t)LEN) {
        printf("scan_len_beyond_2gib: skipped (address space)\n");
        wf_guard_free(g);
        return;
    }
    const uint8_t *base = (const uint8_t *)(window - ((uintptr_t)LEN - W));
    for (int kind = 0; kind < SCAN_KINDS; kind++) {
        memset(g.p, filler_of[kind], W);
        for (int back = 0; back <= W; back++) {
            WF_CHECK_EQ(scan_kernel(kind, base, LEN - (ptrdiff_t)back, LEN), LEN);
        }
        g.p[W - 1] = stopper_of[kind];
        for (int back = 1; back <= W; back++) {
            WF_CHECK_EQ(scan_kernel(kind, base, LEN - (ptrdiff_t)back, LEN), LEN - 1);
        }
    }
    wf_guard_free(g);
}

/* ------------------------------------------------------------------ */
/* Case folding                                                        */
/* ------------------------------------------------------------------ */

static void lower_all_bytes(void)
{
    uint8_t src[256], want[256];
    for (int v = 0; v < 256; v++) src[v] = (uint8_t)v;
    ref_to_lower(src, want, 256);
    for (int v = 0; v < 256; v++) {
        WF_CHECK_EQ(want[v], (v >= 'A' && v <= 'Z') ? v + 32 : v);
    }
    uint8_t *s = wf_dup(src, 256), *d = wf_alloc(256);
    hs_http1_to_lower_ascii(s, d, 256);
    WF_CHECK_MEM(d, want, 256);
    hs_http1_to_lower_ascii(s, s, 256); /* in place */
    WF_CHECK_MEM(s, want, 256);
    wf_free(s, 256);
    wf_free(d, 256);
    /* Boundary bytes around the A..Z range in every lane. */
    static const uint8_t edge[] = { '@', 'A', 'Z', '[', '`', 'a', 'z', '{', 0xc1, 0xda, 0x41 ^ 0x80 };
    uint8_t blk[16], out[16], ref[16];
    for (size_t e = 0; e < sizeof edge; e++) {
        for (int lane = 0; lane < 16; lane++) {
            memset(blk, 'Q', 16);
            blk[lane] = edge[e];
            hs_http1_to_lower_ascii(blk, out, 16);
            ref_to_lower(blk, ref, 16);
            WF_CHECK_MEM(out, ref, 16);
        }
    }
}

/* Every length and destination skew; dst ends at a guard page (skew = 0)
 * or is followed by a canary that must survive, and src is exact-size. */
static void lower_lengths(void)
{
    enum { MAXN = 100 };
    wf_rng r = wf_rng_from_env();
    uint8_t src[MAXN], want[MAXN];
    static const uint8_t canary[16] = { 0xa5, 0xa5, 0xa5, 0xa5, 0xa5, 0xa5, 0xa5, 0xa5,
                                        0xa5, 0xa5, 0xa5, 0xa5, 0xa5, 0xa5, 0xa5, 0xa5 };
    wf_guarded gd = wf_guard_tail(MAXN + 16);
    wf_guarded gs = wf_guard_tail(MAXN);
    for (size_t len = 0; len <= MAXN; len++) {
        for (int skew = 0; skew < 16; skew++) {
            wf_rng_fill(&r, src, (size_t)len);
            for (size_t i = 0; i < len; i += 3) src[i] = (uint8_t)('A' + i % 26);
            ref_to_lower(src, want, len);
            uint8_t *s = gs.p + (gs.n - (size_t)len);
            memcpy(s, src, (size_t)len);
            uint8_t *d = gd.p + (gd.n - (size_t)len) - (size_t)skew;
            memcpy(d + len, canary, (size_t)skew);
            hs_http1_to_lower_ascii(s, d, len);
            WF_CHECK_MEM(d, want, (size_t)len);
            WF_CHECK_MEM(d + len, canary, (size_t)skew);
            WF_CHECK_MEM(s, src, (size_t)len); /* src untouched */
            /* in place, src flush with the guard page */
            hs_http1_to_lower_ascii(s, s, len);
            WF_CHECK_MEM(s, want, (size_t)len);
        }
    }
    /* len == 0 writes nothing. len is a byte count (size_t), so a negative
     * length is not in the contract; the Haskell caller passes BS.length. */
    uint8_t one = 'A';
    hs_http1_to_lower_ascii(&one, &one, 0);
    WF_CHECK_EQ(one, 'A');
    wf_guard_free(gd);
    wf_guard_free(gs);
}

static void ieq_all_pairs(void)
{
    /* Every (a, b) byte pair, in a SIMD lane and in the scalar tail. */
    enum { N = 33 };
    uint8_t *x = wf_alloc(N), *y = wf_alloc(N);
    for (int a = 0; a < 256; a++) {
        for (int b = 0; b < 256; b++) {
            int want = ref_lower((uint8_t)a) == ref_lower((uint8_t)b);
            memset(x, 'k', N);
            memset(y, 'K', N);
            x[5] = (uint8_t)a;
            y[5] = (uint8_t)b;
            x[32] = (uint8_t)b;
            y[32] = (uint8_t)a;
            WF_CHECK_EQ(hs_http1_ascii_ieq(x, y, 16), want);
            WF_CHECK_EQ(hs_http1_ascii_ieq(x + 32, y + 32, 1), want);
            WF_CHECK_EQ(hs_http1_ascii_ieq(x, y, N), want);
        }
    }
    wf_free(x, N);
    wf_free(y, N);
}

static void ieq_lengths(void)
{
    enum { MAXN = 100 };
    wf_rng r = wf_rng_from_env();
    uint8_t src[MAXN], alt[MAXN];
    wf_guarded ga = wf_guard_tail(MAXN), gb = wf_guard_tail(MAXN);
    static const uint8_t alpha[] = { 'a', 'A', 'z', 'Z', '@', '[', '`', '{', '-', 0xc1, 0xe1, 0 };
    for (size_t len = 0; len <= MAXN; len++) {
        wf_rng_fill_from(&r, src, (size_t)len, alpha, sizeof alpha);
        /* Flip the case of random letters: still equal. */
        for (size_t i = 0; i < len; i++) {
            uint8_t c = src[i];
            int letter = (c | 0x20) >= 'a' && (c | 0x20) <= 'z';
            alt[i] = (letter && (wf_rng_next(&r) & 1)) ? (uint8_t)(c ^ 0x20) : c;
        }
        uint8_t *a = ga.p + (ga.n - (size_t)len), *b = gb.p + (gb.n - (size_t)len);
        memcpy(a, src, (size_t)len);
        memcpy(b, alt, (size_t)len);
        WF_CHECK_EQ(hs_http1_ascii_ieq(a, b, len), 1);
        /* A mismatch at each position is seen; the 0x20 flip on a
         * non-letter must not be folded away. */
        for (size_t pos = 0; pos < len; pos++) {
            uint8_t keep = b[pos];
            b[pos] = (uint8_t)(keep ^ 0x20);
            WF_CHECK_EQ(hs_http1_ascii_ieq(a, b, len), ref_ieq(a, b, len));
            b[pos] = (uint8_t)(keep ^ 0x01);
            WF_CHECK_EQ(hs_http1_ascii_ieq(a, b, len), 0);
            b[pos] = keep;
        }
    }
    WF_CHECK_EQ(hs_http1_ascii_ieq(ga.p, gb.p, 0), 1);
    /* A negative length is not in the size_t contract (see api.h). */
    wf_guard_free(ga);
    wf_guard_free(gb);
    static const char *pairs[][2] = {
        { "Transfer-Encoding", "transfer-encoding" }, { "CHUNKED", "chunked" },
        { "Content-Length", "content-length" }, { "keep-alive", "Keep-Alive" },
    };
    for (size_t k = 0; k < sizeof pairs / sizeof pairs[0]; k++) {
        int n = (int)strlen(pairs[k][0]);
        WF_CHECK_EQ(hs_http1_ascii_ieq(pairs[k][0], pairs[k][1], n), 1);
    }
    WF_CHECK_EQ(hs_http1_ascii_ieq("chunked", "chunkee", 7), 0);
}

/* ------------------------------------------------------------------ */
/* Chunk-size hex                                                      */
/* ------------------------------------------------------------------ */

static void hex_check(const char *s, int off, int len, int ret, uint64_t val, int consumed)
{
    uint8_t *b = wf_dup(s, (size_t)len);
    uint64_t out = 0xababababababababULL;
    int c = -77;
    WF_CHECK_EQ(hs_http1_parse_hex(b, off, len, &out, &c), ret);
    WF_CHECK_EQ(c, consumed);
    if (ret >= 0) WF_CHECK(out == val);
    else WF_CHECK(out == 0xababababababababULL); /* untouched on overflow */
    wf_free(b, (size_t)len);
}

static void hex_vectors(void)
{
    hex_check("0", 0, 1, 1, 0, 1);
    hex_check("a", 0, 1, 1, 10, 1);
    hex_check("F", 0, 1, 1, 15, 1);
    hex_check("1a2B;ext=1\r\n", 0, 12, 1, 0x1a2b, 4);
    hex_check("ffffffffffffffff", 0, 16, 1, UINT64_MAX, 16);
    hex_check("FFFFFFFFFFFFFFFF\r\n", 0, 18, 1, UINT64_MAX, 16);
    hex_check("0000000000000000", 0, 16, 1, 0, 16);
    hex_check("10000000000000000", 0, 17, -1, 0, 16);
    hex_check("00000000000000001", 0, 17, -1, 0, 16); /* 17 digits, even with leading zeros */
    hex_check("", 0, 0, 0, 0, 0);
    hex_check(";", 0, 1, 0, 0, 0);
    hex_check(" 1", 0, 2, 0, 0, 0);
    hex_check("+1", 0, 2, 0, 0, 0);
    hex_check("0x10", 0, 4, 1, 0, 1);
    hex_check("xx7f", 2, 4, 1, 0x7f, 2);
    hex_check("7f", 2, 2, 0, 0, 0);  /* offset == len */
    hex_check("7f", 5, 2, 0, 0, 0);  /* offset > len */
    hex_check("abc", 0, 2, 1, 0xab, 2); /* stops at len */
    /* Bytes adjacent to the digit / letter ranges are not digits. */
    static const char notdig[] = "/:@G`g\x80\xff";
    for (const char *p = notdig; *p; p++) {
        char s[2] = { *p, 0 };
        hex_check(s, 0, 1, 0, 0, 0);
    }
}

static void hex_random(void)
{
    wf_rng r = wf_rng_from_env();
    static const uint8_t alpha[] = { '0', '1', '9', 'a', 'f', 'A', 'F', 'g', ';', '\r', '0', '0', 'e', 'E' };
    long iters = wf_iters(50000);
    uint8_t tmp[40];
    for (long it = 0; it < iters; it++) {
        int len = (int)wf_rng_below(&r, sizeof tmp + 1);
        wf_rng_fill_from(&r, tmp, (size_t)len, alpha, sizeof alpha);
        int off = (int)wf_rng_below(&r, (uint64_t)len + 3);
        uint8_t *b = wf_dup(tmp, (size_t)len);
        uint64_t o1 = 1, o2 = 1;
        int c1 = -1, c2 = -1;
        int r1 = hs_http1_parse_hex(b, off, len, &o1, &c1);
        int r2 = ref_parse_hex(b, off, len, &o2, &c2);
        WF_CHECK_EQ(r1, r2);
        WF_CHECK_EQ(c1, c2);
        WF_CHECK(o1 == o2);
        wf_free(b, (size_t)len);
    }
}

/* parse_hex offset/len are ptrdiff_t as well: hex digits at the end of a
 * 3 GiB logical buffer are found at their real offset. Same sparse mapping
 * trick as scan_len_beyond_2gib (only the last W bytes are readable). */
static void hex_offsets_beyond_2gib(void)
{
    enum { W = 40 };
    const ptrdiff_t LEN = (ptrdiff_t)3 << 30;
    if (sizeof(ptrdiff_t) < 8) {
        printf("hex_offsets_beyond_2gib: skipped (32-bit ptrdiff_t)\n");
        return;
    }
    wf_guarded g = wf_guard_tail(W);
    uintptr_t window = (uintptr_t)g.p;
    if (window < (uintptr_t)LEN) {
        printf("hex_offsets_beyond_2gib: skipped (address space)\n");
        wf_guard_free(g);
        return;
    }
    const uint8_t *base = (const uint8_t *)(window - ((uintptr_t)LEN - W));
    memset(g.p, ';', W);
    g.p[W - 2] = '1';
    g.p[W - 1] = 'f';
    uint64_t out = 0;
    int consumed = -1;
    WF_CHECK_EQ(hs_http1_parse_hex(base, LEN - 2, LEN, &out, &consumed), 1);
    WF_CHECK_EQ(consumed, 2);
    WF_CHECK(out == 0x1f);
    /* offset == len past the end is still a clean "no digits". */
    out = 7;
    consumed = -1;
    WF_CHECK_EQ(hs_http1_parse_hex(base, LEN, LEN, &out, &consumed), 0);
    WF_CHECK_EQ(consumed, 0);
    WF_CHECK(out == 0);
    wf_guard_free(g);
}

/* ------------------------------------------------------------------ */
/* Head model                                                          */
/* ------------------------------------------------------------------ */

static head_summary head_run(const char *s, size_t n)
{
    head_summary sum;
    uint8_t *b = wf_dup(s, n);
    const char *err = head_diff(b, (int)n, &sum);
    if (err) {
        wf__fail(__FILE__, __LINE__, err);
        wf_hexdump("head", b, n);
    }
    wf__checks++;
    wf_free(b, n);
    return sum;
}

#define HEAD(lit) head_run((lit), sizeof(lit) - 1)

static void head_real_messages(void)
{
    head_summary s;
    s = HEAD("GET /index.html HTTP/1.1\r\nHost: example.com\r\nUser-Agent: curl/8.5.0\r\n"
             "Accept: */*\r\n\r\n");
    WF_CHECK_EQ(s.req_status, H_OK);
    WF_CHECK_EQ(s.blk_status, H_OK);
    WF_CHECK_EQ(s.blk_fields, 3);
    WF_CHECK_EQ(s.lines, 4);

    s = HEAD("POST /submit?x=1 HTTP/1.1\r\nHost: a\r\nContent-Type: application/x-www-form-urlencoded\r\n"
             "Content-Length: 27\r\nTransfer-Encoding: gzip, chunked\r\n\r\nfield1=value1&field2=value2");
    WF_CHECK_EQ(s.blk_status, H_OK);
    WF_CHECK_EQ(s.blk_fields, 4);

    s = HEAD("HTTP/1.1 200 OK\r\nDate: Mon, 27 Jul 2009 12:28:53 GMT\r\nServer: Apache\r\n"
             "Last-Modified: Wed, 22 Jul 2009 19:15:56 GMT\r\nETag: \"34aa387-d-1568eb00\"\r\n"
             "Vary: Authorization,Accept\r\nX-Opaque: \x80\xfe\xff obs text\r\n\r\n");
    WF_CHECK_EQ(s.blk_status, H_OK);
    WF_CHECK_EQ(s.blk_fields, 6);
    /* '/' is not a tchar, so a status line never splits as a request
     * line ("HTTP" is not followed by SP). */
    WF_CHECK_EQ(s.req_status, H_BAD_REQ_LINE);

    s = HEAD("GET / HTTP/1.1\r\n\r\n");
    WF_CHECK_EQ(s.blk_status, H_OK);
    WF_CHECK_EQ(s.blk_fields, 0);

    /* Empty values, OWS on both sides, HTAB inside. */
    s = HEAD("GET / HTTP/1.1\r\nX-Empty:\r\nX-Ows: \t v1 \t v2\t \r\nX-Only-Ows:  \t \r\n\r\n");
    WF_CHECK_EQ(s.blk_status, H_OK);
    WF_CHECK_EQ(s.blk_fields, 3);
}

static void head_malformed(void)
{
    head_summary s;
    /* obs-fold (RFC 9112 5.2) */
    s = HEAD("GET / HTTP/1.1\r\nX-A: one\r\n two\r\n\r\n");
    WF_CHECK_EQ(s.blk_status, H_BAD_VALUE);
    s = HEAD("GET / HTTP/1.1\r\nX-A: one\r\n\ttwo\r\n\r\n");
    WF_CHECK_EQ(s.blk_status, H_BAD_VALUE);
    /* bare CR inside a value and at the end of a value */
    s = HEAD("GET / HTTP/1.1\r\nX-A: a\rb\r\n\r\n");
    WF_CHECK_EQ(s.blk_status, H_BAD_VALUE);
    s = HEAD("GET / HTTP/1.1\r\nX-A: a\r\r\n\r\n");
    WF_CHECK_EQ(s.blk_status, H_BAD_VALUE);
    /* bare LF: the line scanner reports it, the block parser rejects it */
    s = HEAD("GET / HTTP/1.1\r\nX-A: a\nX-B: b\r\n\r\n");
    WF_CHECK_EQ(s.line_stop, H_LINE_BARE_LF);
    WF_CHECK_EQ(s.blk_status, H_BAD_VALUE);
    s = HEAD("GET / HTTP/1.1\nHost: a\r\n\r\n");
    WF_CHECK_EQ(s.line_stop, H_LINE_BARE_LF);
    /* NUL / DEL / C0 controls in a value */
    s = HEAD("GET / HTTP/1.1\r\nX-A: a\0b\r\n\r\n");
    WF_CHECK_EQ(s.blk_status, H_BAD_VALUE);
    s = HEAD("GET / HTTP/1.1\r\nX-A: a\x7f\r\n\r\n");
    WF_CHECK_EQ(s.blk_status, H_BAD_VALUE);
    s = HEAD("GET / HTTP/1.1\r\nX-A: \x01\r\n\r\n");
    WF_CHECK_EQ(s.blk_status, H_BAD_VALUE);
    /* bad names: empty, space before colon, non-token, missing colon */
    s = HEAD("GET / HTTP/1.1\r\n: v\r\n\r\n");
    WF_CHECK_EQ(s.blk_status, H_BAD_NAME);
    s = HEAD("GET / HTTP/1.1\r\nHost : a\r\n\r\n");
    WF_CHECK_EQ(s.blk_status, H_BAD_NAME);
    s = HEAD("GET / HTTP/1.1\r\nHo\"st: a\r\n\r\n");
    WF_CHECK_EQ(s.blk_status, H_BAD_NAME);
    s = HEAD("GET / HTTP/1.1\r\nHost\r\n\r\n");
    WF_CHECK_EQ(s.blk_status, H_BAD_NAME);
    s = HEAD("GET / HTTP/1.1\r\nH\xc3\xa9: a\r\n\r\n");
    WF_CHECK_EQ(s.blk_status, H_BAD_NAME);
    /* request line */
    s = HEAD("GET  / HTTP/1.1\r\n\r\n");
    WF_CHECK_EQ(s.req_status, H_BAD_REQ_LINE);
    s = HEAD(" GET / HTTP/1.1\r\n\r\n");
    WF_CHECK_EQ(s.req_status, H_BAD_REQ_LINE);
    s = HEAD("GET /\r\n\r\n");
    WF_CHECK_EQ(s.req_status, H_BAD_REQ_LINE);
    s = HEAD("G(T / HTTP/1.1\r\n\r\n");
    WF_CHECK_EQ(s.req_status, H_BAD_REQ_LINE);
    s = HEAD("GET\r\n\r\n");
    WF_CHECK_EQ(s.req_status, H_BAD_REQ_LINE);
}

/* Every prefix of a complete head is either "need more" for framing or
 * agrees with the reference; the complete head is found exactly once the
 * final LF arrives. */
static void head_truncations(void)
{
    static const char msg[] =
        "PUT /upload/file.bin HTTP/1.1\r\nHost: upload.example\r\nContent-Length: 1048576\r\n"
        "Expect: 100-continue\r\nX-Pad: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r\n\r\n";
    size_t n = sizeof msg - 1;
    for (size_t k = 0; k <= n; k++) {
        head_summary s = head_run(msg, k);
        WF_CHECK_EQ(s.head_end, k == n ? (long long)n - 4 : -1);
    }
}

/* Very long lines and values: several kilobytes of value bytes, so the
 * block loops run thousands of iterations; the terminator, a forbidden
 * byte, or the buffer end can sit at any lane. */
static void head_long_lines(void)
{
    enum { V = 70000 };
    static const char pre[] = "GET / HTTP/1.1\r\nX-Long: ";
    size_t pl = sizeof pre - 1;
    char *m = malloc(pl + V + 8);
    memcpy(m, pre, pl);
    memset(m + pl, 'v', V);
    for (int tail = 0; tail < 20; tail++) {
        size_t vl = V - (size_t)tail;
        memcpy(m + pl + vl, "\r\n\r\n", 4);
        head_summary s = head_run(m, pl + vl + 4);
        WF_CHECK_EQ(s.blk_status, H_OK);
        WF_CHECK_EQ(s.blk_fields, 1);
        m[pl + vl - 1 - (size_t)tail] = '\0';
        s = head_run(m, pl + vl + 4);
        WF_CHECK_EQ(s.blk_status, H_BAD_VALUE);
        m[pl + vl - 1 - (size_t)tail] = 'v';
        s = head_run(m, pl + vl); /* truncated: no line end at all */
        WF_CHECK_EQ(s.head_end, -1);
        WF_CHECK_EQ(s.line_stop, H_LINE_NEED_MORE);
    }
    /* A huge token name. */
    memset(m, 'N', V);
    memcpy(m + V, ":x", 2);
    uint8_t *b = wf_dup(m, V + 2);
    h_fields kf, rf;
    uint8_t *c = wf_dup(m, V + 2);
    kp_header_block(b, V + 2, 1, &kf);
    rp_header_block(c, V + 2, 1, &rf);
    WF_CHECK(hd_fields_equal(&kf, &rf));
    WF_CHECK_EQ(kf.status, H_OK);
    WF_CHECK_EQ(kf.f[0].ne, V);
    WF_CHECK_MEM(b, c, (size_t)V + 2);
    wf_free(b, V + 2);
    wf_free(c, V + 2);
    free(m);
}

/* Random heads assembled from grammar fragments, biased toward the
 * constructs the scanners branch on. */
static void head_random(void)
{
    static const char *frag[] = {
        "GET", "POST", "M-SEARCH", " ", "/", "/a?b=c", "HTTP/1.1", "\r\n", "\r\n", "\r\n",
        "Host", "Content-Length", "transfer-encoding", ":", ": ", ":\t", "chunked", "value",
        " \t", "\r", "\n", "\0", "\x7f", "\x80", "\xff", "\"q\"", "(c)", "a,b", "\r\n\r\n",
        "\r\n ", "\r\n\t", "X-0123456789abcdef-long-name", "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv",
    };
    enum { NF = sizeof frag / sizeof frag[0] };
    wf_rng r = wf_rng_from_env();
    long iters = wf_iters(5000);
    char buf[2048];
    for (long it = 0; it < iters; it++) {
        size_t n = 0;
        int pieces = (int)wf_rng_below(&r, 60);
        for (int k = 0; k < pieces; k++) {
            const char *f = frag[wf_rng_below(&r, NF)];
            size_t fl = (f[0] == 0) ? 1 : strlen(f);
            if (n + fl > sizeof buf) break;
            memcpy(buf + n, f, fl);
            n += fl;
        }
        if ((wf_rng_next(&r) & 3) == 0 && n + 4 <= sizeof buf) {
            memcpy(buf + n, "\r\n\r\n", 4);
            n += 4;
        }
        head_run(buf, n);
    }
}

int main(void)
{
    WF_RUN(scan_byte_classes);
    WF_RUN(scan_rfc_sets);
    WF_RUN(scan_exhaustive);
    WF_RUN(scan_gaps);
    WF_RUN(scan_alignments);
    WF_RUN(scan_head_guard);
    WF_RUN(scan_random);
    WF_RUN(scan_len_near_int_max);
    WF_RUN(scan_len_beyond_2gib);
    WF_RUN(lower_all_bytes);
    WF_RUN(lower_lengths);
    WF_RUN(ieq_all_pairs);
    WF_RUN(ieq_lengths);
    WF_RUN(hex_vectors);
    WF_RUN(hex_random);
    WF_RUN(hex_offsets_beyond_2gib);
    WF_RUN(head_real_messages);
    WF_RUN(head_malformed);
    WF_RUN(head_truncations);
    WF_RUN(head_long_lines);
    WF_RUN(head_random);
    return wf_finish("http1-scan");
}
