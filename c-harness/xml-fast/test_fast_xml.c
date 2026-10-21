/* Unit, boundary and differential tests for wireform-xml fast_xml.c. */
#include "harness.h"
#include "api.h"

#include <limits.h>

typedef ptrdiff_t (*scan_fn)(const uint8_t *, ptrdiff_t, ptrdiff_t);

/* Thin wrappers so the target-byte scanners fit the (buf, offset, len)
 * table; the target is a global the sweeps set. */
static uint8_t g_target;
static ptrdiff_t k_find_byte(const uint8_t *b, ptrdiff_t o, ptrdiff_t n) { return hs_xml_find_byte(b, o, n, g_target); }
static ptrdiff_t r_find_byte(const uint8_t *b, ptrdiff_t o, ptrdiff_t n) { return ref_xml_find_byte(b, o, n, g_target); }
static ptrdiff_t k_attr_end(const uint8_t *b, ptrdiff_t o, ptrdiff_t n) { return hs_xml_find_attr_end(b, o, n, g_target); }

typedef struct {
    const char *name;
    scan_fn kernel;
    scan_fn ref;
} scanner;

static const scanner SCANNERS[] = {
    { "find_lt", hs_xml_find_lt, ref_xml_find_lt },
    { "find_byte", k_find_byte, r_find_byte },
    { "find_attr_end", k_attr_end, r_find_byte },
    { "find_text_end", hs_xml_find_text_end, ref_xml_find_text_end },
    { "html_find_text_end", hs_html_find_text_end, ref_html_find_text_end },
    { "find_cdata_end", hs_xml_find_cdata_end, ref_xml_find_cdata_end },
    { "find_comment_end", hs_xml_find_comment_end, ref_xml_find_comment_end },
};
enum { NSCANNERS = sizeof SCANNERS / sizeof SCANNERS[0] };

static int takes_target(const scanner *s)
{
    return s->kernel == k_find_byte || s->kernel == k_attr_end;
}

static const uint8_t TARGETS[] = { '"', '\'', '>', ';', 0x00, 0x80, 0xff };
/* The exhaustive sweep uses only the first two targets to stay fast; the
 * random and document tests use all of them. */
static size_t g_ntargets = sizeof TARGETS;

/* Bytes written after the buffer's end (inside the mapping, outside
 * [0, len)) so that a kernel reading past len returns a wrong index
 * rather than silently agreeing with the reference. */
static const uint8_t POISON[] = "]]>--><&\0\r\"'/=>;\x80\xff]]>-->";

static ptrdiff_t check_scanner_at(const scanner *s, const uint8_t *b, ptrdiff_t off, ptrdiff_t len)
{
    ptrdiff_t got = s->kernel(b, off, len);
    ptrdiff_t want = s->ref(b, off, len);
    if (got != want) {
        fprintf(stderr, "    %s(off=%td, len=%td, target=0x%02x): got %td want %td\n", s->name,
                off, len, g_target, got, want);
        if (len < 4096) wf_hexdump("buf", b, (size_t)len);
    }
    WF_CHECK_EQ(got, want);
    return got;
}

/* Every scanner at every offset in [0, len + 2], plus find_escape over
 * [off, len) and [off, off + (len - off) / 2) for every off. */
static void check_all_offsets(const uint8_t *b, int len)
{
    for (size_t si = 0; si < NSCANNERS; si++) {
        const scanner *s = &SCANNERS[si];
        size_t nt = takes_target(s) ? g_ntargets : 1;
        for (size_t ti = 0; ti < nt; ti++) {
            g_target = TARGETS[ti];
            for (int off = 0; off <= len + 2; off++) check_scanner_at(s, b, off, len);
        }
    }
    for (int off = 0; off <= len; off++) {
        WF_CHECK_EQ(hs_xml_find_escape(b, off, len - off), ref_xml_find_escape(b, off, len - off));
        int half = (len - off) / 2;
        WF_CHECK_EQ(hs_xml_find_escape(b, off, half), ref_xml_find_escape(b, off, half));
    }
}

static const char *const TOKENS[] = {
    "<", "&", ">", "\"", "'", ";", "/", "=", "\0", "\r", "\x80", "\xff",
    "]]>", "-->", "]]", "--", "]>", "->", "]]]>", "--->", "]-]>", "-]->",
};
static const size_t TOKEN_LEN[] = {
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4,
};

/* Exhaustive over (skew, length, filler, token, position, offset) in a
 * window covering the scalar alignment lead-in, two SIMD blocks and the
 * scalar tail. The token may straddle the end of the buffer (it is
 * truncated there), which is where the three-byte terminators are
 * easiest to get wrong. skew puts `skew` POISON bytes between the buffer
 * end and the guard page, decorrelating the buffer's start alignment
 * from its length; with skew 0 any overread faults. */
static void scanners_exhaustive_small(void)
{
    enum { MAXN = 56, MAXSKEW = 16, SKEWN = 34 };
    static const uint8_t fillers[] = { 'a', ']', '-' };
    wf_guarded g = wf_guard_tail(MAXN + MAXSKEW);
    g_ntargets = 2;
    for (int skew = 0; skew < MAXSKEW; skew++) {
        int maxn = skew == 0 ? MAXN : SKEWN;
        size_t nfill = skew == 0 ? sizeof fillers : 1;
        for (int len = 0; len <= maxn; len++) {
            uint8_t *b = g.p + g.n - (size_t)skew - (size_t)len;
            memcpy(b + len, POISON, (size_t)skew);
            for (size_t fi = 0; fi < nfill; fi++) {
                memset(b, fillers[fi], (size_t)len);
                check_all_offsets(b, len);
                for (size_t ti = 0; ti < sizeof TOKENS / sizeof TOKENS[0]; ti++) {
                    for (int pos = 0; pos < len; pos++) {
                        size_t n = TOKEN_LEN[ti];
                        if (n > (size_t)(len - pos)) n = (size_t)(len - pos);
                        memcpy(b + pos, TOKENS[ti], n);
                        check_all_offsets(b, len);
                        memset(b + pos, fillers[fi], n);
                    }
                }
            }
        }
    }
    g_ntargets = sizeof TARGETS;
    wf_guard_free(g);
}

/* A buffer that starts exactly at a guard page: offset 0 is the first
 * readable byte, so a scanner that reads before `offset` faults. */
static void scanners_head_guard(void)
{
    enum { MAXN = 72 };
    for (int len = 0; len <= MAXN; len++) {
        wf_guarded g = wf_guard_head((size_t)len);
        memset(g.p, 'x', (size_t)len);
        if (len >= 3) memcpy(g.p + len - 3, "]]>", 3);
        check_all_offsets(g.p, len);
        if (len >= 3) memcpy(g.p + len - 3, "-->", 3);
        check_all_offsets(g.p, len);
        wf_guard_free(g);
    }
}

/* Many terminator candidates in one SIMD block: the ']' / '-' bitmask
 * loop must skip the false candidates and return the first real one,
 * including candidates whose confirming bytes sit in the next block. */
static void terminators_dense_candidates(void)
{
    enum { N = 96 };
    for (int pos = 0; pos + 3 <= N - 1; pos++) {
        uint8_t *b = wf_alloc(N);
        memset(b, ']', N);
        b[N - 1] = 'x';
        b[pos + 2] = '>';
        WF_CHECK_EQ(hs_xml_find_cdata_end(b, 0, N), pos);
        for (int off = 0; off <= N; off++) {
            WF_CHECK_EQ(hs_xml_find_cdata_end(b, off, N), ref_xml_find_cdata_end(b, off, N));
        }
        memset(b, '-', N);
        b[N - 1] = 'x';
        b[pos + 2] = '>';
        WF_CHECK_EQ(hs_xml_find_comment_end(b, 0, N), pos);
        for (int off = 0; off <= N; off++) {
            WF_CHECK_EQ(hs_xml_find_comment_end(b, off, N), ref_xml_find_comment_end(b, off, N));
        }
        wf_free(b, N);
    }
    /* All ']' / all '-' up to the very end: no match, no read past len. */
    for (int len = 1; len <= 64; len++) {
        uint8_t *b = wf_alloc((size_t)len);
        memset(b, ']', (size_t)len);
        WF_CHECK_EQ(hs_xml_find_cdata_end(b, 0, len), -1);
        memset(b, '-', (size_t)len);
        WF_CHECK_EQ(hs_xml_find_comment_end(b, 0, len), -1);
        wf_free(b, (size_t)len);
    }
}

/* Every byte value as find_byte's target, including 0x00 and 0x80+
 * (sign bit of the movemask lanes). */
static void find_byte_all_values(void)
{
    enum { N = 64 };
    for (int v = 0; v < 256; v++) {
        uint8_t tmp[N];
        for (int i = 0; i < N; i++) {
            tmp[i] = (uint8_t)(v + 1 + i % 7);
            if (tmp[i] == v) tmp[i] = (uint8_t)(v + 1);
        }
        uint8_t *b = wf_dup(tmp, N);
        WF_CHECK_EQ(hs_xml_find_byte(b, 0, N, (uint8_t)v), -1);
        WF_CHECK_EQ(hs_xml_find_attr_end(b, 0, N, (uint8_t)v), -1);
        b[17] = b[19] = b[63] = (uint8_t)v;
        WF_CHECK_EQ(hs_xml_find_byte(b, 0, N, (uint8_t)v), 17);
        WF_CHECK_EQ(hs_xml_find_byte(b, 18, N, (uint8_t)v), 19);
        WF_CHECK_EQ(hs_xml_find_attr_end(b, 20, N, (uint8_t)v), 63);
        WF_CHECK_EQ(hs_xml_find_byte(b, 20, 63, (uint8_t)v), -1);
        wf_free(b, N);
    }
}

/* Empty and past-the-end ranges return the miss value and read nothing. */
static void empty_ranges(void)
{
    wf_guarded g = wf_guard_tail(0);
    for (int off = 0; off < 4; off++) {
        WF_CHECK_EQ(hs_xml_find_lt(g.p, off, 0), -1);
        WF_CHECK_EQ(hs_xml_find_byte(g.p, off, 0, 0), -1);
        WF_CHECK_EQ(hs_xml_find_attr_end(g.p, off, 0, '"'), -1);
        WF_CHECK_EQ(hs_xml_find_text_end(g.p, off, 0), 0);
        WF_CHECK_EQ(hs_html_find_text_end(g.p, off, 0), 0);
        WF_CHECK_EQ(hs_xml_find_cdata_end(g.p, off, 0), -1);
        WF_CHECK_EQ(hs_xml_find_comment_end(g.p, off, 0), -1);
    }
    WF_CHECK_EQ(hs_xml_find_escape(g.p, 0, 0), 0);
    size_t idx[1] = { 0xdeadbeef };
    WF_CHECK_EQ(hs_xml_scan_structural(g.p, 0, idx, 1), 0);
    WF_CHECK_EQ(idx[0], 0xdeadbeef);
    wf_guard_free(g);
    /* max_indices == 0 writes nothing even when matches exist. */
    uint8_t lt[40];
    memset(lt, '<', sizeof lt);
    uint8_t *b = wf_dup(lt, sizeof lt);
    WF_CHECK_EQ(hs_xml_scan_structural(b, sizeof lt, idx, 0), 0);
    WF_CHECK_EQ(hs_xml_scan_structural(b, 0, idx, 1), 0);
    WF_CHECK_EQ(idx[0], 0xdeadbeef);
    wf_free(b, sizeof lt);
}

/* hs_xml_scan_structural against the reference for every length, start
 * alignment and max_indices limit, with the index array exact-size so a
 * write past max_indices faults. */
static void structural_exhaustive(void)
{
    enum { MAXN = 80, MAXSKEW = 16 };
    wf_rng r = wf_rng_from_env();
    static const uint8_t alpha[] = { '<', '>', '/', '=', '"', '\'', '&', 'a', ' ', 0x80, 0x00 };
    wf_guarded g = wf_guard_tail(MAXN + MAXSKEW);
    size_t want[MAXN + 1];
    for (int skew = 0; skew < MAXSKEW; skew++) {
        for (int len = 0; len <= MAXN; len++) {
            uint8_t *b = g.p + g.n - (size_t)skew - (size_t)len;
            memcpy(b + len, POISON, (size_t)skew);
            for (int rep = 0; rep < 3; rep++) {
                if (rep == 0) memset(b, '<', (size_t)len);
                else if (rep == 1) memset(b, 'a', (size_t)len);
                else wf_rng_fill_from(&r, b, (size_t)len, alpha, sizeof alpha);
                size_t total = ref_xml_scan_structural(b, (size_t)len, want, MAXN + 1);
                long maxes[] = { 0, 1, 2, (long)total - 1, (long)total, (long)total + 1,
                                 (long)len + 1, 15, 16, 17 };
                for (size_t mi = 0; mi < sizeof maxes / sizeof maxes[0]; mi++) {
                    long max = maxes[mi];
                    if (max < 0) continue;
                    idx_buf ib = idx_alloc((size_t)max);
                    size_t *idx = ib.p;
                    size_t got = hs_xml_scan_structural(b, (size_t)len, idx, (size_t)max);
                    size_t exp = total < (size_t)max ? total : (size_t)max;
                    WF_CHECK_EQ(got, exp);
                    WF_CHECK_MEM(idx, want, exp * sizeof(size_t));
                    idx_free(ib);
                }
            }
        }
    }
    wf_guard_free(g);
}

/* Walk every match of every scanner from the start of the document, as
 * the SAX / DOM parsers do (resume at the hit + 1). */
static void walk_document(const uint8_t *src, int len)
{
    uint8_t *b = wf_dup(src, (size_t)len);
    for (size_t si = 0; si < NSCANNERS; si++) {
        const scanner *s = &SCANNERS[si];
        size_t nt = takes_target(s) ? sizeof TARGETS : 1;
        for (size_t ti = 0; ti < nt; ti++) {
            g_target = TARGETS[ti];
            ptrdiff_t off = 0;
            while (off <= len) {
                ptrdiff_t got = check_scanner_at(s, b, off, len);
                if (got < 0 || got >= len) break;
                off = got + 1;
            }
        }
    }
    for (ptrdiff_t off = 0; off <= len;) {
        ptrdiff_t got = hs_xml_find_escape(b, off, len - off);
        WF_CHECK_EQ(got, ref_xml_find_escape(b, off, len - off));
        off = got + 1;
    }
    size_t *idx = malloc(((size_t)len + 1) * sizeof(size_t));
    size_t *want = malloc(((size_t)len + 1) * sizeof(size_t));
    size_t n = hs_xml_scan_structural(b, (size_t)len, idx, (size_t)len + 1);
    WF_CHECK_EQ(n, ref_xml_scan_structural(b, (size_t)len, want, (size_t)len + 1));
    WF_CHECK_MEM(idx, want, n * sizeof(size_t));
    free(idx);
    free(want);
    wf_free(b, (size_t)len);
}

static const char DOC_FEED[] =
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
    "<feed xmlns=\"http://www.w3.org/2005/Atom\" xml:lang='en'>\n"
    "  <!-- generated -- by a tool -->\n"
    "  <title type=\"html\">Tom &amp; Jerry&#x27;s &lt;blog&gt;</title>\n"
    "  <entry><id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>\n"
    "    <content><![CDATA[if (a[b[0]] > 1 && x-->0) { ]] > ] ]> }]]></content>\n"
    "    <link rel=\"alternate\" href=\"http://example.org/2003/12/13/atom03?a=1&amp;b=2\"/>\n"
    "  </entry>\n"
    "</feed>\n";

static const char DOC_SOAP[] =
    "<soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\">"
    "<soap:Body><m:GetPrice xmlns:m=\"https://www.w3schools.com/prices\">"
    "<m:Item attr='it&apos;s \"quoted\"'>Apples\r\n</m:Item></m:GetPrice>"
    "</soap:Body></soap:Envelope>";

static void documents(void)
{
    walk_document((const uint8_t *)DOC_FEED, (int)sizeof DOC_FEED - 1);
    walk_document((const uint8_t *)DOC_SOAP, (int)sizeof DOC_SOAP - 1);
    /* Every third suffix and prefix: shifts both start alignment and end. */
    for (int k = 0; k < (int)sizeof DOC_FEED - 1; k += 3) {
        walk_document((const uint8_t *)DOC_FEED + k, (int)sizeof DOC_FEED - 1 - k);
        walk_document((const uint8_t *)DOC_FEED, k);
    }
}

static void scanners_random(void)
{
    wf_rng r = wf_rng_from_env();
    static const uint8_t alpha[] = { '<', '&', ']', ']', '>', '-', '-', '"', '\'', 'a', 0x00, '\r', 0x80, 0xff };
    long iters = wf_iters(3000);
    for (long it = 0; it < iters; it++) {
        int len = (int)wf_rng_below(&r, 400);
        uint8_t *tmp = malloc((size_t)len + 1);
        wf_rng_fill_from(&r, tmp, (size_t)len, alpha, sizeof alpha);
        uint8_t *b = wf_dup(tmp, (size_t)len);
        for (int k = 0; k < 8; k++) {
            int off = (int)wf_rng_below(&r, (uint64_t)len + 3);
            for (size_t si = 0; si < NSCANNERS; si++) {
                g_target = alpha[wf_rng_below(&r, sizeof alpha)];
                check_scanner_at(&SCANNERS[si], b, off, len);
            }
            if (off <= len) {
                int n = (int)wf_rng_below(&r, (uint64_t)(len - off) + 1);
                WF_CHECK_EQ(hs_xml_find_escape(b, off, n), ref_xml_find_escape(b, off, n));
            }
        }
        wf_free(b, (size_t)len);
        free(tmp);
    }
}

/* Regression: index arithmetic at len == INT_MAX.
 *
 * len is the CInt-truncated length of the document ByteString, so a
 * document of INT_MAX bytes gives len == INT_MAX, and parser offsets run
 * up to len. The kernels used to test `i + 16 <= len` and
 * `i + 2 < len` / `pos + 2 < len`, which overflow int (UB, caught by
 * UBSan's signed-integer-overflow check) once i > INT_MAX - 16.
 *
 * The tests simulate the last d bytes of an INT_MAX-byte document:
 * `virt` is a base pointer such that virt + (INT_MAX - d) is a real
 * exact-size buffer of d bytes, and only indices >= INT_MAX - d are
 * touched (by kernel and reference alike). d == 0 is the parser calling
 * at EOF (offset == len == INT_MAX), which must read nothing. */
static const uint8_t *virtual_base(const uint8_t *real, int d)
{
    return (const uint8_t *)((uintptr_t)real - (uintptr_t)(INT_MAX - d));
}

static void regression_int_max_offsets(void)
{
    static const uint8_t pat[] = "ab]]>c--><&\r\0\"'";
    for (int d = 0; d <= 20; d++) {
        uint8_t *real = wf_alloc((size_t)d);
        for (int k = 0; k < d; k++) real[k] = pat[k % (sizeof pat - 1)];
        const uint8_t *virt = virtual_base(real, d);
        for (size_t si = 0; si < NSCANNERS; si++) {
            g_target = '"';
            check_scanner_at(&SCANNERS[si], virt, INT_MAX - d, INT_MAX);
        }
        WF_CHECK_EQ(hs_xml_find_escape(virt, INT_MAX - d, d),
                    ref_xml_find_escape(virt, INT_MAX - d, d));
        wf_free(real, (size_t)d);
    }
}

static void regression_int_max_tail(void)
{
    enum { N = 70 };
    wf_rng r = wf_rng_from_env();
    static const uint8_t alpha[] = { ']', ']', '>', '-', '-', '<', '&', 'z', '"' };
    for (int skew = 0; skew < 16; skew++) {
        int n = N - skew;
        uint8_t *real = wf_alloc((size_t)n);
        const uint8_t *virt = virtual_base(real, n);
        for (int rep = 0; rep < 20; rep++) {
            if (rep == 0) memset(real, 'a', (size_t)n);
            else wf_rng_fill_from(&r, real, (size_t)n, alpha, sizeof alpha);
            for (int d = 0; d <= n; d++) {
                for (size_t si = 0; si < NSCANNERS; si++) {
                    g_target = '"';
                    check_scanner_at(&SCANNERS[si], virt, INT_MAX - d, INT_MAX);
                }
            }
        }
        wf_free(real, (size_t)n);
    }
}

/* The same overflow in hs_xml_scan_structural, which always scans from
 * index 0 and so needs a real INT_MAX-byte readable buffer. It is built
 * by mapping one 1 MiB file over and over (2 GiB of address space, one
 * MiB of memory) and is opt-in (WF_BIG=1) because the scan takes seconds
 * in the instrumented builds. */
static void regression_structural_int_max(void)
{
    const char *big = getenv("WF_BIG");
    if (!big || strcmp(big, "1") != 0) return;
    const size_t chunk = (size_t)1 << 20;
    const size_t total = ((size_t)INT_MAX / chunk + 1) * chunk; /* 2 GiB */
    char path[] = "/tmp/wf-xml-big-XXXXXX";
    int fd = mkstemp(path);
    if (fd < 0) {
        WF_CHECK(fd >= 0);
        return;
    }
    unlink(path);
    WF_CHECK(ftruncate(fd, (off_t)chunk) == 0);
    uint8_t *w = mmap(NULL, chunk, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
    if (w == MAP_FAILED) abort();
    memset(w, 'a', chunk);
    w[chunk - 2] = '='; /* in the last chunk: index INT_MAX - 1, the last document byte */
    w[chunk - 1] = '<'; /* in the last chunk: index INT_MAX, one past the end */
    munmap(w, chunk);
    uint8_t *base = mmap(NULL, total, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if (base == MAP_FAILED) abort();
    for (size_t o = 0; o < total; o += chunk) {
        if (mmap(base + o, chunk, PROT_READ, MAP_SHARED | MAP_FIXED, fd, (off_t)0) != base + o) abort();
    }
    close(fd);
    size_t nchunks = total / chunk;
    size_t max = 2 * nchunks;
    size_t *idx = malloc(max * sizeof(size_t));
    size_t got = hs_xml_scan_structural(base, INT_MAX, idx, max);
    WF_CHECK_EQ(got, 2 * nchunks - 1);
    if (got > 0) WF_CHECK_EQ(idx[got - 1], (size_t)INT_MAX - 1);
    free(idx);
    ptrdiff_t from = (ptrdiff_t)(total - 2 * chunk);
    WF_CHECK_EQ(hs_xml_find_cdata_end(base, from, INT_MAX), -1);
    WF_CHECK_EQ(hs_xml_find_comment_end(base, from, INT_MAX), -1);
    WF_CHECK_EQ(hs_xml_find_text_end(base, from, INT_MAX), (ptrdiff_t)(total - chunk - 1));
    WF_CHECK_EQ(hs_xml_find_byte(base, from, INT_MAX, 'z'), -1);
    munmap(base, total);
}

/* Regression: positions and lengths above INT_MAX.
 *
 * The scanners used to take and return int, so any offset or document
 * length past INT_MAX wrapped. A sparse mapping just over 2 GiB is
 * reserved and only the last 200 bytes are touched, so every call below
 * runs at an offset and with a len that do not fit in an int, and the
 * expected results do not fit either. Skipped if the address space is
 * unavailable (the mapping is PROT_READ|WRITE but never touched, so no
 * memory is actually committed). */
static void scanners_beyond_2gib(void)
{
    if (sizeof(ptrdiff_t) < 8) return;
    long pg = sysconf(_SC_PAGESIZE);
    if (pg <= 0) pg = 4096;
    size_t total = ((size_t)1 << 31) + 4 * (size_t)pg;
    uint8_t *big = mmap(NULL, total, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if (big == MAP_FAILED) {
        fprintf(stderr, "    scanners_beyond_2gib: mmap failed, skipped\n");
        return;
    }
    ptrdiff_t len = (ptrdiff_t)total;
    ptrdiff_t base = len - 200; /* > INT_MAX */
    memset(big + base, 'a', 200);
    big[base + 3] = '<';
    big[base + 8] = '&';
    big[base + 13] = '"';
    big[base + 21] = ']';
    big[base + 22] = ']';
    big[base + 23] = '>';
    big[base + 31] = '-';
    big[base + 32] = '-';
    big[base + 33] = '>';
    big[base + 40] = '\r';

    WF_CHECK(base > (ptrdiff_t)INT_MAX);
    WF_CHECK_EQ(hs_xml_find_lt(big, base, len), base + 3);
    WF_CHECK_EQ(hs_xml_find_byte(big, base, len, '"'), base + 13);
    WF_CHECK_EQ(hs_xml_find_attr_end(big, base, len, '"'), base + 13);
    WF_CHECK_EQ(hs_xml_find_text_end(big, base, len), base + 3);
    WF_CHECK_EQ(hs_html_find_text_end(big, base, len), base + 3);
    WF_CHECK_EQ(hs_xml_find_cdata_end(big, base, len), base + 21);
    WF_CHECK_EQ(hs_xml_find_comment_end(big, base, len), base + 31);
    /* find_escape takes a count: from just past the '&' up to len. */
    WF_CHECK_EQ(hs_xml_find_escape(big, base + 9, len - (base + 9)), base + 13);
    WF_CHECK_EQ(hs_xml_find_escape(big, base + 41, len - (base + 41)), len);
    /* Everything after the last marker, and offset > len at EOF. */
    WF_CHECK_EQ(hs_xml_find_lt(big, base + 42, len), -1);
    WF_CHECK_EQ(hs_xml_find_cdata_end(big, base + 42, len), -1);
    WF_CHECK_EQ(hs_xml_find_text_end(big, base + 42, len), len);
    WF_CHECK_EQ(hs_xml_find_lt(big, len + 5, len), -1);
    WF_CHECK_EQ(hs_xml_find_byte(big, len, len, '"'), -1);
    /* Whole scanner table, differential, above 2 GiB. */
    for (size_t si = 0; si < NSCANNERS; si++) {
        g_target = '"';
        for (ptrdiff_t off = base; off <= base + 45; off++) {
            check_scanner_at(&SCANNERS[si], big, off, len);
        }
    }
    munmap(big, total);
}

int main(void)
{
    (void)wf_rng_from_env();
    WF_RUN(empty_ranges);
    WF_RUN(find_byte_all_values);
    WF_RUN(terminators_dense_candidates);
    WF_RUN(scanners_head_guard);
    WF_RUN(scanners_exhaustive_small);
    WF_RUN(structural_exhaustive);
    WF_RUN(documents);
    WF_RUN(scanners_random);
    WF_RUN(regression_int_max_offsets);
    WF_RUN(regression_int_max_tail);
    WF_RUN(regression_structural_int_max);
    WF_RUN(scanners_beyond_2gib);
    return wf_finish("xml-fast");
}
