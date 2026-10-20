/* Unit, boundary and differential tests for wireform-html html_scan.c. */
#include "harness.h"
#include "drive.h"

#define ALL_SCANNERS ((1u << HS_NSCANNERS) - 1)

/* Bytes that stop scanner s (for the tagname scanner, bytes that end the
 * name; for is_all_ascii, high bytes). Returns how many. */
static size_t stop_bytes(const hs_scanner *s, uint8_t out[256])
{
    size_t n = 0;
    for (int v = 0; v < 256; v++) {
        unsigned c = ref_class((uint8_t)v);
        int stops;
        switch (s->kind) {
        case HS_KIND_TAGNAME: stops = !(c & HS_TAGNAME); break;
        case HS_KIND_ASCII: stops = v >= 0x80; break;
        case HS_KIND_SKIP: stops = v == '>'; break;
        default: stops = (c & s->cls) != 0; break;
        }
        if (stops) out[n++] = (uint8_t)v;
    }
    return n;
}

/* Exhaustive over every (length, stop position, scan offset) in a window
 * of a few SIMD blocks plus a tail, for every scanner. With gap 0 the
 * buffer ends flush against a guard page so a read past `end` faults;
 * other gaps move the end off the page boundary and fill the gap with a
 * canary that would change the result if it were read (a stop byte, or a
 * tag-name byte for scan_tagname). Sweeping `len` moves the start
 * alignment, sweeping `off` the scan's. A second stop byte 17 bytes after
 * the first checks that the first one wins across blocks. The filler 'a'
 * is a tag-name byte and stops nothing else. Every position is swept at
 * gap 0; the other gaps take the positions next to the end, the middle
 * and none. */
static void sweep_stop_positions(void)
{
    enum { MAXN = 72, MAXGAP = 48 };
    wf_guarded g = wf_guard_tail(MAXN + MAXGAP);
    uint8_t stops[256];
    for (size_t si = 0; si < HS_NSCANNERS; si++) {
        const hs_scanner *s = &hs_scanners[si];
        size_t nstops = stop_bytes(s, stops);
        const uint8_t fill = 'a';
        for (int gap = 0; gap < MAXGAP; gap++) {
            uint8_t canary = s->kind == HS_KIND_TAGNAME ? 'a' : stops[(size_t)gap % nstops];
            for (int len = 0; len <= MAXN; len++) {
                uint8_t *b = g.p + (g.n - (size_t)len - (size_t)gap);
                memset(b + len, canary, (size_t)gap);
                for (int pos = -1; pos < len; pos++) {
                    if (gap && pos != -1 && pos != len / 2 && pos < len - 2) continue;
                    memset(b, fill, (size_t)len);
                    if (pos >= 0) {
                        b[pos] = stops[(size_t)(pos * 7 + len) % nstops];
                        if (pos + 17 < len) b[pos + 17] = stops[(size_t)pos % nstops];
                    }
                    for (int off = 0; off <= len; off++) {
                        WF_CHECK_EQ(hs_check_one(si, b, off, len), 0);
                    }
                }
            }
        }
    }
    wf_guard_free(g);
}

/* Every byte value through every SIMD lane and the scalar tail: a buffer
 * of all-v, then v alone at each position. Covers 0x00 (the NUL stop and
 * the zero vector), 0x80..0xFF (the movemask sign bit) and the neighbours
 * of every stop byte. */
static void every_byte_value(void)
{
    enum { N = 50 };
    static const int offs[] = { 0, 1, 7, 15, 16, 17, 31, 33, 49, 50 };
    uint8_t tmp[N];
    for (int v = 0; v < 256; v++) {
        memset(tmp, v, N);
        uint8_t *b = wf_dup(tmp, N);
        for (size_t k = 0; k < sizeof offs / sizeof offs[0]; k++) {
            WF_CHECK_EQ(hs_check_all(ALL_SCANNERS, b, offs[k], N), 0);
        }
        wf_free(b, N);
        memset(tmp, 'a', N);
        b = wf_dup(tmp, N);
        for (int pos = 0; pos < N; pos++) {
            b[pos] = (uint8_t)v;
            WF_CHECK_EQ(hs_check_all(ALL_SCANNERS, b, 0, N), 0);
            WF_CHECK_EQ(hs_check_all(ALL_SCANNERS, b, 3, N), 0);
            b[pos] = 'a';
        }
        wf_free(b, N);
    }
}

/* The fused scanners' ASCII flag for every (len, off, stop, high byte):
 * exact in the scalar build, chunk-conservative in the SIMD one, and
 * always sound (see ref_ascii_bound). Gaps after the end hold high bytes
 * or stop bytes, either of which changes the result if read. */
static void fused_ascii_flag(void)
{
    enum { MAXN = 48 };
    static const int gaps[] = { 0, 1, 15, 17 };
    static const uint8_t highs[] = { 0x80, 0xC3, 0xFF };
    static const size_t fused[] = { HS_I_TEXT_ASCII, HS_I_DQ_ASCII, HS_I_SQ_ASCII };
    static const uint8_t stop[] = { '<', '"', '\'' };
    wf_guarded g = wf_guard_tail(MAXN + 17);
    for (size_t f = 0; f < 3; f++) {
        for (size_t gi = 0; gi < sizeof gaps / sizeof gaps[0]; gi++) {
            int gap = gaps[gi];
            for (int len = 0; len <= MAXN; len++) {
                uint8_t *b = g.p + (g.n - (size_t)len - (size_t)gap);
                memset(b + len, gi & 1 ? 0x80 : stop[f], (size_t)gap);
                for (int pos = -1; pos < len; pos++) {
                    for (int hb = -1; hb < len; hb++) {
                        memset(b, 'a', (size_t)len);
                        if (hb >= 0) b[hb] = highs[(size_t)(hb + len) % 3];
                        if (pos >= 0) b[pos] = stop[f];
                        for (int off = 0; off <= len; off++) {
                            WF_CHECK_EQ(hs_check_one(fused[f], b, off, len), 0);
                        }
                    }
                }
            }
        }
    }
    wf_guard_free(g);
}

/* Concrete values of the conservative flag, so a change in either
 * direction is noticed. */
static void fused_ascii_vectors(void)
{
    /* stop at 4, high byte at 10: same 16-byte chunk */
    uint8_t s1[] = "abcd<fghij\xc3\xa9mnopqrstuvwxyz";
    uint8_t *b = wf_dup(s1, sizeof s1 - 1);
    ptrdiff_t r = wireform_scan_text_ascii(b, 0, (ptrdiff_t)sizeof s1 - 1);
    WF_CHECK_EQ(r >> 1, 4);
    WF_CHECK_EQ(r & 1, HTML_SCAN_CHUNK ? 0 : 1);
    /* the same with the stop in the scalar tail: exact */
    r = wireform_scan_text_ascii(b, 0, 9);
    WF_CHECK_EQ(r, (4 << 1) | 1);
    /* high byte before the stop: never ASCII */
    r = wireform_scan_text_ascii(b, 6, (ptrdiff_t)sizeof s1 - 1);
    WF_CHECK_EQ(r, ((ptrdiff_t)sizeof s1 - 1) << 1);
    wf_free(b, sizeof s1 - 1);

    uint8_t s2[] = "class=\"caf\xc3\xa9\" id='x&amp;y'";
    b = wf_dup(s2, sizeof s2 - 1);
    ptrdiff_t n = (ptrdiff_t)sizeof s2 - 1;
    WF_CHECK_EQ(wireform_scan_dquote_ascii(b, 7, n), (12 << 1) | 0);
    WF_CHECK_EQ(wireform_scan_dquote(b, 7, n), 12);
    WF_CHECK_EQ(wireform_scan_squote_ascii(b, 18, n), (19 << 1) | 1);
    WF_CHECK_EQ(wireform_skip_to_squote(b, 18, n), 25);
    WF_CHECK_EQ(wireform_skip_to_dquote(b, 0, n), 6);
    wf_free(b, sizeof s2 - 1);
}

/* is_all_ascii with one byte >= 0x80 (or 0x7F, which is ASCII) at every
 * position, every offset and length, ending flush on the guard page or
 * before a gap of high bytes that would flip the answer if read. */
static void is_all_ascii_sweep(void)
{
    enum { MAXN = 72, MAXGAP = 33 };
    static const uint8_t vals[] = { 0x80, 0xFF, 0x7F, 0xC2 };
    wf_guarded g = wf_guard_tail(MAXN + MAXGAP);
    for (int gap = 0; gap < MAXGAP; gap++) {
        for (int len = 0; len <= MAXN; len++) {
            uint8_t *b = g.p + (g.n - (size_t)len - (size_t)gap);
            memset(b + len, 0x80, (size_t)gap);
            for (int hb = -1; hb < len; hb++) {
                memset(b, 0x7E, (size_t)len);
                if (hb >= 0) b[hb] = vals[(size_t)(hb + len) % 4];
                for (int off = 0; off <= len; off++) {
                    WF_CHECK_EQ(wireform_is_all_ascii(b, off, len), ref_is_all_ascii(b, off, len));
                }
            }
        }
    }
    wf_guard_free(g);
}

/* The scanners only read forward; with the buffer starting at a guard
 * page and off = 0, a read before the start faults. */
static void guard_head(void)
{
    enum { MAXN = 72 };
    wf_rng r = wf_rng_from_env();
    static const uint8_t alpha[] = { 'a', '<', '>', '&', '"', '\'', ' ', '/', 0x00, 0x0D, 0xC3 };
    for (int len = 0; len <= MAXN; len++) {
        wf_guarded g = wf_guard_head((size_t)len);
        wf_rng_fill_from(&r, g.p, (size_t)len, alpha, sizeof alpha);
        WF_CHECK_EQ(hs_check_all(ALL_SCANNERS, g.p, 0, len), 0);
        WF_CHECK_EQ(hs_check_tokenize(g.p, len), 0);
        wf_guard_free(g);
    }
}

static void scanner_vectors(void)
{
    static const struct {
        const char *s;
        size_t si;
        ptrdiff_t off, want;
    } v[] = {
        { "", HS_I_TEXT, 0, 0 },
        { "hello", HS_I_TEXT, 0, 5 },
        { "hello <b>", HS_I_TEXT, 0, 6 },
        { "a &amp; b", HS_I_TEXT, 0, 2 },
        { "line\r\nnext", HS_I_TEXT, 0, 4 },
        { "0123456789abcdef0123456789abcdef<", HS_I_TEXT, 0, 32 },
        { "0123456789abcdef0123456789abcde<", HS_I_TEXT, 1, 31 },
        { "x=\"v\"", HS_I_DQ, 3, 4 },
        { "a&b\"", HS_I_DQ, 0, 1 },
        { "it's", HS_I_SQ, 0, 2 },
        { "a&b\"c", HS_I_SKIP_DQ, 0, 3 }, /* skip_to_dquote ignores & */
        { "a&b'c", HS_I_SKIP_SQ, 0, 3 },
        { "val>", HS_I_UNQ, 0, 3 },
        { "val\fx", HS_I_UNQ, 0, 3 },
        { "v\tx", HS_I_UNQ, 0, 1 },
        { "a&x", HS_I_UNQ, 0, 1 },
        { "</p>rest", HS_I_GT, 0, 4 },
        { "no gt", HS_I_GT, 0, 5 },
        { "a<b>&c", HS_I_TEXT_ESC, 0, 1 },
        { "ab\"c>", HS_I_TEXT_ESC, 0, 4 }, /* text escape ignores " */
        { "ab\"c>", HS_I_ATTR_ESC, 0, 2 },
        { "div class", HS_I_TAGNAME, 0, 3 },
        { "my-el_1:x.y>", HS_I_TAGNAME, 0, 11 },
        { "a<<b c", HS_I_TAGNAME, 0, 4 },
        { "svg\xc3", HS_I_TAGNAME, 0, 3 },
        { "DIV/", HS_I_TAGNAME, 0, 3 },
        { "x", HS_I_TAGNAME, 1, 1 },
    };
    for (size_t i = 0; i < sizeof v / sizeof v[0]; i++) {
        size_t n = strlen(v[i].s);
        uint8_t *b = wf_dup(v[i].s, n);
        ptrdiff_t got = hs_scanners[v[i].si].fn(b, v[i].off, (ptrdiff_t)n);
        if (got != v[i].want) {
            fprintf(stderr, "    vector %zu (%s on \"%s\")\n", i, hs_scanners[v[i].si].name,
                    v[i].s);
        }
        WF_CHECK_EQ(got, v[i].want);
        WF_CHECK_EQ(hs_check_one(v[i].si, b, v[i].off, (ptrdiff_t)n), 0);
        wf_free(b, n);
    }
}

static void skip_attrs_vectors(void)
{
    static const struct {
        const char *s;
        ptrdiff_t off, past;
        int self_close;
    } v[] = {
        { ">", 0, 1, 0 },
        { " class=\"a\">x", 0, 11, 0 },
        { " href=\"x>y\">", 0, 12, 0 },
        { " title='a\"b>'>", 0, 14, 0 },
        { " a=\"'\" b='\"'>", 0, 13, 0 },
        { "/>", 0, 2, 1 },
        { " /  >", 0, 5, 0 },
        { " x=\"/>\"/>", 0, 9, 1 },
        { "/", 0, 2, 0 },            /* '/' as the last byte: unclosed */
        { "", 0, 1, 0 },             /* empty: end + 1 */
        { " a=\"unterminated", 0, 17, 0 },
        { " a='unterminated>", 0, 18, 0 },
        { " a=\"", 0, 5, 0 },        /* quote as the last byte */
        { "<br>", 3, 4, 0 },         /* off == '>' */
        { "<br>", 4, 5, 0 },         /* off == end */
    };
    for (size_t i = 0; i < sizeof v / sizeof v[0]; i++) {
        size_t n = strlen(v[i].s);
        uint8_t *b = wf_dup(v[i].s, n);
        ptrdiff_t got = wireform_skip_attrs(b, v[i].off, (ptrdiff_t)n);
        if (got != ((v[i].past << 1) | v[i].self_close))
            fprintf(stderr, "    vector %zu (\"%s\")\n", i, v[i].s);
        WF_CHECK_EQ(got >> 1, v[i].past);
        WF_CHECK_EQ(got & 1, v[i].self_close);
        WF_CHECK_EQ(got, ref_skip_attrs(b, v[i].off, (ptrdiff_t)n));
        wf_free(b, n);
    }
}

/* Quoted attribute values long enough to take the SIMD path inside
 * skip_attrs, placed at every offset so the closing quote and the '>'
 * cross every block boundary. The tag ends flush at the guard page or
 * before a gap of '>' that would close it if read. */
static void skip_attrs_sweep(void)
{
    enum { MAXN = 90, MAXGAP = 17 };
    wf_guarded g = wf_guard_tail(MAXN + MAXGAP);
    static const char *const tails[] = { ">", "/>", "", "/", "\"", "'" };
    for (int qlen = 0; qlen <= 40; qlen++) {
        for (int lead = 0; lead <= 18; lead++) {
            for (size_t t = 0; t < sizeof tails / sizeof tails[0]; t++) {
                for (int qi = 0; qi < 2; qi++) {
                    char tmp[MAXN + 1];
                    char q = qi ? '\'' : '"';
                    int n = 0;
                    for (int k = 0; k < lead; k++) tmp[n++] = ' ';
                    tmp[n++] = 'a';
                    tmp[n++] = '=';
                    tmp[n++] = q;
                    for (int k = 0; k < qlen; k++) tmp[n++] = (k % 5 == 3) ? (char)(q ^ ('"' ^ '\'')) : (k % 7 == 2 ? '>' : 'v');
                    tmp[n++] = q;
                    size_t tl = strlen(tails[t]);
                    memcpy(tmp + n, tails[t], tl);
                    n += (int)tl;
                    int gap = (qlen + lead + (int)t) % MAXGAP;
                    uint8_t *b = g.p + (g.n - (size_t)n - (size_t)gap);
                    memset(b + n, '>', (size_t)gap);
                    memcpy(b, tmp, (size_t)n);
                    for (int off = 0; off <= n; off++) {
                        WF_CHECK_EQ(hs_check_one(HS_I_SKIP, b, off, n), 0);
                    }
                }
            }
        }
    }
    wf_guard_free(g);
}

/* HTML-ish random text: markup bytes, whitespace, NUL, CR and multi-byte
 * UTF-8, at random lengths, offsets and start alignments, every scanner
 * against its reference, plus the escape loop and the tokenizer walk. */
static size_t html_fill(wf_rng *r, uint8_t *dst, size_t n)
{
    static const char *const atoms[] = {
        "<", ">", "&", "\"", "'", "=", "/", " ", "\t", "\n", "\r", "\f", "", "a",
        "b", "div", "p", "x", "1", "-", "_", ":", ".", "&amp;", "&lt;", "&#38;",
        "&#x3C;", "<a href=\"", "\">", "<br/>", "</p>", "<!--", "-->", "\xc3\xa9",
        "\xe2\x80\x94", "\xf0\x9f\x98\x80", "\x80", "\xff", "\xc3", "class='",
    };
    static const size_t natoms = sizeof atoms / sizeof atoms[0];
    size_t i = 0;
    while (i < n) {
        const char *a = atoms[wf_rng_below(r, natoms)];
        size_t al = a[0] ? strlen(a) : 1; /* the "" atom is one NUL byte */
        if (al > n - i) al = n - i;
        memcpy(dst + i, a, al);
        i += al;
    }
    return i;
}

static void random_differential(void)
{
    enum { MAXN = 400, MAXGAP = 32 };
    wf_rng r = wf_rng_from_env();
    wf_guarded g = wf_guard_tail(MAXN + MAXGAP);
    long iters = wf_iters(6000);
    for (long it = 0; it < iters; it++) {
        int len = (int)wf_rng_below(&r, MAXN + 1);
        /* end flush on the guard page half the time, else before a gap of
         * markup bytes */
        size_t gap = wf_rng_below(&r, 2) ? 0 : wf_rng_below(&r, MAXGAP + 1);
        uint8_t *b = g.p + (g.n - (size_t)len - gap);
        html_fill(&r, b + len, gap);
        if (wf_rng_below(&r, 4) == 0) {
            wf_rng_fill(&r, b, (size_t)len);
        } else {
            html_fill(&r, b, (size_t)len);
        }
        for (int k = 0; k < 4; k++) {
            int off = (int)wf_rng_below(&r, (uint64_t)len + 1);
            WF_CHECK_EQ(hs_check_all(ALL_SCANNERS, b, off, len), 0);
        }
        WF_CHECK_EQ(hs_check_all(ALL_SCANNERS, b, 0, len), 0);
        WF_CHECK_EQ(hs_check_escape(0, b, len), 0);
        WF_CHECK_EQ(hs_check_escape(1, b, len), 0);
        WF_CHECK_EQ(hs_check_tokenize(b, len), 0);
    }
    wf_guard_free(g);
}

/* Constant buffers: all-0x00 (every byte stops scan_text), all-0xFF,
 * all-0x80, all-'<', long runs with no stop at all. */
static void adversarial_fills(void)
{
    static const uint8_t vals[] = { 0x00, 0xFF, 0x80, '<', '"', '\'', '&', '>', 'a', ' ' };
    static const int lens[] = { 0, 1, 15, 16, 17, 31, 32, 33, 255, 256, 4095, 4096, 4097, 65536 };
    for (size_t vi = 0; vi < sizeof vals; vi++) {
        for (size_t li = 0; li < sizeof lens / sizeof lens[0]; li++) {
            int len = lens[li];
            wf_guarded g = wf_guard_tail((size_t)len);
            memset(g.p, vals[vi], (size_t)len);
            WF_CHECK_EQ(hs_check_all(ALL_SCANNERS, g.p, 0, len), 0);
            WF_CHECK_EQ(hs_check_all(ALL_SCANNERS, g.p, len / 2, len), 0);
            WF_CHECK_EQ(hs_check_escape(0, g.p, len), 0);
            WF_CHECK_EQ(hs_check_escape(1, g.p, len), 0);
            WF_CHECK_EQ(hs_check_tokenize(g.p, len), 0);
            wf_guard_free(g);
        }
    }
}

/* Regression: hs_html_find_{text,attr}_escape took and returned int, and
 * Encode.hs passed `fromIntegral len :: CInt`, so a ByteString longer than
 * INT_MAX was scanned with a wrapped (negative or short) length and the
 * Haskell loop then indexed and sliced with a bogus position. The kernels
 * now take ptrdiff_t like the other scanners. A sparse mapping just over
 * 2 GiB is reserved; only the pages around the scanned window are
 * touched. */
static void escape_offsets_beyond_2gib(void)
{
    if (sizeof(ptrdiff_t) < 8) return;
    size_t pg = (size_t)sysconf(_SC_PAGESIZE);
    size_t total = ((size_t)1 << 31) + 4 * pg;
    uint8_t *big = mmap(NULL, total, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if (big == MAP_FAILED) {
        fprintf(stderr, "    escape_offsets_beyond_2gib: mmap failed, skipped\n");
        return;
    }
    ptrdiff_t len = (ptrdiff_t)total;
    ptrdiff_t base = len - 200; /* > INT_MAX */
    memset(big + base, 'a', 200);
    big[base + 37] = '"';
    big[base + 120] = '<';
    WF_CHECK_EQ(hs_html_find_text_escape(big, base, len), base + 120);
    WF_CHECK_EQ(hs_html_find_attr_escape(big, base, len), base + 37);
    WF_CHECK_EQ(hs_html_find_text_escape(big, base + 121, len), len);
    WF_CHECK_EQ(hs_html_find_attr_escape(big, base + 121, len), len);
    /* the ptrdiff_t scanners at the same offsets */
    WF_CHECK_EQ(wireform_scan_text(big, base, len), base + 120);
    WF_CHECK_EQ(wireform_scan_dquote(big, base, len), base + 37);
    WF_CHECK_EQ(wireform_scan_text_ascii(big, base + 121, len), (len << 1) | 1);
    WF_CHECK_EQ(wireform_skip_attrs(big, base + 38, len), (len + 1) << 1);
    WF_CHECK_EQ(hs_check_all(ALL_SCANNERS, big, base, len), 0);
    munmap(big, total);
}

/* Realistic documents through the tokenizer walk, the escapers and every
 * scanner at every offset. */
static void html_documents(void)
{
    /* Lengths from sizeof, so the embedded NUL in the <textarea> is kept. */
#define DOC(s) { s, sizeof(s) - 1 }
    static const struct {
        const char *s;
        size_t n;
    } docs[] = {
        DOC("<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\">"
            "<title>Caf\xc3\xa9 &amp; bar</title></head><body class='main page'>"
            "<p id=x data-v=a&amp;b>Hello &lt;world&gt;</p><br/><img src=\"a.png\" alt=''>"
            "<a href=\"/q?a=1&b=2\" title='it&#39;s'>link</a></body></html>"),
        DOC("<svg viewBox=\"0 0 10 10\"><path d=\"M0 0L10 10\"/></svg>\r\n"),
        DOC("<script>if (a < b && c > d) {}</script><style>a>b{}</style>"),
        DOC("<input disabled value=\"x\" / ><textarea>\x00</textarea>"),
        DOC("<div\n\tclass\n=\n\"a\"\f>\xe2\x80\x94</div   >"),
        DOC("<p>unterminated <a href=\"never"),
        DOC("<<<<>>>>&&&&\"\"\"''''"),
    };
#undef DOC
    for (size_t d = 0; d < sizeof docs / sizeof docs[0]; d++) {
        size_t n = docs[d].n;
        uint8_t *b = wf_dup(docs[d].s, n);
        WF_CHECK_EQ(hs_check_tokenize(b, (ptrdiff_t)n), 0);
        WF_CHECK_EQ(hs_check_escape(0, b, (ptrdiff_t)n), 0);
        WF_CHECK_EQ(hs_check_escape(1, b, (ptrdiff_t)n), 0);
        for (size_t off = 0; off <= n; off++) {
            WF_CHECK_EQ(hs_check_all(ALL_SCANNERS, b, (ptrdiff_t)off, (ptrdiff_t)n), 0);
        }
        wf_free(b, n);
    }
}

int main(void)
{
    (void)wf_rng_from_env();
    WF_RUN(scanner_vectors);
    WF_RUN(skip_attrs_vectors);
    WF_RUN(fused_ascii_vectors);
    WF_RUN(sweep_stop_positions);
    WF_RUN(every_byte_value);
    WF_RUN(fused_ascii_flag);
    WF_RUN(is_all_ascii_sweep);
    WF_RUN(guard_head);
    WF_RUN(skip_attrs_sweep);
    WF_RUN(adversarial_fills);
    WF_RUN(html_documents);
    WF_RUN(escape_offsets_beyond_2gib);
    WF_RUN(random_differential);
    return wf_finish("html-scan");
}
