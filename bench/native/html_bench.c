/*
 * html_bench.c — lexbor comparison benchmarks
 *
 * Generates the same ~29KB "mediumHTML" document used by HTMLBench.hs
 * and measures lexbor's throughput for parsing, tree building, and
 * CSS selector matching.  Results are printed in a format that lines
 * up with the Haskell benchmark output for easy comparison.
 *
 * Build:  make            (uses nix-shell to get lexbor)
 * Run:    ./html_bench
 */

#include <lexbor/html/html.h>
#include <lexbor/css/css.h>
#include <lexbor/selectors/selectors.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <stdbool.h>

/* -------------------------------------------------------------------
 * Timing
 * ------------------------------------------------------------------- */

static double now_sec(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (double)ts.tv_sec + (double)ts.tv_nsec / 1e9;
}

/* -------------------------------------------------------------------
 * Generate the same mediumHTML as HTMLBench.hs
 * ------------------------------------------------------------------- */

static char *gen_medium_html(size_t *out_len) {
    size_t cap = 64 * 1024;
    char *buf = malloc(cap);
    size_t len = 0;

#define APPEND(s) do { \
    size_t _n = strlen(s); \
    while (len + _n >= cap) { cap *= 2; buf = realloc(buf, cap); } \
    memcpy(buf + len, s, _n); len += _n; \
} while (0)

#define APPENDF(...) do { \
    char _tmp[512]; \
    int _n = snprintf(_tmp, sizeof _tmp, __VA_ARGS__); \
    while (len + (size_t)_n >= cap) { cap *= 2; buf = realloc(buf, cap); } \
    memcpy(buf + len, _tmp, _n); len += _n; \
} while (0)

    APPEND("<html><body><div class=\"catalog\">\n");
    for (int i = 1; i <= 100; i++) {
        APPENDF("  <div class=\"item\" id=\"i%d\">\n", i);
        APPENDF("    <span class=\"name\">Product %d</span>\n", i);
        APPENDF("    <span class=\"price\">%g</span>\n", (double)i * 9.99);
        APPENDF("    <p class=\"description\">This is the description for "
                 "product number %d in our catalog</p>\n", i);
        APPENDF("    <span class=\"category\">Category %d</span>\n", i % 10);
        APPENDF("    <span class=\"inStock\">%s</span>\n",
                 (i % 2 == 0) ? "true" : "false");
        APPEND("  </div>\n");
    }
    APPEND("</div></body></html>\n");

#undef APPEND
#undef APPENDF

    buf[len] = '\0';
    *out_len = len;
    return buf;
}

/* -------------------------------------------------------------------
 * Benchmark harness
 * ------------------------------------------------------------------- */

typedef struct {
    double mbps;
    double sec;
} bench_result_t;

typedef void (*bench_fn)(const lxb_char_t *html, size_t html_len, void *ctx);

static bench_result_t bench(const char *label, int iters, size_t input_size,
                            bench_fn fn, const lxb_char_t *html, size_t html_len,
                            void *ctx) {
    /* warm up */
    for (int i = 0; i < 10; i++) fn(html, html_len, ctx);

    double t0 = now_sec();
    for (int i = 0; i < iters; i++) {
        fn(html, html_len, ctx);
    }
    double t1 = now_sec();
    double elapsed = t1 - t0;
    double total_bytes = (double)input_size * (double)iters;
    double mbps = total_bytes / (elapsed * 1e6);

    bench_result_t r = { .mbps = mbps, .sec = elapsed };
    return r;
}

static void print_result(const char *label, bench_result_t r) {
    printf("  %-45s %8.0f MB/s   (%6.0f ms)\n", label, r.mbps, r.sec * 1000.0);
}

/* -------------------------------------------------------------------
 * Benchmark: document parse (one-shot)
 * ------------------------------------------------------------------- */

static void bench_parse_oneshot(const lxb_char_t *html, size_t html_len,
                                void *ctx) {
    (void)ctx;
    lxb_html_document_t *doc = lxb_html_document_create();
    lxb_html_document_parse(doc, html, html_len);
    lxb_html_document_destroy(doc);
}

/* -------------------------------------------------------------------
 * Benchmark: document parse with parser reuse
 * ------------------------------------------------------------------- */

static void bench_parse_reuse(const lxb_char_t *html, size_t html_len,
                              void *ctx) {
    lxb_html_parser_t *parser = (lxb_html_parser_t *)ctx;
    lxb_html_document_t *doc = lxb_html_parse(parser, html, html_len);
    lxb_html_document_destroy(doc);
    lxb_html_parser_clean(parser);
}

/* -------------------------------------------------------------------
 * Benchmark: document parse (incremental, 4KB chunks)
 * ------------------------------------------------------------------- */

static void bench_parse_incremental(const lxb_char_t *html, size_t html_len,
                                    void *ctx) {
    (void)ctx;
    lxb_html_document_t *doc = lxb_html_document_create();
    lxb_html_document_parse_chunk_begin(doc);

    size_t off = 0;
    while (off < html_len) {
        size_t chunk = html_len - off;
        if (chunk > 4096) chunk = 4096;
        lxb_html_document_parse_chunk(doc, html + off, chunk);
        off += chunk;
    }

    lxb_html_document_parse_chunk_end(doc);
    lxb_html_document_destroy(doc);
}

/* -------------------------------------------------------------------
 * Benchmark: querySelectorAll via lxb_selectors_find
 * ------------------------------------------------------------------- */

typedef struct {
    lxb_css_parser_t *css_parser;
    lxb_selectors_t *selectors;
    lxb_css_selector_list_t *sel_list;
    int count;
} qsa_ctx_t;

static lxb_status_t qsa_count_cb(lxb_dom_node_t *node,
                                 lxb_css_selector_specificity_t spec,
                                 void *ctx) {
    (void)node; (void)spec;
    qsa_ctx_t *qc = (qsa_ctx_t *)ctx;
    qc->count++;
    return LXB_STATUS_OK;
}

static void bench_qsa(const lxb_char_t *html, size_t html_len, void *ctx) {
    qsa_ctx_t *qc = (qsa_ctx_t *)ctx;
    qc->count = 0;

    lxb_html_document_t *doc = lxb_html_document_create();
    lxb_html_document_parse(doc, html, html_len);

    lxb_selectors_find(qc->selectors,
                       lxb_dom_interface_node(doc),
                       qc->sel_list, qsa_count_cb, qc);

    lxb_html_document_destroy(doc);
}

/* Variant: pre-parsed document, selector matching only */
typedef struct {
    lxb_html_document_t *doc;
    lxb_selectors_t *selectors;
    lxb_css_selector_list_t *sel_list;
    int count;
} qsa_preparse_ctx_t;

static lxb_status_t qsa_preparse_cb(lxb_dom_node_t *node,
                                    lxb_css_selector_specificity_t spec,
                                    void *ctx) {
    (void)node; (void)spec;
    qsa_preparse_ctx_t *qc = (qsa_preparse_ctx_t *)ctx;
    qc->count++;
    return LXB_STATUS_OK;
}

static void bench_qsa_preparse(const lxb_char_t *html, size_t html_len,
                               void *ctx) {
    (void)html; (void)html_len;
    qsa_preparse_ctx_t *qc = (qsa_preparse_ctx_t *)ctx;
    qc->count = 0;
    lxb_selectors_find(qc->selectors,
                       lxb_dom_interface_node(qc->doc),
                       qc->sel_list, qsa_preparse_cb, qc);
}

/* -------------------------------------------------------------------
 * Benchmark: serialization (DOM -> HTML)
 * ------------------------------------------------------------------- */

typedef struct {
    lxb_html_document_t *doc;
    size_t total_bytes;
} serialize_ctx_t;

static lxb_status_t serialize_cb(const lxb_char_t *data, size_t len, void *ctx) {
    serialize_ctx_t *sc = (serialize_ctx_t *)ctx;
    sc->total_bytes += len;
    return LXB_STATUS_OK;
}

static void bench_serialize(const lxb_char_t *html, size_t html_len, void *ctx) {
    (void)html; (void)html_len;
    serialize_ctx_t *sc = (serialize_ctx_t *)ctx;
    sc->total_bytes = 0;
    lxb_html_serialize_deep_cb(
        lxb_dom_interface_node(sc->doc),
        serialize_cb, sc);
}

/* -------------------------------------------------------------------
 * Main
 * ------------------------------------------------------------------- */

int main(void) {
    size_t html_len;
    char *html_raw = gen_medium_html(&html_len);
    const lxb_char_t *html = (const lxb_char_t *)html_raw;

    printf("lexbor %d.%d.%d comparison benchmarks\n",
           LXB_VERSION_MAJOR, LXB_VERSION_MINOR, LXB_VERSION_PATCH);
    printf("Input size: %zu bytes\n", html_len);
    printf("========================================================================\n");

    int n = 5000;

    /* --- Parsing --- */
    printf("\n--- Parsing (full tree build) ---\n");

    bench_result_t r;
    r = bench("parse (one-shot)", n, html_len,
              bench_parse_oneshot, html, html_len, NULL);
    print_result("parse (one-shot)", r);

    lxb_html_parser_t *parser = lxb_html_parser_create();
    lxb_html_parser_init(parser);
    r = bench("parse (parser reuse)", n, html_len,
              bench_parse_reuse, html, html_len, parser);
    print_result("parse (parser reuse)", r);
    lxb_html_parser_destroy(parser);

    r = bench("parse (incremental, 4KB)", n, html_len,
              bench_parse_incremental, html, html_len, NULL);
    print_result("parse (incremental, 4KB)", r);

    /* --- Serialization --- */
    printf("\n--- Serialization ---\n");
    lxb_html_document_t *ser_doc = lxb_html_document_create();
    lxb_html_document_parse(ser_doc, html, html_len);
    serialize_ctx_t ser_ctx = { .doc = ser_doc, .total_bytes = 0 };
    r = bench("serialize (callback)", n, html_len,
              bench_serialize, html, html_len, &ser_ctx);
    print_result("serialize (callback)", r);
    lxb_html_document_destroy(ser_doc);

    /* --- querySelectorAll (parse + match) --- */
    printf("\n--- querySelectorAll (parse + match each iteration) ---\n");

    struct { const char *label; const char *selector; } qsa_full_cases[] = {
        { "qsa(\"div\")",                           "div" },
        { "qsa(\"div.item\")",                      "div.item" },
        { "qsa(\"div.item span.name\")",            "div.item span.name" },
        { "qsa(\"div:first-child\")",               "div:first-child" },
        { "qsa(\"div.item:nth-child(2n+1)\")",      "div.item:nth-child(2n+1)" },
        { "qsa(\":not(.item)\")",                   ":not(.item)" },
        { "qsa(\"[id]\")",                          "[id]" },
        { "qsa(\"div.catalog > div + div\")",       "div.catalog > div + div" },
    };
    int nqsa_full = sizeof(qsa_full_cases) / sizeof(qsa_full_cases[0]);

    for (int i = 0; i < nqsa_full; i++) {
        lxb_css_parser_t *cp = lxb_css_parser_create();
        lxb_css_parser_init(cp, NULL);
        lxb_css_selector_list_t *sl = lxb_css_selectors_parse(
            cp,
            (const lxb_char_t *)qsa_full_cases[i].selector,
            strlen(qsa_full_cases[i].selector));

        lxb_selectors_t *sels = lxb_selectors_create();
        lxb_selectors_init(sels);

        qsa_ctx_t qc = { .css_parser = cp, .selectors = sels,
                          .sel_list = sl, .count = 0 };
        int qn = 2000;
        r = bench(qsa_full_cases[i].label, qn, html_len,
                  bench_qsa, html, html_len, &qc);
        char lbl[128];
        snprintf(lbl, sizeof lbl, "%s  [%d matches]",
                 qsa_full_cases[i].label, qc.count);
        print_result(lbl, r);

        lxb_selectors_destroy(sels, true);
        lxb_css_parser_destroy(cp, true);
    }

    /* --- querySelectorAll (pre-parsed, match only) --- */
    printf("\n--- querySelectorAll (pre-parsed document, match only) ---\n");
    lxb_html_document_t *pre_doc = lxb_html_document_create();
    lxb_html_document_parse(pre_doc, html, html_len);

    struct { const char *label; const char *selector; } qsa_cases[] = {
        { "qsa(\"div\")",                           "div" },
        { "qsa(\"div.item\")",                      "div.item" },
        { "qsa(\"div.item span.name\")",            "div.item span.name" },
        { "qsa(\"div:first-child\")",               "div:first-child" },
        { "qsa(\"div.item:nth-child(2n+1)\")",      "div.item:nth-child(2n+1)" },
        { "qsa(\":not(.item)\")",                   ":not(.item)" },
        { "qsa(\"[id]\")",                          "[id]" },
        { "qsa(\"div.catalog > div + div\")",       "div.catalog > div + div" },
    };
    int nqsa = sizeof(qsa_cases) / sizeof(qsa_cases[0]);

    for (int i = 0; i < nqsa; i++) {
        lxb_css_parser_t *cp = lxb_css_parser_create();
        lxb_css_parser_init(cp, NULL);
        lxb_css_selector_list_t *sl = lxb_css_selectors_parse(
            cp,
            (const lxb_char_t *)qsa_cases[i].selector,
            strlen(qsa_cases[i].selector));

        lxb_selectors_t *sels = lxb_selectors_create();
        lxb_selectors_init(sels);

        qsa_preparse_ctx_t qc = { .doc = pre_doc, .selectors = sels,
                                   .sel_list = sl, .count = 0 };
        int qn = 50000;
        r = bench(qsa_cases[i].label, qn, html_len,
                  bench_qsa_preparse, html, html_len, &qc);
        char lbl[128];
        snprintf(lbl, sizeof lbl, "%s  [%d matches]",
                 qsa_cases[i].label, qc.count);
        print_result(lbl, r);

        lxb_selectors_destroy(sels, true);
        lxb_css_parser_destroy(cp, true);
    }

    lxb_html_document_destroy(pre_doc);

    printf("\n========================================================================\n");
    printf("Done.\n");

    free(html_raw);
    return 0;
}
