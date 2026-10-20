/* Unit, protocol and differential tests for wireform-websocket wf_pmd.c
 * (RFC 7692 permessage-deflate over zlib). */
#include "harness.h"
#include "api.h"

#include <sys/mman.h>

static const uint8_t HELLO[] = "Hello";

/* Inflate one payload on `s` with the Haskell driver and compare. */
static void expect_inflate(wf_pmd_stream *s, const uint8_t *p, size_t n,
                           const void *want, size_t want_n)
{
    pmd_buf out = { 0 };
    int bad = 0;
    int rc = pmd_inflate_msg(s, p, n, 0, 0, &out, &bad);
    WF_CHECK_EQ(rc, WF_PMD_OK);
    WF_CHECK_EQ(bad, 0);
    WF_CHECK_EQ(out.n, want_n);
    if (out.n == want_n) WF_CHECK_MEM(out.p, want, want_n);
    free(out.p);
}

static int inflate_rc(wf_pmd_stream *s, const uint8_t *p, size_t n)
{
    pmd_buf out = { 0 };
    int bad = 0;
    int rc = pmd_inflate_msg(s, p, n, 0, 1 << 20, &out, &bad);
    WF_CHECK_EQ(bad, 0);
    free(out.p);
    return rc;
}

/* RFC 7692 sec 7.2.3: every worked example decodes to "Hello". */
static void rfc7692_examples(void)
{
    /* 7.2.3.1: one fixed-Huffman block, BFINAL = 0. */
    static const uint8_t one_block[] = { 0xf2, 0x48, 0xcd, 0xc9, 0xc9, 0x07, 0x00 };
    /* 7.2.3.2: the same message again, referencing the shared window. */
    static const uint8_t shared[] = { 0xf2, 0x00, 0x11, 0x00, 0x00 };
    /* 7.2.3.3: a stored block. */
    static const uint8_t stored[] = { 0x00, 0x05, 0x00, 0xfa, 0xff, 0x48,
                                      0x65, 0x6c, 0x6c, 0x6f, 0x00 };
    /* 7.2.3.5: two blocks with a sync-flush marker between them. */
    static const uint8_t two_blocks[] = { 0xf2, 0x48, 0x05, 0x00, 0x00, 0x00, 0xff,
                                          0xff, 0xca, 0xc9, 0xc9, 0x07, 0x00 };
    /* 7.2.3.6: the empty message. */
    static const uint8_t empty[] = { 0x00 };

    wf_pmd_stream *s = wf_pmd_inflate_new(15);
    WF_CHECK(s != NULL);
    expect_inflate(s, one_block, sizeof one_block, HELLO, 5);
    expect_inflate(s, shared, sizeof shared, HELLO, 5);
    expect_inflate(s, stored, sizeof stored, HELLO, 5);
    expect_inflate(s, two_blocks, sizeof two_blocks, HELLO, 5);
    expect_inflate(s, empty, sizeof empty, "", 0);
    expect_inflate(s, shared, sizeof shared, HELLO, 5);
    wf_pmd_free(s);

    /* 7.2.3.2 with client_no_context_takeover: the receiver may reset,
     * and the second message then decodes on its own. */
    s = wf_pmd_inflate_new(15);
    expect_inflate(s, one_block, sizeof one_block, HELLO, 5);
    WF_CHECK_EQ(wf_pmd_reset_inflate(s), WF_PMD_OK);
    expect_inflate(s, one_block, sizeof one_block, HELLO, 5);
    /* After a reset the window is empty, so a back-reference into the
     * previous message is a distance-too-far error. */
    WF_CHECK_EQ(wf_pmd_reset_inflate(s), WF_PMD_OK);
    WF_CHECK_EQ(inflate_rc(s, shared, sizeof shared), WF_PMD_BAD_INPUT);
    wf_pmd_free(s);
}

/* RFC 7692 sec 7.2.3.4: a message flushed with a BFINAL = 1 block plus
 * the 0x00 header octet. Later blocks follow the final one and later
 * messages keep using the same LZ77 window (sec 7.2.1 / 7.2.2). Before
 * the fix the stream stayed in zlib's DONE state and every later
 * message on a context-takeover connection inflated to zero bytes. */
static void bfinal_message_then_context_takeover(void)
{
    static const uint8_t bfinal[] = { 0xf3, 0x48, 0xcd, 0xc9, 0xc9, 0x07, 0x00, 0x00 };
    /* Same BFINAL block without the 0x00 octet: only the appended
     * 00 00 FF FF follows the final block. Accepted leniently. */
    static const uint8_t bfinal_bare[] = { 0xf3, 0x48, 0xcd, 0xc9, 0xc9, 0x07, 0x00 };
    static const uint8_t shared[] = { 0xf2, 0x00, 0x11, 0x00, 0x00 };
    /* "He" in a BFINAL block, then "llo" in a normal block that follows
     * it inside the same message. */
    static const uint8_t final_then_more[] = { 0xf3, 0x48, 0x05, 0x00, 0xca, 0xc9, 0xc9, 0x07, 0x00 };
    /* Two BFINAL blocks in one message: rejected (one window restart per
     * message bounds the restart cost an attacker can trigger). */
    static const uint8_t two_finals[] = { 0xf3, 0x48, 0x05, 0x00, 0xcb, 0xc9, 0xc9, 0x07, 0x00, 0x00 };

    for (size_t step = 0; step <= 3; step++) {
        wf_pmd_stream *s = wf_pmd_inflate_new(15);
        pmd_buf out = { 0 };
        int bad = 0;
        WF_CHECK_EQ(pmd_inflate_msg(s, bfinal, sizeof bfinal, step, 0, &out, &bad), WF_PMD_OK);
        WF_CHECK_EQ(pmd_inflate_msg(s, shared, sizeof shared, step, 0, &out, &bad), WF_PMD_OK);
        WF_CHECK_EQ(pmd_inflate_msg(s, bfinal_bare, sizeof bfinal_bare, step, 0, &out, &bad), WF_PMD_OK);
        WF_CHECK_EQ(pmd_inflate_msg(s, shared, sizeof shared, step, 0, &out, &bad), WF_PMD_OK);
        WF_CHECK_EQ(pmd_inflate_msg(s, final_then_more, sizeof final_then_more, step, 0, &out, &bad), WF_PMD_OK);
        WF_CHECK_EQ(pmd_inflate_msg(s, shared, sizeof shared, step, 0, &out, &bad), WF_PMD_OK);
        WF_CHECK_EQ(bad, 0);
        WF_CHECK_EQ(out.n, 30);
        if (out.n == 30) WF_CHECK_MEM(out.p, "HelloHelloHelloHelloHelloHello", 30);
        free(out.p);
        WF_CHECK_EQ(inflate_rc(s, two_finals, sizeof two_finals), WF_PMD_BAD_INPUT);
        wf_pmd_free(s);
    }
}

/* Payloads that are not a complete sequence of blocks once the tail is
 * appended must fail, not return a silently truncated message (before
 * the fix a cut-off message returned OK with partial output and left the
 * stream mid-block, so the next message decoded as garbage). */
static void truncated_and_malformed(void)
{
    static const uint8_t one_block[] = { 0xf2, 0x48, 0xcd, 0xc9, 0xc9, 0x07, 0x00 };
    for (size_t cut = 1; cut < sizeof one_block; cut++) {
        wf_pmd_stream *s = wf_pmd_inflate_new(15);
        int rc = inflate_rc(s, one_block, cut);
        WF_CHECK_EQ(rc, WF_PMD_BAD_INPUT);
        wf_pmd_free(s);
    }

    static const uint8_t reserved_btype[] = { 0x06 }; /* BTYPE = 11 */
    static const uint8_t bad_nlen[] = { 0x00, 0x05, 0x00, 0xfa, 0xfe, 'H', 'e', 'l', 'l', 'o', 0x00 };
    static const uint8_t far_back[] = { 0xf2, 0x00, 0x11, 0x00, 0x00 };
    static const uint8_t empty_payload[] = { 0 };
    static const uint8_t all_ff[] = { 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };
    static const uint8_t all_80[] = { 0x80, 0x80, 0x80, 0x80, 0x80, 0x80 };
    struct { const uint8_t *p; size_t n; } cases[] = {
        { reserved_btype, sizeof reserved_btype },
        { bad_nlen, sizeof bad_nlen },
        { far_back, sizeof far_back },
        { empty_payload, 0 },
        { all_ff, sizeof all_ff },
        { all_80, sizeof all_80 },
    };
    for (size_t i = 0; i < sizeof cases / sizeof cases[0]; i++) {
        for (int wb = 8; wb <= 15; wb++) {
            wf_pmd_stream *s = wf_pmd_inflate_new(wb);
            WF_CHECK_EQ(inflate_rc(s, cases[i].p, cases[i].n), WF_PMD_BAD_INPUT);
            wf_pmd_free(s);
        }
    }
}

/* Deterministic pseudo-text so deflate finds matches. */
static void gen_text(wf_rng *r, uint8_t *p, size_t n)
{
    static const char *words[] = { "the ", "quick ", "brown ", "fox ", "{\"id\":", "42,",
                                   "\"name\":\"", "websocket", "\"}\n", "deflate ", "\x00\xff" };
    size_t i = 0;
    while (i < n) {
        if (wf_rng_below(r, 8) == 0) {
            p[i++] = (uint8_t)wf_rng_next(r);
            continue;
        }
        const char *w = words[wf_rng_below(r, sizeof words / sizeof words[0])];
        size_t wl = strlen(w);
        if (wl == 0) wl = 2;
        for (size_t k = 0; k < wl && i < n; k++) p[i++] = (uint8_t)w[k];
    }
}

/* Round trips over every window size, level and memLevel the API takes,
 * with and without context takeover, through the Haskell-style driver
 * and a fixed tiny per-call capacity. Every compressed message is also
 * inflated by a plain zlib raw inflater at the negotiated window size, so
 * a deflater that used a longer window than agreed (sec 7.2.1) fails. */
static void roundtrip_matrix(void)
{
    wf_rng r = wf_rng_from_env();
    static const int levels[] = { -1, 0, 1, 6, 9, 42, -7 };
    static const int memlevels[] = { 1, 8, 9, 0, 10 };
#ifdef WF_PMD_CHUNK
    /* zlib moves 7 bytes per call in this variant: keep messages small. */
    static const size_t sizes[] = { 0, 1, 5, 0, 300, 1500, 0, 17 };
#else
    static const size_t sizes[] = { 0, 1, 5, 0, 300, 4096, 70000, 0, 17 };
#endif
    for (int wb = 8; wb <= 15; wb++) {
        for (size_t li = 0; li < sizeof levels / sizeof levels[0]; li++) {
            for (size_t mi = 0; mi < sizeof memlevels / sizeof memlevels[0]; mi++) {
                for (int takeover = 0; takeover <= 1; takeover++) {
                    size_t step = (li + mi + (size_t)takeover) % 3 == 0 ? 0 : 1 + (size_t)wf_rng_below(&r, 40);
                    wf_pmd_stream *d = wf_pmd_deflate_new(wb, levels[li], memlevels[mi]);
                    wf_pmd_stream *i = wf_pmd_inflate_new(wb);
                    WF_CHECK(d && i);
                    z_stream ref;
                    memset(&ref, 0, sizeof ref);
                    WF_CHECK_EQ(inflateInit2(&ref, -wb), Z_OK);
                    for (size_t k = 0; k < sizeof sizes / sizeof sizes[0]; k++) {
                        size_t n = sizes[k];
                        uint8_t *msg = malloc(n ? n : 1);
                        gen_text(&r, msg, n);
                        pmd_buf c = { 0 }, p = { 0 }, q = { 0 };
                        int bad = 0;
                        /* Fixed tiny steps only on the small messages
                         * (the 70 KB one would take ~10^4 calls each). */
                        size_t st = (step && n > 4096) ? n / 8 : step;
                        WF_CHECK_EQ(pmd_deflate_msg(d, msg, n, st, &c, &bad), WF_PMD_OK);
                        WF_CHECK(c.n >= 1);
                        WF_CHECK_EQ(pmd_inflate_msg(i, c.p, c.n, st, 0, &p, &bad), WF_PMD_OK);
                        WF_CHECK_EQ(bad, 0);
                        WF_CHECK_EQ(p.n, n);
                        if (p.n == n) WF_CHECK_MEM(p.p, msg, n);
                        WF_CHECK(ref_inflate_msg(&ref, c.p, c.n, &q));
                        WF_CHECK_EQ(q.n, n);
                        if (q.n == n) WF_CHECK_MEM(q.p, msg, n);
                        if (!takeover) {
                            WF_CHECK_EQ(wf_pmd_reset_deflate(d), WF_PMD_OK);
                            WF_CHECK_EQ(wf_pmd_reset_inflate(i), WF_PMD_OK);
                            WF_CHECK_EQ(inflateReset(&ref), Z_OK);
                        }
                        free(c.p);
                        free(p.p);
                        free(q.p);
                        free(msg);
                    }
                    inflateEnd(&ref);
                    wf_pmd_free(d);
                    wf_pmd_free(i);
                }
            }
        }
    }
}

/* Empty messages: Haskell passes BS.empty as (NULL, 0). The result must
 * be a valid PMD payload (sec 7.2.3.6: at least the 0x00 header octet),
 * also when the previous message's sync flush already completed, where
 * zlib's deflate() reports Z_BUF_ERROR ("nothing to do"). Before the fix
 * every empty message after the first returned WF_PMD_BAD_INPUT, so a
 * second empty send threw PmdBadInput. */
static void empty_messages(void)
{
    for (int takeover = 0; takeover <= 1; takeover++) {
        wf_pmd_stream *d = wf_pmd_deflate_new(15, 1, 8);
        wf_pmd_stream *i = wf_pmd_inflate_new(15);
        const char *seq[] = { "", "", "abc", "", "", "abcabc", "" };
        for (size_t k = 0; k < sizeof seq / sizeof seq[0]; k++) {
            size_t n = strlen(seq[k]);
            pmd_buf c = { 0 }, p = { 0 };
            int bad = 0;
            pmd_buf raw = { 0 };
            /* Raw call: NULL source for the empty message, small dst. */
            uint8_t *dst = wf_alloc(64);
            size_t produced = 0;
            int rc = wf_pmd_deflate(d, n ? (const uint8_t *)seq[k] : NULL, n, dst, 64, &produced);
            WF_CHECK_EQ(rc, WF_PMD_OK);
            WF_CHECK(produced >= 5);
            pmd_buf_put(&raw, dst, produced);
            wf_free(dst, 64);
            WF_CHECK(raw.n >= 4 && memcmp(raw.p + raw.n - 4, pmd_tail, 4) == 0);
            pmd_buf_put(&c, raw.p, raw.n - 4);
            WF_CHECK(c.n >= 1);
            WF_CHECK_EQ(pmd_inflate_msg(i, c.p, c.n, 0, 0, &p, &bad), WF_PMD_OK);
            WF_CHECK_EQ(p.n, n);
            if (p.n == n) WF_CHECK_MEM(p.p, seq[k], n);
            if (!takeover) {
                wf_pmd_reset_deflate(d);
                wf_pmd_reset_inflate(i);
            }
            free(raw.p);
            free(c.p);
            free(p.p);
        }
        /* An empty message with less than 5 bytes of room asks for more
         * and then completes on the resume call. */
        for (size_t room = 0; room < 5; room++) {
            uint8_t *dst = wf_alloc(room ? room : 1);
            size_t produced = 9;
            WF_CHECK_EQ(wf_pmd_deflate(d, NULL, 0, dst, room, &produced), WF_PMD_NEEDS_MORE_OUT);
            WF_CHECK_EQ(produced, 0);
            wf_free(dst, room ? room : 1);
            dst = wf_alloc(8);
            WF_CHECK_EQ(wf_pmd_deflate(d, NULL, 0, dst, 8, &produced), WF_PMD_OK);
            WF_CHECK_EQ(produced, 5);
            WF_CHECK_MEM(dst, "\x00\x00\x00\xff\xff", 5);
            wf_free(dst, 8);
        }
        wf_pmd_free(d);
        wf_pmd_free(i);
    }
}

/* Every per-call capacity from 1 to 70 bytes, for a message whose output
 * spans many calls, on both directions; plus cap == 0 calls interleaved
 * (no progress, NEEDS_MORE_OUT, state intact). */
static void every_capacity(void)
{
    wf_rng r = wf_rng_from_env();
    enum { N = 3000 };
    uint8_t msg[N];
    gen_text(&r, msg, N);
    for (size_t step = 1; step <= 70; step++) {
        wf_pmd_stream *d = wf_pmd_deflate_new(15, 6, 8);
        wf_pmd_stream *i = wf_pmd_inflate_new(15);
        for (int rep = 0; rep < 2; rep++) {
            pmd_buf c = { 0 }, p = { 0 };
            int bad = 0;
            WF_CHECK_EQ(pmd_deflate_msg(d, msg, N, step, &c, &bad), WF_PMD_OK);
            WF_CHECK_EQ(pmd_inflate_msg(i, c.p, c.n, step, 0, &p, &bad), WF_PMD_OK);
            WF_CHECK_EQ(bad, 0);
            WF_CHECK_EQ(p.n, N);
            if (p.n == N) WF_CHECK_MEM(p.p, msg, N);
            free(c.p);
            free(p.p);
        }
        wf_pmd_free(d);
        wf_pmd_free(i);
    }

    /* cap == 0 between real calls. */
    wf_pmd_stream *d = wf_pmd_deflate_new(15, 6, 8);
    wf_pmd_stream *i = wf_pmd_inflate_new(15);
    uint8_t *m = wf_dup(msg, N);
    pmd_buf c = { 0 };
    uint8_t zero_dst[1];
    size_t produced = 7;
    WF_CHECK_EQ(wf_pmd_deflate(d, m, N, zero_dst, 0, &produced), WF_PMD_NEEDS_MORE_OUT);
    WF_CHECK_EQ(produced, 0);
    int rc;
    do {
        uint8_t buf[100];
        rc = wf_pmd_deflate(d, NULL, 0, buf, sizeof buf, &produced);
        pmd_buf_put(&c, buf, produced);
        size_t z = 3;
        if (rc == WF_PMD_NEEDS_MORE_OUT) {
            WF_CHECK_EQ(wf_pmd_deflate(d, NULL, 0, zero_dst, 0, &z), WF_PMD_NEEDS_MORE_OUT);
            WF_CHECK_EQ(z, 0);
        }
    } while (rc == WF_PMD_NEEDS_MORE_OUT);
    WF_CHECK_EQ(rc, WF_PMD_OK);
    WF_CHECK(c.n >= 4);
    c.n -= 4;
    pmd_buf p = { 0 };
    int bad = 0;
    WF_CHECK_EQ(pmd_inflate_msg(i, c.p, c.n, 0, 0, &p, &bad), WF_PMD_OK);
    WF_CHECK_EQ(p.n, N);
    wf_free(m, N);
    free(c.p);
    free(p.p);
    wf_pmd_free(d);
    wf_pmd_free(i);
}

/* An inflate bomb (16 MiB of zeros in ~16 KiB; 1 MiB in the chunked
 * variant, where zlib moves 7 bytes per call) stays inside every buffer
 * it is given: each call fills exactly its cap and reports
 * NEEDS_MORE_OUT, so a caller enforcing a message-size limit stops after
 * limit bytes. Resuming afterwards still yields the exact plaintext. */
static void inflate_bomb_respects_caps(void)
{
#ifdef WF_PMD_CHUNK
    size_t n = (size_t)1 << 20;
#else
    size_t n = (size_t)16 << 20;
#endif
    uint8_t *zeros = calloc(1, n);
    wf_pmd_stream *d = wf_pmd_deflate_new(15, 9, 9);
    pmd_buf c = { 0 };
    int bad = 0;
    WF_CHECK_EQ(pmd_deflate_msg(d, zeros, n, 0, &c, &bad), WF_PMD_OK);
    WF_CHECK(c.n < n / 500);

    /* The stream keeps pointing into the message across resumes, so the
     * message buffer must outlive them (pmd_inflate_msg frees its copy). */
    wf_pmd_stream *i = wf_pmd_inflate_new(15);
    uint8_t *m = wf_alloc(c.n + 4);
    memcpy(m, c.p, c.n);
    memcpy(m + c.n, pmd_tail, 4);
    pmd_buf p = { 0 };
    WF_CHECK_EQ(pmd_drive(wf_pmd_inflate, i, m, c.n + 4, 0, 100000, &p, &bad), WF_PMD_NEEDS_MORE_OUT);
    WF_CHECK_EQ(p.n, 100000);
    WF_CHECK_EQ(bad, 0);
    /* Keep draining with 1 MiB buffers. */
    int rc;
    do {
        size_t room = 1 << 20;
        uint8_t *dst = wf_alloc(room);
        size_t produced = 0;
        rc = wf_pmd_inflate(i, NULL, 0, dst, room, &produced);
        WF_CHECK(produced <= room);
        pmd_buf_put(&p, dst, produced);
        wf_free(dst, room);
    } while (rc == WF_PMD_NEEDS_MORE_OUT);
    WF_CHECK_EQ(rc, WF_PMD_OK);
    WF_CHECK_EQ(p.n, n);
    if (p.n == n) WF_CHECK_MEM(p.p, zeros, n);
    wf_free(m, c.n + 4);
    free(p.p);
    free(c.p);
    free(zeros);
    wf_pmd_free(d);
    wf_pmd_free(i);
}

/* A message whose plaintext exactly fills the buffer: either OK at once,
 * or NEEDS_MORE_OUT followed by an OK resume that produces nothing. */
static void exact_fill(void)
{
    for (size_t n = 1; n <= 300; n += 7) {
        uint8_t *msg = malloc(n);
        for (size_t k = 0; k < n; k++) msg[k] = (uint8_t)(k * 13 + n);
        wf_pmd_stream *d = wf_pmd_deflate_new(15, 1, 8);
        pmd_buf c = { 0 };
        int bad = 0;
        WF_CHECK_EQ(pmd_deflate_msg(d, msg, n, 0, &c, &bad), WF_PMD_OK);
        wf_pmd_stream *i = wf_pmd_inflate_new(15);
        uint8_t *m = wf_alloc(c.n + 4);
        memcpy(m, c.p, c.n);
        memcpy(m + c.n, pmd_tail, 4);
        uint8_t *dst = wf_alloc(n);
        size_t produced = 0;
        int rc = wf_pmd_inflate(i, m, c.n + 4, dst, n, &produced);
        WF_CHECK_EQ(produced, n);
        WF_CHECK_MEM(dst, msg, n);
        if (rc == WF_PMD_NEEDS_MORE_OUT) {
            uint8_t more[16];
            rc = wf_pmd_inflate(i, NULL, 0, more, sizeof more, &produced);
            WF_CHECK_EQ(produced, 0);
        }
        WF_CHECK_EQ(rc, WF_PMD_OK);
        wf_free(dst, n);
        wf_free(m, c.n + 4);
        free(c.p);
        free(msg);
        wf_pmd_free(d);
        wf_pmd_free(i);
    }
}

/* Capacities above 4 GiB: zlib's avail_out is a 32-bit uInt. Before the
 * fix the shim cast dst_cap to uInt and reported
 * produced = dst_cap - avail_out, i.e. 4 GiB too much (dst_cap = 2^32 +
 * 100) or looped on NEEDS_MORE_OUT forever (dst_cap = 2^32), which made
 * the Haskell driver copy past its buffer. The buffer is a lazily
 * committed mapping; only the first page is ever touched. */
static void capacity_above_4gib(void)
{
    if (sizeof(size_t) < 8) return;
    static const size_t caps[] = { (size_t)1 << 32, ((size_t)1 << 32) + 100,
                                   ((size_t)1 << 33) + 7 };
    size_t map = ((size_t)1 << 33) + 4096 * 4;
    uint8_t *big = mmap(NULL, map, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if (big == MAP_FAILED) {
        fprintf(stderr, "capacity_above_4gib: mmap failed, skipped\n");
        return;
    }
    static const uint8_t one_block[] = { 0xf2, 0x48, 0xcd, 0xc9, 0xc9, 0x07, 0x00, 0x00, 0x00, 0xff, 0xff };
    for (size_t k = 0; k < sizeof caps / sizeof caps[0]; k++) {
        wf_pmd_stream *i = wf_pmd_inflate_new(15);
        size_t produced = 0;
        WF_CHECK_EQ(wf_pmd_inflate(i, one_block, sizeof one_block, big, caps[k], &produced), WF_PMD_OK);
        WF_CHECK_EQ(produced, 5);
        WF_CHECK_MEM(big, HELLO, 5);
        wf_pmd_free(i);

        wf_pmd_stream *d = wf_pmd_deflate_new(15, 1, 8);
        WF_CHECK_EQ(wf_pmd_deflate(d, HELLO, 5, big, caps[k], &produced), WF_PMD_OK);
        WF_CHECK(produced > 4 && produced < 64);
        WF_CHECK(memcmp(big + produced - 4, pmd_tail, 4) == 0);
        wf_pmd_free(d);
    }
    munmap(big, map);
}

/* Constructor validation, NULL / wrong-kind handles, double free safety. */
static void api_edges(void)
{
    int bad_wb[] = { -15, -1, 0, 7, 16, 31, 1 << 30 };
    for (size_t k = 0; k < sizeof bad_wb / sizeof bad_wb[0]; k++) {
        WF_CHECK(wf_pmd_inflate_new(bad_wb[k]) == NULL);
        WF_CHECK(wf_pmd_deflate_new(bad_wb[k], 1, 8) == NULL);
    }
    wf_pmd_free(NULL);

    wf_pmd_stream *i = wf_pmd_inflate_new(8);
    wf_pmd_stream *d = wf_pmd_deflate_new(8, 1, 8);
    WF_CHECK(i && d);
    uint8_t buf[64];
    size_t produced = 99;
    WF_CHECK_EQ(wf_pmd_inflate(d, HELLO, 5, buf, sizeof buf, &produced), WF_PMD_BAD_INPUT);
    WF_CHECK_EQ(produced, 0);
    produced = 99;
    WF_CHECK_EQ(wf_pmd_deflate(i, HELLO, 5, buf, sizeof buf, &produced), WF_PMD_BAD_INPUT);
    WF_CHECK_EQ(produced, 0);
    WF_CHECK_EQ(wf_pmd_inflate(NULL, HELLO, 5, buf, sizeof buf, &produced), WF_PMD_BAD_INPUT);
    WF_CHECK_EQ(wf_pmd_deflate(NULL, HELLO, 5, buf, sizeof buf, &produced), WF_PMD_BAD_INPUT);
    WF_CHECK_EQ(wf_pmd_reset_inflate(d), WF_PMD_BAD_INPUT);
    WF_CHECK_EQ(wf_pmd_reset_deflate(i), WF_PMD_BAD_INPUT);
    WF_CHECK_EQ(wf_pmd_reset_inflate(NULL), WF_PMD_BAD_INPUT);
    WF_CHECK_EQ(wf_pmd_reset_deflate(NULL), WF_PMD_BAD_INPUT);
    /* out_produced may be NULL. */
    WF_CHECK_EQ(wf_pmd_deflate(d, HELLO, 5, buf, sizeof buf, NULL), WF_PMD_OK);
    static const uint8_t one_block[] = { 0xf2, 0x48, 0xcd, 0xc9, 0xc9, 0x07, 0x00, 0x00, 0x00, 0xff, 0xff };
    WF_CHECK_EQ(wf_pmd_inflate(i, one_block, sizeof one_block, buf, sizeof buf, NULL), WF_PMD_OK);
    WF_CHECK_MEM(buf, HELLO, 5);
    wf_pmd_free(i);
    wf_pmd_free(d);
}

/* Window bits 8 is negotiable (sec 7.1.2) but zlib's raw deflate cannot
 * do it; the shim uses 9, whose longest match distance (512 - 262 = 250)
 * still fits a 256-byte window. Check with a zlib inflater that really
 * has a 256-byte window, over input full of repeats at distances
 * 200..400. */
static void window_bits_8_stays_in_window(void)
{
    wf_rng r = wf_rng_from_env();
    wf_pmd_stream *d = wf_pmd_deflate_new(8, 9, 9);
    z_stream ref;
    memset(&ref, 0, sizeof ref);
    WF_CHECK_EQ(inflateInit2(&ref, -8), Z_OK);
    for (int rep = 0; rep < 20; rep++) {
        size_t n = 20000;
        uint8_t *msg = malloc(n);
        size_t period = 200 + (size_t)wf_rng_below(&r, 200);
        wf_rng_fill(&r, msg, period);
        for (size_t k = period; k < n; k++) msg[k] = msg[k - period];
        pmd_buf c = { 0 }, q = { 0 };
        int bad = 0;
        WF_CHECK_EQ(pmd_deflate_msg(d, msg, n, 0, &c, &bad), WF_PMD_OK);
        WF_CHECK(ref_inflate_msg(&ref, c.p, c.n, &q));
        WF_CHECK_EQ(q.n, n);
        if (q.n == n) WF_CHECK_MEM(q.p, msg, n);
        free(c.p);
        free(q.p);
        free(msg);
    }
    inflateEnd(&ref);
    wf_pmd_free(d);
}

/* Random garbage and bit-flipped valid messages: never a crash or an
 * out-of-cap write; a failed message is BAD_INPUT. Streams are freed
 * after errors (leaks show up under LSan / valgrind). */
static void random_garbage(void)
{
    wf_rng r = wf_rng_from_env();
    long iters = wf_iters(3000);
    for (long it = 0; it < iters; it++) {
        int wb = 8 + (int)wf_rng_below(&r, 8);
        wf_pmd_stream *d = wf_pmd_deflate_new(wb, 1, 8);
        wf_pmd_stream *i = wf_pmd_inflate_new(wb);
        size_t n = (size_t)wf_rng_below(&r, 400);
        uint8_t *msg = malloc(n ? n : 1);
        gen_text(&r, msg, n);
        pmd_buf c = { 0 }, p = { 0 };
        int bad = 0;
        WF_CHECK_EQ(pmd_deflate_msg(d, msg, n, 0, &c, &bad), WF_PMD_OK);
        if (it & 1) {
            for (size_t k = 0; k < c.n; k++) c.p[k] = (uint8_t)wf_rng_next(&r);
        } else if (c.n) {
            c.p[wf_rng_below(&r, c.n)] ^= (uint8_t)(1u << wf_rng_below(&r, 8));
        }
        size_t step = wf_rng_below(&r, 3) ? 0 : 1 + (size_t)wf_rng_below(&r, 16);
        int rc = pmd_inflate_msg(i, c.p, c.n, step, 1 << 20, &p, &bad);
        WF_CHECK(rc == WF_PMD_OK || rc == WF_PMD_BAD_INPUT || rc == WF_PMD_NEEDS_MORE_OUT);
        WF_CHECK_EQ(bad, 0);
        free(c.p);
        free(p.p);
        free(msg);
        wf_pmd_free(d);
        wf_pmd_free(i);
    }
}

int main(void)
{
    WF_RUN(rfc7692_examples);
    WF_RUN(bfinal_message_then_context_takeover);
    WF_RUN(truncated_and_malformed);
    WF_RUN(roundtrip_matrix);
    WF_RUN(empty_messages);
    WF_RUN(every_capacity);
    WF_RUN(inflate_bomb_respects_caps);
    WF_RUN(exact_fill);
    WF_RUN(capacity_above_4gib);
    WF_RUN(api_edges);
    WF_RUN(window_bits_8_stays_in_window);
    WF_RUN(random_garbage);
    return wf_finish("test_pmd");
}
