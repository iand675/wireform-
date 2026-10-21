/* Differential fuzz target for the offset-based XML scanners:
 * hs_xml_find_lt, hs_xml_find_byte, hs_xml_find_attr_end,
 * hs_xml_find_text_end, hs_html_find_text_end, hs_xml_find_cdata_end,
 * hs_xml_find_comment_end and hs_xml_find_escape, each against its
 * scalar reference in api.h.
 *
 * Input layout: [flags][offset][target][document...].
 *   offset  start offset, taken modulo len + 3 (so offset > len, the
 *           parser one past EOF, is exercised too)
 *   target  byte for find_byte / find_attr_end
 *   flags   bit 0: place the document at the very end of a virtual
 *           INT_MAX-byte buffer (indices len' = INT_MAX, offsets shifted
 *           by INT_MAX - len), to exercise the index arithmetic near
 *           INT_MAX; only the real document bytes are ever touched.
 *
 * Every scanner walks every match from `offset`, resuming at hit + 1, the
 * way the SAX / DOM parsers drive it. The document is copied into an
 * exact-size buffer, so any read outside it faults. */
#include "harness.h"
#include "api.h"

#include <limits.h>

static uint8_t g_target;
static ptrdiff_t k_find_byte(const uint8_t *b, ptrdiff_t o, ptrdiff_t n) { return hs_xml_find_byte(b, o, n, g_target); }
static ptrdiff_t r_find_byte(const uint8_t *b, ptrdiff_t o, ptrdiff_t n) { return ref_xml_find_byte(b, o, n, g_target); }
static ptrdiff_t k_attr_end(const uint8_t *b, ptrdiff_t o, ptrdiff_t n) { return hs_xml_find_attr_end(b, o, n, g_target); }
static ptrdiff_t k_attr_dq(const uint8_t *b, ptrdiff_t o, ptrdiff_t n) { return hs_xml_find_attr_end(b, o, n, '"'); }
static ptrdiff_t r_attr_dq(const uint8_t *b, ptrdiff_t o, ptrdiff_t n) { return ref_xml_find_byte(b, o, n, '"'); }
static ptrdiff_t k_attr_sq(const uint8_t *b, ptrdiff_t o, ptrdiff_t n) { return hs_xml_find_attr_end(b, o, n, '\''); }
static ptrdiff_t r_attr_sq(const uint8_t *b, ptrdiff_t o, ptrdiff_t n) { return ref_xml_find_byte(b, o, n, '\''); }

typedef ptrdiff_t (*scan_fn)(const uint8_t *, ptrdiff_t, ptrdiff_t);
static const struct {
    scan_fn kernel, ref;
} SCANNERS[] = {
    { hs_xml_find_lt, ref_xml_find_lt },
    { k_find_byte, r_find_byte },
    { k_attr_end, r_find_byte },
    { k_attr_dq, r_attr_dq },
    { k_attr_sq, r_attr_sq },
    { hs_xml_find_text_end, ref_xml_find_text_end },
    { hs_html_find_text_end, ref_html_find_text_end },
    { hs_xml_find_cdata_end, ref_xml_find_cdata_end },
    { hs_xml_find_comment_end, ref_xml_find_comment_end },
};

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 3 || size > (1u << 20)) return 0;
    uint8_t flags = data[0];
    ptrdiff_t n = (ptrdiff_t)(size - 3);
    ptrdiff_t off = (ptrdiff_t)(data[1] % (unsigned)(n + 3));
    g_target = data[2];
    uint8_t *real = wf_dup(data + 3, (size_t)n);
    /* (buf, len) as the kernel sees them. In virtual mode real[0] sits at
     * index INT_MAX - n, so len is past INT_MAX; offsets past len are
     * clamped to len (the parser's one-past-EOF is exercised in local
     * mode, where it is small enough to stay inside the buffer). */
    const uint8_t *buf = real;
    ptrdiff_t len = n;
    if (flags & 1) {
        ptrdiff_t shift = INT_MAX - n;
        buf = (const uint8_t *)((uintptr_t)real - (uintptr_t)shift);
        len = INT_MAX;
        off = (off > n ? n : off) + shift;
    }

    for (size_t si = 0; si < sizeof SCANNERS / sizeof SCANNERS[0]; si++) {
        ptrdiff_t o = off;
        for (ptrdiff_t steps = 0; steps <= n + 1; steps++) {
            ptrdiff_t got = SCANNERS[si].kernel(buf, o, len);
            ptrdiff_t want = SCANNERS[si].ref(buf, o, len);
            WF_FUZZ_ASSERT(got == want);
            WF_FUZZ_ASSERT(got == -1 || got == len || (got >= o && got < len));
            if (got < 0 || got >= len) break;
            o = got + 1;
        }
    }

    /* find_escape takes a count, not an end index: scan [o, len). */
    for (ptrdiff_t o = off; o <= len;) {
        ptrdiff_t got = hs_xml_find_escape(buf, o, len - o);
        WF_FUZZ_ASSERT(got == ref_xml_find_escape(buf, o, len - o));
        WF_FUZZ_ASSERT(got >= o && got <= len);
        if (got == len) break;
        o = got + 1;
    }

    wf_free(real, (size_t)n);
    return 0;
}
