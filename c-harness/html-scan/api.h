/* Kernel API of wireform-html/cbits/html_scan.c as the Haskell FFI imports
 * it, plus the scalar reference implementations the tests compare against.
 *
 * Callers
 * -------
 * HTML/Parse.hs imports the wireform_* scanners as pure
 * `Addr# -> CPtrdiff -> CPtrdiff -> CPtrdiff` (CInt for is_all_ascii).
 * `addr#` is the start of the input ByteString's payload and `end` is its
 * full length, so the buffer is exactly [buf, buf + end) and a byte past
 * `end` belongs to someone else. Every call site establishes
 * 0 <= off <= end before calling (the tokenizer loops guard `off < len`,
 * attribute scans start at most one past a byte they just read, the
 * entity loops' nextPos is bounded by min len (ampPos + 65)), so the
 * kernels may assume:
 *
 *   - buf is non-NULL and readable for [0, end);
 *   - 0 <= off <= end.  off == end is an empty scan.
 *
 * HTML/Encode.hs imports hs_html_find_{text,attr}_escape as
 * `Ptr Word8 -> CPtrdiff -> CPtrdiff -> IO CPtrdiff` and calls them on a
 * non-empty ByteString with 0 <= off < len. (They used to take and return
 * CInt, which truncated the length of a ByteString over 2 GiB; see
 * escape_offsets_beyond_2gib in test_html_scan.c.)
 *
 * wireform_scan_gt, wireform_skip_to_dquote and wireform_skip_to_squote
 * are exported but only reached from C (skip_attrs uses the skip_to_*
 * pair); they follow the same contract.
 *
 * Result encodings
 * ----------------
 *   - plain scanners return the offset of the first stop byte at or after
 *     off, or `end` when there is none;
 *   - scan_gt returns the offset just past the first '>', or `end`;
 *   - *_ascii scanners return (pos << 1) | ascii, where pos is the plain
 *     scanner's result. ascii == 1 guarantees [off, pos) is all < 0x80.
 *     The SIMD paths are conservative: when the stop byte lies in a full
 *     16-byte chunk, the flag covers that whole chunk (bytes past pos
 *     included). ref_ascii_bound gives the exact span the flag describes.
 *   - skip_attrs returns (past_gt << 1) | self_close, with past_gt = end + 1
 *     when the tag never closes.
 */
#ifndef HTML_SCAN_API_H
#define HTML_SCAN_API_H

#include <stddef.h>
#include <stdint.h>

ptrdiff_t wireform_scan_text(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end);
ptrdiff_t wireform_scan_text_ascii(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end);
ptrdiff_t wireform_scan_tagname(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end);
ptrdiff_t wireform_scan_dquote(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end);
ptrdiff_t wireform_scan_dquote_ascii(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end);
ptrdiff_t wireform_scan_squote(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end);
ptrdiff_t wireform_scan_squote_ascii(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end);
ptrdiff_t wireform_skip_to_dquote(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end);
ptrdiff_t wireform_skip_to_squote(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end);
ptrdiff_t wireform_skip_attrs(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end);
ptrdiff_t wireform_scan_unquoted(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end);
ptrdiff_t wireform_scan_gt(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end);
int wireform_is_all_ascii(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end);
ptrdiff_t hs_html_find_text_escape(const uint8_t *buf, ptrdiff_t offset, ptrdiff_t len);
ptrdiff_t hs_html_find_attr_escape(const uint8_t *buf, ptrdiff_t offset, ptrdiff_t len);

/* Width of the SIMD chunk the fused *_ascii scanners accumulate over, or 0
 * when the build compiles only the scalar loops (mirrors html_scan.c). */
#if !defined(WIREFORM_HTML_SCAN_NO_SIMD) \
    && (defined(__aarch64__) || defined(_M_ARM64) || defined(__SSE2__))
#  define HTML_SCAN_CHUNK 16
#else
#  define HTML_SCAN_CHUNK 0
#endif

/* ------------------------------------------------------------------ */
/* Reference implementations                                          */
/* ------------------------------------------------------------------ */

/* Stop-byte classes, one bit per scanner, indexed by byte value. */
enum {
    HS_TEXT = 1 << 0,     /* scan_text: < & \0 \r */
    HS_DQ = 1 << 1,       /* scan_dquote: " & */
    HS_SQ = 1 << 2,       /* scan_squote: ' & */
    HS_DQ_ONLY = 1 << 3,  /* skip_to_dquote: " */
    HS_SQ_ONLY = 1 << 4,  /* skip_to_squote: ' */
    HS_UNQ = 1 << 5,      /* scan_unquoted: > & SP TAB LF CR FF */
    HS_GT = 1 << 6,       /* scan_gt: > */
    HS_TEXT_ESC = 1 << 7, /* find_text_escape: < > & */
    HS_ATTR_ESC = 1 << 8, /* find_attr_escape: " & < > */
    HS_TAGNAME = 1 << 9   /* scan_tagname continues over these */
};

static inline unsigned ref_class(uint8_t b)
{
    unsigned c = 0;
    if (b == '<' || b == '&' || b == 0x00 || b == '\r') c |= HS_TEXT;
    if (b == '"' || b == '&') c |= HS_DQ;
    if (b == '\'' || b == '&') c |= HS_SQ;
    if (b == '"') c |= HS_DQ_ONLY;
    if (b == '\'') c |= HS_SQ_ONLY;
    if (b == '>' || b == '&' || b == ' ' || b == '\t' || b == '\n' || b == '\r'
        || b == '\f')
        c |= HS_UNQ;
    if (b == '>') c |= HS_GT;
    if (b == '<' || b == '>' || b == '&') c |= HS_TEXT_ESC;
    if (b == '"' || b == '&' || b == '<' || b == '>') c |= HS_ATTR_ESC;
    if ((b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z') || (b >= '0' && b <= '9')
        || b == '-' || b == '_' || b == ':' || b == '.' || b == '<')
        c |= HS_TAGNAME;
    return c;
}

/* First i in [off, end) whose byte is in `cls`, else end. */
static inline ptrdiff_t ref_find(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end,
                                 unsigned cls)
{
    for (ptrdiff_t i = off; i < end; i++) {
        if (ref_class(buf[i]) & cls) return i;
    }
    return end;
}

static inline ptrdiff_t ref_scan_tagname(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end)
{
    ptrdiff_t i = off;
    while (i < end && (ref_class(buf[i]) & HS_TAGNAME)) i++;
    return i;
}

static inline ptrdiff_t ref_scan_gt(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end)
{
    ptrdiff_t i = ref_find(buf, off, end, HS_GT);
    return i < end ? i + 1 : end;
}

static inline int ref_is_all_ascii(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end)
{
    for (ptrdiff_t i = off; i < end; i++) {
        if (buf[i] >= 0x80) return 0;
    }
    return 1;
}

/* The span [off, bound) whose ASCII-ness a fused scanner's flag reports
 * for a stop at `pos`: the whole 16-byte chunk holding pos when that chunk
 * was scanned by SIMD, else exactly [off, pos). */
static inline ptrdiff_t ref_ascii_bound(ptrdiff_t off, ptrdiff_t end, ptrdiff_t pos)
{
    if (HTML_SCAN_CHUNK == 0) return pos;
    ptrdiff_t chunk_end = off + ((pos - off) / 16 + 1) * 16;
    return chunk_end <= end ? chunk_end : pos;
}

/* Packed result a fused scanner must return for class `cls`. */
static inline ptrdiff_t ref_fused(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end,
                                  unsigned cls)
{
    ptrdiff_t pos = ref_find(buf, off, end, cls);
    int ascii = ref_is_all_ascii(buf, off, ref_ascii_bound(off, end, pos));
    return (pos << 1) | ascii;
}

/* Haskell's skipTagBS (Parse.hs), the definition skip_attrs implements. */
static inline ptrdiff_t ref_skip_attrs(const uint8_t *buf, ptrdiff_t off, ptrdiff_t end)
{
    ptrdiff_t i = off;
    while (i < end) {
        uint8_t b = buf[i];
        if (b == '>') return (i + 1) << 1;
        if (b == '/' && i + 1 < end && buf[i + 1] == '>') return ((i + 2) << 1) | 1;
        if (b == '"' || b == '\'') {
            i++;
            while (i < end && buf[i] != b) i++;
            if (i < end) i++;
            continue;
        }
        i++;
    }
    return (end + 1) << 1;
}

/* ------------------------------------------------------------------ */
/* Table of the plain scanners, for the shared sweeps                 */
/* ------------------------------------------------------------------ */

typedef ptrdiff_t (*hs_scan_fn)(const uint8_t *, ptrdiff_t, ptrdiff_t);

typedef enum {
    HS_KIND_FIND,    /* first stop byte or end */
    HS_KIND_FUSED,   /* (find << 1) | ascii */
    HS_KIND_GT,      /* one past the first '>' or end */
    HS_KIND_TAGNAME, /* first non-tagname byte */
    HS_KIND_ASCII,   /* is_all_ascii */
    HS_KIND_SKIP     /* skip_attrs */
} hs_kind;

typedef struct {
    const char *name;
    hs_scan_fn fn;
    hs_kind kind;
    unsigned cls;
} hs_scanner;

static inline ptrdiff_t hs_call_is_all_ascii(const uint8_t *b, ptrdiff_t o, ptrdiff_t e)
{
    return wireform_is_all_ascii(b, o, e);
}

static const hs_scanner hs_scanners[] = {
    { "scan_text", wireform_scan_text, HS_KIND_FIND, HS_TEXT },
    { "scan_text_ascii", wireform_scan_text_ascii, HS_KIND_FUSED, HS_TEXT },
    { "scan_dquote", wireform_scan_dquote, HS_KIND_FIND, HS_DQ },
    { "scan_dquote_ascii", wireform_scan_dquote_ascii, HS_KIND_FUSED, HS_DQ },
    { "scan_squote", wireform_scan_squote, HS_KIND_FIND, HS_SQ },
    { "scan_squote_ascii", wireform_scan_squote_ascii, HS_KIND_FUSED, HS_SQ },
    { "skip_to_dquote", wireform_skip_to_dquote, HS_KIND_FIND, HS_DQ_ONLY },
    { "skip_to_squote", wireform_skip_to_squote, HS_KIND_FIND, HS_SQ_ONLY },
    { "scan_unquoted", wireform_scan_unquoted, HS_KIND_FIND, HS_UNQ },
    { "scan_gt", wireform_scan_gt, HS_KIND_GT, HS_GT },
    { "find_text_escape", hs_html_find_text_escape, HS_KIND_FIND, HS_TEXT_ESC },
    { "find_attr_escape", hs_html_find_attr_escape, HS_KIND_FIND, HS_ATTR_ESC },
    { "scan_tagname", wireform_scan_tagname, HS_KIND_TAGNAME, HS_TAGNAME },
    { "is_all_ascii", hs_call_is_all_ascii, HS_KIND_ASCII, 0 },
    { "skip_attrs", wireform_skip_attrs, HS_KIND_SKIP, 0 },
};
#define HS_NSCANNERS (sizeof hs_scanners / sizeof hs_scanners[0])

/* What scanner s must return on buf[off, end). */
static inline ptrdiff_t hs_expected(const hs_scanner *s, const uint8_t *buf, ptrdiff_t off,
                                    ptrdiff_t end)
{
    switch (s->kind) {
    case HS_KIND_FIND: return ref_find(buf, off, end, s->cls);
    case HS_KIND_FUSED: return ref_fused(buf, off, end, s->cls);
    case HS_KIND_GT: return ref_scan_gt(buf, off, end);
    case HS_KIND_TAGNAME: return ref_scan_tagname(buf, off, end);
    case HS_KIND_ASCII: return ref_is_all_ascii(buf, off, end);
    case HS_KIND_SKIP: return ref_skip_attrs(buf, off, end);
    }
    return -1;
}

#endif
