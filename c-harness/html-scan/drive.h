/* Drivers shared by the html-scan tests and fuzz targets: every scanner
 * checked against its reference at one offset, the Haskell escape loop
 * (Encode.hs) with a round trip, and a tokenizer walk that chains the
 * scanners the way Parse.hs does. Each returns 0 on success or the source
 * line of the first failed check, so tests can report it and fuzz targets
 * can abort on it. */
#ifndef HTML_SCAN_DRIVE_H
#define HTML_SCAN_DRIVE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "api.h"

#define HS_EXPECT(cond)                                                    \
    do {                                                                   \
        if (!(cond)) {                                                     \
            fprintf(stderr, "html-scan: %s:%d: %s\n", __FILE__, __LINE__, #cond); \
            return __LINE__;                                               \
        }                                                                  \
    } while (0)

/* Scanner s on buf[off, end) agrees with its reference. A fused scanner
 * must also agree with its plain twin (the entry just before it). */
static inline int hs_check_one(size_t si, const uint8_t *buf, ptrdiff_t off, ptrdiff_t end)
{
    const hs_scanner *s = &hs_scanners[si];
    ptrdiff_t got = s->fn(buf, off, end);
    ptrdiff_t want = hs_expected(s, buf, off, end);
    if (got != want) {
        fprintf(stderr, "html-scan: %s(off=%td, end=%td) = %td, want %td\n", s->name, off,
                end, got, want);
        return __LINE__;
    }
    if (s->kind == HS_KIND_FUSED) {
        ptrdiff_t pos = got >> 1;
        HS_EXPECT(hs_scanners[si - 1].fn(buf, off, end) == pos);
        /* Soundness of the flag whatever the chunking. */
        HS_EXPECT(!(got & 1) || ref_is_all_ascii(buf, off, pos));
    }
    return 0;
}

/* Every scanner whose bit is set in `which`. */
static inline int hs_check_all(unsigned which, const uint8_t *buf, ptrdiff_t off, ptrdiff_t end)
{
    for (size_t i = 0; i < HS_NSCANNERS; i++) {
        if (!(which & (1u << i))) continue;
        int rc = hs_check_one(i, buf, off, end);
        if (rc) return rc;
    }
    return 0;
}

/* Where the next scan starts after scanner s returned r: past the stop
 * byte, as the Haskell loops do after consuming it. */
static inline ptrdiff_t hs_next_start(const hs_scanner *s, ptrdiff_t from, ptrdiff_t r)
{
    ptrdiff_t pos;
    switch (s->kind) {
    case HS_KIND_FUSED: pos = r >> 1; break;
    case HS_KIND_SKIP: pos = (r >> 1) - 1; break;
    case HS_KIND_GT: pos = r - 1; break;
    case HS_KIND_ASCII: pos = from * 2; break; /* O(n) per call: thin out */
    default: pos = r; break;
    }
    return (pos > from ? pos : from) + 1;
}

/* Chains each scanner in `which` from `off` to the end, every call
 * checked: one pass visits every stop byte of the input. */
static inline int hs_check_chain(unsigned which, const uint8_t *buf, ptrdiff_t off, ptrdiff_t end)
{
    for (size_t si = 0; si < HS_NSCANNERS; si++) {
        if (!(which & (1u << si))) continue;
        const hs_scanner *s = &hs_scanners[si];
        for (ptrdiff_t i = off; i <= end;) {
            int rc = hs_check_one(si, buf, i, end);
            if (rc) return rc;
            i = hs_next_start(s, i, s->fn(buf, i, end));
        }
    }
    return 0;
}

/* ------------------------------------------------------------------ */
/* Escape loop (escapeText / escapeAttrValue in Encode.hs)            */
/* ------------------------------------------------------------------ */

static inline const char *hs_entity(uint8_t b)
{
    switch (b) {
    case '<': return "&lt;";
    case '>': return "&gt;";
    case '&': return "&amp;";
    case '"': return "&quot;";
    }
    return NULL;
}

typedef struct {
    uint8_t *p;
    size_t n, cap;
} hs_out;

static inline void hs_put(hs_out *o, const void *src, size_t n)
{
    if (o->n + n > o->cap) {
        o->cap = (o->n + n) * 2 + 16;
        o->p = realloc(o->p, o->cap);
        if (!o->p) abort();
    }
    if (n) memcpy(o->p + o->n, src, n);
    o->n += n;
}

/* Runs the Haskell loop with the kernel, and a byte-at-a-time escaper,
 * then unescapes the kernel output and compares with the input. */
static inline int hs_check_escape(int attr, const uint8_t *buf, ptrdiff_t len)
{
    hs_scan_fn find = attr ? hs_html_find_attr_escape : hs_html_find_text_escape;
    unsigned cls = attr ? HS_ATTR_ESC : HS_TEXT_ESC;
    hs_out got = { 0 }, want = { 0 }, back = { 0 };
    int rc = 0;

    ptrdiff_t off = 0;
    while (off < len) {
        ptrdiff_t pos = find(buf, off, len);
        if (pos < off || pos > len || pos != ref_find(buf, off, len, cls)) {
            fprintf(stderr, "html-scan: escape(attr=%d) off=%td got %td\n", attr, off, pos);
            rc = __LINE__;
            goto out;
        }
        if (pos >= len) {
            hs_put(&got, buf + off, (size_t)(len - off));
            break;
        }
        hs_put(&got, buf + off, (size_t)(pos - off));
        const char *e = hs_entity(buf[pos]);
        hs_put(&got, e, strlen(e));
        off = pos + 1;
    }

    for (ptrdiff_t i = 0; i < len; i++) {
        const char *e = (ref_class(buf[i]) & cls) ? hs_entity(buf[i]) : NULL;
        if (e) hs_put(&want, e, strlen(e));
        else hs_put(&want, buf + i, 1);
    }
    if (got.n != want.n || (got.n && memcmp(got.p, want.p, got.n) != 0)) {
        fprintf(stderr, "html-scan: escape(attr=%d) output differs from reference\n", attr);
        rc = __LINE__;
        goto out;
    }

    /* Escaped output has no raw stop byte left, and unescapes to the input. */
    for (size_t i = 0; i < got.n; i++) {
        uint8_t b = got.p[i];
        if (b == '&') {
            static const char *const ents[] = { "&lt;", "&gt;", "&amp;", "&quot;" };
            static const uint8_t raw[] = { '<', '>', '&', '"' };
            size_t k;
            for (k = 0; k < 4; k++) {
                size_t el = strlen(ents[k]);
                if (got.n - i >= el && memcmp(got.p + i, ents[k], el) == 0) break;
            }
            if (k == 4) {
                rc = __LINE__;
                goto out;
            }
            hs_put(&back, &raw[k], 1);
            i += strlen(ents[k]) - 1;
        } else {
            if (ref_class(b) & cls) {
                rc = __LINE__;
                goto out;
            }
            hs_put(&back, &b, 1);
        }
    }
    if (back.n != (size_t)len || (len && memcmp(back.p, buf, (size_t)len) != 0)) rc = __LINE__;

out:
    free(got.p);
    free(want.p);
    free(back.p);
    return rc;
}

/* ------------------------------------------------------------------ */
/* Tokenizer walk (the Parse.hs loops that call the kernels)          */
/* ------------------------------------------------------------------ */

static inline int hs_is_alpha(uint8_t b)
{
    return (b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z');
}

static inline int hs_is_ws(uint8_t b)
{
    return b == ' ' || b == '\t' || b == '\n' || b == '\r' || b == '\f';
}

/* Indices into hs_scanners (api.h). */
#define HS_I_TEXT 0
#define HS_I_TEXT_ASCII 1
#define HS_I_DQ 2
#define HS_I_DQ_ASCII 3
#define HS_I_SQ 4
#define HS_I_SQ_ASCII 5
#define HS_I_SKIP_DQ 6
#define HS_I_SKIP_SQ 7
#define HS_I_UNQ 8
#define HS_I_GT 9
#define HS_I_TEXT_ESC 10
#define HS_I_ATTR_ESC 11
#define HS_I_TAGNAME 12
#define HS_I_ASCII 13
#define HS_I_SKIP 14

/* Consumed length of an entity reference after '&' (parseEntityRefInAttr
 * stand-in): alphanumerics and '#', then an optional ';', inside the same
 * 64-byte window the Haskell loop uses. */
static inline ptrdiff_t hs_entity_len(const uint8_t *buf, ptrdiff_t from, ptrdiff_t end)
{
    ptrdiff_t win = from + 64 < end ? from + 64 : end;
    ptrdiff_t i = from;
    while (i < win && (hs_is_alpha(buf[i]) || (buf[i] >= '0' && buf[i] <= '9') || buf[i] == '#'))
        i++;
    if (i < win && buf[i] == ';') i++;
    return i - from;
}

/* One quoted or unquoted attribute value starting at i (just past the
 * opening quote for quoted values). Returns the offset after the value. */
static inline int hs_walk_value(const uint8_t *buf, ptrdiff_t i, ptrdiff_t len, uint8_t quote,
                                ptrdiff_t *after)
{
    size_t fused = quote == '"' ? HS_I_DQ_ASCII : quote == '\'' ? HS_I_SQ_ASCII : 0;
    size_t plain = quote == '"' ? HS_I_DQ : quote == '\'' ? HS_I_SQ : HS_I_UNQ;
    int rc;
    ptrdiff_t h;
    if (fused) {
        if ((rc = hs_check_one(fused, buf, i, len))) return rc;
        h = hs_scanners[fused].fn(buf, i, len) >> 1;
    } else {
        if ((rc = hs_check_one(plain, buf, i, len))) return rc;
        h = hs_scanners[plain].fn(buf, i, len);
    }
    while (h < len && buf[h] == '&') {
        ptrdiff_t next = h + 1 + hs_entity_len(buf, h + 1, len);
        HS_EXPECT(next > h && next <= len);
        if ((rc = hs_check_one(plain, buf, next, len))) return rc;
        h = hs_scanners[plain].fn(buf, next, len);
        HS_EXPECT(h >= next && h <= len);
    }
    *after = (h < len && quote && buf[h] == quote) ? h + 1 : h;
    return 0;
}

/* Attributes of a start tag from just after its name, readTagAttrsBS
 * style. Stops at '>', "/>" or the end; the result is only a progress
 * point, the tag end proper comes from skip_attrs. */
static inline int hs_walk_attrs(const uint8_t *buf, ptrdiff_t i, ptrdiff_t len)
{
    for (int guard = 0; i < len; guard++) {
        HS_EXPECT(guard <= len);
        while (i < len && hs_is_ws(buf[i])) i++;
        if (i >= len || buf[i] == '>') return 0;
        if (buf[i] == '/') {
            i++;
            continue;
        }
        ptrdiff_t name = i;
        while (i < len && !hs_is_ws(buf[i]) && buf[i] != '=' && buf[i] != '>' && buf[i] != '/')
            i++;
        if (i == name) i++; /* a stray '=' */
        while (i < len && hs_is_ws(buf[i])) i++;
        if (i >= len || buf[i] != '=') continue;
        i++;
        while (i < len && hs_is_ws(buf[i])) i++;
        if (i >= len) return 0;
        uint8_t q = buf[i];
        ptrdiff_t after;
        int rc = (q == '"' || q == '\'') ? hs_walk_value(buf, i + 1, len, q, &after)
                                         : hs_walk_value(buf, i, len, 0, &after);
        if (rc) return rc;
        HS_EXPECT(after >= i && after <= len);
        if (after == i) i++; /* unquoted value that is a lone delimiter */
        else i = after;
    }
    return 0;
}

static inline int hs_check_tokenize(const uint8_t *buf, ptrdiff_t len)
{
    ptrdiff_t off = 0;
    int rc;
    while (off < len) {
        uint8_t b = buf[off];
        if (!(ref_class(b) & HS_TEXT)) {
            /* text run: the first byte is known not to stop, scan the rest */
            if ((rc = hs_check_one(HS_I_TEXT_ASCII, buf, off + 1, len))) return rc;
            ptrdiff_t end = wireform_scan_text_ascii(buf, off + 1, len) >> 1;
            HS_EXPECT(end > off && end <= len);
            if ((rc = hs_check_one(HS_I_ASCII, buf, off, end))) return rc;
            off = end;
        } else if (b == '<' && off + 1 < len && hs_is_alpha(buf[off + 1])) {
            if ((rc = hs_check_one(HS_I_TAGNAME, buf, off + 1, len))) return rc;
            ptrdiff_t name_end = wireform_scan_tagname(buf, off + 1, len);
            HS_EXPECT(name_end > off + 1 && name_end <= len);
            if ((rc = hs_walk_attrs(buf, name_end, len))) return rc;
            if ((rc = hs_check_one(HS_I_SKIP, buf, name_end, len))) return rc;
            ptrdiff_t after = wireform_skip_attrs(buf, name_end, len) >> 1;
            HS_EXPECT(after > name_end && after <= len + 1);
            off = after;
        } else if (b == '<' && off + 2 < len && buf[off + 1] == '/'
                   && hs_is_alpha(buf[off + 2])) {
            if ((rc = hs_check_one(HS_I_TAGNAME, buf, off + 2, len))) return rc;
            ptrdiff_t name_end = wireform_scan_tagname(buf, off + 2, len);
            if ((rc = hs_check_one(HS_I_GT, buf, name_end, len))) return rc;
            ptrdiff_t after = wireform_scan_gt(buf, name_end, len);
            HS_EXPECT(after >= name_end && after <= len);
            off = after > off ? after : off + 1;
        } else {
            off++; /* '&', NUL, CR, or a '<' that opens no tag */
        }
    }
    return 0;
}

#endif
