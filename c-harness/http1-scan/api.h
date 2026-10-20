/* Kernel API of wireform-http1/cbits/http1_scan.c as the Haskell side
 * imports it, the caller contracts derived from those call sites, scalar
 * reference implementations, and a small HTTP/1.1 head model the tests and
 * fuzzers use to run the kernels the way the parser does.
 *
 * Call sites:
 *   Network.HTTP1.Parser          findNonToken / findNonFieldValue (pure,
 *                                 over a whole ByteString: offset is an
 *                                 index into it, len its length),
 *                                 lowercaseInPlace (src == dst),
 *                                 parseChunkHeader / parseChunkSize
 *                                 (hs_http1_parse_hex)
 *   Network.HTTP1.Internal.Ascii  asciiLower (disjoint dst of exactly len
 *                                 bytes), asciiIeq (equal lengths checked
 *                                 first, len > 0)
 *   Network.HTTP1.StreamingReader findCRLF / findCRLFCRLF /
 *                                 scanCrlfOrBareLf (find_cr / find_lf over a
 *                                 ring slice, 0 <= offset < avail)
 *
 * Contract shared by the scanners (find_cr, find_lf, find_non_token,
 * find_non_fieldvalue):
 *   - 0 <= offset and 0 <= len; offset may exceed len (a miss, returns len
 *     without reading memory).
 *   - Only bytes buf[offset .. len) are read; buf + len need not be
 *     readable and nothing before buf + offset is touched.
 *   - Returns the absolute index of the first matching byte in
 *     [offset, len), else len.
 *   - len <= INT_MAX: the Haskell wrappers narrow Int to CInt, so a
 *     ByteString of 2 GiB or more is outside the contract (HTTP heads and
 *     recv rings are far smaller). Every len up to INT_MAX itself is in.
 *
 * hs_http1_to_lower_ascii(src, dst, len): len <= 0 is a no-op; src and dst
 *   are exactly len bytes and either identical (in-place) or disjoint.
 * hs_http1_ascii_ieq(a, b, len): both exactly len bytes; len <= 0 -> 1.
 * hs_http1_parse_hex(buf, offset, len, out, consumed): reads hex digits
 *   from buf[offset .. len). Returns 1 with *out / *consumed set when
 *   1..16 digits were read, 0 with *out = 0 / *consumed = 0 when none,
 *   -1 with *consumed = 16 and *out untouched when a 17th digit follows
 *   (leading zeros count: chunk sizes are capped at 16 digits).
 */
#ifndef HTTP1_SCAN_API_H
#define HTTP1_SCAN_API_H

#include <limits.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

int hs_http1_find_cr(const void *buf, int offset, int len);
int hs_http1_find_lf(const void *buf, int offset, int len);
int hs_http1_find_non_token(const void *buf, int offset, int len);
int hs_http1_find_non_fieldvalue(const void *buf, int offset, int len);
void hs_http1_to_lower_ascii(const void *src, void *dst, int len);
int hs_http1_ascii_ieq(const void *a, const void *b, int len);
int hs_http1_parse_hex(const void *buf, int offset, int len, uint64_t *out, int *consumed);

/* ------------------------------------------------------------------ */
/* Byte classes, straight from the RFC grammar                         */
/* ------------------------------------------------------------------ */

/* RFC 9110 5.6.2: tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+"
 * / "-" / "." / "^" / "_" / "`" / "|" / "~" / DIGIT / ALPHA. */
static inline int ref_is_tchar(uint8_t b)
{
    static const char punct[] = "!#$%&'*+-.^_`|~";
    if (b >= '0' && b <= '9') return 1;
    if (b >= 'a' && b <= 'z') return 1;
    if (b >= 'A' && b <= 'Z') return 1;
    for (const char *p = punct; *p; p++) {
        if ((uint8_t)*p == b) return 1;
    }
    return 0;
}

/* RFC 9110 5.5: field-vchar = VCHAR / obs-text, plus SP / HTAB inside the
 * value. VCHAR = %x21-7E, obs-text = %x80-FF. */
static inline int ref_is_fv(uint8_t b)
{
    return b == 0x09 || b == 0x20 || (b >= 0x21 && b <= 0x7e) || b >= 0x80;
}

static inline int ref_is_hex(uint8_t b)
{
    return (b >= '0' && b <= '9') || (b >= 'a' && b <= 'f') || (b >= 'A' && b <= 'F');
}

static inline unsigned ref_hex_val(uint8_t b)
{
    if (b >= '0' && b <= '9') return (unsigned)(b - '0');
    if (b >= 'a' && b <= 'f') return (unsigned)(b - 'a' + 10);
    return (unsigned)(b - 'A' + 10);
}

static inline uint8_t ref_lower(uint8_t b)
{
    return (b >= 'A' && b <= 'Z') ? (uint8_t)(b + 32) : b;
}

/* ------------------------------------------------------------------ */
/* Kernel references                                                   */
/* ------------------------------------------------------------------ */

static inline int ref_find_eq(const uint8_t *p, int off, int len, uint8_t t)
{
    for (int i = off; i < len; i++) {
        if (p[i] == t) return i;
    }
    return len;
}

static inline int ref_find_non_token(const uint8_t *p, int off, int len)
{
    for (int i = off; i < len; i++) {
        if (!ref_is_tchar(p[i])) return i;
    }
    return len;
}

static inline int ref_find_non_fv(const uint8_t *p, int off, int len)
{
    for (int i = off; i < len; i++) {
        if (!ref_is_fv(p[i])) return i;
    }
    return len;
}

static inline void ref_to_lower(const uint8_t *src, uint8_t *dst, int len)
{
    for (int i = 0; i < len; i++) dst[i] = ref_lower(src[i]);
}

static inline int ref_ieq(const uint8_t *a, const uint8_t *b, int len)
{
    for (int i = 0; i < len; i++) {
        if (ref_lower(a[i]) != ref_lower(b[i])) return 0;
    }
    return 1;
}

/* Count every hex digit, then decide; independent of the kernel's
 * accumulate-as-you-go structure. */
static inline int ref_parse_hex(const uint8_t *p, int off, int len, uint64_t *out, int *consumed)
{
    int n = 0;
    while (off + n < len && ref_is_hex(p[off + n])) n++;
    if (n > 16) {
        *consumed = 16;
        return -1;
    }
    uint64_t v = 0;
    for (int k = 0; k < n; k++) v = v * 16 + ref_hex_val(p[off + k]);
    *consumed = n;
    *out = v;
    return n > 0;
}

/* Dispatch used by the table-driven tests and the scan fuzzer. */
enum { SCAN_CR, SCAN_LF, SCAN_NON_TOKEN, SCAN_NON_FV, SCAN_KINDS };

static inline int scan_kernel(int kind, const uint8_t *p, int off, int len)
{
    switch (kind) {
    case SCAN_CR: return hs_http1_find_cr(p, off, len);
    case SCAN_LF: return hs_http1_find_lf(p, off, len);
    case SCAN_NON_TOKEN: return hs_http1_find_non_token(p, off, len);
    default: return hs_http1_find_non_fieldvalue(p, off, len);
    }
}

static inline int scan_ref(int kind, const uint8_t *p, int off, int len)
{
    switch (kind) {
    case SCAN_CR: return ref_find_eq(p, off, len, 0x0d);
    case SCAN_LF: return ref_find_eq(p, off, len, 0x0a);
    case SCAN_NON_TOKEN: return ref_find_non_token(p, off, len);
    default: return ref_find_non_fv(p, off, len);
    }
}

/* 1 iff byte b stops a scan of this kind. */
static inline int scan_stops(int kind, uint8_t b)
{
    switch (kind) {
    case SCAN_CR: return b == 0x0d;
    case SCAN_LF: return b == 0x0a;
    case SCAN_NON_TOKEN: return !ref_is_tchar(b);
    default: return !ref_is_fv(b);
    }
}

/* ------------------------------------------------------------------ */
/* HTTP/1.1 head model                                                 */
/* ------------------------------------------------------------------ */
/*
 * Two implementations of the same head grammar:
 *
 *   kp_*  drives the C kernels exactly the way the Haskell parser does
 *         (StreamingReader.findCRLFCRLF / scanCrlfOrBareLf,
 *         Parser.parseRequestLine / parseHeaderBlock{,Lower} /
 *         parseOneHeader / skipOws / trimOwsEnd);
 *   rp_*  walks the bytes one at a time with the RFC predicates above.
 *
 * Grammar (RFC 9112 2.1, 2.2, 3, 5): lines end in CRLF; a bare LF ends the
 * scan with an error; a CR not followed by LF stays in the line and makes
 * a field line invalid; field-line = token ":" OWS value OWS where value
 * holds only field-vchar / SP / HTAB; a line starting with SP / HTAB
 * (obs-fold) is rejected; the header block handed to the field parser is
 * the bytes between the start line's CRLF and the CRLFCRLF, so its last
 * line has no CRLF.
 */

enum {
    H_OK = 0,
    H_NEED_MORE,     /* no terminator inside the buffer */
    H_BARE_LF,       /* LF without CR before the line's CRLF */
    H_BAD_REQ_LINE,  /* method not 1*tchar SP, or empty / missing target */
    H_BAD_NAME,      /* empty field name or name not followed by ':' */
    H_BAD_VALUE      /* obs-fold, bare CR, NUL / CTL / DEL / LF in value */
};

#define H_MAX_FIELDS 256

typedef struct {
    int ns, ne; /* name [ns, ne) */
    int vs, ve; /* value [vs, ve), OWS trimmed on both sides */
} h_field;

typedef struct {
    int status;
    int nfields;
    h_field f[H_MAX_FIELDS];
} h_fields;

/* Scan result of one line: >= 0 is the CR index of the line's CRLF. */
#define H_LINE_NEED_MORE (-1)
#define H_LINE_BARE_LF (-2)

/* StreamingReader.scanCrlfOrBareLf. */
static inline int kp_scan_line(const uint8_t *p, int i, int avail)
{
    if (avail < 2 && i >= avail) return H_LINE_NEED_MORE;
    while (i < avail) {
        int cr = hs_http1_find_cr(p, i, avail);
        int lf = hs_http1_find_lf(p, i, avail);
        if (lf < cr) return H_LINE_BARE_LF;
        if (cr >= avail) return H_LINE_NEED_MORE;
        if (cr + 1 >= avail) return H_LINE_NEED_MORE;
        if (p[cr + 1] == 0x0a) return cr;
        i = cr + 1;
    }
    return H_LINE_NEED_MORE;
}

static inline int rp_scan_line(const uint8_t *p, int i, int avail)
{
    for (int j = i; j < avail; j++) {
        if (p[j] == 0x0a) return H_LINE_BARE_LF;
        if (p[j] == 0x0d) {
            if (j + 1 >= avail) return H_LINE_NEED_MORE;
            if (p[j + 1] == 0x0a) return j;
        }
    }
    return H_LINE_NEED_MORE;
}

/* StreamingReader.findCRLFCRLF: index of the first CR of CRLFCRLF at or
 * after `from`, or -1. */
static inline int kp_find_head_end(const uint8_t *p, int from, int avail)
{
    if (avail < 4) return -1;
    int off = from;
    while (off + 4 <= avail) {
        int cr = hs_http1_find_cr(p, off, avail);
        if (cr + 4 > avail) return -1;
        if (p[cr + 1] == 0x0a && p[cr + 2] == 0x0d && p[cr + 3] == 0x0a) return cr;
        off = cr + 1;
    }
    return -1;
}

static inline int rp_find_head_end(const uint8_t *p, int from, int avail)
{
    for (int i = from; i + 4 <= avail; i++) {
        if (p[i] == '\r' && p[i + 1] == '\n' && p[i + 2] == '\r' && p[i + 3] == '\n') return i;
    }
    return -1;
}

/* Parser.parseRequestLine, structural part: method = 1*tchar then SP,
 * target = 1*(not SP) then SP, version = the rest (not validated here). */
typedef struct {
    int status;
    int meth_end, tgt_s, tgt_e, ver_s;
} h_reqline;

static inline h_reqline kp_request_line(const uint8_t *p, int len)
{
    h_reqline r = { H_BAD_REQ_LINE, 0, 0, 0, 0 };
    int me = hs_http1_find_non_token(p, 0, len);
    if (me == 0 || me >= len || p[me] != 0x20) return r;
    int ts = me + 1, te = ts;
    while (te < len && p[te] != 0x20) te++;
    if (te >= len || te == ts) return r;
    r.status = H_OK;
    r.meth_end = me;
    r.tgt_s = ts;
    r.tgt_e = te;
    r.ver_s = te + 1;
    return r;
}

static inline h_reqline rp_request_line(const uint8_t *p, int len)
{
    h_reqline r = { H_BAD_REQ_LINE, 0, 0, 0, 0 };
    int i = 0;
    while (i < len && ref_is_tchar(p[i])) i++;
    if (i == 0 || i == len || p[i] != ' ') return r;
    int ts = i + 1, te = ts;
    while (te < len && p[te] != ' ') te++;
    if (te == len || te == ts) return r;
    r.status = H_OK;
    r.meth_end = i;
    r.tgt_s = ts;
    r.tgt_e = te;
    r.ver_s = te + 1;
    return r;
}

static inline int h_is_ows(uint8_t b) { return b == 0x20 || b == 0x09; }

/* Parser.parseOneHeader over raw[off .. len); returns the next offset or a
 * negative H_* status. */
static inline int kp_one_header(const uint8_t *raw, int off0, int len, h_field *out)
{
    int ne = hs_http1_find_non_token(raw, off0, len);
    if (ne == off0) return -H_BAD_NAME;
    if (ne >= len || raw[ne] != 0x3a) return -H_BAD_NAME;
    int vs = ne + 1;
    while (vs < len && h_is_ows(raw[vs])) vs++;
    int ve0 = hs_http1_find_non_fieldvalue(raw, vs, len);
    int next;
    if (ve0 >= len) {
        next = len;
    } else {
        if (raw[ve0] != 0x0d) return -H_BAD_VALUE;
        if (ve0 + 1 >= len || raw[ve0 + 1] != 0x0a) return -H_BAD_VALUE;
        next = ve0 + 2;
    }
    int ve = ve0;
    while (ve > vs && h_is_ows(raw[ve - 1])) ve--;
    out->ns = off0;
    out->ne = ne;
    out->vs = vs;
    out->ve = ve;
    return next;
}

/* Parser.parseHeaderBlock / parseHeaderBlockLower. With lower != 0 the
 * names are lowercased in place by hs_http1_to_lower_ascii first, exactly
 * as parseHeaderBlockLower does (so raw must be writable). */
static inline void kp_header_block(uint8_t *raw, int len, int lower, h_fields *out)
{
    out->status = H_OK;
    out->nfields = 0;
    int off = 0;
    while (off < len) {
        if (raw[off] == 0x20 || raw[off] == 0x09) {
            out->status = H_BAD_VALUE;
            return;
        }
        if (lower) {
            int ne = hs_http1_find_non_token(raw, off, len);
            if (ne - off > 0) hs_http1_to_lower_ascii(raw + off, raw + off, ne - off);
        }
        h_field f;
        int next = kp_one_header(raw, off, len, &f);
        if (next < 0) {
            out->status = -next;
            return;
        }
        if (out->nfields < H_MAX_FIELDS) out->f[out->nfields] = f;
        out->nfields++;
        off = next;
    }
}

/* Byte-at-a-time field parser: one state machine over the whole block. */
static inline void rp_header_block(uint8_t *raw, int len, int lower, h_fields *out)
{
    out->status = H_OK;
    out->nfields = 0;
    int i = 0;
    while (i < len) {
        h_field f;
        if (h_is_ows(raw[i])) { /* obs-fold */
            out->status = H_BAD_VALUE;
            return;
        }
        f.ns = i;
        while (i < len && ref_is_tchar(raw[i])) {
            if (lower) raw[i] = ref_lower(raw[i]);
            i++;
        }
        f.ne = i;
        if (f.ne == f.ns || i == len || raw[i] != ':') {
            out->status = H_BAD_NAME;
            return;
        }
        i++;
        while (i < len && h_is_ows(raw[i])) i++;
        f.vs = i;
        int last_vchar_end = i; /* end of the value without trailing OWS */
        for (;;) {
            if (i == len) break; /* last line of the block: no CRLF */
            uint8_t b = raw[i];
            if (b == '\r') {
                if (i + 1 < len && raw[i + 1] == '\n') break;
                out->status = H_BAD_VALUE;
                return;
            }
            if (!ref_is_fv(b)) {
                out->status = H_BAD_VALUE;
                return;
            }
            i++;
            if (!h_is_ows(b)) last_vchar_end = i;
        }
        f.ve = last_vchar_end;
        if (out->nfields < H_MAX_FIELDS) out->f[out->nfields] = f;
        out->nfields++;
        if (i < len) i += 2; /* CRLF */
    }
}

/* Spec properties every accepted field must have, checked independently
 * of either parser: the name is 1*tchar followed by ':', the value has no
 * byte outside field-vchar / SP / HTAB, does not start or end with OWS,
 * and only OWS separates ':' from the value and the value from the line
 * end. Returns 1 when all hold. */
static inline int h_field_well_formed(const uint8_t *raw, int len, const h_field *f)
{
    if (!(0 <= f->ns && f->ns < f->ne && f->ne < len && raw[f->ne] == ':')) return 0;
    for (int k = f->ns; k < f->ne; k++) {
        if (!ref_is_tchar(raw[k])) return 0;
    }
    if (!(f->ne + 1 <= f->vs && f->vs <= f->ve && f->ve <= len)) return 0;
    for (int k = f->ne + 1; k < f->vs; k++) {
        if (!h_is_ows(raw[k])) return 0;
    }
    for (int k = f->vs; k < f->ve; k++) {
        if (!ref_is_fv(raw[k])) return 0;
    }
    if (f->ve > f->vs && (h_is_ows(raw[f->vs]) || h_is_ows(raw[f->ve - 1]))) return 0;
    int k = f->ve;
    while (k < len && h_is_ows(raw[k])) k++;
    return k == len || (raw[k] == '\r' && k + 1 < len && raw[k + 1] == '\n');
}

#endif
