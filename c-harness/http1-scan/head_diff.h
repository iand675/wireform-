/* Runs one buffer through the kernel-driven head model (kp_*) and the
 * byte-at-a-time reference (rp_*) from api.h and compares every result:
 * head framing, line splitting, request-line split, header-block parse
 * (plain and with in-place name lowercasing), the per-line field parse the
 * streaming reader does, plus the spec properties of accepted fields.
 *
 * head_diff returns NULL when everything agrees, else a short description
 * of the first disagreement. All buffers handed to the kernels are exact
 * size (wf_dup), so any read past a line or block faults. */
#ifndef HTTP1_HEAD_DIFF_H
#define HTTP1_HEAD_DIFF_H

#include "harness.h"
#include "api.h"

typedef struct {
    int head_end;   /* CR of CRLFCRLF, or -1 */
    int lines;      /* complete lines found by the line scanner */
    int line_stop;  /* how the line scan ended: H_LINE_* or empty-line CR */
    int req_status; /* request-line status of the first line, or -1 */
    int blk_status; /* header-block status, or -1 when no head end */
    int blk_fields;
} head_summary;

static inline int hd_fields_equal(const h_fields *a, const h_fields *b)
{
    if (a->status != b->status || a->nfields != b->nfields) return 0;
    if (a->status != H_OK) return 1;
    int n = a->nfields < H_MAX_FIELDS ? a->nfields : H_MAX_FIELDS;
    for (int k = 0; k < n; k++) {
        if (memcmp(&a->f[k], &b->f[k], sizeof a->f[k]) != 0) return 0;
    }
    return 1;
}

/* Parse raw[0 .. len) as a header block both ways, in both case modes. */
static inline const char *hd_block(const uint8_t *raw, int len, h_fields *res)
{
    static h_fields kf, rf;
    for (int lower = 0; lower <= 1; lower++) {
        uint8_t *kb = wf_dup(raw, (size_t)len);
        uint8_t *rb = wf_dup(raw, (size_t)len);
        kp_header_block(kb, len, lower, &kf);
        rp_header_block(rb, len, lower, &rf);
        const char *err = NULL;
        if (!hd_fields_equal(&kf, &rf)) err = "header block: kernel and reference disagree";
        else if (len && memcmp(kb, rb, (size_t)len) != 0) err = "header block: lowered bytes differ";
        if (!err && kf.status == H_OK) {
            int n = kf.nfields < H_MAX_FIELDS ? kf.nfields : H_MAX_FIELDS;
            for (int k = 0; k < n && !err; k++) {
                const h_field *f = &kf.f[k];
                if (!h_field_well_formed(raw, len, f)) err = "accepted field is not well formed";
                else if (lower) {
                    int nl = f->ne - f->ns;
                    for (int j = 0; j < nl; j++) {
                        if (kb[f->ns + j] != ref_lower(raw[f->ns + j])) err = "name not lowercased";
                    }
                    if (!hs_http1_ascii_ieq(raw + f->ns, kb + f->ns, nl)) err = "ieq(name, lower(name)) == 0";
                }
            }
            /* The fields tile the block: each starts where the previous
             * line's CRLF ends. */
            if (!err && n == kf.nfields && n > 0 && kf.f[0].ns != 0) err = "first field not at block start";
        }
        if (lower == 0) *res = kf;
        wf_free(kb, (size_t)len);
        wf_free(rb, (size_t)len);
        if (err) return err;
    }
    return NULL;
}

static inline const char *head_diff(const uint8_t *buf, int len, head_summary *s)
{
    head_summary dummy;
    if (!s) s = &dummy;
    memset(s, 0, sizeof *s);
    s->req_status = -1;
    s->blk_status = -1;

    /* Framing. */
    int hk = kp_find_head_end(buf, 0, len), hr = rp_find_head_end(buf, 0, len);
    if (hk != hr) return "head end differs";
    s->head_end = hk;
    if (hk >= 0) {
        /* Resuming the search after a partial scan (the reader resumes at
         * avail - 3 after a refill) finds the same terminator. */
        int from = hk > 3 ? hk - 3 : 0;
        if (kp_find_head_end(buf, from, len) != hk) return "resumed head end differs";
    }

    /* Line splitting (StreamingReader.scanCrlfOrBareLf). */
    int i = 0, first_line_end = -1;
    for (;;) {
        int lk = kp_scan_line(buf, i, len), lr = rp_scan_line(buf, i, len);
        if (lk != lr) return "line scan differs";
        if (lk < 0) {
            s->line_stop = lk;
            break;
        }
        if (first_line_end < 0) first_line_end = lk;
        if (lk == i) { /* empty line: end of head */
            s->line_stop = lk;
            break;
        }
        s->lines++;
        /* Per-line field parse for lines after the start line. */
        if (i > 0) {
            h_fields one;
            const char *e = hd_block(buf + i, lk - i, &one);
            if (e) return e;
        }
        i = lk + 2;
    }

    /* Request-line split of the first line. */
    if (first_line_end >= 0) {
        uint8_t *line = wf_dup(buf, (size_t)first_line_end);
        h_reqline a = kp_request_line(line, first_line_end);
        h_reqline b = rp_request_line(line, first_line_end);
        wf_free(line, (size_t)first_line_end);
        if (memcmp(&a, &b, sizeof a) != 0) return "request line differs";
        s->req_status = a.status;
    }

    /* Whole header block between the first CRLF and the CRLFCRLF. */
    if (hk >= 0) {
        int sl = 0;
        while (!(buf[sl] == '\r' && buf[sl + 1] == '\n')) sl++; /* sl <= hk */
        int bs = sl + 2 <= hk ? sl + 2 : hk;
        h_fields blk;
        const char *e = hd_block(buf + bs, hk - bs, &blk);
        if (e) return e;
        s->blk_status = blk.status;
        s->blk_fields = blk.nfields;
    }
    return NULL;
}

#endif
