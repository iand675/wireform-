/* Kernel API as wireform-websocket/src/Network/WebSocket/PerMessageDeflate.hs
 * imports it, a driver that calls it the way the Haskell does, and a
 * plain-zlib reference inflater.
 *
 * Caller contract (from PerMessageDeflate.hs):
 *   - wf_pmd_inflate_new(wbits) / wf_pmd_deflate_new(wbits, 1, 8): wbits
 *     is the negotiated *_max_window_bits, 8..15 after parsing (anything
 *     else returns NULL). NULL also means allocation failure; the Haskell
 *     throws PmdInitFailed and frees the other stream.
 *   - wf_pmd_free(NULL) is a no-op (freePmdContext swaps in nullPtr and
 *     may run twice).
 *   - wf_pmd_inflate(s, src, n, dst, cap, &produced): the first call per
 *     message passes the whole reassembled message payload with
 *     00 00 FF FF appended (so n >= 4 and src != NULL); dst is a
 *     malloc'd buffer of `cap` writable bytes (cap >= 64). While it
 *     returns WF_PMD_NEEDS_MORE_OUT the Haskell grows the buffer and
 *     calls again with src == NULL, n == 0, dst/cap = the new free tail;
 *     the stream keeps the unconsumed input. *produced is the number of
 *     bytes written to dst by this call and never exceeds cap.
 *     The payload is untrusted peer data: every input must give OK,
 *     NEEDS_MORE_OUT or BAD_INPUT without touching memory outside
 *     [src, src+n) and [dst, dst+cap).
 *   - wf_pmd_deflate: same calling pattern with the plaintext message;
 *     an empty message may arrive as src == NULL, n == 0 (BS.empty). The
 *     output ends with 00 00 FF FF which the Haskell strips. Every call
 *     must offer more than 6 bytes of room (zlib's own requirement for
 *     sync flushes; the Haskell offers >= 64), otherwise a flush that
 *     ends exactly at the end of the buffer is followed by another empty
 *     stored block on every resume, without end.
 *   - wf_pmd_reset_*: called between messages when *_no_context_takeover
 *     was negotiated.
 *   - src/dst lengths are size_t; zlib counts in 32-bit uInt, so the shim
 *     must split anything above 4 GiB itself.
 */
#ifndef WEBSOCKET_PMD_API_H
#define WEBSOCKET_PMD_API_H

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <zlib.h>

#include "wf_pmd.h"

static const uint8_t pmd_tail[4] = { 0x00, 0x00, 0xFF, 0xFF };

typedef struct {
    uint8_t *p;
    size_t n;
    size_t cap;
} pmd_buf;

static inline void pmd_buf_put(pmd_buf *b, const void *src, size_t n)
{
    if (b->n + n > b->cap) {
        size_t c = b->cap ? b->cap : 64;
        while (c < b->n + n) c *= 2;
        b->p = realloc(b->p, c);
        if (!b->p) abort();
        b->cap = c;
    }
    if (n) memcpy(b->p + b->n, src, n);
    b->n += n;
}

typedef int (*pmd_op)(wf_pmd_stream *, const uint8_t *, size_t, uint8_t *,
                      size_t, size_t *);

/* Drive one message through `op` the way driveZ does.
 *
 * step == 0: the Haskell policy, a first buffer of max(64, 2n) bytes and
 *   doubling on every WF_PMD_NEEDS_MORE_OUT.
 * step > 0: every call gets exactly `step` fresh bytes (step 0 calls are
 *   not possible here; see the tests for cap == 0).
 * Each call writes into its own exact-size buffer (wf_alloc), so a write
 * past `cap` faults under ASan / the guard page. Output accumulates in
 * *out. `limit` caps the total output (0 = none): once reached the
 * driver stops and returns WF_PMD_NEEDS_MORE_OUT. `bad` is set when an
 * invariant of the call protocol breaks (produced > cap, a resume that
 * makes no progress). */
static inline int pmd_drive(pmd_op op, wf_pmd_stream *s, const uint8_t *src,
                            size_t n, size_t step, size_t limit, pmd_buf *out,
                            int *bad)
{
    size_t cap = step ? step : (n * 2 > 64 ? n * 2 : 64);
    size_t total = 0;
    const uint8_t *in = src;
    size_t in_n = n;
    int stalls = 0;
    for (;;) {
        size_t room = step ? step : cap - total;
        if (limit && total + room > limit) room = limit - total;
        uint8_t *dst = wf_alloc(room);
        size_t produced = (size_t)-1;
        int rc = op(s, in, in_n, dst, room, &produced);
        if (produced > room) {
            *bad = 1;
            produced = 0;
        }
        pmd_buf_put(out, dst, produced);
        wf_free(dst, room);
        total += produced;
        in = NULL;
        in_n = 0;
        if (rc != WF_PMD_NEEDS_MORE_OUT) return rc;
        /* NEEDS_MORE_OUT promises the output buffer was full. */
        if (produced != room) *bad = 1;
        if (limit && total >= limit) return rc;
        stalls = produced ? 0 : stalls + 1;
        if (stalls > 4) {
            *bad = 1;
            return rc;
        }
        if (!step) cap *= 2;
    }
}

/* Haskell decompressMessage: payload ++ 00 00 FF FF, then drive. */
static inline int pmd_inflate_msg(wf_pmd_stream *s, const uint8_t *payload,
                                  size_t n, size_t step, size_t limit,
                                  pmd_buf *out, int *bad)
{
    uint8_t *m = wf_alloc(n + 4);
    if (n) memcpy(m, payload, n);
    memcpy(m + n, pmd_tail, 4);
    int rc = pmd_drive(wf_pmd_inflate, s, m, n + 4, step, limit, out, bad);
    wf_free(m, n + 4);
    return rc;
}

/* Haskell compressMessage: drive, then strip a trailing 00 00 FF FF. */
static inline int pmd_deflate_msg(wf_pmd_stream *s, const uint8_t *msg,
                                  size_t n, size_t step, pmd_buf *out,
                                  int *bad)
{
    size_t start = out->n;
    /* zlib (deflate(), Z_SYNC_FLUSH): "make sure that avail_out is
     * greater than six to avoid repeated flush markers due to
     * avail_out == 0 on return". With fewer bytes per call every call
     * that ends exactly on a completed flush starts another empty
     * stored block, forever. The Haskell's buffers start at 64 bytes. */
    if (step && step < 7) step = 7;
    uint8_t *m = n ? wf_dup(msg, n) : NULL;
    int rc = pmd_drive(wf_pmd_deflate, s, m, n, step, 0, out, bad);
    if (m) wf_free(m, n);
    size_t got = out->n - start;
    if (rc == WF_PMD_OK && got >= 4
        && memcmp(out->p + out->n - 4, pmd_tail, 4) == 0) {
        out->n -= 4;
    }
    return rc;
}

/* Reference: a plain zlib raw inflater fed one complete PMD message
 * (payload ++ tail) in a single call against a large buffer. Succeeds
 * when zlib consumes every byte and stops at a block boundary, which is
 * where every RFC 7692 message ends. BFINAL handling is not modelled:
 * callers only use it on streams without BFINAL blocks (our deflater
 * never emits them). Returns 1 on success. */
static inline int ref_inflate_msg(z_stream *zs, const uint8_t *payload,
                                  size_t n, pmd_buf *out)
{
    uint8_t *m = malloc(n + 4);
    if (!m) abort();
    if (n) memcpy(m, payload, n);
    memcpy(m + n, pmd_tail, 4);
    zs->next_in = m;
    zs->avail_in = (uInt)(n + 4);
    int rc = Z_OK;
    uint8_t chunk[16384];
    while (rc == Z_OK && zs->avail_in) {
        zs->next_out = chunk;
        zs->avail_out = sizeof chunk;
        rc = inflate(zs, Z_SYNC_FLUSH);
        pmd_buf_put(out, chunk, sizeof chunk - zs->avail_out);
    }
    /* Drain output still pending after the last input byte. */
    while (rc == Z_OK && zs->avail_out == 0) {
        zs->next_out = chunk;
        zs->avail_out = sizeof chunk;
        rc = inflate(zs, Z_SYNC_FLUSH);
        pmd_buf_put(out, chunk, sizeof chunk - zs->avail_out);
    }
    free(m);
    if (rc == Z_BUF_ERROR) rc = Z_OK;
    return rc == Z_OK && zs->avail_in == 0 && (zs->data_type & 128);
}

#endif
