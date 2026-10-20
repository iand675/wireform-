/* The direct read/write path (wf_ssl_read_into / wf_ssl_write_from) over
 * a real handshaken connection, driven by a little program.
 *
 * Input: [flags u8] then ops, each [op u8][operands]:
 *   0 write   [dir u8][len u16][seed u8]   `len` bytes from client (dir
 *                                          even) or server; on WANT_RETRY
 *                                          the peer is drained and the
 *                                          SAME (buf, len) retried
 *   1 read    [dir u8][cap u16]            read into an exact-size `cap`
 *                                          buffer (cap 0 allowed)
 *   2 inject  [dir u8][len u8][bytes]      raw bytes onto the wire towards
 *                                          one side (corrupts that stream)
 *   3 notify  [dir u8]                     wf_ssl_shutdown (close_notify)
 *   4 hangup  [dir u8]                     shutdown(SHUT_WR) of that side's
 *                                          socket: TCP-level EOF
 *   5 stale   [lib u8][reason u16]         leave an entry in the error
 *                                          queue before the next op
 *   6 errno   [v u8]                       leave a stale errno
 * flags bit 0 caps both sides at TLS 1.2.
 *
 * Invariants: return codes valid; out <= cap; every byte read equals the
 * byte written at that stream position (until the stream is corrupted);
 * after the writer's close_notify or hangup and all its data, the reader
 * gets EOF, and EOF is never reported before the data is all read; a
 * stale queue entry or errno never changes the outcome (idle reads are
 * WANT_RETRY, not FATAL/EOF).
 */
#include "harness.h"
#include "api.h"

enum { MAXSTREAM = 1 << 18 };

typedef struct stream {
    SSL *w, *r;          /* writer and reader of this direction */
    int wfd, rfd;        /* writer's and reader's socket */
    uint8_t *sent;       /* every plaintext byte written, in order */
    size_t nsent, nrecv;
    int corrupt;         /* raw bytes injected towards the reader */
    int notify_stuck;    /* close_notify could not be flushed (socket full):
                            wf_ssl_shutdown is best effort and does not retry */
    int closed;          /* writer sent close_notify or hung up */
    int dead;            /* reader saw FATAL or EOF */
    int wdead;           /* writer can no longer write (closed or failed) */
    int wfatal;          /* writer's write failed: its SSL is dead */
    struct stream *rev;  /* the opposite direction */
} stream;

/* Raw bytes were injected, or the writer's SSL died (reading the opposite
 * direction, or on a failed write) and may have sent a fatal alert into
 * this one, or the reader's SSL died on a failed write. */
static int disturbed(const stream *d)
{
    return d->corrupt || d->wfatal || d->rev->dead || d->rev->wfatal;
}

static uint16_t take_u16(wf_input *in)
{
    uint16_t lo = wf_take_u8(in);
    return (uint16_t)(lo | (uint16_t)wf_take_u8(in) << 8);
}

/* One read on direction d into an exact-size buffer of `cap` bytes. */
static int do_read(stream *d, size_t cap)
{
    uint8_t *buf = wf_alloc(cap);
    size_t out = 777;
    int rc = wf_ssl_read_into(d->r, buf, cap, &out);
    WF_FUZZ_ASSERT(tls_rc_valid(rc));
    if (rc == WF_SSL_OK) {
        WF_FUZZ_ASSERT(out <= cap);
        WF_FUZZ_ASSERT(cap == 0 || out > 0);
        if (!disturbed(d) && !d->dead) {
            WF_FUZZ_ASSERT(d->nrecv + out <= d->nsent);
            WF_FUZZ_ASSERT(memcmp(buf, d->sent + d->nrecv, out) == 0);
            d->nrecv += out;
        }
    } else {
        WF_FUZZ_ASSERT(out == 0);
        if (!disturbed(d) && !d->dead && cap > 0) {
            if (rc == WF_SSL_EOF) {
                /* EOF only once the writer is gone and all data is in. */
                WF_FUZZ_ASSERT(d->closed);
                WF_FUZZ_ASSERT(d->nrecv == d->nsent);
            }
            if (rc == WF_SSL_FATAL) WF_FUZZ_ASSERT(d->closed);
            if (wf_ssl_want(rc)) {
                /* Nothing pending can be readable data we lost. */
                WF_FUZZ_ASSERT(!d->closed || d->nrecv < d->nsent || d->notify_stuck);
            }
        }
        if (rc == WF_SSL_EOF || rc == WF_SSL_FATAL) d->dead = 1;
    }
    wf_free(buf, cap);
    return rc;
}

static void do_write(stream *d, stream *back, size_t len, uint8_t seed)
{
    if (d->closed || d->wdead || d->nsent + len > MAXSTREAM) return;
    uint8_t *src = wf_alloc(len);
    for (size_t i = 0; i < len; i++) src[i] = (uint8_t)(seed + i * 7 + (i >> 9));
    for (int spins = 0; spins < 200; spins++) {
        size_t out = 555;
        int rc = wf_ssl_write_from(d->w, src, len, &out);
        WF_FUZZ_ASSERT(tls_rc_valid(rc));
        if (rc == WF_SSL_OK) {
            WF_FUZZ_ASSERT(out == len);
            memcpy(d->sent + d->nsent, src, len);
            d->nsent += len;
            break;
        }
        WF_FUZZ_ASSERT(out == 0);
        if (!wf_ssl_want(rc)) {
            /* Only a peer that is gone or a poisoned session fails. */
            WF_FUZZ_ASSERT(disturbed(d) || d->dead || back->closed || back->dead
                           || back->corrupt);
            d->wdead = d->wfatal = 1;
            break;
        }
        /* Socket full: let the reader catch up, then retry identically. */
        if (do_read(d, 16384) != WF_SSL_OK && d->dead) {
            d->wdead = 1;
            break;
        }
    }
    wf_free(src, len);
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 1 || size > 8192) return 0;
    wf_input in = { data, size };
    int tls12 = wf_take_u8(&in) & 1;

    int fds[2];
    tls_socketpair(fds);
    tls_bigbufs(fds[0]);
    tls_bigbufs(fds[1]);
    SSL *c = wf_ssl_new_for_fd(tls_fuzz_client_ctx(tls12), fds[0]);
    SSL *s = wf_ssl_new_for_fd(tls_fuzz_server_ctx(tls12), fds[1]);
    WF_FUZZ_ASSERT(c && s);
    WF_FUZZ_ASSERT(wf_ssl_set_verify_hostname(c, "localhost") == WF_SSL_OK);
    WF_FUZZ_ASSERT(tls_handshake(c, s, NULL, NULL) == 0);
    WF_FUZZ_ASSERT(tls_alpn_ok(c) && tls_alpn_ok(s));

    stream st[2];
    memset(st, 0, sizeof st);
    st[0] = (stream){ .w = c, .r = s, .wfd = fds[0], .rfd = fds[1], .rev = &st[1] };
    st[1] = (stream){ .w = s, .r = c, .wfd = fds[1], .rfd = fds[0], .rev = &st[0] };
    st[0].sent = malloc(MAXSTREAM);
    st[1].sent = malloc(MAXSTREAM);
    if (!st[0].sent || !st[1].sent) abort();

    int steps = 0;
    while (in.n && steps++ < 48) {
        uint8_t op = wf_take_u8(&in) % 7;
        if (op == 5) {
            static const int libs[] = { ERR_LIB_SSL, ERR_LIB_SYS, ERR_LIB_X509, ERR_LIB_EVP };
            int lib = libs[wf_take_u8(&in) % 4];
            int reason = take_u16(&in) % 1200 + 1;
            ERR_raise(lib, reason);
            continue;
        }
        if (op == 6) {
            static const int errs[] = { EAGAIN, EINTR, ECONNRESET, EPIPE, 0 };
            errno = errs[wf_take_u8(&in) % 5];
            continue;
        }
        stream *d = &st[wf_take_u8(&in) & 1];
        stream *back = d == &st[0] ? &st[1] : &st[0];
        if (op == 0) {
            size_t len = take_u16(&in) % 40000;
            do_write(d, back, len, wf_take_u8(&in));
        } else if (op == 1) {
            do_read(d, take_u16(&in) % 20000);
        } else if (op == 2) {
            size_t len = wf_take_u8(&in);
            if (len > in.n) len = in.n;
            if (!d->closed && len && tls_push(d->wfd, in.p, len) > 0) d->corrupt = 1;
            in.p += len;
            in.n -= len;
        } else if (op == 3) {
            if (!d->wdead) {
                wf_ssl_shutdown(d->w);
                WF_FUZZ_ASSERT(ERR_peek_error() == 0);
                d->closed = 1;
                d->notify_stuck = SSL_want_write(d->w);
                d->wdead = 1; /* no application data after close_notify */
            }
        } else {
            shutdown(d->wfd, SHUT_WR);
            d->closed = 1;
            d->wdead = 1;
        }
    }
    /* Drain: every byte written arrives intact, then EOF if closed. */
    for (int k = 0; k < 2; k++) {
        stream *d = &st[k];
        for (int it = 0; it < 64 && !d->dead && d->nrecv < d->nsent; it++) do_read(d, 16384);
        if (!disturbed(d) && !d->dead) {
            WF_FUZZ_ASSERT(d->nrecv == d->nsent);
            int rc = do_read(d, 64);
            WF_FUZZ_ASSERT(rc == (d->closed ? WF_SSL_EOF : WF_SSL_WANT_RETRY)
                           || (d->notify_stuck && wf_ssl_want(rc)));
        }
    }
    WF_FUZZ_ASSERT(tls_last_error_ok(256));
    free(st[0].sent);
    free(st[1].sent);
    wf_ssl_free(c);
    wf_ssl_free(s);
    close(fds[0]);
    close(fds[1]);
    ERR_clear_error();
    return 0;
}
