/* A TLS server (wf_ssl_new_for_fd + wf_ssl_accept, with the ALPN select
 * callback installed) fed arbitrary client bytes over a socketpair.
 *
 * Input: [flags u8][client bytes...]. flags bit 0 caps the server at TLS
 * 1.2; bits 1-2 split the client bytes into 1, 2 or 4 deliveries with an
 * accept step between each; bit 3 leaves the connection open (no EOF)
 * after the last delivery; bits 4-7 pick a wf_ssl_last_error buffer size.
 *
 * Invariants: every return is one of the four codes; a peer that has
 * stopped sending ends in WANT_RETRY (open) or EOF/FATAL (closed), never
 * OK unless the handshake really completed; a negotiated ALPN protocol is
 * one the server offered; wf_ssl_last_error output is well formed and
 * drains the queue. The seeds are real ClientHellos (TLS 1.2 / 1.3, with
 * SNI and ALPN), so mutations reach the ALPN select callback.
 */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 1 || size > 65536) return 0;
    uint8_t flags = data[0];
    const uint8_t *in = data + 1;
    size_t n = size - 1;
    int parts = 1 << ((flags >> 1) & 3);
    if (parts == 8) parts = 4;
    int keep_open = (flags >> 3) & 1;
    size_t errcap = (size_t)((flags >> 4) * 37);

    int fds[2];
    tls_socketpair(fds);
    tls_bigbufs(fds[0]);
    tls_bigbufs(fds[1]);
    SSL *s = wf_ssl_new_for_fd(tls_fuzz_server_ctx(flags & 1), fds[1]);
    WF_FUZZ_ASSERT(s != NULL);

    size_t sent = 0;
    int rc = WF_SSL_WANT_RETRY;
    for (int p = 1; p <= parts && wf_ssl_want(rc); p++) {
        size_t upto = n * (size_t)p / (size_t)parts;
        sent += tls_push(fds[0], in + sent, upto - sent);
        if (p == parts && !keep_open) shutdown(fds[0], SHUT_WR);
        for (int step = 0; step < 4 && wf_ssl_want(rc); step++) {
            rc = wf_ssl_accept(s);
            WF_FUZZ_ASSERT(tls_rc_valid(rc));
            tls_drain(fds[0]); /* the server's flight */
        }
    }
    if (!keep_open && wf_ssl_want(rc)) {
        /* Everything delivered and the peer gone: must not keep waiting. */
        rc = wf_ssl_accept(s);
        WF_FUZZ_ASSERT(!wf_ssl_want(rc) || sent < n);
    }
    if (rc == WF_SSL_OK) {
        WF_FUZZ_ASSERT(tls_alpn_ok(s));
        uint8_t *buf = wf_alloc(4096);
        size_t out = 0;
        int r = wf_ssl_read_into(s, buf, 4096, &out);
        WF_FUZZ_ASSERT(tls_rc_valid(r));
        WF_FUZZ_ASSERT(out <= 4096);
        wf_free(buf, 4096);
    }
    WF_FUZZ_ASSERT(tls_last_error_ok(errcap));
    wf_ssl_shutdown(s);
    WF_FUZZ_ASSERT(ERR_peek_error() == 0);
    wf_ssl_free(s);
    close(fds[0]);
    close(fds[1]);
    ERR_clear_error();
    return 0;
}
