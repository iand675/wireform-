/* Tests for wireform-http1 http1_send.c over real sockets and files:
 * socketpairs (blocking and non-blocking with a small SO_SNDBUF), temp
 * files, signals that interrupt blocked calls (EINTR), full buffers
 * (EAGAIN), offsets and counts at and past end of file, zero lengths and
 * invalid descriptors. The byte stream that arrives is always compared
 * with what the shims report as sent. */
#include "harness.h"
#include "api.h"

#include <stdatomic.h>

static void ignore_sigpipe(void)
{
    signal(SIGPIPE, SIG_IGN); /* what the GHC RTS does */
}

/* SIGUSR1 handler without SA_RESTART, so blocked send / sendfile calls
 * fail with EINTR (or return a short count) instead of restarting. */
static volatile sig_atomic_t usr1_hits;
static void on_usr1(int sig)
{
    (void)sig;
    usr1_hits++;
}
static void install_usr1(void)
{
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_usr1;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    if (sigaction(SIGUSR1, &sa, NULL) != 0) sx_die("sigaction");
}

static uint8_t *pattern(size_t n, uint64_t seed)
{
    uint8_t *p = malloc(n ? n : 1);
    wf_rng r = { seed };
    wf_rng_fill(&r, p, n);
    return p;
}

/* ------------------------------------------------------------------ */
/* send_more / send_all                                                */
/* ------------------------------------------------------------------ */

static void send_basic(void)
{
    int sv[2];
    sx_pair(sv);
    static const char msg[] = "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\n";
    size_t n = sizeof msg - 1;
    uint8_t *b = wf_dup(msg, n);
    WF_CHECK_EQ(hs_http1_send_more(sv[0], b, n), n);
    WF_CHECK_EQ(hs_http1_send_all(sv[0], b, n), n);
    WF_CHECK_EQ(hs_http1_send_more(sv[0], b, 0), 0);
    WF_CHECK_EQ(hs_http1_send_all(sv[0], b, 0), 0);
    uint8_t got[256];
    size_t have = 0;
    sx_nonblock(sv[1], 1);
    sx_drain(sv[1], got, &have, sizeof got);
    WF_CHECK_EQ(have, 2 * n);
    WF_CHECK_MEM(got, b, n);
    WF_CHECK_MEM(got + n, b, n);
    wf_free(b, n);
    close(sv[0]);
    close(sv[1]);
}

static void send_bad_fds(void)
{
    uint8_t x[4] = { 1, 2, 3, 4 };
    WF_CHECK_EQ(hs_http1_send_more(-1, x, 4), -EBADF);
    WF_CHECK_EQ(hs_http1_send_all(-1, x, 4), -EBADF);
    int sv[2];
    sx_pair(sv);
    close(sv[0]);
    WF_CHECK_EQ(hs_http1_send_more(sv[0], x, 4), -EBADF); /* closed fd */
    close(sv[1]);
    int p[2];
    if (pipe(p) != 0) sx_die("pipe");
    WF_CHECK_EQ(hs_http1_send_more(p[1], x, 4), -ENOTSOCK);
    WF_CHECK_EQ(hs_http1_send_all(p[1], x, 4), -ENOTSOCK);
    int f = sx_tempfile(x, 4);
    WF_CHECK_EQ(hs_http1_send_more(f, x, 4), -ENOTSOCK);
    close(f);
    close(p[0]);
    close(p[1]);
}

/* Peer gone: -EPIPE (SIGPIPE ignored like the RTS does), or -ECONNRESET. */
static void send_closed_peer(void)
{
    int sv[2];
    sx_pair(sv);
    close(sv[1]);
    uint8_t x[64] = { 0 };
    hs_ssize_t a = hs_http1_send_more(sv[0], x, sizeof x);
    hs_ssize_t b = hs_http1_send_all(sv[0], x, sizeof x);
    WF_CHECK(a == -EPIPE || a == -ECONNRESET);
    WF_CHECK(b == -EPIPE || b == -ECONNRESET);
    close(sv[0]);
}

/* Non-blocking socket with a small buffer: send_more takes what fits, then
 * reports -EAGAIN; send_all returns a short count after partial progress
 * and -EAGAIN when nothing fits. Every accepted byte arrives, in order. */
static void send_nonblocking_partial(void)
{
    enum { N = 1 << 20 };
    uint8_t *src = pattern(N, 11);
    uint8_t *got = malloc(N);
    for (int which = 0; which < 2; which++) {
        int sv[2];
        sx_pair(sv);
        sx_sndbuf(sv[0], 4096);
        sx_nonblock(sv[0], 1);
        sx_nonblock(sv[1], 1);
        size_t sent = 0, have = 0;
        int eagains = 0, shorts = 0;
        while (sent < N) {
            size_t want = N - sent;
            hs_ssize_t r = which ? hs_http1_send_all(sv[0], src + sent, want)
                                 : hs_http1_send_more(sv[0], src + sent, want);
            if (hs_is_eagain(r)) {
                eagains++;
                sx_drain(sv[1], got, &have, N);
                /* Bytes arrived that no call reported as sent (send_all
                 * used to drop its partial count on EAGAIN). */
                WF_CHECK(have <= sent);
                if (have > sent) break;
                continue;
            }
            WF_CHECK(r > 0 && (size_t)r <= want);
            if (r <= 0) break;
            if ((size_t)r < want) shorts++;
            sent += (size_t)r;
        }
        while (have < sent) {
            if (!sx_drain(sv[1], got, &have, N)) sx_msleep(1);
        }
        WF_CHECK_EQ(have, sent);
        WF_CHECK_MEM(got, src, N);
        WF_CHECK(eagains > 0);
        WF_CHECK(shorts > 0);
        /* Buffer full: the very first send of a call fails, no progress. */
        sx_fill(sv[0]);
        WF_CHECK(hs_is_eagain(hs_http1_send_more(sv[0], src, 100)));
        WF_CHECK(hs_is_eagain(hs_http1_send_all(sv[0], src, 100)));
        close(sv[0]);
        close(sv[1]);
    }
    free(src);
    free(got);
}

/* Blocking socket, large payload, a reader thread: send_all writes it
 * all; send_more may return short counts that the caller loops on. */
typedef struct {
    int fd;
    uint8_t *buf;
    size_t want, have;
} reader_arg;

static void *reader_main(void *p)
{
    reader_arg *a = p;
    while (a->have < a->want) {
        ssize_t r = read(a->fd, a->buf + a->have, a->want - a->have);
        if (r > 0) a->have += (size_t)r;
        else if (r == 0) break;
        else if (errno != EINTR) break;
    }
    return NULL;
}

static void send_blocking_large(void)
{
    enum { N = 3 << 20 };
    uint8_t *src = pattern(N, 12);
    for (int which = 0; which < 2; which++) {
        int sv[2];
        sx_pair(sv);
        sx_sndbuf(sv[0], 8192);
        reader_arg ra = { sv[1], malloc(N), N, 0 };
        pthread_t t;
        pthread_create(&t, NULL, reader_main, &ra);
        size_t sent = 0;
        if (which) {
            WF_CHECK_EQ(hs_http1_send_all(sv[0], src, N), N);
            sent = N;
        } else {
            while (sent < N) {
                hs_ssize_t r = hs_http1_send_more(sv[0], src + sent, N - sent);
                WF_CHECK(r > 0);
                if (r <= 0) break;
                sent += (size_t)r;
            }
        }
        pthread_join(t, NULL);
        WF_CHECK_EQ(ra.have, N);
        WF_CHECK_MEM(ra.buf, src, N);
        free(ra.buf);
        close(sv[0]);
        close(sv[1]);
    }
    free(src);
}

/* ------------------------------------------------------------------ */
/* EINTR: a sender blocked on a full buffer gets signalled repeatedly   */
/* while the receiver drains slowly. Whatever each call reports must be */
/* exactly the bytes that arrive: nothing lost, nothing sent twice.     */
/* ------------------------------------------------------------------ */

enum { OP_SEND_MORE, OP_SEND_ALL, OP_SENDFILE };

typedef struct {
    int op, sock, file;
    const uint8_t *src;
    int64_t offset;
    size_t count;
    hs_ssize_t result;
    atomic_int done;
} sender_arg;

static void *sender_main(void *p)
{
    sender_arg *a = p;
    switch (a->op) {
    case OP_SEND_MORE: a->result = hs_http1_send_more(a->sock, a->src, a->count); break;
    case OP_SEND_ALL: a->result = hs_http1_send_all(a->sock, a->src, a->count); break;
    default: a->result = hs_http1_sendfile(a->sock, a->file, a->offset, a->count); break;
    }
    atomic_store(&a->done, 1);
    return NULL;
}

static void eintr_case(int op)
{
    enum { N = 256 * 1024, OFF = 1000 };
    uint8_t *src = pattern(N + OFF, 13 + (uint64_t)op);
    int file = sx_tempfile(src, N + OFF);
    int sv[2];
    sx_pair(sv);
    sx_sndbuf(sv[0], 4096);
    /* Pre-fill so the call blocks before sending anything. */
    sx_nonblock(sv[0], 1);
    size_t prefill = sx_fill(sv[0]);
    sx_nonblock(sv[0], 0);
    sx_nonblock(sv[1], 1);

    sender_arg a;
    memset(&a, 0, sizeof a);
    a.op = op;
    a.sock = sv[0];
    a.file = file;
    a.src = src + OFF;
    a.offset = OFF;
    a.count = N;
    atomic_init(&a.done, 0);
    pthread_t t;
    pthread_create(&t, NULL, sender_main, &a);

    size_t cap = prefill + N + 1;
    uint8_t *got = malloc(cap);
    size_t have = 0;
    /* Phase 1: signals while blocked with zero progress (EINTR, retried).
     * Phase 2: drain a little at a time between signals, so signals also
     * land after partial progress. */
    for (int k = 0; k < 400 && !atomic_load(&a.done); k++) {
        pthread_kill(t, SIGUSR1);
        sx_msleep(1);
        if (k >= 10) {
            uint8_t chunk[1500];
            ssize_t r = read(sv[1], chunk, sizeof chunk);
            if (r > 0) {
                if (have + (size_t)r <= cap) memcpy(got + have, chunk, (size_t)r);
                have += (size_t)r;
            }
        }
    }
    /* Let it finish if it is still going. */
    while (!atomic_load(&a.done)) {
        if (!sx_drain(sv[1], got, &have, cap)) sx_msleep(1);
    }
    pthread_join(t, NULL);
    sx_drain(sv[1], got, &have, cap);

    WF_CHECK(a.result > 0 && (size_t)a.result <= N);
    if (a.result > 0) {
        size_t sent = (size_t)a.result;
        if (op == OP_SEND_ALL) WF_CHECK_EQ(sent, N);
        WF_CHECK_EQ(have, prefill + sent);
        if (have == prefill + sent) WF_CHECK_MEM(got + prefill, src + OFF, sent);
    }
    free(got);
    free(src);
    close(file);
    close(sv[0]);
    close(sv[1]);
}

static void send_eintr(void)
{
    install_usr1();
    for (int rep = 0; rep < 3; rep++) {
        eintr_case(OP_SEND_MORE);
        eintr_case(OP_SEND_ALL);
        eintr_case(OP_SENDFILE);
    }
    WF_CHECK(usr1_hits > 0);
}

/* ------------------------------------------------------------------ */
/* sendfile                                                            */
/* ------------------------------------------------------------------ */

/* Expected result of sendfile on an empty, blocking-enough socket. */
static size_t sf_expect(size_t fsize, int64_t off, size_t count)
{
    if (off < 0 || (uint64_t)off >= fsize) return 0;
    size_t left = fsize - (size_t)off;
    return count < left ? count : left;
}

/* Every offset / count combination around the end of small files; each
 * call's bytes are checked on the receiving side. Socket buffers are big
 * enough that no call blocks. */
static void sendfile_offsets(void)
{
    static const size_t sizes[] = { 0, 1, 2, 15, 16, 17, 100, 4095, 4096, 4097, 20000 };
    int sv[2];
    sx_pair(sv);
    sx_sndbuf(sv[0], 256 * 1024);
    sx_sndbuf(sv[1], 256 * 1024); /* AF_UNIX capacity is the peer's rcvbuf on Darwin */
    sx_nonblock(sv[1], 1);
    uint8_t *got = malloc(64 * 1024);
    for (size_t si = 0; si < sizeof sizes / sizeof sizes[0]; si++) {
        size_t fsize = sizes[si];
        uint8_t *src = pattern(fsize, 100 + si);
        int f = sx_tempfile(src, fsize);
        int64_t offs[] = { 0, 1, (int64_t)fsize / 2, (int64_t)fsize - 17, (int64_t)fsize - 16,
                           (int64_t)fsize - 1, (int64_t)fsize, (int64_t)fsize + 1, (int64_t)fsize + 4096,
                           (int64_t)1 << 40 };
        size_t counts[] = { 0, 1, 15, 16, 17, 1000, fsize, fsize + 1, (size_t)1 << 20, SIZE_MAX };
        for (size_t oi = 0; oi < sizeof offs / sizeof offs[0]; oi++) {
            if (offs[oi] < 0) continue;
            for (size_t ci = 0; ci < sizeof counts / sizeof counts[0]; ci++) {
                size_t want = sf_expect(fsize, offs[oi], counts[ci]);
                hs_ssize_t r = hs_http1_sendfile(sv[0], f, offs[oi], counts[ci]);
                WF_CHECK_EQ(r, want);
                size_t have = 0;
                sx_drain(sv[1], got, &have, 64 * 1024);
                WF_CHECK_EQ(have, want);
                if (have == want && want) WF_CHECK_MEM(got, src + offs[oi], want);
            }
        }
        /* sendfile reads at the given offset, not the file position. */
        if (fsize > 4) {
            lseek(f, 3, SEEK_SET);
            WF_CHECK_EQ(hs_http1_sendfile(sv[0], f, 1, 2), 2);
            size_t have = 0;
            sx_drain(sv[1], got, &have, 64 * 1024);
            WF_CHECK_EQ(have, 2);
            WF_CHECK_MEM(got, src + 1, 2);
        }
        close(f);
        free(src);
    }
    free(got);
    close(sv[0]);
    close(sv[1]);
}

/* Regression: count == 0 meant "to end of file" on Darwin / FreeBSD (and
 * TransmitFile), so a zero-length request sent the whole file. */
static void sendfile_zero_count(void)
{
    int sv[2];
    sx_pair(sv);
    sx_nonblock(sv[1], 1);
    uint8_t *src = pattern(5000, 7);
    int f = sx_tempfile(src, 5000);
    WF_CHECK_EQ(hs_http1_sendfile(sv[0], f, 0, 0), 0);
    WF_CHECK_EQ(hs_http1_sendfile(sv[0], f, 4990, 0), 0);
    uint8_t got[16];
    size_t have = 0;
    sx_drain(sv[1], got, &have, sizeof got);
    WF_CHECK_EQ(have, 0);
    close(f);
    free(src);
    close(sv[0]);
    close(sv[1]);
}

static void sendfile_bad_args(void)
{
    uint8_t x[100] = { 0 };
    int f = sx_tempfile(x, sizeof x);
    int sv[2];
    sx_pair(sv);
    /* Word64 offsets >= 2^63 arrive negative from the Haskell side. */
    WF_CHECK_EQ(hs_http1_sendfile(sv[0], f, -1, 10), -EINVAL);
    WF_CHECK_EQ(hs_http1_sendfile(sv[0], f, INT64_MIN, 10), -EINVAL);
    WF_CHECK_EQ(hs_http1_sendfile(sv[0], -1, 0, 10), -EBADF);
    WF_CHECK_EQ(hs_http1_sendfile(-1, f, 0, 10), -EBADF);
    int closed = dup(f);
    close(closed);
    WF_CHECK_EQ(hs_http1_sendfile(sv[0], closed, 0, 10), -EBADF);
    /* Source that is not a regular file. */
    int p[2];
    if (pipe(p) != 0) sx_die("pipe");
    hs_ssize_t r = hs_http1_sendfile(sv[0], p[0], 0, 10);
    WF_CHECK(r < 0);
    /* Destination that is not a socket: an error everywhere except Linux
     * native sendfile, which also writes to pipes; never a crash or a
     * bogus positive count. */
    r = hs_http1_sendfile(p[1], f, 0, 10);
    WF_CHECK(r < 0 || r == 10);
    /* Write-only source fd. */
    int wo = open("/dev/null", O_WRONLY);
    r = hs_http1_sendfile(sv[0], wo, 0, 10);
    WF_CHECK(r <= 0);
    close(wo);
    /* Peer closed. */
    close(sv[1]);
    r = hs_http1_sendfile(sv[0], f, 0, 10);
    WF_CHECK(r == -EPIPE || r == -ECONNRESET || r == -ENOTCONN);
    close(sv[0]);
    close(p[0]);
    close(p[1]);
    close(f);
}

/* Regression: on Darwin, a non-blocking socket with a full buffer made
 * sendfile fail with EAGAIN and *len == 0, which the shim returned as 0,
 * the EOF signal: sendFileFd then stopped and truncated the response.
 * Loop the way sendFileFd does; 0 must only ever mean EOF. */
static void sendfile_nonblocking_full(void)
{
    enum { N = 2 << 20 };
    uint8_t *src = pattern(N, 21);
    int f = sx_tempfile(src, N);
    int sv[2];
    sx_pair(sv);
    sx_sndbuf(sv[0], 4096);
    sx_nonblock(sv[0], 1);
    sx_nonblock(sv[1], 1);
    uint8_t *got = malloc(N);
    size_t off = 0, have = 0;
    int eagains = 0, zeros = 0;
    while (off < N) {
        size_t want = N - off < (1u << 20) ? N - off : (1u << 20);
        hs_ssize_t r = hs_http1_sendfile(sv[0], f, (int64_t)off, want);
        if (hs_is_eagain(r)) {
            eagains++;
            sx_drain(sv[1], got, &have, N);
            continue;
        }
        if (r == 0) {
            zeros++;
            break;
        }
        WF_CHECK(r > 0 && (size_t)r <= want);
        if (r < 0) break;
        off += (size_t)r;
    }
    while (have < off) {
        if (!sx_drain(sv[1], got, &have, N)) sx_msleep(1);
    }
    WF_CHECK_EQ(zeros, 0);
    WF_CHECK_EQ(off, N);
    WF_CHECK(eagains > 0);
    WF_CHECK_EQ(have, N);
    WF_CHECK_MEM(got, src, N);
    /* Full buffer, nothing fits: -EAGAIN, not 0. */
    sx_fill(sv[0]);
    WF_CHECK(hs_is_eagain(hs_http1_sendfile(sv[0], f, 0, 100)));
    free(got);
    free(src);
    close(f);
    close(sv[0]);
    close(sv[1]);
}

/* Blocking socket, reader thread, the exact sendFileFd loop (1 MiB cap per
 * call) over a file that is not a multiple of anything. */
static void sendfile_blocking_loop(void)
{
    enum { N = (5 << 20) + 12345 };
    uint8_t *src = pattern(N, 22);
    int f = sx_tempfile(src, N);
    int sv[2];
    sx_pair(sv);
    sx_sndbuf(sv[0], 16384);
    reader_arg ra = { sv[1], malloc(N), N, 0 };
    pthread_t t;
    pthread_create(&t, NULL, reader_main, &ra);
    size_t off = 0;
    while (off < N) {
        size_t want = N - off < (1u << 20) ? N - off : (1u << 20);
        hs_ssize_t r = hs_http1_sendfile(sv[0], f, (int64_t)off, want);
        WF_CHECK(r > 0 && (size_t)r <= want);
        if (r <= 0) break;
        off += (size_t)r;
    }
    /* The length lied: one more call at EOF returns 0. */
    WF_CHECK_EQ(hs_http1_sendfile(sv[0], f, (int64_t)off, 1000), 0);
    pthread_join(t, NULL);
    WF_CHECK_EQ(off, N);
    WF_CHECK_EQ(ra.have, N);
    WF_CHECK_MEM(ra.buf, src, N);
    free(ra.buf);
    free(src);
    close(f);
    close(sv[0]);
    close(sv[1]);
}

/* Random interleavings of the three calls on a non-blocking pair; the
 * model is the concatenation of what each call reported. */
static void send_random_mix(void)
{
    enum { FS = 70000, CAP = 1 << 22 };
    wf_rng r = wf_rng_from_env();
    uint8_t *src = pattern(FS, 31);
    int f = sx_tempfile(src, FS);
    long iters = wf_iters(20);
    uint8_t *want = malloc(CAP), *got = malloc(CAP);
    for (long it = 0; it < iters; it++) {
        int sv[2];
        sx_pair(sv);
        sx_sndbuf(sv[0], 2048 + (int)wf_rng_below(&r, 65536));
        sx_nonblock(sv[0], 1);
        sx_nonblock(sv[1], 1);
        size_t wl = 0, have = 0;
        for (int op = 0; op < 200 && wl < CAP - 70000; op++) {
            int kind = (int)wf_rng_below(&r, 3);
            hs_ssize_t res;
            if (kind == 2) {
                int64_t off = (int64_t)wf_rng_below(&r, FS + 50);
                size_t cnt = (size_t)wf_rng_below(&r, 40000);
                res = hs_http1_sendfile(sv[0], f, off, cnt);
                size_t lim = sf_expect(FS, off, cnt);
                /* -EAGAIN on a full buffer, also at EOF on Darwin (see api.h). */
                WF_CHECK(res == (hs_ssize_t)lim || (res > 0 && (size_t)res < lim) || (cnt && hs_is_eagain(res)));
                if (res > 0) {
                    memcpy(want + wl, src + off, (size_t)res);
                    wl += (size_t)res;
                }
            } else {
                size_t s = (size_t)wf_rng_below(&r, FS);
                size_t n = (size_t)wf_rng_below(&r, FS - s + 1);
                res = kind ? hs_http1_send_all(sv[0], src + s, n) : hs_http1_send_more(sv[0], src + s, n);
                WF_CHECK((res >= 0 && (size_t)res <= n) || (n && hs_is_eagain(res)));
                if (n == 0) WF_CHECK_EQ(res, 0);
                if (res > 0) {
                    memcpy(want + wl, src + s, (size_t)res);
                    wl += (size_t)res;
                }
            }
            if (wf_rng_next(&r) & 1) sx_drain(sv[1], got, &have, CAP);
        }
        while (have < wl) {
            if (!sx_drain(sv[1], got, &have, CAP)) sx_msleep(1);
        }
        WF_CHECK_EQ(have, wl);
        WF_CHECK_MEM(got, want, wl);
        close(sv[0]);
        close(sv[1]);
    }
    free(want);
    free(got);
    free(src);
    close(f);
}

int main(void)
{
    ignore_sigpipe();
    WF_RUN(send_basic);
    WF_RUN(send_bad_fds);
    WF_RUN(send_closed_peer);
    WF_RUN(send_nonblocking_partial);
    WF_RUN(send_blocking_large);
    WF_RUN(send_eintr);
    WF_RUN(sendfile_offsets);
    WF_RUN(sendfile_zero_count);
    WF_RUN(sendfile_bad_args);
    WF_RUN(sendfile_nonblocking_full);
    WF_RUN(sendfile_blocking_loop);
    WF_RUN(send_random_mix);
    return wf_finish("http1-send");
}
