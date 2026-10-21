/* Tests for iouring_glue.c (Linux, liburing) over a socketpair, receiving
 * into a power-of-two buffer that ends flush against a guard page, so a
 * recv clamped wrongly at the end of the ring faults. */
#define _GNU_SOURCE
#include "harness.h"
#include "api.h"

#include <errno.h>
#include <poll.h>
#include <sys/socket.h>
#include <sys/eventfd.h>

typedef struct {
    struct hs_iouring *u;
    int fds[2];          /* [0] peer (writer), [1] receiver */
    wf_guarded g;
    uint8_t *buf;
    size_t size;
} rig;

static int rig_open(rig *r, size_t size, int depth)
{
    memset(r, 0, sizeof *r);
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, r->fds) != 0) abort();
    r->g = wf_guard_tail(size);
    r->buf = r->g.p;
    r->size = size;
    r->u = calloc(1, HS_IOURING_SIZE);
    return hs_iouring_create(r->fds[1], depth, 0, r->buf, size, r->u);
}

static void rig_close(rig *r, int created)
{
    if (created) hs_iouring_destroy(r->u);
    free(r->u);
    if (r->fds[0] >= 0) close(r->fds[0]);
    close(r->fds[1]);
    wf_guard_free(r->g);
}

static void peer_send(rig *r, const uint8_t *p, size_t n)
{
    size_t done = 0;
    while (done < n) {
        ssize_t w = write(r->fds[0], p + done, n - done);
        if (w <= 0) abort();
        done += (size_t)w;
    }
}

/* Submit, send n bytes, wait; returns the wait result. */
static int recv_once(rig *r, size_t max_bytes, const uint8_t *p, size_t n, uint64_t *head)
{
    int s = hs_iouring_submit_recv(r->u, max_bytes);
    WF_CHECK(s >= 0);
    if (n) peer_send(r, p, n);
    return hs_iouring_wait_cqe(r->u, head);
}

static void create_validation(void)
{
    uint8_t dummy[64];
    struct { void *buf; size_t size; int depth; } bad[] = {
        { NULL, 4096, 8 }, { dummy, 0, 8 }, { dummy, 3, 8 }, { dummy, 4097, 8 },
        { dummy, 4096 + 2048, 8 }, { dummy, 4096, 0 }, { dummy, 4096, -1 },
    };
    int fds = ref_open_fds();
    for (size_t i = 0; i < sizeof bad / sizeof bad[0]; i++) {
        struct hs_iouring *u = calloc(1, HS_IOURING_SIZE);
        WF_CHECK_EQ(hs_iouring_create(0, bad[i].depth, 0, bad[i].buf, bad[i].size, u), -EINVAL);
        hs_iouring_destroy(u); /* no-op after a failed create */
        free(u);
    }
    WF_CHECK(ref_fds_unchanged(fds));
}

static void basic_receive(void)
{
    rig r;
    int rc = rig_open(&r, 4 * 4096, 8);
    WF_CHECK_EQ(rc, 0);
    if (rc) { rig_close(&r, 0); return; }
    int efd = hs_iouring_get_eventfd(r.u);
    WF_CHECK(efd >= 0);
    WF_CHECK_EQ(hs_iouring_get_head(r.u), 0);
    uint64_t head = 99;
    WF_CHECK_EQ(hs_iouring_peek_cqe(r.u, &head), -EAGAIN);
    WF_CHECK_EQ(head, 99); /* untouched on error */

    uint8_t msg[100];
    for (int i = 0; i < 100; i++) msg[i] = (uint8_t)(i * 3 + 1);
    WF_CHECK_EQ(recv_once(&r, r.size, msg, sizeof msg, &head), 100);
    WF_CHECK_EQ(head, 100);
    WF_CHECK_EQ(hs_iouring_get_head(r.u), 100);
    WF_CHECK_MEM(r.buf, msg, 100);
    /* Consuming the completion drains the eventfd (see
     * eventfd_signals_then_drains), so it must read as empty now. */
    uint64_t ev = 0;
    WF_CHECK_EQ(read(efd, &ev, sizeof ev), (ssize_t)-1);
    WF_CHECK_EQ(errno, EAGAIN);

    /* peek path */
    WF_CHECK(hs_iouring_submit_recv(r.u, 50) >= 0);
    peer_send(&r, msg, 80);
    int got = -EAGAIN;
    for (int i = 0; i < 100000 && got == -EAGAIN; i++) got = hs_iouring_peek_cqe(r.u, &head);
    WF_CHECK_EQ(got, 50);
    WF_CHECK_EQ(head, 150);
    WF_CHECK_MEM(r.buf + 100, msg, 50);
    /* the remaining 30 */
    WF_CHECK_EQ(recv_once(&r, 1000, NULL, 0, &head), 30);
    WF_CHECK_MEM(r.buf + 150, msg + 50, 30);
    rig_close(&r, 1);
}

/* The eventfd is level-triggered, so it must be readable exactly while a
 * completion is waiting to be consumed: the transport peeks after every
 * wake and parks otherwise, and an eventfd left set would spin that loop
 * (or, drained too eagerly, lose the wakeup). */
static void eventfd_signals_then_drains(void)
{
    rig r;
    WF_CHECK_EQ(rig_open(&r, 4096, 4), 0);
    int efd = hs_iouring_get_eventfd(r.u);
    WF_CHECK(efd >= 0);
    WF_CHECK(hs_iouring_submit_recv(r.u, r.size) >= 0);
    uint64_t ev = 0;
    errno = 0;
    WF_CHECK_EQ(read(efd, &ev, sizeof ev), (ssize_t)-1);
    WF_CHECK_EQ(errno, EAGAIN); /* nothing completed yet */

    peer_send(&r, (const uint8_t *)"hello", 5);
    struct pollfd pfd = { efd, POLLIN, 0 };
    WF_CHECK_EQ(poll(&pfd, 1, 1000), 1);
    WF_CHECK((pfd.revents & POLLIN) != 0);

    uint64_t head = 0;
    WF_CHECK_EQ(hs_iouring_peek_cqe(r.u, &head), 5);
    WF_CHECK_EQ(head, 5);
    pfd.revents = 0;
    WF_CHECK_EQ(poll(&pfd, 1, 0), 0); /* drained with the completion */
    errno = 0;
    WF_CHECK_EQ(read(efd, &ev, sizeof ev), (ssize_t)-1);
    WF_CHECK_EQ(errno, EAGAIN);
    rig_close(&r, 1);
}

/* A recv near the end of the buffer is clamped to the bytes left before
 * the end (the guard page follows), then continues at offset 0. */
static void clamps_at_ring_end(void)
{
    size_t size = 2 * 4096;
    size_t lefts[] = { 1, 10, 4095, 4096 };
    for (size_t k = 0; k < sizeof lefts / sizeof lefts[0]; k++) {
        rig r;
        WF_CHECK_EQ(rig_open(&r, size, 4), 0);
        uint8_t *fill = malloc(size);
        memset(fill, 0xEE, size);
        size_t pre = size - lefts[k];
        uint64_t head = 0;
        while (head < pre) {
            int n = recv_once(&r, pre - head, fill, (size_t)(pre - head) < 1000 ? pre - head : 1000, &head);
            WF_CHECK(n > 0);
            if (n <= 0) break;
        }
        WF_CHECK_EQ(head, pre);
        uint8_t msg[6000];
        for (size_t i = 0; i < sizeof msg; i++) msg[i] = (uint8_t)(i * 7);
        size_t maxes[] = { 1000000, SIZE_MAX };
        int n = recv_once(&r, maxes[k & 1], msg, lefts[k] + 50, &head);
        WF_CHECK_EQ(n, (int)lefts[k]);
        WF_CHECK_EQ(head, size);
        WF_CHECK_MEM(r.buf + pre, msg, lefts[k]);
        n = recv_once(&r, size, NULL, 0, &head);
        WF_CHECK_EQ(n, 50);
        WF_CHECK_MEM(r.buf, msg + lefts[k], 50);
        WF_CHECK_EQ(head, size + 50);
        free(fill);
        rig_close(&r, 1);
    }
}

/* A zero-length recv used to complete with 0 and read as EOF. */
static void zero_length_submit_rejected(void)
{
    rig r;
    WF_CHECK_EQ(rig_open(&r, 4096, 4), 0);
    WF_CHECK_EQ(hs_iouring_submit_recv(r.u, 0), -EINVAL);
    uint64_t head = 0;
    WF_CHECK_EQ(hs_iouring_peek_cqe(r.u, &head), -EAGAIN);
    WF_CHECK_EQ(recv_once(&r, 64, (const uint8_t *)"abc", 3, &head), 3);
    rig_close(&r, 1);
}

static void eof_and_errors(void)
{
    rig r;
    WF_CHECK_EQ(rig_open(&r, 4096, 4), 0);
    uint64_t head = 0;
    WF_CHECK_EQ(recv_once(&r, 4096, (const uint8_t *)"hello", 5, &head), 5);
    WF_CHECK(hs_iouring_submit_recv(r.u, 4096) >= 0);
    close(r.fds[0]);
    r.fds[0] = -1;
    head = 0;
    WF_CHECK_EQ(hs_iouring_wait_cqe(r.u, &head), 0);
    WF_CHECK_EQ(head, 5);
    WF_CHECK_EQ(hs_iouring_get_head(r.u), 5);
    rig_close(&r, 1);

    /* recv on something that is not a socket fails with a negative errno. */
    int p[2];
    WF_CHECK_EQ(pipe(p), 0);
    struct hs_iouring *u = calloc(1, HS_IOURING_SIZE);
    wf_guarded g = wf_guard_tail(4096);
    WF_CHECK_EQ(hs_iouring_create(p[0], 4, 0, g.p, 4096, u), 0);
    WF_CHECK(hs_iouring_submit_recv(u, 100) >= 0);
    head = 77;
    int rc = hs_iouring_wait_cqe(u, &head);
    WF_CHECK(rc < 0);
    WF_CHECK_EQ(head, 77);
    hs_iouring_destroy(u);
    free(u);
    wf_guard_free(g);
    close(p[0]);
    close(p[1]);
}

static void destroy_idempotent_no_leaks(void)
{
    int fds = ref_open_fds();
    for (int i = 0; i < 200; i++) {
        rig r;
        WF_CHECK_EQ(rig_open(&r, 4096, 1 + i % 16), 0);
        if (i % 3 == 0) {
            uint64_t head;
            WF_CHECK_EQ(recv_once(&r, 4096, (const uint8_t *)"x", 1, &head), 1);
        }
        hs_iouring_destroy(r.u);
        hs_iouring_destroy(r.u); /* second destroy is a no-op */
        rig_close(&r, 0);
    }
    WF_CHECK(ref_fds_unchanged(fds));
}

/* A ring of 2^33 bytes (virtual only) and max_bytes = 2^32: the length
 * used to be truncated to the 32-bit sqe->len, so a 2^32 recv became a
 * 0-byte recv that completed with 0, i.e. a false EOF, and 2^32 + 5 a
 * 5-byte recv. */
static void huge_lengths_not_truncated(void)
{
    size_t size = (size_t)1 << 33;
    void *big = mmap(NULL, size, PROT_READ | PROT_WRITE,
                     MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0);
    if (big == MAP_FAILED) {
        printf("huge_lengths_not_truncated: skipped (no 8 GiB of address space)\n");
        return;
    }
    size_t maxes[] = { (size_t)1 << 32, ((size_t)1 << 32) + 5 };
    for (int k = 0; k < 2; k++) {
        int fds[2];
        WF_CHECK_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);
        struct hs_iouring *u = calloc(1, HS_IOURING_SIZE);
        WF_CHECK_EQ(hs_iouring_create(fds[1], 4, 0, big, size, u), 0);
        WF_CHECK(hs_iouring_submit_recv(u, maxes[k]) >= 0);
        WF_CHECK_EQ(write(fds[0], "0123456789", 10), 10);
        uint64_t head = 0;
        WF_CHECK_EQ(hs_iouring_wait_cqe(u, &head), 10);
        WF_CHECK_EQ(head, 10);
        WF_CHECK_MEM(big, "0123456789", 10);
        hs_iouring_destroy(u);
        free(u);
        close(fds[0]);
        close(fds[1]);
    }
    munmap(big, size);
}

/* The receive transport's wait loop (Wireform.Network.Transport.IOUring.
 * doWait), modelled at the shim: ensure one recv is in flight while the
 * ring has space, otherwise park on the eventfd and look again.
 *
 * The loop it mirrors used to hang when a completion found the ring full
 * (nothing resubmitted, so the next wait parked on an eventfd nothing
 * could signal). This test pins the shim half of the fix: submitting
 * after the consumer drains a full ring does produce a completion and a
 * drained eventfd that wakes exactly once per completion. The Haskell
 * side of the loop is compile-checked only (no io_uring on macOS). */
static uint64_t model_wait(struct hs_iouring *u, int efd, uint64_t pos,
                           uint64_t tail, size_t ring)
{
    for (int spins = 0; spins < 100; spins++) {
        uint64_t h = hs_iouring_get_head(u);
        if (h > pos) return h;
        size_t space = ring - (size_t)(h - tail);
        if (space > 0) WF_CHECK(hs_iouring_submit_recv(u, space) >= 0);
        struct pollfd p = { efd, POLLIN, 0 };
        WF_CHECK_EQ(poll(&p, 1, 1000), 1);
        uint64_t nh = 0;
        int rc = hs_iouring_peek_cqe(u, &nh);
        if (rc >= 0) return nh;
    }
    WF_CHECK(!"model_wait spun without progress");
    return pos;
}

static void wait_loop_recovers_from_a_full_ring(void)
{
    rig r;
    WF_CHECK_EQ(rig_open(&r, 4096, 4), 0);
    int efd = hs_iouring_get_eventfd(r.u);
    WF_CHECK(efd >= 0);
    uint8_t first[4096], second[100];
    for (size_t i = 0; i < sizeof first; i++) first[i] = (uint8_t)(i * 7 + 3);
    for (size_t i = 0; i < sizeof second; i++) second[i] = (uint8_t)(i * 13 + 5);

    /* Fill the whole ring, the way a fast peer would. */
    WF_CHECK(hs_iouring_submit_recv(r.u, r.size) >= 0);
    peer_send(&r, first, sizeof first);
    uint64_t tail = 0;
    WF_CHECK_EQ(model_wait(r.u, efd, 0, tail, r.size), r.size);
    WF_CHECK_MEM(r.buf, first, sizeof first);

    /* Head is a whole ring ahead of the consumer, so a completion-based
     * resubmit would submit nothing. The consumer drains; the next wait
     * must submit for itself and get the new bytes. */
    tail = r.size;
    peer_send(&r, second, sizeof second);
    uint64_t head = model_wait(r.u, efd, r.size, tail, r.size);
    WF_CHECK_EQ(head, r.size + sizeof second);
    /* The second recv wrapped back to offset 0. */
    WF_CHECK_MEM(r.buf, second, sizeof second);
    rig_close(&r, 1);
}

/* SQPOLL needs privileges on older kernels: either it works or it fails
 * cleanly with a negative errno. */
static void sqpoll_mode(void)
{
    int fds[2];
    WF_CHECK_EQ(socketpair(AF_UNIX, SOCK_STREAM, 0, fds), 0);
    struct hs_iouring *u = calloc(1, HS_IOURING_SIZE);
    wf_guarded g = wf_guard_tail(4096);
    int rc = hs_iouring_create(fds[1], 4, 10, g.p, 4096, u);
    if (rc == 0) {
        WF_CHECK(hs_iouring_submit_recv(u, 4096) >= 0);
        WF_CHECK_EQ(write(fds[0], "sq", 2), 2);
        uint64_t head = 0;
        WF_CHECK_EQ(hs_iouring_wait_cqe(u, &head), 2);
        hs_iouring_destroy(u);
    } else {
        WF_CHECK(rc < 0);
        hs_iouring_destroy(u);
    }
    free(u);
    wf_guard_free(g);
    close(fds[0]);
    close(fds[1]);
}

int main(void)
{
    WF_RUN(create_validation);
    if (!ref_iouring_available()) {
        printf("test_iouring: io_uring unavailable here (disabled by sysctl or seccomp); "
               "only argument validation ran\n");
        return wf_finish("test_iouring");
    }
    WF_RUN(basic_receive);
    WF_RUN(eventfd_signals_then_drains);
    WF_RUN(clamps_at_ring_end);
    WF_RUN(zero_length_submit_rejected);
    WF_RUN(eof_and_errors);
    WF_RUN(destroy_idempotent_no_leaks);
    WF_RUN(huge_lengths_not_truncated);
    WF_RUN(wait_loop_recovers_from_a_full_ring);
    WF_RUN(sqpoll_mode);
    return wf_finish("test_iouring");
}
