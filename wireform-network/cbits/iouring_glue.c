/*
 * io_uring receive glue for Wireform.Network.Transport.IOUring.
 *
 * The cabal file compiles this file only under `flag(iouring) && os(linux)`.
 * It used to be wrapped in `#ifdef HAVE_IOURING`, but that macro comes
 * from the component's cpp-options, which Cabal applies to Haskell sources
 * only (C sources get cc-options), so the whole file compiled to nothing
 * and every hs_iouring_* import was an undefined symbol.
 *
 * Caller contract (see c-harness/network-iouring/api.h): the Haskell side
 * callocs 512 bytes for struct hs_iouring, keeps at most one recv in
 * flight, and only submits a recv with free ring space > 0.
 */
#include <liburing.h>
#include <sys/eventfd.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include <stdint.h>

struct hs_iouring {
    struct io_uring ring;
    int event_fd;
    int socket_fd;
    void *ring_buf;
    size_t ring_buf_size;
    size_t ring_buf_mask;
    uint64_t head;
    int closed;
};

/* Wireform.Network.Transport.IOUring allocates hsIouringSize = 512 bytes
 * for this struct; a liburing whose struct io_uring outgrows that would
 * otherwise overflow the Haskell allocation. */
_Static_assert(sizeof(struct hs_iouring) <= 512,
               "struct hs_iouring exceeds the 512 bytes Haskell allocates");

/* Largest single recv: sqe->len is 32 bits and cqe->res is a signed
 * 32-bit count, so cap well inside both (the kernel caps a recv at
 * MAX_RW_COUNT, INT_MAX rounded down to a page, anyway). */
#define IOURING_MAX_RECV ((size_t)1 << 30)

int hs_iouring_create(int socket_fd, int queue_depth, int sqpoll_idle_ms,
                      void *ring_buf, size_t ring_buf_size,
                      struct hs_iouring *out) {
    struct io_uring_params params;
    memset(&params, 0, sizeof(params));

    /* Marks the struct as not owning a ring until creation succeeds, so
     * hs_iouring_destroy on a failed create is a no-op. */
    out->event_fd = -1;

    /* The ring is indexed with `head & (size - 1)`. */
    if (ring_buf == NULL || ring_buf_size == 0
        || (ring_buf_size & (ring_buf_size - 1)) != 0 || queue_depth <= 0)
        return -EINVAL;

    if (sqpoll_idle_ms > 0) {
        params.flags = IORING_SETUP_SQPOLL;
        params.sq_thread_idle = (unsigned)sqpoll_idle_ms;
    }

    int ret = io_uring_queue_init_params((unsigned)queue_depth, &out->ring, &params);
    if (ret < 0) return ret;

    int efd = eventfd(0, EFD_NONBLOCK | EFD_CLOEXEC);
    if (efd < 0) {
        ret = -errno;
        io_uring_queue_exit(&out->ring);
        return ret;
    }

    ret = io_uring_register_eventfd(&out->ring, efd);
    if (ret < 0) {
        close(efd);
        io_uring_queue_exit(&out->ring);
        return ret;
    }

    out->event_fd = efd;
    out->socket_fd = socket_fd;
    out->ring_buf = ring_buf;
    out->ring_buf_size = ring_buf_size;
    out->ring_buf_mask = ring_buf_size - 1;
    out->head = 0;
    out->closed = 0;

    return 0;
}

/* Submit a recv SQE into the ring at the current head position, for at
 * most max_bytes and never past the end of the buffer. Returns the
 * io_uring_submit result (>= 0) or a negative errno: -EINVAL for
 * max_bytes == 0 (a zero-length recv completes with 0, which would read
 * as EOF), -EBUSY when the submission queue is full. */
int hs_iouring_submit_recv(struct hs_iouring *uring, size_t max_bytes) {
    if (max_bytes == 0) return -EINVAL;
    struct io_uring_sqe *sqe = io_uring_get_sqe(&uring->ring);
    if (!sqe) return -EBUSY;

    size_t write_off = uring->head & uring->ring_buf_mask;
    size_t available = max_bytes;
    if (available > uring->ring_buf_size - write_off)
        available = uring->ring_buf_size - write_off;
    if (available > IOURING_MAX_RECV)
        available = IOURING_MAX_RECV;

    io_uring_prep_recv(sqe, uring->socket_fd,
                       (char *)uring->ring_buf + write_off,
                       available, 0);
    sqe->user_data = uring->head;

    return io_uring_submit(&uring->ring);
}

/* Consume one completion: >0 = bytes received (head advanced), 0 = EOF,
 * <0 = -errno from the recv. */
static int consume_cqe(struct hs_iouring *uring, struct io_uring_cqe *cqe,
                       uint64_t *new_head) {
    int res = cqe->res;
    io_uring_cqe_seen(&uring->ring, cqe);

    if (res > 0) {
        uring->head += (uint64_t)res;
        *new_head = uring->head;
        return res;
    } else if (res == 0) {
        uring->closed = 1;
        *new_head = uring->head;
        return 0;
    } else {
        return res;
    }
}

/* Wait for a completion and process it.
 * Returns: >0 = bytes received (head advanced), 0 = EOF, <0 = error */
int hs_iouring_wait_cqe(struct hs_iouring *uring, uint64_t *new_head) {
    struct io_uring_cqe *cqe;
    int ret = io_uring_wait_cqe(&uring->ring, &cqe);
    if (ret < 0) return ret;
    return consume_cqe(uring, cqe, new_head);
}

/* Non-blocking check for completions (-EAGAIN when none is ready). */
int hs_iouring_peek_cqe(struct hs_iouring *uring, uint64_t *new_head) {
    struct io_uring_cqe *cqe;
    int ret = io_uring_peek_cqe(&uring->ring, &cqe);
    if (ret < 0) return ret;
    return consume_cqe(uring, cqe, new_head);
}

int hs_iouring_get_eventfd(struct hs_iouring *uring) {
    return uring->event_fd;
}

uint64_t hs_iouring_get_head(struct hs_iouring *uring) {
    return uring->head;
}

/* Release the ring and eventfd. Idempotent, and a no-op after a failed
 * hs_iouring_create (event_fd < 0 marks "owns nothing"). */
void hs_iouring_destroy(struct hs_iouring *uring) {
    if (uring->event_fd < 0) return;
    close(uring->event_fd);
    uring->event_fd = -1;
    io_uring_queue_exit(&uring->ring);
}
