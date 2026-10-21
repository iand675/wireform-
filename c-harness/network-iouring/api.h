/*
 * wireform-network/cbits/iouring_glue.c, as
 * Wireform.Network.Transport.IOUring imports it:
 *
 *   hs_iouring_create     :: CInt -> CInt -> CInt -> Ptr Word8 -> CSize
 *                            -> Ptr IOUringState -> IO CInt      (unsafe)
 *   hs_iouring_submit_recv :: Ptr IOUringState -> CSize -> IO CInt (unsafe)
 *   hs_iouring_wait_cqe   :: Ptr IOUringState -> Ptr Word64 -> IO CInt (safe)
 *   hs_iouring_peek_cqe   :: Ptr IOUringState -> Ptr Word64 -> IO CInt (unsafe)
 *   hs_iouring_get_eventfd :: Ptr IOUringState -> IO CInt          (unsafe)
 *   hs_iouring_get_head   :: Ptr IOUringState -> IO Word64         (unsafe)
 *   hs_iouring_destroy    :: Ptr IOUringState -> IO ()             (unsafe)
 *
 * Caller contract, from withIOUringReceiveTransport:
 * - The state is an opaque callocBytes 512 (hsIouringSize); the C side
 *   static-asserts the struct fits. Tests allocate it the same way.
 * - ring_buf / ring_buf_size are a live magic ring (power of two, page
 *   multiple); create now rejects a non-power-of-two size with -EINVAL.
 * - destroy runs once after a successful create (IORef guard); after a
 *   failed create the Haskell side never calls it, and it is a no-op
 *   anyway. destroy on a calloc'd struct that create never saw is NOT
 *   allowed (event_fd 0 would be closed).
 * - At most one recv is in flight: one submit initially, one after each
 *   completion, with max_bytes = free ring space; it skips the submit
 *   when that is 0, and submit_recv(0) now returns -EINVAL.
 * - new_head points at a Word64; it is written only on a return >= 0.
 * Returns are >= 0 or a negative errno.
 */
#ifndef WF_NETWORK_IOURING_API_H
#define WF_NETWORK_IOURING_API_H

#include <stddef.h>
#include <stdint.h>
#include <fcntl.h>
#include <liburing.h>

#define HS_IOURING_SIZE 512

struct hs_iouring;

int hs_iouring_create(int socket_fd, int queue_depth, int sqpoll_idle_ms,
                      void *ring_buf, size_t ring_buf_size,
                      struct hs_iouring *out);
int hs_iouring_submit_recv(struct hs_iouring *uring, size_t max_bytes);
int hs_iouring_wait_cqe(struct hs_iouring *uring, uint64_t *new_head);
int hs_iouring_peek_cqe(struct hs_iouring *uring, uint64_t *new_head);
int hs_iouring_get_eventfd(struct hs_iouring *uring);
uint64_t hs_iouring_get_head(struct hs_iouring *uring);
void hs_iouring_destroy(struct hs_iouring *uring);

/* Is io_uring usable here (not disabled by sysctl / seccomp)? */
static inline int ref_iouring_available(void)
{
    struct io_uring r;
    if (io_uring_queue_init(2, &r, 0) < 0) return 0;
    io_uring_queue_exit(&r);
    return 1;
}

/* -1 under valgrind, which opens descriptors of its own. */
static inline int ref_open_fds(void)
{
    if (wf_under_valgrind() || wf_under_rosetta()) return -1;
    int n = 0;
    for (int fd = 0; fd < 1024; fd++) {
        if (fcntl(fd, F_GETFD) != -1) n++;
    }
    return n;
}

/* Unknown counts pass (see 'ref_open_fds'). */
static inline int ref_fds_unchanged(int before)
{
    int now = ref_open_fds();
    return now < 0 || before < 0 || now == before;
}

#endif
