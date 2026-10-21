/* Model-based fuzz target for the io_uring receive glue, driven the way
 * withIOUringReceiveTransport drives it (at most one recv in flight).
 *
 * Input: [size class u8] then ops, each [op u8][operands]:
 *   0 submit [max u32]   submit a recv (if none in flight) for max bytes,
 *                        0 allowed (must be rejected), huge allowed
 *   1 send   [len u16]   the peer writes len bytes of the stream
 *   2 reap               wait for the in-flight recv if the peer has sent
 *                        data or hung up (so it cannot block), else peek
 *   3 consume [n u16]    the reader frees n bytes of ring space
 *   4 hangup             the peer closes its end
 *
 * Invariants: head == bytes received; every received byte equals the
 * stream byte at that position, read back from ring[pos & (size - 1)];
 * a recv never returns more than it was allowed (free space and the end
 * of the buffer, which is flush against a guard page); EOF only after
 * hangup with nothing left; no descriptor leaks.
 */
#define _GNU_SOURCE
#include "harness.h"
#include "api.h"

#include <errno.h>
#include <sys/socket.h>

static int available = -1;

static uint8_t stream_byte(uint64_t pos)
{
    return (uint8_t)(pos * 131 + (pos >> 8) + 7);
}

static uint16_t take_u16(wf_input *in)
{
    uint16_t lo = wf_take_u8(in);
    return (uint16_t)(lo | (uint16_t)wf_take_u8(in) << 8);
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 1 || size > 4096) return 0;
    if (available < 0) available = ref_iouring_available();
    if (!available) return 0;
    int fds_before = ref_open_fds();
    wf_input in = { data, size };
    size_t ring = (size_t)4096 << (wf_take_u8(&in) % 3);

    int fds[2];
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, fds) != 0) abort();
    wf_guarded g = wf_guard_tail(ring);
    fcntl(fds[0], F_SETFL, fcntl(fds[0], F_GETFL) | O_NONBLOCK);
    struct hs_iouring *u = calloc(1, HS_IOURING_SIZE);
    WF_FUZZ_ASSERT(hs_iouring_create(fds[1], 4, 0, g.p, ring, u) == 0);

    uint64_t sent = 0, head = 0, tail = 0;
    int inflight = 0, hung = 0, eof = 0;
    size_t allowed = 0; /* max bytes the in-flight recv may return */
    int steps = 0;

    while (in.n && steps++ < 64 && !eof) {
        uint8_t op = wf_take_u8(&in) % 5;
        if (op == 0) {
            uint32_t raw = wf_take_u32(&in);
            size_t max = raw & 0x80000000u ? SIZE_MAX - (raw & 0xff) : raw % (ring + 2);
            if (inflight) continue;
            size_t space = ring - (size_t)(head - tail);
            if (max > space) max = space; /* the Haskell caller's bound */
            int rc = hs_iouring_submit_recv(u, max);
            if (max == 0) {
                WF_FUZZ_ASSERT(rc == -EINVAL);
                continue;
            }
            WF_FUZZ_ASSERT(rc >= 0);
            size_t off = head & (ring - 1);
            allowed = max < ring - off ? max : ring - off;
            inflight = 1;
        } else if (op == 1) {
            size_t len = (size_t)take_u16(&in) % 3000;
            if (hung) continue;
            uint8_t buf[3000];
            for (size_t i = 0; i < len; i++) buf[i] = stream_byte(sent + i);
            ssize_t w = write(fds[0], buf, len);
            if (w > 0) sent += (uint64_t)w;
        } else if (op == 2) {
            if (!inflight) continue;
            uint64_t nh = 0;
            int rc;
            if (sent > head || hung) {
                rc = hs_iouring_wait_cqe(u, &nh);
            } else {
                rc = hs_iouring_peek_cqe(u, &nh);
                if (rc == -EAGAIN) continue;
            }
            inflight = 0;
            WF_FUZZ_ASSERT(rc >= 0);
            if (rc == 0) {
                WF_FUZZ_ASSERT(hung && head == sent);
                WF_FUZZ_ASSERT(nh == head);
                eof = 1;
                continue;
            }
            WF_FUZZ_ASSERT((size_t)rc <= allowed);
            WF_FUZZ_ASSERT(nh == head + (uint64_t)rc);
            WF_FUZZ_ASSERT(hs_iouring_get_head(u) == nh);
            for (uint64_t p = head; p < nh; p++)
                WF_FUZZ_ASSERT(g.p[p & (ring - 1)] == stream_byte(p));
            head = nh;
        } else if (op == 3) {
            uint64_t n = take_u16(&in);
            tail += n < head - tail ? n : head - tail;
        } else {
            if (!hung) {
                shutdown(fds[0], SHUT_WR);
                hung = 1;
            }
        }
    }
    hs_iouring_destroy(u);
    hs_iouring_destroy(u);
    free(u);
    close(fds[0]);
    close(fds[1]);
    wf_guard_free(g);
    if (!ref_fds_unchanged(fds_before)) {
        fprintf(stderr, "fd count %d -> %d\n", fds_before, ref_open_fds());
    }
    WF_FUZZ_ASSERT(ref_fds_unchanged(fds_before));
    return 0;
}
