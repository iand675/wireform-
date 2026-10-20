/* Fuzz target: random programs of send_more / send_all / sendfile calls
 * on a non-blocking socketpair with a fuzzer-chosen buffer size, against
 * a temp file whose contents come from the input.
 *
 * Input layout:
 *   [sndbuf byte][file length u16][file bytes...][ops...]
 * each op is [kind byte][a u16][b u16]: kind % 4 picks send_more,
 * send_all, sendfile or a drain of the receiver; for sends (a, b) select
 * the slice of the file bytes used as the buffer, for sendfile a is the
 * offset (up to 64 bytes past EOF, plus a sign case) and b the count.
 *
 * Invariants: each call returns a count within what was asked (exactly
 * the bytes left in the file, or fewer only with progress), -EAGAIN only
 * when something was asked for (sendfile at EOF may also see -EAGAIN on a
 * full buffer), 0 from sendfile only at EOF or count 0, a
 * documented -errno for bad offsets, and the receiver gets exactly the
 * concatenation of all reported bytes. */
#include "harness.h"
#include "api.h"

static int initialised;

static uint16_t take_u16(wf_input *in)
{
    uint16_t lo = wf_take_u8(in);
    return (uint16_t)(lo | (uint16_t)(wf_take_u8(in) << 8));
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (!initialised) {
        signal(SIGPIPE, SIG_IGN);
        initialised = 1;
    }
    if (size > 65536) return 0;
    wf_input in = { data, size };
    int sndbuf = 1024 + 512 * (int)wf_take_u8(&in);
    size_t fs = take_u16(&in);
    if (fs > in.n) fs = in.n;
    uint8_t *file = wf_dup(in.p, fs);
    in.p += fs;
    in.n -= fs;

    int f = sx_tempfile(file, fs);
    int sv[2];
    sx_pair(sv);
    sx_sndbuf(sv[0], sndbuf);
    sx_nonblock(sv[0], 1);
    sx_nonblock(sv[1], 1);

    size_t cap = 1 << 21, wl = 0, have = 0;
    uint8_t *want = malloc(cap), *got = malloc(cap);

    while (in.n >= 5 && wl + 65536 * 2 < cap) {
        int kind = wf_take_u8(&in) % 4;
        size_t a = take_u16(&in), b = take_u16(&in);
        if (kind == 3) {
            sx_drain(sv[1], got, &have, cap);
            continue;
        }
        if (kind == 2) {
            int64_t off = (a == 0xffff) ? -(int64_t)b - 1 : (int64_t)(a % (fs + 65));
            size_t cnt = b;
            hs_ssize_t r = hs_http1_sendfile(sv[0], f, off, cnt);
            if (off < 0) {
                WF_FUZZ_ASSERT(r == -EINVAL || (cnt == 0 && r == 0));
                continue;
            }
            size_t left = (uint64_t)off < fs ? fs - (size_t)off : 0;
            size_t lim = cnt < left ? cnt : left;
            WF_FUZZ_ASSERT(r <= (hs_ssize_t)lim);
            /* At EOF Darwin checks for socket space before the file, so
             * a full buffer can give -EAGAIN there too; the caller waits
             * for writability and then sees 0. Never a positive count. */
            if (cnt == 0) WF_FUZZ_ASSERT(r == 0);
            else if (lim == 0) WF_FUZZ_ASSERT(r == 0 || hs_is_eagain(r));
            else WF_FUZZ_ASSERT(r > 0 || hs_is_eagain(r));
            if (r > 0) {
                memcpy(want + wl, file + off, (size_t)r);
                wl += (size_t)r;
            }
        } else {
            size_t s = fs ? a % fs : 0;
            size_t n = fs ? b % (fs - s + 1) : 0;
            const uint8_t *p = fs ? file + s : file;
            hs_ssize_t r = kind ? hs_http1_send_all(sv[0], p, n) : hs_http1_send_more(sv[0], p, n);
            WF_FUZZ_ASSERT(r <= (hs_ssize_t)n);
            if (n == 0) WF_FUZZ_ASSERT(r == 0);
            else WF_FUZZ_ASSERT(r > 0 || hs_is_eagain(r));
            if (r > 0) {
                memcpy(want + wl, p, (size_t)r);
                wl += (size_t)r;
            }
        }
    }
    sx_drain(sv[1], got, &have, cap);
    WF_FUZZ_ASSERT(have == wl);
    WF_FUZZ_ASSERT(wl == 0 || memcmp(got, want, wl) == 0);

    free(want);
    free(got);
    close(sv[0]);
    close(sv[1]);
    close(f);
    wf_free(file, fs);
    return 0;
}
