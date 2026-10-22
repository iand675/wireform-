/*
 * failfns.c - see failfns.h. One translation unit per program (via
 * http1-send_EXTRA) defines the fakes. The command-line renames are
 * undefined first, so the pass-through calls reach the real syscalls
 * under their own names, while every other translation unit (the
 * kernels, the tests) keeps the renamed calls.
 */
#undef send
#undef pread

#include "failfns.h"
#include "wf_fail.h"

#include <errno.h>
#include <sys/types.h>

/* The force-included headers saw the renames, so they declared the
 * fakes' names instead of the real syscalls. Declare the real ones
 * under their own names for the pass-through calls. */
extern ssize_t send(int, const void *, size_t, int);
extern ssize_t pread(int, void *, size_t, off_t);

/* The mode order is the arming order documented in failfns.h. */
ssize_t wf_fail_send(int sock, const void *buf, size_t len, int flags)
{
    if (wf_failpoint("send_short")) {
        /* Really send just over half the buffer, so the caller's loop
         * runs again and what it reports as sent still arrives. */
        size_t half = len > 1 ? (len + 1) / 2 : len;
        return send(sock, buf, half, flags);
    }
    if (wf_failpoint("send_zero")) return 0;
    if (wf_failpoint("send_eintr")) {
        errno = EINTR;
        return -1;
    }
    if (wf_failpoint("send_error")) {
        errno = EPIPE;
        return -1;
    }
    return send(sock, buf, len, flags);
}

ssize_t wf_fail_pread(int fd, void *buf, size_t n, off_t off)
{
    if (wf_failpoint("pread_eintr")) {
        errno = EINTR;
        return -1;
    }
    return pread(fd, buf, n, off);
}
