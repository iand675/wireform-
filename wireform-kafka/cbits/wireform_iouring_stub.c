/* wireform_iouring_stub.c
 *
 * Non-Linux fallback for the io_uring shim. Defines the same
 * symbol names as `wireform_iouring.c` but always reports
 * "not supported". Compiled instead of the real implementation
 * on macOS, Windows, FreeBSD, etc.
 *
 * The Haskell side calls 'wf_iouring_is_supported' once at
 * startup; that returning 0 keeps the rest of the API
 * unreachable so the stub bodies for open / send / recv are
 * defensive and never invoked in practice.
 */

#ifndef __linux__

#include "wireform_iouring.h"

struct wf_iouring_ring { int unused; };

int
wf_iouring_is_supported(void)
{
  return 0;
}

wf_iouring_ring*
wf_iouring_open(unsigned int entries)
{
  (void)entries;
  return 0;
}

void
wf_iouring_close(wf_iouring_ring* r)
{
  (void)r;
}

int64_t
wf_iouring_send(wf_iouring_ring* r, int fd,
                const void* buf, size_t len)
{
  (void)r; (void)fd; (void)buf; (void)len;
  return -1;
}

int64_t
wf_iouring_recv(wf_iouring_ring* r, int fd,
                void* buf, size_t len)
{
  (void)r; (void)fd; (void)buf; (void)len;
  return -1;
}

#endif /* !__linux__ */
