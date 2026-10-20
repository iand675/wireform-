/*
 * wireform-core magic ring, as Wireform.Ring.Internal and
 * Wireform.Transport.Capabilities import it:
 *
 *   foreign import capi unsafe "magic_ring.h hs_ring_create"
 *     c_ring_create :: CSize -> Ptr CRing -> IO CInt
 *   foreign import capi unsafe "magic_ring.h hs_ring_destroy"
 *     c_ring_destroy :: Ptr CRing -> IO ()
 *   foreign import ccall unsafe "hs_page_size"
 *     c_page_size :: IO CLong
 *
 * Caller contract, from the Haskell call sites:
 *
 * hs_ring_create(requested, out)
 *   `requested` is `fromIntegral (n :: Int)`, so ANY size_t is possible:
 *   0, and the huge values a negative Int becomes. The kernel must reject
 *   what it cannot map with -1 (ENOMEM above 2^(bits-2)) and never wrap.
 *   `out` points at a poked {NULL, 0} CRing (alloca'd {void*, size_t},
 *   exactly struct hs_ring off Windows). On 0 the caller peeks base and
 *   size and uses `size - 1` as an index mask, reading and writing windows
 *   of up to `size` bytes starting anywhere in [0, size).
 *
 * hs_ring_destroy(ring)
 *   Called by destroyMagicRing on a FRESH alloca'd copy {base, size} each
 *   time, so the C-side "base = NULL" never reaches the Haskell value: a
 *   second destroyMagicRing of the same MagicRing would unmap whatever
 *   occupies that address by then. Wireform.Network.Transport.Duplex
 *   guards this with mkDestroyOnce; withMagicRing's bracket calls it once.
 *   Destroying the same struct twice is a no-op (tested).
 *
 * hs_page_size()
 *   May be called concurrently from several capabilities.
 */
#ifndef WF_CORE_MAGIC_RING_API_H
#define WF_CORE_MAGIC_RING_API_H

#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>

#include "magic_ring.h"

/* Largest size the kernel will try to map (mirrors RING_MAX_SIZE). */
#define REF_RING_MAX ((size_t)1 << (sizeof(size_t) * 8 - 2))

/* Reference: the ring size a successful create must return. */
static inline size_t ref_ring_size(size_t requested, size_t ps)
{
    size_t s = ps;
    while (s < requested) s <<= 1;
    return s;
}

/* Is every page of [p, p + n) mapped (any protection)? msync fails with
 * ENOMEM on an unmapped range. */
static inline int ref_is_mapped(const void *p, size_t n)
{
    return msync((void *)p, n, MS_ASYNC) == 0;
}

/* Ring bytes are accessed through volatile pointers: b[i] and b[i + size]
 * are the same byte, but the compiler assumes distinct addresses do not
 * alias and at -O3 forwards a store to b[i] past a store to b[i + size].
 * (The same holds for any C code that writes one copy and reads the
 * other in one function.) */
typedef volatile uint8_t ring_byte;

static inline void ring_copy_in(ring_byte *dst, const uint8_t *src, size_t n)
{
    for (size_t i = 0; i < n; i++) dst[i] = src[i];
}

static inline void ring_fill(ring_byte *dst, uint8_t v, size_t n)
{
    for (size_t i = 0; i < n; i++) dst[i] = v;
}

/* Index of the first mismatch, or n. */
static inline size_t ring_cmp(const ring_byte *a, const uint8_t *b, size_t n)
{
    for (size_t i = 0; i < n; i++) {
        if (a[i] != b[i]) return i;
    }
    return n;
}

/* Number of open descriptors below a fixed bound; enough to see a leak. */
static inline int ref_open_fds(void)
{
    int n = 0;
    for (int fd = 0; fd < 1024; fd++) {
        if (fcntl(fd, F_GETFD) != -1) n++;
    }
    return n;
}

#endif
