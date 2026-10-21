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
#define REF_RING_MAX ((size_t)1 << (sizeof(size_t) >= 8 ? 32 : 30))

/* Reference: the ring size a successful create must return. */
static inline size_t ref_ring_size(size_t requested, size_t ps)
{
    size_t s = ps;
    while (s < requested) s <<= 1;
    return s;
}

/* Is every page of [p, p + n) mapped (any protection)? On Linux the map
 * is read from /proc/self/maps: probing a deliberately unmapped range
 * with msync()/mincore() makes valgrind report it as a syscall on
 * unaddressable bytes, and the harness has to stay clean there. Elsewhere
 * msync answers it (it fails with ENOMEM on an unmapped range). */
static inline int ref_is_mapped(const void *p, size_t n)
{
    /* -1 (unknown) under valgrind: it keeps freed mappings in its own
     * pool, so /proc/self/maps still lists them. Callers treat unknown
     * as "cannot tell" and skip; valgrind's memcheck is the authority
     * there anyway. (Under Rosetta the map is the real kernel's, so the
     * probe stays valid.) */
    if (wf_under_valgrind()) return -1;
#if defined(__linux__)
    /* The two halves of a ring are separate VMAs when they come from
     * MAP_SHARED file mappings, so walk the ascending map and require
     * the lines to cover [lo, hi) contiguously. */
    uintptr_t lo = (uintptr_t)p, hi = lo + n, cur = lo;
    FILE *f = fopen("/proc/self/maps", "r");
    if (!f) return 0;
    int ok = 0;
    char line[512];
    while (fgets(line, sizeof line, f)) {
        unsigned long a, b;
        if (sscanf(line, "%lx-%lx", &a, &b) != 2) continue;
        if ((uintptr_t)a <= cur && cur < (uintptr_t)b) {
            cur = (uintptr_t)b;
            if (cur >= hi) {
                ok = 1;
                break;
            }
        }
    }
    fclose(f);
    return ok;
#else
    return msync((void *)p, n, MS_ASYNC) == 0;
#endif
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

/* Number of open descriptors below a fixed bound; enough to see a leak.
 * -1 under valgrind, which opens descriptors of its own. */
static inline int ref_open_fds(void)
{
    if (wf_under_valgrind() || wf_under_rosetta()) return -1;
    int n = 0;
    for (int fd = 0; fd < 1024; fd++) {
        if (fcntl(fd, F_GETFD) != -1) n++;
    }
    return n;
}

/* Did the descriptor count stay at `before`? Unknown counts pass: under
 * valgrind the probe is unavailable and valgrind's own leak tracking
 * covers the same ground. */
static inline int ref_fds_unchanged(int before)
{
    int now = ref_open_fds();
    return now < 0 || before < 0 || now == before;
}

#endif
