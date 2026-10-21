#ifndef WIREFORM_MAGIC_RING_H
#define WIREFORM_MAGIC_RING_H

#include <stddef.h>

/*
 * A "magic" ring: `size` bytes of memory mapped twice, back to back, at
 * base[0, size) and base[size, 2*size), so byte i and byte i + size are
 * the same byte and any window of up to `size` bytes starting inside the
 * first copy is contiguous.
 */
struct hs_ring {
    void   *base;
    size_t  size;
};

/* System page size (the allocation granularity on Windows). Thread-safe. */
long hs_page_size(void);

/*
 * Map a ring of at least `requested` bytes. The size is rounded up to a
 * page multiple and then to a power of two (minimum one page), so
 * `size - 1` is an index mask. A ring is mapped twice, so 2 * size must
 * fit the address space; a request above 4 GiB (1 GiB on a 32-bit
 * target), which includes the huge values a negative Haskell Int
 * converts to, fails with ENOMEM before anything is mapped. Returns 0
 * and fills *out on success; returns -1 with errno set and leaves *out
 * untouched on failure. The memory starts zeroed. Thread-safe; leaks no
 * descriptors.
 */
int  hs_ring_create(size_t requested, struct hs_ring *out);

/*
 * Unmap both copies. NULL and a struct with base == NULL are no-ops; the
 * struct is cleared, so destroying the SAME struct twice is safe. Calling
 * it on a copy of an already destroyed struct unmaps whatever now lives at
 * that address: the caller must ensure each ring is destroyed once.
 */
void hs_ring_destroy(struct hs_ring *ring);

#endif
