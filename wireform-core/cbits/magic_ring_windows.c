#ifdef _WIN32

#include <windows.h>
#include <memoryapi.h>
#include <errno.h>
#include <stdint.h>
#include <string.h>
#include "magic_ring.h"

/* Filled lazily; every racer stores the same value. */
static volatile LONG cached_page_size = 0;

long hs_page_size(void) {
    LONG ps = InterlockedCompareExchange(&cached_page_size, 0, 0);
    if (ps == 0) {
        SYSTEM_INFO si;
        GetSystemInfo(&si);
        ps = (LONG)si.dwAllocationGranularity;
        InterlockedExchange(&cached_page_size, ps);
    }
    return (long)ps;
}

/* Largest ring. A ring is mapped twice, so 2N has to fit the address
 * space; 4 GiB (1 GiB on a 32-bit target) is already far beyond any use,
 * since these are network buffers of KiB to MiB. Capping well below
 * SIZE_MAX / 2 means an absurd request fails with ENOMEM before any
 * mapping (and before the size arithmetic below could wrap). */
#define RING_MAX_SIZE ((size_t)1 << (sizeof(size_t) >= 8 ? 32 : 30))

/* Round up to the next power of two >= val, for 1 <= val <= RING_MAX_SIZE. */
static size_t next_pow2(size_t val) {
    val--;
    for (unsigned s = 1; s < sizeof(size_t) * 8; s <<= 1)
        val |= val >> s;
    return val + 1;
}

int hs_ring_create(size_t requested, struct hs_ring *out) {
    size_t ps = (size_t)hs_page_size();
    size_t size;
    PVOID placeholder = NULL;
    HANDLE section = NULL;
    PVOID view1 = NULL, view2 = NULL;

    if (requested > RING_MAX_SIZE) {
        errno = ENOMEM;
        return -1;
    }
    if (requested < ps)
        requested = ps;

    size = (requested + ps - 1) & ~(ps - 1);
    size = next_pow2(size);

    placeholder = VirtualAlloc2(NULL, NULL, size * 2,
        MEM_RESERVE | MEM_RESERVE_PLACEHOLDER, PAGE_NOACCESS, NULL, 0);
    if (!placeholder) {
        errno = ENOMEM;
        return -1;
    }

    /* Split the placeholder into two halves */
    if (!VirtualFree(placeholder, size, MEM_RELEASE | MEM_PRESERVE_PLACEHOLDER)) {
        VirtualFree(placeholder, 0, MEM_RELEASE);
        errno = ENOMEM;
        return -1;
    }

    section = CreateFileMappingW(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE,
        (DWORD)((unsigned long long)size >> 32), (DWORD)(size & 0xFFFFFFFF), NULL);
    if (!section)
        goto fail;

    view1 = MapViewOfFile3(section, NULL, placeholder, 0, size,
        MEM_REPLACE_PLACEHOLDER, PAGE_READWRITE, NULL, 0);
    if (!view1)
        goto fail;

    view2 = MapViewOfFile3(section, NULL, (char *)placeholder + size, 0, size,
        MEM_REPLACE_PLACEHOLDER, PAGE_READWRITE, NULL, 0);
    if (!view2) {
        UnmapViewOfFile(view1);
        goto fail;
    }

    /* The two views keep the section alive; the handle is no longer needed
     * (and struct hs_ring has no room for it: the Haskell side allocates
     * exactly base + size). */
    CloseHandle(section);

    /* Pre-fault */
    memset(view1, 0, size);

    out->base = view1;
    out->size = size;
    return 0;

fail:
    if (section)
        CloseHandle(section);
    VirtualFree(placeholder, 0, MEM_RELEASE);
    VirtualFree((char *)placeholder + size, 0, MEM_RELEASE);
    errno = ENOMEM;
    return -1;
}

void hs_ring_destroy(struct hs_ring *ring) {
    if (ring && ring->base) {
        UnmapViewOfFile(ring->base);
        UnmapViewOfFile((char *)ring->base + ring->size);
        ring->base = NULL;
        ring->size = 0;
    }
}

#endif /* _WIN32 */
