/* Buffers for core-hash beyond the shared harness.
 *
 * vec_alloc / vec_free: element-aligned exact-size buffers.
 * Data.Vector.Storable memory (the Int32 destinations, the Word16 sources
 * of the Roaring kernels) is naturally aligned, so the FFI contract lets
 * the kernels store int32_t / load uint16_t directly. wf_alloc/wf_dup
 * deliberately misalign byte buffers, which would turn those accesses into
 * false UBSan alignment reports. These keep element alignment and end
 * flush against a guard page in every build mode, so any overrun faults.
 *
 * zero_span_map: a multi-GiB read-only all-zero input that costs 1 MiB of
 * memory (one zero-filled temp file mapped repeatedly over a reserved
 * range), ending flush against a PROT_NONE page. For lengths near
 * INT32_MAX. Returns 0, mapping nothing, under valgrind (minutes of
 * emulation) or if the mapping fails. */
#ifndef CORE_HASH_VECBUF_H
#define CORE_HASH_VECBUF_H

#include "harness.h"

static inline void *vec_alloc(size_t n)
{
    return wf_guard_tail(n).p;
}

static inline void vec_free(void *p, size_t n)
{
    size_t pg = wf__page();
    size_t data_pages = (n + pg - 1) / pg;
    if (data_pages == 0) data_pages = 1;
    uint8_t *base = (uint8_t *)p + n - data_pages * pg - pg;
    munmap(base, (data_pages + 2) * pg);
}

typedef struct {
    uint8_t *base;   /* reservation start */
    uint8_t *end;    /* one past the last readable byte; guard page here */
    size_t total;    /* reservation length */
    FILE *file;
} zero_span;

static inline int zero_span_map(zero_span *z, size_t span)
{
    enum { CHUNK = 1 << 20 };
    const char *pre = getenv("LD_PRELOAD");
    if (pre && strstr(pre, "vgpreload")) return 0;
    size_t chunks = (span + CHUNK - 1) / CHUNK;
    z->total = chunks * (size_t)CHUNK + wf__page();
    z->base = mmap(NULL, z->total, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if (z->base == MAP_FAILED) return 0;
    z->end = z->base + chunks * (size_t)CHUNK;
    z->file = tmpfile();
    if (!z->file || ftruncate(fileno(z->file), CHUNK) != 0) goto fail;
    for (size_t c = 0; c < chunks; c++) {
        if (mmap(z->base + c * CHUNK, CHUNK, PROT_READ, MAP_SHARED | MAP_FIXED,
                 fileno(z->file), 0) == MAP_FAILED) goto fail;
    }
    return 1;
fail:
    fprintf(stderr, "zero_span_map: mapping failed, test skipped\n");
    munmap(z->base, z->total);
    if (z->file) fclose(z->file);
    return 0;
}

static inline void zero_span_unmap(zero_span *z)
{
    munmap(z->base, z->total);
    fclose(z->file);
}

#endif
