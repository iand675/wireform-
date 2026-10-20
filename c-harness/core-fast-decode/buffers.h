/* Exact-size buffers for typed arrays (uint64_t outputs, int64_t Arrow
 * buffer pairs) whose C type requires 8-byte alignment. wf_dup/wf_alloc
 * deliberately misalign their start, which is right for byte buffers but
 * makes a typed uint64_t * access undefined. These keep the end exact:
 * under ASan/MSan a plain malloc of n bytes (n is a multiple of 8, so
 * the redzone starts right at the end); otherwise the end is flush
 * against a guard page, which also makes the start 8-aligned. */
#ifndef CORE_FAST_DECODE_BUFFERS_H
#define CORE_FAST_DECODE_BUFFERS_H

#include "harness.h"

typedef struct {
    void *p;
    wf_guarded g;
} a8buf;

static inline a8buf a8_alloc(size_t n)
{
    a8buf b;
    memset(&b, 0, sizeof b);
#if WF_SANITIZED_HEAP
    b.p = malloc(n ? n : 8);
    if (!b.p) abort();
#else
    b.g = wf_guard_tail(n);
    b.p = b.g.p;
#endif
    return b;
}

static inline a8buf a8_dup(const void *src, size_t n)
{
    a8buf b = a8_alloc(n);
    if (n) memcpy(b.p, src, n);
    return b;
}

static inline void a8_free(a8buf b)
{
#if WF_SANITIZED_HEAP
    free(b.p);
#else
    wf_guard_free(b.g);
#endif
}

#endif
