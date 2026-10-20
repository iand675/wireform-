/* Helpers shared by the columnar-simd tests (local to this module). */
#ifndef COLUMNAR_SIMD_TESTUTIL_H
#define COLUMNAR_SIMD_TESTUTIL_H

#include "harness.h"

/* An exact-size buffer whose payload starts `mis` bytes into the
 * allocation, so typed arrays can be passed at any byte alignment. The
 * payload's end is flush with the allocation end (redzone under ASan, guard
 * page otherwise). */
typedef struct {
    uint8_t *base;
    size_t total;
    uint8_t *p;
} xbuf;

static inline xbuf xb_new(const void *src, size_t bytes, unsigned mis)
{
    xbuf b;
    b.total = bytes + mis;
    b.base = wf_alloc(b.total);
    memset(b.base, 0xA5, mis);
    if (bytes && src) memcpy(b.base + mis, src, bytes);
    else if (bytes) memset(b.base + mis, 0xCC, bytes);
    b.p = b.base + mis;
    return b;
}

static inline void xb_free(xbuf b)
{
    wf_free(b.base, b.total);
}

/* The last n bytes of a tail-guarded buffer. */
static inline uint8_t *tail_of(wf_guarded g, size_t n)
{
    return g.p + (g.n - n);
}

/* n bytes ending `gap` bytes before the guard page (g.n >= n + gap), with
 * the gap filled with `canary`: a kernel that reads past n sees bytes that
 * change its result, and one that writes past n clobbers the canary. */
static inline uint8_t *gap_of(wf_guarded g, size_t n, size_t gap, uint8_t canary)
{
    uint8_t *p = g.p + (g.n - n - gap);
    memset(p + n, canary, gap);
    return p;
}

static inline int gap_intact(const uint8_t *p, size_t n, size_t gap, uint8_t canary)
{
    for (size_t i = 0; i < gap; i++) {
        if (p[n + i] != canary) return 0;
    }
    return 1;
}

static inline int64_t rng_i64_between(wf_rng *r, int64_t lo, int64_t hi)
{
    return lo + (int64_t)wf_rng_below(r, (uint64_t)(hi - lo) + 1);
}

/* Random bytes biased towards 0x00 / 0xFF so long runs appear. */
static inline void fill_bits(wf_rng *r, uint8_t *p, size_t n)
{
    switch (wf_rng_below(r, 4)) {
    case 0: memset(p, 0, n); break;
    case 1: memset(p, 0xFF, n); break;
    default: wf_rng_fill(r, p, n); break;
    }
    if (n && wf_rng_below(r, 2)) p[wf_rng_below(r, n)] ^= (uint8_t)(1u << wf_rng_below(r, 8));
}

#endif
