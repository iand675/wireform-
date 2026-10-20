/*
 * Shared support for the wireform C kernel harnesses.
 *
 * Every test binary and fuzz target in c-harness/ includes this header.
 * It provides:
 *
 *   - a tiny check/report layer for unit and property tests (WF_CHECK*,
 *     WF_RUN, wf_finish), with failures reported as file:line plus the
 *     PRNG seed needed to replay them;
 *   - a deterministic PRNG (splitmix64) seeded from WF_SEED so property
 *     tests are reproducible, and an iteration multiplier (WF_ITERS) so
 *     CI can run them short and nightly jobs long;
 *   - exact-size buffers that make any out-of-bounds read or write fault
 *     immediately, both under the sanitizers and in plain -O3 builds.
 *
 * Buffer helpers
 * --------------
 * wf_dup(src, n) returns a heap copy of exactly n bytes. Under ASan/MSan
 * that is a plain malloc (the sanitizer redzone catches the overrun to
 * the byte); without a sanitizer it is placed flush against an
 * inaccessible guard page, so a one-byte overread SIGSEGVs in an -O3
 * build too. Free with wf_free(p, n).
 *
 * Placement varies with n so tails and prologues get exercised: under a
 * sanitizer the start is misaligned by wf__skew(n) bytes (the end still
 * touches the redzone); with guard pages, half of all lengths end flush
 * against the guard and the other half leave a wf__gap(n)-byte gap, so
 * the end is not always page (and therefore 16/32/64-byte) aligned.
 *
 * wf_guard_tail / wf_guard_head always use guard pages: the buffer ends
 * exactly at (tail) or starts exactly at (head) an unmapped page. Use
 * them to pin down page-crossing SIMD loads in every build mode. Beware
 * that a tail-guarded buffer's END is always page aligned, so its SIMD
 * tail is a whole number of vectors: an exhaustive sweep must also place
 * the end at every offset 0..63 before the guard (fill the gap with a
 * canary; the ASan config catches overreads into it exactly). To sweep
 * start alignments with the end flush, take sub-slices (p + k, n - k).
 *
 * Fuzz targets must not call exit() and should abort on an invariant
 * violation with WF_FUZZ_ASSERT so libFuzzer records a crash artifact.
 * Do not define LLVMFuzzerInitialize: the standalone replay driver does
 * not call it.
 */
#ifndef WF_HARNESS_H
#define WF_HARNESS_H

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

#if defined(__has_feature)
#  if __has_feature(address_sanitizer) || __has_feature(memory_sanitizer) \
      || __has_feature(hwaddress_sanitizer)
#    define WF_SANITIZED_HEAP 1
#  endif
#  if __has_feature(memory_sanitizer)
#    define WF_MSAN 1
#  endif
#endif
#if defined(__SANITIZE_ADDRESS__)
#  define WF_SANITIZED_HEAP 1
#endif
#ifndef WF_SANITIZED_HEAP
#  define WF_SANITIZED_HEAP 0
#endif

#if defined(__GNUC__)
#  define WF_UNUSED __attribute__((unused))
#else
#  define WF_UNUSED
#endif

/* ------------------------------------------------------------------ */
/* Deterministic PRNG                                                  */
/* ------------------------------------------------------------------ */

typedef struct {
    uint64_t s;
} wf_rng;

static inline uint64_t wf_rng_next(wf_rng *r)
{
    uint64_t z = (r->s += 0x9E3779B97F4A7C15ULL);
    z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
    z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
    return z ^ (z >> 31);
}

/* Uniform in [0, bound). bound == 0 returns 0. */
static inline uint64_t wf_rng_below(wf_rng *r, uint64_t bound)
{
    if (bound == 0) return 0;
    return wf_rng_next(r) % bound;
}

static inline void wf_rng_fill(wf_rng *r, uint8_t *dst, size_t n)
{
    size_t i = 0;
    while (i < n) {
        uint64_t v = wf_rng_next(r);
        for (int k = 0; k < 8 && i < n; k++, i++) {
            dst[i] = (uint8_t)(v >> (8 * k));
        }
    }
}

/* Fill from a small alphabet; biases random data towards the bytes a
 * scanner or parser actually branches on. */
static inline void wf_rng_fill_from(wf_rng *r, uint8_t *dst, size_t n,
                                    const uint8_t *alphabet, size_t alen)
{
    for (size_t i = 0; i < n; i++) {
        dst[i] = alphabet[wf_rng_below(r, alen)];
    }
}

static WF_UNUSED uint64_t wf__seed;

/* Seed from WF_SEED (decimal or 0x-hex), else a fixed default so plain
 * `make test` is reproducible. */
static inline wf_rng wf_rng_from_env(void)
{
    const char *s = getenv("WF_SEED");
    wf__seed = s ? strtoull(s, NULL, 0) : 0x5EED5EED5EED5EEDULL;
    wf_rng r = { wf__seed };
    return r;
}

/* Scale a base iteration count by WF_ITERS (default 1). */
static inline long wf_iters(long base)
{
    const char *s = getenv("WF_ITERS");
    long m = s ? strtol(s, NULL, 10) : 1;
    if (m < 1) m = 1;
    return base * m;
}

/* ------------------------------------------------------------------ */
/* Guarded buffers                                                     */
/* ------------------------------------------------------------------ */

typedef struct {
    uint8_t *p;     /* usable bytes start here */
    size_t n;       /* number of usable bytes */
    void *base;     /* mapping base (guard page included) */
    size_t total;   /* mapping length */
} wf_guarded;

static inline size_t wf__page(void)
{
    static size_t pg;
    if (!pg) pg = (size_t)sysconf(_SC_PAGESIZE);
    return pg;
}

static inline wf_guarded wf__guard_alloc(size_t n, int flush_head)
{
    size_t pg = wf__page();
    size_t data_pages = (n + pg - 1) / pg;
    if (data_pages == 0) data_pages = 1;
    size_t total = (data_pages + 2) * pg;
    uint8_t *base = mmap(NULL, total, PROT_READ | PROT_WRITE,
                         MAP_PRIVATE | MAP_ANON, -1, 0);
    if (base == MAP_FAILED) {
        perror("wf_guard: mmap");
        abort();
    }
    if (mprotect(base, pg, PROT_NONE) != 0
        || mprotect(base + (data_pages + 1) * pg, pg, PROT_NONE) != 0) {
        perror("wf_guard: mprotect");
        abort();
    }
    uint8_t *data = base + pg;
    wf_guarded g;
    g.p = flush_head ? data : data + data_pages * pg - n;
    g.n = n;
    g.base = base;
    g.total = total;
    return g;
}

/* Buffer whose last byte is immediately followed by a PROT_NONE page. */
static inline wf_guarded wf_guard_tail(size_t n)
{
    return wf__guard_alloc(n, 0);
}

/* Buffer whose first byte is immediately preceded by a PROT_NONE page. */
static inline wf_guarded wf_guard_head(size_t n)
{
    return wf__guard_alloc(n, 1);
}

static inline void wf_guard_free(wf_guarded g)
{
    munmap(g.base, g.total);
}

/* Start misalignment under a sanitizer / end gap with guard pages; both a
 * function of n alone, so an input always lands at the same addresses. */
static inline size_t wf__skew(size_t n)
{
    return ((n >> 4) + n) & 15;
}

static inline size_t wf__gap(size_t n)
{
    return ((n >> 4) & 1) ? ((n >> 5) % 15) + 1 : 0;
}

/* Exact-size uninitialised buffer (output space). Under MSan, reading a
 * byte the kernel did not write is reported. */
static inline uint8_t *wf_alloc(size_t n)
{
#if WF_SANITIZED_HEAP
    size_t skew = wf__skew(n);
    uint8_t *p = malloc(skew + n ? skew + n : 1);
    if (!p) abort();
    return p + skew;
#else
    return wf_guard_tail(n + wf__gap(n)).p;
#endif
}

/* Exact-size heap copy; see the header comment. src may be NULL when
 * n == 0. Under MSan the copy is fully initialised from src. */
static inline uint8_t *wf_dup(const void *src, size_t n)
{
    uint8_t *p = wf_alloc(n);
    if (n) memcpy(p, src, n);
    return p;
}

static inline void wf_free(void *p, size_t n)
{
#if WF_SANITIZED_HEAP
    free((uint8_t *)p - wf__skew(n));
#else
    /* Recover the mapping from the pointer: data (plus the gap) is flush
     * with the end of the data pages, which start one page after the
     * mapping base. */
    size_t pg = wf__page();
    size_t span = n + wf__gap(n);
    size_t data_pages = (span + pg - 1) / pg;
    if (data_pages == 0) data_pages = 1;
    uint8_t *end = (uint8_t *)p + span;
    uint8_t *base = end - data_pages * pg - pg;
    munmap(base, (data_pages + 2) * pg);
#endif
}

/* ------------------------------------------------------------------ */
/* Checks                                                              */
/* ------------------------------------------------------------------ */

static WF_UNUSED long wf__checks;
static WF_UNUSED long wf__failures;
static WF_UNUSED const char *wf__current = "?";

static inline void wf_hexdump(const char *label, const void *p, size_t n)
{
    const uint8_t *b = p;
    size_t shown = n > 64 ? 64 : n;
    fprintf(stderr, "    %s (%zu bytes):", label, n);
    for (size_t i = 0; i < shown; i++) fprintf(stderr, " %02x", b[i]);
    if (shown < n) fprintf(stderr, " ...");
    fputc('\n', stderr);
}

static inline void wf__fail(const char *file, int line, const char *what)
{
    wf__failures++;
    fprintf(stderr, "FAIL %s:%d [%s] %s (WF_SEED=0x%llx)\n", file, line,
            wf__current, what, (unsigned long long)wf__seed);
    if (wf__failures >= 20) {
        fprintf(stderr, "too many failures, stopping\n");
        exit(1);
    }
}

#define WF_CHECK(cond)                                                    \
    do {                                                                  \
        wf__checks++;                                                     \
        if (!(cond)) wf__fail(__FILE__, __LINE__, #cond);                 \
    } while (0)

#define WF_CHECK_EQ(a, b)                                                 \
    do {                                                                  \
        long long wf__a = (long long)(a), wf__b = (long long)(b);         \
        wf__checks++;                                                     \
        if (wf__a != wf__b) {                                             \
            wf__fail(__FILE__, __LINE__, #a " == " #b);                   \
            fprintf(stderr, "    left=%lld right=%lld\n", wf__a, wf__b);  \
        }                                                                 \
    } while (0)

#define WF_CHECK_MEM(a, b, n)                                             \
    do {                                                                  \
        size_t wf__n = (n);                                               \
        wf__checks++;                                                     \
        if (wf__n && memcmp((a), (b), wf__n) != 0) {                      \
            wf__fail(__FILE__, __LINE__, "memcmp(" #a ", " #b ")");       \
            wf_hexdump(#a, (a), wf__n);                                   \
            wf_hexdump(#b, (b), wf__n);                                   \
        }                                                                 \
    } while (0)

#define WF_RUN(fn)                                                        \
    do {                                                                  \
        wf__current = #fn;                                                \
        fn();                                                             \
    } while (0)

static inline int wf_finish(const char *suite)
{
    if (wf__failures) {
        fprintf(stderr, "%s: %ld of %ld checks FAILED\n", suite, wf__failures,
                wf__checks);
        return 1;
    }
    printf("%s: ok (%ld checks)\n", suite, wf__checks);
    return 0;
}

/* Abort with a message; for fuzz targets (libFuzzer saves the input). */
#define WF_FUZZ_ASSERT(cond)                                              \
    do {                                                                  \
        if (!(cond)) {                                                    \
            fprintf(stderr, "WF_FUZZ_ASSERT %s:%d: %s\n", __FILE__,       \
                    __LINE__, #cond);                                     \
            abort();                                                      \
        }                                                                 \
    } while (0)

/* Byte-stream consumer for structuring fuzz input into parameters. */
typedef struct {
    const uint8_t *p;
    size_t n;
} wf_input;

static inline uint8_t wf_take_u8(wf_input *in)
{
    if (!in->n) return 0;
    uint8_t v = *in->p++;
    in->n--;
    return v;
}

static inline uint32_t wf_take_u32(wf_input *in)
{
    uint32_t v = 0;
    for (int k = 0; k < 4; k++) v |= (uint32_t)wf_take_u8(in) << (8 * k);
    return v;
}

#endif /* WF_HARNESS_H */
